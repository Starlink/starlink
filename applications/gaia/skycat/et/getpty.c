/* This file contains code that executes a command connected to 
** a virtual- or pseudo-teletype device.  It is adapted from
** the "rxvt" terminal program, the copyright for which is shown
** below.  The adaptation is by D. Richard Hipp, whose contributions
** are placed in the public domain.
*/
/* Copyright 1992 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */
/*
 * This module has been very heavily modified by R. Nation
 * (nation@rocket.sanders.lockheed.com).
 * No additional restrictions are applied
 *
 * Additional modification by Garrett D'Amore (garrett@netcom.com) to
 * allow vt100 printing.  No additional restrictions are applied.
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 */

#include <stdarg.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <grp.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <ctype.h>

#ifdef ALPHA
#define FREEBSD
#endif
#ifdef AIXV3
#include <sys/select.h>
#endif   
#ifdef SVR4
#include <sys/stropts.h>
#define _NEW_TTY_CTRL
#endif
#ifdef AIXV3
#include <termio.h>
#else
#include <termios.h>
#endif /* AIXV3 */

#ifdef FREEBSD
#include <sys/ioctl.h>
#endif

#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>

#if defined(VDISCRD) && !defined(VDISCARD)
#define	VDISCARD	VDISCRD
#endif

#if defined(VWERSE) && !defined(VWERASE)
#define	VWERASE		VWERSE
#endif

#ifndef FREEBSD
#ifndef CINTR
#ifndef _POSIX_VDISABLE
#define _POSIX_VDISABLE 0
#endif

#undef CTRL
#define CTRL(c) ((c) - '@')

#ifdef SVR4
#define CINTR 0177
#define CQUIT CTRL('U')
#define CERASE CTRL('H')
#else
#define CINTR CTRL('C')
#define CQUIT CTRL('\\')
#define CERASE 0177
#endif

#define CEOF CTRL('D')
#define CKILL CTRL('U')
#define CEOL _POSIX_VDISABLE
#define CEOL2 _POSIX_VDISABLE
#define CNSWTCH _POSIX_VDISABLE
#define CSTART CTRL('Q')
#define CSTOP CTRL('S')
#define CSUSP CTRL('Z');
#define CDSUSP _POSIX_VDISABLE
#define CRPRNT CTRL('R')
#define CFLUSH CTRL('O')
#define CWERASE CTRL('W')
#define CLNEXT CTRL('V')
#endif
#endif

#ifndef CEOL
#define CEOL _POSIX_VDISABLE
#endif

/* Print an error message
*/
static void PError(char *zFormat, char *zArg){
  char zBuf[1000];
  sprintf(zBuf,zFormat,zArg);
  perror(zBuf);
}

/* The function GetDTableSize() returns the maximum number of
** file descriptors.  This is very OS dependent, so there are
** serveral versions of the function.
*/
#ifdef SVR4
#define _NEW_TTY_CTRL
#include <sys/resource.h>
static int GetDTableSize(){
  struct rlimit   rlp;
  getrlimit(RLIMIT_NOFILE, &rlp);
  return(rlp.rlim_cur);
}
#elif HPUX
static int GetDTableSize(){
  return sysconf(_SC_OPEN_MAX);
}
#else
#define GetDTableSize getdtablesize
#endif


/* The next variable hold the process ID of the child process.  If
** -1, then there is no child process. */
int iChildPid = -1;

/*  Catch a SIGCHLD signal and exit if the direct child has died.
*/
extern void CleanShutdown(void);
static void CatchDeathOfChild(int nonsense){
  if( wait((int*)0)==iChildPid ){
    CleanShutdown();
  }
}

/* Kill off the child process
*/
void KillChildProcess(void){
  if( iChildPid ) kill(iChildPid,SIGHUP);
#if 0
  if( kill(iChildPid,0) ) return;
  sleep(1);
  if( kill(iChildPid,0) ) return;
  kill(iChildPid,SIGTERM);
  sleep(1);
  kill(iChildPid,SIGKILL);
#endif
}

/* Return TRUE if the child process is alive
*/
int IsChildAlive(void){
  return kill(iChildPid,0)==0;
}

/*  Tell the teletype handler what size the window is.  Called after a window
 *  size change.
 */
void SetSizeOfTTY(
  int fd,         /* The file descriptor for the tty to be set */
  int width,      /* The new width */
  int height      /* The new height */
){
#ifdef TIOCSWINSZ
  struct winsize wsize;
  
  if( fd>=0 ){
    wsize.ws_row = (unsigned short)height;
    wsize.ws_col = (unsigned short)width;
    ioctl(fd,TIOCSWINSZ,(char *)&wsize);
  }
#endif
}

/*  Run the command in a subprocess and return a file descriptor for the
 *  master end of the pseudo-teletype pair with the command talking to
 *  the slave.
 */
int RunCommand(
  char *zCommandName,  /* Name of the command to run in the pseudo-tty */
  char **azArgv,       /* All arguments. (Normally azArgv[0]==zCommandName) */
  int isConsole        /* True if the pseudo-tty is to be the console */
){
  int ptyfd;                             /* File descriptor for the PTY */
  int uid, gid;
  int i;
  struct termios ttmode;
#ifdef SVR4
  char *ttynam;                          /* Name of the TTY */
  extern char *ptsname();                /* Used to find TTY name given PTY */
#else
  static char ptynam[] = "/dev/ptyXX";   /* Name of the PTY */
  static char ttynam[] = "/dev/ttyXX";   /* Name of the TTY */
  char *s3, *s4;                         /* Loop variables used to insert
                                         ** valid characters in place of the
                                         ** X's in the PTY and TTY names */
#endif

  /* Find and open a master pseudo-tty.  Set "ptyfd" to the
  ** file descriptor for this pseudo-tty.
  */
#ifdef SVR4
  ptyfd = open("/dev/ptmx",O_RDWR);
  if (ptyfd < 0) 
    {
      PError("Can't open a pseudo teletype",0);
      return(-1);
    }
  grantpt(ptyfd);
  unlockpt(ptyfd);
  fcntl(ptyfd,F_SETFL,O_NDELAY);
  ttynam = ptsname(ptyfd);
#else
  ptyfd = -1;
  for(s3="pqrstuvwxyz"; *s3; s3++){
    for(s4="0123456789abcdef"; *s4; s4++){
      ptynam[8] = ttynam[8] = *s3;
      ptynam[9] = ttynam[9] = *s4;
      if( (ptyfd = open(ptynam,O_RDWR)) >= 0 ){
        if( geteuid()==0 || access(ttynam,R_OK|W_OK) == 0 ){
          break;
        }else{
          close(ptyfd);
          ptyfd = -1;
        }
      }
    }
    if( ptyfd>=0 ) break;
  }
  if( ptyfd<0 ){
    PError("Can't open a pseudo teletype",0);
    return ptyfd;
  }
  fcntl(ptyfd,F_SETFL,O_NDELAY);
#endif

  /* Gotta deal with the death of the child process */
  signal(SIGCHLD,CatchDeathOfChild);

  /* Create the child process */
  iChildPid = fork();
  if( iChildPid<0 ){
    PError("Can't fork",0);
    close(ptyfd);
    return -1;
  }else if( iChildPid==0 ){
    struct group *gr;
    int ttyfd;

#ifdef UNSETENV
    /* bash may set these env vars, but they should be clear for the child
    ** otherwise the old settings are passed on and will confuse term size 
    */
    unsetenv("LINES");
    unsetenv("COLUMNS");
#endif 
    if( (ttyfd = open(ttynam,O_RDWR))<0 ){
      PError("could not open slave tty %s",ttynam);
      CleanShutdown();
    }
#ifdef SVR4
    ioctl(ttyfd,I_PUSH,"ptem");
    ioctl(ttyfd,I_PUSH,"ldterm");
#endif

    uid = getuid();
#ifndef SVR4
    gr = getgrnam("tty");
    gid = gr ? gr->gr_gid : -1;
    fchown(ttyfd,uid,gid);
    fchmod(ttyfd,0600);
#endif
#ifdef TIOCCONS
    if( isConsole ){
      int on = 1;
      if(  ioctl(ttyfd, TIOCCONS, (unsigned char *)&on) == -1 ){
        fprintf(stderr, "xvt: cannot open console\n");
      }
    }
#endif  /* TIOCCONS */
    for(i=GetDTableSize()-1; i>=0; i--){
      if( i!=ttyfd ) close(i);
    }
    dup(ttyfd);
    dup(ttyfd);
    dup(ttyfd);
    if( ttyfd>2 ) close(ttyfd);
    if( setsid()<0 ) PError("failed to set process group",0);
#if defined(TIOCSCTTY)
    ioctl(0, TIOCSCTTY, 0);
#endif
    {
      int pgrp = getpid();
      ioctl(0, TIOCSPGRP, (char *)&pgrp);
      setpgrp();
      close(open(ttynam, O_WRONLY, 0));
      setpgrp ();
    }

    /* Set up the attributes of the TTY
    */
#ifdef FREEBSD
    ioctl(0,TIOCGETA,(char *)&ttmode);
#else
#   ifdef HPUX
      tcgetattr(0, &ttmode);
#   else
      ioctl(0,TCGETS,(char *)&ttmode);
#   endif        
#endif
 
#ifdef HPUX
    ttmode.c_iflag = BRKINT | IGNPAR | ICRNL| IXON;
    ttmode.c_lflag = ISIG|IEXTEN|ICANON|ECHO|ECHOE|ECHOK;
#else
    ttmode.c_iflag = BRKINT | IGNPAR | ICRNL| IXON | IMAXBEL;
    ttmode.c_lflag = ISIG|IEXTEN|ICANON|ECHO|ECHOE|ECHOK|ECHOCTL|ECHOKE;
#endif                                                       
    ttmode.c_oflag = OPOST | ONLCR ;
    ttmode.c_cflag = B9600 | CS8 | CREAD;

    ttmode.c_cc[VEOF] = CEOF;
#ifdef ALPHA
    (unsigned) ttmode.c_cc[VEOL] = CEOL;
#else
    ttmode.c_cc[VEOL] = CEOL; 
#endif
    ttmode.c_cc[VINTR] = CINTR;
    ttmode.c_cc[VQUIT] = CQUIT;
    ttmode.c_cc[VERASE] = CERASE;
    ttmode.c_cc[VKILL] = CKILL;
#ifdef HPUX
    ttmode.c_cc[VSUSP] = CSWTCH;
#else
    ttmode.c_cc[VSUSP] = CSUSP;
#endif
#ifdef VDSUSP
    ttmode.c_cc[VDSUSP] = CDSUSP;
#endif
    ttmode.c_cc[VSTART] = CSTART;
    ttmode.c_cc[VSTOP] = CSTOP;
#ifdef VREPRINT
    ttmode.c_cc[VREPRINT] = CRPRNT;
#endif
#ifdef VDISCARD
    ttmode.c_cc[VDISCARD] = CFLUSH;
#endif
#ifdef VWERASE
    ttmode.c_cc[VWERASE] = CWERASE;
#endif
#ifdef VLNEXT
    ttmode.c_cc[VLNEXT] = CLNEXT;
#endif
#ifdef VSWTC
    ttmode.c_cc[VSWTC] = 0;
#endif                           
#ifdef VSWTCH
    ttmode.c_cc[VSWTCH] = 0;
#endif 
#if VMIN != VEOF
    ttmode.c_cc[VMIN] = 1;
#endif
#if VTIME != VEOL
    ttmode.c_cc[VTIME] = 0;
#endif
#if 0
    if( mask==0x7f ){
      ttmode.c_cflag = B9600 | PARENB | CS7 | CREAD;
    }
#endif
#ifdef FREEBSD
    ioctl(0,TIOCSETA,(char *)&ttmode);
#else
# ifdef HPUX
    tcsetattr(0, TCSANOW, &ttmode);
# else
    ioctl(0,TCSETS,(char *)&ttmode);
# endif        
#endif
    SetSizeOfTTY(0,80,24);
    setgid(getgid());
    setuid(uid);

    execvp(zCommandName,azArgv);
    PError("Couldn't execute %s",zCommandName);
    exit(1);
  }
  return ptyfd;
}
