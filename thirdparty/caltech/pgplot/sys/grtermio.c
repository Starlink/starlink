#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTER, GRCTER, GRWTER, GRPTER. */

#include <stdio.h>
#include <termios.h>

#ifdef PG_PPU
#define GROTER groter_
#define GRWTER grwter_
#define GRCTER grcter_
#define GRPTER grpter_
#else
#define GROTER groter
#define GRWTER grwter
#define GRCTER grcter
#define GRPTER grpter
#endif

/* Open a channel to the device specified by 'cdev'.
 *
 * cdev      I    The name of the device to be opened
 * ldev      I    Number of valid characters in cdev
 * groter    O    The open channel number (-1 indicates an error)
 */
int GROTER(cdev, ldev, cdev_len)
     char *cdev; int *ldev;
     int cdev_len;
{
  int fd;        /* The returned file descriptor */
  char name[64]; /* A copy of the given terminal device name */
/*
 * Make a copy of the given file if there is sufficient room in name[].
 */
  if(*ldev <= sizeof(name)-1) {
    strncpy(name, cdev, *ldev);
    name[*ldev] = '\0';
  } else {
    fprintf(stderr, "groter: Terminal file name too long.\n");
    return -1;
  };
/*
 * Open the terminal.
 */
  if((fd = open(name, 2)) == -1) {
    perror(name);
    return -1;
  };
  return fd;
}


/* Close a previously opened channel.
 *
 * fd        I    The channel number to be closed
 */
void GRCTER(fd)
     int *fd;
{
  close(*fd);
  return;
}

/* Write lbuf bytes from cbuf to the channel fd.  Data is written without
 * any formating.
 *
 * fd        I    The channel number
 * cbuf      I    Character array of data to be written
 * lbuf      I/O  The number of bytes to write, set to zero on return
 */
void GRWTER(fd, cbuf, lbuf, cbuf_len)
     int *fd; char *cbuf; int *lbuf; int cbuf_len;
{
   int nwritten = write (*fd, cbuf, *lbuf);
   if (nwritten != *lbuf)
     perror("Error writing to graphics device");
   *lbuf = 0;
   return;
}

/* Write prompt string on terminal and then read response.  This version
 * will try to read lbuf characters.
 *
 * fd        I    The channel number
 * cprom     I    An optional prompt string
 * lprom     I    Number of valid characters in cprom
 * cbuf      O    Character array of data read
 * lbuf    I/O    The number of bytes to read, on return number read
 */
void GRPTER(fd, cprom, lprom, cbuf, lbuf, cprom_len, cbuf_len)
     int *fd; char *cprom; int *lprom; char *cbuf; int *lbuf;
     int cprom_len; int cbuf_len;
{
  char *buff = cbuf;   /* C pointer to FORTRAN string */
  int ndone=0;         /* The number of characters read */
  struct termios term; /* Terminal mode flags */
/*
 * Get the current set of terminal mode flags.
 */
   if(tcgetattr(*fd, &term)==0) {
     struct termios saveterm; /* Saved terminal attributes */
     int ntry;  /* The number of characters still to be read */
     int nread; /* The number of characters read in one iteration */
/*
 * Save the existing terminal mode flags to be restored later.
 */
     saveterm = term;
/*
 * Enable raw single character input.
 */
     term.c_lflag &= ~ICANON;
     term.c_cc[VMIN] = 1;
/*
 * Install the new terminal flags after first waiting for all pending
 * output to be delivered to the terminal and after discarding any
 * lingering input.
 */
     tcsetattr(*fd, TCSAFLUSH, &term);
/*
 * Prompt for input.
 */
     if(*lprom>0) write(*fd, cprom, *lprom);
/*
 * Read up to 'ntry' characters from the terminal.
 */
     ndone = 0;
     ntry = *lbuf;
     do {
       nread = read(*fd, &buff[ndone], ntry);
       ndone += nread;
       ntry  -= nread;
     } while(nread>0 && ntry>0);
/*
 * Restore the previous terminal mode flags.
 */
     tcsetattr(*fd, TCSAFLUSH, &saveterm);
   };
   *lbuf=ndone;
   return;
}
