/*
*+
*  Name:
*     DTASK_SETSIG

*  Purpose:
*     C Version - Set up default Unix signal handlers

*  Language:
*     C

*  Invocation:
*     DTASK_SETSIG is called during dtask initialisation.
*     The routine DTASK_SIGHDLR is called by Unix when a signal occurs

*  Description:
*     Called during DTASK initialisation to alter the defaults for likely
*     Unix Signals (from the terminal, programming or system faults) to
*     cause the process to terminate via the exit() system service. This allows
*     any library "exit handlers" declared via atexit() ( or the non-standard
*     on_exit() on SunOS) to be called.

*  Arguments:
*     Boolean argument IFLAG.
*        Set to .TRUE.  if this task has been started from ICL
*        Set to .FALSE. if this task has been started from the shell

*  Algorithm:
*     Uses standard Unix C system service routines

*  Copyright:
*     Copyright (C) 1966, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999, 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     BKM: B.K. McIlwrath (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     02-DEC-1994 (BKM):
*        Original version.
*     14-JUL-1995 (BKM):
*        Totally revised logic (exit handler idea from RFWS)
*     13-DEC-1995 (BKM):
*        Added signal handlers passing no signal context (traditional Unix,
*        POSIX 1 and Linux).
*     20-MAY-1997 (BKM):
*        Revision to DTASK_SIGHDLR to reset SIGCHLD before forking
*        duplicate process in case the user has his/her own handler
*        for this signal.
*     14-MAY-1999 (BKM):
*        Revise logic for selecting Alpha version of sigaction.
*     17-MAY-1999 (BKM):
*        Revise for new header files with egcs-2.91.66 on RedHat Linux
*        version 6.0.
*     11-JAN-2001 (BKM):
*        Revise to defeat new C compiler extreme fussiness on Compaq Tru64 V5.0
*        (C version 6.1-019) and tested again on all systems.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include <signal.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>
#include "f77.h"
/*
 * DEC Unix has a flags word for FPU control - set by -fpe[0..4] compiler
 * option (on a program unit basis) or by runtime call(s) to the routine
 * for_set_fpe_flags()
 */
#ifdef __alpha
#include <for_fpe_flags.h>
#endif

/* Linked list of signal numbers and the RTL handler routine addresses */
struct siglist
{ int signo;
  struct sigaction act;
  struct siglist *next;
};

/* Linked list head */
static struct siglist *stack_top = NULL;

/* Status bitmask for signals we have processed */
static unsigned int sigisset=0;

/* Function declaration */
static void
#ifndef SA_SIGINFO
dtask_sighdlr(int isig);
#else
#ifndef __alpha
dtask_sighdlr(int isig, siginfo_t *info, void *dummy);
#else
dtask_sighdlr(int isig, int sigcode, struct sigcontext *scp);
#endif
#endif

/* Local function to process a signal */
static void
process_signal(int signo, struct sigaction oact)
{
    struct siglist *entry, *new_entry;
#ifdef __alpha
    void (*hanfun) (int,int,struct sigcontext *);
#else
    void (*hanfun);
#endif

    if( (sigisset>>signo) & 1) /* Signal handler already processed */
	return;

    if( oact.sa_handler != SIG_DFL ) {

/* Create an entry to add to the list */
	if( (new_entry = (struct siglist *) malloc(sizeof(struct siglist)))
		== NULL ) {
	    perror("process_signal - malloc error");
	    exit(1);
	}
	new_entry->signo = signo;
	oact.sa_flags |= SA_RESTART;
	new_entry->act   = oact;
	new_entry->next  = NULL;

/* Find the end of our linked list */
	if( stack_top != NULL ) {
	    for(entry = stack_top; entry->next != NULL; entry=entry->next)
		/* Null statement */;
	    entry->next = new_entry;
	} else
	    stack_top = new_entry;
    }
/* Retarget the signal handler towards dtask_sighdlr - leave all flags etc
 * as they were
 */
#ifdef __alpha
    oact.sa_handler = dtask_sighdlr;
#else
    oact.sa_handler = (void *) dtask_sighdlr;
#endif
    sigaction(signo, &oact, NULL);

/* Mark signal handler set in bitmask */
   sigisset |= (1<<signo);

}

/* DTASK_SETSIG - Fortran callable routine to set up signal handlers.
 *                Called from DTASK_MAIN
 */

F77_SUBROUTINE(dtask_setsig)( LOGICAL(iflag) )
{
    struct sigaction ignact, oact;
    sigset_t curset;

/*   Set up an exit handler and default signal handlers for our process
 *   Note: all signal handlers can be overridden by the user elsewhere in
 *   the process and we should document any side effects of doing this.
 */
    ignact.sa_handler = SIG_IGN;
    ignact.sa_flags = 0;
    sigemptyset(&ignact.sa_mask);

/* The defaults for three terminal related signals depend if we are running
 * from the shell or from ICL or Tcl (via the messaging system).
 */
    if ( F77_ISTRUE(*iflag) ) {	/* Running from ICL or Tcl */
	sigaction(SIGINT,  &ignact, NULL);
	sigaction(SIGQUIT, &ignact, NULL);
/* We use socket based communications - avoid process abort on SIGPIPE
 * (the read/write errors on the closed socket will be detected and
 *  processed by the software)
 */
	sigaction( SIGPIPE, &ignact, NULL);
    } else {			/* Running from the shell */
	sigaction(SIGINT, NULL, &oact);
	    process_signal(SIGINT, oact);
	sigaction(SIGQUIT, NULL, &oact);
	    process_signal(SIGQUIT, oact);
    }

/* Defaults for all processes */

/* Hangup and Default kill signals  - we use these to make an ADAM task exit
 * quietly after running all exit handlers
 */
    sigaction ( SIGHUP, NULL, &oact );
	sigaddset(&oact.sa_mask, SIGTERM);
	process_signal(SIGHUP, oact);
    sigaction ( SIGTERM, NULL, &oact );
	sigaddset(&oact.sa_mask, SIGHUP);
	process_signal(SIGTERM, oact);

/* Make various other programming and system errors report and tidy up */

/* Signals common to most Unix variants - from Stevens page 266 */
    sigaction(SIGABRT, NULL, &oact);
	process_signal(SIGABRT, oact);
    sigaction(SIGFPE, NULL, &oact);
	process_signal(SIGFPE, oact);
    sigaction(SIGILL, NULL, &oact);
	process_signal(SIGILL, oact);
    sigaction(SIGSEGV, NULL, &oact);
	process_signal(SIGSEGV, oact);

/* Signals which may not be present on all variants of Unix */
#ifdef SIGALRM
	if (oact.sa_handler != SIG_DFL)
	    process_signal(SIGALRM, oact);
#endif
#ifdef SIGBUS
    sigaction(SIGBUS, NULL, &oact);
	    process_signal(SIGBUS, oact);
#endif
#ifdef SIGEMT
    sigaction(SIGEMT, NULL, &oact);
	    process_signal(SIGEMT, oact);
#endif
#ifdef SIGIOT
    sigaction(SIGIOT, NULL, &oact);
	    process_signal(SIGIOT, oact);
#endif
#ifdef SIGSYS
    sigaction(SIGSYS, NULL, &oact);
	    process_signal(SIGSYS, oact);
#endif
#ifdef SIGTRAP
    sigaction(SIGTRAP, NULL, &oact);
	    process_signal(SIGTRAP, oact);
#endif
#ifdef SIGXCPU
    sigaction(SIGXCPU, NULL, &oact);
	    process_signal(SIGXCPU, oact);
#endif
#ifdef SIGXFSZ
    sigaction(SIGXFSZ, NULL, &oact);
	    process_signal(SIGXFSZ, oact);
#endif
}

/* DTASK_SIGHDLR - ADAM task signal handler for all process signals */

static void
#ifndef SA_SIGINFO
dtask_sighdlr(int isig)
#else
#ifndef __alpha
dtask_sighdlr(int isig, siginfo_t *info, void *dummy)
#else
dtask_sighdlr(int isig, int sigcode, struct sigcontext *scp)
#endif
#endif
{
    int i, pid, child_status;
    struct siglist *entry, *cur_entry = NULL;
    sigset_t set, curset;
#ifdef __alpha
    void (*hanfun) (int,int,struct sigcontext *);
#endif

#ifdef __alpha
/*
 * DEC Fortran under OSF1 uses SIGFPE to call trap code which produces
 * the IEEE behaviour specified by for_set_fpe() or the -fpe compiler options.
 */
    static struct sigaction fpehandler;
    unsigned int for_fpe_flags, fpe_handler_returns=0;

    if (isig == SIGFPE) {
	if(fpehandler.sa_handler == NULL) {
	    for(entry=stack_top; entry != NULL; entry=entry->next)
		if(entry->signo == SIGFPE) {
		    fpehandler = entry->act;
		    break;
		}
	}
	for_fpe_flags = for_get_fpe_();
	if(
	( sigcode == FPE_FLTOVF_FAULT  && for_fpe_flags & FPE_M_ABRUPT_OVF )||
	( sigcode == FPE_FLTUND_FAULT  && for_fpe_flags & FPE_M_ABRUPT_UND )||
	( sigcode == FPE_FLTDIV_FAULT  && for_fpe_flags & FPE_M_ABRUPT_DIV0)||
	( sigcode == FPE_INVALID_FAULT && for_fpe_flags & FPE_M_ABRUPT_INV ))
	    {
                hanfun = fpehandler.sa_handler;
		hanfun(isig, sigcode, scp);
		return;
	    } else {
		if( (pid = fork()) < 0 )
		    perror("dtask_sighdlr - fork error");
		else if(pid == 0) { /* Child */
		    exit(isig);
		} else { /* Parent */
		    if( waitpid(pid, &child_status, 0) < 0)
			perror("dtask_sighdlr - waitpid error");
                        hanfun = fpehandler.sa_handler;
			hanfun(isig, sigcode, scp);
			_exit(isig);	/* Should never be reached! */
		}
	    }
    } else
#endif
       {

/* Find the RTL handler for the current signal */
	for(entry=stack_top; entry != NULL; entry=entry->next)
	    if(entry->signo == isig) {
		cur_entry = entry;
		break;
	    }

/* Reset all signal handlers to the system default */
        for(i=0; i<sizeof(int)*8; i++) {
	    if( (sigisset>>i) & 1)
		signal(i, SIG_DFL);
	}

/* SIGHUP or SIGTERM are the normal way of quietly exiting an ADAM program */
	if( isig == SIGHUP || isig == SIGTERM)
	    exit(0);
	else {
/* We fork a duplicate process - the parent calls the RTL handler for the
 * current signal while the child process merely exits. This allows exit
 * handlers to be obeyed for abormal termination.
 */

/* First reset any handler the user may have set for SIGCHLD */
	    signal(SIGCHLD, SIG_DFL);

	    if( (pid = fork()) < 0 )
		perror("dtask_sighdlr - fork error");
	    else if(pid == 0) { /* Child */
		exit(isig);
	    } else { /* Parent */
		if( waitpid(pid, &child_status, 0) < 0)
		    perror("dtask_sighdlr - waitpid error");

		if(cur_entry != NULL) {
#ifndef SA_SIGINFO
		    cur_entry->act.sa_handler(isig);
#else
#ifndef __alpha
		    cur_entry->act.sa_sigaction(isig, info, dummy);
#else
                    hanfun = cur_entry->act.sa_handler;
		    hanfun(isig, sigcode, scp);
#endif
#endif

/* We do NOT expect the RTL handler for fatal signals to return. Just in case
 * it does we exit as fast as possible!
 */
		    _exit(isig);
		} else /* No RTL handler - we just resignal and exit */
		    kill(getpid(), isig);
	    }
	}
    }
}

F77_SUBROUTINE(dtask_decbug) ()
{
/*
 * There is bug in OSF Fortran RTL Version 3.6 such that process signals
 * remain blocked after calls to some RTL routines. This (temporary) routine
 * fixes the bug. It should be removed when DEC supply a fix.
 * It is called from DTASK_DTASK()
 *
 */
#ifdef __alpha
    sigset_t newmask=0, curmask;

    if( sigprocmask(SIG_SETMASK, &newmask, &curmask) != 0 ) {
	perror("dtask_decbug - sigprocmask");
	return;
    }
/*    if( curmask != 0 )
 *	fprintf(stderr, "dtask_decbug - bug fixed - mask was %lx\n", curmask);
 */
#endif
}
