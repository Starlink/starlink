/* This routine is the main routine for the Figaro X Window display server. */
/* The initial features will include the initial line graphics capabilites */
/* as descried in the SPECS document. */

/* Sam Southard, Jr. */
/* Created: 25-Oct-1990 */
/*  2-Nov-1990	SNS/CIT	mainloop now returns an integer */
/*  8-Nov-1990	SNS/CIT	added cursor location globals */
/* 14-Nov-1990	SNS/CIT	modified to use a pixmap */
/* 17-Nov-1990	SNS/CIT	lgyoff, lgheight, and lgwidth global variables added */
/* 10-Dec-1990	SNS/CIT	All line graphics variables put into a single struct. */
/*			Wininfo struct for bitmapped graphics added. */
/*  9-May-1991	SNS/CIT	All reference to imaging capabilities removed to */
/*			produce a PGPLOT-only display server.  Variable names */
/*			changed to co-exist with Lick Xvideo routines. */
/* 15-Aug-1991	SNS/CIT	No longer includes xvideo hooks. */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Global variables now in globals.h */
/* 14-Oct-1991	SNS/CIT	Now handles resource specification */
/* 24-Feb-1992	SNS/CIT	Now gets a visual */
/* 14-Oct-1992	SNS/CIT	Now #defines INC_HEADER_RCS to include .h RCS id */
/*			strings.  Now includes all .h files so that all RCS */
/*			id strings get into the executable. */


/* Wish list: */
/*	Add a command line flag to ignore possible presence of a lock. */
/*	Get rid of global variables. */

/* system include files */
#include <stdio.h>

/* X Window include files */
#include <X11/Xlib.h>

/* Program include files */
#define DEFINE_GLOBALS
#define INC_HEADER_RCS
#include "figdisp.h"
#include "messages.h"
#include "globals.h"
#include "commands.h"
#undef DEFINE_GLOBALS
#undef INC_HEADER_RCS

int main(argc,argv)
int argc;
char **argv;
{
	Display *XOpenDisplay();

	void cleanup();	/* clean up before exiting */
	void parsedisp();
	void mergeops();
	void extractops();

	/* Initialize stuff for the resource manager */
	XrmInitialize();

	/* parse the command line options */
	parsedisp(&argc, argv);

	screen=DefaultScreen(display);

	/* get server defaults, program defaults, and .Xdefaults merged */
	/* in with the command line */
	mergeops();

	/* extract the options into a program readable form */
	extractops();

#ifdef DEBUG
	XSynchronize(display, True);
#endif

	/* Set up the resource/locking mechanism and the Atoms needed to */
	/* communicate with applications and initialize the window */
	if (initlock() || getvisuals() || initlgwin()) return(FAIL);

	/* Map the window.  This is not done in initlgwin because initlgwin */
	/* is shared with the Vista server, and the vista server is busy */
	/* enough as it is. */
	XMapWindow(display,lg.win);
	lg.mapped=1;

	/* process events from the applications */
	(void)mainloop();

	/* clean up the windows */
	cleanup();

	/* note that we still need to clean up misc. things */

#ifdef VMS
	return (1);
#else
	return(SUCCEED);
#endif
}
