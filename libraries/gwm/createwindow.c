/*
**
**  INCLUDE FILES
**
*/
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "gwm_err.h"
#include "gwm.h"
#if defined(VMS)
#include <X11/Intrinsic.h>
#include ssdef
#include starlet
#include dvidef
#include iodef
#include descrip
#include prcdef
#include lib$routines.h
#else
#include <time.h>
#include <unistd.h>
#endif

#if defined(VMS)
/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**	Creates a subprocess with the refresh program running in it.
**
**  FORMAL PARAMETERS:
**
**	name:
**	    Name of image to run
**
**	argv:
**	    argument array
**
**  RETURN VALUE:
**
**      VMS status
**
**  DESIGN:
**
**	A temporary mail box is created and a subprocess created with the
**	mail box name as SYS$INPUT. The arguments are then written to the
**	mail box followed by an EOF.
**
**--
*/
int GWM_VMSexecvp( char *name, char *argv[], Display **display,
    char *win_name)
{
    int status, i;
    int iochan1, iochan2, dummy;
    short iosb[4];
    unsigned short l;
    char inp[32], out[32], buf[255], disp[64], prname[14];
    $DESCRIPTOR(inp_descr, inp);
    $DESCRIPTOR(out_descr, out);
    $DESCRIPTOR(name_descr, "");
    $DESCRIPTOR(prname_descr, prname);
/*
**  Create mailbox for sending arguments to the child
*/
    status = sys$crembx( 0, &iochan1, 0, 0, 0xff0f, 0, 0);
    if (status != SS$_NORMAL) return GWM_SS_ERR;

/*
**  Create mailbox for reading replies from the child
*/
    status = sys$crembx( 0, &iochan2, 0, 0, 0xff0f, 0, 0);
    if (status != SS$_NORMAL) return GWM_SS_ERR;

/*
**  Get the physical names of the mail boxes.
*/
    status = lib$getdvi( &DVI$_DEVNAM, &iochan1, 0, 0, &inp_descr, &l);
    if (status != SS$_NORMAL) return GWM_SS_ERR;
    status = lib$getdvi( &DVI$_DEVNAM, &iochan2, 0, 0, &out_descr, &l);
    if (status != SS$_NORMAL) return GWM_SS_ERR;

/*
**  Create the subprocess
*/
    name_descr.dsc$a_pointer = name;
    name_descr.dsc$w_length = strlen(name);

    for ( i = 0; i < 9999; i++ )
    {
	sprintf(prname, "%s%04d", "XREFRESH_", i);
    	status = sys$creprc( 0, &name_descr, &inp_descr, &out_descr, &out_descr,
	    0, 0, &prname_descr, 4, 0, 0, PRC$M_DETACH);
	if (status != SS$_DUPLNAM) break;
    }
    if (status != SS$_NORMAL) return GWM_SS_ERR;

/*
**  Copy the arguments to the mailbox
*/
    for (i = 0; argv[i]; i++)
    {
	strcpy(buf, argv[i]);
	buf[strlen(argv[i])] = '\n';
	buf[strlen(argv[i]) + 1] = '\0';

	status = sys$qiow( 0, iochan1, IO$_WRITEVBLK,
	    &iosb, 0, 0, buf, strlen(buf), 0, 0, 0, 0);
	if (status != SS$_NORMAL) return GWM_SS_ERR;
	if (iosb[0] != SS$_NORMAL) return GWM_SS_ERR;
    }

/*
**  Write an EOF to the mail box to finish the dialogue
*/
    status = sys$qiow( 0, iochan1, IO$_WRITEOF,
	&iosb, 0, 0, 0, 0, 0, 0, 0, 0);
    if (status != SS$_NORMAL) return GWM_SS_ERR;

/*
**  Read replies from the child
*/
    for (;;)
    {
	status = sys$qiow( 0, iochan2, IO$_READVBLK,
	    &iosb, 0, 0, buf, sizeof(buf), 0, 0, 0, 0);
	if (status != SS$_NORMAL) return GWM_SS_ERR;
	if (iosb[0] != SS$_NORMAL) return GWM_CHILD_DEAD;

/*
**	    Decode the message from the child
*/
	buf[iosb[1]] = '\n';
	buf[iosb[1]+1] = '\0';
	if( strncmp("GWM", buf, 3) == 0 )	/* its an GWM message	*/
	{
	    sscanf(buf, "GWM %d %s %s", &status, disp, win_name);
	    if (status == GWM_SUCCESS)
	    {
	        *display = XOpenDisplay(disp);
	        if (!(*display)) status = GWM_NO_DISPLAY;
	    }
	    return status;
	}
	else			/* something else - copy it to stderr */
	{
	    fputs(buf, stderr);
	}
    }
}
#endif

int GWM_CreateWindow( int argc, char *argv[], Display **display, char
	name[])
/*
*+
*  Name:
*     GWM_CreateWindow
*
*  Purpose:
*     Create a window
*
*  Language:
*     C
*
*  Invocation:
*     status = GWM_CreateWindow( argc, argv, &display, &name);
*
*  Description:
*     A GWM window is created according to the specification in the
*     argument list and the display id of the X connection and the name
*     of the window returned.
*
*  Arguments:
*     argc = int (given)
*         Count of number of arguments
*     argv = *char[] (given)
*          xmake arguments
*     display = *Display (returned)
*          display id
*     name = char[] (returned)
*          window name
*
*  Algorithm:
*     A connection to the X server is opened and -display added to the argument
*     list. A "refresh" process is created and the argument list passed to it.
*     A reply is read from the refresh process and if the window is created
*     sucessfully, a display id and window name are returned.
*
*  Copyright:
*     Copyright (C) 1991, 1992, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      7-MAR-1991 (DLT):
*        Orignal version
*      9-MAR-1992 (DLT):
*        Correct selection of VMS process name
*     17-JUL-1992 (DLT):
*        Add unistd.h for dup etc.
*        Wait for \n when reading from pipe
*        Use fork instead of vfork (vfork doesn't exist on all systems
*          or in POSIX).
*      2-MAR-1994 (DLT):
*        Add lib$routines.h for VMS.
*        Reorder to define GWM_VMSexecvp before use
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/
{
    int status, pid, fd[2];
    unsigned int i;
    char buf[255], disp[64];

#if defined(VMS)	/* VMS						    */
/*
**  On VMS the refresh process is does not have access to the logical
**  names in the parent process, including the logical names for X
**  displays. Therefore we have to parse the command line and get the
**  name of the display and add a "-display" argument to the argument
**  list.
*/

    Widget top;
    char *argv1[255], dname[64];
    int argc1;
    short int l;
    $DESCRIPTOR(dname_descr, dname);
    $DESCRIPTOR(disp_descr, "");

/*
**  Copy the argument list and count
*/
    for ( argc1 = 0 ; argc1 <= argc; argc1++) argv1[argc1] = argv[argc1];
    argc1--;

/*
**  Use XtInitialize to parse the command arguments using the standard Xt
**  options. The one we are interested in is -display.
*/
    top = XtInitialize("main", "Xrefresh", NULL, 0, &argc, argv);

/*
**  Open the display and get the display name
*/
    *display = XtDisplay( top );
    if (!*display) return GWM_NO_DISPLAY;

    disp_descr.dsc$w_length = strlen( DisplayString( *display ));
    disp_descr.dsc$a_pointer = DisplayString( *display );
    status = lib$getdvi(&DVI$_DEVNAM, 0, &disp_descr, 0, &dname_descr, &l);
    if (status != SS$_NORMAL) return GWM_SS_ERR;
    dname[l] = '\0';

/*
**  Append -display <name> to the argument list
*/
    argv1[argc1++] = "-display";
    argv1[argc1++] = dname;
    argv1[argc1] = NULL;

    status = GWM_VMSexecvp("GWM_DIR:XREFRESH.EXE", argv1, display, name);

#else			/* UNIX						    */
    pipe(fd);
    pid = fork();
    if (pid == 0)	/* We are the child				    */
    {
/*
**      Remap the standard error channel to the write end of the pipe
**      and close the read end
*/
	close(fd[0]);
	close(2);
	dup2(fd[1],2);
	close(fd[1]);

/*
**	Overlay the xrefresh image
*/
	execvp( "gwmXrefresh", argv);
	_exit( errno );
    }
    else		/* we are the parent */
    {

/*
**	Close the write end of the pipe and wait for a message from the
**	child.
*/
	close(fd[1]);

	for (;;)
	{
	    for ( i = 0; i < sizeof(buf); i++ )
	    {
		status = read( fd[0], &buf[i], 1);
	        if (status <= 0 ) return GWM_CHILD_DEAD;
		if ( buf[i] == '\n') break;
	    }

/*
**	    Decode the message from the child
*/
	    if( strncmp("GWM", buf, 3) == 0 )	/* its an GWM message	*/
	    {
	        buf[i] = '\0';
	        sscanf(buf, "GWM %d %s %s", &status, disp, name);
	        if (status == GWM_SUCCESS)
	        {
	            *display = XOpenDisplay(disp);
		    if (!(*display)) status = GWM_NO_DISPLAY;
	        }
    		return status;
	    }
	    else			/* something else - copy it to stderr */
	    {
	        fputs(buf, stderr);
	    }
	}
    }
#endif
}
