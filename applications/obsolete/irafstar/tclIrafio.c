#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_prtype
#define import_zfstat
#define import_spp
#include <iraf.h>
#include "tcl.h"

static int ipc_read( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static int ipc_write( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static int ipc_pid( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[]);
static void termhandler( int sig);

#define BUFSIZE 4096
XCHAR ibuf[BUFSIZE];   /* Buffer for data read from IRAF cl */
XCHAR *ibuf_p=ibuf;    /* Pointer to next character in ibuf */
int nbuf=0;            /* Number of character in ibuf */
int escape=0;          /* Flag for escape processing */
int pid=0;             /* Pid of slave process */

int Tclirafio_Init ( Tcl_Interp *interp)
{
/*
  Create the commands for doing iraf i/o.
*/
   Tcl_CreateCommand(interp, "ipc_read", ipc_read, (ClientData)NULL,
            (Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateCommand(interp, "ipc_write", ipc_write, (ClientData)NULL,
            (Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateCommand(interp, "ipc_pid", ipc_pid, (ClientData)NULL,
            (Tcl_CmdDeleteProc *)NULL);

/*
  Establish a signal handler for SIGTERM (this is what the cl sends when it
  is interrupted.
*/
   signal(SIGTERM, termhandler);

   return TCL_OK;
}

static int ipc_read( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[])
{
    XINT chan = 0;
    XINT maxbytes = BUFSIZE;
    XINT loffset = 0;
    XINT status;
    char cbuf[BUFSIZE];    /* Buffer for input data after conversion */
    char *cbuf_p=cbuf;
    char next;

    for (;;)
    {

/*
    If the input buffer is empty, read again.
*/
	if ( nbuf <= (ibuf_p - ibuf) )
	{
	    ZARDPR(&chan, ibuf, &maxbytes, &loffset);
	    ZAWTPR (&chan, &status);
	    nbuf = status / 2;
	    ibuf_p = ibuf;
	}
	else
	{
	    next = *(ibuf_p++);
	    if (escape)
	    {
		if ( next == '\n' )
		{
		    *(cbuf_p++) = ' ';
		}
		else
		{
		    *(cbuf_p++) = next;
		}
		escape = 0;
	    }
	    else
	    {
		if ( next == '\\' )
		{
		    escape = 1;
		}
		else if ( next == '\n' )
		{
		    *(cbuf_p) = '\0';
		    Tcl_AppendResult ( interp, cbuf, (char *) NULL );
		    cbuf_p = cbuf;
#if defined(DEBUG)
fprintf(stderr, "< %s\n", cbuf);
#endif
		    return TCL_OK;
		}
		else
		{
		    *(cbuf_p++) = next;
		}
	    }
	}
    }
}

static int ipc_pid( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[])
{
/*
    Store the pid of the slave process.
*/
    sscanf(argv[1],"%d",&pid);
    return TCL_OK;
}

static int ipc_write( ClientData clientData, Tcl_Interp *interp, int argc,
    char *argv[])
{
    XINT chan = 1;
    XINT nbytes;
    XINT loffset = 0;
    int i;

    nbytes = strlen(argv[1])*2;
    for ( i = 0; i < strlen(argv[1]); i++) ibuf[i] = (argv[1])[i];
    ZAWRPR (&chan, ibuf, &nbytes, &loffset);
#if defined(DEBUG)
fprintf(stderr, "> %s", argv[1]);
#endif
    return TCL_OK;
}

static void termhandler( int sig)
{
    XCHAR errmsg[] = {'e','r','r','o','r',' ','1',' ',
	'"','s','o','f','t','w','a','r','e',' ',
	't','e','r','m','i','n','a','t','e',' ',
	'(','i','n','t','e','r','r','u','p','t',')','"','\n'};
    XINT chan = 1;
    XINT nbytes = sizeof(errmsg);
    XINT loffset = 0;

    ZAWRPR (&chan, errmsg, &nbytes, &loffset);
    if (pid) kill(pid, SIGTERM);
    exit(0);
}
