
/*
 * bltBgexec.c --
 *
 *	This module implements a background "exec" command for the
 *	Tk toolkit.
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 *
 * The "bgexec" command was created by George Howlett.
 */

#include "bltInt.h"

#ifndef NO_BGEXEC

#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/types.h>

#ifdef HAVE_WAITFLAGS_H
#   include <waitflags.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#   include <sys/wait.h>
#endif

#if (TCL_MAJOR_VERSION == 7)
#define FILEHANDLER_USES_TCLFILES 1
#else
typedef int Tcl_File;
#endif

#ifdef __STDC__
static Tcl_CmdProc BgexecCmd;
static Tcl_CmdDeleteProc BgexecCmdDeleted;
#endif /* __STDC__ */

/*
 *	As of Tcl 7.6, we're using our own version of the old
 *	Tcl_CreatePipeline routine.  I would have tried to use
 *	Tcl_OpenCommandChannel but you can't get at the array of
 *	process ids, unless of course you pry open the undocumented
 *	structure PipeStatus as clientData.  Nor could I figure out
 *	how to set one side of the pipe to be non-blocking.  The whole
 *	channel API seems overly complex for what its supposed to
 *	do. [And maybe that's why it keeps changing every release.]
 */
extern int Blt_CreatePipeline _ANSI_ARGS_((Tcl_Interp *interp, int argc,
	char **argv, int **pidPtrPtr, int *inPipePtr, int *outPipePtr,
	int *errFilePtr));

#ifdef WIN32
#define read(fd, buf, size)	Blt_AsyncRead((fd),(buf),(size))
#define close(fd)		CloseHandle((HANDLE)fd)
#define Tcl_CreateFileHandler	Blt_CreateFileHandler
#define Tcl_DeleteFileHandler	Blt_DeleteFileHandler
#endif

#define READ_AGAIN	(0)
#define READ_EOF	(-1)
#define READ_ERROR	(-2)

/* The wait-related definitions are taken from tclUnix.h */

/*
 * Not all systems declare the errno variable in errno.h. so this
 * file does it explicitly.  The list of system error messages also
 * isn't generally declared in a header file anywhere.
 */

#ifndef WIN32
extern int errno;
#else
static Tcl_HashTable pidTable;
static int initialized = 0;
#endif

/*
 * The type of the status returned by wait varies from UNIX system
 * to UNIX system.  The macro below defines it:
 */

#ifdef AIX
#   define WAIT_STATUS_TYPE pid_t
#else
#ifndef NO_UNION_WAIT
#   define WAIT_STATUS_TYPE union wait
#else
#   define WAIT_STATUS_TYPE int
#endif
#endif

/*
 * Supply definitions for macros to query wait status, if not already
 * defined in header files above.
 */

#ifndef WIFEXITED
#   define WIFEXITED(stat)  (((*((int *) &(stat))) & 0xff) == 0)
#endif

#ifndef WEXITSTATUS
#   define WEXITSTATUS(stat) (((*((int *) &(stat))) >> 8) & 0xff)
#endif

#ifndef WIFSIGNALED
#   define WIFSIGNALED(stat) (((*((int *) &(stat)))) && ((*((int *) &(stat))) == ((*((int *) &(stat))) & 0x00ff)))
#endif

#ifndef WTERMSIG
#   define WTERMSIG(stat)    ((*((int *) &(stat))) & 0x7f)
#endif

#ifndef WIFSTOPPED
#   define WIFSTOPPED(stat)  (((*((int *) &(stat))) & 0xff) == 0177)
#endif

#ifndef WSTOPSIG
#   define WSTOPSIG(stat)    (((*((int *) &(stat))) >> 8) & 0xff)
#endif


#define TRACE_FLAGS (TCL_TRACE_WRITES | TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY)

#define BLOCK_SIZE	1024	/* Size of allocation blocks for string buffer */
#define DEF_BUFFER_SIZE	(BLOCK_SIZE * 8)
#define MAX_READS       100	/* Maximum number of successful reads
			         * before stopping to let Tk catch up
			         * on events */

#ifndef NSIG
#define NSIG 		32	/* Number of signals available */
#endif /*NSIG*/

#ifndef SIGINT
#define SIGINT		2
#endif /* SIGINT */

#ifndef SIGQUIT
#define SIGQUIT		3
#endif /* SIGQUIT */

#ifndef SIGKILL
#define SIGKILL		9
#endif /* SIGKILL */

#ifndef SIGTERM
#define SIGTERM		14
#endif /* SIGTERM */

typedef struct {
    int number;
    char *name;
} SignalId;

static SignalId signalIds[] =
{
#ifdef SIGABRT
    {SIGABRT, "SIGABRT"},
#endif
#ifdef SIGALRM
    {SIGALRM, "SIGALRM"},
#endif
#ifdef SIGBUS
    {SIGBUS, "SIGBUS"},
#endif
#ifdef SIGCHLD
    {SIGCHLD, "SIGCHLD"},
#endif
#if defined(SIGCLD) && (!defined(SIGCHLD) || (SIGCLD != SIGCHLD))
    {SIGCLD, "SIGCLD"},
#endif
#ifdef SIGCONT
    {SIGCONT, "SIGCONT"},
#endif
#if defined(SIGEMT) && (!defined(SIGXCPU) || (SIGEMT != SIGXCPU))
    {SIGEMT, "SIGEMT"},
#endif
#ifdef SIGFPE
    {SIGFPE, "SIGFPE"},
#endif
#ifdef SIGHUP
    {SIGHUP, "SIGHUP"},
#endif
#ifdef SIGILL
    {SIGILL, "SIGILL"},
#endif
#ifdef SIGINT
    {SIGINT, "SIGINT"},
#endif
#ifdef SIGIO
    {SIGIO, "SIGIO"},
#endif
#if defined(SIGIOT) && (!defined(SIGABRT) || (SIGIOT != SIGABRT))
    {SIGIOT, "SIGIOT"},
#endif
#ifdef SIGKILL
    {SIGKILL, "SIGKILL"},
#endif
#if defined(SIGLOST) && (!defined(SIGIOT) || (SIGLOST != SIGIOT)) && (!defined(SIGURG) || (SIGLOST != SIGURG))
    {SIGLOST, "SIGLOST"},
#endif
#ifdef SIGPIPE
    {SIGPIPE, "SIGPIPE"},
#endif
#if defined(SIGPOLL) && (!defined(SIGIO) || (SIGPOLL != SIGIO))
    {SIGPOLL, "SIGPOLL"},
#endif
#ifdef SIGPROF
    {SIGPROF, "SIGPROF"},
#endif
#if defined(SIGPWR) && (!defined(SIGXFSZ) || (SIGPWR != SIGXFSZ))
    {SIGPWR, "SIGPWR"},
#endif
#ifdef SIGQUIT
    {SIGQUIT, "SIGQUIT"},
#endif
#ifdef SIGSEGV
    {SIGSEGV, "SIGSEGV"},
#endif
#ifdef SIGSTOP
    {SIGSTOP, "SIGSTOP"},
#endif
#ifdef SIGSYS
    {SIGSYS, "SIGSYS"},
#endif
#ifdef SIGTERM
    {SIGTERM, "SIGTERM"},
#endif
#ifdef SIGTRAP
    {SIGTRAP, "SIGTRAP"},
#endif
#ifdef SIGTSTP
    {SIGTSTP, "SIGTSTP"},
#endif
#ifdef SIGTTIN
    {SIGTTIN, "SIGTTIN"},
#endif
#ifdef SIGTTOU
    {SIGTTOU, "SIGTTOU"},
#endif
#if defined(SIGURG) && (!defined(SIGIO) || (SIGURG != SIGIO))
    {SIGURG, "SIGURG"},
#endif
#if defined(SIGUSR1) && (!defined(SIGIO) || (SIGUSR1 != SIGIO))
    {SIGUSR1, "SIGUSR1"},
#endif
#if defined(SIGUSR2) && (!defined(SIGURG) || (SIGUSR2 != SIGURG))
    {SIGUSR2, "SIGUSR2"},
#endif
#ifdef SIGVTALRM
    {SIGVTALRM, "SIGVTALRM"},
#endif
#ifdef SIGWINCH
    {SIGWINCH, "SIGWINCH"},
#endif
#ifdef SIGXCPU
    {SIGXCPU, "SIGXCPU"},
#endif
#ifdef SIGXFSZ
    {SIGXFSZ, "SIGXFSZ"},
#endif
    {-1, "unknown signal"},
};

static int StringToSignal _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *SignalToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin, char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption signalOption =
{
    StringToSignal, SignalToString, (ClientData)0
};

typedef struct Sink {
    char *name;			/* Name of the sink */

    char *doneVar;		/* Name of a Tcl variable (malloc'ed)
				 * set to the collected data of the
				 * last UNIX subprocess. */

    char *updateVar;		/* Name of a Tcl variable (malloc'ed)
				 * updated as data is read from the
				 * pipe. */

    char *updateCmd;		/* Start of a Tcl command executed
				 * whenever data is read from the
				 * pipe. */

    Tcl_File file;		/* Used for backward compatability
				 * with Tcl 7.5 */
    int fd;			/* File descriptor of the pipe. */

    int prevEnd;		/* Number of bytes read the last time a
				 * buffer was retrieved */
    int fixMark;		/* Index of fixed newline character in
				 * buffer.  If -1, no fix was made. */

    int echo;			/* Indicates if the pipeline's stderr stream
				 * should be echoed */
    char *byteArr;		/* Stores command output (malloc-ed):
				 * Initially points to static storage
				 */
    int end;			/* Number of characters in the buffer */
    int arraySize;		/* Size of buffer allocated */

    char staticSpace[DEF_BUFFER_SIZE];	/* Static space */

} Sink;

typedef struct BackgroundInfo {
    Tk_Uid statVarId;		/* Name of a Tcl variable set to the
				 * exit status of the last
				 * process. Setting this variable
				 * triggers the termination of all
				 * subprocesses (regardless whether
				 * they have already completed) */

    int signalNum;		/* If non-zero, indicates the signal
				 * to send subprocesses when cleaning
				 * up.*/
    int keepTrailingNewLine;	/* If non-zero, indicates to set Tcl
				 * output variables with trailing
				 * newlines intact */
    int interval;		/* Interval to poll for the exiting
				 * processes */

    /* Private */
    Tk_Window tkwin;		/* Main window of interpreter. Used with
				 * Tk_ConfigureWidget to handle options */
    Tcl_Interp *interp;		/* Interpreter containing variables */

    int numProcs;		/* Number of processes in pipeline */
    int *procArr;		/* Array of process tokens from pipeline.
				 * The token for Unix are pid_t, while
				 * for Win32 they're handles. */

    int traced;			/* Indicates that the status variable
				 * is currently being traced. */
    int detached;		/* Indicates that the pipeline is
				 * detached from standard I/O, running
				 * in the background. */
    Tk_TimerToken timerToken;	/* Token for timer handler which polls
				 * for the exit status of each
				 * sub-process. If zero, there's no
				 * timer handler queued. */

    int *exitCodePtr;		/* Pointer to a memory location to
				 * contain the last process' exit
				 * code. */
    int *donePtr;

    Sink stdoutSink, stderrSink;

} BackgroundInfo;


static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BOOLEAN, "-echo", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stderrSink.echo),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-output", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stdoutSink.doneVar),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-update", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stdoutSink.updateVar),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-error", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stderrSink.doneVar),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-lasterror", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stderrSink.updateVar),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-lastoutput", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stdoutSink.updateVar),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-onerror", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stderrSink.updateCmd),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_STRING, "-onoutput", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, stdoutSink.updateCmd),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-keepnewline", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, keepTrailingNewLine), 0},
    {TK_CONFIG_CUSTOM, "-killsignal", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, signalNum), 0, &signalOption},
    {TK_CONFIG_INT, "-check", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(BackgroundInfo, interval), 0},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

static char *VariableProc _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, char *part1, char *part2, int flags));
static void TimerProc _ANSI_ARGS_((ClientData clientData));
static void StdoutProc _ANSI_ARGS_((ClientData clientData, int mask));
static void StderrProc _ANSI_ARGS_((ClientData clientData, int mask));

/*
 *----------------------------------------------------------------------
 *
 * StringToSignal --
 *
 *	Convert a string represent a signal number into its integer
 *	value.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToSignal(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Signal number */
    char *widgRec;		/* Background info record */
    int offset;			/* Offset of vector in Element record */
{
    int *signalPtr = (int *)(widgRec + offset);
    int signalNum;

    if ((string == NULL) || (*string == '\0')) {
	*signalPtr = 0;
	return TCL_OK;
    }
    if (isdigit(UCHAR(string[0]))) {
	if (Tcl_GetInt(interp, string, &signalNum) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	char *name;
	register SignalId *sigPtr;

	name = string;

	/*  Clip off any "SIG" prefix from the signal name */
	if ((name[0] == 'S') && (name[1] == 'I') && (name[2] == 'G')) {
	    name += 3;
	}
	signalNum = -1;
	for (sigPtr = signalIds; sigPtr->number > 0; sigPtr++) {
	    if (strcmp(sigPtr->name + 3, name) == 0) {
		signalNum = sigPtr->number;
		break;
	    }
	}
	if (signalNum < 0) {
	    Tcl_AppendResult(interp, "unknown signal \"", string, "\"",
		(char *)NULL);
	    return TCL_ERROR;
	}
    }
    if ((signalNum < 1) || (signalNum > NSIG)) {
	/* Outside range of signals */
	Tcl_AppendResult(interp, "signal number \"", string,
	    "\" is out of range", (char *)NULL);
	return TCL_ERROR;
    }
    *signalPtr = signalNum;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SignalToString --
 *
 *	Convert the integer signal number into an ASCII string.
 *
 * Results:
 *	The string representation of the kill signal is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
SignalToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Not used */
    Tk_Window tkwin;		/* Not used */
    char *widgRec;		/* BackgroundInfo record */
    int offset;			/* Offset of signal number in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    int signalNum = *(int *)(widgRec + offset);

    if (signalNum == 0) {
	return "";
    } else {
	char *result;
	char string[20];

	sprintf(string, "%d", signalNum);
	*freeProcPtr = (Tcl_FreeProc *)free;
	result = strdup(string);
	return result;
    }
}

#ifdef WIN32
/*
 *----------------------------------------------------------------------
 *
 * TranslateEOL --
 *
 *	For Windows, translate CR/NL combinations to NL alone.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The size of the byte array may shrink and array contents
 *	shifted as carriage returns are found and removed.
 *
 *----------------------------------------------------------------------
 */
static void
TranslateEOL(sinkPtr)
    Sink *sinkPtr;
{
    register int count;
    register char *s, *p;
    register int i;
    int numBytes;

    count = 0;
    numBytes = sinkPtr->end - sinkPtr->prevEnd;
    s = p = sinkPtr->byteArr + sinkPtr->prevEnd;
    for (i = 0; i < numBytes; i++, p++) {
	if (*p == '\r') {
	    continue;
	}
	if (count < i) {
	    *s = *p;
	}
	s++, count++;
    }
    sinkPtr->end = sinkPtr->prevEnd + count;
}

static int
GetPid(HANDLE hProcess)
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&pidTable, (char *)hProcess);
    if (hPtr == NULL) {
	return -1;
    }
    return (int) Tcl_GetHashValue(hPtr);
}

void
Blt_MapPid(HANDLE hProcess, DWORD pid)
{
    Tcl_HashEntry *hPtr;
    int isNew;

    PurifyPrintf("MapPid(%d)\n", hProcess);
    hPtr = Tcl_CreateHashEntry(&pidTable, (char *)hProcess, &isNew);
    Tcl_SetHashValue(hPtr, (ClientData)pid);
}

static int
DeletePid(HANDLE hProcess)
{
    Tcl_HashEntry *hPtr;
    int pid;

    PurifyPrintf("DeletePid(%d)\n", hProcess);
    hPtr = Tcl_FindHashEntry(&pidTable, (char *)hProcess);
    if (hPtr == NULL) {
	PurifyPrintf("can't find process (%d) in table???\n", hProcess);
    }
    assert(hPtr != NULL);
    pid = (int)Tcl_GetHashValue(hPtr);
    Tcl_DeleteHashEntry(hPtr);
    return pid;
}

/*
 *----------------------------------------------------------------------
 *
 * waitpid --
 *
 *	Emulates the waitpid system call.
 *
 * Results:
 *	Returns 0 if the process is still alive, -1 on an error, or
 *	the pid on a clean close.
 *
 * Side effects:
 *	Unless WNOHANG is set and the wait times out, the process
 *	information record will be deleted and the process handle
 *	will be closed.
 *
 *----------------------------------------------------------------------
 */
#define DEBUG 0
static int
waitpid(child, statusPtr, flags)
    HANDLE child;
    int *statusPtr;
    int flags;
{
    int result;
    DWORD status, exitCode;

#if DEBUG
    PurifyPrintf("WAITPID(%x)\n", child);
#endif
    *statusPtr = 0;
    if (child == INVALID_HANDLE_VALUE) {
	errno = EINVAL;
	return -1;
    }
#if DEBUG
    PurifyPrintf("WAITPID: waiting for 0x%x\n", child);
#endif
    status = WaitForSingleObject(child, (flags & WNOHANG) ? 0 : INFINITE);

#if DEBUG
    PurifyPrintf("WAITPID: wait status is %d\n", status);
#endif
    switch (status) {
    case WAIT_FAILED:
	errno = ECHILD;
	*statusPtr = ECHILD;
	result = -1;
	break;

    case WAIT_TIMEOUT:
	if (flags & WNOHANG) {
	    return 0;		/* Try again */
	}
	result = 0;
	break;

    default:
    case WAIT_ABANDONED:
    case WAIT_OBJECT_0:
	GetExitCodeProcess(child, (DWORD *)&exitCode);
	*statusPtr = ((exitCode << 8) & 0xff00);
#if DEBUG
	PurifyPrintf("WAITPID: exit code of %d is %d (%x)\n", child, 
		*statusPtr, exitCode);
#endif
#ifdef WIN32
	result = DeletePid(child);
#else
	result = (int)child;
#endif
	break;
    }
    CloseHandle(child);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * kill --
 *
 *	Emulates the kill system call.
 *
 * Results:
 *	Returns 0 if the process is killed, -1 on an error.
 *
 * Side effects:
 *	Process is terminated.
 *
 *----------------------------------------------------------------------
 */
static int
kill(pid, signal)
    int pid;
    int signal;
{
    HANDLE hProcess = (HANDLE)pid;

    PurifyPrintf("killing process (handle=%d)\n", hProcess);
    if ((hProcess == NULL) || (hProcess == INVALID_HANDLE_VALUE)) {
	errno = EINVAL;
	return -1;
    }
    switch (signal) {
    case SIGINT:
	/*FALLTHRU*/

    case SIGTERM:
	/*FALLTHRU*/

    case SIGQUIT:
	/*FALLTHRU*/

    case SIGKILL:
    default:
	if (!TerminateProcess(hProcess, 1)) {
	    PurifyPrintf("can't terminate process (handle=%d): %s\n", 
		hProcess, Blt_Win32Error());
	    return -1;
	}
    }
    return 0;
}

#endif /* WIN32 */

/*
 *----------------------------------------------------------------------
 *
 * GetSinkData --
 *
 *	Returns the data currently saved in the buffer
 *
 *----------------------------------------------------------------------
 */
static char *
GetSinkData(sinkPtr)
    Sink *sinkPtr;
{
    sinkPtr->byteArr[sinkPtr->end] = '\0';
    return sinkPtr->byteArr;
}

/*
 *----------------------------------------------------------------------
 *
 * LastRead --
 *
 *	Returns the data saved from the last time this routine
 *	was called.
 *
 *----------------------------------------------------------------------
 */
static char *
LastRead(sinkPtr)
    Sink *sinkPtr;
{
    char *string;

    sinkPtr->byteArr[sinkPtr->end] = '\0';
    string = sinkPtr->byteArr + sinkPtr->prevEnd;
    sinkPtr->prevEnd = sinkPtr->end;
    return string;
}

/*
 *----------------------------------------------------------------------
 *
 * FlushSink --
 *
 *	Flushes the buffer, resetting it to empty.
 *	This is used when we don't want to save all the data from
 *	the pipeline.
 *
 *----------------------------------------------------------------------
 */
static void
FlushSink(sinkPtr)
    Sink *sinkPtr;
{
    sinkPtr->byteArr[0] = '\0';
    sinkPtr->end = sinkPtr->prevEnd = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * InitSink --
 *
 *	Initializes the buffer's storage.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Storage is cleared.
 *
 *----------------------------------------------------------------------
 */
static void
InitSink(sinkPtr, name)
    Sink *sinkPtr;
    char *name;
{
    sinkPtr->name = name;
    sinkPtr->fd = -1;
    sinkPtr->file = (Tcl_File)NULL;
    sinkPtr->byteArr = sinkPtr->staticSpace;
    sinkPtr->fixMark = -1;
    sinkPtr->arraySize = DEF_BUFFER_SIZE;
    FlushSink(sinkPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ResetSinkBuffer --
 *
 *	Resets the buffer's storage, freeing any malloc'ed space.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
ResetSinkBuffer(sinkPtr)
    Sink *sinkPtr;
{
    if (sinkPtr->byteArr != sinkPtr->staticSpace) {
	free(sinkPtr->byteArr);
    }
    InitSink(sinkPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ExtendSinkBuffer --
 *
 *	Doubles the size of the current buffer.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
ExtendSinkBuffer(sinkPtr)
    Sink *sinkPtr;
{
    char *newPtr;

    /*
     * Allocate a new array, double the old size
     */
    sinkPtr->arraySize += sinkPtr->arraySize;
    newPtr = (char *)malloc(sizeof(char) * sinkPtr->arraySize);
    if (newPtr == NULL) {
	return TCL_ERROR;
    }
    strcpy(newPtr, sinkPtr->byteArr);
    if (sinkPtr->byteArr != sinkPtr->staticSpace) {
	free((char *)sinkPtr->byteArr);
    }
    sinkPtr->byteArr = newPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadBytes --
 *
 *	Reads and appends any available data from a given file descriptor
 *	to the buffer.
 *
 * Results:
 *	Returns TCL_OK when EOF is found, TCL_RETURN if reading
 *	data would block, and TCL_ERROR if an error occurred.
 *
 *----------------------------------------------------------------------
 */
static int
ReadBytes(sinkPtr)
    Sink *sinkPtr;
{
    int numBytes, bytesLeft;
    register int i, n;
    char *array;

    /*
     * ------------------------------------------------------------------
     *
     * 	Worry about indefinite postponement.
     *
     * 	Typically we want to stay in the read loop as long as it takes
     * 	to collect all the data that's currently available.  But if
     * 	it's coming in at a constant high rate, we need to arbitrarily
     * 	break out at some point. This allows for both setting the
     * 	update variable and the Tk program to handle idle events.
     *
     * ------------------------------------------------------------------
     */

    for (i = 0; i < MAX_READS; i++) {

	/*
	 * Allocate a larger buffer when the number of remaining bytes
	 * is below the threshold BLOCK_SIZE.
	 */

	bytesLeft = sinkPtr->arraySize - sinkPtr->end;

	if (bytesLeft < BLOCK_SIZE) {
	    if (ExtendSinkBuffer(sinkPtr) != TCL_OK) {
		return READ_ERROR;
	    }
	    /* Size of buffer has changed. */
	    bytesLeft = sinkPtr->arraySize - sinkPtr->end;
	}
	array = sinkPtr->byteArr + sinkPtr->end;

	/*
	 * Read into a buffer but make sure we leave room for a
	 * trailing NUL byte.
	 */
	numBytes = read(sinkPtr->fd, array, bytesLeft - 1);
	if (numBytes == 0) {	/* EOF: break out of loop. */
	    return READ_EOF;
	}
	if (numBytes < 0) {

	    /*
	     * Either an error has occurred or no more data is
	     * currently available to read.
	     */
#ifdef O_NONBLOCK
	    if (errno == EAGAIN) {
#else
	    if (errno == EWOULDBLOCK) {
#endif /*O_NONBLOCK*/
		return READ_AGAIN;
	    }
	    sinkPtr->byteArr[0] = '\0';
	    return READ_ERROR;
	}
	/* Clean out NUL bytes, make spaces */
	for (n = 0; n < numBytes; n++) {
	    if (array[n] == 0) {
		array[n] = ' ';
	    }
	}
	sinkPtr->end += numBytes;
	sinkPtr->byteArr[sinkPtr->end] = '\0';
    }
    return numBytes;
}

/*
 *----------------------------------------------------------------------
 *
 * FixNewline --
 *
 *	Clips off the trailing newline in the buffer (if one exists).
 *	Saves the location in the buffer where the fix was made.
 *
 *----------------------------------------------------------------------
 */
static void
FixNewline(sinkPtr)
    Sink *sinkPtr;
{
    sinkPtr->fixMark = -1;
    if (sinkPtr->end > 0) {
	int mark = sinkPtr->end - 1;

	if (sinkPtr->byteArr[mark] == '\n') {
	    sinkPtr->byteArr[mark] = '\0';
	    sinkPtr->fixMark = mark;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * UnfixNewline --
 *
 *	Restores the previously clipped newline in the buffer.
 *	The fixMark field indicates whether one was clipped.
 *
 *----------------------------------------------------------------------
 */
static void
UnfixNewline(sinkPtr)
    Sink *sinkPtr;
{
    if (sinkPtr->fixMark >= 0) {
	sinkPtr->byteArr[sinkPtr->fixMark] = '\n';
	sinkPtr->fixMark = -1;
    }
}


#define IsOpenSink(sinkPtr)  ((sinkPtr)->fd != -1)
    
static void
CloseSink(bgPtr, sinkPtr)
    BackgroundInfo *bgPtr;
    Sink *sinkPtr;
{
    if (IsOpenSink(sinkPtr)) {
#ifndef WIN32
	close(sinkPtr->fd);
#endif
#ifdef FILEHANDLER_USES_TCLFILES
	Tcl_DeleteFileHandler(sinkPtr->file);
	Tcl_FreeFile(sinkPtr->file);
#else
	Tcl_DeleteFileHandler(sinkPtr->fd);
#endif
	sinkPtr->file = (Tcl_File)NULL;
	sinkPtr->fd = -1;

#if DEBUG
	PurifyPrintf("CloseSink: set done var %s\n", sinkPtr->name);
#endif
	if (sinkPtr->doneVar != NULL) {
	    /*
	     * If data is to be collected, set the "done" variable with
	     * the contents of the buffer.
	     */
	    if (!bgPtr->keepTrailingNewLine) {
		FixNewline(sinkPtr);
	    }
	    if (Tcl_SetVar(bgPtr->interp, sinkPtr->doneVar,
		    GetSinkData(sinkPtr), TCL_GLOBAL_ONLY) == NULL) {
		Tk_BackgroundError(bgPtr->interp);
	    }
	}
#if DEBUG
	PurifyPrintf("CloseSink %s: done\n", sinkPtr->name);
#endif
    }
}

static int
CollectData(bgPtr, sinkPtr)
    BackgroundInfo *bgPtr;
    Sink *sinkPtr;
{
    int status;
    int flags;

    /*
     * If there is no output variable (-update used alone) there's no
     * need to accumulate the output in memory.  Reset the counters so
     * we only keep what's last read.
     */
    if ((bgPtr->detached) && (sinkPtr->doneVar == NULL)) {
	FlushSink(sinkPtr);
    }
    flags = (TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
    status = ReadBytes(sinkPtr);
#ifdef WIN32
    TranslateEOL(sinkPtr);
#endif
    if (((sinkPtr->updateCmd != NULL) || (sinkPtr->updateVar != NULL) ||
	    (sinkPtr->echo)) && (sinkPtr->prevEnd < sinkPtr->end)) {
	char *data;

	if (!bgPtr->keepTrailingNewLine) {
	    FixNewline(sinkPtr);
	}
	data = LastRead(sinkPtr);
#if DEBUG_0
	PurifyPrintf("read %s", data);
#endif
	if (data[0] != '\0') {
	    if (sinkPtr->echo) {
		Tcl_Channel channel;

		channel = Tcl_GetStdChannel(TCL_STDERR);
		if (channel == NULL) {
		    Tcl_AppendResult(bgPtr->interp,
			"can't get stderr channel", (char *)NULL);
		    Tk_BackgroundError(bgPtr->interp);
		    sinkPtr->echo = FALSE;
		}
		Tcl_Write(channel, data, -1);
		if (sinkPtr->fixMark >= 0) {
		    Tcl_Write(channel, "\n", -1);
		}
		Tcl_Flush(channel);
	    }
	    if (sinkPtr->updateCmd != NULL) {
		Tcl_DString dString;
		int result;

		Tcl_DStringInit(&dString);
		Tcl_DStringAppend(&dString, sinkPtr->updateCmd, -1);
		Tcl_DStringAppend(&dString, " ", -1);
		Tcl_DStringAppendElement(&dString, data);
		result = Tcl_GlobalEval(bgPtr->interp,
		    Tcl_DStringValue(&dString));
		Tcl_DStringFree(&dString);
		if (result != TCL_OK) {
		    Tk_BackgroundError(bgPtr->interp);
		}
	    }
	    if (sinkPtr->updateVar != NULL) {
		if (Tcl_SetVar(bgPtr->interp, sinkPtr->updateVar,
			data, flags) == NULL) {
		    Tk_BackgroundError(bgPtr->interp);
		}
	    }
	}
	if (!bgPtr->keepTrailingNewLine) {
	    UnfixNewline(sinkPtr);
	}
    }
    if (status >= 0) {
	return TCL_RETURN;
    }
    if (status == READ_ERROR) {
	Tcl_AppendResult(bgPtr->interp, "can't read data from ", sinkPtr->name,
		 ": ", Tcl_PosixError(bgPtr->interp), (char *)NULL);
	Tk_BackgroundError(bgPtr->interp);
    }
    /*
     * Either EOF or an error has occurred.  In either case,
     * close the sink.
     */
    CloseSink(bgPtr, sinkPtr);
#if DEBUG
    PurifyPrintf("CollectData %s: done\n", sinkPtr->name);
#endif
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateSinkHandler --
 *
 *	Creates a file handler for the given sink.  The file
 *	descriptor is also set for non-blocking I/O.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The memory allocated to the BackgroundInfo structure released.
 *
 *----------------------------------------------------------------------
 */
static int
CreateSinkHandler(bgPtr, sinkPtr, proc)
    BackgroundInfo *bgPtr;
    Sink *sinkPtr;
    Tcl_FileProc *proc;
{
#ifndef WIN32
    int flags;

    flags = fcntl(sinkPtr->fd, F_GETFL);
#ifdef O_NONBLOCK
    flags |= O_NONBLOCK;
#else
    flags |= O_NDELAY;
#endif
    if (fcntl(sinkPtr->fd, F_SETFL, flags) < 0) {
	Tcl_AppendResult(bgPtr->interp, "can't set file descriptor ",
	    Blt_Int(sinkPtr->fd), " to non-blocking:",
	    Tcl_PosixError(bgPtr->interp), (char *)NULL);
	return TCL_ERROR;
    }
#endif /* WIN32 */
#ifdef FILEHANDLER_USES_TCLFILES
    sinkPtr->file = Tcl_GetFile((ClientData)sinkPtr->fd, TCL_UNIX_FD);
    Tcl_CreateFileHandler(sinkPtr->file, TK_READABLE, proc, (ClientData)bgPtr);
#else
    Tcl_CreateFileHandler(sinkPtr->fd, TK_READABLE, proc, (ClientData)bgPtr);
#endif /* FILEHANDLER_USES_TCLFILES */
    return TCL_OK;
}


static void
DisableTriggers(bgPtr)
    BackgroundInfo *bgPtr;	/* Background info record. */
{

    if (bgPtr->traced) {
	Tcl_UntraceVar(bgPtr->interp, bgPtr->statVarId, TRACE_FLAGS, 
		       VariableProc, (ClientData)bgPtr);
	bgPtr->traced = FALSE;
    }

    if (IsOpenSink(&(bgPtr->stdoutSink))) {
	CloseSink(bgPtr, &(bgPtr->stdoutSink));
    }
    if (IsOpenSink(&(bgPtr->stderrSink))) {
	CloseSink(bgPtr, &(bgPtr->stderrSink));
    }
    if (bgPtr->timerToken != (Tk_TimerToken) 0) {
	Tk_DeleteTimerHandler(bgPtr->timerToken);
	bgPtr->timerToken = 0;
    }
    if (bgPtr->donePtr != NULL) {
	*bgPtr->donePtr = TRUE;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyBackgroundInfo --
 *
 * 	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 * 	to clean up the internal structure (BackgroundInfo) at a safe
 * 	time (when no one is using it anymore).
 *
 * Results:
 *	None.b
 *
 * Side effects:
 *	The memory allocated to the BackgroundInfo structure released.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
DestroyBackgroundInfo(bgPtr)
    BackgroundInfo *bgPtr;	/* Background info record. */
{
    DisableTriggers(bgPtr);
    ResetSinkBuffer(&(bgPtr->stderrSink));
    ResetSinkBuffer(&(bgPtr->stdoutSink));
    if (bgPtr->procArr != NULL) {
	if (bgPtr->signalNum > 0) {
	    register int i;
	    
	    for (i = 0; i < bgPtr->numProcs; i++) {
		kill(bgPtr->procArr[i], bgPtr->signalNum);
	    }
	}
	if (bgPtr->numProcs > 0) {
#if (TCL_MAJOR_VERSION >= 8)
	    Tcl_DetachPids(bgPtr->numProcs, (Tcl_Pid *)bgPtr->procArr);
#else
	    Tcl_DetachPids(bgPtr->numProcs, (int *)bgPtr->procArr);
#endif
	}
	free((char *)bgPtr->procArr);
    }
    Tk_FreeOptions(configSpecs, (char *)bgPtr, Tk_Display(bgPtr->tkwin), 0);
    Blt_FreeUid(bgPtr->statVarId);
    Tcl_ReapDetachedProcs();
    free((char *)bgPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * VariableProc --
 *
 *	Kills all currently running subprocesses (given the specified
 *	signal). This procedure is called when the user sets the status 
 *	variable associated with this group of child subprocesses. 
 *
 * Results:
 *	Always returns NULL.  Only called from a variable trace.
 *
 * Side effects:
 *	The subprocesses are signaled for termination using the 
 *	specified kill signal.  Additionally, any resources allocated
 *	to track the subprocesses is released.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static char *
VariableProc(clientData, interp, part1, part2, flags)
    ClientData clientData;	/* File output information. */
    Tcl_Interp *interp;
    char *part1, *part2;	/* Unused */
    int flags;
{
    if (flags & TRACE_FLAGS) {
	BackgroundInfo *bgPtr = (BackgroundInfo *)clientData;

	/* Kill all child processes that remain alive. */
	if ((bgPtr->procArr != NULL) && (bgPtr->signalNum > 0)) {
	    register int i;
	    
	    for (i = 0; i < bgPtr->numProcs; i++) {
		kill(bgPtr->procArr[i], bgPtr->signalNum);
	    }
	}
#ifdef notdef
	/* If the pipeline is detached, clean up resources used. */
	if (bgPtr->detached) {
	    DestroyBackgroundInfo(bgPtr);
	}
#endif
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * TimerProc --
 *
 *	This is a timer handler procedure which gets called
 *	periodically to reap any of the sub-processes if they have
 *	terminated.  After the last process has terminated, the
 *	contents of standard output are stored
 *	in the output variable, which triggers the cleanup proc (using
 *	a variable trace). The status the last process to exit is
 *	written to the status variable.
 *
 * Results:
 *	None.  Called from the Tk event loop.
 *
 * Side effects:
 *	Many. The contents of procArr is shifted, leaving only those
 *	sub-processes which have not yet terminated.  If there are
 *	still subprocesses left, this procedure is placed in the timer
 *	queue again. Otherwise the output and possibly the status
 *	variables are updated.  The former triggers the cleanup
 *	routine which will destroy the information and resources
 *	associated with these background processes.
 *
 *----------------------------------------------------------------------
 */
static void
TimerProc(clientData)
    ClientData clientData;
{
    BackgroundInfo *bgPtr = (BackgroundInfo *)clientData;
    register int i;
    int lastPid, pid;
    WAIT_STATUS_TYPE waitStatus, lastStatus;
    int numLeft;		/* Number of processes still not reaped */
    char *mesg, mesgStr[200];
    Tcl_DString dString;
    int code;
    char *result;

    lastPid = -1;
    *((int *)&waitStatus) = 0;
    *((int *)&lastStatus) = 0;

    numLeft = 0;
    for (i = 0; i < bgPtr->numProcs; i++) {
	pid = waitpid(bgPtr->procArr[i], (int *)&waitStatus, WNOHANG);
	if (pid == 0) {		/*  Process has not terminated yet */
	    if (numLeft < i) {
		bgPtr->procArr[numLeft] = bgPtr->procArr[i];
	    }
	    numLeft++;		/* Count the number of processes left */
	} else if (pid > 0) {
	    /*
	     * Save the status information associated with the subprocess.
	     * We'll use it only if this is the last subprocess to be reaped.
	     */
	    lastStatus = waitStatus;
	    lastPid = pid;
	}
    }
    bgPtr->numProcs = numLeft;

    if (numLeft > 0) {
	/* Keep polling for the status of the children that are left */
	bgPtr->timerToken = Tk_CreateTimerHandler(bgPtr->interval, TimerProc,
	    (ClientData)bgPtr);
#if DEBUG
	PurifyPrintf("schedule TimerProc(numProcs=%d)\n", numLeft);
#endif
	return;
    } 

    /*  
     * All child processes have completed.  Set the status variable
     * with the status of the last process reaped.  The status is a
     * list of an error token, the exit status, and a message.  
     */
    code = WEXITSTATUS(lastStatus);
    Tcl_DStringInit(&dString);
    if (WIFEXITED(lastStatus)) {
	Tcl_DStringAppendElement(&dString, "EXITED");
	mesg = "child completed normally";
    } else if (WIFSIGNALED(lastStatus)) {
	Tcl_DStringAppendElement(&dString, "KILLED");
	mesg = Tcl_SignalMsg((int)(WTERMSIG(lastStatus)));
	code = -1;
    } else if (WIFSTOPPED(lastStatus)) {
	Tcl_DStringAppendElement(&dString, "STOPPED");
	mesg = Tcl_SignalMsg((int)(WSTOPSIG(lastStatus)));
	code = -1;
    } else {
	Tcl_DStringAppendElement(&dString, "UNKNOWN");
	sprintf(mesgStr, "child completed with unknown status 0x%x",
		*((int *)&lastStatus));
	mesg = mesgStr;
    }
    Tcl_DStringAppendElement(&dString, Blt_Int(lastPid));
    Tcl_DStringAppendElement(&dString, Blt_Int(code));
    Tcl_DStringAppendElement(&dString, mesg);
    if (bgPtr->exitCodePtr != NULL) {
	*bgPtr->exitCodePtr = code;
    }
    DisableTriggers(bgPtr);
    result = Tcl_SetVar(bgPtr->interp, bgPtr->statVarId, 
	Tcl_DStringValue(&dString), TCL_GLOBAL_ONLY);
    Tcl_DStringFree(&dString);
    if (result == NULL) {
	Tk_BackgroundError(bgPtr->interp);
    }
    if (bgPtr->detached) {
	DestroyBackgroundInfo(bgPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Stdoutproc --
 *
 *	This procedure is called when output from the detached command
 *	is available.  The output is read and saved in a buffer in the
 *	BackgroundInfo structure.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Data is stored in the buffer.  This character array may
 *	be increased as more space is required to contain the output
 *	of the command.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
StdoutProc(clientData, mask)
    ClientData clientData;	/* File output information. */
    int mask;			/* Not used. */
{
    BackgroundInfo *bgPtr = (BackgroundInfo *)clientData;

    if (CollectData(bgPtr, &(bgPtr->stdoutSink)) != TCL_RETURN) {
	/*
	 * We're here if we've seen EOF or an error has occurred.  In
	 * either case, set up a timer handler to periodically poll
	 * for exit status of each process.  Initially check at the
	 * next idle interval.
	 */
	bgPtr->timerToken = Tk_CreateTimerHandler(0, TimerProc,
	    (ClientData)bgPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StderrProc --
 *
 *	This procedure is called when error from the detached command
 *	is available.  The error is read and saved in a buffer in the
 *	BackgroundInfo structure.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Data is stored in the buffer.  This character array may
 *	be increased as more space is required to contain the stderr
 *	of the command.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
StderrProc(clientData, mask)
    ClientData clientData;	/* File output information. */
    int mask;			/* Not used. */
{
    BackgroundInfo *bgPtr = (BackgroundInfo *)clientData;

    CollectData(bgPtr, &(bgPtr->stderrSink));
}

/*
 *----------------------------------------------------------------------
 *
 * BgexecCmd --
 *
 *	This procedure is invoked to process the "bgexec" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
BgexecCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int *errFdPtr;
    int numProcs, *pidPtr;
    char *lastArg;
    BackgroundInfo *bgPtr;
    int i;
    int detached;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " varName ?options? command ?arg...?\"", (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * Check if the command is to be run detached (indicated a '&' as
     * the last argument of the command)
     */
    lastArg = argv[argc - 1];
    detached = ((lastArg[0] == '&') && (lastArg[1] == '\0'));
    if (detached) {
	argc--;
	argv[argc] = NULL;	/* Remove the '&' argument */
    }
    for (i = 2; i < argc; i += 2) {
	/* Count the number of option-value pairs */
	if ((argv[i][0] != '-') || (argv[i][1] == '-')) {
	    break;		/* Not an option or "--" */
	}
    }
    if (i > argc) {
	i = argc;
    }
    bgPtr = (BackgroundInfo *)calloc(1, sizeof(BackgroundInfo));
    assert(bgPtr);

    /* Initialize the background information record */
    bgPtr->interp = interp;
    bgPtr->tkwin = (Tk_Window)clientData;	/* Main window of interpreter */
    bgPtr->signalNum = SIGKILL;
    bgPtr->numProcs = -1;
    bgPtr->interval = 1000;
    bgPtr->detached = detached;
    bgPtr->statVarId = Blt_GetUid(argv[1]);
    InitSink(&(bgPtr->stdoutSink), "stdout");
    InitSink(&(bgPtr->stderrSink), "stderr");
#ifdef WIN32
    bgPtr->stderrSink.echo = FALSE;
#endif
    if (Tk_ConfigureWidget(interp, bgPtr->tkwin, configSpecs, i - 2, argv + 2,
	    (char *)bgPtr, 0) != TCL_OK) {
	free((char *)bgPtr);
	return TCL_ERROR;
    }
    if (argc <= i) {
	Tcl_AppendResult(interp, "missing command to execute: should be \"",
	    argv[0], " varName ?options? command ?arg...?\"", (char *)NULL);
	/* Try to clean up any detached processes */
	Tcl_ReapDetachedProcs();
	Tk_FreeOptions(configSpecs, (char *)bgPtr, Tk_Display(bgPtr->tkwin), 0);
	free((char *)bgPtr);
	return TCL_ERROR;
    }
    if (argv[i][0] == '-') {
	i++;			/* If the last option was "--", skip it. */
    }
    /*
     * Put a trace on the exit status variable.  The will also
     * allow the user to prematurely terminate the pipeline by
     * simply setting it.
     */

    Tcl_TraceVar(interp, bgPtr->statVarId, TRACE_FLAGS, VariableProc,
	(ClientData)bgPtr);
    bgPtr->traced = TRUE;

    errFdPtr = (int *)NULL;
    if ((bgPtr->stderrSink.doneVar != NULL) ||
	(bgPtr->stderrSink.updateVar != NULL) ||
	(bgPtr->stderrSink.updateCmd != NULL) ||
	(bgPtr->stderrSink.echo)) {
	errFdPtr = &(bgPtr->stderrSink.fd);
    }
    numProcs = Blt_CreatePipeline(interp, argc - i, argv + i, &pidPtr,
	(int *)NULL, &(bgPtr->stdoutSink.fd), errFdPtr);
    if (numProcs < 0) {
	goto error;
    }
    bgPtr->procArr = pidPtr;
    bgPtr->numProcs = numProcs;

    if (bgPtr->stdoutSink.fd == -1) {
	/*
	 * If output has been redirected, start polling immediately
	 * for the exit status of each process.  Normally, this is
	 * done only after stdout has been closed by the last process.
	 * The default polling interval is every 1 second.
	 */
	bgPtr->timerToken = Tk_CreateTimerHandler(bgPtr->interval, TimerProc,
	    (ClientData)bgPtr);

    } else if (CreateSinkHandler(bgPtr, &(bgPtr->stdoutSink), 
			 StdoutProc) != TCL_OK) {
	goto error;
    }
    if ((bgPtr->stderrSink.fd != -1) &&
	(CreateSinkHandler(bgPtr, &(bgPtr->stderrSink), 
			   StderrProc) != TCL_OK)) {
	goto error;
    }
    if (bgPtr->detached) {
	/* Return a list of the child process ids */
	for (i = 0; i < numProcs; i++) {
	    int pid;

#ifdef WIN32
	    pid = GetPid((HANDLE) bgPtr->procArr[i]);
#else
	    pid = bgPtr->procArr[i];
#endif
	    Tcl_AppendElement(interp, Blt_Int(pid));
	}
    } else {
	int exitCode;
	int done;

	bgPtr->exitCodePtr = &exitCode;
	bgPtr->donePtr = &done;

	exitCode = done = 0;
	while (!done) {
	    Tk_DoOneEvent(0);
	}
	DisableTriggers(bgPtr);
	if ((exitCode == 0) && (bgPtr->stdoutSink.doneVar == NULL)) {
	    /* Return the output of the command */
	    Tcl_SetResult(interp, GetSinkData(&(bgPtr->stdoutSink)),
		TCL_VOLATILE);
	}
	/* Clean up resources used. */
	DestroyBackgroundInfo(bgPtr);
	if (exitCode != 0) {
	    Tcl_AppendResult(interp, "child process exited abnormally",
		(char *)NULL);
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
  error:
    DisableTriggers(bgPtr);
    DestroyBackgroundInfo(bgPtr);
    return TCL_ERROR;
}

static void
BgexecCmdDeleted(clientData)
    ClientData clientData;
{
#ifdef WIN32
    if (initialized) {
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch cursor;
	HANDLE hProcess;

	for (hPtr = Tcl_FirstHashEntry(&pidTable, &cursor); hPtr != NULL;
	    hPtr = Tcl_NextHashEntry(&cursor)) {
	    hProcess = (HANDLE) Tcl_GetHashKey(&pidTable, hPtr);
	    kill((int)hProcess, SIGKILL);
	}
	Tcl_DeleteHashTable(&pidTable);
	initialized = FALSE;
    }
#endif
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_BgexecInit --
 *
 *	This procedure is invoked to initialize the "bgexec" Tcl
 *	command.  See the user documentation for details on what it
 *	does.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */
int
Blt_BgexecInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"bgexec", BgexecCmd, BgexecCmdDeleted};

#ifdef WIN32
    if (!initialized) {
	Tcl_InitHashTable(&pidTable, TCL_ONE_WORD_KEYS);
	initialized = TRUE;
    }
#endif
    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_BGEXEC */
