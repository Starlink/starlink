
/*
 * bltWinPipe.c --
 *
 *	Lifted from tclPipe.c and tclWinPipe.c in the Tcl
 *	distribution, this is the first step toward freedom from the
 *	tyranny of the former Tcl_CreatePipeline API.
 *
 *	This file contains the generic portion of the command channel
 *	driver as well as various utility routines used in managing
 *	subprocesses.
 *
 *	[It's not clear why we needed a whole new API for I/O. Channels
 *	are one of the few losing propositions in Tcl. While it's easy
 *	to see that one needs to handle the different platform I/O
 *	semantics in a coherent fashion, it's usually better to pick
 *	an API from one of platforms (hopefully a mature, well-known model)
 *	and crowbar the other platforms to follow that.  At least then
 *	you're working from a known set of sematics. With Tcl Channels,
 *	no one's an expert and the interface is incomplete.]
 *
 * Copyright (c) 1997 by Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

/*
 * Todo:
 *	Get list of DOS commands, so we don't call cmd.exe unless we need to.
 *	Test win95
 *	Does terminating bltwish kill child processes?
 *	Handle EOL translation more cleanly.
 */

#include "bltInt.h"
#include <fcntl.h>

#ifdef HAVE_WAITFLAGS_H
#   include <waitflags.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#   include <sys/wait.h>
#endif

#define PEEK_DEBUG 0
#define QUEUE_DEBUG 0
#define READER_DEBUG 0
#define ASYNC_DEBUG 0
#define KILL_DEBUG 0

/*
 * The following defines identify the various types of applications that
 * run under windows.  There is special case code for the various types.
 */

#define APPL_NONE	0
#define APPL_DOS	1
#define APPL_WIN3X	2
#define APPL_WIN32	3
#define APPL_INTERP	4

#ifndef IMAGE_OS2_SIGNATURE 
#   define IMAGE_OS2_SIGNATURE    (0x454E)
#endif
#ifndef IMAGE_VXD_SIGNATURE 
#   define IMAGE_VXD_SIGNATURE    (0x454C)
#endif

#define PIPE_PENDING	(1<<13)	/* Message is pending in the queue. */
#define PIPE_EOF	(1<<14)	/* Pipe has reached EOF. */

#define PIPE_BUFSIZ	(BUFSIZ*2)	/* Size of pipe read buffer. */

typedef struct PipeHandler {
    int flags;			/* State flags, see above for a list. */
    HANDLE hPipe;		/* Pipe handle */
    HANDLE thread;		/* Thread watching I/O on this pipe. */
    HANDLE parent;		/* Handle of main thread. */
    DWORD parentId;		/* Handle of main thread. */
    HWND hWindow;		/* Notifier window in main thread. Used to
				 * wake up notifier system that an event
				 * has occurred that it needs to process. */
    HANDLE idleEvent;		/* Signals that the pipe is idle (no one
				 *  is reading/writing from it). */
    HANDLE readyEvent;		/* Signals that the pipe is ready for
				 * the next I/O operation. */

    DWORD lastError;		/* Error. */

    int start, end;		/* Pointers into the output buffer */
    char *buffer;		/* Current background output buffer. */
    unsigned int size;		/* Size of buffer. */

    Tcl_FileProc *proc;
    ClientData clientData;

} PipeHandler;


typedef struct PipeEvent {
    Tcl_Event header;		/* Information that is standard for
				 * all events. */

    PipeHandler *pipePtr;	/* Pointer to pipe handler structure.
				 * Note that we still have to verify
				 * that the pipe exists before
				 * dereferencing this pointer.
				 */
} PipeEvent;

static int initialized = 0;
static int pending = 0;
static Blt_List pipeList;
static CRITICAL_SECTION pipeCriticalSection;

static DWORD WINAPI PipeWriterThread(void *clientData);
static DWORD WINAPI PipeReaderThread(void *clientData);

extern void Blt_MapPid(HANDLE hProcess, DWORD pid);
extern HINSTANCE TclWinGetTclInstance(void);
extern void TclWinConvertError(DWORD lastError);

/*
 *----------------------------------------------------------------------
 *
 * NotifierWindowProc --
 *
 *	This procedure is invoked by Windows to process events on
 *	the notifier window.  Messages will be sent to this window
 *	in response to external timer events or calls to
 *	TclpAlertTsdPtr->
 *
 * Results:
 *	A standard windows result.
 *
 * Side effects:
 *	Services any pending events.
 *
 *----------------------------------------------------------------------
 */
static LRESULT CALLBACK
NotifierWindowProc(
    HWND hWindow,
    UINT message,
    WPARAM wParam,
    LPARAM lParam)
{
    switch (message) {
    case WM_USER:
	pending = 0;
	break;

    case WM_TIMER:
	break;

    default:
	return DefWindowProc(hWindow, message, wParam, lParam);
    }

    Tcl_ServiceAll();		/* Process all of the runnable events. */
    return 0;
}

static void
WakeupNotifier(HWND hWindow)
{
    if (!pending) {
	if (PostMessage(hWindow, WM_USER, 0, 0)) {
	    pending = TRUE;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetNotifierWindow --
 *
 *	Initializes the platform specific notifier state.
 *
 * Results:
 *	Returns a handle to the notifier state for this thread..
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static HWND
GetNotifierWindow(void)
{
    static HWND hWindow = NULL;
    /*
     * Register Notifier window class if this is the first thread to
     * use this module.
     */
    if (hWindow == NULL) {
	WNDCLASS class;
	HINSTANCE hInstance;

	memset(&class, 0, sizeof(WNDCLASS));
	hInstance = TclWinGetTclInstance();
	class.hInstance = hInstance;
	class.lpszClassName = "PipeNotifier";
	class.lpfnWndProc = NotifierWindowProc;

	if (!RegisterClassA(&class)) {
	    panic("Unable to register PipeNotifier window class");
	}
	/*
	 * Create a window for communication with the notifier.
	 */
	hWindow = CreateWindowA("PipeNotifier", "PipeNotifier", WS_TILED,
	    0, 0, 0, 0, NULL, NULL, hInstance, NULL);
    }
    return hWindow;
}

/*
 *----------------------------------------------------------------------
 *
 * PeekOnPipe --
 *
 *	See if some data is available, the pipe is at EOF, or the
 *	reader thread is currently blocked waiting for data.
 *
 * Results:
 *	Return TRUE if data is available, FALSE if otherwise.  Note
 *	that errors and EOF always return TRUE.  We always make the
 *	condition available until the caller handles it by deleting
 *	the pipe event handler.
 *
 *	On TRUE, the number of bytes returned indicates the following:
 *	  0	EOF.
 *	  -1	An error has occured or the thread is currently
 *		blocked reading.  In that last case, errno is set
 *		to EAGAIN.
 *        1+    Number of bytes of data in the buffer.
 *
 *----------------------------------------------------------------------
 */
static int
PeekOnPipe(pipePtr, numAvailPtr)
    PipeHandler *pipePtr;	/* Pipe state. */
    int *numAvailPtr;
{
    int state;

    *numAvailPtr = -1;
#if PEEK_DEBUG
    PurifyPrintf("PEEK: waiting for reader\n");
#endif
    state = WaitForSingleObject(pipePtr->readyEvent, 0);
#if PEEK_DEBUG
    PurifyPrintf("PEEK: state is %d\n", state);
#endif
    if (state == WAIT_TIMEOUT) {
#if PEEK_DEBUG
	PurifyPrintf("PEEK: try again, %d\n", state);
#endif
	errno = EAGAIN;
	return FALSE;		/* Reader thread is currently blocked. */
    }
    /*
     * At this point the two threads are synchronized. So it's safe
     * to access shared information.
     */
    if (state == WAIT_OBJECT_0) {
	int numAvail;

	numAvail = pipePtr->end - pipePtr->start;
#if PEEK_DEBUG
	PurifyPrintf("PEEK: Found %d bytes available\n", numAvail);
#endif
	if ((numAvail <= 0) && !(pipePtr->flags & PIPE_EOF)) {
	    TclWinConvertError(pipePtr->lastError);
#if PEEK_DEBUG
	    PurifyPrintf("PEEK: Error = %d\n", pipePtr->lastError);
#endif
	    numAvail = -1;
	}
	*numAvailPtr = numAvail;
    }
#if PEEK_DEBUG
    PurifyPrintf("PEEK: Reseting events\n");
#endif
    return TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeEventProc --
 *
 *	This function is invoked by Tcl_ServiceEvent when a file event
 *	reaches the front of the event queue.  This procedure calls back
 *	the handler procedure designated for this pipe.
 *
 * Results:
 *	Returns 1 if the event was handled, meaning it should be removed
 *	from the queue.  Returns 0 if the event was not handled, meaning
 *	it should stay on the queue.  The only time the event isn't
 *	handled is if the TCL_FILE_EVENTS flag bit isn't set.
 *
 * Side effects:
 *	Whatever the pipe handler callback does.
 *
 *----------------------------------------------------------------------
 */
static int
PipeEventProc(Tcl_Event * eventPtr, int flags)
{
    PipeHandler *pipePtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return 0;
    }
    pipePtr = ((PipeEvent *)eventPtr)->pipePtr;
    if (pipePtr == NULL) {
	return 1;
    }
    if (pipePtr->proc != NULL) {
	(*pipePtr->proc) (pipePtr->clientData, flags);
    }
    /* Allow more events again. */
    pipePtr->flags &= ~PIPE_PENDING;
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * SetupHandlers --
 *
 *	This procedure is invoked before Tcl_DoOneEvent blocks waiting
 *	for an event.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Adjusts the block time if needed.
 *
 *----------------------------------------------------------------------
 */
void
SetupHandlers(ClientData clientData, int flags)
{
    Blt_List *listPtr = (Blt_List *)clientData;
    register PipeHandler *pipePtr;
    register Blt_ListItem item;
    int dontBlock, numBytes;
    Tcl_Time blockTime;

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }
    /* 
     * Loop through the list of pipe handlers.  Check if any I/O
     * events are currently pending. 
     */
    dontBlock = FALSE;
    blockTime.sec = blockTime.usec = 0L;
#if QUEUE_DEBUG
    PurifyPrintf("SetupHandlers: before loop\n");
#endif
    for (item = Blt_ListFirstItem(listPtr); item != NULL;
	item = Blt_ListNextItem(item)) {
	pipePtr = (PipeHandler *)Blt_ListGetValue(item);
	if (pipePtr->flags & TCL_READABLE) {
	    if (PeekOnPipe(pipePtr, &numBytes)) {
		dontBlock = TRUE;
	    }
	}
	if (pipePtr->flags & TCL_WRITABLE) {
	    if (WaitForSingleObject(pipePtr->readyEvent, 0) != WAIT_TIMEOUT) {
		dontBlock = TRUE;
	    }
	}
    }
#if QUEUE_DEBUG
    PurifyPrintf("SetupHandlers: after loop\n");
#endif
    if (dontBlock) {
	Tcl_SetMaxBlockTime(&blockTime);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * CheckHandlers --
 *
 *	This procedure is called by Tcl_DoOneEvent to check the pipe
 *	event source for events.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	May queue an event.
 *
 *----------------------------------------------------------------------
 */
static void
CheckHandlers(ClientData clientData, int flags)
{
    Blt_List *listPtr = (Blt_List *)clientData;
    PipeHandler *pipePtr;
    register Blt_ListItem item;
    int queueEvent, numBytes;

    if ((flags & TCL_FILE_EVENTS) == 0) {
	return;
    }
    /*
     * Queue events for any ready pipes that don't already have events
     * queued.
     */
    for (item = Blt_ListFirstItem(listPtr); item != NULL;
	item = Blt_ListNextItem(item)) {
	pipePtr = (PipeHandler *)Blt_ListGetValue(item);
	if (pipePtr->flags & PIPE_PENDING) {
	    continue;		/* If this pipe already is scheduled to
				 * service an event, wait for it to handle
				 * it. */
	}
	/*
	 * Queue an event if the pipe is signaled for reading or writing.
	 */
	queueEvent = FALSE;
	if (pipePtr->flags & TCL_READABLE) {
	    if (PeekOnPipe(pipePtr, &numBytes)) {
		queueEvent = TRUE;
	    }
	}
	if (pipePtr->flags & TCL_WRITABLE) {
	    if (WaitForSingleObject(pipePtr->readyEvent, 0) != WAIT_TIMEOUT) {
		queueEvent = TRUE;
	    }
	}
#if QUEUE_DEBUG
	PurifyPrintf("Queue event is %d \n", queueEvent);
#endif
	if (queueEvent) {
	    PipeEvent *eventPtr;

	    pipePtr->flags |= PIPE_PENDING;
	    eventPtr = (PipeEvent *) malloc(sizeof(PipeEvent));
	    assert(eventPtr);
	    eventPtr->header.proc = PipeEventProc;
	    eventPtr->pipePtr = pipePtr;
	    Tcl_QueueEvent((Tcl_Event *) eventPtr, TCL_QUEUE_TAIL);
	}
    }
}

static PipeHandler *
CreatePipeHandler(hFile, flags)
    HANDLE hFile;
    int flags;
{
    DWORD id;
    PipeHandler *pipePtr;
    LPTHREAD_START_ROUTINE threadProc;

    pipePtr = (PipeHandler *) calloc(1, sizeof(PipeHandler));
    assert(pipePtr);

    pipePtr->hPipe = hFile;
    pipePtr->flags = flags;
    pipePtr->parentId = GetCurrentThreadId();
    pipePtr->parent = GetCurrentThread();
    pipePtr->hWindow = GetNotifierWindow();
    pipePtr->readyEvent = CreateEvent(
	NULL,			/* Security attributes. */
	TRUE,			/* Manual reset event */
	FALSE,			/* Initially not signaled. */
	NULL);			/* Event object's name. */
    pipePtr->idleEvent = CreateEvent(
	NULL,			/* Security attributes. */
	FALSE,			/* Auto reset event. */
	TRUE,			/* Initially signaled. */
	NULL);			/* Event object's name. */

    if (flags & TCL_READABLE) {
	threadProc = (LPTHREAD_START_ROUTINE)PipeReaderThread;
	pipePtr->buffer = (char *)calloc(1, PIPE_BUFSIZ);
	pipePtr->size = PIPE_BUFSIZ;
    } else {
	threadProc = (LPTHREAD_START_ROUTINE) PipeWriterThread;
    }

    pipePtr->thread = CreateThread(
	NULL,			/* Security attributes */
	8000,			/* Initial stack size. */
	threadProc,		/* Starting address of thread routine */
	(DWORD *) pipePtr,	/* One-word of data passed to routine. */
	0,			/* Creation flags */
	&id);			/* (out) Will contain Id of new thread. */
    return pipePtr;
}

static void
DestroyPipeHandler(PipeHandler * pipePtr)
{
    DWORD status;

#if KILL_DEBUG
    PurifyPrintf("DestroyPipeHandler");
#endif
    if ((pipePtr->flags & TCL_WRITABLE) &&
	(pipePtr->hPipe != INVALID_HANDLE_VALUE)) {
	/* Wait for the writer thread to finish with the current buffer */
	WaitForSingleObject(pipePtr->idleEvent, INFINITE);
    }
    if (pipePtr->hPipe != INVALID_HANDLE_VALUE) {
	CloseHandle(pipePtr->hPipe);
    }
    CloseHandle(pipePtr->readyEvent);
    CloseHandle(pipePtr->idleEvent);
    /*
     * The thread should terminate once the pipe is closed, if
     * it hasn't already. We'll still check.
     */
    /* Terminate the thread and close all handles. */
    status = (DWORD)-1;
#ifdef notdef
    if ((!GetExitCodeThread(pipePtr->thread, &status)) ||
	(status == STILL_ACTIVE)) {
	/* This isn't a good thing. It causes too many things to break. */
	TerminateThread(pipePtr->thread, 0);
    }
#endif
    CloseHandle(pipePtr->thread);

    if (pipePtr->buffer != NULL) {
	free((char *)pipePtr->buffer);
    }
    free((char *)pipePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * PipeInit --
 *
 *	This function initializes the static variables for this file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates a new event source.
 *
 *----------------------------------------------------------------------
 */
static void
PipeInit(void)
{
    initialized = TRUE;
    InitializeCriticalSection(&pipeCriticalSection);
    Blt_InitList(&pipeList, TCL_ONE_WORD_KEYS);
    Tcl_CreateEventSource(SetupHandlers, CheckHandlers, (ClientData)&pipeList);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_PipeTeardown --
 *
 *	This function releases any storage allocated for this file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates a new event source.
 *
 *----------------------------------------------------------------------
 */
void
Blt_PipeTeardown(void)
{
    register Blt_ListItem item;
    PipeHandler *pipePtr;

    if (!initialized) {
	return;			/* Was never initialized. */
    }
    initialized = FALSE;
    EnterCriticalSection(&pipeCriticalSection);
    for (item = Blt_ListFirstItem(&pipeList); item != NULL;
	item = Blt_ListNextItem(item)) {
	pipePtr = (PipeHandler *) Blt_ListGetValue(item);
	if (pipePtr != NULL) {
	    DestroyPipeHandler(pipePtr);
	}
    }

    DestroyWindow(GetNotifierWindow());
    UnregisterClassA("PipeNotifier", TclWinGetTclInstance());

    Blt_ListReset(&pipeList);
    LeaveCriticalSection(&pipeCriticalSection);
    Tcl_DeleteEventSource(SetupHandlers, CheckHandlers, (ClientData)&pipeList);
    DeleteCriticalSection(&pipeCriticalSection);
}

/*
 *----------------------------------------------------------------------
 *
 * PipeReaderThread --
 *
 *	This function runs in a separate thread and waits for input
 *	to become available on a pipe.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Signals the main thread when input become available.  May
 *	cause the main thread to wake up by posting a message.
 *
 *----------------------------------------------------------------------
 */
static DWORD WINAPI
PipeReaderThread(void *clientData)
{
    PipeHandler *pipePtr = (PipeHandler *) clientData;
    DWORD count;
    BOOL result;

    for (;;) {
	/*
	 * Synchronize with the main thread, so that we don't try to
	 * read from the pipe while it's copying to the buffer.
	 */
#if READER_DEBUG
	PurifyPrintf("READER: waiting");
#endif
	WaitForSingleObject(pipePtr->idleEvent, INFINITE);
#if READER_DEBUG
	PurifyPrintf("READER: ok");
#endif
	/*
	 * Read from the pipe. The thread will be blocked here until
	 * some data has been read into its buffer.
	 */
#if READER_DEBUG
	PurifyPrintf("READER: before read");
#endif
	assert(pipePtr->start == pipePtr->end);
	result = ReadFile(
	    pipePtr->hPipe,	/* Handle to anonymous pipe. */
	    pipePtr->buffer,	/* Data buffer. */
	    pipePtr->size,	/* Requested number of bytes (the size
				 * of the buffer) */
	    &count,		/* (out) Number of bytes actually read. */
	    NULL);		/* Overlapping I/O */

#if READER_DEBUG
	PurifyPrintf("READER: after read. status=%d, count=%d", result, count);
#endif
	/*
	 * Reset counters to indicate that the buffer has been refreshed.
	 */
	pipePtr->start = 0;
	pipePtr->end = count;
	if (count == 0) {
	    /* We've hit EOF or an error. */
	    pipePtr->lastError = GetLastError();
	    if ((pipePtr->lastError == ERROR_BROKEN_PIPE) ||
		(pipePtr->lastError == ERROR_HANDLE_EOF)) {
		pipePtr->flags |= PIPE_EOF;
	    }
#if READER_DEBUG
	    PurifyPrintf("READER: error is %s", Blt_Win32Error());
#endif
	}
	WakeupNotifier(pipePtr->hWindow);
	SetEvent(pipePtr->readyEvent);
	if (count == 0) {
#if READER_DEBUG
	    PurifyPrintf("READER: exiting\n");
#endif
	    ExitThread(0);
	}
    }
    /* NOTREACHED */
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeWriterThread --
 *
 *	This function runs in a separate thread and writes data
 *	to the process' standard input pipe.
 *
 * Results:
 *	Always returns 0.
 *
 * Side effects:
 *	Signals the main thread when an output operation is completed.
 *	May cause the main thread to wake up by posting a message.
 *
 *----------------------------------------------------------------------
 */
static DWORD WINAPI
PipeWriterThread(void *clientData)
{
    PipeHandler *pipePtr = (PipeHandler *) clientData;
    DWORD count, bytesLeft;
    register char *ptr;

    for (;;) {
	/*
	 * Synchronize with the main thread, so that we don't test the pipe
	 * until its done writing.
	 */
	WaitForSingleObject(pipePtr->idleEvent, INFINITE);

	ptr = pipePtr->buffer;
	bytesLeft = pipePtr->end;

	/*
	 * Loop until all of the bytes are written or an error occurs.
	 */
	while (bytesLeft > 0) {
	    if (!WriteFile(pipePtr->hPipe, ptr, bytesLeft, &count, NULL)) {
		pipePtr->lastError = GetLastError();
		break;
	    }
	    bytesLeft -= count;
	    ptr += count;
	}
	/*
	 * Tell the main thread that data can be written to the pipe.
	 * Remember to wake up the notifier thread.
	 */
	SetEvent(pipePtr->readyEvent);
	WakeupNotifier(pipePtr->hWindow);
    }
    /* NOTREACHED */
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * TempFileName --
 *
 *	Gets a temporary file name and deals with the fact that the
 *	temporary file path provided by Windows may not actually exist
 *	if the TMP or TEMP environment variables refer to a
 *	non-existent directory.
 *
 * Results:
 *	0 if error, non-zero otherwise.  If non-zero is returned, the
 *	name buffer will be filled with a name that can be used to
 *	construct a temporary file.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
TempFileName(name)
    char name[];		/* (out) Buffer to hold name of temporary file. */
{
    if ((GetTempPath(MAX_PATH, name) > 0) &&
	(GetTempFileName(name, "TCL", 0, name))) {
	return 1;
    }
    /* Bail out and use the current working directory. */
    return GetTempFileName(".", "TCL", 0, name);
}

/*
 *----------------------------------------------------------------------
 *
 * OpenRedirectFile --
 *
 *	Open a file for use in a pipeline.
 *
 * Results:
 *	Returns a new handle or NULL on failure.
 *
 * Side effects:
 *	May cause a file to be created on the file system.
 *
 *----------------------------------------------------------------------
 */
static HANDLE
OpenRedirectFile(const char *path, DWORD accessFlags, DWORD createFlags)
{
    HANDLE hFile;
    DWORD attribFlags;
    int useExisting;

    attribFlags = 0;
    useExisting = (createFlags & (TRUNCATE_EXISTING | OPEN_EXISTING));
    if (useExisting) {
	attribFlags = GetFileAttributes(path);
	if (attribFlags == 0xFFFFFFFF) {
	    attribFlags = 0;
	}
    }
    hFile = CreateFile(path,
	accessFlags,		/* Access mode flags */
	FILE_SHARE_READ | FILE_SHARE_WRITE,
	NULL,			/* No security */
	createFlags,		/* Creation attributes */
	attribFlags,		/* File attribute flags */
	NULL);			/* Template file */

    if (hFile == INVALID_HANDLE_VALUE) {
	DWORD lastError;

	lastError = GetLastError();
	if ((lastError & 0xffffL) == ERROR_OPEN_FAILED) {
	    lastError = (useExisting)
		? ERROR_FILE_NOT_FOUND : ERROR_FILE_EXISTS;
	}
	TclWinConvertError(lastError);
	return INVALID_HANDLE_VALUE;
    }
    /*
     * Seek to the end of file if we are writing.
     */
    if (createFlags & GENERIC_WRITE) {
	SetFilePointer(hFile, 0, NULL, FILE_END);
    }
    return hFile;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateTempFile --
 *
 *	This function creates a temporary file initialized with an
 *	optional string, and returns a file handle with the file pointer
 *	at the beginning of the file.
 *
 * Results:
 *	A handle to a file.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static HANDLE
CreateTempFile(const char *data)
/* data		String to write into temp file, or NULL. */
{
    char fileName[MAX_PATH + 1];
    HANDLE hFile;
    DWORD lastError;

    if (!TempFileName(fileName)) {
	return INVALID_HANDLE_VALUE;
    }
    hFile = CreateFile(
	fileName,		/* File path */
	GENERIC_READ | GENERIC_WRITE,	/* Access mode */
	0,			/* No sharing. */
	NULL,			/* Security attributes */
	CREATE_ALWAYS,		/* Overwrite any existing file */
	FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE,
	NULL);			/* No template file */

    if (hFile == INVALID_HANDLE_VALUE) {
	goto error;
    }
    if (data != NULL) {
	DWORD result, length;
	const char *p;
	const char *string;

	string = data;
	for (p = string; *p != '\0'; p++) {
	    if (*p == '\n') {
		length = p - string;
		if (length > 0) {
		    if (!WriteFile(hFile, string, length, &result, NULL)) {
			goto error;
		    }
		}
		if (!WriteFile(hFile, "\r\n", 2, &result, NULL)) {
		    goto error;
		}
		string = p + 1;
	    }
	}
	length = p - string;
	if (length > 0) {
	    if (!WriteFile(hFile, string, length, &result, NULL)) {
		goto error;
	    }
	}
	if (SetFilePointer(hFile, 0, NULL, FILE_BEGIN) == (DWORD)-1) {
	    goto error;
	}
    }
    return hFile;

  error:
    lastError = GetLastError();
    CloseHandle(hFile);
    DeleteFile(fileName);	/* Do I need this? Delete on close? */
    TclWinConvertError(lastError);
    return INVALID_HANDLE_VALUE;
}

/*
 *----------------------------------------------------------------------
 *
 * HasConsole --
 *
 *	Determines whether the current application is attached to a
 *	console.
 *
 * Results:
 *	Returns TRUE if this application has a console, else FALSE.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static BOOL
HasConsole(void)
{
    HANDLE hFile;

    hFile = CreateFileA("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
	OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
	return FALSE;
    }
    CloseHandle(hFile);
    return TRUE;
}


static int
GetApplicationType(const char *file, char *cmdPrefix)
{
    char *dot;
    HANDLE hFile;
    IMAGE_DOS_HEADER imageDosHeader;
    ULONG signature;
    BOOL result;
    DWORD offset;
    DWORD numBytes;
    int type;

    dot = strrchr(file, '.');
    if ((dot != NULL) && (_stricmp(dot, ".bat") == 0)) {
	return APPL_DOS;
    }
    /* Work a little harder. Open the binary and read the header */
    hFile = CreateFileA(file, GENERIC_READ, FILE_SHARE_READ, NULL,
	OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
	return APPL_NONE;
    }
    type = APPL_NONE;
    result = ReadFile(hFile, &imageDosHeader, sizeof(IMAGE_DOS_HEADER),
	&numBytes, NULL);
    if ((!result) || (numBytes != sizeof(IMAGE_DOS_HEADER))) {
	goto done;
    }
#if KILL_DEBUG
    PurifyPrintf("magic number is %x\n", imageDosHeader.e_magic);
#endif
    if (imageDosHeader.e_magic == 0x2123) {	/* #! */
	register char *p;
	register unsigned int i;

	offset = SetFilePointer(hFile, 2, NULL, FILE_BEGIN);
	if (offset == (DWORD)-1) {
	    goto done;
	}
	result = ReadFile(hFile, cmdPrefix, MAX_PATH + 1, &numBytes, NULL);
	if ((!result) || (numBytes < 1)) {
	    goto done;
	}
	for (p = cmdPrefix, i = 0; i < numBytes; i++, p++) {
	    if ((*p == '\n') || (*p == '\r')) {
		break;
	    }
	}
	*p = '\0';
	type = APPL_INTERP;
	goto done;
    }
    /*
     * Doesn't have the magic number for relocatable executables.  If
     * filename ends with .com, assume it's a DOS application anyhow.
     * Note that we didn't make this assumption at first, because some
     * supposed .com files are really 32-bit executables with all the
     * magic numbers and everything.
     */
    if ((dot != NULL) && (strcmp(dot, ".com") == 0)) {
#if KILL_DEBUG
	PurifyPrintf(".com\n");
#endif
	type = APPL_DOS;
	goto done;
    }
    if (imageDosHeader.e_magic != IMAGE_DOS_SIGNATURE) {
#if KILL_DEBUG
	PurifyPrintf("Application doesn't have correct sig?\n");
#endif
    }
    if (imageDosHeader.e_lfarlc != sizeof(IMAGE_DOS_HEADER)) {
	/* This assumes that all 3.x and Win32 programs have their
	 * file relocation table immediately following this header. */
	/*
	 * All Windows 3.X and Win32 and some DOS programs have this value
	 * set here.  If it doesn't, assume that since it already had the
	 * other magic number it was a DOS application.
	 */
#if KILL_DEBUG
	PurifyPrintf("wrong reloc table address\n");
#endif
	type = APPL_DOS;
	goto done;
    }
    offset = SetFilePointer(hFile, imageDosHeader.e_lfanew, NULL, FILE_BEGIN);
    if (offset == (DWORD)-1) {
	goto done;
    }
    result = ReadFile(hFile, &signature, sizeof(ULONG), &numBytes, NULL);
    if ((!result) || (numBytes != sizeof(ULONG))) {
	goto done;
    }
#if KILL_DEBUG
    PurifyPrintf("signature is %x\n", signature);
#endif
    switch (signature) {
    case IMAGE_NT_SIGNATURE:
	type = APPL_WIN32;
	break;
    case IMAGE_OS2_SIGNATURE:
	type = APPL_WIN3X;
	break;
    case IMAGE_VXD_SIGNATURE:
	type = APPL_WIN32;
	break;
    default:
	type = APPL_DOS;
	break;
    }
  done:
    CloseHandle(hFile);
    return type;
}

/*
 *----------------------------------------------------------------------
 *
 * GetFullPath --
 *
 *	Look for the program as an external program.  First try the
 *	name as it is, then try adding .com, .exe, and .bat, in that
 *	order, to the name, looking for an executable.
 *
 *	Using the raw SearchPath() procedure doesn't do quite what is
 *	necessary.  If the name of the executable already contains a
 *	'.'  character, it will not try appending the specified
 *	extension when searching (in other words, SearchPath will
 *	not find the program "a.b.exe" if the arguments specified
 *	"a.b" and ".exe").  So, first look for the file as it is
 *	named.  Then manually append * the extensions, looking for a
 *	match.
 *
 * Results:
 *	Always returns TCL_OK.
 *
 * Side Effects:
 *
 *----------------------------------------------------------------------
 */
static int
GetFullPath(
    Tcl_Interp *interp,		/* Interpreter to report errors to */
    const char *name,		/* Name of program. */
    char *fullPath,		/* (out) Returned full path. */
    char *cmdPrefix,		/* (out) If program is a script, this contains
				 * the name of the interpreter. */
    int *typePtr)
{				/* (out) Type of program */
    BOOL found;
    TCHAR *rest;
    DWORD attr;
    int length;
    char progName[MAX_PATH + 5];
    const char **ext;

    static char *extensionList[] =
    {
	"", ".com", ".exe", ".bat", NULL
    };

    *typePtr = APPL_NONE;

    length = strlen(name);
    strcpy(progName, name);
    cmdPrefix[0] = '\0';
    for (ext = extensionList; *ext != NULL; ext++) {
	progName[length] = '\0';
	strcat(progName, *ext);

	found = SearchPath(
	    NULL,		/* Use standard Windows search paths */
	    progName,		/* Program name */
	    NULL,		/* Extension provided by program name. */
	    MAX_PATH,		/* Buffer size */
	    fullPath,		/* Buffer for absolute path of program */
	    &rest);		/*  */

	if (!found) {
	    continue;		/* Can't find program with that extension */
	}
	/*
	 * Ignore matches on directories or data files, return if
	 * identified a known type.
	 */
	attr = GetFileAttributesA(fullPath);
	if ((attr == (DWORD)-1) || (attr & FILE_ATTRIBUTE_DIRECTORY)) {
	    continue;
	}
	*typePtr = GetApplicationType(fullPath, cmdPrefix);
	if (*typePtr != APPL_NONE) {
	    break;
	}
    }
    if (*typePtr == APPL_NONE) {
	/*
	 * Can't find the program.  Might still be a DOS command like
	 * "copy" or "dir", so let cmd.exe deal with it.
	 */
#ifndef notdef
	register char **p;
	static char *dosCmds[] =
	{
	    "copy", "del" "dir", "echo", "edit", "erase", "label",
	    "md", "rd", "ren", "time", "type", "ver", "vol", NULL
	};
	for (p = dosCmds; *p != NULL; p++) {
	    if (((*p)[0] == name[0]) && (strcmp(*p, name) == 0)) {
		break;
	    }
	}
	if (*p == NULL) {
	    Tcl_AppendResult(interp, "can't find a command \"", name,
		"\" to execute", (char *)NULL);
	    return TCL_ERROR;
	}
#endif
	*typePtr = APPL_DOS;
	strcpy(fullPath, name);
    }
    if ((*typePtr == APPL_DOS) || (*typePtr == APPL_WIN3X)) {
	/*
	 * Replace long path name of executable with short path name for
	 * 16-bit applications.  Otherwise the application may not be able
	 * to correctly parse its own command line to separate off the
	 * application name from the arguments.
	 */
	GetShortPathName(fullPath, fullPath, MAX_PATH);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ConcatCmdArgs --
 *
 *	Concatonates command line arguments parsed from Tcl into a
 *	single string.  If an argument contain spaces, it is grouped
 *	with surrounding double quotes. Must also escape any quotes we
 *	find.
 *
 * Results:
 *	Returns a malloc-ed string containing the concatonated command
 *	line.
 *
 *----------------------------------------------------------------------
 */
static char *
ConcatCmdArgs(int argc, char **argv)
{
    BOOL needQuote;
    register const char *s;
    register char *cp;
    char *string;		/* Will contain the new command line */
    register int count;
    register int i;

    /*
     * Pass 1.	Compute how much space we need for an array hold the entire
     *		command line.  Then allocate the string.
     */
    count = 0;
    for (i = 0; i < argc; i++) {
	needQuote = FALSE;
	if (*argv[i] == '\0') {
	    needQuote = TRUE;	/* Zero length args also need quotes. */
	}
	for (s = argv[i]; *s != '\0'; s++) {
	    if (*s == '"') {
		register const char *bp;

		count++;	/* +1 Backslash needed to escape quote */
		for (bp = s - 1; (*bp == '\\') && (bp >= argv[i]); bp--) {
		    count++;	/* +? one for each preceding backslash */
		}
	    } else if (isspace(*s)) {
		needQuote = TRUE;
	    }
	    count++;		/* +1 Normal character */
	}
	if (needQuote) {
	    count += 2;		/* +2 Pair of quotes */
	}
	count++;		/* +1 Space separating arguments */
    }

    string = (char *)malloc(count + 1);
    assert(string);

    /*
     * Pass 2.	Copy the arguments, quoting arguments with embedded spaces and
     *		escaping all other quotes in the string.
     */
    cp = string;
    for (i = 0; i < argc; i++) {
	needQuote = FALSE;

	if (*argv[i] == '\0') {
	    needQuote = TRUE;
	}
	for (s = argv[i]; *s != '\0'; s++) {
	    if (isspace(*s)) {
		needQuote = TRUE;
	    }
	}
	if (needQuote) {
	    *cp++ = '"';
	}
	for (s = argv[i]; *s; s++) {
	    if (*s == '"') {
		register const char *bp;

		for (bp = s - 1; (*bp == '\\') && (bp >= argv[i]); bp--) {
		    *cp++ = '\\';
		}
		*cp++ = '\\';
	    }
	    *cp++ = *s;
	}
	if (needQuote) {
	    *cp++ = '"';
	}
	*cp++ = ' ';
    }
    *cp = '\0';
    assert((cp - string) == count);
    return string;
}

/*
 *----------------------------------------------------------------------
 *
 * StartProcess --
 *
 *	Create a child process that has the specified files as its
 *	standard input, output, and error.
 *
 *	The complete Windows search path is searched to find the specified
 *	executable.  If an executable by the given name is not found,
 *	automatically tries appending ".com", ".exe", and ".bat" to the
 *	executable name.
 *
 * Results:
 *	The return value is TCL_ERROR and an error message is left in
 *	the interp's result if there was a problem creating the child
 *	process.  Otherwise, the return value is TCL_OK and *pidPtr is
 *	filled with the process id of the child process.
 *
 * Side effects:
 *	A process is created.
 *
 *----------------------------------------------------------------------
 */
static int
StartProcess(
    Tcl_Interp *interp,		/* Interpreter to report errors that
				 * occurred when creating the child process.
				 * Error messages from the child process
				 * itself are sent to errorFile. */
    int argc,			/* Number of arguments. */
    char **argv,		/* Command line arguments. */
    HANDLE hStdin,		/* File handle to use as input (stdin) for the
				 * child process. If handle is -1, no
				 * standard input. */
    HANDLE hStdout,		/* File handle to receive output (stdout)
				 * from the child process.  If -1, output
				 * is discarded. */
    HANDLE hStderr,		/* File handle to receive errors (stderr)
				 * from the child process.  If -1, stderr
				 * will be discarded. Can be the same handle
				 * as hStdOut */
    HANDLE * hProcessPtr)
{				/* (out) Handle of child process. */
    int result, applType, createFlags;
    Tcl_DString dString;	/* Complete command line */
    char *command;
    BOOL hasConsole;
    DWORD idleResult;
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    SECURITY_ATTRIBUTES securityAttrs;
    HANDLE hProcess, hPipe;
    char progPath[MAX_PATH];
    char cmdPrefix[MAX_PATH];

    *hProcessPtr = INVALID_HANDLE_VALUE;
    GetFullPath(interp, argv[0], progPath, cmdPrefix, &applType);
#if KILL_DEBUG
    PurifyPrintf("Application type is %d\n", applType);
#endif
    if (applType == APPL_NONE) {
	return TCL_ERROR;
    }
    result = TCL_ERROR;

    hProcess = GetCurrentProcess();

    /*
     * STARTF_USESTDHANDLES must be used to pass handles to child process.
     * Using SetStdHandle() and/or dup2() only works when a console mode
     * parent process is spawning an attached console mode child process.
     */
    ZeroMemory(&si, sizeof(STARTUPINFOA));
    si.cb = sizeof(STARTUPINFOA);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdInput = si.hStdOutput = si.hStdError = INVALID_HANDLE_VALUE;

    securityAttrs.nLength = sizeof(SECURITY_ATTRIBUTES);
    securityAttrs.lpSecurityDescriptor = NULL;
    securityAttrs.bInheritHandle = TRUE;

    /*
     * Duplicate all the handles which will be passed off as stdin, stdout
     * and stderr of the child process. The duplicate handles are set to
     * be inheritable, so the child process can use them.
     */
    if (hStdin == INVALID_HANDLE_VALUE) {
	/*
	 * If handle was not set, stdin should return immediate EOF.
	 * Under Windows95, some applications (both 16 and 32 bit!)
	 * cannot read from the NUL device; they read from console
	 * instead.  When running tk, this is fatal because the child
	 * process would hang forever waiting for EOF from the unmapped
	 * console window used by the helper application.
	 *
	 * Fortunately, the helper application detects a closed pipe
	 * as an immediate EOF and can pass that information to the
	 * child process.
	 */
	if (CreatePipe(&si.hStdInput, &hPipe, &securityAttrs, 0)) {
	    CloseHandle(hPipe);
	}
    } else {
	DuplicateHandle(hProcess, hStdin, hProcess, &si.hStdInput, 0, TRUE,
	    DUPLICATE_SAME_ACCESS);
    }

    if (si.hStdInput == INVALID_HANDLE_VALUE) {
	Tcl_AppendResult(interp, "can't duplicate input handle: ", 
		Blt_Win32Error(), (char *)NULL);
	goto closeHandles;
    }
    if (hStdout == INVALID_HANDLE_VALUE) {
	/*
	 * If handle was not set, output should be sent to an infinitely
	 * deep sink.  Under Windows 95, some 16 bit applications cannot
	 * have stdout redirected to NUL; they send their output to
	 * the console instead.  Some applications, like "more" or "dir /p",
	 * when outputting multiple pages to the console, also then try and
	 * read from the console to go the next page.  When running tk, this
	 * is fatal because the child process would hang forever waiting
	 * for input from the unmapped console window used by the helper
	 * application.
	 *
	 * Fortunately, the helper application will detect a closed pipe
	 * as a sink.
	 */
	if ((Blt_GetPlatformId() == VER_PLATFORM_WIN32_WINDOWS)
	    && (applType == APPL_DOS)) {
	    if (CreatePipe(&hPipe, &si.hStdOutput, &securityAttrs, 0)) {
		CloseHandle(hPipe);
	    }
	} else {
	    si.hStdOutput = CreateFileA("NUL:", GENERIC_WRITE, 0,
		&securityAttrs, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
    } else {
	DuplicateHandle(hProcess, hStdout, hProcess, &si.hStdOutput, 0, TRUE,
	    DUPLICATE_SAME_ACCESS);
    }
    if (si.hStdOutput == INVALID_HANDLE_VALUE) {
	Tcl_AppendResult(interp, "can't duplicate output handle: ", 
		Blt_Win32Error(), (char *)NULL);
	goto closeHandles;
    }
    if (hStderr == INVALID_HANDLE_VALUE) {
	/*
	 * If handle was not set, errors should be sent to an infinitely
	 * deep sink.
	 */
	si.hStdError = CreateFileA("NUL:", GENERIC_WRITE, 0,
	    &securityAttrs, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    } else {
	DuplicateHandle(hProcess, hStderr, hProcess, &si.hStdError, 0, TRUE,
	    DUPLICATE_SAME_ACCESS);
    }
    if (si.hStdError == INVALID_HANDLE_VALUE) {
	Tcl_AppendResult(interp, "can't duplicate error handle: ", 
			 Blt_Win32Error(), (char *)NULL);
	goto closeHandles;
    }
    Tcl_DStringInit(&dString);
    createFlags = 0;
    hasConsole = HasConsole();
    if (!hasConsole) {
	createFlags |= DETACHED_PROCESS;
    }
    /*
     * If we do not have a console window, then we must run DOS and
     * WIN32 console mode applications as detached processes. This tells
     * the loader that the child application should not inherit the
     * console, and that it should not create a new console window for
     * the child application.  The child application should get its stdio
     * from the redirection handles provided by this application, and run
     * in the background.
     *
     * If we are starting a GUI process, they don't automatically get a
     * console, so it doesn't matter if they are started as foreground or
     * detached processes.  The GUI window will still pop up to the
     * foreground.
     */
    if (applType == APPL_DOS) {
	if (Blt_GetPlatformId() == VER_PLATFORM_WIN32_NT) {
	    /*
	     * Under NT, 16-bit DOS applications will not run unless they
	     * can be attached to a console.  If we are running without a
	     * console, run the 16-bit program as an normal process inside
	     * of a hidden console application, and then run that hidden
	     * console as a detached process.
	     */
	    si.wShowWindow = SW_HIDE;
	    si.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    Tcl_DStringAppend(&dString, "cmd.exe /c ", -1);
	} else {
	    /*
	     * Under Windows 95, 16-bit DOS applications do not work well
	     * with pipes:
	     *
	     * 1. EOF on a pipe between a detached 16-bit DOS application
	     *	  and another application is not seen at the other
	     *	  end of the pipe, so the listening process blocks forever on
	     *	  reads.  This inablity to detect EOF happens when either a
	     *    16-bit app or the 32-bit app is the listener.
	     *
	     * 2. If a 16-bit DOS application (detached or not) blocks when
	     *    writing to a pipe, it will never wake up again, and it
	     *    eventually brings the whole system down around it.
	     *
	     * The 16-bit application is run as a normal process
	     * inside of a hidden helper console app, and this helper
	     * may be run as a detached process.  If a stdio handle is
	     * a pipe, the helper application accumulates information
	     * into temp files and forwards it to or from the DOS
	     * application as appropriate.  This means that DOS apps
	     * must receive EOF from a stdin pipe before they will
	     * actually begin, and must finish generating stdout or
	     * stderr before the data will be sent to the next stage
	     * of the pipe.
	     *
	     * The helper app should be located in the same directory
	     * as the tcl dll.
	     */
	    if (!hasConsole) {
		si.wShowWindow = SW_HIDE;
		si.dwFlags |= STARTF_USESHOWWINDOW;
		createFlags = CREATE_NEW_CONSOLE;
	    }
	    Tcl_DStringAppend(&dString, "tclpip" STRINGIFY(TCL_MAJOR_VERSION)
		STRINGIFY(TCL_MINOR_VERSION) ".dll ", -1);
	}
    } else if (applType == APPL_INTERP) {
	Tcl_DStringAppend(&dString, cmdPrefix, -1);
	Tcl_DStringAppend(&dString, " ", -1);
    }
    argv[0] = progPath;
    command = ConcatCmdArgs(argc, argv);
    Tcl_DStringAppend(&dString, command, -1);
    free((char *)command);
#if KILL_DEBUG
    PurifyPrintf("command is %s\n", Tcl_DStringValue(&dString));
#endif
    result = CreateProcess(
	NULL,			/* Module name. */
	Tcl_DStringValue(&dString),	/* Command line */
	NULL,			/* Process security */
	NULL,			/* Thread security */
	TRUE,			/* Inherit handles */
	createFlags,		/* Creation flags */
	NULL,			/* Environment */
	NULL,			/* Current working directory */
	&si,			/* Initialization for process: includes
				 * standard handles, appearance and location
				 * of window */
	&pi);			/* (out) Information about newly
				   created process */
    Tcl_DStringFree(&dString);

    if (!result) {
	Tcl_AppendResult(interp, "can't execute \"", argv[0], "\": ", Blt_Win32Error(),
	    (char *)NULL);
	goto closeHandles;
    }
#if KILL_DEBUG
    PurifyPrintf("Starting process with handle of %d\n", pi.hProcess);
    PurifyPrintf("Starting process with id of %d\n", pi.dwProcessId);
#endif
    if (applType == APPL_DOS) {
	/* Force the OS to give some time to the DOS process. */
	WaitForSingleObject(hProcess, 50);
    }
    /*
     * PSS ID Number: Q124121
     *
     *	  "When an application spawns a process repeatedly, a new
     *	  thread instance will be created for each process but the
     *	  previous instances may not be cleaned up.  This results in
     *	  a significant virtual memory loss each time the process is
     *	  spawned.  If there is a WaitForInputIdle() call between
     *	  CreateProcess() and CloseHandle(), the problem does not
     *	  occur."
     */
    /* 
     * This usually fails. GetLastError reports "Access is denied".  
     * The process handle isn't valid yet?  When you put in a delay,
     * WaitForInputIdle actually times out.
     */
    idleResult = WaitForInputIdle(pi.hProcess, 1000);
    if (idleResult == (DWORD)-1) {
	PurifyPrintf("wait failed on %d: %s\n", pi.hProcess, Blt_Win32Error());
    }
    CloseHandle(pi.hThread);

    *hProcessPtr = pi.hProcess;

    /*
     * Add the entry to mapping table. Its purpose is to translate
     * process handles to process ids. Most things we do with
     * the Win32 API take handles, but we still want to present
     * process ids to the user.
     */
    Blt_MapPid(pi.hProcess, pi.dwProcessId);
    result = TCL_OK;

  closeHandles:
    if (si.hStdInput != INVALID_HANDLE_VALUE) {
	CloseHandle(si.hStdInput);
    }
    if (si.hStdOutput != INVALID_HANDLE_VALUE) {
	CloseHandle(si.hStdOutput);
    }
    if (si.hStdError != INVALID_HANDLE_VALUE) {
	CloseHandle(si.hStdError);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * FileForRedirect --
 *
 *	This procedure does much of the work of parsing redirection
 *	operators.  It handles "@" if specified and allowed, and a file
 *	name, and opens the file if necessary.
 *
 * Results:
 *	The return value is the descriptor number for the file.  If an
 *	error occurs then NULL is returned and an error message is left
 *	in interp->result.  Several arguments are side-effected; see
 *	the argument list below for details.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static HANDLE
FileForRedirect(interp, spec, atOK, arg, nextArg, accessFlags,
    createFlags, skipPtr, closePtr)
    Tcl_Interp *interp;		/* Intepreter to use for error reporting. */
    char *spec;			/* Points to character just after
				 * redirection character. */
    char *arg;			/* Pointer to entire argument containing
				 * spec:  used for error reporting. */
    BOOL atOK;			/* Non-zero means that '@' notation can be
				 * used to specify a channel, zero means that
				 * it isn't. */
    char *nextArg;		/* Next argument in argc/argv array, if needed
				 * for file name or channel name.  May be
				 * NULL. */
    DWORD accessFlags;		/* Flags to use for opening file or to
				 * specify mode for channel. */
    DWORD createFlags;		/* Flags to use for opening file or to
				 * specify mode for channel. */
    int *skipPtr;		/* Filled with 1 if redirection target was
				 * in spec, 2 if it was in nextArg. */
    int *closePtr;		/* Filled with one if the caller should
				 * close the file when done with it, zero
				 * otherwise. */
{
    int writing = (accessFlags & GENERIC_WRITE);
    Tcl_Channel chan;
    HANDLE hFile;

    *skipPtr = 1;
    *closePtr = FALSE;
    if ((atOK) && (*spec == '@')) {
	spec++;
	if (*spec == '\0') {
	    spec = nextArg;
	    if (spec == NULL) {
		goto badLastArg;
	    }
	    *skipPtr = 2;
	}
	chan = Tcl_GetChannel(interp, spec, NULL);
	if (chan == NULL) {
	    return INVALID_HANDLE_VALUE;
	}
	if (Tcl_GetChannelHandle(chan, (writing) ? TCL_WRITABLE : TCL_READABLE,
		(ClientData *)&hFile) != TCL_OK) {
	    hFile = INVALID_HANDLE_VALUE;
	}
	if (hFile == INVALID_HANDLE_VALUE) {
	    Tcl_AppendResult(interp, "channel \"", Tcl_GetChannelName(chan),
		"\" wasn't opened for ",
		((writing) ? "writing" : "reading"), (char *)NULL);
	    return INVALID_HANDLE_VALUE;
	}
	if (writing) {
	    /*
	     * Be sure to flush output to the file, so that anything
	     * written by the child appears after stuff we've already
	     * written.
	     */
	    Tcl_Flush(chan);
	}
    } else {
	char *name;
	Tcl_DString nameString;

	if (*spec == '\0') {
	    spec = nextArg;
	    if (spec == NULL) {
		goto badLastArg;
	    }
	    *skipPtr = 2;
	}
	name = Tcl_TranslateFileName(interp, spec, &nameString);
	if (name != NULL) {
	    hFile = OpenRedirectFile(name, accessFlags, createFlags);
	} else {
	    hFile = INVALID_HANDLE_VALUE;
	}
	Tcl_DStringFree(&nameString);

	if (hFile == INVALID_HANDLE_VALUE) {
	    Tcl_AppendResult(interp, "can't ", (writing) ? "write" : "read",
		" file \"", spec, "\": ", Tcl_PosixError(interp),
		(char *)NULL);
	    return INVALID_HANDLE_VALUE;
	}
	*closePtr = TRUE;
    }
    return hFile;

  badLastArg:
    Tcl_AppendResult(interp, "can't specify \"", arg,
	"\" as last word in command", (char *)NULL);
    return INVALID_HANDLE_VALUE;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreatePipeline --
 *
 *	Given an argc/argv array, instantiate a pipeline of processes
 *	as described by the argv.
 *
 * Results:
 *	The return value is a count of the number of new processes
 *	created, or -1 if an error occurred while creating the pipeline.
 *	*pidArrayPtr is filled in with the address of a dynamically
 *	allocated array giving the ids of all of the processes.  It
 *	is up to the caller to free this array when it isn't needed
 *	anymore.  If inPipePtr is non-NULL, *inPipePtr is filled in
 *	with the file id for the input pipe for the pipeline (if any):
 *	the caller must eventually close this file.  If outPipePtr
 *	isn't NULL, then *outPipePtr is filled in with the file id
 *	for the output pipe from the pipeline:  the caller must close
 *	this file.  If errPipePtr isn't NULL, then *errPipePtr is filled
 *	with a file id that may be used to read error output after the
 *	pipeline completes.
 *
 * Side effects:
 *	Processes and pipes are created.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CreatePipeline(interp, argc, argv, pidArrayPtr, inPipePtr,
    outPipePtr, errPipePtr)
    Tcl_Interp *interp;		/* Interpreter to use for error reporting. */
    int argc;			/* Number of entries in argv. */
    char **argv;		/* Array of strings describing commands in
				 * pipeline plus I/O redirection with <,
				 * <<,  >, etc.  Argv[argc] must be NULL. */
    int **pidArrayPtr;		/* Word at *pidArrayPtr gets filled in with
				 * address of array of pids for processes
				 * in pipeline (first pid is first process
				 * in pipeline). */
    int *inPipePtr;		/* If non-NULL, input to the pipeline comes
				 * from a pipe (unless overridden by
				 * redirection in the command).  The file
				 * id with which to write to this pipe is
				 * stored at *inPipePtr.  NULL means command
				 * specified its own input source. */
    int *outPipePtr;		/* If non-NULL, output to the pipeline goes
				 * to a pipe, unless overriden by redirection
				 * in the command.  The file id with which to
				 * read frome this pipe is stored at
				 * *outPipePtr.  NULL means command specified
				 * its own output sink. */
    int *errPipePtr;		/* If non-NULL, all stderr output from the
				 * pipeline will go to a temporary file
				 * created here, and a descriptor to read
				 * the file will be left at *errPipePtr.
				 * The file will be removed already, so
				 * closing this descriptor will be the end
				 * of the file.  If this is NULL, then
				 * all stderr output goes to our stderr.
				 * If the pipeline specifies redirection
				 * then the file will still be created
				 * but it will never get any data. */
{
    HANDLE *procArr = NULL;	/* Points to malloc-ed array holding all
				 * the handles of child processes. */
    int numPids;		/* Actual number of processes that exist
				 * at *procArr right now. */
    int cmdCount;		/* Count of number of distinct commands
				 * found in argc/argv. */
    char *inputLiteral = NULL;	/* If non-null, then this points to a
				 * string containing input data (specified
				 * via <<) to be piped to the first process
				 * in the pipeline. */
    HANDLE hStdin;		/* If != -1, gives file to use as input for
				 * first process in pipeline (specified via <
				 * or <@). */
    BOOL closeStdin;		/* If non-zero, then hStdin should be
				 * closed when cleaning up. */
    HANDLE hStdout;		/* Writable file for output from last command
				 * in pipeline (could be file or pipe).  NULL
				 * means use stdout. */
    BOOL closeStdout;		/* If non-zero, then hStdout should be
				 * closed when cleaning up. */
    HANDLE hStderr;		/* Writable file for error output from all
				 * commands in pipeline.  NULL means use
				 * stderr. */
    BOOL closeStderr;		/* If non-zero, then hStderr should be
				 * closed when cleaning up. */

    HANDLE hInPipe, hOutPipe, hErrPipe;
    char *p;
    int skip, lastBar, lastArg, i, j, flags;
    BOOL atOK, errorToOutput;
    Tcl_DString execBuffer;
    HANDLE hPipe;
    HANDLE thisInput, thisOutput, thisError;

    if (inPipePtr != NULL) {
	*inPipePtr = -1;
    }
    if (outPipePtr != NULL) {
	*outPipePtr = -1;
    }
    if (errPipePtr != NULL) {
	*errPipePtr = -1;
    }
    Tcl_DStringInit(&execBuffer);

    hStdin = hStdout = hStderr = INVALID_HANDLE_VALUE;
    hPipe = thisInput = thisOutput = INVALID_HANDLE_VALUE;
    hInPipe = hOutPipe = hErrPipe = INVALID_HANDLE_VALUE;
    closeStdin = closeStdout = closeStderr = FALSE;
    numPids = 0;

    /*
     * First, scan through all the arguments to figure out the structure
     * of the pipeline.  Process all of the input and output redirection
     * arguments and remove them from the argument list in the pipeline.
     * Count the number of distinct processes (it's the number of "|"
     * arguments plus one) but don't remove the "|" arguments because
     * they'll be used in the second pass to seperate the individual
     * child processes.  Cannot start the child processes in this pass
     * because the redirection symbols may appear anywhere in the
     * command line -- e.g., the '<' that specifies the input to the
     * entire pipe may appear at the very end of the argument list.
     */

    lastBar = -1;
    cmdCount = 1;
    for (i = 0; i < argc; i++) {
	skip = 0;
	p = argv[i];
	switch (*p++) {
	case '|':
	    if (*p == '&') {
		p++;
	    }
	    if (*p == '\0') {
		if ((i == (lastBar + 1)) || (i == (argc - 1))) {
		    Tcl_SetResult(interp, "illegal use of | or |& in command",
			TCL_STATIC);
		    goto error;
		}
	    }
	    lastBar = i;
	    cmdCount++;
	    break;

	case '<':
	    if (closeStdin) {
		closeStdin = FALSE;
		CloseHandle(hStdin);
	    }
	    if (*p == '<') {
		hStdin = INVALID_HANDLE_VALUE;
		inputLiteral = p + 1;
		skip = 1;
		if (*inputLiteral == '\0') {
		    inputLiteral = argv[i + 1];
		    if (inputLiteral == NULL) {
			Tcl_AppendResult(interp, "can't specify \"", argv[i],
			    "\" as last word in command", (char *)NULL);
			goto error;
		    }
		    skip = 2;
		}
	    } else {
		inputLiteral = NULL;
		hStdin = FileForRedirect(interp, p, 1, argv[i], argv[i + 1],
		    GENERIC_READ, 0, &skip, &closeStdin);
		if (hStdin == INVALID_HANDLE_VALUE) {
		    goto error;
		}
	    }
	    break;

	case '>':
	    atOK = 1;
	    flags = CREATE_ALWAYS;
	    errorToOutput = FALSE;
	    if (*p == '>') {
		p++;
		atOK = 0;
		flags = OPEN_ALWAYS;
	    }
	    if (*p == '&') {
		if (closeStderr) {
		    closeStderr = FALSE;
		    CloseHandle(hStderr);
		}
		errorToOutput = TRUE;
		p++;
	    }
	    if (closeStdout) {
		closeStdout = FALSE;
		CloseHandle(hStdout);
	    }
	    hStdout = FileForRedirect(interp, p, atOK, argv[i], argv[i + 1],
		GENERIC_WRITE, flags, &skip, &closeStdout);
	    if (hStdout == INVALID_HANDLE_VALUE) {
		goto error;
	    }
	    if (errorToOutput) {
		closeStderr = FALSE;
		hStderr = hStdout;
	    }
	    break;

	case '2':
	    if (*p != '>') {
		break;
	    }
	    p++;
	    atOK = TRUE;
	    flags = CREATE_ALWAYS;
	    if (*p == '>') {
		p++;
		atOK = FALSE;
		flags = OPEN_ALWAYS;
	    }
	    if (closeStderr) {
		closeStderr = FALSE;
		CloseHandle(hStderr);
	    }
	    hStderr = FileForRedirect(interp, p, atOK, argv[i], argv[i + 1],
		GENERIC_WRITE, flags, &skip, &closeStderr);
	    if (hStderr == INVALID_HANDLE_VALUE) {
		goto error;
	    }
	    break;
	}

	if (skip != 0) {
	    for (j = i + skip; j < argc; j++) {
		argv[j - skip] = argv[j];
	    }
	    argc -= skip;
	    i -= 1;
	}
    }

    if (hStdin == INVALID_HANDLE_VALUE) {
	if (inputLiteral != NULL) {
	    /*
	     * The input for the first process is immediate data coming from
	     * Tcl.  Create a temporary file for it and put the data into the
	     * file.
	     */
	    hStdin = CreateTempFile(inputLiteral);
	    if (hStdin == INVALID_HANDLE_VALUE) {
		Tcl_AppendResult(interp,
		    "can't create input file for command: ",
		    Tcl_PosixError(interp), (char *)NULL);
		goto error;
	    }
	    closeStdin = TRUE;
	} else if (inPipePtr != NULL) {
	    /*
	     * The input for the first process in the pipeline is to
	     * come from a pipe that can be written from by the caller.
	     */

	    if (!CreatePipe(&hStdin, &hInPipe, NULL, 0)) {
		Tcl_AppendResult(interp,
		    "can't create input pipe for command: ",
		    Tcl_PosixError(interp), (char *)NULL);
		goto error;
	    }
	    closeStdin = TRUE;
	} else {
	    /*
	     * The input for the first process comes from stdin.
	     */
	}
    }
    if (hStdout == INVALID_HANDLE_VALUE) {
	if (outPipePtr != NULL) {
	    /*
	     * Output from the last process in the pipeline is to go to a
	     * pipe that can be read by the caller.
	     */

	    if (!CreatePipe(&hOutPipe, &hStdout, NULL, 0)) {
		Tcl_AppendResult(interp,
		    "can't create output pipe for command: ",
		    Tcl_PosixError(interp), (char *)NULL);
		goto error;
	    }
	    closeStdout = TRUE;
	} else {
	    /*
	     * The output for the last process goes to stdout.
	     */
	}
    }
    if (hStderr == INVALID_HANDLE_VALUE) {
	if (errPipePtr != NULL) {
	    /*
	     * Stderr from the last process in the pipeline is to go to a
	     * pipe that can be read by the caller.
	     */
	    if (CreatePipe(&hErrPipe, &hStderr, NULL, 0) == 0) {
		Tcl_AppendResult(interp,
		    "can't create error pipe for command: ",
		    Tcl_PosixError(interp), (char *)NULL);
		goto error;
	    }
	    closeStderr = TRUE;
	} else {
	    /*
	     * Errors from the pipeline go to stderr.
	     */
	}
    }
    /*
     * Scan through the argc array, creating a process for each
     * group of arguments between the "|" characters.
     */

    Tcl_ReapDetachedProcs();
    procArr = (HANDLE *) malloc((unsigned)((cmdCount + 1) * sizeof(HANDLE)));

    thisInput = hStdin;

    lastArg = 0;		/* Suppress compiler warning */
    for (i = 0; i < argc; i = lastArg + 1) {
	BOOL joinThisError;
	HANDLE hProcess;

	/* Convert the program name into native form. */
	argv[i] = Tcl_TranslateFileName(interp, argv[i], &execBuffer);
	if (argv[i] == NULL) {
	    goto error;
	}
	/* Find the end of the current segment of the pipeline. */
	joinThisError = FALSE;
	for (lastArg = i; lastArg < argc; lastArg++) {
	    if (argv[lastArg][0] == '|') {
		if (argv[lastArg][1] == '\0') {
		    break;
		}
		if ((argv[lastArg][1] == '&') && (argv[lastArg][2] == '\0')) {
		    joinThisError = TRUE;
		    break;
		}
	    }
	}
	argv[lastArg] = NULL;

	/*
	 * If this is the last segment, use the specified output handle.
	 * Otherwise create an intermediate pipe.  hPipe will become the
	 * input for the next segment of the pipe.
	 */
	if (lastArg == argc) {
	    thisOutput = hStdout;
	} else {
	    if (CreatePipe(&hPipe, &thisOutput, NULL, 0) == 0) {
		Tcl_AppendResult(interp, "can't create pipe: ",
		    Tcl_PosixError(interp), (char *)NULL);
		goto error;
	    }
	}

	if (joinThisError) {
	    thisError = thisOutput;
	} else {
	    thisError = hStderr;
	}

	if (StartProcess(interp, lastArg - i, argv + i, thisInput, thisOutput,
		thisError, &hProcess) != TCL_OK) {
	    goto error;
	}
	Tcl_DStringFree(&execBuffer);

	procArr[numPids] = hProcess;
	numPids++;

	/*
	 * Close off our copies of file descriptors that were set up for
	 * this child, then set up the input for the next child.
	 */

	if ((thisInput != INVALID_HANDLE_VALUE) && (thisInput != hStdin)) {
	    CloseHandle(thisInput);
	}
	thisInput = hPipe;
	hPipe = INVALID_HANDLE_VALUE;

	if ((thisOutput != INVALID_HANDLE_VALUE) && (thisOutput != hStdout)) {
	    CloseHandle(thisOutput);
	}
	thisOutput = INVALID_HANDLE_VALUE;
    }

    *pidArrayPtr = (int *)procArr;

    if (inPipePtr != NULL) {
	*inPipePtr = (int)hInPipe;
    }
    if (outPipePtr != NULL) {
	*outPipePtr = (int)hOutPipe;
    }
    if (errPipePtr != NULL) {
	*errPipePtr = (int)hErrPipe;
    }
    /*
     * All done.  Cleanup open files lying around and then return.
     */
  cleanup:
    Tcl_DStringFree(&execBuffer);

    if (closeStdin) {
	CloseHandle(hStdin);
    }
    if (closeStdout) {
	CloseHandle(hStdout);
    }
    if (closeStderr) {
	CloseHandle(hStderr);
    }
    return numPids;

    /*
     * An error occurred.  There could have been extra files open, such
     * as pipes between children.  Clean them all up.  Detach any child
     * processes that have been created.
     */
  error:
    if (hPipe != INVALID_HANDLE_VALUE) {
	CloseHandle(hPipe);
    }
    if ((thisOutput != INVALID_HANDLE_VALUE) && (thisOutput != hStdout)) {
	CloseHandle(thisOutput);
    }
    if ((thisInput != INVALID_HANDLE_VALUE) && (thisInput != hStdin)) {
	CloseHandle(thisInput);
    }
    if (hInPipe != INVALID_HANDLE_VALUE) {
	CloseHandle(hInPipe);
    }
    if (hOutPipe != INVALID_HANDLE_VALUE) {
	CloseHandle(hOutPipe);
    }
    if (hErrPipe != INVALID_HANDLE_VALUE) {
	CloseHandle(hErrPipe);
    }
    if (procArr != NULL) {
	for (i = 0; i < numPids; i++) {
	    if (procArr[i] != INVALID_HANDLE_VALUE) {
		/* It's Ok to use Tcl_DetachPids, since for WIN32 it's really
		 * using process handles, not process ids. */
#if (TCL_MAJOR_VERSION >= 8)
		Tcl_DetachPids(1, (Tcl_Pid *)&procArr[i]);
#else
		Tcl_DetachPids(1, (int *)&procArr[i]);
#endif
	    }
	}
	free((char *)procArr);
    }
    numPids = -1;
    goto cleanup;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateFileHandler --
 *
 *	Limited emulation Tcl_CreateFileHandler for Win32. Works
 *	with pipes. Don't know if anything else will (such as sockets).
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Registers procedure and data to call back when data
 *	is available on the pipe.
 *
 *----------------------------------------------------------------------
 */
void
Blt_CreateFileHandler(fd, flags, proc, clientData)
    int fd;			/* Descriptor or handle of file */
    int flags;			/* TCL_READABLE or TCL_WRITABLE  */
    Tcl_FileProc *proc;
    ClientData clientData;
{
    PipeHandler *pipePtr;

    if (!initialized) {
	PipeInit();
    }
    if ((flags != TCL_READABLE) && (flags != TCL_WRITABLE)) {
	return;			/* Only one of the flags can be set. */
    }
    pipePtr = CreatePipeHandler((HANDLE)fd, flags);
    pipePtr->proc = proc;
    pipePtr->clientData = clientData;

    /* Add the handler to the list of managed pipes. */
    EnterCriticalSection(&pipeCriticalSection);
    Blt_ListAppend(&pipeList, (char *)pipePtr->hPipe, (ClientData)pipePtr);
    LeaveCriticalSection(&pipeCriticalSection);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DeleteFileHandler --
 *
 *	Win32 emulation Tcl_DeleteFileHandler.  Cleans up resources
 *	used.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DeleteFileHandler(fd)
    int fd;			/* Descriptor or handle of file */
{
    Blt_ListItem item;
    if (!initialized) {
	PipeInit();
    }
#if KILL_DEBUG
    PurifyPrintf("DeleteFileHandler");
#endif
    EnterCriticalSection(&pipeCriticalSection);
    item = Blt_ListFind(&pipeList, (char *)fd);
#if KILL_DEBUG
    PurifyPrintf("Found %d", fd);
#endif
    if (item != NULL) {
	PipeHandler *pipePtr;

	pipePtr = Blt_ListGetValue(item);
	assert(pipePtr != NULL);
	Blt_ListDeleteItem(item);
	DestroyPipeHandler(pipePtr);
    }
    LeaveCriticalSection(&pipeCriticalSection);
#if KILL_DEBUG
    PurifyPrintf("Blt_DeleteFileHandler: done\n");
#endif
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_AsyncRead --
 *
 *	Reads input from the pipe into the given buffer.
 *
 * Results:
 *	Returns the number of bytes read.
 *
 *----------------------------------------------------------------------
 */
int
Blt_AsyncRead(f, buffer, size)
    int f;
    char buffer[];
    unsigned int size;
{
    register PipeHandler *pipePtr;
    unsigned int count;
    int numBytes;
    register Blt_ListItem item;

#if ASYNC_DEBUG
    PurifyPrintf("ASYNCREAD (f=%d)\n", f);
#endif
    item = Blt_ListFind(&pipeList, (char *)f);
    if (item == NULL) {
	errno = EBADF;
#if ASYNC_DEBUG
	PurifyPrintf("ASYNCREAD: bad file\n");
#endif
	return -1;
    }
    pipePtr = (PipeHandler *) Blt_ListGetValue(item);
    if (!PeekOnPipe(pipePtr, &numBytes)) {
#if ASYNC_DEBUG
	PurifyPrintf("ASYNCREAD: pipe is drained (numBytes=%d).\n", numBytes);
#endif
	return -1;		/* No data available. */
    }
    /*
     * numBytes is	0	EOF found.
     *			-1	Error occured.
     *			1+	Number of bytes available.
     */
    if (numBytes == -1) {
#if ASYNC_DEBUG
	PurifyPrintf("ASYNCREAD: Error\n");
#endif
	return -1;
    }
    if (numBytes == 0) {
#if ASYNC_DEBUG
	PurifyPrintf("ASYNCREAD: EOF\n");
#endif
	return 0;
    }
    count = pipePtr->end - pipePtr->start;
#if ASYNC_DEBUG
    PurifyPrintf("ASYNCREAD: numBytes is %d, %d\n", numBytes, count);
#endif
    assert(count == (unsigned int)numBytes);
    if (size > count) {
	size = count;		/* Reset request to what's available. */
    }
    memcpy(buffer, pipePtr->buffer + pipePtr->start, size);
    pipePtr->start += size;
    if (pipePtr->start == pipePtr->end) {
#if ASYNC_DEBUG
	PurifyPrintf("ASYNCREAD: signaling idle\n");
#endif
	ResetEvent(pipePtr->readyEvent);
	SetEvent(pipePtr->idleEvent);
    }
    return size;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_AsyncWrite --
 *
 *	Writes output to the pipe from the given buffer.
 *
 * Results:
 *	Returns the number of bytes written.
 *
 *----------------------------------------------------------------------
 */
int
Blt_AsyncWrite(f, buffer, size)
    int f;
    char buffer[];
    unsigned int size;
{
    register PipeHandler *pipePtr;
    Blt_ListItem item;

    item = Blt_ListFind(&pipeList, (char *)f);
    if (item == NULL) {
	errno = EBADF;
	return -1;
    }
    pipePtr = (PipeHandler *)Blt_ListGetValue(item);
    if (WaitForSingleObject(pipePtr->readyEvent, 0) == WAIT_TIMEOUT) {
	/*
	 * Writer thread is currently blocked waiting for a write to
	 * complete.
	 */
	errno = EAGAIN;
	return -1;
    }
    /* Check for a background error on the last write. */
    if (pipePtr->lastError) {
	TclWinConvertError(pipePtr->lastError);
	pipePtr->lastError = 0;
	return -1;
    }
    /* Reallocate the buffer to be large enough to hold the data. */
    if (size > pipePtr->size) {
	pipePtr->buffer = (char *)realloc(pipePtr->buffer, size);
	assert(pipePtr->buffer);
    }
    memcpy(pipePtr->buffer, buffer, size);
    pipePtr->end = pipePtr->size = size;
    ResetEvent(pipePtr->readyEvent);
    SetEvent(pipePtr->idleEvent);
    return size;
}
