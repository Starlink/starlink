/*
 * tclXfilescan.c --
 *
 * Tcl file scanning: regular expression matching on lines of a file.  
 * Implements awk.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXfilescan.c,v 8.8 1997/07/05 08:00:30 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

/*
 * FIX: Does not support binary data.  Can't fix until the regexp facility
 * handles it.
 */

/*
 * A scan context describes a collection of match patterns and commands,
 * along with a match default command to apply to a file on a scan.
 */
 
#define CONTEXT_A_CASE_INSENSITIVE_FLAG 2
#define MATCH_CASE_INSENSITIVE_FLAG 4

typedef struct matchDef_t {
    TclX_regexp         regExpInfo;
    Tcl_Obj            *command;
    struct matchDef_t  *nextMatchDefPtr;
    short               matchflags;
} matchDef_t;

typedef struct scanContext_t {
    matchDef_t  *matchListHead;
    matchDef_t  *matchListTail;
    Tcl_Obj     *defaultAction;
    short        flags;
    char         contextHandle [16];
    Tcl_Channel  copyFileChannel;
    int          fileOpen;
} scanContext_t;

/*
 * Data kept on a specific scan.
 */
typedef struct {
    int               storedLine;   /* Has the current line been stored in
                                       matchInfo? */
    scanContext_t    *contextPtr;   /* Current scan context. */
    Tcl_Channel       channel;      /* The channel being scanned. */
    char             *line;         /* The line from the file. */
    off_t             offset;       /* The offset into the file. */
    long              bytesRead;    /* Number of translated bytes read.*/
    long              lineNum;      /* Current scanned line in the file. */
    matchDef_t       *matchPtr;     /* The current match, or NULL for the
                                       default. */
    Tcl_SubMatchInfo  subMatchInfo; /* Information about the subexpressions
                                       that matched, or NULL if this is the
                                       default match. */
} scanData_t;

/*
 * Prototypes of internal functions.
 */
static void
CleanUpContext _ANSI_ARGS_((void_pt         scanTablePtr,
                            scanContext_t  *contextPtr));

static int
ScanContextCreate _ANSI_ARGS_((Tcl_Interp  *interp,
                               void_pt      scanTablePtr));

static int
ScanContextDelete _ANSI_ARGS_((Tcl_Interp  *interp,
                               void_pt      scanTablePtr,
                               Tcl_Obj     *contextHandleObj));

static int
ScanContextCopyFile _ANSI_ARGS_((Tcl_Interp  *interp,
                                 void_pt      scanTablePtr,
                                 Tcl_Obj     *contextHandleObj,
                                 Tcl_Obj     *fileHandleObj));

static int
TclX_ScancontextObjCmd _ANSI_ARGS_((ClientData  clientData,
                                    Tcl_Interp *interp,
                                    int         objc,
                                    Tcl_Obj    *CONST objv[]));

static int
TclX_ScanmatchObjCmd _ANSI_ARGS_((ClientData  clientData,
                                  Tcl_Interp *interp,
                                  int         objc,
                                  Tcl_Obj    *CONST objv[]));

static void
CopyFileCloseHandler _ANSI_ARGS_((ClientData clientData));

static int
SetCopyFileObj _ANSI_ARGS_((Tcl_Interp    *interp,
                            scanContext_t *contextPtr,
                            Tcl_Obj       *fileHandleObj));

static void
ClearCopyFile _ANSI_ARGS_((scanContext_t *contextPtr));

static int
SetMatchInfoVar _ANSI_ARGS_((Tcl_Interp       *interp,
                             scanData_t       *scanData));

static int
ScanFile _ANSI_ARGS_((Tcl_Interp    *interp,
                      scanContext_t *contextPtr,
                      Tcl_Channel    channel));

static void
ScanFileCloseHandler _ANSI_ARGS_((ClientData clientData));

static int
TclX_ScanfileObjCmd _ANSI_ARGS_((ClientData  clientData,
                                 Tcl_Interp *interp,
                                 int         objc,
                                 Tcl_Obj    *CONST objv[]));

static void
FileScanCleanUp _ANSI_ARGS_((ClientData  clientData,
                             Tcl_Interp *interp));


/*-----------------------------------------------------------------------------
 * CleanUpContext --
 *
 *   Release all resources allocated to the specified scan context.  Doesn't
 * free the table entry.
 *-----------------------------------------------------------------------------
 */
static void
CleanUpContext (scanTablePtr, contextPtr)
    void_pt        scanTablePtr;
    scanContext_t *contextPtr;
{
    matchDef_t  *matchPtr, *oldMatchPtr;

    for (matchPtr = contextPtr->matchListHead; matchPtr != NULL;) {
        TclX_RegExpClean (&matchPtr->regExpInfo);
        if (matchPtr->command != NULL)
            Tcl_DecrRefCount (matchPtr->command);
        oldMatchPtr = matchPtr;
        matchPtr = matchPtr->nextMatchDefPtr;
        ckfree ((char *) oldMatchPtr);
    }
    if (contextPtr->defaultAction != NULL) {
        Tcl_DecrRefCount (contextPtr->defaultAction);
    }
    ClearCopyFile (contextPtr);
    ckfree ((char *) contextPtr);
}

/*-----------------------------------------------------------------------------
 * ScanContextCreate --
 *
 *   Create a new scan context, implements the subcommand:
 *         scancontext create
 *-----------------------------------------------------------------------------
 */
static int
ScanContextCreate (interp, scanTablePtr)
    Tcl_Interp  *interp;
    void_pt      scanTablePtr;
{
    scanContext_t *contextPtr, **tableEntryPtr;

    contextPtr = (scanContext_t *) ckalloc (sizeof (scanContext_t));
    contextPtr->flags = 0;
    contextPtr->matchListHead = NULL;
    contextPtr->matchListTail = NULL;
    contextPtr->defaultAction = NULL;
    contextPtr->copyFileChannel = NULL;

    tableEntryPtr = (scanContext_t **)
        TclX_HandleAlloc (scanTablePtr,
                          contextPtr->contextHandle);
    *tableEntryPtr = contextPtr;

    Tcl_SetStringObj (Tcl_GetObjResult (interp),
                      contextPtr->contextHandle, -1);
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * ScanContextDelete --
 *
 *   Deletes the specified scan context, implements the subcommand:
 *         scancontext delete contexthandle
 *-----------------------------------------------------------------------------
 */
static int
ScanContextDelete (interp, scanTablePtr, contextHandleObj)
    Tcl_Interp  *interp;
    void_pt      scanTablePtr;
    Tcl_Obj     *contextHandleObj;
{
    scanContext_t **tableEntryPtr;
    char           *contextHandle;

    contextHandle = Tcl_GetStringFromObj (contextHandleObj, NULL);

    tableEntryPtr = (scanContext_t **) TclX_HandleXlate (interp,
                                                         scanTablePtr,
                                                         contextHandle);
    if (tableEntryPtr == NULL)
        return TCL_ERROR;

    CleanUpContext (scanTablePtr, *tableEntryPtr);
    TclX_HandleFree (scanTablePtr, tableEntryPtr);

    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * CopyFileCloseHandler --
 *   Close handler for the copyfile.  Turns off copying to the file.
 * Parameters:
 *   o clientData (I) - Pointer to the scan context.
 *-----------------------------------------------------------------------------
 */
static void
CopyFileCloseHandler (clientData)
    ClientData clientData;
{
    ((scanContext_t *) clientData)->copyFileChannel = NULL;
}

/*-----------------------------------------------------------------------------
 * SetCopyFileObj --
 *   Set the copy file handle for a context.
 * Parameters:
 *   o interp  n     (O) - The Tcl interpreter, errors are returned in result.
 *   o contextPtr    (I) - Pointer to the scan context.
 *   o fileHandleObj (I) - Object containing file handle of the copy file.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
SetCopyFileObj (interp, contextPtr, fileHandleObj)
    Tcl_Interp    *interp;
    scanContext_t *contextPtr;
    Tcl_Obj       *fileHandleObj;
{
    Tcl_Channel copyFileChannel;

    copyFileChannel = TclX_GetOpenChannelObj (interp, fileHandleObj,
                                              TCL_WRITABLE);
    if (copyFileChannel == NULL)
        return TCL_ERROR;

    /*
     * Delete the old copyfile and set the new one.
     */
    if (contextPtr->copyFileChannel != NULL) {
        Tcl_DeleteCloseHandler (contextPtr->copyFileChannel,
                                CopyFileCloseHandler,
                                (ClientData) contextPtr);
    }
    Tcl_CreateCloseHandler (copyFileChannel,
                            CopyFileCloseHandler,
                            (ClientData) contextPtr);
    contextPtr->copyFileChannel = copyFileChannel;
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * ClearCopyFile --
 *   Clear the copy file handle for a context.
 * Parameters:
 *   o contextPtr (I) - Pointer to the scan context.
 *-----------------------------------------------------------------------------
 */
static void
ClearCopyFile (contextPtr)
    scanContext_t *contextPtr;
{
    if (contextPtr->copyFileChannel != NULL) {
        Tcl_DeleteCloseHandler (contextPtr->copyFileChannel,
                                CopyFileCloseHandler,
                                (ClientData) contextPtr);
        contextPtr->copyFileChannel = NULL;
    }
}

/*-----------------------------------------------------------------------------
 * ScanContextCopyFile --
 *
 *   Access or set the copy file handle for the specified scan context,
 * implements the subcommand:
 *         scancontext copyfile contexthandle ?filehandle?
 *-----------------------------------------------------------------------------
 */
static int
ScanContextCopyFile (interp, scanTablePtr, contextHandleObj, fileHandleObj)
    Tcl_Interp  *interp;
    void_pt      scanTablePtr;
    Tcl_Obj     *contextHandleObj;
    Tcl_Obj     *fileHandleObj;
{
    scanContext_t *contextPtr, **tableEntryPtr;
    char         *contextHandle;

    contextHandle = Tcl_GetStringFromObj (contextHandleObj, NULL);

    tableEntryPtr = (scanContext_t **) TclX_HandleXlate (interp,
                                                         scanTablePtr,
                                                         contextHandle);
    if (tableEntryPtr == NULL)
        return TCL_ERROR;
    contextPtr = *tableEntryPtr;

    /*
     * Return the copy file handle if not specified.
     */
    if (fileHandleObj == NULL) {
	Tcl_SetStringObj (Tcl_GetObjResult (interp),
                          Tcl_GetChannelName (contextPtr->copyFileChannel),
			  -1);
        return TCL_OK;
    }

    return SetCopyFileObj (interp, contextPtr, fileHandleObj);
}


/*-----------------------------------------------------------------------------
 * TclX_ScancontextObjCmd --
 *
 *   Implements the TCL scancontext Tcl command, which has the following forms:
 *         scancontext create
 *         scancontext delete
 *-----------------------------------------------------------------------------
 */
static int
TclX_ScancontextObjCmd (clientData, interp, objc, objv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         objc;
    Tcl_Obj    *CONST objv[];
{
    char *command;
    char *subCommand;

    if (objc < 2)
	return TclX_WrongArgs (interp, objv [0], "option ...");

    command = Tcl_GetStringFromObj (objv [0], NULL);
    subCommand = Tcl_GetStringFromObj (objv [1], NULL);

    /*
     * Create a new scan context.
     */
    if (STREQU (subCommand, "create")) {
        if (objc != 2)
	    return TclX_WrongArgs (interp, objv [0], "create");

        return ScanContextCreate (interp,
                                  (void_pt) clientData);
    }
    
    /*
     * Delete a scan context.
     */
    if (STREQU (subCommand, "delete")) {
        if (objc != 3)
	    return TclX_WrongArgs (interp, objv [0], "delete contexthandle");

        return ScanContextDelete (interp,
                                  (void_pt) clientData,
                                  objv [2]);
    }

    /*
     * Access or set the copyfile.
     */
    if (STREQU (subCommand, "copyfile")) {
        if ((objc < 3) || (objc > 4))
	    return TclX_WrongArgs (interp, objv [0],
                              "copyfile contexthandle ?filehandle?");

        return ScanContextCopyFile (interp,
                                    (void_pt) clientData,
                                    objv [2],
                                    (objc == 4) ? objv [3] : NULL);
    }

    TclX_AppendObjResult (interp, "invalid argument, expected one of: ",
                          "\"create\", \"delete\", or \"copyfile\"",
                          (char *) NULL);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------------
 * TclX_ScanmatchObjCmd --
 *
 *   Implements the TCL command:
 *         scanmatch ?-nocase? contexthandle ?regexp? command
 *
 *   This uses both Boyer_Moore and regular expressions matching.
 *-----------------------------------------------------------------------------
 */
static int
TclX_ScanmatchObjCmd (clientData, interp, objc, objv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         objc;
    Tcl_Obj    *CONST objv[];
{
    scanContext_t  *contextPtr, **tableEntryPtr;
    matchDef_t     *newmatch;
    int             compFlags = TCLX_REXP_BOTH_ALGORITHMS;
    int             firstArg = 1;

    if (objc < 3)
        goto argError;

    if (STREQU (Tcl_GetStringFromObj (objv[1], NULL), "-nocase")) {
        compFlags |= TCLX_REXP_NO_CASE;
        firstArg = 2;
    }
      
    /*
     * If firstArg == 2 (-nocase), the both a regular expression and a command
     * string must be specified, otherwise the regular expression is optional.
     */
    if (((firstArg == 2) && (objc != 5)) || ((firstArg == 1) && (objc > 4)))
        goto argError;

    tableEntryPtr = (scanContext_t **)
        TclX_HandleXlateObj (interp,
                             (void_pt) clientData, 
                             objv [firstArg]);
    if (tableEntryPtr == NULL)
        return TCL_ERROR;
    contextPtr = *tableEntryPtr;

    /*
     * Handle the default case (no regular expression).
     */
    if (objc == 3) {
        if (contextPtr->defaultAction) {
            Tcl_AppendStringsToObj (Tcl_GetObjResult (interp),
		Tcl_GetStringFromObj (objv[0], NULL),
		": default match already specified in this scan context", 
		(char *) NULL);
            return TCL_ERROR;
        }
	Tcl_IncrRefCount (objv [2]);
        contextPtr->defaultAction = objv [2];

        return TCL_OK;
    }

    /*
     * Add a regular expression to the context.
     */

    newmatch = (matchDef_t *) ckalloc(sizeof (matchDef_t));
    newmatch->matchflags = 0;

    if (compFlags & TCLX_REXP_NO_CASE) {
        newmatch->matchflags |= MATCH_CASE_INSENSITIVE_FLAG;
        contextPtr->flags |= CONTEXT_A_CASE_INSENSITIVE_FLAG;
    }

    if (TclX_RegExpCompileObj (interp, &newmatch->regExpInfo,
              objv [firstArg + 1], compFlags) != TCL_OK) {
        ckfree ((char *) newmatch);
        return (TCL_ERROR);
    }

    Tcl_IncrRefCount (objv [firstArg + 2]);
    newmatch->command = objv [firstArg + 2];

    /*
     * Link in the new match.
     */
    newmatch->nextMatchDefPtr = NULL;
    if (contextPtr->matchListHead == NULL)
        contextPtr->matchListHead = newmatch;
    else
        contextPtr->matchListTail->nextMatchDefPtr = newmatch;
    contextPtr->matchListTail = newmatch;

    return TCL_OK;

argError:
    return TclX_WrongArgs (interp, objv [0],
                           "?-nocase? contexthandle ?regexp? command");
}

/*-----------------------------------------------------------------------------
 * SetMatchInfoVar --
 *
 *   Sets the Tcl array variable "matchInfo" to contain information about the
 * current match.  This function is optimize to store per line information
 * only once.
 *
 * Parameters:
 *   o interp (O) - The Tcl interpreter to set the matchInfo variable in.
 *     Errors are returned in result.
 *   o scanData (I/O) - Data about the current line being scanned.
 *     been stored.  Should be set to FALSE when a new line is read.
 *-----------------------------------------------------------------------------
 */
static int
SetMatchInfoVar (interp, scanData)
    Tcl_Interp *interp;
    scanData_t *scanData;
{
    static char *MATCHINFO = "matchInfo";
    int idx, start, end;
    char key [32];
    Tcl_Obj *valueObjPtr, *indexObjv [2];

    /*
     * Save information about the current line, if it hasn't been saved.
     */
    if (!scanData->storedLine) {
        scanData->storedLine = TRUE;

        Tcl_UnsetVar (interp, MATCHINFO, 0);
        
        if (Tcl_SetVar2 (interp, MATCHINFO, "line", scanData->line, 
                         TCL_LEAVE_ERR_MSG) == NULL)
            return TCL_ERROR;

        valueObjPtr = Tcl_NewLongObj ((long) scanData->offset);
        if (TclX_ObjSetVar2S (interp, MATCHINFO, "offset", valueObjPtr,
                              TCL_LEAVE_ERR_MSG) == NULL) {
            Tcl_DecrRefCount (valueObjPtr);
            return TCL_ERROR;
        }

#if 0
        /*
         * FIX: Don't expose till we decide on semantics: Should it include the
         * current line?  All the pieces are here, include doc and tests, just
         * disabled.
         */
        valueObjPtr = Tcl_NewLongObj ((long) scanData->bytesRead);
        if (TclX_SetVar2S (interp, MATCHINFO, "bytesread", valueObjPtr,
                           TCL_LEAVE_ERR_MSG) == NULL) {
            Tcl_DecrRefCount (valueObjPtr);
            return TCL_ERROR;
        }
#endif
        valueObjPtr = Tcl_NewIntObj ((long) scanData->lineNum);
        if (TclX_ObjSetVar2S (interp, MATCHINFO, "linenum", valueObjPtr,
                              TCL_LEAVE_ERR_MSG) == NULL) {
            Tcl_DecrRefCount (valueObjPtr);
            return TCL_ERROR;
        }

        if (Tcl_SetVar2 (interp, MATCHINFO, "context",
                         scanData->contextPtr->contextHandle,
                         TCL_LEAVE_ERR_MSG) == NULL)
            return TCL_ERROR;

        if (Tcl_SetVar2 (interp, MATCHINFO, "handle", 
                         Tcl_GetChannelName (scanData->channel),
                         TCL_LEAVE_ERR_MSG) == NULL)
            return TCL_ERROR;

    }

    if (scanData->contextPtr->copyFileChannel != NULL) {
        if (Tcl_SetVar2 (interp, MATCHINFO, "copyHandle", 
                Tcl_GetChannelName (scanData->contextPtr->copyFileChannel),
                TCL_LEAVE_ERR_MSG) == NULL)
            return TCL_ERROR;
    }

    if (scanData->matchPtr == NULL)
        return TCL_OK;

    for (idx = 0; idx < scanData->matchPtr->regExpInfo.numSubExprs; idx++) {
        start = scanData->subMatchInfo [idx].start;
        end = scanData->subMatchInfo [idx].end;

        sprintf (key, "subindex%d", idx);
        indexObjv [0] = Tcl_NewIntObj (start);
        indexObjv [1] = Tcl_NewIntObj (end);
        valueObjPtr = Tcl_NewListObj (2, indexObjv);
        if (TclX_ObjSetVar2S (interp, "matchInfo", key, valueObjPtr,
                              TCL_LEAVE_ERR_MSG) == NULL) {
            Tcl_DecrRefCount (valueObjPtr);
            return TCL_ERROR;
        }

        sprintf (key, "submatch%d", idx);
        if (start < 0) {
            valueObjPtr = Tcl_NewStringObj ("", 0);
        } else {
            valueObjPtr = Tcl_NewStringObj (scanData->line + start,
                                            (end - start) + 1);
        }
        if (TclX_ObjSetVar2S (interp, "matchInfo", key, valueObjPtr,
                              TCL_LEAVE_ERR_MSG) == NULL) {
            Tcl_DecrRefCount (valueObjPtr);
            return TCL_ERROR;
        }
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * ScanFile --
 *
 *   Scan a file given a scancontext.
 *-----------------------------------------------------------------------------
 */
static int
ScanFile (interp, contextPtr, channel)
    Tcl_Interp    *interp;
    scanContext_t *contextPtr;
    Tcl_Channel    channel;
{
    Tcl_DString buffer, lowerBuffer;
    int result, matchedAtLeastOne;
    scanData_t data;
    
    if (contextPtr->matchListHead == NULL) {
        TclX_AppendObjResult (interp, "no patterns in current scan context",
                              (char *) NULL);
        return TCL_ERROR;
    }

    data.storedLine = FALSE;
    data.contextPtr = contextPtr;
    data.channel = channel;
    data.bytesRead = 0;
    data.lineNum = 0;
    
    Tcl_DStringInit (&buffer);
    Tcl_DStringInit (&lowerBuffer);

    result = TCL_OK;
    while (TRUE) {
        if (!contextPtr->fileOpen)
            goto scanExit;  /* Closed by a callback */

        data.offset = Tcl_Tell (channel);
        Tcl_DStringSetLength (&buffer, 0);
        if (Tcl_Gets (channel, &buffer) < 0) {
            if (Tcl_Eof (channel) || Tcl_InputBlocked (channel))
                goto scanExit;
            Tcl_SetStringObj (Tcl_GetObjResult (interp),
                              Tcl_PosixError (interp), -1);
            result = TCL_ERROR;
            goto scanExit;
        }
        data.line = buffer.string;
        data.bytesRead += (buffer.length + 1);  /* Include EOLN */
        data.lineNum++;
        data.storedLine = FALSE;
        matchedAtLeastOne = FALSE;

        if (contextPtr->flags & CONTEXT_A_CASE_INSENSITIVE_FLAG) {
            Tcl_DStringSetLength (&lowerBuffer, 0);
            Tcl_DStringAppend (&lowerBuffer, buffer.string, -1);
            TclX_DownShift (lowerBuffer.string, lowerBuffer.string);
        }

        for (data.matchPtr = contextPtr->matchListHead; 
             data.matchPtr != NULL; 
             data.matchPtr = data.matchPtr->nextMatchDefPtr) {

            if (!TclX_RegExpExecute (interp,
                                     &(data.matchPtr->regExpInfo),
                                     buffer.string,
                                     lowerBuffer.string,
                                     data.subMatchInfo))
                continue;  /* Try next match pattern */

            matchedAtLeastOne = TRUE;

            result = SetMatchInfoVar (interp,
                                      &data);
            if (result != TCL_OK)
                goto scanExit;

            result = Tcl_EvalObj (interp, data.matchPtr->command);
            if (result == TCL_ERROR) {
                Tcl_AddObjErrorInfo (interp, 
                    "\n    while executing a match command", -1);
                goto scanExit;
            }
            if (result == TCL_CONTINUE) {
                /* 
                 * Don't process any more matches for this line.
                 */
                goto matchLineExit;
            }
            if ((result == TCL_BREAK) || (result == TCL_RETURN)) {
                /*
                 * Terminate scan.
                 */
                result = TCL_OK;
                goto scanExit;
            }
        }

      matchLineExit:
        /*
         * Process default action if required.
         */
        if ((contextPtr->defaultAction != NULL) && (!matchedAtLeastOne)) {
            data.matchPtr = NULL;
            result = SetMatchInfoVar(interp,
                                     &data);
            if (result != TCL_OK)
                goto scanExit;

            result = Tcl_EvalObj (interp, contextPtr->defaultAction);
            if (result == TCL_ERROR) {
                Tcl_AddObjErrorInfo (interp, 
                    "\n    while executing a match default command", -1);
                goto scanExit;
            }
            if ((result == TCL_BREAK) || (result == TCL_RETURN)) {
                /*
                 * Terminate scan.
                 */
                result = TCL_OK;
                goto scanExit;
            }
        }

	if ((contextPtr->copyFileChannel != NULL) && (!matchedAtLeastOne)) {
	    if ((Tcl_Write (contextPtr->copyFileChannel,
                            buffer.string, buffer.length) < 0) ||
                (TclX_WriteNL (contextPtr->copyFileChannel) < 0)) {
                Tcl_SetStringObj (Tcl_GetObjResult (interp),
                                  Tcl_PosixError (interp), -1);
		return TCL_ERROR;
	    }
	}
    }

  scanExit:
    Tcl_DStringFree (&buffer);
    Tcl_DStringFree (&lowerBuffer);
    if (result == TCL_ERROR)
        return TCL_ERROR;
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * ScanFileCloseHandler --
 *   Close handler for the file being scanned.  Marks it as not open.
 * Parameters:
 *   o clientData (I) - Pointer to the scan context.
 *-----------------------------------------------------------------------------
 */
static void
ScanFileCloseHandler (clientData)
    ClientData clientData;
{
    ((scanContext_t *) clientData)->fileOpen = FALSE;
}

/*-----------------------------------------------------------------------------
 * TclX_ScanfileObjCmd --
 *
 *   Implements the TCL command:
 *        scanfile ?-copyfile copyhandle? contexthandle filehandle
 *-----------------------------------------------------------------------------
 */
static int
TclX_ScanfileObjCmd (clientData, interp, objc, objv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         objc;
    Tcl_Obj    *CONST objv[];
{
    scanContext_t *contextPtr, **tableEntryPtr;
    Tcl_Obj       *contextHandleObj, *fileHandleObj, *copyFileHandleObj;
    Tcl_Channel    channel;
    int            status;

    if ((objc != 3) && (objc != 5))
        goto argError;

    if (objc == 3) {
	contextHandleObj = objv [1];
	fileHandleObj = objv [2];
	copyFileHandleObj = NULL;
    } else {
	if (!STREQU (Tcl_GetStringFromObj (objv[1], NULL), "-copyfile"))
            goto argError;
	copyFileHandleObj = objv [2];
	contextHandleObj = objv [3];
	fileHandleObj = objv [4];
    }

    tableEntryPtr = (scanContext_t **)
        TclX_HandleXlateObj (interp,
                             (void_pt) clientData, 
                             contextHandleObj);
    if (tableEntryPtr == NULL)
        return TCL_ERROR;
    contextPtr = *tableEntryPtr;

    channel = TclX_GetOpenChannelObj (interp, fileHandleObj, TCL_READABLE);
    if (channel == NULL)
        return TCL_ERROR;

    if (copyFileHandleObj != NULL) {
        if (SetCopyFileObj (interp, contextPtr, copyFileHandleObj) == TCL_ERROR)
            return TCL_ERROR;
    }

    /*
     * Scan the file, protecting it with a close handler.
     */
    contextPtr->fileOpen = TRUE;
    Tcl_CreateCloseHandler (channel,
                            ScanFileCloseHandler,
                            (ClientData) contextPtr);
    status = ScanFile (interp,
                       contextPtr,
                       channel);
    Tcl_DeleteCloseHandler (channel,
                            ScanFileCloseHandler,
                            (ClientData) contextPtr);

    /*
     * If we set the copyfile, disassociate it from the context.
     */
    if (copyFileHandleObj != NULL) {
        ClearCopyFile (contextPtr);
    }
    return status;

  argError:
    return TclX_WrongArgs (interp, objv [0],
		           "?-copyfile filehandle? contexthandle filehandle");
}

/*-----------------------------------------------------------------------------
 * FileScanCleanUp --
 *
 *    Called when the interpreter is deleted to cleanup all filescan
 * resources
 *-----------------------------------------------------------------------------
 */
static void
FileScanCleanUp (clientData, interp)
    ClientData  clientData;
    Tcl_Interp *interp;
{
    scanContext_t **tableEntryPtr;
    int             walkKey;
    
    walkKey = -1;
    while (TRUE) {
        tableEntryPtr =
            (scanContext_t **) TclX_HandleWalk ((void_pt) clientData, 
                                                &walkKey);
        if (tableEntryPtr == NULL)
            break;
        CleanUpContext ((void_pt) clientData, *tableEntryPtr);
    }
    TclX_HandleTblRelease ((void_pt) clientData);
}

/*-----------------------------------------------------------------------------
 *  TclX_FilescanInit --
 *
 *    Initialize the TCL file scanning facility..
 *-----------------------------------------------------------------------------
 */
void
TclX_FilescanInit (interp)
    Tcl_Interp *interp;
{
    void_pt  scanTablePtr;

    scanTablePtr = TclX_HandleTblInit ("context",
                                       sizeof (scanContext_t *),
                                       10);

    Tcl_CallWhenDeleted (interp, FileScanCleanUp, (ClientData) scanTablePtr);

    /*
     * Initialize the commands.
     */
    Tcl_CreateObjCommand (interp, 
			  "scanfile",
			  TclX_ScanfileObjCmd,
                          (ClientData) scanTablePtr,
                          (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp,
			  "scanmatch",
			  TclX_ScanmatchObjCmd,
                          (ClientData) scanTablePtr, 
			  (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateObjCommand (interp,
			  "scancontext",
			  TclX_ScancontextObjCmd,
                          (ClientData) scanTablePtr,
			  (Tcl_CmdDeleteProc*) NULL);
}



