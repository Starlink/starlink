/*
 * bltInit.c --
 *
 *	This module initials the BLT toolkit, registering its commands
 *	with the Tcl/Tk interpreter.
 *
 * Copyright 1991-1998 Lucent Technologies, Inc.
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
 */

#include <bltInt.h>

#ifdef __STDC__
static Tcl_MathProc ExprMinProc, ExprMaxProc;
#endif

Tk_Window bltMainWindow;

static Tcl_AppInitProc *initProcArr[] =
{
#ifndef NO_HTEXT
    Blt_HtextInit,
#endif
#ifndef NO_GRAPH
    Blt_GraphInit,
#endif
#ifndef NO_TABLE
    Blt_TableInit,
#endif
#ifndef NO_BUSY
    Blt_BusyInit,
#endif
#ifndef NO_WINOP
    Blt_WinopInit,
#endif
#ifndef NO_BITMAP
    Blt_BitmapInit,
#endif
#ifndef NO_BGEXEC
    Blt_BgexecInit,
#endif
#ifndef NO_DRAGDROP
    Blt_DndInit,
#endif
#ifndef NO_DEBUG
    Blt_DebugInit,
#endif
#ifndef NO_WATCH
    Blt_WatchInit,
#endif
#ifndef NO_CONTAINER
    Blt_ContainerInit,
#endif
#ifndef NO_VECTOR
    Blt_VectorInit,
#endif
#ifndef NO_SPLINE
    Blt_SplineInit,
#endif
#ifndef NO_BELL
    Blt_BeepInit,
#endif
#ifndef NO_CUTBUFFER
    Blt_CutbufferInit,
#endif
#ifndef NO_TILEFRAME
    Blt_FrameInit,
#endif
#ifndef NO_TILEBUTTON
    Blt_ButtonInit,
#endif
#ifndef NO_TILESCROLLBAR
    Blt_ScrollbarInit,
#endif
#ifndef NO_HIERBOX
    Blt_HierboxInit,
#endif
#ifndef NO_TABSET
    Blt_TabsetInit,
#endif
#if _MSC_VER
#ifndef NO_PRINTER
    Blt_PrinterInit,
#endif
#endif
#if (BLT_MAJOR_VERSION == 3)
#ifndef NO_MOUNTAIN
    Blt_MountainInit,
#ifndef NO_TED
    Blt_TedInit,
#endif
#endif
#endif
    (Tcl_AppInitProc *) NULL
};

/* ARGSUSED */
static int
ExprMinProc(clientData, interp, argsPtr, resultPtr)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    Tcl_Value *argsPtr;
    Tcl_Value *resultPtr;
{
    Tcl_Value *op1Ptr, *op2Ptr;

    op1Ptr = argsPtr, op2Ptr = argsPtr + 1;
    if ((op1Ptr->type == TCL_INT) && (op2Ptr->type == TCL_INT)) {
	resultPtr->intValue = MIN(op1Ptr->intValue, op2Ptr->intValue);
	resultPtr->type = TCL_INT;
    } else {
	double a, b;

	a = (op1Ptr->type == TCL_INT)
	    ? (double)op1Ptr->intValue : op1Ptr->doubleValue;
	b = (op2Ptr->type == TCL_INT)
	    ? (double)op2Ptr->intValue : op2Ptr->doubleValue;
	resultPtr->doubleValue = MIN(a, b);
	resultPtr->type = TCL_DOUBLE;
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
ExprMaxProc(clientData, interp, argsPtr, resultPtr)
    ClientData clientData;	/* Unused */
    Tcl_Interp *interp;
    Tcl_Value *argsPtr;
    Tcl_Value *resultPtr;
{
    Tcl_Value *op1Ptr, *op2Ptr;

    op1Ptr = argsPtr, op2Ptr = argsPtr + 1;
    if ((op1Ptr->type == TCL_INT) && (op2Ptr->type == TCL_INT)) {
	resultPtr->intValue = MAX(op1Ptr->intValue, op2Ptr->intValue);
	resultPtr->type = TCL_INT;
    } else {
	double a, b;

	a = (op1Ptr->type == TCL_INT)
	    ? (double)op1Ptr->intValue : op1Ptr->doubleValue;
	b = (op2Ptr->type == TCL_INT)
	    ? (double)op2Ptr->intValue : op2Ptr->doubleValue;
	resultPtr->doubleValue = MAX(a, b);
	resultPtr->type = TCL_DOUBLE;
    }
    return TCL_OK;
}

#ifndef BLT_LIBRARY
#define BLT_LIBRARY ""
#endif

static char libPath[1024] =
{
    BLT_LIBRARY
};


/*
 * Script to set the BLT library path in the variable global "blt_library"
 */

static char initScript[] =
{"\n\
global blt_library  \n\
if { [info exists blt_library] } { \n\
    return \n\
} \n\
global tk_library env \n\
if { [info exists env(BLT_LIBRARY)] } { \n\
    set blt_library $env(BLT_LIBRARY) \n\
} elseif { [file isdir $default] } { \n\
    set blt_library $default \n\
} elseif { [info exists tk_library] } { \n\
    set blt_library [file join [file dirname $tk_library] blt$blt_version] \n\
} elseif { [info exists env(TK_LIBRARY)] } { \n\
    set blt_library [file join [file dirname $env(TK_LIBRARY)] blt$blt_version] \n\
}\n"
};

static int
GetVersionInfo(interp)
    Tcl_Interp *interp;
{
    /* Check that we have compiled and linked against the same version of Tk. */
    if (Tcl_PkgRequire(interp, "Tk", TK_VERSION, 1) == NULL) {
	return TCL_ERROR;
    }
    /* Set the version variable. We'll use it in the following script. */
    if (Tcl_SetVar(interp, "blt_version", BLT_VERSION, TCL_GLOBAL_ONLY) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_SetVar(interp, "default", libPath, TCL_GLOBAL_ONLY) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_Eval(interp, initScript) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#if (TCL_MAJOR_VERSION >= 8)

/*LINTLIBRARY*/
static Tcl_Namespace *spacePtr;

#ifdef WIN32
extern int bltPlatformId;

/*
 *----------------------------------------------------------------------
 *
 * DllMain --
 *
 *	This wrapper function is used by Windows to invoke the
 *	initialization code for the DLL.
 *
 * Results:
 *	Returns TRUE;
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
BOOL APIENTRY
DllMain(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    return TRUE;
}

#endif

EXPORT
int
Blt_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    Tk_Window tkwin;
    register Tcl_AppInitProc **procPtrPtr;
    Tcl_ValueType argTypes[2];

    if (GetVersionInfo(interp) != TCL_OK) {
	return TCL_ERROR;
    }
    bltMainWindow = Tk_MainWindow(interp);
    spacePtr = Tcl_CreateNamespace(interp, "blt::tile", (ClientData)0,
	(Tcl_NamespaceDeleteProc *) NULL);
    if (spacePtr == NULL) {
	return TCL_ERROR;
    }
    for (procPtrPtr = initProcArr; *procPtrPtr != NULL; procPtrPtr++) {
	if ((**procPtrPtr) (interp) != TCL_OK) {
	    Tcl_DeleteNamespace(spacePtr);
	    return TCL_ERROR;
	}
    }
    if (Tcl_PkgProvide(interp, "BLT", BLT_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }
    argTypes[0] = argTypes[1] = TCL_EITHER;
    Tcl_CreateMathFunc(interp, "min", 2, argTypes, ExprMinProc, (ClientData)0);
    Tcl_CreateMathFunc(interp, "max", 2, argTypes, ExprMaxProc, (ClientData)0);

    tkwin = Tk_MainWindow(interp);
    Blt_InitEpsCanvasItem(interp);
    Blt_InitBitmapGC(interp, tkwin);
    return TCL_OK;
}

#else

/*LINTLIBRARY*/
EXPORT
int
Blt_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    Tk_Window tkwin;
    register Tcl_AppInitProc **procPtrPtr;
    Tcl_ValueType argTypes[2];

#ifdef ITCL_NAMESPACES
    Itcl_Namespace dummy, spaceId;	/* Token for "blt" namespace
				    * created, used to delete the
				    * namespace on errors. */
#endif
    if (GetVersionInfo(interp) != TCL_OK) {
	return TCL_ERROR;
    }
    bltMainWindow = Tk_MainWindow(interp);
#ifdef ITCL_NAMESPACES
    if (Itcl_CreateNamesp(interp, "blt", (ClientData)0,
	    (Itcl_DeleteProc *) 0, &spaceId) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Itcl_CreateNamesp(interp, "blt::tile", (ClientData)0,
	    (Itcl_DeleteProc *) 0, &dummy) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
    if (Tcl_PkgProvide(interp, "BLT", BLT_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }
    for (procPtrPtr = initProcArr; *procPtrPtr != NULL; procPtrPtr++) {
	if ((**procPtrPtr) (interp) != TCL_OK) {
#ifdef ITCL_NAMESPACES
	    Itcl_DeleteNamesp(spaceId);
#endif
	    return TCL_ERROR;
	}
    }
    argTypes[0] = argTypes[1] = TCL_EITHER;
    Tcl_CreateMathFunc(interp, "min", 2, argTypes, ExprMinProc, (ClientData)0);
    Tcl_CreateMathFunc(interp, "max", 2, argTypes, ExprMaxProc, (ClientData)0);

    tkwin = Tk_MainWindow(interp);
    Blt_InitEpsCanvasItem(interp);
    Blt_InitBitmapGC(interp, tkwin);
    return TCL_OK;
}

#endif /* TCL_MAJOR_VERION >= 8 */

/*LINTLIBRARY*/
EXPORT
int
Blt_SafeInit(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    return Blt_Init(interp);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitCmd --
 *
 *      Given the name of a command, return a pointer to the
 *      clientData field of the command.
 *
 * Results:
 *      A standard TCL result. If the command is found, TCL_OK
 *	is returned and clientDataPtr points to the clientData
 *	field of the command (if the clientDataPtr in not NULL).
 *
 * Side effects:
 *      If the command is found, clientDataPtr is set to the address
 *	of the clientData of the command.  If not found, an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
Tcl_Command
Blt_InitCmd(interp, nameSpace, specPtr)
    Tcl_Interp *interp;
    char *nameSpace;
    Blt_CmdSpec *specPtr;
{
    Tk_Window tkwin;
    char *cmdPath;
    Tcl_DString dString;
    Tcl_Command cmdToken;

    Tcl_DStringInit(&dString);
#if HAVE_NAMESPACES
    if (nameSpace != NULL) {
	Tcl_DStringAppend(&dString, nameSpace, -1);
    }
    Tcl_DStringAppend(&dString, "::", -1);
#endif /* HAVE_NAMESPACES */
    Tcl_DStringAppend(&dString, specPtr->name, -1);

    cmdPath = Tcl_DStringValue(&dString);
    cmdToken = Tcl_FindCommand(interp, cmdPath, (Tcl_Namespace *) NULL, 0);
    if (cmdToken != NULL) {
	Tcl_DStringFree(&dString);
	return cmdToken;	/* Assume command was already initialized */
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, ": \"", cmdPath, "\" requires Tk", (char *)NULL);
	return NULL;
    }
    if (specPtr->clientData == (ClientData)0) {
	specPtr->clientData = (ClientData)tkwin;
    }
    cmdToken = Tcl_CreateCommand(interp, cmdPath, specPtr->cmdProc,
	specPtr->clientData, specPtr->cmdDeleteProc);
    Tcl_DStringFree(&dString);

#if (HAVE_NAMESPACES) && (TCL_MAJOR_VERSION >= 8)
    {
	Tcl_Namespace *nsPtr;
	int dontResetList = 0;

	nsPtr = Tcl_FindNamespace(interp, nameSpace, (Tcl_Namespace *) NULL,
	    TCL_LEAVE_ERR_MSG);
	if (nsPtr == NULL) {
	    return NULL;
	}
	if (Tcl_Export(interp, nsPtr, specPtr->name, dontResetList) != TCL_OK) {
	    return NULL;
	}
    }
#endif /* TCL_MAJOR_VERSION >= 8 */
    return cmdToken;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitCmds --
 *
 *      Given the name of a command, return a pointer to the
 *      clientData field of the command.
 *
 * Results:
 *      A standard TCL result. If the command is found, TCL_OK
 *	is returned and clientDataPtr points to the clientData
 *	field of the command (if the clientDataPtr in not NULL).
 *
 * Side effects:
 *      If the command is found, clientDataPtr is set to the address
 *	of the clientData of the command.  If not found, an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
int
Blt_InitCmds(interp, nameSpace, specPtr, numCmds)
    Tcl_Interp *interp;
    char *nameSpace;
    Blt_CmdSpec *specPtr;
    int numCmds;
{
    register int i;

    for (i = 0; i < numCmds; i++) {
	if (Blt_InitCmd(interp, nameSpace, specPtr) == NULL) {
	    return TCL_ERROR;
	}
	specPtr++;
    }
    return TCL_OK;
}
