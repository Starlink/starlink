/*
 * bltNsUtil.c --
 *
 *	This module implements utility procedures for namespaces
 *	in the BLT toolkit.
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

#include "bltInt.h"
#include <ctype.h>


/* Namespace related routines */

typedef struct TclInterp {
    char *result;
    Tcl_FreeProc *freeProc;
    int errorLine;
    Tcl_HashTable commandTable;
    Tcl_HashTable mathFuncTable;

    Tcl_HashTable globalTable;	/* This is the only field we care about */

    int numLevels;
    int maxNestingDepth;
} TclInterp;

/*
 * ----------------------------------------------------------------------
 *
 * Blt_NamespaceOfVariable --
 *
 *	Returns the namespace context of the vector variable.  If NULL,
 *	this indicates that the variable is local to the call frame.
 *
 *	Note the ever-dangerous manner in which we get this information.
 *	All of these structures are "private".   Now who's calling Tcl
 *	an "extension" language?
 *
 * Results:
 *	Returns the context of the namespace in an opaque type.
 *
 * ----------------------------------------------------------------------
 */

#if (TCL_MAJOR_VERSION < 8)

#ifdef ITCL_NAMESPACES

struct VarTrace;
struct ArraySearch;
struct NamespCacheRef;

typedef struct Var {
    int valueLength;
    int valueSpace;
    union {
	char *string;
	Tcl_HashTable *tablePtr;
	struct Var *upvarPtr;
    } value;
    Tcl_HashEntry *hPtr;
    int refCount;
    struct VarTrace *tracePtr;
    struct ArraySearch *searchPtr;
    int flags;

    /* >>>>>>>>>> stuff for [incr Tcl] namespaces <<<<<<<<<< */

    char *name;
    int protection;

    Itcl_Namespace *namesp;
    struct NamespCacheRef *cacheInfo;

} Var;


Tcl_Namespace *
Tcl_FindNamespace(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    Itcl_Namespace namesp;

    if (Itcl_FindNamesp(interp, name, 0, &namesp) != TCL_OK) {
	Tcl_ResetResult(interp);
	return NULL;
    }
    return (Tcl_Namespace *) namesp;
}

Tcl_Namespace *
Tcl_GetGlobalNamespace(interp)
    Tcl_Interp *interp;
{
    return (Tcl_Namespace *) Itcl_GetGlobalNamesp(interp);
}

Tcl_Namespace *
Tcl_GetCurrentNamespace(interp)
    Tcl_Interp *interp;
{
    return (Tcl_Namespace *) Itcl_GetActiveNamesp(interp);
}

Tcl_Namespace *
Blt_NamespaceOfVariable(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    Tcl_Var varToken;
    Var *varPtr;

    if (Itcl_FindVariable(interp, name, 0, &varToken) != TCL_OK) {
	return NULL;
    }
    varPtr = (Var *) varToken;
    if (varPtr == NULL) {
	return NULL;
    }
    return (Tcl_Namespace *) varPtr->namesp;
}

Tcl_CallFrame *
Blt_EnterNamespace(interp, nsPtr)
    Tcl_Interp *interp;
    Tcl_Namespace *nsPtr;
{
    Itcl_Namespace nameSpace = (Itcl_Namespace) nsPtr;

    return (Tcl_CallFrame *) Itcl_ActivateNamesp(interp, nameSpace);
}

void
Blt_LeaveNamespace(interp, framePtr)
    Tcl_Interp *interp;
    Tcl_CallFrame *framePtr;
{
    Itcl_DeactivateNamesp(interp, (Itcl_ActiveNamespace) framePtr);
}

#else

Tcl_Namespace *
Blt_NamespaceOfVariable(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    TclInterp *iPtr = (TclInterp *) interp;

    return (Tcl_Namespace *) Tcl_FindHashEntry(&(iPtr->globalTable), name);
}

Tcl_CallFrame *
Blt_EnterNamespace(interp, nsPtr)
    Tcl_Interp *interp;
    Tcl_Namespace *nsPtr;	/* Not used. */
{
    return NULL;
}

void
Blt_LeaveNamespace(interp, framePtr)
    Tcl_Interp *interp;
    Tcl_CallFrame *framePtr;
{
    /* empty */
}

Tcl_Namespace *
Tcl_GetGlobalNamespace(interp)
    Tcl_Interp *interp;
{
    return (Tcl_Namespace *) interp;
}

Tcl_Namespace *
Tcl_GetCurrentNamespace(interp)
    Tcl_Interp *interp;
{
    return (Tcl_Namespace *) interp;
}

Tcl_Namespace *
Tcl_FindNamespace(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    return (Tcl_Namespace *) interp;
}

#endif /* ITCL_NAMESPACES */

#else

struct VarTrace;
struct ArraySearch;

typedef struct Var {
    union {
	Tcl_Obj *objPtr;
	Tcl_HashTable *tablePtr;
	struct Var *linkPtr;
    } value;
    char *name;
    Tcl_Namespace *nsPtr;
    Tcl_HashEntry *hPtr;
    int refCount;
    struct VarTrace *tracePtr;
    struct ArraySearch *searchPtr;
    int flags;
} Var;

Tcl_Namespace *
Blt_NamespaceOfVariable(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    Var *varPtr;

    varPtr = (Var *) Tcl_FindNamespaceVar(interp, name, (Tcl_Namespace *) NULL, 0);
    if (varPtr == NULL) {
	return NULL;
    }
    return varPtr->nsPtr;
}

Tcl_CallFrame *
Blt_EnterNamespace(interp, nsPtr)
    Tcl_Interp *interp;
    Tcl_Namespace *nsPtr;
{
    Tcl_CallFrame *framePtr;

    framePtr = (Tcl_CallFrame *) malloc(sizeof(Tcl_CallFrame));
    assert(framePtr);
    if (Tcl_PushCallFrame(interp, framePtr, (Tcl_Namespace *) nsPtr, 0)
	!= TCL_OK) {
	free((char *)framePtr);
	return NULL;
    }
    return framePtr;
}

void
Blt_LeaveNamespace(interp, framePtr)
    Tcl_Interp *interp;
    Tcl_CallFrame *framePtr;
{
    Tcl_PopCallFrame(interp);
    free((char *)framePtr);
}

#endif /* TCL_MAJOR_VERSION < 8 */

int
Blt_ParseQualifiedName(interp, qualName, nsPtrPtr, namePtrPtr)
    Tcl_Interp *interp;
    char *qualName;
    Tcl_Namespace **nsPtrPtr;
    char **namePtrPtr;
{
    register char *p, *colon;
    Tcl_Namespace *nsPtr;

    colon = NULL;
    p = qualName + strlen(qualName);
    while (--p > qualName) {
	if ((*p == ':') && (*(p - 1) == ':')) {
	    p++;		/* just after the last "::" */
	    colon = p - 2;
	    break;
	}
    }
    if (colon == NULL) {
	*nsPtrPtr = NULL;
	*namePtrPtr = qualName;
	return TCL_OK;
    }
    *colon = '\0';
    nsPtr = Tcl_FindNamespace(interp, qualName, (Tcl_Namespace *) NULL, 0);
    *colon = ':';
    if (nsPtr == NULL) {
	return TCL_ERROR;
    }
    *nsPtrPtr = nsPtr;
    *namePtrPtr = p;
    return TCL_OK;
}

#if (TCL_MAJOR_VERSION >= 8)

typedef struct Callback {
    Tcl_HashTable clientTable;

    /* Original clientdata and delete procedure. */
    ClientData origClientData;
    Tcl_NamespaceDeleteProc *origDeleteProc;

} Callback;

#ifdef __STDC__
static Tcl_NamespaceDeleteProc NamespaceDeleteNotify;
#endif

static void
NamespaceDeleteNotify(clientData)
    ClientData clientData;
{
    Callback *callPtr = (Callback *) clientData;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;

    for (hPtr = Tcl_FirstHashEntry(&(callPtr->clientTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	Tcl_NamespaceDeleteProc *deleteProc;

	deleteProc = (Tcl_NamespaceDeleteProc *) Tcl_GetHashValue(hPtr);
	clientData = (ClientData)Tcl_GetHashKey(&(callPtr->clientTable), hPtr);
	if (deleteProc != NULL) {
	    (*deleteProc) (clientData);
	}
    }
    /* Invoke to original call back and free memory allocated for the chain */
    if (callPtr->origDeleteProc != NULL) {
	(*callPtr->origDeleteProc) (callPtr->origClientData);
    }
    Tcl_DeleteHashTable(&(callPtr->clientTable));
    free((char *)callPtr);
}

void
Blt_DestroyNsDeleteNotify(nsPtr, clientData)
    Tcl_Namespace *nsPtr;
    ClientData clientData;
{
    if (nsPtr->deleteProc == NamespaceDeleteNotify) {
	Callback *callPtr;
	Tcl_HashEntry *hPtr;

	callPtr = (Callback *) nsPtr->clientData;
	hPtr = Tcl_FindHashEntry(&(callPtr->clientTable), (char *)clientData);
	if (hPtr != NULL) {
	    Tcl_DeleteHashEntry(hPtr);
	}
    }
}

int
Blt_CreateNsDeleteNotify(nsPtr, clientData, deleteProc)
    Tcl_Namespace *nsPtr;
    ClientData clientData;
    Tcl_NamespaceDeleteProc *deleteProc;
{
    Callback *callPtr;
    Tcl_HashEntry *hPtr;
    int isNew;

    if (nsPtr->deleteProc != NamespaceDeleteNotify) {
	/* Attach a new call back structure to the namespace. */
	callPtr = (Callback *) malloc(sizeof(Callback));
	assert(callPtr);
	callPtr->origClientData = nsPtr->clientData;
	callPtr->origDeleteProc = nsPtr->deleteProc;
	Tcl_InitHashTable(&(callPtr->clientTable), TCL_ONE_WORD_KEYS);
	nsPtr->clientData = (ClientData)callPtr;
	nsPtr->deleteProc = NamespaceDeleteNotify;
    } else {
	callPtr = (Callback *) nsPtr->clientData;
    }
    hPtr = Tcl_CreateHashEntry(&(callPtr->clientTable), (char *)clientData,
	&isNew);
    Tcl_SetHashValue(hPtr, (ClientData)deleteProc);
    return TCL_OK;
}

#endif /* TCL_MAJOR_VERSION >= 8 */

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateCommand --
 *
 *	Like Tcl_CreateCommand, but creates command in current namespace
 *	instead of global, if one isn't defined.  Not a problem with
 *	[incr Tcl] namespaces.
 *
 * Results:
 *	The return value is a token for the command, which can
 *	be used in future calls to Tcl_GetCommandName.
 *
 *----------------------------------------------------------------------
 */
Tcl_Command
Blt_CreateCommand(interp, cmdName, proc, clientData, deleteProc)
    Tcl_Interp *interp;		/* Token for command interpreter returned by
				 * a previous call to Tcl_CreateInterp. */
    char *cmdName;		/* Name of command. If it contains namespace
				 * qualifiers, the new command is put in the
				 * specified namespace; otherwise it is put
				 * in the global namespace. */
    Tcl_CmdProc *proc;		/* Procedure to associate with cmdName. */
    ClientData clientData;	/* Arbitrary value passed to string proc. */
    Tcl_CmdDeleteProc *deleteProc;
    /* If not NULL, gives a procedure to call
				 * when this command is deleted. */
{
#if (TCL_MAJOR_VERSION >= 8)
    register char *p;

    p = cmdName + strlen(cmdName);
    while (--p > cmdName) {
	if ((*p == ':') && (*(p - 1) == ':')) {
	    p++;		/* just after the last "::" */
	    break;
	}
    }
    if (cmdName == p) {
	Tcl_DString dString;
	Tcl_Namespace *nsPtr;
	Tcl_Command cmdToken;

	Tcl_DStringInit(&dString);
	nsPtr = Tcl_GetCurrentNamespace(interp);
	Tcl_DStringAppend(&dString, nsPtr->fullName, -1);
	Tcl_DStringAppend(&dString, "::", -1);
	Tcl_DStringAppend(&dString, cmdName, -1);
	cmdToken = Tcl_CreateCommand(interp, Tcl_DStringValue(&dString), proc,
	    clientData, deleteProc);
	Tcl_DStringFree(&dString);
	return cmdToken;
    }
#endif
    return Tcl_CreateCommand(interp, cmdName, proc, clientData, deleteProc);
}

#if (TCL_MAJOR_VERSION < 8)

/*ARGSUSED*/
Tcl_Command
Tcl_FindCommand(interp, cmdName, nsPtr, flags)
    Tcl_Interp *interp;
    char *cmdName;
    Tcl_Namespace *nsPtr;	/* Not used. */
    int flags;			/* Not used. */
{
    Tcl_HashEntry *hPtr;
    TclInterp *iPtr = (TclInterp *) interp;

    hPtr = Tcl_FindHashEntry(&iPtr->commandTable, cmdName);
    if (hPtr == NULL) {
	return NULL;
    }
    return (Tcl_Command) Tcl_GetHashValue(hPtr);
}

#endif /* TCL_MAJOR_VERSION < 8 */
