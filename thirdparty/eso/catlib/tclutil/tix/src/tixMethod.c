/*
 * tixMethod.c --
 *
 *	Handle the calling of class methods.
 *
 *	Implements the basic OOP class mechanism for the Tix Intrinsics.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */


/* ToDo:
 *
 * 1) Tix_CallMethod() needs to be re-written
 *
 */
#include <tixPort.h>
#include <tclInt.h>
#include <tk.h>
#include <tixInt.h>

#define GetMethodTable(interp) (_TixGetHashTable(interp, "tixMethodTab", MethodTableDeleteProc))

static int		Tix_CallMethodByContext _ANSI_ARGS_((
			    Tcl_Interp * interp, char * context,
			    char * widRec, char * method, int argc,
			    char ** argv));
static void		Tix_RestoreContext _ANSI_ARGS_((
			    Tcl_Interp * interp, char * widRec,
			    char * oldContext));
static void		Tix_SetContext _ANSI_ARGS_((
			    Tcl_Interp * interp, char * widRec,
			    char * newContext));
static char *		Tix_SaveContext _ANSI_ARGS_((Tcl_Interp * interp,
			    char * widRec));
static void		MethodTableDeleteProc _ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp));

/*
 *
 * argv[1] = widget record 
 * argv[2] = method
 * argv[3+] = args
 *
 */
TIX_DEFINE_CMD(Tix_CallMethodCmd)
{
    char * context;
    char * newContext;
    char * widRec = argv[1];
    char * method = argv[2];
    int    result;

    if (argc<3) {
	return Tix_ArgcError(interp, argc, argv, 1, "w method ...");
    }
 
    if ((context = (char*)GET_RECORD(interp, widRec, "className")) == NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "invalid object reference \"", widRec,
	    "\"", (char*)NULL);
	return TCL_ERROR;
    }

    newContext = Tix_FindMethod(interp, context, method);

    if (newContext) {
	result = Tix_CallMethodByContext(interp, newContext, widRec, method,
	    argc-3, argv+3);
    } else {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "cannot call method \"", method,
	    "\" for context \"", context, "\".", (char*)NULL);
	Tcl_SetVar(interp, "errorInfo", interp->result, TCL_GLOBAL_ONLY);
	result = TCL_ERROR;
    }

    return result;
}

/*
 *
 * argv[1] = widget record 
 * argv[2] = method
 * argv[3+] = args
 *
 */
TIX_DEFINE_CMD(Tix_ChainMethodCmd)
{
    char * context;
    char * superClassContext;
    char * newContext;
    char * widRec = argv[1];
    char * method = argv[2];
    int    result;

    if (argc<3) {
	return Tix_ArgcError(interp, argc, argv, 1, "w method ...");
    }

    if ((context = Tix_GetContext(interp, widRec)) == NULL) {
	return TCL_ERROR;
    }

    if (Tix_SuperClass(interp, context, &superClassContext) != TCL_OK) {
	return TCL_ERROR;
    }

    if (superClassContext == NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "no superclass exists for context \"",
	    context, "\".", (char*)NULL);
	result = TCL_ERROR;
	goto done;
    }

    newContext = Tix_FindMethod(interp, superClassContext, method);

    if (newContext) {
	result = Tix_CallMethodByContext(interp, newContext, widRec,
	    method, argc-3, argv+3);
    } else {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "cannot chain method \"", method,
	    "\" for context \"", context, "\".", (char*)NULL);
	Tcl_SetVar(interp, "errorInfo", interp->result, TCL_GLOBAL_ONLY);
	result = TCL_ERROR;
	goto done;
    }

  done:
    return result;
}

/*
 *
 * argv[1] = widget record 
 * argv[2] = class (context)
 * argv[3] = method
 *
 */
TIX_DEFINE_CMD(Tix_GetMethodCmd)
{
    char * newContext;
    char * context= argv[2];
    char * method = argv[3];
    char * cmdName;

    if (argc!=4) {
	return Tix_ArgcError(interp, argc, argv, 1, "w class method");
    }

    newContext = Tix_FindMethod(interp, context, method);

    if (newContext) {
	cmdName = Tix_GetMethodFullName(newContext, method);
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, cmdName, NULL);
	ckfree(cmdName);
    } else {
	Tcl_SetResult(interp, "", TCL_STATIC);
    }

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_FindMethod
 *
 *	Starting with class "context", find the first class that defines
 * the method. This class must be the same as the class "context" or
 * a superclass of the class "context".
 */
char *
Tix_FindMethod(interp, context, method)
    Tcl_Interp * interp;
    char * context;
    char * method;
{
    char      * theContext;
    int    	isNew;
    char      * key;
    Tcl_HashEntry *hashPtr;

    key = Tix_GetMethodFullName(context, method);
    hashPtr = Tcl_CreateHashEntry(GetMethodTable(interp), key, &isNew);
    ckfree(key);

    if (!isNew) {
	theContext = (char *) Tcl_GetHashValue(hashPtr);
    } else {
	for (theContext = context; theContext;) {
	    if (Tix_ExistMethod(interp, theContext, method)) {
		break;
	    }
	    /* Go to its superclass and see if it has the method */
	    if (Tix_SuperClass(interp, theContext, &theContext) != TCL_OK) {
		return NULL;
	    }
	    if (theContext == NULL) {
		return NULL;
	    }
	}

	if (theContext != NULL) {
	    /*
	     * theContext may point to the stack. We have to put it
	     * in some more permanent place.
	     */
	    theContext = (char*)strdup(theContext);
	}
	Tcl_SetHashValue(hashPtr, (char*)theContext);
    }

    return theContext;
}

/*----------------------------------------------------------------------
 * Tix_CallMethod
 *
 *	Starting with class "context", find the first class that defines
 * the method. Call this method.
 */
int Tix_CallMethod(interp, context, widRec, method, argc, argv)
    Tcl_Interp * interp;
    char * context;
    char * widRec;
    char * method;
    int argc;
    char ** argv;
{
    char * targetContext;

    targetContext = Tix_FindMethod(interp, context, method);
    if (targetContext != NULL) {
	return Tix_CallMethodByContext(interp, targetContext, widRec, method,
	    argc, argv);
    }
    else {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "cannot call method \"", method,
	    "\" for context \"", context, "\".", (char*)NULL);
	Tcl_SetVar(interp, "errorInfo", interp->result, TCL_GLOBAL_ONLY);
	return TCL_ERROR;
    }
}

/*----------------------------------------------------------------------
 * Tix_FindConfigSpec
 *
 *	Starting with class "classRec", find the first class that defines
 * the option flag. This class must be the same as the class "classRec" or
 * a superclass of the class "classRec".
 */

/* save the old context: calling a method of a superclass will
 * change the context of a widget.
 */
static char * Tix_SaveContext(interp, widRec)
    Tcl_Interp * interp;
    char * widRec;
{
    char * context;

    if ((context = (char*)GET_RECORD(interp, widRec, "context")) == NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "invalid object reference \"", widRec,
	    "\"", (char*)NULL);
	return NULL;
    }
    else {
	return (char*)strdup(context);
    }
}

static void Tix_RestoreContext(interp, widRec, oldContext)
    Tcl_Interp * interp;
    char * widRec;
    char * oldContext;
{
    SET_RECORD(interp, widRec, "context", oldContext);
    ckfree(oldContext);
}

static void Tix_SetContext(interp, widRec, newContext)
    Tcl_Interp * interp;
    char * widRec;
    char * newContext;
{
    SET_RECORD(interp, widRec, "context", newContext);
}


char * Tix_GetContext(interp, widRec)
    Tcl_Interp * interp;
    char * widRec;
{
    char * context;

    if ((context = (char*)GET_RECORD(interp, widRec, "context")) == NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "invalid object reference \"", widRec,
	    "\"", (char*)NULL);
	return NULL;
    } else {
	return context;
    }
}

int Tix_SuperClass(interp, class, superClass_ret)
    Tcl_Interp * interp;
    char * class;
    char ** superClass_ret;
{
    char * superclass;

    if ((superclass = (char*)GET_RECORD(interp, class, "superClass")) == NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "invalid class \"", class,
	    "\"; ", (char*)NULL);
	return TCL_ERROR;
    }

    if (strlen(superclass) == 0) {
	*superClass_ret = (char*) NULL;
    } else {
	*superClass_ret =  superclass;
    }

    return TCL_OK;
}

char * Tix_GetMethodFullName(context, method)
    char * context;
    char * method;
{
    char * buff;
    int    max;
    int    conLen;

    conLen = strlen(context);
    max = conLen + strlen(method) + 3;
    buff = (char*)ckalloc(max * sizeof(char));

    strcpy(buff, context);
    strcpy(buff+conLen, ":");
    strcpy(buff+conLen+1, method);

    return buff;
}

#undef ITCL_2

#ifndef ITCL_2

#define Tix_GetCommandInfo Tcl_GetCommandInfo

#else
/*
 *----------------------------------------------------------------------
 *
 * Tix_GetCommandInfo --
 *
 *	Returns various information about a Tcl command. Modified from
 *	Tcl_GetCommandInfo to work with ITcl 2.0. Always work in the global
 *	name space.
 *
 * Results:
 *	If cmdName exists in interp, then *infoPtr is modified to
 *	hold information about cmdName and 1 is returned.  If the
 *	command doesn't exist then 0 is returned and *infoPtr isn't
 *	modified.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
int Tix_GetCommandInfo(interp, cmdName, infoPtr)
    Tcl_Interp *interp;
    char *cmdName;
    Tcl_CmdInfo *infoPtr;
{
    register Interp *iPtr = (Interp *) interp;
    int result;
    CallFrame *savedVarFramePtr;
    Itcl_ActiveNamespace nsToken;

    savedVarFramePtr = iPtr->varFramePtr;
    iPtr->varFramePtr = NULL;

    nsToken = Itcl_ActivateNamesp(interp, (Itcl_Namespace)iPtr->globalNs);
    if (nsToken == NULL) {
        result = 0;
    }
    else {
        result = Tcl_GetCommandInfo(interp, cmdName, infoPtr);
        Itcl_DeactivateNamesp(interp, nsToken);
    }

    iPtr->varFramePtr = savedVarFramePtr;
    return result;
}
#endif

int Tix_ExistMethod(interp, context, method)
    Tcl_Interp * interp;
    char * context;
    char * method;
{
    char * cmdName;
    Tcl_CmdInfo dummy;
    int exist;

    cmdName = Tix_GetMethodFullName(context, method);
    exist = Tix_GetCommandInfo(interp, cmdName, &dummy);

    if (!exist) {
	if (Tix_GlobalVarEval(interp, "auto_load ", cmdName, 
	        (char*)NULL)!= TCL_OK) {
	    goto done;
	}
	if (strcmp(interp->result, "1") == 0) {
	    exist = 1;
	}
    }

  done:
    ckfree(cmdName);
    Tcl_SetResult(interp, NULL, TCL_STATIC);
    return exist;
}

/* %% There is a dirty version that uses the old argv, without having to
 * malloc a new argv.
 */
static int Tix_CallMethodByContext(interp, context, widRec, method, argc, argv)
    Tcl_Interp * interp;
    char * context;
    char * widRec;
    char * method;
    int    argc;
    char ** argv;
{
    char  * cmdName;
    int     i, result;
    char  * oldContext;
    char ** newArgv;

    if ((oldContext = Tix_SaveContext(interp, widRec)) == NULL) {
	return TCL_ERROR;
    }
    Tix_SetContext(interp, widRec, context);

    cmdName = Tix_GetMethodFullName(context, method);

    /* Create a new argv list */
    newArgv = (char**)ckalloc((argc+2)*sizeof(char*));
    newArgv[0] = cmdName;
    newArgv[1] = widRec;
    for (i=0; i< argc; i++) {
	newArgv[i+2] = argv[i];
    }
    result = Tix_EvalArgv(interp, argc+2, newArgv);

    Tix_RestoreContext(interp, widRec, oldContext);
    ckfree((char*)newArgv);
    ckfree(cmdName);

    return result;
}

#ifndef ITCL_2

#define Tix_GlobalEvalArgv(interp, cmdInfoPtr, argc, argv) \
    (*(cmdInfoPtr)->proc)((cmdInfoPtr)->clientData, interp, argc, argv)

#else

EXTERN int		Tix_GlobalEvalArgv _ANSI_ARGS_((Tcl_Interp * interp,
			    Tcl_CmdInfo * cmdInfoPtr, int argc));

int
Tix_GlobalEvalArgv(interp, cmdInfoPtr, argc, argv)
    Tcl_Interp * interp;
    Tcl_CmdInfo * cmdInfoPtr;
    int argc;
    char ** argv;
{
    register Interp *iPtr = (Interp *) interp;
    int result;
    CallFrame *savedVarFramePtr;
    Itcl_ActiveNamespace nsToken;

    savedVarFramePtr = iPtr->varFramePtr;
    iPtr->varFramePtr = NULL;

    nsToken = Itcl_ActivateNamesp(interp, (Itcl_Namespace)iPtr->globalNs);
    if (nsToken == NULL) {
        result = TCL_ERROR;
    }
    else {
	result = (*cmdInfoPtr->proc)(cmdInfoPtr->clientData,interp,argc,argv);
        Itcl_DeactivateNamesp(interp, nsToken);
    }

    iPtr->varFramePtr = savedVarFramePtr;
    return result;
}
#endif /* ITCL_2 */

int Tix_EvalArgv(interp, argc, argv)
    Tcl_Interp * interp;
    int argc;
    char ** argv;
{
    Tcl_CmdInfo cmdInfo;

    if (!Tix_GetCommandInfo(interp, argv[0], &cmdInfo)) {
	char * cmdArgv[2];

	/*
	 * This comand is not defined yet -- looks like we have to auto-load it
	 */
	if (!Tix_GetCommandInfo(interp, "auto_load", &cmdInfo)) {
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, "cannot execute command \"auto_load\"",
		NULL);
	    return TCL_ERROR;
	}

	cmdArgv[0] = "auto_load";
	cmdArgv[1] = argv[0];

	if ((*cmdInfo.proc)(cmdInfo.clientData, interp, 2, cmdArgv)!= TCL_OK){ 
	    return TCL_ERROR;
	}

	if (!Tix_GetCommandInfo(interp, argv[0], &cmdInfo)) {
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, "cannot autoload command \"",
		argv[0], "\"",NULL);
	    return TCL_ERROR;
	}
    }

    return Tix_GlobalEvalArgv(interp, &cmdInfo, argc, argv);
}

char *
Tix_FindPublicMethod(interp, cPtr, method)
    Tcl_Interp * interp;
    TixClassRecord * cPtr;
    char * method;
{
    int i;
    int len = strlen(method);

    for (i=0; i<cPtr->nMethods; i++) {
	if (cPtr->methods[i][0] == method[0] &&
	    strncmp(cPtr->methods[i], method, len)==0) {
	    return cPtr->methods[i];
	}
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 * MethodTableDeleteProc --
 *
 *	This procedure is called when the interp is about to
 *	be deleted. It cleans up the hash entries and destroys the hash
 *	table.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	All class method contexts are deleted for this interpreter.
 *----------------------------------------------------------------------
 */

static void
MethodTableDeleteProc(clientData, interp)
    ClientData clientData;
    Tcl_Interp *interp;
{
    Tcl_HashTable * methodTablePtr = (Tcl_HashTable*)clientData;
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry *hashPtr;
    char * context;

    for (hashPtr = Tcl_FirstHashEntry(methodTablePtr, &hashSearch);
	 hashPtr;
	 hashPtr = Tcl_NextHashEntry(&hashSearch)) {

	context = (char*)Tcl_GetHashValue(hashPtr);
	if (context) {
	    ckfree(context);
	}
	Tcl_DeleteHashEntry(hashPtr);
    }
    Tcl_DeleteHashTable(methodTablePtr);
    ckfree((char*)methodTablePtr);
}
