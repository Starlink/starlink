/*
 * tixWidget.c --
 *
 *	Constructs Tix-based compound widgets
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tixPort.h>
#include <tclInt.h>
#include <tixInt.h>
#include <tixItcl.h>

static int			ParseOptions _ANSI_ARGS_((
				    Tcl_Interp * interp,TixClassRecord * cPtr,
				    char *widRec, int argc, char** argv));

TIX_DECLARE_CMD(Tix_InstanceCmd);

/*----------------------------------------------------------------------
 * Tix_CreateWidgetCmd
 *
 * 	Create an instance object of a Tix widget class.
 *
 * argv[0]  = object name.
 * argv[1+] = args
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_CreateWidgetCmd)
{
    TixClassRecord * cPtr =(TixClassRecord *)clientData;
    TixConfigSpec * spec;
    char * widRec = NULL;
    char * rootCmd = NULL;
    char * tmpArgv[3];
    char * value;
    int i;
    int code = TCL_OK;
    Tk_Window mainWin = Tk_MainWindow(interp);
    Tcl_DString ds;

    DECLARE_ITCL_NAMESP(nameSp, interp);

    if (argc <= 1) {
	return Tix_ArgcError(interp, argc, argv, 1, "pathname ?arg? ...");
    } else {
	widRec = argv[1];
    }

    if (Tk_NameToWindow(interp, widRec, mainWin) != NULL) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "window name \"", widRec,
	    "\" already exists", NULL);
	return TCL_ERROR;
    } else {
	Tcl_ResetResult(interp);
    }

    if (!TixItclSetGlobalNameSp(&nameSp, interp)) {
        code = TCL_ERROR;
	goto done;
    }

    /*
     * Before doing anything, let's reset the TCL result, errorInfo,
     * errorCode, etc.
     */
    Tcl_SetVar2(interp, "errorInfo", NULL, "", TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, "errorCode", NULL, "", TCL_GLOBAL_ONLY);
    Tcl_ResetResult(interp);

    /* Set up the widget record */
    rootCmd = ckalloc(strlen(widRec)+10);
    sprintf(rootCmd, "%s:root", widRec);
    Tcl_SetVar2(interp, widRec, "className", cPtr->className, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, widRec, "ClassName", cPtr->ClassName, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, widRec, "context",   cPtr->className, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, widRec, "w:root",    widRec,  	      TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, widRec, "rootCmd",   rootCmd,         TCL_GLOBAL_ONLY);

    /* We need to create the root widget in order to parse the options
     * database
     */
    if (Tix_CallMethod(interp, cPtr->className, widRec, "CreateRootWidget",
	    argc-2, argv+2) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }

    /* Parse the options specified in the option database and supplied
     * in the command line.
     */
    Tcl_ResetResult(interp);
    if (ParseOptions(interp, cPtr, widRec, argc-2, argv+2) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }

    /* Rename the root widget command and create a new TCL command for
     * this widget
     */

#ifndef TK_8_0_OR_LATER
    tmpArgv[0] = "rename";
    tmpArgv[1] = widRec;
    tmpArgv[2] = rootCmd;

    if (Tcl_RenameCmd((ClientData)0, interp, 3, tmpArgv) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }
#else
    Tcl_DStringInit(&ds);
    Tcl_DStringAppendElement(&ds, "rename");
    Tcl_DStringAppendElement(&ds, widRec);
    Tcl_DStringAppendElement(&ds, rootCmd);
	
    if (Tcl_Eval(interp, ds.string) != TCL_OK) {
	Tcl_DStringFree(&ds);
	code = TCL_ERROR;
	goto done;
    } else {
	Tcl_DStringFree(&ds);
    }
#endif

    Tcl_CreateCommand(interp, widRec, Tix_InstanceCmd,
	(ClientData)cPtr, NULL);

    /* Now call the initialization methods defined by the Tix Intrinsics
     */
    if (Tix_CallMethod(interp, cPtr->className, widRec, "InitWidgetRec",
	    0, 0) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }

    if (Tix_CallMethod(interp, cPtr->className, widRec, "ConstructWidget",
	    0, 0) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }

    if (Tix_CallMethod(interp, cPtr->className, widRec, "SetBindings",
		0, 0) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }

    /* The widget has been successfully initialized. Now call the config
     * method for all -forceCall options
     */
    for (i=0; i<cPtr->nSpecs; i++) {
	spec = cPtr->specs[i];
	if (spec->forceCall) {
	    value = (char*)Tcl_GetVar2(interp, widRec, spec->argvName,
		TCL_GLOBAL_ONLY);
	    if (Tix_CallConfigMethod(interp, cPtr, widRec, spec,
		    value)!=TCL_OK){
		code = TCL_ERROR;
		goto done;
	    }
	}
    }

    Tcl_SetResult(interp, widRec, TCL_VOLATILE);

  done:

    if (code != TCL_OK) {
	/* %% TCL CORE USED !! %% */
	Interp *iPtr = (Interp *) interp;
	char * oldResult, * oldErrorInfo, * oldErrorCode;
	Tk_Window topLevel, tkwin;

	/* We need to save the old error message because
	 * interp->result may be changed by some of the following function
	 * calls.
	 */
	if (interp->result) {
	    oldResult = (char*)strdup(interp->result);
#if 0
	    printf("%s -->\n%s\n", widRec, oldResult);
#endif
	} else {
	    oldResult = NULL;
	}
	oldErrorInfo = (char*)Tcl_GetVar2(interp, "errorInfo", NULL, TCL_GLOBAL_ONLY);
	oldErrorCode = (char*)Tcl_GetVar2(interp, "errorCode", NULL, TCL_GLOBAL_ONLY);

	Tcl_ResetResult(interp);

	/* (1) window */
	topLevel = cPtr->mainWindow;

	if (widRec != NULL) {
	    Display *display = NULL;

	    tkwin = Tk_NameToWindow(interp, widRec, topLevel);
	    if (tkwin != NULL) {
		display = Tk_Display(tkwin);
		Tk_DestroyWindow(tkwin);
	    }

	    /* (2) widget command + root command */
	    Tcl_DeleteCommand(interp, widRec);
	    Tcl_DeleteCommand(interp, rootCmd);

	    /* (3) widget record */
	    Tcl_UnsetVar(interp, widRec, TCL_GLOBAL_ONLY);

	    if (display) {
#ifndef _WINDOWS
		XSync(display, False);
#endif
		while (1) {
		    if (Tk_DoOneEvent(TK_X_EVENTS|TK_DONT_WAIT) == 0) {
			break;
		    }
		}
	    }
	}
	if (oldResult) {
	    Tcl_SetResult(interp, oldResult, TCL_DYNAMIC);
	}
	if (oldErrorInfo && *oldErrorInfo) {
	    Tcl_SetVar2(interp, "errorInfo", NULL, oldErrorInfo,
		TCL_GLOBAL_ONLY);
	} else {
	    Tcl_SetVar2(interp, "errorInfo", NULL, oldResult,
		TCL_GLOBAL_ONLY);
	}
	if (oldErrorCode) {
	    Tcl_SetVar2(interp, "errorCode", NULL, oldErrorCode,
		TCL_GLOBAL_ONLY);
	}
	iPtr->flags |= ERR_IN_PROGRESS;
    }
    if (rootCmd) {
	ckfree(rootCmd);
    }

    TixItclRestoreGlobalNameSp(&nameSp, interp);

    return code;
}

/*----------------------------------------------------------------------
 * Subroutines for object instantiation.
 *
 *
 *----------------------------------------------------------------------
 */
static int ParseOptions(interp, cPtr, widRec, argc, argv)
    Tcl_Interp * interp;
    TixClassRecord * cPtr;
    char *widRec;
    int argc;
    char** argv;
{
    int i;
    TixConfigSpec *spec;
    Tk_Window tkwin;
    char * value;

    if ((argc %2) != 0) {
	Tcl_AppendResult(interp, "missing argument for \"", argv[argc-1],
	    "\"", NULL);
	return TCL_ERROR;
    }

    if ((tkwin = Tk_NameToWindow(interp, widRec, cPtr->mainWindow)) == NULL) {
	return TCL_ERROR;
    }

    /* Set all specs by their default values */
    for (i=0; i<cPtr->nSpecs; i++) {
	spec = cPtr->specs[i];

	if (!spec->isAlias) {
	    if ((value=(char*)Tk_GetOption(tkwin,spec->dbName,spec->dbClass))==NULL) {
		value = spec->defValue;
	    }
	    if (Tix_ChangeOneOption(interp, cPtr, widRec, spec,
		value, 1, 0)!=TCL_OK) {
		return TCL_ERROR;
	    }
	}
    }

    /* Set specs according to argument line values */
    for (i=0; i<argc; i+=2) {
	spec = Tix_FindConfigSpecByName(interp, cPtr, argv[i]);

	if (spec == NULL) {	/* this is an invalid flag */
	    return TCL_ERROR;
	}
	
	if (Tix_ChangeOneOption(interp, cPtr, widRec, spec,
		argv[i+1], 0, 1)!=TCL_OK) {
	    return TCL_ERROR;
	}
    }

    return TCL_OK;
}
