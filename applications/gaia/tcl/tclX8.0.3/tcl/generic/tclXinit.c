/*
 * tclXinit.c --
 *
 * Extended Tcl initialzation and initialization utilitied.
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
 * $Id: tclXinit.c,v 8.12 1997/11/11 05:33:15 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

/*
 * Tcl procedure to search for an init for either TclX or TkX startup file.  
 * The algorithm is, with $w being either tcl or tk:
 *    o The directory specified in the environment variable ${w}X_LIBRARY,
 *      if it exists, with $w upshifted.
 *    o The specified default library directory,
 *    o [info nameofexectutable]/../../$lib/${w}/$version, best guess if
 *      default directory does not work.
 *    o [info nameofexectutable]/../../tclX$version/$w/$platform, for
 *      running before installation.  Platform is either "unix" or "win".
 * The source -rsrc commands are used when TclX has standalone support built
 * in.  It can't be ifdefed, as many cpp can't handled an #ifdef in a string.
 *
 * Parameters:
 *   o w - Which are we configuring. Either "tcl" or "tk".
 *   o defaultLib - Default path to the library directory
 *   o version - The version, which is the directory just below extdir.
 *   o noInit - If a true value, skip actually eval the init file,
 *     just set the variable.
 * Globals:
 *   o ${w}x_library - Set to the directory containing the init file.
 */
static char tclx_findinit [] =
"proc tclx_findinit {w defaultLib version noInit} {\n\
    upvar #0 env env ${w}x_library libDir tcl_platform tcl_platform\n\
    set dirs {}\n\
    set envVar [string toupper ${w}X_LIBRARY]\n\
    if [info exists env($envVar)] {lappend dirs $env($envVar)}\n\
    if [info exists env(EXT_FOLDER)] {\n\
	lappend dirs [file join $env(EXT_FOLDER) \"Tool Command Language\" ${w}X$version]\n\
    }\n\
    lappend dirs $defaultLib\n\
    set libDir {}\n\
    if ![catch {uplevel #0 source -rsrc ${w}x}] {\n\
	uplevel #0 source -rsrc ${w}x:tclIndex\n\
	return\n\
    }\n\
    set prefix [file dirname [info nameofexecutable]]\n\
    set plat [file tail $prefix]\n\
    set prefix [file dirname $prefix]\n\
    lappend dirs [file join $prefix lib ${w}X$version]\n\
    set prefix [file dirname $prefix]\n\
    lappend dirs [file join $prefix ${w}X${version} $w $plat]\n\
    lappend dirs [file join [file dirname $prefix] ${w}X${version} $w $plat]\n\
    foreach libDir $dirs {\n\
        set init [file join $libDir ${w}x.tcl]\n\
        if [file exists $init] {\n\
            if !$noInit {uplevel #0 source [list $init]}; return\n\
        }\n\
    }\n\
    set libDir {}\n\
    set msg \"Can't find ${w}x.tcl in the following directories: \n\"\n\
    foreach d $dirs {append msg \"  $d\n\"}\n\
    append msg \"This probably means that TclX wasn't installed properly.\n\"\n\
    error $msg\n\
}";

static char tclx_findinitProc [] = "tclx_findinit";

/*
 * Prototypes of internal functions.
 */
static int
InsureVarExists _ANSI_ARGS_((Tcl_Interp *interp,
                             char       *varName,
                             char       *defaultValue));

static int
InitSetup _ANSI_ARGS_((Tcl_Interp *interp));


/*-----------------------------------------------------------------------------
 * TclXRuntimeInit --
 *
 * Find the location of the init file, set the *_library Tcl variable to
 * the directory containing it and evaluate the init file.  This uses the
 * inline proc tclx_findinit defined above, cause its easier in Tcl.  See
 * that proc's documentation for a description of the search algorithm,
 *
 * Parameters:
 *   o interp - A pointer to the interpreter.
 *   o which - Either "tcl" or "tk", used to generate the names of 
 *     the environment variable, the init file and the Tcl global variable
 *     that points to the library.
 *   o defaultLib - Default path to the library directory.
 *   o version - Version string use in file paths.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
int
TclXRuntimeInit (interp, which, defaultLib, version)
    Tcl_Interp *interp;
    char       *which;
    char       *defaultLib;
    char       *version;
    
{
#define PROC_ARGC 5
    Tcl_CmdInfo cmdInfo;
    char *procArgv [PROC_ARGC + 1], *quick;
    
    /*
     * Find the init procedure.  If its not defined, define it now.
     */
    if (!Tcl_GetCommandInfo (interp, tclx_findinitProc, &cmdInfo)) {
        if (Tcl_GlobalEval (interp, tclx_findinit) != TCL_OK)
            return TCL_ERROR;
        if (!Tcl_GetCommandInfo (interp, tclx_findinitProc, &cmdInfo)) {
            panic ("can't find %s\n", tclx_findinitProc);
        }
    }

    quick = Tcl_GetVar2 (interp, "TCLXENV", "quick", TCL_GLOBAL_ONLY);
    if (quick == NULL)
        quick = "0";
    
    procArgv [0] = tclx_findinitProc;
    procArgv [1] = which;
    procArgv [2] = defaultLib;
    procArgv [3] = version;
    procArgv [4] = quick;
    procArgv [5] = NULL;

    return cmdInfo.proc (cmdInfo.clientData,
                         interp,
                         PROC_ARGC,
                         procArgv);
}

/*-----------------------------------------------------------------------------
 * TclX_EvalRCFile --
 *
 * Evaluate the file stored in tcl_RcFileName it is readable.  Exit if an
 * error occurs.
 *
 * Parameters:
 *   o interp - A pointer to the interpreter.
 *-----------------------------------------------------------------------------
 */
void
TclX_EvalRCFile (interp)
    Tcl_Interp  *interp;
{
    Tcl_DString  buffer;
    char        *path;

    path = Tcl_GetVar (interp, "tcl_rcFileName", TCL_GLOBAL_ONLY);
    if (path == NULL)
        return;

    Tcl_DStringInit (&buffer);

    path = Tcl_TranslateFileName (interp, path, &buffer);
    if (path == NULL) {
        TclX_ErrorExit (interp, 1,
                        "\n    while\ntranslating RC file name \"%.*s\"",
                         TCLX_ERR_EXIT_MSG_MAX-64, path);
    }

    if (access (path, R_OK) == 0) {
        if (TclX_Eval (interp,
                       TCLX_EVAL_GLOBAL | TCLX_EVAL_FILE |
                       TCLX_EVAL_ERR_HANDLER,
                       path) == TCL_ERROR) {
            TclX_ErrorExit (interp, 1,
                            "\n    while\nevaluating RC file \"%.*s\"",
                            TCLX_ERR_EXIT_MSG_MAX-64, path);
        }
    }
    Tcl_DStringFree(&buffer);
}

/*-----------------------------------------------------------------------------
 * TclX_ErrorExit --
 *
 * Display error information and abort when an error is returned in the
 * interp result. It uses TCLXENV(noDump) to determine if the stack should be
 * dumped.
 *
 * Parameters:
 *   o interp - A pointer to the interpreter, should contain the
 *     error message in `result'.
 *   o exitCode - The code to pass to exit.
 *   o message - If not NULL, contains a message to add to errorInfo. 
 *     Arguments will be formatted into the message, but the total maximum
 *     size is TCLX_ERR_EXIT_MSG_MAX, so use this when formatting arguments
 *     of unknown length.
 *   o ... - Arguments to format into message.
 *-----------------------------------------------------------------------------
 */
void
TclX_ErrorExit TCL_VARARGS_DEF(Tcl_Interp *, interpArg)
{
    va_list argList;
    Tcl_Interp  *interp;
    int exitCode;
    char *message, *errorStack, *noDump, *strResult;
    Tcl_Channel stdoutChan, stderrChan;
    Tcl_Obj *savedResult;

    interp = TCL_VARARGS_START (Tcl_Interp *, interpArg, argList);
    exitCode = va_arg (argList, int);
    message = va_arg (argList, char *);

    if (message != NULL) {
        char fmtMessage [TCLX_ERR_EXIT_MSG_MAX];
        vsprintf (fmtMessage, message, argList);
        Tcl_AddErrorInfo (interp, fmtMessage);
    }
    va_end (argList);

    savedResult = Tcl_GetObjResult (interp);
    Tcl_IncrRefCount (savedResult);
    strResult = Tcl_GetStringFromObj (savedResult, NULL);

    stdoutChan = Tcl_GetStdChannel (TCL_STDOUT);
    stderrChan = Tcl_GetStdChannel (TCL_STDERR);

    if (stdoutChan != NULL)
        Tcl_Flush (stdoutChan);

    if (stderrChan != NULL) {
        /*
         * Get the error stack, if available.
         */
        noDump = Tcl_GetVar2 (interp, "TCLXENV", "noDump", TCL_GLOBAL_ONLY);
        if ((noDump == NULL) || STREQU (noDump, "0")) {
            errorStack = Tcl_GetVar (interp, "errorInfo", TCL_GLOBAL_ONLY);
            if ((errorStack != NULL) && (errorStack [0] == '\0'))
                errorStack = NULL;
        } else {
            errorStack = NULL;
        }

        TclX_WriteStr (stderrChan, "Error: ");
        
        /*
         * Don't output the result if its the first thing on the error stack.
         */
        if ((errorStack == NULL) || 
            (strncmp (strResult, errorStack, strlen (strResult)) != 0)) {
            TclX_WriteStr (stderrChan, strResult);
            TclX_WriteNL (stderrChan);
        }
        if (errorStack != NULL) {
            TclX_WriteStr (stderrChan, errorStack);
            TclX_WriteNL (stderrChan);
        }
        Tcl_Flush (stderrChan);
    }

    Tcl_DecrRefCount (savedResult);
    Tcl_Exit (exitCode);
}

/*-----------------------------------------------------------------------------
 * InsureVarExists --
 *
 *   Insure that the specified global variable exists.
 *
 * Parameters:
 *   o interp  - A pointer to the interpreter.
 *   o varName - Name of the variable.
 *   o defaultValue - Value to set the variable to if it doesn't already
 *     exist.
 * Returns:
 *   TCL_OK if all is ok, TCL_ERROR if an error occured.
 *-----------------------------------------------------------------------------
 */
static int
InsureVarExists (interp, varName, defaultValue)
    Tcl_Interp *interp;
    char       *varName;
    char       *defaultValue;
{
    if (Tcl_GetVar (interp, varName, TCL_GLOBAL_ONLY) == NULL) {
        if (Tcl_SetVar (interp, varName, defaultValue, 
                        TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG) == NULL)
            return TCL_ERROR;
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------------
 * InitSetup --
 *
 *   So setup common to both normal and safe initialization.
 *-----------------------------------------------------------------------------
 */
static int
InitSetup (interp)
    Tcl_Interp *interp;
{
    if (Tcl_PkgRequire (interp, "Tcl", TCL_VERSION, 1) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_PkgProvide (interp, "Tclx", TCLX_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Make sure a certain set of variable exists.  If not, default them.
     * Tcl code often assumes that these exists.
     */
    if (InsureVarExists (interp, "errorInfo", "") == TCL_ERROR)
        return TCL_ERROR;
    if (InsureVarExists (interp, "errorCode", "") == TCL_ERROR)
        return TCL_ERROR;
    if (InsureVarExists (interp, "tcl_interactive", "0") == TCL_ERROR)
        return TCL_ERROR;
    return TCL_OK;
}


/*-----------------------------------------------------------------------------
 * Tclx_Init --
 *
 *   Initialize all Extended Tcl commands, set auto_path and source the
 * Tcl init file.
 *-----------------------------------------------------------------------------
 */
int
Tclx_Init (interp)
    Tcl_Interp *interp;
{
    if (InitSetup (interp) == TCL_ERROR)
        goto errorExit;

    if (Tclxcmd_Init (interp) == TCL_ERROR)
        goto errorExit;

    if (TclXRuntimeInit (interp,
                         "tcl",
                         TCLX_LIBRARY,
                         TCLX_FULL_VERSION) == TCL_ERROR)
        goto errorExit;

    if (Tclxlib_Init (interp) == TCL_ERROR)
        goto errorExit;

    return TCL_OK;

  errorExit:
    Tcl_AddErrorInfo (interp,
                     "\n    (while initializing TclX)");
    return TCL_ERROR;
}


/*-----------------------------------------------------------------------------
 * Tclx_SafeInit --
 *
 *   Initialize safe Extended Tcl commands.
 *-----------------------------------------------------------------------------
 */
int
Tclx_SafeInit (interp)
    Tcl_Interp *interp;
{
    if (InitSetup (interp) == TCL_ERROR)
        goto errorExit;

    if (Tclxcmd_SafeInit (interp) == TCL_ERROR)
        goto errorExit;

    return TCL_OK;

  errorExit:
    Tcl_AddErrorInfo (interp,
                     "\n    (while initializing safe TclX)");
    return TCL_ERROR;
}


