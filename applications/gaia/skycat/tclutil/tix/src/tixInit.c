/*
 * tixInit.c --
 *
 *	Initialze the internals of Tix.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tixPort.h>
#include <tixInt.h>

#ifdef ITCL_2
#include <itcl.h>
#endif

#ifdef _WINDOWS
#include <tkWinInt.h>
#endif

static Tix_TclCmd commands[] = {
    /*
     * Commands that are part of the intrinsics:
     */
    {"tixCallMethod",           Tix_CallMethodCmd},
    {"tixChainMethod",          Tix_ChainMethodCmd},
    {"tixClass",                Tix_ClassCmd},
    {"tixDisplayStyle",         Tix_ItemStyleCmd},
    {"tixDoWhenIdle",           Tix_DoWhenIdleCmd},
    {"tixDoWhenMapped",         Tix_DoWhenMappedCmd},
    {"tixFalse",                Tix_FalseCmd},
    {"tixFile",                 Tix_FileCmd},
    {"tixFlushX",           	Tix_FlushXCmd},
    {"tixForm",                 Tix_FormCmd},
    {"tixHList",                Tix_HListCmd},
    {"tixItemStyle",            Tix_ItemStyleCmd},	/* Old name */
    {"tixGeometryRequest",      Tix_GeometryRequestCmd},
    {"tixGet3DBorder",		Tix_Get3DBorderCmd},
    {"tixGetBoolean",		Tix_GetBooleanCmd},
    {"tixGetInt",		Tix_GetIntCmd},
    {"tixGetMethod",            Tix_GetMethodCmd},
    {"tixHandleOptions",        Tix_HandleOptionsCmd},
#ifndef _WINDOWS
    {"tixInputOnly",		Tix_InputOnlyCmd},
#endif
    {"tixManageGeometry",       Tix_ManageGeometryCmd},
    {"tixMapWindow",            Tix_MapWindowCmd},
    {"tixMoveResizeWindow",     Tix_MoveResizeWindowCmd},
#ifndef _WINDOWS
    {"tixMwm",     		Tix_MwmCmd},
#endif
    {"tixNoteBookFrame",        Tix_NoteBookFrameCmd},
    {"tixRaiseWindow",          Tix_RaiseWindowCmd},
    {"tixStringSub",		Tix_StringSubCmd},
    {"tixStrEq",		Tix_StrEqCmd},
    {"tixTmpLine",              Tix_TmpLineCmd},
    {"tixTrue",                 Tix_TrueCmd},
    {"tixUnmapWindow",          Tix_UnmapWindowCmd},
    {"tixWidgetClass",          Tix_ClassCmd},
    {"tixWidgetDoWhenIdle",     Tix_DoWhenIdleCmd},

#ifndef TIX_VERSION_4_0_x
    {"tixTList",     		Tix_TListCmd},
    {"tixGrid",     		Tix_GridCmd},
#endif

    {(char *) NULL,		(Tix_CmdProc)NULL}
};

typedef struct {
    int		isBeta;
    char      * binding;
    int		isDebug;
    char      * fontSet;
    char      * tixlibrary;
    char      * scheme;
    char      * schemePriority;
} OptionStruct;

static OptionStruct tixOption;

/*
 * TIX_DEF_FONTSET and TIX_DEF_SCHEME should have been defined in the
 * Makefile by the configure script. We define them here just in case
 * the configure script failed to determine the proper values.
 */

#ifndef TIX_DEF_FONTSET
#define TIX_DEF_FONTSET "14Point"
#endif

#ifndef TIX_DEF_SCHEME
#define TIX_DEF_SCHEME "TixGray"
#endif


#define DEF_TIX_TOOLKIT_OPTION_BETA		"1"
#define DEF_TIX_TOOLKIT_OPTION_BINDING		"Motif"
#define DEF_TIX_TOOLKIT_OPTION_DEBUG		"1"
#define DEF_TIX_TOOLKIT_OPTION_FONTSET		TIX_DEF_FONTSET
#define DEF_TIX_TOOLKIT_OPTION_LIBRARY		""
#define DEF_TIX_TOOLKIT_OPTION_SCHEME		TIX_DEF_SCHEME
#define DEF_TIX_TOOLKIT_OPTION_SCHEME_PRIORITY	"79"

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_BOOLEAN, "-beta", "tixBeta", "TixBeta",
       DEF_TIX_TOOLKIT_OPTION_BETA, Tk_Offset(OptionStruct, isBeta),
       0},
    {TK_CONFIG_STRING, "-binding", "binding", "TixBinding",
       DEF_TIX_TOOLKIT_OPTION_BINDING, Tk_Offset(OptionStruct, binding),
       0},
    {TK_CONFIG_BOOLEAN, "-debug", "tixDebug", "TixDebug",
       DEF_TIX_TOOLKIT_OPTION_DEBUG, Tk_Offset(OptionStruct, isDebug),
       0},
    {TK_CONFIG_STRING, "-fontset", "tixFontSet", "TixFontSet",
       DEF_TIX_TOOLKIT_OPTION_FONTSET, Tk_Offset(OptionStruct, fontSet),
       0},
    {TK_CONFIG_STRING, "-scheme", "tixScheme", "TixScheme",
       DEF_TIX_TOOLKIT_OPTION_SCHEME, Tk_Offset(OptionStruct, scheme),
       0},
    {TK_CONFIG_STRING, "-scheme", "tixSchemePriority", "TixSchemePriority",
       DEF_TIX_TOOLKIT_OPTION_SCHEME_PRIORITY,
       Tk_Offset(OptionStruct, schemePriority),
       0},
    {TK_CONFIG_STRING, "-tixlibrary", "tixLibrary", "TixLibrary",
       DEF_TIX_TOOLKIT_OPTION_LIBRARY, Tk_Offset(OptionStruct, tixlibrary),
       TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

#ifndef TIX_LIBRARY
#ifndef _WINDOWS
#define TIX_LIBRARY "/usr/local/lib/tix"
#else
#define TIX_LIBRARY "../../library"
#endif
#endif

/*----------------------------------------------------------------------
 *
 * 			Some global variables
 *
 *----------------------------------------------------------------------
 */
Tk_Uid tixNormalUid   = (Tk_Uid)NULL;
Tk_Uid tixCellUid     = (Tk_Uid)NULL;
Tk_Uid tixRowUid      = (Tk_Uid)NULL;
Tk_Uid tixColumnUid   = (Tk_Uid)NULL;
Tk_Uid tixDisabledUid = (Tk_Uid)NULL;

/*----------------------------------------------------------------------
 *
 * 			The Display Item types
 *
 *----------------------------------------------------------------------
 */

extern Tix_DItemInfo tix_ImageTextItemType;
extern Tix_DItemInfo tix_TextItemType;
extern Tix_DItemInfo tix_WindowItemType;
extern Tix_DItemInfo tix_ImageItemType;

static int		ParseToolkitOptions _ANSI_ARGS_((Tcl_Interp * interp));
extern int 		TixMwmProtocolHandler _ANSI_ARGS_((
			    ClientData clientData, XEvent *eventPtr));
static int		Tix_Init_Internal _ANSI_ARGS_((Tcl_Interp * interp,
			     int doSource));
int 			Tix_EtInit _ANSI_ARGS_((Tcl_Interp * interp));

/*----------------------------------------------------------------------
 * ParseToolkitOptions() --
 *
 *	Before the Tix initialized, we need to determine the toolkit
 *	options which are set by the options database.
 *----------------------------------------------------------------------
 */
static int
ParseToolkitOptions(interp)
    Tcl_Interp * interp;
{
    char buff[10];
    int flag;

    tixOption.isBeta = 0;
    tixOption.binding = NULL;
    tixOption.isDebug = 0;
    tixOption.fontSet = NULL;
    tixOption.tixlibrary = NULL;
    tixOption.scheme = NULL;
    tixOption.schemePriority = NULL;

    /*
     * The toolkit options may be set in the resources of the main window
     */
    if (Tk_ConfigureWidget(interp, Tk_MainWindow(interp), configSpecs,
	    0, 0, (char *) &tixOption, 0) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Now lets set the Tix toolkit variables so that the Toolkit can
     * initialize according to user options.
     */
    flag = TCL_GLOBAL_ONLY;
    sprintf(buff, "%d", tixOption.isBeta);
    Tcl_SetVar2(interp, "tix_priv", "-beta", buff, flag);
    sprintf(buff, "%d", tixOption.isDebug);
    Tcl_SetVar2(interp, "tix_priv", "-debug", buff, flag);

    if (tixOption.tixlibrary == 0 || strlen(tixOption.tixlibrary) == 0) {
	/*
	 * Set up the TCL variable "tix_library" according to the environment
	 * variable.
	 */
	if (tixOption.tixlibrary != NULL) {
	    ckfree((char*)tixOption.tixlibrary);
	}

	tixOption.tixlibrary = (char*)getenv("TIX_LIBRARY");
	if (tixOption.tixlibrary == NULL) {
	    tixOption.tixlibrary = TIX_LIBRARY;
	}
	Tcl_SetVar2(interp, "tix_priv", "-libdir",  
	 	tixOption.tixlibrary, flag);
    } else {
	Tcl_SetVar2(interp, "tix_priv", "-libdir",  
	 	tixOption.tixlibrary, flag);
	ckfree((char*)tixOption.tixlibrary);
    }

    /*
     * tixOption.tixlibrary may not be allocated by Tk_ConfigureWidget().
     * We have already freed it (if necessary). We set it to NULL so
     * that Tk_FreeOptions() won't try to free it.
     */
    tixOption.tixlibrary = NULL;

    Tcl_SetVar2(interp, "tix_priv", "-binding",
	tixOption.binding,    		flag);
    Tcl_SetVar2(interp, "tix_priv", "-fontset", 
	tixOption.fontSet,    		flag);
    Tcl_SetVar2(interp, "tix_priv", "-scheme",  
	tixOption.scheme,     		flag);
    Tcl_SetVar2(interp, "tix_priv", "-schemepriority",
	tixOption.schemePriority,     flag);

    Tk_FreeOptions(configSpecs, (char *)&tixOption,
	Tk_Display(Tk_MainWindow(interp)), 0);

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_Init_Internal() --
 *
 *	Initialize the Tix library. The doSource argument specifies
 *	we should source the file Init.tcl from the Tix script library
 *	path. A doSource is not necessary if Tix was included in an ET
 *	applicattion.
 *----------------------------------------------------------------------
 */

static int
Tix_Init_Internal(interp, doSource)
	 Tcl_Interp * interp;
	 int doSource;
{
    Tk_Window topLevel;
    char * appName;
    static int globalInitialized = 0;

    /*
     * This procedure may be called  several times for several
     * interpreters. Since some global variables are shared by
     * all of the interpreters, we initialize these variables only
     * once. The variable "globalInitialized" keeps track of this
     */

    extern Tk_ImageType tixPixmapImageType;
    extern Tk_ImageType tixCompoundImageType;


#ifdef TCL_7_5_OR_LATER
    /*
     * The new package mechanism, available in Tcl7.5 or later
     */
    if (Tcl_PkgRequire(interp, "Tcl", TCL_VERSION, 1) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_PkgRequire(interp, "Tk", TK_VERSION, 1) == NULL) {
	return TCL_ERROR;
    }
#ifdef ITCL_2
    if (Tcl_PkgRequire(interp, "Itcl", ITCL_VERSION, 0) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_PkgRequire(interp, "Itk", ITCL_VERSION, 0) == NULL) {
	return TCL_ERROR;
    }
#endif
/*
 * // This is now done in Init.tcl
 *   if (Tcl_PkgProvide(interp, "Tix", TIX_VERSION) != TCL_OK) {
 *	return TCL_ERROR;
 *   }
 */
#endif

    topLevel = Tk_MainWindow(interp);

    if (!globalInitialized) {
	globalInitialized = 1;

	/*
	 * Initialize the global variables shared by all interpreters
	 */
	tixNormalUid   = Tk_GetUid("normal");
	tixCellUid     = Tk_GetUid("cell");
	tixRowUid      = Tk_GetUid("row");
	tixColumnUid   = Tk_GetUid("column");
	tixDisabledUid = Tk_GetUid("disabled");

#ifndef _WINDOWS
	/* This is for tixMwm command */
	Tk_CreateGenericHandler(TixMwmProtocolHandler, NULL);
#endif

	/* Initialize the image readers */
	Tk_CreateImageType(&tixPixmapImageType);
	Tk_CreateImageType(&tixCompoundImageType);

	/* Initialize the display item types */
	Tix_AddDItemType(&tix_ImageTextItemType);
	Tix_AddDItemType(&tix_TextItemType);
	Tix_AddDItemType(&tix_WindowItemType);
	Tix_AddDItemType(&tix_ImageItemType);

	/*
	 * Initializes all the Tix built-in bitmaps.
	 */
#define Et_Interp interp
#include "tixBitmaps.h"

    }
    else {
	/*
	 * This variable is used in the __tixInit procedure.
	 */
	Tcl_SetVar2(interp, "tix_priv", "slaveInterp", "", TCL_GLOBAL_ONLY);
    }

    /*
     * Initialize the per-interpreter variables
     */

    /*  Set the "tix_version" variable */
    Tcl_SetVar(interp, "tix_version",    TIX_VERSION,    TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tix_patchLevel", TIX_PATCHLEVEL, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tix_release",    TIX_RELEASE,    TCL_GLOBAL_ONLY);

    /* Initialize the Tix commands */
    Tix_CreateCommands(interp, commands, (ClientData) topLevel,
	(void (*)_ANSI_ARGS_((ClientData))) NULL);

#ifdef _WINDOWS
    Tcl_GlobalEval(interp, "set tixPriv(isWindows) 1");
#endif

    /* Parse options database for fontSets, schemes, etc */
    if (ParseToolkitOptions(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if ((appName = Tcl_GetVar(interp, "argv0", TCL_GLOBAL_ONLY))== NULL) {
	appName = "tixwish";
    }

    if (doSource) {

	if (TixLoadLibrary(interp) != TCL_OK) {
	    return TCL_ERROR;
	}

	/*
	 * Check whether the TIX_LIBRARY variable is set to a
	 * pre-4.0.2 version of Tix. (All 4.0.2+ version will
	 * correctly identify their own versions and will print out
	 * warning messages if the version of the binary does not
	 * match with the script library
	 */
	if (Tcl_GlobalEval(interp, "tixScriptVersion") != TCL_OK) {
	    fprintf(stderr, 
		"Warning: Tix script library version (pre 4.0.2)\n");
	    fprintf(stderr, "  in \"%s\"\n", Tcl_GetVar(interp, "tix_library",
		    TCL_GLOBAL_ONLY));
	    fprintf(stderr, "  does not match binary version (%s).\n",
		TIX_PATCHLEVEL);
	    fprintf(stderr, "  Please check your TIX_LIBRARY environment ");
	    fprintf(stderr, "variable and your Tix installation.\n");
	    Tcl_ResetResult(interp);
	}

	if (Tcl_GlobalEval(interp, "__tixInit") != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	Tcl_SetVar(interp, "tix_library", "", TCL_GLOBAL_ONLY);
    }

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_Init --
 *
 * 	This is the function to call in your Tcl_AppInit() function
 *
 *----------------------------------------------------------------------
 */

int
Tix_Init(interp)
    Tcl_Interp * interp;
{
    int code = Tix_Init_Internal(interp, 1);

#ifdef _WINDOWS
    if (code != TCL_OK) {
	char * errorInfo;

	errorInfo = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
	if (errorInfo == NULL) {
	    Tcl_SetVar(interp, "errorInfo", "unknown error", TCL_GLOBAL_ONLY);
	}
	Tix_GlobalVarEval(interp,
	    "toplevel .err; ",
		"bind .err <Destroy> {set err_ok 1}; ",
		"frame .err.f; pack .err.f -side bottom -fill both; "
	    "button .err.f.i -text Ignore -width 6 -command {set err_ok 1}; ",
	    "button .err.f.e -text Exit   -width 6 -command {exit}; ",
	    "pack .err.f.i -side left -padx 4 -pady 4;"
	    "pack .err.f.e -side left -padx 4 -pady 4; "
	    "text .err.text -width 70 -wrap none -height 5; "
	    "pack .err.text -side top -expand yes -fill both; "
	    ".err.text insert end $errorInfo; ",
	    "tkwait variable err_ok; ",
	    "catch {destroy .err}; ",
	    NULL);
    }
#endif
    return code;
}

/*----------------------------------------------------------------------
 * TixInitSam --
 *
 * 	This takes special care when you initialize the Tix library
 * 	to run in stand-alone mode.
 *----------------------------------------------------------------------
 */
int TixInitSam(interp)
    Tcl_Interp * interp;
{
    return Tix_Init_Internal(interp, 0);
}

/*----------------------------------------------------------------------
 * Tix_SafeInit --
 *
 * 	This is the function to call in your Tcl_AppInit() function
 *
 *----------------------------------------------------------------------
 */

int
Tix_SafeInit(interp)
    Tcl_Interp * interp;
{
    Tcl_SetVar2(interp, "tix_priv", "isSafe", "1", TCL_GLOBAL_ONLY);
    return Tix_Init(interp);
}

/*
 *----------------------------------------------------------------------
 * TixLoadLibrary --
 *
 *	Loads the Tix library.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side effects:
 *	Tix gets initialized.
 *----------------------------------------------------------------------
 */

static char *initScript =
"if [catch {file join a a}] {\n\
    proc tixFileJoin {args} {\n\
	set p [join $args /]\n\
	regsub -all {/+} $p / p\n\
	return $p\n\
    }\n\
} else {\n\
    proc tixFileJoin {args} {\n\
	return [eval file join $args]\n\
   }\n\
}\n\
\n\
proc init {} {\n\
    global tix_library tix_version tix_patchLevel env errorInfo\n\
    rename init {}\n\
    set dirs {}\n\
    set errors {}\n\
    if [info exists env(TIX_LIBRARY)] {\n\
	lappend dirs $env(TIX_LIBRARY)\n\
    }\n\
    if [info exists tix_library] {\n\
	lappend dirs $tix_library\n\
    }\n\
    if [string match {*[ab]*} $tix_patchLevel] {\n\
	set lib tix$tix_patchLevel\n\
	set Lib Tix$tix_patchLevel\n\
    } else {\n\
	set lib tix$tix_version\n\
	set Lib Tix$tix_version\n\
    }\n\
    catch {\n\
        # [pwd] may not work inside safe Tcl\n\
        set p [pwd]\n\
	lappend dirs [tixFileJoin $p library]\n\
        set p [file dirname $p]\n\
	lappend dirs [tixFileJoin $p library]\n\
        set p [file dirname $p]\n\
	lappend dirs [tixFileJoin $p library]\n\
    }\n\
    set instDir [file dirname [info library]]\n\
    lappend dirs [tixFileJoin $instDir $lib]\n\
    lappend dirs [tixFileJoin [tixFileJoin $instDir lib] $lib]\n\
    catch {\n\
    lappend dirs [tixFileJoin [tixFileJoin [file dirname [file dirname [info nameofexecutable]]] lib] $lib]\n\
    }\n\
    lappend dirs [tixFileJoin [tixFileJoin [file dirname [file dirname [info library]]] $lib] library]\n\
    lappend dirs [tixFileJoin [tixFileJoin [file dirname [file dirname [info library]]] $Lib] library]\n\
    foreach i $dirs {\n\
	set tix_library $i\n\
        set tixfile [tixFileJoin $i Init.tcl]\n\
        if {[interp issafe] || [file exists $tixfile]} {\n\
            if ![catch {uplevel #0 [list source $tixfile]} err] {\n\
                return\n\
            } else {\n\
                append errors \"\n$tixfile: $err\n$errorInfo\n\"\n\
	    }\n\
        }\n\
    }\n\
    set msg \"Can't find a usable Init.tcl in the following directories: \n\"\n\
    append msg \"    $dirs\n\"\n\
    append msg \"$errors\n\n\"\n\
    append msg \"This probably means that Tix wasn't installed properly.\n\"\n\
    error $msg\n\
}\n\
init";

int
TixLoadLibrary(interp)
    Tcl_Interp * interp;
{
    Tcl_SetVar(interp, "tix_library", TIX_LIBRARY, TCL_GLOBAL_ONLY);
    return Tcl_Eval(interp, initScript);
}
