/*
 * tixUtils.c --
 *
 *	This file contains some utility functions for Tix, such as the
 *	subcommand handling functions and option handling functions.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

/*
 * tclInt.h is needed for the va_list declaration.
 */
#include <tclInt.h>
#include <tixPort.h>
#include <tixInt.h>

/*
 * Forward declarations for procedures defined later in this file:
 */

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));

static int		ReliefParseProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, Tk_Window tkwin, char *value,
			    char *widRec, int offset));
static char *		ReliefPrintProc _ANSI_ARGS_((ClientData clientData,
			    Tk_Window tkwin, char *widRec, int offset,
			    Tix_FreeProc **freeProcPtr));
/*
 * Global vars used in this file
 */
static Tcl_DString command;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */


#define WRONG_ARGC 1
#define NO_MATCH   2


/*----------------------------------------------------------------------
 * TixSaveInterpState --
 *
 *	Save the current application-visible state of the interpreter.
 *	This can later be restored by the TixSaveInterpState() function.
 *	These two functions are useful if you want to evaluate a Tcl
 *	command, which may cause errors, inside a command function.
 *
 *	Each TixSaveInterpState() call much be matched by one
 *	TixRestoreInterpState() call with the same statePtr. statePtr
 *	should be allocated by the calling function, usually
 *	as a variable on the stack.
 *----------------------------------------------------------------------
 */

void
TixSaveInterpState(interp, statePtr)
    Tcl_Interp * interp;
    TixInterpState * statePtr;
{
    char * p;
    if (interp->result) {
	statePtr->result = (char*)strdup(interp->result);
    } else {
	statePtr->result = NULL;
    }

    p = Tcl_GetVar2(interp, "errorInfo", NULL, TCL_GLOBAL_ONLY);
    if (p) {
	statePtr->errorInfo = (char*)strdup(p);
    } else {
	statePtr->errorInfo = NULL;
    }

    p = Tcl_GetVar2(interp, "errorCode", NULL, TCL_GLOBAL_ONLY);
    if (p) {
	statePtr->errorCode = (char*)strdup(p);
    } else {
	statePtr->errorCode = NULL;
    }
}

/*----------------------------------------------------------------------
 * TixRestoreInterpState --
 *
 *	See TixSaveInterpState above.
 *----------------------------------------------------------------------
 */

void
TixRestoreInterpState(interp, statePtr)
    Tcl_Interp * interp;
    TixInterpState * statePtr;
{
    if (statePtr->result) {
	Tcl_SetResult(interp, statePtr->result, TCL_DYNAMIC);
    }
    if (statePtr->errorInfo) {
	Tcl_SetVar2(interp, "errorInfo", NULL, statePtr->errorInfo,
		TCL_GLOBAL_ONLY);
	ckfree((char*)statePtr->errorInfo);
    } else {
	Tcl_UnsetVar2(interp, "errorInfo", NULL, TCL_GLOBAL_ONLY);
    }
    if (statePtr->errorCode) {
	Tcl_SetVar2(interp, "errorCode", NULL, statePtr->errorCode,
		TCL_GLOBAL_ONLY);
	ckfree((char*)statePtr->errorCode);
    } else {
	Tcl_UnsetVar2(interp, "errorCode", NULL, TCL_GLOBAL_ONLY);
    }
}

/*----------------------------------------------------------------------
 * Tix_HandleSubCmds --
 *
 *	This function makes it easier to write major-minor style TCL
 *	commands.  It matches the minor command (sub-command) names
 *	with names defined in the cmdInfo structure and call the
 *	appropriate sub-command functions for you. This function will
 *	automatically generate error messages when the user calls an
 *	invalid sub-command or calls a sub-command with incorrect
 *	number of arguments.
 *
 *----------------------------------------------------------------------
 */

int Tix_HandleSubCmds(cmdInfo, subCmdInfo, clientData, interp, argc, argv)
    Tix_CmdInfo * cmdInfo;
    Tix_SubCmdInfo * subCmdInfo;
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{

    int i;
    int len;
    int error = NO_MATCH;
    Tix_SubCmdInfo * s;

    /*
     * First check if the number of arguments to the major command 
     * is correct
     */
    argc -= 1;
    if (argc < cmdInfo->minargc || 
	(cmdInfo->maxargc != TIX_VAR_ARGS && argc > cmdInfo->maxargc)) {

	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " ", cmdInfo->info, "\".", (char *) NULL);

	return TCL_ERROR;
    }

    /*
     * Now try to match the subcommands with argv[1]
     */
    argc -= 1;
    len = strlen(argv[1]);

    for (i = 0, s = subCmdInfo; i < cmdInfo->numSubCmds; i++, s++) {
	if (s->name == TIX_DEFAULT_SUBCMD) {
	    if (s->checkArgvProc) {
	      if (!((*s->checkArgvProc)(clientData, interp, argc+1, argv+1))) {
		    /* Some improper argv in the arguments of the default
		     * subcommand
		     */
		    break;
		}
	    }
	    return (*s->proc)(clientData, interp, argc+1, argv+1);
	}

	if (s->namelen == TIX_DEFAULT_LEN) {
	    s->namelen = strlen(s->name);
	}
	if (s->name[0] == argv[1][0] && strncmp(argv[1],s->name,len)==0) {
	    if (argc < s->minargc) {
		error = WRONG_ARGC;
		break;
	    }

	    if (s->maxargc != TIX_VAR_ARGS && 
		argc > s->maxargc) {
		error = WRONG_ARGC;
		break;
	    }

	    /*
	     * Here we have a matched argc and command name --> go for it!
	     */
	    return (*s->proc)(clientData, interp, argc, argv+2);
	}
    }

    if (error == WRONG_ARGC) {
	/*
	 * got a match but incorrect number of arguments
	 */
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " ", argv[1], " ", s->info, "\"", (char *) NULL);
    } else {
	int max;

	/*
	 * no match: let print out all the options
	 */
	Tcl_AppendResult(interp, "unknown option \"",
	    argv[1], "\".",  (char *) NULL);
	
	if (cmdInfo->numSubCmds == 0) {
	    max = 0;
	} else {
	    if (subCmdInfo[cmdInfo->numSubCmds-1].name == TIX_DEFAULT_SUBCMD) {
		max = cmdInfo->numSubCmds-1;
	    } else {
		max = cmdInfo->numSubCmds;
	    }
	}

	if (max == 0) {
	    Tcl_AppendResult(interp,
		" This command does not take any options.",
		(char *) NULL);
	} else if (max == 1) {
	    Tcl_AppendResult(interp, 
		" Must be ", subCmdInfo->name, ".", (char *)NULL);
	} else {
	    Tcl_AppendResult(interp, " Must be ", (char *) NULL);

	    for (i = 0, s = subCmdInfo; i < max; i++, s++) {
		if (i == max-1) {
		    Tcl_AppendResult(interp,"or ",s->name, ".", (char *) NULL);
		} else if (i == max-2) {
		    Tcl_AppendResult(interp, s->name, " ", (char *) NULL); 
		} else {
		    Tcl_AppendResult(interp, s->name, ", ", (char *) NULL); 
		}
	    }
	} 
    }
    return TCL_ERROR;
}

/*----------------------------------------------------------------------
 * Tix_Exit --
 *
 *	Call the "exit" tcl command so that things can be cleaned up
 *	before calling the unix exit(2);
 *
 *----------------------------------------------------------------------
 */
void Tix_Exit(interp, code)
    Tcl_Interp* interp;
    int code;
{
    if (code != 0 && interp && interp->result != 0) {
	fprintf(stderr, "%s\n", interp->result);
	fprintf(stderr, "%s\n", 
	    Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
    }

    if (interp) {
	Tcl_GlobalEval(interp, "exit");
    }
    exit(code);
}

/*
 *----------------------------------------------------------------------
 *
 * Tix_LoadTclLibrary --
 *
 *	Loads in a TCL library for an application according to 
 *	the library settings.
 *
 * Results:
 *	TCL_OK or TCL_ERROR
 *
 * envName	the environment variable that indicates the library
 * tclName	the TCL variable that points to the TCL library.
 * initFile	the file to load in during initialization.
 * defDir	the default directory to search if the user hasn't set
 *		the environment variable.
 * appName	the name of the application.
 *----------------------------------------------------------------------
 */

/* Some compilers can't handle multi-line character strings very well ...
 * So I just using this big lump of mess here.
 */

static char _format[] = "lappend auto_path $%s \nif [file exists $%s/%s] {\nsource $%s/%s\n} else {\nset msg \"\ncan't find $%s/%s;\\nperhaps you \"\nappend msg \"need to install %s\\nor set your %s \"\nappend msg \"environment variable?\"\nerror $msg\n}";

int
Tix_LoadTclLibrary(interp, envName, tclName, initFile, defDir, appName)
    Tcl_Interp *interp;
    char *envName;
    char *tclName;
    char *initFile;
    char *defDir;
    char *appName;
{
    char * libDir, *initCmd;
    size_t size;
    int code;
    char *format;
    format = _format;

    libDir = getenv(envName);
    if (libDir == NULL) {
	libDir = defDir;
    }

    /*
     * This size should be big enough.
     */

    size = strlen(format) + strlen(tclName)*4 + strlen(initFile)*3
	+ strlen(appName) + strlen(envName) + 100;
    initCmd = ckalloc(sizeof(char) * size);

    Tcl_SetVar(interp, tclName, libDir, TCL_GLOBAL_ONLY);

    sprintf(initCmd, format,
	tclName,
	tclName, initFile,
	tclName, initFile,
	tclName, initFile,
	appName, envName
    );

    code =  Tcl_GlobalEval(interp, initCmd);
    ckfree(initCmd);
    return code;
}

/*----------------------------------------------------------------------
 * Tix_CreateCommands --
 *
 *
 *	Creates a list of commands stored in the array "commands"
 *----------------------------------------------------------------------
 */

void Tix_CreateCommands(interp, commands, clientData, deleteProc)
    Tcl_Interp *interp;
    Tix_TclCmd *commands;
    ClientData clientData;
    Tcl_CmdDeleteProc *deleteProc;
{
    Tix_TclCmd * cmdPtr;

    for (cmdPtr = commands; cmdPtr->name != NULL; cmdPtr++) {
	Tcl_CreateCommand(interp, cmdPtr->name,
	     cmdPtr->cmdProc, clientData, deleteProc);
    }
}

/*----------------------------------------------------------------------
 * Tix_DrawAnchorLines --
 *
 * 	Draw dotted anchor lines around anchor elements
 *----------------------------------------------------------------------
 */

void Tix_DrawAnchorLines(display, drawable, gc, x, y, w, h)
    Display *display;
    Drawable drawable;
    GC gc;
    int x;
    int y;
    int w;
    int h;
{
    TixpDrawAnchorLines(display, drawable, gc, x, y, w, h);
}

/*----------------------------------------------------------------------
 * Tix_CreateSubWindow --
 *
 *	Creates a subwindow for a widget (usually used to draw headers,
 *	e.g, HList and Grid widgets)
 *----------------------------------------------------------------------
 */

Tk_Window
Tix_CreateSubWindow(interp, tkwin, subPath)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    char * subPath;
{
    Tcl_DString dString;
    Tk_Window subwin;

    Tcl_DStringInit(&dString);
    Tcl_DStringAppend(&dString, Tk_PathName(tkwin),
	strlen(Tk_PathName(tkwin)));
    Tcl_DStringAppend(&dString, ".tixsw:", 7);
    Tcl_DStringAppend(&dString, subPath, strlen(subPath));

    subwin = Tk_CreateWindowFromPath(interp, tkwin, dString.string,
	(char *) NULL);

    Tcl_DStringFree(&dString);

    return subwin;
}

/*----------------------------------------------------------------------
 * Tix_GetRenderBuffer --
 *
 *	Returns a drawable for rendering a widget. If there is sufficient
 *	resource, a pixmap is returned so that double-buffering can
 *	be done. However, if resource is insufficient, then the
 *	windowId is returned. In the second case happens, the caller
 *	of this function has two choices: (1) draw to the window directly
 *	(which may lead to flashing on the screen) or (2) try to allocate
 *	smaller pixmaps.
 *----------------------------------------------------------------------
 */

static int
ErrorProc(clientData, errorEventPtr)
    ClientData clientData;
    XErrorEvent *errorEventPtr;		/* unused */
{
    int * badAllocPtr = (int*) clientData;

    * badAllocPtr = 1;
    return 0;				/* return 0 means error has been
					 * handled properly */
}

Drawable Tix_GetRenderBuffer(display, windowId, width, height, depth)
    Display *display;
    Window windowId;
    int width;
    int height;
    int depth;
{
    Tk_ErrorHandler handler;
    Pixmap pixmap;
    int badAlloc = 0;

    handler= Tk_CreateErrorHandler(display, BadAlloc,
	-1, -1, (Tk_ErrorProc *) ErrorProc, (ClientData) &badAlloc);
    pixmap = Tk_GetPixmap(display, windowId, width, height, depth);

#ifndef _WINDOWS
    /*
     * This XSync call is necessary because X may delay the delivery of the
     * error message. This will make our graphics a bit slower, though,
     * especially over slow lines
     */
    XSync(display, 0);
#endif
    /* If ErrorProc() is eevr called, it is called before XSync returns */

    Tk_DeleteErrorHandler(handler);

    if (!badAlloc) {
	return pixmap;
    } else {
	return windowId;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tix_GlobalVarEval --
 *
 *	Given a variable number of string arguments, concatenate them
 *	all together and execute the result as a Tcl command in the global
 *	scope.
 *
 * Results:
 *	A standard Tcl return result.  An error message or other
 *	result may be left in interp->result.
 *
 * Side effects:
 *	Depends on what was done by the command.
 *
 *----------------------------------------------------------------------
 */
	/* VARARGS2 */ /* ARGSUSED */
int
#ifdef TCL_VARARGS_DEF
Tix_GlobalVarEval TCL_VARARGS_DEF(Tcl_Interp *,arg1)
#else
#ifndef lint
Tix_GlobalVarEval(va_alist)
#else
Tix_GlobalVarEval(iPtr, p, va_alist)
    Tcl_Interp *iPtr;		/* Interpreter in which to execute command. */
    char *p;			/* One or more strings to concatenate,
				 * terminated with a NULL string. */
#endif
    va_dcl
#endif
{
    va_list argList;
    Tcl_DString buf;
    char *string;
    Tcl_Interp *interp;
    int result;

#ifdef TCL_VARARGS_DEF
    /*
     * Copy the strings one after the other into a single larger
     * string.  Use stack-allocated space for small commands, but if
     * the command gets too large than call ckalloc to create the
     * space.
     */

    interp = TCL_VARARGS_START(Tcl_Interp *,arg1,argList);
    Tcl_DStringInit(&buf);
    while (1) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	Tcl_DStringAppend(&buf, string, -1);
    }
    va_end(argList);

    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&buf));
    Tcl_DStringFree(&buf);
    return result;
#else
    va_start(argList);
    interp = va_arg(argList, Tcl_Interp *);
    Tcl_DStringInit(&buf);
    while (1) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	Tcl_DStringAppend(&buf, string, -1);
    }
    va_end(argList);

    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&buf));
    Tcl_DStringFree(&buf);
    return result;
#endif
}

/*----------------------------------------------------------------------
 * TixGetHashTable --
 *
 *	This functions makes it possible to keep one hash table per
 *	interpreter. This way, Tix classes can be used in multiple
 *	interpreters.
 *
 *----------------------------------------------------------------------
 */

#ifdef TK_4_1_OR_LATER

static void		DeleteHashTableProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp * interp));
static void
DeleteHashTableProc(clientData, interp)
    ClientData clientData;
    Tcl_Interp * interp;
{
    Tcl_HashTable * htPtr = (Tcl_HashTable *)clientData;
    Tcl_HashSearch hashSearch;
    Tcl_HashEntry * hashPtr;

    for (hashPtr = Tcl_FirstHashEntry(htPtr, &hashSearch);
	    hashPtr;
	    hashPtr = Tcl_NextHashEntry(&hashSearch)) {
	Tcl_DeleteHashEntry(hashPtr);
    }

    Tcl_DeleteHashTable(htPtr);
    ckfree((char*)htPtr);
}

Tcl_HashTable *
TixGetHashTable(interp, name, deleteProc)
    Tcl_Interp * interp;
    char * name;
    Tcl_InterpDeleteProc *deleteProc;
{
    Tcl_HashTable * htPtr;

    htPtr = (Tcl_HashTable*)Tcl_GetAssocData(interp, name, NULL);
    if (htPtr == NULL) {
	htPtr = (Tcl_HashTable *)ckalloc(sizeof(Tcl_HashTable));
	Tcl_InitHashTable(htPtr, TCL_STRING_KEYS);
	Tcl_SetAssocData(interp, name, NULL, (ClientData)htPtr);
	if (deleteProc) {
	    Tcl_CallWhenDeleted(interp, deleteProc, (ClientData)htPtr);
	} else {
	    Tcl_CallWhenDeleted(interp, DeleteHashTableProc,
		    (ClientData)htPtr);
	}
    }

    return htPtr;
}

#else

Tcl_HashTable *
TixGetHashTable(interp, name)
    Tcl_Interp * interp;	/* Current interpreter. */
    char * name;		/* Textual name of the hash table. */
{
    static int inited = 0;
    static Tcl_HashTable classTable;
    static Tcl_HashTable methodTable;
    static Tcl_HashTable specTable;

    if (!inited) {
	Tcl_InitHashTable(&classTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&methodTable, TCL_STRING_KEYS);
	Tcl_InitHashTable(&specTable, TCL_STRING_KEYS);
	inited = 1;
    }

    if (strcmp(name, "tixClassTab") == 0) {
	return &classTable;
    } else if (strcmp(name, "tixSpecTab") == 0) {
	return &specTable;
    } else if (strcmp(name, "tixMethodTab") == 0) {
	return &methodTable;
    } else {
	panic("Unknown hash table %s", name);
    }
}
#endif

/*----------------------------------------------------------------------
 *
 *		 The Tix Customed Config Options
 *
 *----------------------------------------------------------------------
 */

/*----------------------------------------------------------------------
 *  ReliefParseProc --
 *
 *	Parse the text string and store the Tix_Relief information
 *	inside the widget record.
 *----------------------------------------------------------------------
 */
static int ReliefParseProc(clientData, interp, tkwin, value, widRec,offset)
    ClientData clientData;
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *value;
    char *widRec;		/* Must point to a valid Tix_DItem struct */
    int offset;
{
    Tix_Relief * ptr = (Tix_Relief *)(widRec + offset);
    Tix_Relief   newVal;

    if (value != NULL) {
	size_t len = strlen(value);

	if (strncmp(value, "raised", len) == 0) {
	    newVal = TIX_RELIEF_RAISED;
	} else if (strncmp(value, "flat", len) == 0) {
	    newVal = TIX_RELIEF_FLAT;
	} else if (strncmp(value, "sunken", len) == 0) {
	    newVal = TIX_RELIEF_SUNKEN;
	} else if (strncmp(value, "groove", len) == 0) {
	    newVal = TIX_RELIEF_GROOVE;
	} else if (strncmp(value, "ridge", len) == 0) {
	    newVal = TIX_RELIEF_RIDGE;
	} else if (strncmp(value, "solid", len) == 0) {
	    newVal = TIX_RELIEF_SOLID;
	} else {
	    goto error;
	}
    } else {
	value = "";
	goto error;
    }

    *ptr = newVal;
    return TCL_OK;

  error:
    Tcl_AppendResult(interp, "bad relief type \"", value,
	"\":  must be flat, groove, raised, ridge, solid or sunken", NULL);
    return TCL_ERROR;
}

static char *ReliefPrintProc(clientData, tkwin, widRec,offset, freeProcPtr)
    ClientData clientData;
    Tk_Window tkwin;
    char *widRec;
    int offset;
    Tix_FreeProc **freeProcPtr;
{
    Tix_Relief *ptr = (Tix_Relief*)(widRec+offset);

    switch (*ptr) {
      case TIX_RELIEF_RAISED:
	return "raised";
      case TIX_RELIEF_FLAT:
	return "flat";
      case TIX_RELIEF_SUNKEN:
	return "sunken";
      case TIX_RELIEF_GROOVE:
	return "groove";
      case TIX_RELIEF_RIDGE:
	return "ridge";
      case TIX_RELIEF_SOLID:
	return "solid";
      default:
	return "unknown";
    }
}
/*
 * The global data structures to use in widget configSpecs arrays
 *
 * These are declared in <tix.h>
 */

Tk_CustomOption tixConfigRelief = {
    ReliefParseProc, ReliefPrintProc, 0,
};

/* Tix_SetRcFileName --
 *
 *	Sets a user-specific startup file in a way that's compatible with
 *	different versions of Tclsh
 */
void Tix_SetRcFileName(interp, rcFileName)
    Tcl_Interp * interp;
    char * rcFileName;
{
#ifdef TCL_7_5_OR_LATER
    /*
     * Starting from TCL 7.5, the symbol tcl_rcFileName is no longer
     * exported by libtcl.a. Instead, this variable must be set using
     * a TCL global variable
     */
    Tcl_SetVar(interp, "tcl_rcFileName", rcFileName, TCL_GLOBAL_ONLY);
#else
    tcl_RcFileName = rcFileName;
#endif
}

#if (TK_MAJOR_VERSION > 4)

/*
 * The TkComputeTextGeometry function is no longer supported in Tk 8.0+
 */

/*
 *----------------------------------------------------------------------
 *
 * TixComputeTextGeometry --
 *
 *	This procedure computes the amount of screen space needed to
 *	display a multi-line string of text.
 *
 * Results:
 *	There is no return value.  The dimensions of the screen area
 *	needed to display the text are returned in *widthPtr, and *heightPtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
TixComputeTextGeometry(font, string, numChars, wrapLength,
	widthPtr, heightPtr)
    TixFont font;		/* Font that will be used to display text. */
    char *string;		/* String whose dimensions are to be
				 * computed. */
    int numChars;		/* Number of characters to consider from
				 * string. */
    int wrapLength;		/* Longest permissible line length, in
				 * pixels.  <= 0 means no automatic wrapping:
				 * just let lines get as long as needed. */
    int *widthPtr;		/* Store width of string here. */
    int *heightPtr;		/* Store height of string here. */
{
    Tk_TextLayout textLayout;

    /*
     * The justification itself doesn't affect the geometry (size) of 
     * the text string. We pass TK_JUSTIFY_LEFT.
     */

    textLayout = Tk_ComputeTextLayout(font,
	string, numChars, wrapLength, TK_JUSTIFY_LEFT, 0,
	widthPtr, heightPtr);
    Tk_FreeTextLayout(textLayout);
}

/*
 *----------------------------------------------------------------------
 *
 * TixDisplayText --
 *
 *	Display a text string on one or more lines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The text given by "string" gets displayed at the given location
 *	in the given drawable with the given font etc.
 *
 *----------------------------------------------------------------------
 */

void
TixDisplayText(display, drawable, font, string, numChars, x, y,
	length, justify, underline, gc)
    Display *display;		/* X display to use for drawing text. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * text. */
    TixFont font;		/* Font that determines geometry of text
				 * (should be same as font in gc). */
    char *string;		/* String to display;  may contain embedded
				 * newlines. */
    int numChars;		/* Number of characters to use from string. */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int length;			/* Line length in pixels;  used to compute
				 * word wrap points and also for
				 * justification.   Must be > 0. */
    Tk_Justify justify;		/* How to justify lines. */
    int underline;		/* Index of character to underline, or < 0
				 * for no underlining. */
    GC gc;			/* Graphics context to use for drawing text. */
{
    Tk_TextLayout textLayout;
    int dummy;

    textLayout = Tk_ComputeTextLayout(font,
	string, numChars, length, justify, 0,
	&dummy, &dummy);

    Tk_DrawTextLayout(display, drawable, gc, textLayout,
	    x, y, 0, -1);
    Tk_UnderlineTextLayout(display, drawable, gc,
	    textLayout, x, y, underline);

    Tk_FreeTextLayout(textLayout);
}
#endif
