/*
 * tixCmds.c --
 *
 *	Implements various TCL commands for Tix.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tixPort.h>
#include <tixInt.h>
#include <math.h>

TIX_DECLARE_CMD(Tix_ParentWindow);

/*
 * Maximum intensity for a color:
 */

#define MAX_INTENSITY 65535

/*
 * Data structure used by the tixDoWhenIdle command.
 */
typedef struct {
    Tcl_Interp * interp;
    char       * command;
    Tk_Window  tkwin;
} IdleStruct;

/*
 * Data structures used by the tixDoWhenMapped command.
 */
typedef struct _MapCmdLink {
    char * command;
    struct _MapCmdLink * next;
} MapCmdLink;

typedef struct {
    Tcl_Interp * interp;
    Tk_Window	 tkwin;
    MapCmdLink * cmds;
} MapEventStruct;

/*
 * Global vars
 */
static Tcl_HashTable idleTable;		/* hash table for TixDoWhenIdle */
static Tcl_HashTable mapEventTable;	/* hash table for TixDoWhenMapped */


/*
 * Functions used only in this file.
 */
static void		IdleHandler _ANSI_ARGS_((ClientData clientData));
static void		EventProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static void		MapEventProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static int		IsOption _ANSI_ARGS_((char *option,
			    int optArgc, char **optArgv));
static XColor *		ScaleColor _ANSI_ARGS_((Tk_Window tkwin,
			    XColor * color, double scale));
static char *		NameOfColor _ANSI_ARGS_((XColor * colorPtr));


/*----------------------------------------------------------------------
 * Tix_DoWhenIdle --
 *
 *	The difference between "tixDoWhenIdle" and "after" is: the
 *	"after" handler is called after all other TK Idel Event
 *	Handler are called.  Sometimes this will cause some toplevel
 *	windows to be mapped before the Idle Event Handler is
 *	executed.
 *
 *	This behavior of "after" is not suitable for implementing
 *	geometry managers. Therefore I wrote "tixDoWhenIdle" which is
 *	an exact TCL interface for Tk_DoWhenIdle()
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_DoWhenIdleCmd)
{
    int			isNew;
    char       	      * command;
    static int 	        inited = 0;
    IdleStruct	      * iPtr;
    Tk_Window		tkwin;
    Tcl_HashEntry * hashPtr;
 
    if (!inited) {
	Tcl_InitHashTable(&idleTable, TCL_STRING_KEYS);
	inited = 1;
    }

    /*
     * parse the arguments
     */
    if (strncmp(argv[0], "tixWidgetDoWhenIdle", strlen(argv[0]))== 0) {
	if (argc<3) {
	    return Tix_ArgcError(interp, argc, argv, 1,
		"command window ?arg arg ...?");
	}
	/* tixWidgetDoWhenIdle reqires that the second argument must
	 * be the name of a mega widget
	 */
	tkwin=Tk_NameToWindow(interp, argv[2], Tk_MainWindow(interp));
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
    } else {
	if (argc<2) {
	    return Tix_ArgcError(interp, argc, argv, 1,
		"command ?arg arg ...?");
	}
	tkwin = NULL;
    }

    command = Tcl_Merge(argc-1, argv+1);

    hashPtr = Tcl_CreateHashEntry(&idleTable, command, &isNew);

    if (!isNew) {
	ckfree(command);
    } else {
	iPtr = (IdleStruct *) ckalloc(sizeof(IdleStruct));
	iPtr->interp  = interp;
	iPtr->command = command;
	iPtr->tkwin = tkwin;

	Tcl_SetHashValue(hashPtr, (char*)iPtr);

	if (tkwin) {
	    /* we just want one event handler for all idle events
	     * associated with a window. This is done by first calling
	     * Delete and then Create EventHandler.
	     */
	    Tk_DeleteEventHandler(tkwin, StructureNotifyMask, EventProc,
		(ClientData)tkwin);
	    Tk_CreateEventHandler(tkwin, StructureNotifyMask, EventProc,
		(ClientData)tkwin);
	}

	Tk_DoWhenIdle(IdleHandler, (ClientData) iPtr);
    }

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_DoWhenMapped
 *
 *	Arranges a command to be called when the window received an
 *	<Map> event.
 *
 * argv[1..] = command argvs
 *
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_DoWhenMappedCmd)
{
    Tcl_HashEntry     * hashPtr;
    int			isNew;
    MapEventStruct    * mPtr;
    MapCmdLink	      * cmd;
    Tk_Window		tkwin;
    static int 	        inited = 0;

    if (argc!=3) {
	return Tix_ArgcError(interp, argc, argv, 1, " pathname command");
    }

    tkwin = Tk_NameToWindow(interp, argv[1], Tk_MainWindow(interp));
    if (tkwin == NULL) {
	return TCL_ERROR;
    }

    if (!inited) {
	Tcl_InitHashTable(&mapEventTable, TCL_ONE_WORD_KEYS);
	inited = 1;
    }

    hashPtr = Tcl_CreateHashEntry(&mapEventTable, (char*)tkwin, &isNew);

    if (!isNew) {
	mPtr = (MapEventStruct*) Tcl_GetHashValue(hashPtr);
    } else {
	mPtr = (MapEventStruct*) ckalloc(sizeof(MapEventStruct));
	mPtr->interp = interp;
	mPtr->tkwin  = tkwin;
	mPtr->cmds   = 0;

	Tcl_SetHashValue(hashPtr, (char*)mPtr);

	Tk_CreateEventHandler(tkwin, StructureNotifyMask,
	    MapEventProc, (ClientData)mPtr);
    }

    /*
     * Add this into a link list
     */
    cmd = (MapCmdLink*) ckalloc(sizeof(MapCmdLink));
    cmd->command = (char*)strdup(argv[2]);

    cmd->next = mPtr->cmds;
    mPtr->cmds = cmd;

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_FalseCmd --
 *
 *	Returns a false value regardless of the arguments. This is used to
 *	skip run-time debugging 
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_FalseCmd)
{
    Tcl_SetResult(interp, "0",TCL_STATIC);
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_FileCmd --
 *
 *	(obsolete)
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_FileCmd)
{
    char *expandedFileName;
    Tcl_DString buffer;
    size_t len;

    if (argc!=3) {
	return Tix_ArgcError(interp, argc, argv, 1, "option filename");
    }
    len = strlen(argv[1]);
    if (argv[1][0]=='t' && strncmp(argv[1], "tildesubst", len)==0) {

	expandedFileName = Tcl_TildeSubst(interp, argv[2], &buffer);
	Tcl_ResetResult(interp);
	if (expandedFileName == NULL) {
	    Tcl_AppendResult(interp, argv[2], NULL);
	} else {
	    Tcl_AppendResult(interp, expandedFileName, NULL);
	    Tcl_DStringFree(&buffer);	/* Was initialized by Tcl_TildeSubst */
	}

	return TCL_OK;
    }
    else if (argv[1][0]=='t' && strncmp(argv[1], "trimslash", len)==0) {
	/*
	 * Compress the extra "/"
	 */
	char *src, *dst, *p;
	int isSlash = 0;

	p = (char*)strdup(argv[2]);

	for (src=dst=p; *src; src++) {
	    if (*src == '/') {
		if (!isSlash) {
		    *dst++ = *src;
		    isSlash = 1;
		}
	    } else {
		*dst++ = *src;
		isSlash = 0;
	    }
	}
	* dst = '\0';

	if (dst > p) {
	    /*
	     * Trim the tariling "/", but only if this filename is not "/"
	     */
	    -- dst;
	    if (*dst == '/') {
		if (dst != p) {
		    * dst = '\0';
		}
	    }
	}
	Tcl_SetResult(interp, p, TCL_DYNAMIC);
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "unknown option \"", argv[1], 
	"\", must be tildesubst or trimslash", NULL);
    return TCL_ERROR;
}

/*----------------------------------------------------------------------
 * Tix_Get3DBorderCmd
 *
 * 	Returns the upper and lower border shades of a color. Returns then
 *	in a list of two X color names.
 *
 *	The color is not very useful if the display is a mono display:
 *	it will just return black and white. So a clever program may
 *	want to check the [tk colormodel] and if it is mono, then
 *	dither using a bitmap.
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_Get3DBorderCmd)
{
    XColor * color, * light, * dark;
    Tk_Window tkwin;
    Tk_Uid colorUID;

    if (argc != 2) {
	return Tix_ArgcError(interp, argc, argv, 0, "colorName");
    }

    tkwin = Tk_MainWindow(interp);

    colorUID = Tk_GetUid(argv[1]);
    color = Tk_GetColor(interp, tkwin, colorUID);
    if (color == NULL) {
	return TCL_ERROR;
    }

    if ((light = ScaleColor(tkwin, color, 1.4)) == NULL) {
	return TCL_ERROR;
    }
    if ((dark  = ScaleColor(tkwin, color, 0.6)) == NULL) {
	return TCL_ERROR;
    }

    Tcl_ResetResult(interp);
    Tcl_AppendElement(interp, NameOfColor(light));
    Tcl_AppendElement(interp, NameOfColor(dark));

    Tk_FreeColor(color);
    Tk_FreeColor(light);
    Tk_FreeColor(dark);

    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_GetBooleanCmd
 *
 * 	Return "1" if is a true boolean number. "0" otherwise
 *
 * argv[1]  = string to test
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_GetBooleanCmd)
{
    int value;
    int nocomplain = 0;
    char *string;
    static char *results[2] = {"0", "1"};

    if (argc == 3) {
	if (strcmp(argv[1], "-nocomplain") != 0) {
	    goto error;
	}
	nocomplain = 1;
	string = argv[2];
    }
    else if (argc != 2) {
	goto error;
    }
    else {
	string = argv[1];
    }

    if (Tcl_GetBoolean(interp, string, &value) != TCL_OK) {
	if (nocomplain) {
	    value = 0;
	}
	else {
	    return TCL_ERROR;
	}
    }

    Tcl_SetResult(interp, results[value], TCL_STATIC);
    return TCL_OK;

  error:
    return Tix_ArgcError(interp, argc, argv, 1, "?-nocomplain? string");
}

/*----------------------------------------------------------------------
 * Tix_GetIntCmd
 *
 * 	Return "1" if is a true boolean number. "0" otherwise
 *
 * argv[1]  = string to test
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_GetIntCmd)
{
    int    i;
    int    opTrunc = 0;
    int    opNocomplain = 0;
    int    i_value;
    double f_value;
    char * string = 0;
    char   buff[20];

    for (i=1; i<argc; i++) {
	if (strcmp(argv[i], "-nocomplain") == 0) {
	    opNocomplain = 1;
	}
	else if (strcmp(argv[i], "-trunc") == 0) {
	    opTrunc = 1;
	}
	else {
	    string = argv[i];
	    break;
	}
    }
    if (i != argc-1) {
	return Tix_ArgcError(interp, argc, argv, 1, 
	    "?-nocomplain? ?-trunc? string");
    }

    if (Tcl_GetInt(interp, string, &i_value) == TCL_OK) {
	;
    }
    else if (Tcl_GetDouble(interp, string, &f_value) == TCL_OK) {
#if 0
	/* Some machines don't have the "trunc" function */
	if (opTrunc) {
	    i_value = (int) trunc(f_value);
	}
	else {
	    i_value = (int) f_value;
	}
#else
	i_value = (int) f_value;
#endif
    }
    else if (opNocomplain) {
	i_value = 0;
    }
    else {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "\"", string, 
	    "\" is not a valid numerical value", NULL);
	return TCL_ERROR;
    }
    
    sprintf(buff, "%d", i_value);
    Tcl_SetResult(interp, buff, TCL_VOLATILE);
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_HandleOptionsCmd
 *
 * 
 * argv[1] = recordName
 * argv[2] = validOptions
 * argv[3] = argList
 *           if (argv[3][0] == "-nounknown") then 
 * 		don't complain about unknown options
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_HandleOptionsCmd)
{
    int		listArgc;
    int		optArgc;
    char     ** listArgv = 0;
    char     ** optArgv  = 0;
    int		i, code = TCL_OK;
    int		noUnknown = 0;

    if (argc >= 2 && (strcmp(argv[1], "-nounknown") == 0)) {
	noUnknown = 1;
	argv[1] = argv[0];
	argc --;
	argv ++;
    }

    if (argc!=4) {
	return Tix_ArgcError(interp, argc, argv, 2, "w validOptions argList");
    }

    if (Tcl_SplitList(interp, argv[2], &optArgc,  &optArgv ) != TCL_OK) {
	code = TCL_ERROR;
	goto done;
    }
    if (Tcl_SplitList(interp, argv[3], &listArgc, &listArgv) != TCL_OK) {
	code = TCL_ERROR;	
	goto done;
    }

    if ((listArgc %2) == 1) {
	if (noUnknown || IsOption(listArgv[listArgc-1], optArgc, optArgv)) {
	    Tcl_AppendResult(interp, "value for \"", listArgv[listArgc-1],
		"\" missing", (char*)NULL);
	} else {
	    Tcl_AppendResult(interp, "unknown option \"", listArgv[listArgc-1],
		"\"", (char*)NULL);
	}
	code = TCL_ERROR;
	goto done;
    }
    for (i=0; i<listArgc; i+=2) {
	if (IsOption(listArgv[i], optArgc, optArgv)) {
	    Tcl_SetVar2(interp, argv[1], listArgv[i], listArgv[i+1], 0);
	}
	else if (!noUnknown) {
	    Tcl_AppendResult(interp, "unknown option \"", listArgv[i],
		"\"; must be one of \"", argv[2], "\".", NULL);
	    code = TCL_ERROR;
	    goto done;
	}
    }

  done:

    if (listArgv) {
	ckfree((char *) listArgv);
    }
    if (optArgv) {
	ckfree((char *) optArgv);
    }

    return code;
}

/*----------------------------------------------------------------------
 * Tix_SetWindowParent --
 *
 *	Sets the parent of a window. This is normally to change the
 *	state of toolbar and MDI windows between docking and free
 *	modes.
 *
 * Results:
 *	Standard Tcl results.
 *
 * Side effects:
 *	Windows may be re-parented. See user documentation.
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_ParentWindow)
{
    Tk_Window mainWin, tkwin, newParent;
    int parentId;

    if (argc != 3) {
	return Tix_ArgcError(interp, argc, argv, 1, "window parent");
    }
    mainWin = Tk_MainWindow(interp);
    if (mainWin == NULL) {
	Tcl_SetResult(interp, "interpreter does not have a main window",
	    TCL_STATIC);
	return TCL_ERROR;
    }

    tkwin = Tk_NameToWindow(interp, argv[1], mainWin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }

    newParent = Tk_NameToWindow(interp, argv[2], mainWin);
    if (newParent == NULL) {
	if (Tcl_GetInt(interp, argv[2], &parentId) != TCL_OK) {
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, "\"", argv[2],
		"\" must be a window pathname or ID", NULL);
	    return TCL_ERROR;
	}
    }

    return TixpSetWindowParent(interp, tkwin, newParent, parentId);
}

/*----------------------------------------------------------------------
 * Tix_StrEqCmd --
 *
 *	To test string equality. It is more readable to write
 *		if [tixStrEq $var1 $var2]
 *	than
 *		if ![string comp $var1 $var2]
 * 
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_StrEqCmd)
{
    if (argc != 3) {
	return Tix_ArgcError(interp, argc, argv, 1, "string1 string2");
    }
    if (strcmp(argv[1], argv[2]) == 0) {
	Tcl_SetResult(interp, "1", TCL_STATIC);
    } else {
	Tcl_SetResult(interp, "0", TCL_STATIC);
    }
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_StringSubCmd --
 *
 *	What does this do??
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_StringSubCmd)
{
    Tcl_DString buffer;
    char * str, *from, *to, *s, *e, *f;
    int n, m, l, k;
    int inited = 0;

    if (argc!=4) {
	return Tix_ArgcError(interp, argc, argv, 1, "strVar from to");
    }
    if ((str = (char*)Tcl_GetVar(interp, argv[1], 0)) == NULL) {
	Tcl_AppendResult(interp, "variable ", argv[1]," does not exist", NULL);
	return TCL_ERROR;
    }
    from = argv[2];
    to = argv[3];

    n = strlen(from);
    l = strlen(to);

    while (1) {
	s = str;
	k = 0;
	
	while (*s && *s != *from) {
	    /* Find the beginning of token */
	    s++; k++;
	}

	if (*s && *s == *from) {
	    for (m=0,e=s,f=from; *e && *f && *e==*f && m<n; e++,f++,m++) {
		;
	    }
	    if (!inited) {
		Tcl_DStringInit(&buffer);
		inited = 1;
	    }
	    if (m == n) {
		/* We found a match */
		if (s > str) {
		    /* copy the unmatched prefix */
		    Tcl_DStringAppend(&buffer, str, k);
		}
		Tcl_DStringAppend(&buffer, to, l);
		str = e;
	    } else {
		Tcl_DStringAppend(&buffer, str, k+m);
		str += k+m;
	    }
	    continue;
	}

	/* No match at all */
	if (*str) {
	    if (inited) {
		Tcl_DStringAppend(&buffer, str, k);
	    }
	}
	break;
    }

    if (inited) {
	Tcl_SetVar(interp, argv[1], buffer.string, 0);
	Tcl_DStringFree(&buffer);
    }
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_TmpLineCmd
 *
 * 	Draw a temporary line on the root window
 *
 * argv[1..] = x1 y1 x2 y2
 *----------------------------------------------------------------------
 */
TIX_DEFINE_CMD(Tix_TmpLineCmd)
{
    Tk_Window mainWin = (Tk_Window)clientData;
    Tk_Window tkwin;
    int x1, y1, x2, y2;
    
    if (argc != 5 && argc != 6) {
	return Tix_ArgcError(interp, argc, argv, 0,
	    "tixTmpLine x1 y1 x2 y2 ?window?");
    }
    if (Tcl_GetInt(interp, argv[1], &x1) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_GetInt(interp, argv[2], &y1) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_GetInt(interp, argv[3], &x2) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_GetInt(interp, argv[4], &y2) != TCL_OK) {
	return TCL_ERROR;
    }
    if (argc == 6) {
	/*
	 * argv[5] tells which display to draw the tmp lines on, in
	 * case the application has opened more than one displays. If
	 * this argv[5] is omitted, draws to the display where the
	 * main window is on.
	 */
	tkwin = Tk_NameToWindow(interp, argv[5], mainWin);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
    } else {
	tkwin = Tk_MainWindow(interp);
    }

    TixpDrawTmpLine(x1, y1, x2, y2, tkwin);
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * Tix_TrueCmd
 *
 *	Returns a true value regardless of the arguments. This is used to
 *	skip run-time debugging 
 *----------------------------------------------------------------------
 */

TIX_DEFINE_CMD(Tix_TrueCmd)
{
    Tcl_SetResult(interp, "1",TCL_STATIC);
    return TCL_OK;
}

/*----------------------------------------------------------------------
 * EventProc --
 *
 *	Monitors events sent to a window associated with a
 *	tixWidgetDoWhenIdle command. If this window is destroyed,
 *	remove the idle handlers associated with this window.
 *----------------------------------------------------------------------
 */

static void EventProc(clientData, eventPtr)
    ClientData clientData;
    XEvent *eventPtr;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    Tcl_HashSearch hSearch;
    Tcl_HashEntry * hashPtr;
    IdleStruct * iPtr;

    if (eventPtr->type != DestroyNotify) {
	return;
    }

    /* Iterate over all the entries in the hash table */
    for (hashPtr = Tcl_FirstHashEntry(&idleTable, &hSearch);
	 hashPtr;
	 hashPtr = Tcl_NextHashEntry(&hSearch)) {

	iPtr = (IdleStruct *)Tcl_GetHashValue(hashPtr);

	if (iPtr->tkwin == tkwin) {
	    Tcl_DeleteHashEntry(hashPtr);
	    Tk_CancelIdleCall(IdleHandler, (ClientData) iPtr);
	    ckfree((char*)iPtr->command);
	    ckfree((char*)iPtr);
	}
    }
}
/*----------------------------------------------------------------------
 * IdleHandler --
 *
 *	Called when Tk is idle. Dispatches all commands registered by
 *	tixDoWhenIdle and tixWidgetDoWhenIdle.
 *----------------------------------------------------------------------
 */

static void IdleHandler(clientData)
    ClientData clientData;	/* TCL command to evaluate */
{
    Tcl_HashEntry * hashPtr;
    IdleStruct * iPtr;

    iPtr = (IdleStruct *) clientData;

    /*
     * Clean up the hash table. Note that we have to do this BEFORE
     * calling the TCL command. Otherwise if the TCL command tries
     * to register itself again, it will fail in Tix_DoWhenIdleCmd()
     * because the command is still in the hashtable
     */
    hashPtr = Tcl_FindHashEntry(&idleTable, iPtr->command);
    if (hashPtr) {
	Tcl_DeleteHashEntry(hashPtr);
    } else {
	/* Probably some kind of error */
	return;
    }

    if (Tcl_GlobalEval(iPtr->interp, iPtr->command) != TCL_OK) {
	if (iPtr->tkwin != NULL) {
	    Tcl_AddErrorInfo(iPtr->interp,
		"\n    (idle event handler executed by tixWidgetDoWhenIdle)");
	} else {
	    Tcl_AddErrorInfo(iPtr->interp,
		"\n    (idle event handler executed by tixDoWhenIdle)");
	} 
	Tk_BackgroundError(iPtr->interp);
    }

    ckfree((char*)iPtr->command);
    ckfree((char*)iPtr);
}

/*----------------------------------------------------------------------
 * IsOption --
 *
 *	Checks whether the string pointed by "option" is one of the
 *	options given by the "optArgv" array.
 *----------------------------------------------------------------------
 */
static int IsOption(option, optArgc, optArgv)
    char *option;		/* Number of arguments. */ 
    int optArgc;		/* Number of arguments. */
    char **optArgv;		/* Argument strings. */
{
    int i;

    for (i=0; i<optArgc; i++) {
	if (strcmp(option, optArgv[i]) == 0) {
	    return 1;
	}
    }
    return 0;
}


static void MapEventProc(clientData, eventPtr)
    ClientData clientData;	/* TCL command to evaluate */
    XEvent *eventPtr;		/* Information about event. */
{
    Tcl_HashEntry     * hashPtr;
    MapEventStruct    * mPtr;
    MapCmdLink	      * cmd;

    if (eventPtr->type != MapNotify) {
	return;
    }

    mPtr = (MapEventStruct *) clientData;

    Tk_DeleteEventHandler(mPtr->tkwin, StructureNotifyMask,
	MapEventProc, (ClientData)mPtr);

    /* Clean up the hash table.
     */
    if ((hashPtr = Tcl_FindHashEntry(&mapEventTable, (char*)mPtr->tkwin))) {
	Tcl_DeleteHashEntry(hashPtr);
    }

    for (cmd = mPtr->cmds; cmd; ) {
	MapCmdLink * old;

	/* Execute the event handler */
	if (Tcl_GlobalEval(mPtr->interp, cmd->command) != TCL_OK) {
	    Tcl_AddErrorInfo(mPtr->interp,
		"\n    (event handler executed by tixDoWhenMapped)");
	    Tk_BackgroundError(mPtr->interp);
	}

	/* Delete the link */
	old = cmd;
	cmd = cmd->next;

	ckfree(old->command);
	ckfree((char*)old);
    }

    /* deallocate the mapEventStruct */
    ckfree((char*)mPtr);
}

static char *
NameOfColor(colorPtr)
   XColor * colorPtr;
{
    static char string[20];
    char *ptr;

    sprintf(string, "#%4x%4x%4x", colorPtr->red, colorPtr->green,
	colorPtr->blue);

    for (ptr = string; *ptr; ptr++) {
	if (*ptr == ' ') {
	    *ptr = '0';
	}
    }
    return string;
}


static XColor *
ScaleColor(tkwin, color, scale)
    Tk_Window tkwin;
    XColor * color;
    double scale;
{
    XColor test;

    test.red   = (int)((float)(color->red)   * scale);
    test.green = (int)((float)(color->green) * scale);
    test.blue  = (int)((float)(color->blue)  * scale);
    if (test.red > MAX_INTENSITY) {
	test.red = MAX_INTENSITY;
    }
    if (test.green > MAX_INTENSITY) {
	test.green = MAX_INTENSITY;
    }
    if (test.blue > MAX_INTENSITY) {
	test.blue = MAX_INTENSITY;
    }

    return Tk_GetColorByValue(tkwin, &test);
}
