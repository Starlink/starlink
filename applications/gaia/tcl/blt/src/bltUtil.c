/*
 * bltUtil.c --
 *
 *	This module implements utility procedures for the BLT
 *	toolkit.
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
#include <X11/Xutil.h>
#ifndef WIN32
#include <X11/Xproto.h>
#endif

#if defined(__STDC__)
#include <stdarg.h>
#else
#include <varargs.h>
#endif


#ifndef NDEBUG

void
Blt_Assert(testExpr, fileName, lineNumber)
    char *testExpr;
    char *fileName;
    int lineNumber;
{
#ifdef WIN32
    PurifyPrintf("line %d of %s: Assert \"%s\" failed", lineNumber, 
	fileName, testExpr);
#endif
    fprintf(stderr, "line %d of %s: Assert \"%s\" failed",
	lineNumber, fileName, testExpr);
    abort();
}

#endif


/*
 *----------------------------------------------------------------------
 *
 * Blt_AdjustViewport --
 *
 *	Adjusts the offsets of the viewport according to the scroll mode.
 *	This is to accommodate both "listbox" and "canvas" style scrolling.
 *
 *	"canvas"	The viewport scrolls within the range of world
 *			coordinates.  This way the viewport always displays
 *			a full page of the world.  If the world is smaller
 *			than the viewport, then (bizarrely) the world and
 *			viewport are inverted so that the world moves up
 *			and down within the viewport.
 *
 *	"listbox"	The viewport can scroll beyond the range of world
 *			coordinates.  Every entry can be displayed at the
 *			top of the viewport.  This also means that the
 *			scrollbar thumb weirdly shrinks as the last entry
 *			is scrolled upward.
 *
 * Results:
 *	The corrected offset is returned.
 *
 *----------------------------------------------------------------------
 */
int
Blt_AdjustViewport(offset, worldSize, windowSize, scrollUnits, scrollMode)
    int offset, worldSize, windowSize;
    int scrollUnits;
    int scrollMode;
{
    switch (scrollMode) {
    case SCROLL_MODE_CANVAS:
	/*
	 * Canvas-style scrolling allows the world to be scrolled
	 * within the window.
	 */
	if (worldSize < windowSize) {
	    if ((worldSize - offset) > windowSize) {
		offset = worldSize - windowSize;
	    }
	    if (offset > 0) {
		offset = 0;
	    }
	} else {
	    if ((offset + windowSize) > worldSize) {
		offset = worldSize - windowSize;
	    }
	    if (offset < 0) {
		offset = 0;
	    }
	}
	break;

    case SCROLL_MODE_LISTBOX:
	if (offset < 0) {
	    offset = 0;
	}
	if (offset >= worldSize) {
	    offset = worldSize - scrollUnits;
	}
	break;
    }
    return offset;
}

int
Blt_GetScrollInfo(interp, argc, argv, offsetPtr, worldSize, windowSize,
    scrollUnits, scrollMode)
    Tcl_Interp *interp;
    int argc;
    char **argv;
    int *offsetPtr;
    int worldSize, windowSize;
    int scrollUnits;
    int scrollMode;
{
    char c;
    unsigned int length;
    int offset;
    int count;
    double fract;

    offset = *offsetPtr;
    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 's') && (strncmp(argv[2], "scroll", length) == 0)) {
	/* scroll number unit/page */
	if (Tcl_GetInt(interp, argv[3], &count) != TCL_OK) {
	    return TCL_ERROR;
	}
	c = argv[4][0];
	length = strlen(argv[4]);
	if ((c == 'u') && (strncmp(argv[4], "units", length) == 0)) {
	    fract = (double)count *scrollUnits;
	} else if ((c == 'p') && (strncmp(argv[4], "pages", length) == 0)) {
	    /* A page is 90% of the view-able window. */
	    fract = (double)count *windowSize * 0.9;
	} else {
	    Tcl_AppendResult(interp, "unknown \"scroll\" units \"", argv[4],
		"\"", (char *)NULL);
	    return TCL_ERROR;
	}
	offset += (int)fract;
    } else if ((c == 'm') && (strncmp(argv[2], "moveto", length) == 0)) {
	/* moveto fraction */
	if (Tcl_GetDouble(interp, argv[3], &fract) != TCL_OK) {
	    return TCL_ERROR;
	}
	offset = (int)(worldSize * fract);
    } else {
	/* Treat like "scroll units" */
	if (Tcl_GetInt(interp, argv[2], &count) != TCL_OK) {
	    return TCL_ERROR;
	}
	fract = (double)count *scrollUnits;
	offset += (int)fract;
	return TCL_OK;
    }
    *offsetPtr = Blt_AdjustViewport(offset, worldSize, windowSize, scrollUnits,
	scrollMode);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_UpdateScrollbar --
 *
 * 	Invoke a Tcl command to the scrollbar, defining the new
 *	position and length of the scroll. See the Tk documentation
 *	for further information on the scrollbar.  It is assumed the
 *	scrollbar command prefix is valid.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Scrollbar is commanded to change position and/or size.
 *
 * ----------------------------------------------------------------------
 */
void
Blt_UpdateScrollbar(interp, scrollCmd, firstFract, lastFract)
    Tcl_Interp *interp;
    char *scrollCmd;		/* scrollbar command */
    double firstFract, lastFract;
{
    char string[200];
    Tcl_DString cmdString;

    Tcl_DStringInit(&cmdString);
    Tcl_DStringAppend(&cmdString, scrollCmd, -1);
    sprintf(string, " %f %f", firstFract, lastFract);
    Tcl_DStringAppend(&cmdString, string, -1);
    if (Tcl_GlobalEval(interp, Tcl_DStringValue(&cmdString)) != TCL_OK) {
	Tk_BackgroundError(interp);
    }
    Tcl_DStringFree(&cmdString);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ConfigModified --
 *
 *      Given the configuration specifications and one or more option
 *	patterns (terminated by a NULL), indicate if any of the matching
 *	configuration options has been reset.
 *
 * Results:
 *      Returns 1 if one of the options has changed, 0 otherwise.
 *
 *----------------------------------------------------------------------
 */
int Blt_ConfigModified
TCL_VARARGS_DEF(Tk_ConfigSpec *, arg1)
{
    va_list argList;
    Tk_ConfigSpec *specs;
    register Tk_ConfigSpec *specPtr;
    register char *option;

    specs = TCL_VARARGS_START(Tk_ConfigSpec *, arg1, argList);
    while ((option = va_arg(argList, char *)) != NULL) {
	for (specPtr = specs; specPtr->type != TK_CONFIG_END; specPtr++) {
	    if ((Tcl_StringMatch(specPtr->argvName, option)) &&
		(specPtr->specFlags & TK_CONFIG_OPTION_SPECIFIED)) {
		va_end(argList);
		return 1;
	    }
	}
    }
    va_end(argList);
    return 0;
}

void Blt_DStringAppendElements
TCL_VARARGS_DEF(Tcl_DString *, arg1)
{
    va_list argList;
    Tcl_DString *dStrPtr;
    register char *elem;

    dStrPtr = TCL_VARARGS_START(Tcl_DString *, arg1, argList);
    while ((elem = va_arg(argList, char *)) != NULL) {
	Tcl_DStringAppendElement(dStrPtr, elem);
    }
    va_end(argList);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LookupOperation --
 *
 *      Performs a binary search on the array of command operation
 *      specifications to find a partial, anchored match for the
 *      given operation string.
 *
 * Results:
 *	If the string matches unambiguously the index of the specification
 *	in the array is returned.  If the string does not match, even
 *	as an abbreviation, any operation, -1 is returned.  If the string
 *	matches, but ambiguously -2 is returned.
 *
 *----------------------------------------------------------------------
 */
int
Blt_LookupOperation(specArr, numSpecs, operation)
    Blt_OpSpec specArr[];
    int numSpecs;
    char *operation;		/* Name of minor operation to search for */
{
    Blt_OpSpec *specPtr;
    char c;
    register int high, low, median;
    register int compare, length;

    low = 0;
    high = numSpecs - 1;
    c = operation[0];
    length = strlen(operation);
    while (low <= high) {
	median = (low + high) >> 1;
	specPtr = specArr + median;

	/* Test the first character */
	compare = c - specPtr->name[0];
	if (!compare) {
	    /* Now test the entire string */
	    compare = strncmp(operation, specPtr->name, length);
	    if ((compare == 0) && (length < specPtr->minChars)) {
		return -2;	/* Ambiguous operation name */
	    }
	}
	if (compare < 0) {
	    high = median - 1;
	} else if (compare > 0) {
	    low = median + 1;
	} else {
	    return median;	/* Op found. */
	}
    }
    return -1;			/* Can't find operation */
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetOperation --
 *
 *      Find the command operation given a string name.  This is useful
 *      where a group of command operations have the same argument
 *      signature.
 *
 * Results:
 *      If found, a pointer to the procedure (function pointer) is
 *      returned.  Otherwise NULL is returned and an error message
 *      containing a list of the possible commands is returned in
 *      interp->result.
 *
 *----------------------------------------------------------------------
 */
Blt_Operation
Blt_GetOperation(interp, numSpecs, specArr, argIndex, numArgs, argArr)
    Tcl_Interp *interp;		/* Interpreter to report errors to */
    int numSpecs;		/* Number of specifications in array */
    Blt_OpSpec specArr[];	/* Operation specification array */
    Blt_OpIndex argIndex;	/* Index of the operation name argument */
    int numArgs;		/* Number of arguments in the argument vector.
				 * This includes any prefixed arguments */
    char *argArr[];		/* Argument vector */
{
    Blt_OpSpec *specPtr;
    char *string;
    register int i;
    register int specIndex;

    if (numArgs <= argIndex) {	/* No operation argument */
	Tcl_AppendResult(interp, "wrong # args: ", (char *)NULL);
      usage:
	Tcl_AppendResult(interp, "should be one of...", (char *)NULL);
	for (specIndex = 0; specIndex < numSpecs; specIndex++) {
	    Tcl_AppendResult(interp, "\n  ", (char *)NULL);
	    for (i = 0; i < argIndex; i++) {
		Tcl_AppendResult(interp, argArr[i], " ", (char *)NULL);
	    }
	    specPtr = specArr + specIndex;
	    Tcl_AppendResult(interp, specPtr->name, " ", specPtr->usage,
		(char *)NULL);
	}
	return NULL;
    }
    string = argArr[argIndex];
    specIndex = Blt_LookupOperation(specArr, numSpecs, string);

    if (specIndex == -2) {
	char c;
	int length;

	Tcl_AppendResult(interp, "ambiguous", (char *)NULL);
	if (argIndex > 2) {
	    Tcl_AppendResult(interp, " ", argArr[argIndex - 1], (char *)NULL);
	}
	Tcl_AppendResult(interp, " operation \"", string, "\" matches:",
	    (char *)NULL);

	c = string[0];
	length = strlen(string);
	for (specIndex = 0; specIndex < numSpecs; specIndex++) {
	    specPtr = specArr + specIndex;
	    if ((c == specPtr->name[0]) &&
		(strncmp(string, specPtr->name, length) == 0)) {
		Tcl_AppendResult(interp, " ", specPtr->name, (char *)NULL);
	    }
	}
	return NULL;

    } else if (specIndex == -1) {	/* Can't find operation, display help */
	Tcl_AppendResult(interp, "bad", (char *)NULL);
	if (argIndex > 2) {
	    Tcl_AppendResult(interp, " ", argArr[argIndex - 1], (char *)NULL);
	}
	Tcl_AppendResult(interp, " operation \"", string, "\": ", (char *)NULL);
	goto usage;
    }
    specPtr = specArr + specIndex;
    if ((numArgs < specPtr->minArgs) || ((specPtr->maxArgs > 0) &&
	    (numArgs > specPtr->maxArgs))) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", (char *)NULL);
	for (i = 0; i < argIndex; i++) {
	    Tcl_AppendResult(interp, argArr[i], " ", (char *)NULL);
	}
	Tcl_AppendResult(interp, specPtr->name, " ", specPtr->usage, "\"",
	    (char *)NULL);
	return NULL;
    }
    return (specPtr->proc);
}

#ifndef HAVE_STRDUP
/*
 *----------------------------------------------------------------------
 *
 * strdup --
 *
 *      Create a copy of the string from heap storage.
 *
 * Results:
 *      Returns a pointer to the need string copy.
 *
 *----------------------------------------------------------------------
 */
char *
strdup(string)
    char *string;
{
    char *newPtr;

    newPtr = (char *)malloc(sizeof(char) * (strlen(string) + 1));
    if (newPtr != NULL) {
	strcpy(newPtr, string);
    }
    return (newPtr);
}

#endif /*HAVE_STRDUP*/

#ifdef notdef
#ifndef HAVE_STRCASECMP

static unsigned char lcase[] =
{
    '\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
    '\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
    '\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
    '\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
    '\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
    '\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
    '\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
    '\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
    '\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
    '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
    '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
    '\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
    '\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
    '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
    '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
    '\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
    '\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
    '\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
    '\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
    '\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
    '\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
    '\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
    '\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
    '\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
    '\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
    '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
    '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
    '\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
    '\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
    '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
    '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
    '\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

/*
 *----------------------------------------------------------------------
 *
 * strcasecmp --
 *
 *      Compare two strings, disregarding case.
 *
 * Results:
 *      Returns a signed integer representing the following:
 *
 *	zero      - two strings are equal
 *	negative  - first string is less than second
 *	positive  - first string is greater than second
 *
 *----------------------------------------------------------------------
 */
int
strcasecmp(str1, str2)
    CONST char *str1;
    CONST char *str2;
{
    unsigned char *s = (unsigned char *)str1;
    unsigned char *t = (unsigned char *)str2;

    for ( /* empty */ ; (lcase[*s] == lcase[*t]); s++, t++) {
	if (*s == '\0') {
	    return 0;
	}
    }
    return (lcase[*s] - lcase[*t]);
}

/*
 *----------------------------------------------------------------------
 *
 * strncasecmp --
 *
 *      Compare two strings, disregarding case, up to a given length.
 *
 * Results:
 *      Returns a signed integer representing the following:
 *
 *	zero      - two strings are equal
 *	negative  - first string is less than second
 *	positive  - first string is greater than second
 *
 *----------------------------------------------------------------------
 */
int
strncasecmp(str1, str2, length)
    CONST char *str1;
    CONST char *str2;
    size_t length;
{
    register unsigned char *s = (unsigned char *)str1;
    register unsigned char *t = (unsigned char *)str2;

    for ( /* empty */ ; (length > 0); s++, t++, length--) {
	if (lcase[*s] != lcase[*t]) {
	    return (lcase[*s] - lcase[*t]);
	}
	if (*s == '\0') {
	    return 0;
	}
    }
    return 0;
}

#endif /* !HAVE_STRNCASECMP */
#endif

static char stringRep[200];

char *
Blt_Int(value)
    int value;
{
    sprintf(stringRep, "%d", value);
    return stringRep;
}

char *
Blt_Double(interp, value)
    Tcl_Interp *interp;
    double value;
{
    Tcl_PrintDouble(interp, value, stringRep);
    return stringRep;
}

/* -------------- */
/*
 *----------------------------------------------------------------------
 *
 * Blt_GetPrivateGCFromDrawable --
 *
 *      Like Tk_GetGC, but doesn't share the GC with any other widget.
 *	This is needed because the certain GC parameters (like dashes)
 *	can not be set via XCreateGC, therefore there is no way for
 *	Tk's hashing mechanism to recognize that two such GCs differ.
 *
 * Results:
 *      A new GC is returned.
 *
 *----------------------------------------------------------------------
 */
GC
Blt_GetPrivateGCFromDrawable(tkwin, drawable, gcMask, valuePtr)
    Tk_Window tkwin;
    Drawable drawable;
    unsigned long gcMask;
    XGCValues *valuePtr;
{
    Pixmap pixmap;
    GC newGC;

    pixmap = None;
    if (drawable == None) {
	Drawable root;
	int depth;

	root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
	depth = Tk_Depth(tkwin);

	if (depth == DefaultDepth(Tk_Display(tkwin), Tk_ScreenNumber(tkwin))) {
	    drawable = root;
	} else {
	    pixmap = Tk_GetPixmap(Tk_Display(tkwin), root, 1, 1, depth);
	    drawable = pixmap;
	}
    }
#ifdef WIN32
    newGC = Blt_CreateGC(Tk_Display(tkwin), drawable, gcMask, valuePtr);
#else
    newGC = XCreateGC(Tk_Display(tkwin), drawable, gcMask, valuePtr);
#endif    
    if (pixmap != None) {
	Tk_FreePixmap(Tk_Display(tkwin), pixmap);
    }
    return (newGC);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetPrivateGC --
 *
 *      Like Tk_GetGC, but doesn't share the GC with any other widget.
 *	This is needed because the certain GC parameters (like dashes)
 *	can not be set via XCreateGC, therefore there is no way for
 *	Tk's hashing mechanism to recognize that two such GCs differ.
 *
 * Results:
 *      A new GC is returned.
 *
 *----------------------------------------------------------------------
 */
GC
Blt_GetPrivateGC(tkwin, gcMask, valuePtr)
    Tk_Window tkwin;
    unsigned long gcMask;
    XGCValues *valuePtr;
{
    return Blt_GetPrivateGCFromDrawable(tkwin, Tk_WindowId(tkwin), gcMask,
	valuePtr);
}

void
Blt_FreePrivateGC(display, gc)
    Display *display;
    GC gc;
{
    Tk_FreeXId(display, (XID) XGContextFromGC(gc));
    XFreeGC(display, gc);
}

#ifndef WIN32
void
Blt_SetDashes(display, gc, dashesPtr)
    Display *display;
    GC gc;
    Dashes *dashesPtr;
{
    XSetDashes(display, gc, dashesPtr->offset, dashesPtr->valueArr, 
	       dashesPtr->numValues);
}
#endif

    
/*
 *----------------------------------------------------------------------
 *
 * Blt_FindChild --
 *
 *      Searches the parent for a named child window.
 *
 *	Oddly enough, the isn't in Tk's C API, but only in it's Tcl API.
 *	It's easy enough to do here--that is, if you peek into the
 *	Tk_Window structure.
 *
 * Results:
 *      The child Tk_Window. If the named child can't be found, NULL
 *	is returned.
 *
 *----------------------------------------------------------------------
 */

#define FirstChild(tkwin)	((Tk_Window)(((Tk_FakeWin *) (tkwin))->dummy2))
#define NextChild(tkwin)	((Tk_Window)(((Tk_FakeWin *) (tkwin))->dummy4))

/*LINTLIBRARY*/
Tk_Window
Blt_FindChild(parent, name)
    Tk_Window parent;
    char *name;
{
    register Tk_Window tkwin;

    for (tkwin = FirstChild(parent); tkwin != NULL; tkwin = NextChild(tkwin)) {
	if (strcmp(name, Tk_Name(tkwin)) == 0) {
	    return (tkwin);
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_Toplevel --
 *
 *      Climbs up the widget hierarchy to find the top level window of
 *      the window given.
 *
 * Results:
 *      Returns the Tk_Window of the toplevel widget.
 *
 *----------------------------------------------------------------------
 */
Tk_Window
Blt_Toplevel(tkwin)
    register Tk_Window tkwin;
{
    while (!Tk_IsTopLevel(tkwin)) {
	tkwin = Tk_Parent(tkwin);
    }
    return tkwin;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_WindowId --
 *
 *      Returns the XID for the Tk_Window given.  Starting in Tk 8.0,
 *      the toplevel widgets are wrapped by another window.
 *      Currently there's no way to get at that window, other than
 *      what is done here: query the X window hierarchy and grab the
 *      parent.
 *
 * Results:
 *      Returns the X Window ID of the widget.  If it's a toplevel, then
 *	the XID of the wrapper is returned.
 *
 *----------------------------------------------------------------------
 */
Window
Blt_WindowId(tkwin)
    Tk_Window tkwin;
{
    Window window;
#ifdef WIN32
    HWND hWnd;

    hWnd = Tk_GetHWND(Tk_WindowId(tkwin));
#if (TK_MAJOR_VERSION >= 8)
    if (Tk_IsTopLevel(tkwin)) {
	hWnd = GetParent(hWnd);
    }
#endif /* TK_MAJOR_VERSION >= 8 */
    window = (Window) hWnd;
#else
    window = Tk_WindowId(tkwin);
#if (TK_MAJOR_VERSION >= 8)
    if (Tk_IsTopLevel(tkwin)) {
	Window root, parent;
	Window *childrenPtr;
	unsigned int numChildren;

	parent = None;
	if (XQueryTree(Tk_Display(tkwin), Tk_WindowId(tkwin), &root, &parent,
		&childrenPtr, &numChildren) > 0) {
	    XFree(childrenPtr);
	    window = parent;
	}
    }
#endif /* TK_MAJOR_VERSION >= 8 */
#endif /* WIN32 */
    return window;
}

#ifdef WIN32

int
Blt_ReparentWindow(display, window, newParent, x, y)
    Display *display;
    Window window, newParent;
    int x, y;
{
    XReparentWindow(display, window, newParent, x, y);
    return TCL_OK;
}

#else

/* ARGSUSED */
static int
XReparentWindowErrorProc(clientData, errEventPtr)
    ClientData clientData;
    XErrorEvent *errEventPtr;
{
    int *errorPtr = (int *)clientData;

    *errorPtr = TCL_ERROR;
    return 0;
}

int
Blt_ReparentWindow(display, window, newParent, x, y)
    Display *display;
    Window window, newParent;
    int x, y;
{
    Tk_ErrorHandler handler;
    int result;
    int any = -1;

    result = TCL_OK;
    handler = Tk_CreateErrorHandler(display, any, X_ReparentWindow, any,
	XReparentWindowErrorProc, (ClientData)&result);
    XReparentWindow(display, window, newParent, x, y);
    Tk_DeleteErrorHandler(handler);
    XSync(display, False);
    return result;
}

#endif


/*
 *----------------------------------------------------------------------
 *
 * Blt_RaiseToLevelWindow --
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_RaiseTopLevelWindow(tkwin)
    Tk_Window tkwin;
{
#ifdef WIN32
    HWND hWnd;

    hWnd = (HWND) Blt_WindowId(tkwin);
    SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
#else
    Window window;

    window = (Window) Blt_WindowId(tkwin);
    XRaiseWindow(Tk_Display(tkwin), window);
#endif /* WIN32 */
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_MapTopLevelWindow --
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_MapTopLevelWindow(tkwin)
    Tk_Window tkwin;
{
#ifdef WIN32
    HWND hWnd;

    hWnd = (HWND) Blt_WindowId(tkwin);
    ShowWindow(hWnd, SW_SHOWNORMAL);
#else
    Window window;

    window = (Window) Blt_WindowId(tkwin);
    XMapWindow(Tk_Display(tkwin), window);
#endif /* WIN32 */
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_MapTopLevelWindow --
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Blt_UnmapTopLevelWindow(tkwin)
    Tk_Window tkwin;
{
#ifdef WIN32
    HWND hWnd;

    hWnd = (HWND) Blt_WindowId(tkwin);
    ShowWindow(hWnd, SW_HIDE);
#else
    Window window;

    window = (Window) Blt_WindowId(tkwin);
    XUnmapWindow(Tk_Display(tkwin), window);
#endif /* WIN32 */
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ConfigureWidgetComponent --
 *
 *	Configures a component of a widget.  This is useful for
 *	widgets that have multiple components which aren't uniquely
 *	identified by a Tk_Window. It allows us, for example, set
 *	resources for axes of the graph widget. The graph really has
 *	only one window, but its convenient to specify components in a
 *	hierarchy of options.
 *
 *		*graph.x.logScale yes
 *		*graph.Axis.logScale yes
 *		*graph.temperature.scaleSymbols yes
 *		*graph.Element.scaleSymbols yes
 *
 *	This is really a hack to work around the limitations of the Tk
 *	resource database.  It creates a temporary window, needed to
 *	call Tk_ConfigureWidget, using the name of the component.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side Effects:
 *	A temporary window is created merely to pass to Tk_ConfigureWidget.
 *
 *----------------------------------------------------------------------
 */
int
Blt_ConfigureWidgetComponent(interp, parent, name, class, specsPtr, argc, argv,
    widgRec, flags)
    Tcl_Interp *interp;
    Tk_Window parent;		/* Window to associate with component */
    char name[];		/* Name of component */
    char class[];
    Tk_ConfigSpec *specsPtr;
    int argc;
    char *argv[];
    char *widgRec;
    int flags;
{
    Tk_Window tkwin;
    int result;
    char *tempName;
    int temporary = 0;

    tempName = strdup(name);

    /* Window name can't start with an upper case letter */
    tempName[0] = tolower(name[0]);

    /*
     * Create component if a child window by the component's name
     * doesn't already exist.
     */
    tkwin = Blt_FindChild(parent, tempName);
    if (tkwin == NULL) {
	tkwin = Tk_CreateWindow(interp, parent, tempName, (char *)NULL);
	temporary = 1;
    }
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "can't find window in \"", Tk_PathName(parent),
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    assert(Tk_Depth(tkwin) == Tk_Depth(parent));
    free(tempName);

    Tk_SetClass(tkwin, class);
    result = Tk_ConfigureWidget(interp, tkwin, specsPtr, argc, argv, widgRec,
	flags);
    if (temporary) {
	Tk_DestroyWindow(tkwin);
    }
    return (result);
}

/*
 * The hash table below is used to keep track of all the Tk_Uids created
 * so far.
 */
static Tcl_HashTable uidTable;
static int uidInitialized = 0;

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetUid --
 *
 *	Given a string, returns a unique identifier for the string.
 *	A reference count is maintained, so that the identifier
 *	can be freed when it is not needed any more. This can be used
 *	in many places to replace Tcl_GetUid.
 *
 * Results:
 *	This procedure returns a Tk_Uid corresponding to the "string"
 *	argument.  The Tk_Uid has a string value identical to string
 *	(strcmp will return 0), but it's guaranteed that any other
 *	calls to this procedure with a string equal to "string" will
 *	return exactly the same result (i.e. can compare Tk_Uid
 *	*values* directly, without having to call strcmp on what they
 *	point to).
 *
 * Side effects:
 *	New information may be entered into the identifier table.
 *
 *----------------------------------------------------------------------
 */
Tk_Uid
Blt_GetUid(string)
    char *string;		/* String to convert. */
{
    int isNew;
    Tcl_HashEntry *hPtr;
    int refCount;

    if (!uidInitialized) {
	Tcl_InitHashTable(&uidTable, TCL_STRING_KEYS);
	uidInitialized = 1;
    }
    hPtr = Tcl_CreateHashEntry(&uidTable, string, &isNew);
    if (isNew) {
	refCount = 0;
    } else {
	refCount = (int)Tcl_GetHashValue(hPtr);
    }
    refCount++;
    Tcl_SetHashValue(hPtr, (ClientData)refCount);
    return (Tk_Uid) Tcl_GetHashKey(&uidTable, hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FreeUid --
 *
 *	Frees the Tk_Uid if there are no more clients using this
 *	identifier.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The identifier may be deleted from the identifier table.
 *
 *----------------------------------------------------------------------
 */
void
Blt_FreeUid(uid)
    Tk_Uid uid;			/* Identifier to release. */
{
    Tcl_HashEntry *hPtr;

    if (!uidInitialized) {
	Tcl_InitHashTable(&uidTable, TCL_STRING_KEYS);
	uidInitialized = 1;
    }
    hPtr = Tcl_FindHashEntry(&uidTable, uid);
    if (hPtr) {
	int refCount;

	refCount = (int)Tcl_GetHashValue(hPtr);
	refCount--;
	if (refCount == 0) {
	    Tcl_DeleteHashEntry(hPtr);
	} else {
	    Tcl_SetHashValue(hPtr, (ClientData)refCount);
	}
    } else {
	fprintf(stderr, "tried to release unknown identifier \"%s\"\n", uid);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FindUid --
 *
 *	Returns a Tk_Uid associated with a given string, if one exists.
 *
 * Results:
 *	A Tk_Uid for the string if one exists. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
Tk_Uid
Blt_FindUid(string)
    char *string;		/* String to find. */
{
    Tcl_HashEntry *hPtr;

    if (!uidInitialized) {
	Tcl_InitHashTable(&uidTable, TCL_STRING_KEYS);
	uidInitialized = 1;
    }
    hPtr = Tcl_FindHashEntry(&uidTable, string);
    if (hPtr == NULL) {
	return NULL;
    }
    return (Tk_Uid) Tcl_GetHashKey(&uidTable, hPtr);
}

/*
 *--------------------------------------------------------------
 *
 * Blt_InitHexTable --
 *
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 *
 *	We build the table at run time for several reasons:
 *
 *     	  1.  portable to non-ASCII machines.
 *	  2.  still reentrant since we set the init flag after setting
 *            table.
 *        3.  easier to extend.
 *        4.  less prone to bugs.
 *
 * Results:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
Blt_InitHexTable(tab)
    char tab[];
{
    tab['0'] = 0;
    tab['1'] = 1;
    tab['2'] = 2;
    tab['3'] = 3;
    tab['4'] = 4;
    tab['5'] = 5;
    tab['6'] = 6;
    tab['7'] = 7;
    tab['8'] = 8;
    tab['9'] = 9;
    tab['a'] = tab['A'] = 10;
    tab['b'] = tab['B'] = 11;
    tab['c'] = tab['C'] = 12;
    tab['d'] = tab['D'] = 13;
    tab['e'] = tab['E'] = 14;
    tab['f'] = tab['F'] = 15;
}
