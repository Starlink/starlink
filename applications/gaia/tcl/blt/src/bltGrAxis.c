
/*
 * bltGrAxis.c --
 *
 *	This module implements coordinate axes for the BLT graph widget.
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
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

#include "bltGraph.h"
#include "bltGrElem.h"
#include <ctype.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define DEF_NUM_TICKS		4	/* Each minor tick is 20% */
#define STATIC_TICK_SPACE	10

#define TICK_LABEL_SIZE		200
#define MAXTICKS		1000


#define TitleHeight(a)		((a)->virtAxisPtr->titleAttr.height)

/*
 * Round x in terms of units
 */
#define UROUND(x,u)		(Round((x)/(u))*(u))
#define UCEIL(x,u)		(ceil((x)/(u))*(u))
#define UFLOOR(x,u)		(floor((x)/(u))*(u))

#define LENGTH_MAJOR_TICK 	0.030	/* Length of a major tick */
#define LENGTH_MINOR_TICK 	0.015	/* Length of a minor (sub)tick */
#define LENGTH_LABEL_TICK 	0.040	/* Distance from graph to start of the
					 * label */

#ifdef __xxlinux__
#define NUMDIGITS	14	/* Specifies the number of digits of accuracy
				 * used when outputting axis tick labels. */
#else
#define NUMDIGITS	15	/* Specifies the number of digits of accuracy
				 * used when outputting axis tick labels. */
#endif

typedef enum AxisComponents {
    MAJOR_TICK, MINOR_TICK, TICK_LABEL, AXIS_LINE
} AxisComponent;

#define AVG_TICK_NUM_CHARS	16	/* Assumed average tick label size */

/* Map normalized coordinates to window coordinates */
#define MAPH(G,x)  	(Round((x) * (G)->hRange) + (G)->hOffset)
#define MAPV(G,y)   	(Round((1.0 - (y)) * (G)->vRange) + (G)->vOffset)

#define HORIZAXIS(a)	(!((a)->site & 1))	/* Even sites are horizontal */

/* Map graph coordinates to normalized coordinates [0..1] */
#define NORMALIZE(A,x) 	(((x) - (A)->limitsPtr->min) / (A)->limitsPtr->range)


static int StringToAxisLimit _ANSI_ARGS_((ClientData clientData, 
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *AxisLimitToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption minOption =
{
    StringToAxisLimit, AxisLimitToString, (ClientData)AXIS_AUTO_MIN,
};

static Tk_CustomOption maxOption =
{
    StringToAxisLimit, AxisLimitToString, (ClientData)AXIS_AUTO_MAX,
};

static int StringToTicks _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *TicksToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption ticksOption =
{
    StringToTicks, TicksToString, (ClientData)0,
};

static int StringToAxis _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *AxisToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

Tk_CustomOption bltXAxisOption =
{
    StringToAxis, AxisToString, (ClientData)AXIS_TYPE_X
};

Tk_CustomOption bltYAxisOption =
{
    StringToAxis, AxisToString, (ClientData)AXIS_TYPE_Y
};

Tk_CustomOption bltAnyXAxisOption =
{
    StringToAxis, AxisToString, (ClientData)(AXIS_TYPE_X | AXIS_ALLOW_NULL)
};


Tk_CustomOption bltAnyYAxisOption =
{
    StringToAxis, AxisToString, (ClientData)(AXIS_TYPE_Y | AXIS_ALLOW_NULL)
};

static int StringToLimits _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *LimitsToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption limitsOption =
{
    StringToLimits, LimitsToString, (ClientData)0,
};

extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltShadowOption;

/* Axis flags: */

#define DEF_AXIS_ALT_HIDE		"no"
#define DEF_AXIS_COMMAND		(char *)NULL
#define DEF_AXIS_DESCENDING		"no"
#define DEF_AXIS_FG_COLOR		RGB_COLOR_BLACK
#define DEF_AXIS_FG_MONO		RGB_COLOR_BLACK
#define DEF_AXIS_HIDE			"no"
#define DEF_AXIS_HIDE_ALT		"no"
#define DEF_AXIS_HIDE_STD		"yes"
#define DEF_AXIS_JUSTIFY		"center"
#define DEF_AXIS_LIMITS_STR		(char *)NULL
#define DEF_AXIS_LINE_WIDTH		"1"
#define DEF_AXIS_LOGSCALE		"no"
#define DEF_AXIS_LOG_SCALE		"no"
#define DEF_AXIS_LOOSE			"no"
#define DEF_AXIS_MAJOR_TICKS		(char *)NULL
#define DEF_AXIS_MAX			(char *)NULL
#define DEF_AXIS_MIN			(char *)NULL
#define DEF_AXIS_RANGE			"0.0"
#define DEF_AXIS_ROTATE			"0.0"
#define DEF_AXIS_SHADOW_COLOR		(char *)NULL
#define DEF_AXIS_SHADOW_MONO		(char *)NULL
#define DEF_AXIS_SHIFTBY		"0.0"
#define DEF_AXIS_SHOWTICKS		"yes"
#define DEF_AXIS_STEPSIZE		"0.0"
#define DEF_AXIS_SUBDIVISIONS		"2"
#define DEF_AXIS_TICKS			"0"
#ifdef WIN32
#define DEF_AXIS_TICK_FONT		"{Courier New Bold} 8"
#else
#define DEF_AXIS_TICK_FONT		"*-Courier-Bold-R-Normal-*-10-100-*"
#endif
#define DEF_AXIS_TICK_LENGTH		"0.1i"
#define DEF_AXIS_TICK_VALUES		(char *)NULL
#define DEF_AXIS_TITLE			(char *)NULL
#define DEF_AXIS_TITLE_FG		RGB_COLOR_BLACK
#define DEF_AXIS_TITLE_FONT		STD_FONT
#define DEF_AXIS_X_STEPSIZE_BARCHART	"1.0"
#define DEF_AXIS_X_SUBDIVISIONS_BARCHART "0"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_DOUBLE, "-autorange", "autoRange", "AutoRange",
	DEF_AXIS_RANGE, Tk_Offset(VirtualAxis, autoRange),
	BARCHART | STRIPCHART | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_AXIS_FG_COLOR, Tk_Offset(VirtualAxis, tickAttr.color),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_AXIS_FG_MONO, Tk_Offset(VirtualAxis, tickAttr.color),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS},
    {TK_CONFIG_STRING, "-command", "command", "Command",
	DEF_AXIS_COMMAND, Tk_Offset(VirtualAxis, formatCmd),
	TK_CONFIG_NULL_OK | ALL_GRAPHS},
    {TK_CONFIG_BOOLEAN, "-descending", "descending", "Descending",
	DEF_AXIS_DESCENDING, Tk_Offset(VirtualAxis, descending),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_AXIS_HIDE, Tk_Offset(VirtualAxis, hidden),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_JUSTIFY, "-justify", "justify", "Justify",
	DEF_AXIS_JUSTIFY, Tk_Offset(VirtualAxis, titleAttr.justify),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-limitcolor", "limitColor", "Color",
	DEF_AXIS_FG_COLOR, Tk_Offset(VirtualAxis, limitAttr.color),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS},
    {TK_CONFIG_COLOR, "-limitcolor", "limitColor", "Color",
	DEF_AXIS_FG_MONO, Tk_Offset(VirtualAxis, limitAttr.color),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS},
    {TK_CONFIG_FONT, "-limitfont", "limitFont", "Font",
	DEF_AXIS_TICK_FONT, Tk_Offset(VirtualAxis, limitAttr.font), ALL_GRAPHS},
    {TK_CONFIG_CUSTOM, "-limits", "limits", "Limits",
	DEF_AXIS_LIMITS_STR, Tk_Offset(VirtualAxis, limitFormats),
	TK_CONFIG_NULL_OK | ALL_GRAPHS, &limitsOption},
    {TK_CONFIG_CUSTOM, "-limitshadow", "limitShadow", "Shadow",
	DEF_AXIS_SHADOW_COLOR, Tk_Offset(VirtualAxis, limitAttr.shadow),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-limitshadow", "limitShadow", "Shadow",
	DEF_AXIS_SHADOW_MONO, Tk_Offset(VirtualAxis, limitAttr.shadow),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	DEF_AXIS_LINE_WIDTH, Tk_Offset(VirtualAxis, lineWidth),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-logscale", "logScale", "LogScale",
	DEF_AXIS_LOGSCALE, Tk_Offset(VirtualAxis, logScale),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-loose", "loose", "Loose",
	DEF_AXIS_LOOSE, Tk_Offset(VirtualAxis, loose),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-majorticks", "majorTicks", "MajorTicks",
	DEF_AXIS_MAJOR_TICKS, Tk_Offset(VirtualAxis, reqMajorPtr),
	TK_CONFIG_NULL_OK | ALL_GRAPHS, &ticksOption},
    {TK_CONFIG_CUSTOM, "-max", "max", "Max",
	DEF_AXIS_MIN, 0, TK_CONFIG_NULL_OK | ALL_GRAPHS, &maxOption},
    {TK_CONFIG_CUSTOM, "-min", "min", "Min",
	DEF_AXIS_MAX, 0, TK_CONFIG_NULL_OK | ALL_GRAPHS, &minOption},
    {TK_CONFIG_CUSTOM, "-minorticks", "minorTicks", "MinorTicks",
	DEF_AXIS_TICK_VALUES, Tk_Offset(VirtualAxis, reqMinorPtr),
	TK_CONFIG_NULL_OK | ALL_GRAPHS, &ticksOption},
    {TK_CONFIG_DOUBLE, "-rotate", "rotate", "Rotate",
	DEF_AXIS_ROTATE, Tk_Offset(VirtualAxis, theta),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-shiftby", "shiftBy", "ShiftBy",
	DEF_AXIS_SHIFTBY, Tk_Offset(VirtualAxis, shiftBy),
	BARCHART | STRIPCHART | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-showticks", "showTicks", "ShowTicks",
	DEF_AXIS_SHOWTICKS, Tk_Offset(VirtualAxis, showTicks),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-stepsize", "stepSize", "StepSize",
	DEF_AXIS_STEPSIZE, Tk_Offset(VirtualAxis, reqStep),
	ALL_GRAPHS | TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-subdivisions", "subdivisions", "Subdivisions",
	DEF_AXIS_SUBDIVISIONS, Tk_Offset(VirtualAxis, reqNumMinorTicks),
	ALL_GRAPHS},
    {TK_CONFIG_FONT, "-tickfont", "tickFont", "Font",
	DEF_AXIS_TICK_FONT, Tk_Offset(VirtualAxis, tickAttr.font), ALL_GRAPHS},
    {TK_CONFIG_PIXELS, "-ticklength", "tickLength", "TickLength",
	DEF_AXIS_TICK_LENGTH, Tk_Offset(VirtualAxis, tickLength), ALL_GRAPHS},
    {TK_CONFIG_CUSTOM, "-tickshadow", "tickShadow", "Shadow",
	DEF_AXIS_SHADOW_COLOR, Tk_Offset(VirtualAxis, tickAttr.shadow),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-tickshadow", "tickShadow", "Shadow",
	DEF_AXIS_SHADOW_MONO, Tk_Offset(VirtualAxis, tickAttr.shadow),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_STRING, "-title", "title", "Title",
	DEF_AXIS_TITLE, Tk_Offset(VirtualAxis, titleText),
	TK_CONFIG_DONT_SET_DEFAULT | TK_CONFIG_NULL_OK | ALL_GRAPHS},
    {TK_CONFIG_COLOR, "-titlecolor", "titleColor", "Color",
	DEF_AXIS_FG_COLOR, Tk_Offset(VirtualAxis, titleAttr.color),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS},
    {TK_CONFIG_COLOR, "-titlecolor", "titleColor", "TitleColor",
	DEF_AXIS_FG_MONO, Tk_Offset(VirtualAxis, titleAttr.color),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS},
    {TK_CONFIG_FONT, "-titlefont", "titleFont", "Font",
	DEF_AXIS_TITLE_FONT, Tk_Offset(VirtualAxis, titleAttr.font), ALL_GRAPHS},
    {TK_CONFIG_CUSTOM, "-titleshadow", "titleShadow", "Shadow",
	DEF_AXIS_SHADOW_COLOR, Tk_Offset(VirtualAxis, titleAttr.shadow),
	TK_CONFIG_COLOR_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-titleshadow", "titleShadow", "Shadow",
	DEF_AXIS_SHADOW_MONO, Tk_Offset(VirtualAxis, titleAttr.shadow),
	TK_CONFIG_MONO_ONLY | ALL_GRAPHS, &bltShadowOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/* Forward declarations */
static void DestroyVirtualAxis _ANSI_ARGS_((Graph *graphPtr,
	VirtualAxis *axisPtr));
static int GetVirtualAxis _ANSI_ARGS_((Graph *graphPtr, char *name,
	unsigned int flags, VirtualAxis **axisPtrPtr));
static void FreeVirtualAxis _ANSI_ARGS_((Graph *graphPtr,
	VirtualAxis *axisPtr));

INLINE static int
Round(x)
    register double x;
{
    return (int) (x + ((x < 0.0) ? -0.5 : 0.5));
}

INLINE static double
Fabs(x)
    register double x;
{
    return ((x < 0.0) ? -x : x);
}

/*
 * ----------------------------------------------------------------------
 *
 * OutOfRange --
 *
 *	Determines if a value does not lie within a given range.
 *
 *	The value is normalized and compared against the interval
 *	[0..1], where 0.0 is the minimum and 1.0 is the maximum.
 *	DBL_EPSILON is the smallest number that can be represented
 *	on the host machine, such that (1.0 + epsilon) != 1.0.
 *
 *	Please note, *max* can't equal *min*.
 *
 * Results:
 *	Returns whether the value lies outside of the given range.
 *	If value is outside of the interval [min..max], 1 is returned;
 *	0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
INLINE static int
OutOfRange(value, limitsPtr)
    register double value;
    Limits *limitsPtr;
{
    register double norm;

    norm = (value - limitsPtr->min) / (limitsPtr->range);
    return (((norm - 1.0) > DBL_EPSILON) || 
	    (((1.0 - norm) - 1.0) > DBL_EPSILON));
}

/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * StringToAxis --
 *
 *	Convert the name of virtual axis to an pointer.
 *
 * Results:
 *	The return value is a standard Tcl result.  The axis flags are
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToAxis(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Flag indicating whether to allow the
				 * empty string as a valid axis name */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Used to look up pointer to graph */
    char *string;		/* String representing axis name */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of the flags field */
{
    VirtualAxis **axisPtrPtr = (VirtualAxis **)(widgRec + offset);
    unsigned int flags = (unsigned int)clientData;
    Graph *graphPtr;

    graphPtr = Blt_FindGraph(tkwin);
    if (*axisPtrPtr != NULL) {
	FreeVirtualAxis(graphPtr, *axisPtrPtr);
    }
    if ((flags & AXIS_ALLOW_NULL) && (string[0] == '\0')) {
	*axisPtrPtr = NULL;
    } else if (GetVirtualAxis(graphPtr, string, flags, axisPtrPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AxisToString --
 *
 *	Convert the window coordinates into a string.
 *
 * Results:
 *	The string representing the coordinate position is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
AxisToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of axis field */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    VirtualAxis *axisPtr = *(VirtualAxis **)(widgRec + offset);

    if (axisPtr == NULL) {
	return "";
    }
    return axisPtr->name;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToLimits --
 *
 *	Convert the name of virtual axis to an pointer.
 *
 * Results:
 *	The return value is a standard Tcl result.  The axis flags are
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToLimits(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Used to look up pointer to graph */
    char *string;		/* String representing formats list */
    char *widgRec;		/* Widget record */
    int offset;			/* Not used */
{
    VirtualAxis *axisPtr = (VirtualAxis *)(widgRec);
    char **elemArr;
    int numElem;

    if (axisPtr->limitFormats != NULL) {
	free((char *)axisPtr->limitFormats);
    }
    axisPtr->limitFormats = NULL;
    axisPtr->numFormats = 0;

    if ((string == NULL) || (*string == '\0')) {
	return TCL_OK;
    }
    if (Tcl_SplitList(interp, string, &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem > 2) {
	Tcl_AppendResult(interp, "too many elements in limits format list \"",
	    string, "\"", (char *)NULL);
	free((char *)elemArr);
	return TCL_ERROR;
    }
    axisPtr->limitFormats = elemArr;
    axisPtr->numFormats = numElem;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LimitsToString --
 *
 *	Convert the window coordinates into a string.
 *
 * Results:
 *	The string representing the coordinate position is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
LimitsToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of limits field */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    VirtualAxis *axisPtr = (VirtualAxis *)(widgRec);

    if (axisPtr->numFormats == 0) {
	return "";
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return Tcl_Merge(axisPtr->numFormats, axisPtr->limitFormats);
}

/*
 * ----------------------------------------------------------------------
 *
 * StringToAxisLimit --
 *
 *	Convert the string representation of an axis limit into its numeric
 *	form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToAxisLimit(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* Either AXIS_AUTO_MIN or AXIS_AUTO_MAX.
				 * Indicates which axis limit to set. */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* */
    char *widgRec;		/* Axis structure */
    int offset;			/* Offset of limit */
{
    VirtualAxis *axisPtr = (VirtualAxis *)(widgRec);
    unsigned int mask = (int)clientData;

    if ((string == NULL) || (*string == '\0')) {
	axisPtr->flags |= mask;
    } else {
	double value;

	if (Tcl_ExprDouble(interp, string, &value) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (mask == AXIS_AUTO_MIN) {
	    axisPtr->dataLimits.min = value;
	} else {
	    axisPtr->dataLimits.max = value;
	}
	axisPtr->flags &= ~mask;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * AxisLimitToString --
 *
 *	Convert the floating point axis limit into a string.
 *
 * Results:
 *	The string representation of the limit is returned.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
AxisLimitToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* Either LMIN or LMAX */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* */
    int offset;
    Tcl_FreeProc **freeProcPtr;
{
    VirtualAxis *axisPtr = (VirtualAxis *)(widgRec);
    unsigned int mask = (int)clientData;
    char *result;

    result = "";
    if (!(axisPtr->flags & mask)) {
	char string[TCL_DOUBLE_SPACE + 1];
	double value;

	value = (mask == AXIS_AUTO_MIN)
	    ? axisPtr->dataLimits.min : axisPtr->dataLimits.max;
	Tcl_PrintDouble(Blt_FindGraph(tkwin)->interp, value, string);
	result = strdup(string);
	if (result == NULL) {
	    return "";
	}
	*freeProcPtr = (Tcl_FreeProc *)free;
    }
    return result;
}

/*
 * ----------------------------------------------------------------------
 *
 * StringToTicks --
 *
 *
 * Results:
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToTicks(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* List of expressions representing axis
				 * values indicating where to display ticks */
    char *widgRec;		/* Axis structure */
    int offset;			/* not used */
{
    TickPositions **ticksPtrPtr = (TickPositions **)(widgRec + offset);
    TickPositions *ticksPtr;
    int numExprs;
    char **exprArr;
    register int i;
    double tick;

    if ((string == NULL) || (*string == '\0')) {
      noTicks:
	if (*ticksPtrPtr != NULL) {
	    free((char *)*ticksPtrPtr);
	}
	*ticksPtrPtr = NULL;
	return TCL_OK;
    }
    if (Tcl_SplitList(interp, string, &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numExprs == 0) {
	goto noTicks;
    }
    ticksPtr = (TickPositions *)
	malloc(sizeof(TickPositions) + (numExprs * sizeof(double)));
    assert(ticksPtr);
    for (i = 0; i < numExprs; i++) {
	if (Tcl_ExprDouble(interp, exprArr[i], &tick) != TCL_OK) {
	    free((char *)ticksPtr);
	    return TCL_ERROR;
	}
	ticksPtr->tickArr[i] = tick;
    }
    ticksPtr->numTicks = numExprs;
    if (*ticksPtrPtr != NULL) {
	free((char *)*ticksPtrPtr);
    }
    *ticksPtrPtr = ticksPtr;
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TicksToString --
 *
 *	Convert array of tick coordinates to a list.
 *
 * Results:
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
TicksToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* */
    int offset;
    Tcl_FreeProc **freeProcPtr;
{
    TickPositions **ticksPtrPtr = (TickPositions **)(widgRec + offset);
    char string[TCL_DOUBLE_SPACE + 1];
    register int i;
    char *result;
    Tcl_DString valueDStr;
    Graph *graphPtr;

    if (*ticksPtrPtr == NULL) {
	return "";
    }
    Tcl_DStringInit(&valueDStr);
    graphPtr = Blt_FindGraph(tkwin);
    for (i = 0; i < (*ticksPtrPtr)->numTicks; i++) {
	Tcl_PrintDouble(graphPtr->interp, (*ticksPtrPtr)->tickArr[i], string);
	Tcl_DStringAppendElement(&valueDStr, string);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    result = strdup(Tcl_DStringValue(&valueDStr));
    Tcl_DStringFree(&valueDStr);
    return result;
}

/*
 * ----------------------------------------------------------------------
 *
 * MakeLabel --
 *
 *	Converts a floating point tick value to a string to be used as its
 *	label.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Returns a new label in the string character buffer.  The formatted
 *	tick label will be displayed on the graph.
 *
 * ---------------------------------------------------------------------- */
static void
MakeLabel(graphPtr, axisPtr, value, string)
    Graph *graphPtr;
    VirtualAxis *axisPtr;	/* Axis structure */
    double value;		/* Value to be convert to a decimal string */
    char string[];		/* String (length is TICK_LABEL_SIZE+1)
				 * containing the formatted label */
{
    char *result;
    /*
     * Generate the default tick label based upon the tick value.
     */
    if (axisPtr->logScale) {
	sprintf(string, "1E%d", ROUND(value));
    } else {
	sprintf(string, "%.*g", NUMDIGITS, value);
    }

    if (axisPtr->formatCmd != NULL) {
	Tcl_Interp *interp = graphPtr->interp;
	Tk_Window tkwin = graphPtr->tkwin;

	/*
	 * A Tcl proc was designated to format tick labels. Append the path
	 * name of the widget and the default tick label as arguments when
	 * invoking it. Copy and save the new label from interp->result.
	 */

	Tcl_ResetResult(interp);
	if (Tcl_VarEval(interp, axisPtr->formatCmd, " ", Tk_PathName(tkwin),
		" ", string, (char *)NULL) != TCL_OK) {
	    Tk_BackgroundError(interp);
	    return;		/* Error in format proc */
	}
	/*
	 * The proc could return a string of any length, so limit its size to
	 * what will fit in the return string.
	 */
	result = Tcl_GetStringResult(interp);
	strncpy(string, result, TICK_LABEL_SIZE);
	string[TICK_LABEL_SIZE] = '\0';
	/* Make sure to clear the interpreter's result */
	Tcl_ResetResult(interp);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * InvHMap --
 *
 *	Maps the given window y-coordinate back to a graph coordinate
 *	value. Called by the graph locater routine.
 *
 * Results:
 *	Returns the graph coordinate value at the given window
 *	y-coordinate.
 *
 * ----------------------------------------------------------------------
 */
static double
InvHMap(graphPtr, axisPtr, x)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double x;
{
    double norm, value;

    norm = (double)(x - graphPtr->hOffset) / (double)(graphPtr->hRange);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    value = (norm * axisPtr->range) + axisPtr->limitsPtr->min;
    if (axisPtr->logScale) {
	value = EXP10(value);
    }
    return value;
}

/*
 * ----------------------------------------------------------------------
 *
 * InvVMap --
 *
 *	Maps the given window y-coordinate back to a graph coordinate
 *	value. Called by the graph locater routine.
 *
 * Results:
 *	Returns the graph coordinate value at the given window
 *	y-coordinate.
 *
 * ----------------------------------------------------------------------
 */
static double
InvVMap(graphPtr, axisPtr, y)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double y;
{
    double norm, value;

    norm = (double)(y - graphPtr->vOffset) / (double)graphPtr->vRange;
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    value = ((1.0 - norm) * axisPtr->range) + axisPtr->limitsPtr->min;
    if (axisPtr->logScale) {
	value = EXP10(value);
    }
    return value;
}

/*
 * ----------------------------------------------------------------------
 *
 * HMap --
 *
 *	Map the given graph coordinate value to its axis, returning a window
 *	position.
 *
 * Results:
 *	Returns a double precision number representing the window coordinate
 *	position on the given axis.
 *
 * ----------------------------------------------------------------------
 */
static double
HMap(graphPtr, axisPtr, x)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double x;
{
    register double norm;

    if (axisPtr->logScale) {
	if (x > 0.0) {
	    x = log10(x);
	} else if (x < 0.0) {
	    x = 0.0;
	}
    }
    norm = NORMALIZE(axisPtr, x);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    /* Horizontal transformation */
    return ((norm * (graphPtr->hRange)) + graphPtr->hOffset);
}

/*
 * ----------------------------------------------------------------------
 *
 * VMap --
 *
 *	Map the given graph coordinate value to its axis, returning a window
 *	position.
 *
 * Results:
 *	Returns a double precision number representing the window coordinate
 *	position on the given axis.
 *
 * ----------------------------------------------------------------------
 */
static double
VMap(graphPtr, axisPtr, y)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double y;
{
    register double norm;

    if (axisPtr->logScale) {
	if (y > 0.0) {
	    y = log10(y);
	} else if (y < 0.0) {
	    y = 0.0;
	}
    }
    norm = NORMALIZE(axisPtr, y);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    /* Vertical transformation */
    return (((1.0 - norm) * (graphPtr->vRange)) + graphPtr->vOffset);
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_Transform2DPt --
 *
 *	Maps the given graph x,y coordinate values to a window position.
 *
 * Results:
 *	Returns a XPoint structure containing the window coordinates of
 *	the given graph x,y coordinate.
 *
 * ----------------------------------------------------------------------
 */

Point2D
Blt_Transform2DPt(graphPtr, x, y, axesPtr)
    Graph *graphPtr;
    double x, y;		/* Graph x and y coordinates */
    Axis2D *axesPtr;		/* Specifies which axes to use */
{
    Point2D point;

    if (graphPtr->inverted) {
	point.x = HMap(graphPtr, axesPtr->y, y);
	point.y = VMap(graphPtr, axesPtr->x, x);
    } else {
	point.x = HMap(graphPtr, axesPtr->x, x);
	point.y = VMap(graphPtr, axesPtr->y, y);
    }
    return point;
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_InvTransform2DPt --
 *
 *	Maps the given window x,y coordinates to graph values.
 *
 * Results:
 *	Returns a structure containing the graph coordinates of
 *	the given window x,y coordinate.
 *
 * ----------------------------------------------------------------------
 */
Point2D
Blt_InvTransform2DPt(graphPtr, x, y, axesPtr)
    Graph *graphPtr;
    double x, y;		/* Window x and y coordinates */
    Axis2D *axesPtr;		/* Specifies which axes to use */
{
    Point2D point;

    if (graphPtr->inverted) {
	point.x = InvVMap(graphPtr, axesPtr->x, y);
	point.y = InvHMap(graphPtr, axesPtr->y, x);
    } else {
	point.x = InvHMap(graphPtr, axesPtr->x, x);
	point.y = InvVMap(graphPtr, axesPtr->y, y);
    }
    return point;
}


static void
UpdateElementAxes(elemPtr)
    Element *elemPtr;
{
    VirtualAxis *xAxisPtr, *yAxisPtr;
    Extents2D extents;

    (*elemPtr->infoPtr->extentsProc) (elemPtr, &extents);

    xAxisPtr = elemPtr->axes.x;
    yAxisPtr = elemPtr->axes.y;

    /* Set data limits (if not already set by user) */
    if ((xAxisPtr->flags & AXIS_AUTO_MIN) &&
	(xAxisPtr->dataLimits.min > extents.xMin)) {
	xAxisPtr->dataLimits.min = extents.xMin;
    }
    if ((xAxisPtr->flags & AXIS_AUTO_MAX) &&
	(xAxisPtr->dataLimits.max < extents.xMax)) {
	xAxisPtr->dataLimits.max = extents.xMax;
    }
    if ((yAxisPtr->flags & AXIS_AUTO_MIN) &&
	(yAxisPtr->dataLimits.min > extents.yMin)) {
	yAxisPtr->dataLimits.min = extents.yMin;
    }
    if ((yAxisPtr->flags & AXIS_AUTO_MAX) &&
	(yAxisPtr->dataLimits.max < extents.yMax)) {
	yAxisPtr->dataLimits.max = extents.yMax;
    }
    SetRange(xAxisPtr->dataLimits);
    SetRange(yAxisPtr->dataLimits);

    /* Mark axes in use */

    xAxisPtr->flags |= AXIS_MAPS_ELEM;
    yAxisPtr->flags |= AXIS_MAPS_ELEM;
}



static void
FixAxisLimits(axisPtr)
    VirtualAxis *axisPtr;
{
    /*
     * When auto-scaling, the axis limits are the bounds of the element
     * data.  If no data exists, set arbitrary limits (wrt to log/linear
     * scale).
     */
    if (axisPtr->dataLimits.min == bltPosInfinity) {
	axisPtr->dataLimits.min = (axisPtr->logScale) ? 0.001 : 0.0;
    }
    if (axisPtr->dataLimits.max == bltNegInfinity) {
	axisPtr->dataLimits.max = 1.0;
    }
    /* Auto range */
    if ((axisPtr->autoRange > 0.0) && (axisPtr->flags & AXIS_AUTO_MAX)
	&& (axisPtr->flags & AXIS_AUTO_MIN)) {
	double max;

#ifdef notdef
	fprintf(stderr, "before dataLimits.min=%g, max=%g\n",
	    axisPtr->dataLimits.min, axisPtr->dataLimits.max);
#endif
	if (axisPtr->shiftBy < 0.0) {
	    axisPtr->shiftBy = 0.0;
	}
	max = axisPtr->dataLimits.min + axisPtr->autoRange;
	if (axisPtr->dataLimits.max >= max) {
	    if (axisPtr->shiftBy > 0.0) {
		max = UCEIL(axisPtr->dataLimits.max, axisPtr->shiftBy);
	    }
	    axisPtr->dataLimits.min = max - axisPtr->autoRange;
	}
	axisPtr->dataLimits.max = max;
#ifdef notdef
	fprintf(stderr, "after dataLimits.min=%g, max=%g\n", axisPtr->dataLimits.min,
	    axisPtr->dataLimits.max);
#endif
    }
    /* If there's no range of data (min == max), manufacture one */

    if (axisPtr->dataLimits.min >= axisPtr->dataLimits.max) {
	double value;

	value = axisPtr->dataLimits.min;
	if (value == 0.0) {
	    axisPtr->dataLimits.min = -0.1, axisPtr->dataLimits.max = 0.1;
	} else {
	    double x;

	    x = Fabs(value * 0.1);
	    axisPtr->dataLimits.min = value - x;
	    axisPtr->dataLimits.max = value + x;
	}
    }
    if ((axisPtr->dataLimits.max != axisPtr->prevMax) ||
	(axisPtr->dataLimits.min != axisPtr->prevMin)) {
	/* Indicate if the axis limits have changed */
	axisPtr->flags |= AXIS_CONFIG_DIRTY;
	/* and save the previous minimum and maximum values */
	axisPtr->prevMin = axisPtr->dataLimits.min;
	axisPtr->prevMax = axisPtr->dataLimits.max;
    }
    SetRange(axisPtr->dataLimits);
}

/*
 * ----------------------------------------------------------------------
 *
 * NiceNum --
 *
 * 	Taken from Paul Heckbert's "Nice Numbers for Graph Labels" in
 * 	Graphics Gems (pp 61-63).  Finds a "nice" number approximately
 * 	equal to x.
 *
 * ----------------------------------------------------------------------
 */
static double
NiceNum(x, round)
    double x;
    int round;			/* If non-zero, round. Otherwise take ceiling
				 * of value. */
{
    double exponX;		/* exponent of x */
    double fractX;		/* fractional part of x */
    double nf;			/* nice, rounded fraction */

    exponX = floor(log10(x));
    fractX = x / EXP10(exponX);	/* between 1 and 10 */
    if (round) {
	if (fractX < 1.5) {
	    nf = 1.0;
	} else if (fractX < 3.0) {
	    nf = 2.0;
	} else if (fractX < 7.0) {
	    nf = 5.0;
	} else {
	    nf = 10.0;
	}
    } else if (fractX <= 1.0) {
	nf = 1.0;
    } else if (fractX <= 2.0) {
	nf = 2.0;
    } else if (fractX <= 5.0) {
	nf = 5.0;
    } else {
	nf = 10.0;
    }
    return nf * EXP10(exponX);
}


static TickPositions *
GenerateTicks(sweepPtr)
    TickSweep *sweepPtr;
{
    TickPositions *ticksPtr;
    register int i;
    double value;

    static float logTable[] =	/* Precomputed log10 values [1..10] */
    {
	(float)0.0, (float)0.301,
	(float)0.477, (float)0.602,
	(float)0.699, (float)0.778,
	(float)0.845, (float)0.903,
	(float)0.954, (float)1.0
    };

    ticksPtr = (TickPositions *)
	malloc(sizeof(TickPositions) + (sweepPtr->numSteps * sizeof(double)));
    assert(ticksPtr);
    value = sweepPtr->initial;	/* Start from smallest axis tick */

    if (sweepPtr->step == 0.0) {/* Use log values */
	for (i = 0; i < sweepPtr->numSteps; i++) {
	    ticksPtr->tickArr[i] = (double)logTable[i];
	}
    } else {
	for (i = 0; i < sweepPtr->numSteps; i++) {
	    ticksPtr->tickArr[i] = Round(value / sweepPtr->step) * sweepPtr->step;
	    value = ticksPtr->tickArr[i] + sweepPtr->step;
	}
    }
    ticksPtr->numTicks = sweepPtr->numSteps;
    return ticksPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * LogAxis --
 *
 * 	Determine the range and units of a log scaled axis.
 *
 * 	Unless the axis limits are specified, the axis is scaled
 * 	automatically, where the smallest and largest major ticks encompass
 * 	the range of actual data values.  When an axis limit is specified,
 * 	that value represents the smallest(min)/largest(max) value in the
 * 	displayed range of values.
 *
 * 	Both manual and automatic scaling are affected by the step used.  By
 * 	default, the step is the largest power of ten to divide the range in
 * 	more than one piece.
 *
 *	Automatic scaling:
 *	Find the smallest number of units which contain the range of values.
 *	The minimum and maximum major tick values will be represent the
 *	range of values for the axis. This greatest number of major ticks
 *	possible is 10.
 *
 * 	Manual scaling:
 *   	Make the minimum and maximum data values the represent the range of
 *   	the values for the axis.  The minimum and maximum major ticks will be
 *   	inclusive of this range.  This provides the largest area for plotting
 *   	and the expected results when the axis min and max values have be set
 *   	by the user (.e.g zooming).  The maximum number of major ticks is 20.
 *
 *   	For log scale, there is always the possibility that the minimum and
 *   	maximum data values are the same magnitude.  To represent the points
 *   	properly, at least one full decade should be shown.  However, if you
 *   	zoom a log scale plot, the results should be predictable. Therefore,
 *   	in that case, show only minor ticks.  Lastly, there should be an
 *   	appropriate way to handle numbers <=0.
 *
 *          maxY
 *            |    units = magnitude (of least significant digit)
 *            |    high  = largest unit tick < max axis value
 *      high _|    low   = smallest unit tick > min axis value
 *            |
 *            |    range = high - low
 *            |    # ticks = greatest factor of range/units
 *           _|
 *        U   |
 *        n   |
 *        i   |
 *        t  _|
 *            |
 *            |
 *            |
 *       low _|
 *            |
 *            |_minX________________maxX__
 *            |   |       |      |       |
 *     minY  low                        high
 *           minY
 *
 *
 * 	numTicks = Number of ticks
 * 	min = Minimum value of axis
 * 	max = Maximum value of axis
 * 	range    = Range of values (max - min)
 *
 * 	If the number of decades is greater than ten, it is assumed
 *	that the full set of log-style ticks can't be drawn properly.
 *
 * Results:
 *	None
 *
 * ----------------------------------------------------------------------
 */
static void
LogAxis(axisPtr)
    VirtualAxis *axisPtr;
{
    double range;
    double min, max;
    double majorStep, minorStep;
    int numMajor, numMinor;


    min = axisPtr->dataLimits.min, max = axisPtr->dataLimits.max;

    if (min > 0.0) {
	min = floor(log10(min));
    } else {
	min = 0.0;
    }
    if (max > 0.0) {
	max = ceil(log10(max));
    } else {
	max = 1.0;
    }

    range = max - min;

    if (range > 10) {

	/*
	 * There are too many decades to display every major tick.
	 * Instead, treat the axis as a linear scale.
	 */

	range = NiceNum(range, 0);
	majorStep = NiceNum(range / DEF_NUM_TICKS, 1);

	/* Find the outer limits in terms of the step. */
	min = UFLOOR(min, majorStep);
	max = UCEIL(max, majorStep);
	numMajor = (int)((max - min) / majorStep) + 1;
	minorStep = EXP10(floor(log10(majorStep)));
	if (minorStep == majorStep) {
	    numMinor = 4;
	    minorStep = 0.2;
	} else {
	    numMinor = Round(majorStep / minorStep) - 1;
	}
    } else {
	if (min == max) {
	    max++;
	}
	majorStep = 1.0;

	/* FIXME: CHECK THIS. */
	numMajor = (int)((max - min) + 1);
	minorStep = 0.0;
	numMinor = 10;
    }

    axisPtr->range = (max - min);
    SetLimits(axisPtr->tickLimits, min, max);
    axisPtr->limitsPtr = &(axisPtr->tickLimits);

    axisPtr->majorTicks.step = majorStep;

    axisPtr->majorTicks.initial = min;
    axisPtr->majorTicks.numSteps = numMajor;
    axisPtr->minorTicks.initial = minorStep;
    axisPtr->minorTicks.step = minorStep;
    axisPtr->minorTicks.numSteps = numMinor;

}

/*
 * ----------------------------------------------------------------------
 *
 * LinearAxis --
 *
 * 	Determine the units of a linear scaled axis.
 *
 * 	Unless the axis limits are specified, the axis is scaled
 * 	automatically, where the smallest and largest major ticks
 * 	encompass the range of actual data values.  When an axis
 * 	limit is specified, that value represents the
 * 	smallest(min)/largest(max) value in the displayed range of
 * 	values.
 *
 * 	Both manual and automatic scaling are affected by the
 * 	step used.  By default, the step is the largest
 * 	power of ten to divide the range in more than one piece.
 *
 * 	Automatic scaling:
 *   	Find the smallest number of units which contain the range of
 *   	values.  The minimum and maximum major tick values will be
 *   	represent the range of values for the axis. This greatest
 *   	number of major ticks possible is 10.
 *
 * 	Manual scaling:
 *   	Make the minimum and maximum data values the represent the
 *   	range of the values for the axis.  The minimum and maximum
 *   	major ticks will be inclusive of this range.  This provides
 *   	the largest area for plotting and the expected results when
 *   	the axis min and max values have be set by the user (.e.g zooming).
 *   	The maximum number of major ticks is 20.
 *
 *   	For log scale, there is always the possibility that the minimum
 *   	and maximum data values are the same magnitude.  To represent
 *   	the points properly, at least one full decade should be shown.
 *   	However, if you zoom a log scale plot, the results should be
 *   	predictable. Therefore, in that case, show only minor ticks.
 *   	Lastly, there should be an appropriate way to handle numbers <=0.
 *
 *          maxY
 *            |    units = magnitude (of least significant digit)
 *            |    high  = largest unit tick < max axis value
 *      high _|    low   = smallest unit tick > min axis value
 *            |
 *            |    range = high - low
 *            |    # ticks = greatest factor of range/units
 *           _|
 *        U   |
 *        n   |
 *        i   |
 *        t  _|
 *            |
 *            |
 *            |
 *       low _|
 *            |
 *            |_minX________________maxX__
 *            |   |       |      |       |
 *     minY  low                        high
 *           minY
 *
 *
 * 	numTicks = Number of ticks
 * 	min = Minimum value of axis
 * 	max = Maximum value of axis
 * 	range    = Range of values (max - min)
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
LinearAxis(axisPtr)
    VirtualAxis *axisPtr;
{
    double range;
    double min, max;
    double majorStep, minorStep;
    int numMajor, numMinor;

    min = axisPtr->dataLimits.min;
    max = axisPtr->dataLimits.max;

    range = max - min;

    /*
     * Calculate the major step.  If a step interval was designated, use it,
     * only if it fits within the current range of the axis.
     */
    if ((axisPtr->reqStep > 0.0) &&
	((int)(range / axisPtr->reqStep) <= MAXTICKS)) {
	majorStep = axisPtr->reqStep;
    } else {
	range = NiceNum(range, 0);
	majorStep = NiceNum(range / DEF_NUM_TICKS, 1);
    }

    /*
     * Find the outer tick values in terms of the major step interval.  Add
     * +0.0 to preclude the possibility of an IEEE -0.0.
     */
    axisPtr->tickLimits.min = UFLOOR(min, majorStep) + 0.0;
    axisPtr->tickLimits.max = UCEIL(max, majorStep) + 0.0;
    SetRange(axisPtr->tickLimits);
    numMajor = Round(axisPtr->tickLimits.range / majorStep) + 1;

    /*
     * If the axis is "loose", the range is between the two outermost ticks.
     * Otherwise if it's "tight", the range is between the data min and max.
     */
    axisPtr->limitsPtr =
	(axisPtr->loose) ? &(axisPtr->tickLimits) : &(axisPtr->dataLimits);

    axisPtr->range = axisPtr->limitsPtr->max - axisPtr->limitsPtr->min;
    axisPtr->majorTicks.step = majorStep;
    axisPtr->majorTicks.initial = axisPtr->tickLimits.min;
    axisPtr->majorTicks.numSteps = numMajor;

    /* Now calculate the minor tick step and number. */

    if ((axisPtr->reqNumMinorTicks > 0) && (axisPtr->reqMajorPtr == NULL)) {
	numMinor = axisPtr->reqNumMinorTicks - 1;
	minorStep = 1.0 / (numMinor + 1);
    } else {
	minorStep = 0.2;
	numMinor = 0;		/* (1.0 / minorStep) - 1; */
    }

    axisPtr->minorTicks.initial = minorStep;
    axisPtr->minorTicks.step = minorStep;
    axisPtr->minorTicks.numSteps = numMinor;
}


static void
SweepTicks(axisPtr)
    Axis *axisPtr;
{
    axisPtr->majorPtr = axisPtr->virtAxisPtr->reqMajorPtr;
    axisPtr->minorPtr = axisPtr->virtAxisPtr->reqMinorPtr;
    if (axisPtr->majorPtr == NULL) {
	if (axisPtr->genMajorPtr != NULL) {
	    free((char *)axisPtr->genMajorPtr);
	}
	axisPtr->genMajorPtr = GenerateTicks(&(axisPtr->virtAxisPtr->majorTicks));
	axisPtr->majorPtr = axisPtr->genMajorPtr;
    }
    if (axisPtr->minorPtr == NULL) {
	if (axisPtr->genMinorPtr != NULL) {
	    free((char *)axisPtr->genMinorPtr);
	}
	axisPtr->genMinorPtr = GenerateTicks(&(axisPtr->virtAxisPtr->minorTicks));
	axisPtr->minorPtr = axisPtr->genMinorPtr;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_ResetAxes --
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
void
Blt_ResetAxes(graphPtr)
    Graph *graphPtr;
{
    Blt_ListItem item;
    Element *elemPtr;
    VirtualAxis *axisPtr;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;

    /* FIXME: This should be called whenever the display list of
     *	      elements change. Maybe yet another flag INIT_STACKS to
     *	      indicate that the element display list has changed.
     *	      Needs to be done before the axis limits are set.
     */
    Blt_InitFreqTable(graphPtr);
    if ((graphPtr->mode == MODE_STACKED) && (graphPtr->numStacks > 0)) {
	Blt_ComputeStacks(graphPtr);
    }
    /*
     * Step 1:  Reset the flags indicating axes are mapped to elements.
     *		Initialize axis limits that are to be autoscaled (user
     *		did not specify a -min or -max).
     */

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);
	if (axisPtr->flags & AXIS_AUTO_MIN) {
	    axisPtr->dataLimits.min = bltPosInfinity;
	}
	if (axisPtr->flags & AXIS_AUTO_MAX) {
	    axisPtr->dataLimits.max = bltNegInfinity;
	}
	axisPtr->flags &= ~AXIS_MAPS_ELEM;
    }

    /*
     * Step 2:  For each element displayed, update the min and max values
     *		in its respective x and y axes.  The is for autoscaling
     *		the axes according to the data points mapped to them.
     */

    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (!elemPtr->hidden) {
	    UpdateElementAxes(elemPtr);
	}
    }

    /*
     * Step 3:  For each axis, set axis limits and get sweep values for
     *		ticks.
     */

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);

	FixAxisLimits(axisPtr);

	/* Calculate min/max tick (major/minor) layouts */
	if (axisPtr->logScale) {
	    LogAxis(axisPtr);
	} else {
	    LinearAxis(axisPtr);
	}
	axisPtr->flags |= AXIS_CONFIG_DIRTY;
    }
    graphPtr->flags &= ~RESET_AXES;
    graphPtr->flags |= (GET_AXIS_GEOMETRY | LAYOUT_NEEDED);
    /*
     * When any axis changes, we need to layout the entire graph.
     */
    graphPtr->flags |= (COORDS_WORLD | REDRAW_WORLD);
}

/*
 * ----------------------------------------------------------------------
 *
 * ResetAttributes --
 *
 *	Configures axis attributes (font, line width, label, etc) and
 *	allocates a new (possibly shared) graphics context.  Line cap
 *	style is projecting.  This is for the problem of when a tick
 *	sits directly at the end point of the axis.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Axis resources are allocated (GC, font). Axis layout is
 *	deferred until the height and width of the window are known.
 *
 * ----------------------------------------------------------------------
 */
static void
ResetAttributes(graphPtr, axisPtr)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    Blt_ResetTextAttributes(graphPtr->tkwin, &(axisPtr->titleAttr));
    Blt_ResetTextAttributes(graphPtr->tkwin, &(axisPtr->tickAttr));
    Blt_ResetTextAttributes(graphPtr->tkwin, &(axisPtr->limitAttr));

    gcMask = (GCForeground | GCLineWidth | GCCapStyle);
    gcValues.foreground = axisPtr->tickAttr.color->pixel;
    gcValues.line_width = LineWidth(axisPtr->lineWidth);
    gcValues.cap_style = CapProjecting;

    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (axisPtr->tickGC != NULL) {
	Tk_FreeGC(graphPtr->display, axisPtr->tickGC);
    }
    axisPtr->tickGC = newGC;
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyVirtualAxis --
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources (font, color, gc, labels, etc.) associated with the
 *	axis are deallocated.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyVirtualAxis(graphPtr, axisPtr)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
{
    int flags;

    flags = GraphType(graphPtr);
    Tk_FreeOptions(configSpecs, (char *)axisPtr, graphPtr->display, flags);

    if (axisPtr->name != NULL) {
	free(axisPtr->name);
    }
    if (axisPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(axisPtr->hashPtr);
    }
    Blt_FreeTextAttributes(graphPtr->display, &(axisPtr->titleAttr));
    Blt_FreeTextAttributes(graphPtr->display, &(axisPtr->limitAttr));
    Blt_FreeTextAttributes(graphPtr->display, &(axisPtr->tickAttr));

    if (axisPtr->tickGC != NULL) {
	Tk_FreeGC(graphPtr->display, axisPtr->tickGC);
    }
    if (axisPtr->reqMajorPtr != NULL) {
	free((char *)axisPtr->reqMajorPtr);
    }
    if (axisPtr->reqMinorPtr != NULL) {
	free((char *)axisPtr->reqMinorPtr);
    }
    if (axisPtr->limitFormats != NULL) {
	free((char *)axisPtr->limitFormats);
    }
    free((char *)axisPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyAxis --
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources (font, color, gc, labels, etc.) associated with the
 *	axis are deallocated.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
DestroyAxis(graphPtr, axisPtr)
    Graph *graphPtr;		/* Unused */
    Axis *axisPtr;
{
    Blt_ListReset(&(axisPtr->labelList));
    if (axisPtr->segArr != NULL) {
	free((char *)axisPtr->segArr);
    }
    if (axisPtr->genMajorPtr != NULL) {
	free((char *)axisPtr->genMajorPtr);
    }
    if (axisPtr->genMinorPtr != NULL) {
	free((char *)axisPtr->genMinorPtr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CalculateOffsets --
 *
 *	Determines the sites of the axis, major and minor ticks,
 *	and title of the axis.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
CalculateOffsets(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;
    int pad;			/* Offset of axis from interior region. This
				 * includes a possible border and the axis
				 * line width. */
    int innerPos, outerPos;
    int majorOffset, minorOffset, labelOffset;

    majorOffset = ABS(virtAxisPtr->tickLength);
    minorOffset = majorOffset / 2;
    labelOffset = Round(majorOffset * 1.4) + virtAxisPtr->lineWidth / 2;

    /* Adjust offset for the interior border width and the line width */
    pad = graphPtr->plotBW + virtAxisPtr->lineWidth + 2;
    if (graphPtr->plotBW > 0) {
	pad++;
    }
    if ((virtAxisPtr->site == AXIS_LEFT) || (virtAxisPtr->site == AXIS_TOP)) {
	majorOffset = -majorOffset;
	minorOffset = -minorOffset;
	labelOffset = -labelOffset;
    }
    /*
     * Pre-calculate the x-coordinate positions of the axis, tick labels, and
     * the individual major and minor ticks.
     */

    innerPos = 0;		/* Suppress compiler warning */
    switch (virtAxisPtr->site) {
    case AXIS_BOTTOM:
	innerPos = graphPtr->yMax + pad;
	virtAxisPtr->titleX = (graphPtr->xMax + graphPtr->xMin) / 2;
	virtAxisPtr->titleY = graphPtr->yMax + (axisPtr->height + TITLE_PAD);
	virtAxisPtr->titleAttr.anchor = virtAxisPtr->tickAttr.anchor = TK_ANCHOR_N;
	break;

    case AXIS_LEFT:
	innerPos = graphPtr->xMin - pad;
	virtAxisPtr->titleX = graphPtr->xMin - (axisPtr->width + TITLE_PAD);
	virtAxisPtr->titleY = (graphPtr->yMax + graphPtr->yMin) / 2;
	virtAxisPtr->titleAttr.anchor = virtAxisPtr->tickAttr.anchor = TK_ANCHOR_E;
	break;

    case AXIS_TOP:
	innerPos = graphPtr->yMin - pad;
	virtAxisPtr->titleX = (graphPtr->xMax + graphPtr->xMin) / 2;
	virtAxisPtr->titleY = graphPtr->yMin - (axisPtr->height + TITLE_PAD);
	virtAxisPtr->titleAttr.anchor = virtAxisPtr->tickAttr.anchor = TK_ANCHOR_S;
	break;

    case AXIS_RIGHT:
	innerPos = graphPtr->xMax + pad;
	virtAxisPtr->titleX = graphPtr->xMax + (axisPtr->width + TITLE_PAD);
	virtAxisPtr->titleY = (graphPtr->yMax + graphPtr->yMin) / 2;
	virtAxisPtr->titleAttr.anchor = virtAxisPtr->tickAttr.anchor = TK_ANCHOR_W;
	break;

    case AXIS_VIRTUAL:
	break;
    }

    outerPos = innerPos + majorOffset;
    axisPtr->posArr[MAJOR_TICK] = outerPos;
    axisPtr->posArr[AXIS_LINE] = innerPos - (virtAxisPtr->lineWidth / 2);
    axisPtr->posArr[MINOR_TICK] = innerPos + minorOffset;
    axisPtr->posArr[TICK_LABEL] = innerPos + labelOffset;
    if (virtAxisPtr->tickLength < 0) {
	int hold;

	hold = axisPtr->posArr[MAJOR_TICK];
	axisPtr->posArr[MAJOR_TICK] = axisPtr->posArr[AXIS_LINE];
	axisPtr->posArr[AXIS_LINE] = hold;
    }
}

static XSegment
AxisLine(graphPtr, axisPtr, limitsPtr)
    Graph *graphPtr;
    Axis *axisPtr;		/* Axis information */
    Limits *limitsPtr;		/* Limits of axis in graph coordinates */
{
    XSegment segment;
    double min, max;

    max = NORMALIZE(axisPtr->virtAxisPtr, limitsPtr->max);
    min = NORMALIZE(axisPtr->virtAxisPtr, limitsPtr->min);
    if (axisPtr->virtAxisPtr->descending) {
	max = 1.0 - max;
	min = 1.0 - min;
    }
    if (HORIZAXIS(axisPtr->virtAxisPtr)) {
	segment.y1 = segment.y2 = axisPtr->posArr[AXIS_LINE];
	segment.x1 = MAPH(graphPtr, min);
	segment.x2 = MAPH(graphPtr, max);
    } else {
	segment.x1 = segment.x2 = axisPtr->posArr[AXIS_LINE];
	segment.y1 = MAPV(graphPtr, min);
	segment.y2 = MAPV(graphPtr, max);
    }
    return segment;
}


static XSegment
Tick(graphPtr, axisPtr, value, whichTick)
    Graph *graphPtr;
    Axis *axisPtr;
    double value;
    AxisComponent whichTick;	/* MAJOR_TICK or MINOR_TICK */
{
    double norm;
    XSegment segment;
    int tick;

    norm = NORMALIZE(axisPtr->virtAxisPtr, value);
    if (axisPtr->virtAxisPtr->descending) {
	norm = 1.0 - norm;
    }
    tick = axisPtr->posArr[(int)whichTick];
    if (HORIZAXIS(axisPtr->virtAxisPtr)) {
	segment.y1 = axisPtr->posArr[AXIS_LINE];
	segment.y2 = tick;
	segment.x1 = segment.x2 = MAPH(graphPtr, norm);
    } else {
	segment.x1 = axisPtr->posArr[AXIS_LINE];
	segment.x2 = tick;
	segment.y1 = segment.y2 = MAPV(graphPtr, norm);
    }
    return segment;
}

/*
 * -----------------------------------------------------------------
 *
 * TransformAxis --
 *
 *	Pre-calculates positions of the axis, ticks, and labels (to be
 *	used later when displaying the axis).  Calculates the values
 *	for each major and minor tick and checks to see if they are in
 *	range (the outer ticks may be outside of the range of plotted
 *	values).
 *
 *	Line segments for the minor and major ticks are saved into one
 *	XSegment array so that they can be drawn by a single
 *	XDrawSegments call. The positions of the tick labels are also
 *	computed and saved.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Line segments and tick labels are saved and used later to draw
 *	the axis.  
 *
 * ----------------------------------------------------------------- 
 */
void
Blt_TransformAxis(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    int arraySize;
    Limits *limitsPtr;
    register int sgmts, labels;
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;
    Blt_ListItem item;

    CalculateOffsets(graphPtr, axisPtr);

    /* Save all line coordinates in an array of line segments. */

    if (axisPtr->segArr != NULL) {
	free((char *)axisPtr->segArr);
    }
    arraySize = 1 + (axisPtr->majorPtr->numTicks *
	(axisPtr->minorPtr->numTicks + 1));
    axisPtr->segArr = (XSegment *)malloc(arraySize * sizeof(XSegment));
    assert(axisPtr->segArr);

    if ((virtAxisPtr->logScale) || (virtAxisPtr->loose) ||
	(virtAxisPtr->dataLimits.min == virtAxisPtr->dataLimits.max)) {
	limitsPtr = &(virtAxisPtr->tickLimits);
    } else {
	limitsPtr = &(virtAxisPtr->dataLimits);
    }

    /* Axis baseline */
    axisPtr->segArr[0] = AxisLine(graphPtr, axisPtr, limitsPtr);

    sgmts = 1, labels = 0;
    item = Blt_ListFirstItem(&(axisPtr->labelList));
    if (virtAxisPtr->showTicks) {
	double tick, subTick;
	short int labelPos;
	register int i, j;
	int position;
	TickLabel *labelPtr = (TickLabel *)&position;

	for (i = 0; i < axisPtr->majorPtr->numTicks; i++) {
	    tick = axisPtr->majorPtr->tickArr[i];

	    /* Minor ticks */
	    for (j = 0; j < axisPtr->minorPtr->numTicks; j++) {
		subTick = tick + (virtAxisPtr->majorTicks.step *
		    axisPtr->minorPtr->tickArr[j]);
		if (OutOfRange(subTick, limitsPtr)) {
		    continue;
		}
		axisPtr->segArr[sgmts] =
		    Tick(graphPtr, axisPtr, subTick, MINOR_TICK);
		sgmts++;
	    }
	    if (OutOfRange(tick, limitsPtr)) {
		continue;
	    }

	    /* Major tick and label position */
	    axisPtr->segArr[sgmts] = Tick(graphPtr, axisPtr, tick, MAJOR_TICK);
	    labelPos = (short int)axisPtr->posArr[TICK_LABEL];

	    /* Save tick label X-Y position. */
	    if (HORIZAXIS(axisPtr->virtAxisPtr)) {
		labelPtr->x = axisPtr->segArr[sgmts].x1;
		labelPtr->y = labelPos;
	    } else {
		labelPtr->x = labelPos;
		labelPtr->y = axisPtr->segArr[sgmts].y1;
	    }
	    Blt_ListSetValue(item, (ClientData)position);
	    item = Blt_ListNextItem(item);
	    sgmts++, labels++;
	}
    }
    assert(sgmts <= arraySize);
    axisPtr->numSegments = sgmts;
}

/*
 * -----------------------------------------------------------------
 *
 * DrawAxis --
 *
 *	Draws the axis, ticks, and labels onto the canvas.
 *
 *	Initializes and passes text attribute information through
 *	TextAttributes structure.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Axis gets drawn on window.
 *
 * -----------------------------------------------------------------
 */

static float titleRot[4] =	/* Rotation for each axis title */
{
    0.0, 90.0, 0.0, 270.0
};

static void
DrawAxis(graphPtr, drawable, axisPtr)
    Graph *graphPtr;
    Drawable drawable;
    Axis *axisPtr;
{
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;

    if (virtAxisPtr->titleText != NULL) {
	virtAxisPtr->titleAttr.theta = (double)titleRot[virtAxisPtr->site];
	Blt_DrawText(graphPtr->tkwin, drawable, virtAxisPtr->titleText,
	    &(virtAxisPtr->titleAttr),
	    virtAxisPtr->titleX, virtAxisPtr->titleY);
    }
    if (virtAxisPtr->showTicks) {
	register Blt_ListItem item;
	int position;
	TickLabel *labelPtr = (TickLabel *)&position;

	/* Draw major tick labels */
	for (item = Blt_ListFirstItem(&(axisPtr->labelList)); item != NULL; 
	     item = Blt_ListNextItem(item)) {
	    position = (int)Blt_ListGetValue(item);
	    Blt_DrawText(graphPtr->tkwin, drawable, Blt_ListGetKey(item),
		&(virtAxisPtr->tickAttr), labelPtr->x, labelPtr->y);
	}
    }
    if (axisPtr->numSegments > 0) {
	/* Draw the tick marks */
	XDrawSegments(graphPtr->display, drawable, virtAxisPtr->tickGC,
	    axisPtr->segArr, axisPtr->numSegments);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintAxis --
 *
 *	Generates PostScript output to draw the axis, ticks, and
 *	labels.
 *
 *	Initializes and passes text attribute information through
 *	TextAttributes structure.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	PostScript output is left in graphPtr->interp->result;
 *
 * -----------------------------------------------------------------
 */
/* ARGSUSED */
static void
PrintAxis(printable, axisPtr)
    Printable printable;
    Axis *axisPtr;
{
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;

    if (virtAxisPtr->titleText != NULL) {
	virtAxisPtr->titleAttr.theta = (double)titleRot[virtAxisPtr->site];
	Blt_PrintText(printable, virtAxisPtr->titleText,
	    &(virtAxisPtr->titleAttr),
	    virtAxisPtr->titleX, virtAxisPtr->titleY);
    }
    if (virtAxisPtr->showTicks) {
	register Blt_ListItem item;
	int position;
	TickLabel *labelPtr = (TickLabel *)&position;

	for (item = Blt_ListFirstItem(&(axisPtr->labelList)); item != NULL; 
	     item = Blt_ListNextItem(item)) {
	    position = (int)Blt_ListGetValue(item);
	    Blt_PrintText(printable, Blt_ListGetKey(item),
		&(virtAxisPtr->tickAttr), labelPtr->x, labelPtr->y);
	}
    }
    if (axisPtr->numSegments > 0) {
	Blt_LineAttributesToPostScript(printable, virtAxisPtr->tickAttr.color,
	    virtAxisPtr->lineWidth, (Dashes *)NULL, CapButt, JoinMiter);
	Blt_SegmentsToPostScript(printable, axisPtr->segArr,
	    axisPtr->numSegments);
    }
}

static XSegment
GridLine(graphPtr, axisPtr, value)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double value;
{
    XSegment segment;
    double norm;

    norm = NORMALIZE(axisPtr, value);
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    /* Grid lines run orthogonally to the axis */
    if (HORIZAXIS(axisPtr)) {
	segment.y1 = graphPtr->yMin;
	segment.y2 = graphPtr->yMax;
	segment.x1 = segment.x2 = MAPH(graphPtr, norm);
    } else {
	segment.x1 = graphPtr->xMin;
	segment.x2 = graphPtr->xMax;
	segment.y1 = segment.y2 = MAPV(graphPtr, norm);
    }
    return segment;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetAxisSegments --
 *
 *	Assembles the grid lines associated with an axis. Generates
 *	tick positions if necessary (this happens when the axis is
 *	not a logical axis too).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
int
Blt_GetAxisSegments(graphPtr, virtAxisPtr, segPtrPtr)
    Graph *graphPtr;
    VirtualAxis *virtAxisPtr;
    XSegment **segPtrPtr;
{
    register int count;
    int needed;
    TickPositions *majorPosPtr, *minorPosPtr;
    Grid *gridPtr = graphPtr->gridPtr;
    register int i;
    Limits *limitsPtr;
    double value;
    XSegment *segArr;

    count = needed = 0;

    if (virtAxisPtr == NULL) {
	return 0;
    }
    if (virtAxisPtr->reqMajorPtr == NULL) {
	majorPosPtr = GenerateTicks(&(virtAxisPtr->majorTicks));
    } else {
	majorPosPtr = virtAxisPtr->reqMajorPtr;
    }
    needed += majorPosPtr->numTicks;

    if (virtAxisPtr->reqMinorPtr == NULL) {
	minorPosPtr = GenerateTicks(&(virtAxisPtr->minorTicks));
    } else {
	minorPosPtr = virtAxisPtr->reqMinorPtr;
    }
    if (gridPtr->minorGrid) {
	needed += (majorPosPtr->numTicks * minorPosPtr->numTicks);
    }
    if (needed == 0) {
	return 0;
    }
    segArr = (XSegment *)malloc(sizeof(XSegment) * needed);
    assert(segArr);

    if ((virtAxisPtr->logScale) || (virtAxisPtr->loose) ||
	(virtAxisPtr->dataLimits.min == virtAxisPtr->dataLimits.max)) {
	limitsPtr = &(virtAxisPtr->tickLimits);
    } else {
	limitsPtr = &(virtAxisPtr->dataLimits);
    }
    for (i = 0; i < majorPosPtr->numTicks; i++) {
	value = majorPosPtr->tickArr[i];
	if (gridPtr->minorGrid) {
	    register int j;
	    double subValue;

	    for (j = 0; j < minorPosPtr->numTicks; j++) {
		subValue = value +
		    (virtAxisPtr->majorTicks.step * minorPosPtr->tickArr[j]);
		if (OutOfRange(subValue, limitsPtr)) {
		    continue;
		}
		segArr[count] = GridLine(graphPtr, virtAxisPtr, subValue);
		count++;
	    }
	}
	if (OutOfRange(value, limitsPtr)) {
	    continue;
	}
	segArr[count] = GridLine(graphPtr, virtAxisPtr, value);
	count++;
    }
    if (majorPosPtr != virtAxisPtr->reqMajorPtr) {
	free((char *)majorPosPtr);
    }
    if (minorPosPtr != virtAxisPtr->reqMinorPtr) {
	free((char *)minorPosPtr);
    }
    assert(count <= needed);
    *segPtrPtr = segArr;
    return count;
}

/*
 *----------------------------------------------------------------------
 *
 * GetAxisGeometry --
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
GetAxisGeometry(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    register int i, numLabels;
    char label[TICK_LABEL_SIZE + 1];
    int width, height;
    int maxWidth, maxHeight;
    double value;
    Limits *limitsPtr;
    int pad;
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;

    /* Release any previously generated ticks labels */
    Blt_ListReset(&(axisPtr->labelList));
    axisPtr->width = axisPtr->height = 0;
    SweepTicks(axisPtr);

    /*
     * First handle the two special cases where the axis is either hidden
     * (-hide yes) or no ticks are being shown (-showticks no).
     */
    if (virtAxisPtr->hidden) {
	return;			/* Axis is hidden, leave dimensions at zero */
    }
    if (!virtAxisPtr->showTicks) {
	if (HORIZAXIS(virtAxisPtr)) {
	    axisPtr->height = virtAxisPtr->lineWidth + TITLE_PAD;
	} else {
	    axisPtr->width = virtAxisPtr->lineWidth + TITLE_PAD;
	}
	return;			/* Leave room for axis baseline (and pad) */
    }

    /*
     * Set the limits of the axis, considering how the axis is the be
     * styled (loose or tight).  We'll use this information to generate
     * ticks (and subticks) within this range.
     */
    if ((virtAxisPtr->logScale) || (virtAxisPtr->loose) ||
	(virtAxisPtr->dataLimits.min == virtAxisPtr->dataLimits.max)) {
	limitsPtr = &(virtAxisPtr->tickLimits);
    } else {
	limitsPtr = &(virtAxisPtr->dataLimits);
    }

    /* Hey Sani, does this check fail under AIX? */
    assert((axisPtr->majorPtr->numTicks >= 0) && 
	   (axisPtr->majorPtr->numTicks <= MAXTICKS));

    maxHeight = maxWidth = 0;
    numLabels = 0;
    for (i = 0; i < axisPtr->majorPtr->numTicks; i++) {
	value = axisPtr->majorPtr->tickArr[i];
	if (OutOfRange(value, limitsPtr)) {
	    continue;
	}
	MakeLabel(graphPtr, axisPtr->virtAxisPtr, value, label);
	Blt_ListAppend(&(axisPtr->labelList), label, 0);
	numLabels++;

	/* 
	 * Compute the dimensions of the tick label.  Remember that
	 * tick labels can be multi-lined and/or rotated.  
	 */
	Blt_GetTextExtents(&(virtAxisPtr->tickAttr), label, &width, &height);
	if (virtAxisPtr->theta > 0.0) {
	    Blt_GetBoundingBox(width, height, virtAxisPtr->theta, &width, &height, 
		(XPoint *)NULL);
	}
	if (width > maxWidth) {
	    maxWidth = width;
	}
	if (height > maxHeight) {
	    maxHeight = height;
	}
    }
    assert(numLabels <= axisPtr->majorPtr->numTicks);

    /*
     * Because the axis cap style is "CapProjecting", there's an extra 1.5
     * linewidth to be accounted for at the ends of each line.
     */
    pad = ((virtAxisPtr->lineWidth * 15) / 10) + 2;
    axisPtr->width = maxWidth + pad;
    axisPtr->height = maxHeight + pad;

    value = ABS(virtAxisPtr->tickLength) * 1.4;
    pad = ROUND(value) + graphPtr->plotBW + 1;
    if (graphPtr->plotBW > 0) {
	pad++;
    }
    if (HORIZAXIS(virtAxisPtr)) {
	axisPtr->height += pad;
    } else {
	axisPtr->width += pad;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetAxisMargin --
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
int
Blt_GetAxisMargin(axisPtr)
    Axis *axisPtr;
{
    int extent;

    if (HORIZAXIS(axisPtr->virtAxisPtr)) {
	extent = axisPtr->height;
    } else {
	extent = axisPtr->width + PADX;
    }
    return axisPtr->virtAxisPtr->titleAttr.height + extent;
}

/*
 *----------------------------------------------------------------------
 *
 * DefaultMargin --
 *
 *	Computes the size of the margin along the axis.  The axis
 *	geometry is computed if necessary.
 *
 * Results:
 *	Returns the default size of the margin (left, right, top,
 *	or bottom).
 *
 *----------------------------------------------------------------------
 */
static int
DefaultMargin(graphPtr, axisPtr)
    Graph *graphPtr;
    Axis *axisPtr;
{
    VirtualAxis *virtAxisPtr = axisPtr->virtAxisPtr;

    if (graphPtr->flags & GET_AXIS_GEOMETRY) {
	GetAxisGeometry(graphPtr, axisPtr);
    }
    if (virtAxisPtr->site != AXIS_VIRTUAL) {
	if ((!virtAxisPtr->hidden) && (virtAxisPtr->titleText != NULL)) {
	    int textWidth, textHeight;

	    Blt_GetTextExtents(&(virtAxisPtr->titleAttr), virtAxisPtr->titleText,
		&textWidth, &textHeight);
	    /* Pad title height */
	    virtAxisPtr->titleAttr.height = textHeight + (2 * TITLE_PAD);
	} else {
	    virtAxisPtr->titleAttr.width = virtAxisPtr->titleAttr.height = 0;
	}
    }
    if (HORIZAXIS(virtAxisPtr)) {
	return axisPtr->height;
    } else {
	return (axisPtr->width + PADX);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeMargins --
 *
 * Results:
 *
 *----------------------------------------------------------------------
 */
static void
ComputeMargins(graphPtr)
    Graph *graphPtr;
{
    int left, right, top, bottom;
    int defLeft, defRight, defTop, defBot;
    int margin, width, height;
    int bdWidths;

    defTop = DefaultMargin(graphPtr, graphPtr->topAxis);
    defBot = DefaultMargin(graphPtr, graphPtr->bottomAxis);
    defLeft = DefaultMargin(graphPtr, graphPtr->leftAxis);
    defRight = DefaultMargin(graphPtr, graphPtr->rightAxis);

    top = MAX(defTop + TitleHeight(graphPtr->topAxis), defBot);
    bottom = MAX(defBot + TitleHeight(graphPtr->bottomAxis), defTop);
    left = MAX(defLeft + TitleHeight(graphPtr->leftAxis), defRight);
    right = MAX(defRight + TitleHeight(graphPtr->rightAxis), defLeft);

    top = MAX(top, MINIMUM_MARGIN);
    bottom = MAX(bottom, MINIMUM_MARGIN);
    left = MAX(left, MINIMUM_MARGIN);
    right = MAX(right, MINIMUM_MARGIN);

    if (graphPtr->titleText != NULL) {
	if (graphPtr->topAxis->virtAxisPtr->hidden) {
	    top = MAX(top, graphPtr->titleAttr.height);
	} else {
	    top += graphPtr->titleAttr.height;
	}
    }
    bdWidths = graphPtr->borderWidth + graphPtr->plotBW;
    width = graphPtr->width - ((2 * bdWidths) + left + right);
    height = graphPtr->height - ((2 * bdWidths) + top + bottom);
    if (graphPtr->aspectRatio > 0.0) {
	double ratio;
	int w, h;

	/*
	 * Shrink the height or width of the calculated plot area to
	 * conform the requested aspect ratio
	 */
	ratio = (double)width / (double)height;
	if (ratio > graphPtr->aspectRatio) {
	    w = (int)(height * graphPtr->aspectRatio);
	    if (w < 1) {
		w = 1;
	    }
	    right += (width - w);
	    width = w;
	} else {
	    h = (int)(width / graphPtr->aspectRatio);
	    if (h < 1) {
		h = 1;
	    }
	    bottom += (height - h);
	    height = h;
	}
    }
    Blt_LayoutLegend(graphPtr, width, height);

    if (!graphPtr->legendPtr->hidden) {
	width = graphPtr->legendPtr->width + TITLE_PAD;
	height = graphPtr->legendPtr->height + TITLE_PAD;
	switch (GetLegendSite(graphPtr)) {
	case LEGEND_SITE_RIGHT:
	    if (graphPtr->rightAxis->virtAxisPtr->hidden) {
		right = MAX(right, width);
	    } else {
		right += width;
	    }
	    break;
	case LEGEND_SITE_LEFT:
	    if (graphPtr->leftAxis->virtAxisPtr->hidden) {
		left = MAX(left, width);
	    } else {
		left += width;
	    }
	    break;
	case LEGEND_SITE_TOP:
	    margin = height + graphPtr->titleAttr.height +
		Blt_GetAxisMargin(graphPtr->topAxis);
	    top = MAX(top, margin);
	    break;
	case LEGEND_SITE_BOTTOM:
	    margin = height + Blt_GetAxisMargin(graphPtr->bottomAxis);
	    bottom = MAX(bottom, margin);
	    break;
	case LEGEND_SITE_XY:
	case LEGEND_SITE_PLOT:
	    break;
	}
    }
    /* Override calculated values if user specified margins */

    graphPtr->leftMargin = (graphPtr->reqLeftMargin > 0)
	? graphPtr->reqLeftMargin : left + bdWidths;
    graphPtr->rightMargin = (graphPtr->reqRightMargin > 0)
	? graphPtr->reqRightMargin : right + bdWidths;
    graphPtr->topMargin = (graphPtr->reqTopMargin > 0)
	? graphPtr->reqTopMargin : top + bdWidths;
    graphPtr->bottomMargin = (graphPtr->reqBottomMargin > 0)
	? graphPtr->reqBottomMargin : bottom + bdWidths;

}

/*
 * -----------------------------------------------------------------
 *
 * Blt_LayoutMargins --
 *
 * 	Calculate the layout of the graph.  Based upon the data,
 *	axis limits, X and Y titles, and title height, determine
 *	the cavity left which is the plotting surface.  The first
 *	step get the data and axis limits for calculating the space
 *	needed for the top, bottom, left, and right margins.
 *
 * 	1) The LEFT margin is the area from the left border to the
 *	   Y axis (not including ticks). It composes the border
 *	   width, the width an optional Y axis label and its padding,
 *	   and the tick numeric labels. The Y axis label is rotated
 *	   90 degrees so that the width is the font height.
 *
 * 	2) The RIGHT margin is the area from the end of the graph
 *	   to the right window border. It composes the border width,
 *	   some padding, the font height (this may be dubious. It
 *	   appears to provide a more even border), the max of the
 *	   legend width and 1/2 max X tick number. This last part is
 *	   so that the last tick label is not clipped.
 *
 *           Window Width
 *      ___________________________________________________________
 *      |          |                               |               |
 *      |          |   TOP  height of title        |               |
 *      |          |                               |               |
 *      |          |           x2 title            |               |
 *      |          |                               |               |
 *      |          |        height of x2-axis      |               |
 *      |__________|_______________________________|_______________|  W
 *      |          | -plotpady                     |               |  i
 *      |__________|_______________________________|_______________|  n
 *      |          | yMin                   xMax   |               |  d
 *      |          |                               |               |  o
 *      |   LEFT   |                               |     RIGHT     |  w
 *      |          |                               |               |
 *      | y        |     Free area = 104%          |      y2       |  H
 *      |          |     Plotting surface = 100%   |               |  e
 *      | t        |     Tick length = 2 + 2%      |      t        |  i
 *      | i        |                               |      i        |  g
 *      | t        |                               |      t  legend|  h
 *      | l        |                               |      l   width|  t
 *      | e        |                               |      e        |
 *      |    height|                               |height         |
 *      |       of |                               | of            |
 *      |    y-axis|                               |y2-axis        |
 *      |          |                               |               |
 *      |          |origin 0,0                     |               |
 *      |__________|_xMin___________________yMax___|_______________|
 *      |          |-plotpady                      |               |
 *      |__________|_______________________________|_______________|
 *      |          | (xoffset, yoffset)            |               |
 *      |          |                               |               |
 *      |          |       height of x-axis        |               |
 *      |          |                               |               |
 *      |          |   BOTTOM   x title            |               |
 *      |__________|_______________________________|_______________|
 *
 * 3) The TOP margin is the area from the top window border to the top
 *    of the graph. It composes the border width, twice the height of
 *    the title font (if one is given) and some padding between the
 *    title.
 *
 * 4) The BOTTOM margin is area from the bottom window border to the
 *    X axis (not including ticks). It composes the border width, the height
 *    an optional X axis label and its padding, the height of the font
 *    of the tick labels.
 *
 * The plotting area is between the margins which includes the X and Y axes
 * including the ticks but not the tick numeric labels. The length of
 * the ticks and its padding is 5% of the entire plotting area.  Hence the
 * entire plotting area is scaled as 105% of the width and height of the
 * area.
 *
 * The axis labels, ticks labels, title, and legend may or may not be
 * displayed which must be taken into account.
 *
 *
 * -----------------------------------------------------------------
 */
void
Blt_LayoutMargins(graphPtr)
    Graph *graphPtr;
{
    int width, height;
    int titleY;
    int bdWidths;

    ComputeMargins(graphPtr);

    bdWidths = graphPtr->borderWidth + graphPtr->plotBW;

    /* Based upon the margins, calculate the space left for the graph. */
    width = graphPtr->width - (graphPtr->leftMargin + graphPtr->rightMargin);
    height = graphPtr->height - (graphPtr->topMargin + graphPtr->bottomMargin);
    if (width < 1) {
	width = 1;
    }
    if (height < 1) {
	height = 1;
    }
    graphPtr->xMin = graphPtr->leftMargin;
    graphPtr->xMax = graphPtr->leftMargin + width - 1;
    graphPtr->yMax = graphPtr->topMargin + height - 1;
    graphPtr->yMin = graphPtr->topMargin;

    graphPtr->vOffset = graphPtr->topMargin + graphPtr->padTop;
    graphPtr->vRange = height - PADDING(graphPtr->padY);
    graphPtr->hOffset = graphPtr->leftMargin + graphPtr->padLeft;
    graphPtr->hRange = width - PADDING(graphPtr->padX);
    if (graphPtr->vRange < 1) {
	graphPtr->vRange = 1;
    }
    if (graphPtr->hRange < 1) {
	graphPtr->hRange = 1;
    }
    /*
     * Calculate the placement of the graph title so it is centered within the
     * space provided for it in the top margin
     */

    titleY = graphPtr->topMargin - (bdWidths +
	Blt_GetAxisMargin(graphPtr->axisArr + 2));
    if ((!graphPtr->legendPtr->hidden) &&
	(GetLegendSite(graphPtr) == LEGEND_SITE_TOP)) {
	titleY -= graphPtr->legendPtr->height;
    }
    graphPtr->titleY = (titleY / 2) + graphPtr->borderWidth;
    graphPtr->titleX = (graphPtr->xMax + graphPtr->xMin) / 2;

}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureVirtualAxis --
 *
 *	Configures axis attributes (font, line width, label, etc).
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Axis layout is deferred until the height and width of the
 *	window are known.
 *
 * ----------------------------------------------------------------------
 */

static int
ConfigureVirtualAxis(graphPtr, axisPtr)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
{
    char errMsg[200];
    /*
     * Check requested X and Y axis limits. Can't allow min to be greater than
     * max, or have undefined log scale limits.
     */
    if (!(axisPtr->flags & (AXIS_AUTO_MIN | AXIS_AUTO_MAX)) &&
	(axisPtr->dataLimits.min >= axisPtr->dataLimits.max)) {
	sprintf(errMsg, "impossible limits (min %g >= max %g) for \"%s\" axis",
	    axisPtr->dataLimits.min, axisPtr->dataLimits.max, axisPtr->name);
	Tcl_AppendResult(graphPtr->interp, errMsg, (char *)NULL);
	/* Bad values, turn on axis auto-scaling */
	axisPtr->flags |= (AXIS_AUTO_MIN | AXIS_AUTO_MAX);
	return TCL_ERROR;
    }
    if ((axisPtr->logScale) && !(axisPtr->flags & AXIS_AUTO_MIN) &&
	(axisPtr->dataLimits.min <= 0.0)) {
	sprintf(errMsg, "bad logscale limits (min=%g,max=%g) for \"%s\" axis",
	    axisPtr->dataLimits.min, axisPtr->dataLimits.max, axisPtr->name);
	Tcl_AppendResult(graphPtr->interp, errMsg, (char *)NULL);
	/* Bad minimum value, turn on auto-scaling */
	axisPtr->flags |= AXIS_AUTO_MIN;
	return TCL_ERROR;
    }
    axisPtr->theta = FMOD(axisPtr->theta, 360.0);
    if (axisPtr->theta < 0.0) {
	axisPtr->theta += 360.0;
    }
    axisPtr->tickAttr.theta = axisPtr->theta;
    ResetAttributes(graphPtr, axisPtr);

    /*
     * Don't bother to check what options have changed.  Almost every axis
     * configuration option changes the size of the plotting area (except for
     * -foreground).
     *
     * Recompute the scale and offset of the axis in case -min, -max options
     * have changed.
     */
    graphPtr->flags |= (REDRAW_WORLD | COORDS_WORLD | RESET_AXES);
    axisPtr->flags |= AXIS_CONFIG_DIRTY;
    Blt_EventuallyRedrawGraph(graphPtr);

    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateVirtualAxis --
 *
 *	Create and initialize a structure containing information to
 * 	display a graph axis.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
static VirtualAxis *
CreateVirtualAxis(graphPtr, name, site)
    Graph *graphPtr;
    char *name;			/* Identifier for axis. */
    AxisSite site;
{
    VirtualAxis *axisPtr;
    Tcl_HashEntry *hPtr;
    int isNew;

    hPtr = Tcl_CreateHashEntry(&(graphPtr->axisTable), name, &isNew);
    if (!isNew) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);
	if (!axisPtr->deletePending) {
	    Tcl_AppendResult(graphPtr->interp, "axis \"", name,
		"\" already exists in \"", Tk_PathName(graphPtr->tkwin), "\"",
		(char *)NULL);
	    return NULL;
	}
	axisPtr->deletePending = 0;
    } else {
	axisPtr = (VirtualAxis *)calloc(1, sizeof(VirtualAxis));
	assert(axisPtr);

	axisPtr->name = strdup(name);
	axisPtr->hashPtr = hPtr;

	/* Default flags */
	axisPtr->showTicks = 1;
	axisPtr->flags = (AXIS_AUTO_MIN | AXIS_AUTO_MAX);
	axisPtr->reqStep = 0.0;
	axisPtr->reqNumMinorTicks = 2;
	axisPtr->range = 1.0;
	axisPtr->shiftBy = 0.0;
	axisPtr->theta = 0.0;
	axisPtr->showTicks = 1;
	if ((site == AXIS_TOP) || (site == AXIS_BOTTOM)) {
	    if (graphPtr->type == TYPE_ELEM_BAR) {
		axisPtr->reqStep = 1.0;
		axisPtr->reqNumMinorTicks = 0;
	    }
	}
	if ((site == AXIS_RIGHT) || (site == AXIS_TOP)) {
	    axisPtr->hidden = 1;
	}
	axisPtr->site = site;
	Blt_InitTextAttributes(&(axisPtr->titleAttr));
	Blt_InitTextAttributes(&(axisPtr->limitAttr));
	Blt_InitTextAttributes(&(axisPtr->tickAttr));
	axisPtr->lineWidth = 1;
	Tcl_SetHashValue(hPtr, (ClientData)axisPtr);
    }
    return axisPtr;
}

static VirtualAxis *
NameToVirtualAxis(graphPtr, name)
    Graph *graphPtr;
    char *name;
{
    Tcl_HashEntry *hPtr;
    VirtualAxis *axisPtr;

    hPtr = Tcl_FindHashEntry(&(graphPtr->axisTable), name);
    if (hPtr == NULL) {
      notFound:
	Tcl_AppendResult(graphPtr->interp, "can't find axis \"", name,
	    "\" in \"", Tk_PathName(graphPtr->tkwin), "\"", (char *)NULL);
	return NULL;
    }
    axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);
    if (axisPtr->deletePending) {
	goto notFound;
    }
    return (VirtualAxis *) Tcl_GetHashValue(hPtr);
}

static int
GetVirtualAxis(graphPtr, axisName, flags, axisPtrPtr)
    Graph *graphPtr;
    char *axisName;
    unsigned int flags;
    VirtualAxis **axisPtrPtr;
{
    VirtualAxis *axisPtr;
    unsigned int mask;

    mask = (flags & AXIS_TYPE_MASK);
    axisPtr = NameToVirtualAxis(graphPtr, axisName);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    if (axisPtr->refCount == 0) { /* Set type if we're the only ones using it */
	axisPtr->flags &= ~AXIS_TYPE_MASK;
	axisPtr->flags |= mask;
    } else if ((axisPtr->flags & AXIS_TYPE_MASK) != mask) {
	Tcl_AppendResult(graphPtr->interp, "axis \"", axisName,
	    "\" is already in use on an opposite axis", (char *)NULL);
	return TCL_ERROR;
    }
    axisPtr->refCount++;
    *axisPtrPtr = axisPtr;
    return TCL_OK;
}

static void
FreeVirtualAxis(graphPtr, axisPtr)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
{
    axisPtr->refCount--;
    if ((axisPtr->deletePending) && (axisPtr->refCount == 0)) {
	DestroyVirtualAxis(graphPtr, axisPtr);
    }
}


void
Blt_DestroyAxes(graphPtr)
    Graph *graphPtr;
{
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    VirtualAxis *virtAxisPtr;
    Axis *axisPtr;
    register int i;

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	virtAxisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);
	virtAxisPtr->hashPtr = NULL;
	DestroyVirtualAxis(graphPtr, virtAxisPtr);
    }
    Tcl_DeleteHashTable(&(graphPtr->axisTable));
    for (axisPtr = graphPtr->axisArr, i = 0; i < 4; i++, axisPtr++) {
	DestroyAxis(graphPtr, axisPtr);
    }
}


int
Blt_DefaultAxes(graphPtr)
    Graph *graphPtr;
{
    register int i;
    VirtualAxis *axisPtr;
    static char *axisNames[] = {"x", "y", "x2", "y2"};
    int flags;

    flags = GraphType(graphPtr);
    for (i = 0; i < 4; i++) {
	axisPtr = CreateVirtualAxis(graphPtr, axisNames[i], (AxisSite)i);
	if (axisPtr == NULL) {
	    return TCL_ERROR;
	}
	axisPtr->refCount = 1;
	axisPtr->flags |= (i & 1) ? AXIS_TYPE_Y : AXIS_TYPE_X;
	graphPtr->axisArr[i].virtAxisPtr = axisPtr;
	Blt_InitList(&(graphPtr->axisArr[i].labelList), TCL_STRING_KEYS);

	/*
	 * Call Blt_ConfigureWidgetComponent when we create the axis. This
	 * procedure will create a dummy window before configuring
	 * the axis so that the resource database can be accessed.
	 */
	if (Blt_ConfigureWidgetComponent(graphPtr->interp, graphPtr->tkwin,
		axisPtr->name, "Axis", configSpecs, 0, (char **)NULL,
		(char *)axisPtr, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (ConfigureVirtualAxis(graphPtr, axisPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * CgetOp --
 *
 *	Queries axis attributes (font, line width, label, etc).
 *
 * Results:
 *	Return value is a standard Tcl result.  If querying configuration
 *	values, interp->result will contain the results.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
CgetOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    int argc;			/* Not used */
    char *argv[];
{
    return Tk_ConfigureValue(graphPtr->interp, graphPtr->tkwin, configSpecs,
	(char *)axisPtr, argv[0], GraphType(graphPtr));
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 *	Queries or resets axis attributes (font, line width, label, etc).
 *
 * Results:
 *	Return value is a standard Tcl result.  If querying configuration
 *	values, interp->result will contain the results.
 *
 * Side Effects:
 *	Axis resources are possibly allocated (GC, font). Axis layout is
 *	deferred until the height and width of the window are known.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    int argc;
    char *argv[];
{
    int flags;

    flags = TK_CONFIG_ARGV_ONLY | GraphType(graphPtr);
    if (argc == 0) {
	return Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    (char *)axisPtr, (char *)NULL, flags);
    } else if (argc == 1) {
	return Tk_ConfigureInfo(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    (char *)axisPtr, argv[0], flags);
    }
    if (Tk_ConfigureWidget(graphPtr->interp, graphPtr->tkwin, configSpecs,
	    argc, argv, (char *)axisPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if (ConfigureVirtualAxis(graphPtr, axisPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (axisPtr->refCount > 0) {
	graphPtr->flags |= REDRAW_BACKING_STORE | REDRAW_MARGINS;
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * LimitsOp --
 *
 *	This procedure returns a string representing the axis limits
 *	of the graph.  The format of the string is { xmin ymin xmax ymax}.
 *
 * Results:
 *	Always returns TCL_OK.  The interp->result field is
 *	a list of the graph axis limits.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
LimitsOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    int argc;			/* not used */
    char **argv;		/* not used */

{
    char *result;
    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    if (axisPtr->logScale) {
	result = Blt_Double(graphPtr->interp, EXP10(axisPtr->limitsPtr->min));
	Tcl_AppendElement(graphPtr->interp, result);
	result = Blt_Double(graphPtr->interp, EXP10(axisPtr->limitsPtr->max));
	Tcl_AppendElement(graphPtr->interp, result);
    } else {
	result = Blt_Double(graphPtr->interp, axisPtr->limitsPtr->min);
	Tcl_AppendElement(graphPtr->interp, result);
	result = Blt_Double(graphPtr->interp, axisPtr->limitsPtr->max);
	Tcl_AppendElement(graphPtr->interp, result);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * InvTransformOp --
 *
 *	Maps the given window coordinate into an axis-value.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the axis value. If an error occurred, TCL_ERROR is returned
 *	and interp->result will contain an error message.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InvTransformOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    int argc;			/* not used */
    char **argv;
{
    int x;			/* Integer window coordinate*/
    double y;			/* Real graph coordinate */

    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    if (Tcl_GetInt(graphPtr->interp, argv[0], &x) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Is the axis vertical or horizontal?
     *
     * Check the site where the axis was positioned.  If the axis is virtual,
     * all we have to go on is how it was mapped to an element (using either
     * -mapx or -mapy options).
     */
    if (HORIZAXIS(axisPtr)) {
	y = InvHMap(graphPtr, axisPtr, (double)x);
    } else {
	y = InvVMap(graphPtr, axisPtr, (double)x);
    }
    Tcl_AppendElement(graphPtr->interp, Blt_Double(graphPtr->interp, y));
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformOp --
 *
 *	Maps the given axis-value to a window coordinate.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the window coordinate. If an error occurred, TCL_ERROR
 *	is returned and interp->result will contain an error
 *	message.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TransformOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;	/* Axis */
    int argc;			/* not used */
    char **argv;
{
    double x;

    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    if (Blt_GetCoordinate(graphPtr->interp, argv[0], &x) != TCL_OK) {
	return TCL_ERROR;
    }
    if (HORIZAXIS(axisPtr)) {
	x = HMap(graphPtr, axisPtr, x);
    } else {
	x = VMap(graphPtr, axisPtr, x);
    }
    Tcl_SetResult(graphPtr->interp, Blt_Int((int)x), TCL_VOLATILE);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * UseOp --
 *
 *	Changes the virtual axis used by the logical axis.
 *
 * Results:
 *	Always returns TCL_OK.  The interp->result field is
 *	a list of the graph axis limits.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
UseOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    int argc;
    char **argv;

{
    if (argc == 1) {
	VirtualAxis *oldPtr;

	oldPtr = axisPtr;
	if (GetVirtualAxis(graphPtr, argv[0], oldPtr->flags, &axisPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	graphPtr->axisArr[oldPtr->site].virtAxisPtr = axisPtr;
	oldPtr->site = AXIS_VIRTUAL;
	Blt_AdjustAxisPointers(graphPtr);
	Blt_EventuallyRedrawGraph(graphPtr);
	graphPtr->flags |= (GET_AXIS_GEOMETRY | LAYOUT_NEEDED | RESET_AXES);
	/*
	 * When any axis changes, we need to layout the entire graph.
	 */
	graphPtr->flags |= (COORDS_WORLD | REDRAW_WORLD);
    }
    Tcl_SetResult(graphPtr->interp, axisPtr->name, TCL_STATIC);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateVirtualOp --
 *
 *	Creates a new axis.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CreateVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    VirtualAxis *axisPtr;
    int flags;

    axisPtr = CreateVirtualAxis(graphPtr, argv[3], AXIS_VIRTUAL);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    flags = GraphType(graphPtr);
    if (Blt_ConfigureWidgetComponent(graphPtr->interp, graphPtr->tkwin,
	    axisPtr->name, "Axis", configSpecs, argc - 4, argv + 4,
	    (char *)axisPtr, flags) != TCL_OK) {
	goto error;
    }
    if (ConfigureVirtualAxis(graphPtr, axisPtr) != TCL_OK) {
	goto error;
    }
    Tcl_SetResult(graphPtr->interp, axisPtr->name, TCL_STATIC);
    return TCL_OK;
  error:
    DestroyVirtualAxis(graphPtr, axisPtr);
    return TCL_ERROR;
}

/*
 * ----------------------------------------------------------------------
 *
 * CgetVirtualOp --
 *
 *	Queries axis attributes (font, line width, label, etc).
 *
 * Results:
 *	Return value is a standard Tcl result.  If querying configuration
 *	values, interp->result will contain the results.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
CgetVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char *argv[];
{
    VirtualAxis *axisPtr;

    axisPtr = NameToVirtualAxis(graphPtr, argv[3]);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    return CgetOp(graphPtr, axisPtr, argc - 4, argv + 4);
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureVirtualOp --
 *
 *	Queries or resets axis attributes (font, line width, label, etc).
 *
 * Results:
 *	Return value is a standard Tcl result.  If querying configuration
 *	values, interp->result will contain the results.
 *
 * Side Effects:
 *	Axis resources are possibly allocated (GC, font). Axis layout is
 *	deferred until the height and width of the window are known.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char *argv[];
{
    VirtualAxis *axisPtr;
    int numNames, numOpts;
    char **options;
    register int i;

    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] == '-') {
	    break;
	}
	if (NameToVirtualAxis(graphPtr, argv[i]) == NULL) {
	    return TCL_ERROR;
	}
    }
    numNames = i;		/* Number of pen names specified */
    numOpts = argc - i;		/* Number of options specified */
    options = argv + i;		/* Start of options in argv  */

    for (i = 0; i < numNames; i++) {
	axisPtr = NameToVirtualAxis(graphPtr, argv[i]);
	if (ConfigureOp(graphPtr, axisPtr, numOpts, options) != TCL_OK) {
	    break;
	}
    }
    if (i < numNames) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * DeleteVirtualOp --
 *
 *	Deletes one or more axes.  The actual removal may be deferred
 *	until the axis is no longer used by any element. The axis
 *	can't be referenced by its name any longer and it may be
 *	recreated.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;
    char **argv;
{
    register int i;
    VirtualAxis *axisPtr;

    for (i = 3; i < argc; i++) {
	axisPtr = NameToVirtualAxis(graphPtr, argv[i]);
	if (axisPtr == NULL) {
	    return TCL_ERROR;
	}
	axisPtr->deletePending = 1;
	if (axisPtr->refCount == 0) {
	    DestroyVirtualAxis(graphPtr, axisPtr);
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * InvTransformOp --
 *
 *	Maps the given window coordinate into an axis-value.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the axis value. If an error occurred, TCL_ERROR is returned
 *	and interp->result will contain an error message.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InvTransformVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;			/* not used */
    char **argv;
{
    VirtualAxis *axisPtr;

    axisPtr = NameToVirtualAxis(graphPtr, argv[3]);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    return InvTransformOp(graphPtr, axisPtr, argc - 4, argv + 4);
}

/*
 *--------------------------------------------------------------
 *
 * LimitsVirtualOp --
 *
 *	This procedure returns a string representing the axis limits
 *	of the graph.  The format of the string is { xmin ymin xmax ymax}.
 *
 * Results:
 *	Always returns TCL_OK.  The interp->result field is
 *	a list of the graph axis limits.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
LimitsVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;			/* not used */
    char **argv;		/* not used */

{
    VirtualAxis *axisPtr;

    axisPtr = NameToVirtualAxis(graphPtr, argv[3]);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    return LimitsOp(graphPtr, axisPtr, argc - 4, argv + 4);
}

/*
 * ----------------------------------------------------------------------
 *
 * NamesVirtualOp --
 *
 *	Return a list of the names of all the axes.
 *
 * Results:
 *	Returns a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */

/*ARGSUSED*/
static int
NamesVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;			/* not used */
    char **argv;		/* not used */
{
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    VirtualAxis *axisPtr;
    register int i;

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);
	if (axisPtr->deletePending) {
	    continue;
	}
	if (argc == 3) {
	    Tcl_AppendElement(graphPtr->interp, axisPtr->name);
	    continue;
	}
	for (i = 3; i < argc; i++) {
	    if (Tcl_StringMatch(axisPtr->name, argv[i])) {
		Tcl_AppendElement(graphPtr->interp, axisPtr->name);
		break;
	    }
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformOp --
 *
 *	Maps the given axis-value to a window coordinate.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the window coordinate. If an error occurred, TCL_ERROR
 *	is returned and interp->result will contain an error
 *	message.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TransformVirtualOp(graphPtr, argc, argv)
    Graph *graphPtr;
    int argc;			/* not used */
    char **argv;
{
    VirtualAxis *axisPtr;

    axisPtr = NameToVirtualAxis(graphPtr, argv[3]);
    if (axisPtr == NULL) {
	return TCL_ERROR;
    }
    return TransformOp(graphPtr, axisPtr, argc - 4, argv + 4);
}

int
Blt_VirtualAxisOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;
    static Blt_OpSpec axisOps[] =
    {
	{"cget", 2, (Blt_Operation)CgetVirtualOp, 5, 5, "axisName option",},
	{"configure", 2, (Blt_Operation)ConfigureVirtualOp, 4, 0,
	    "axisName ?axisName?... ?option value?...",},
	{"create", 2, (Blt_Operation)CreateVirtualOp, 4, 0,
	    "axisName ?option value?...",},
	{"delete", 1, (Blt_Operation)DeleteVirtualOp, 3, 0, "?axisName?...",},
	{"invtransform", 1, (Blt_Operation)InvTransformVirtualOp, 5, 5,
	    "axisName value",},
	{"limits", 1, (Blt_Operation)LimitsVirtualOp, 4, 4, "axisName",},
	{"names", 1, (Blt_Operation)NamesVirtualOp, 3, 0, "?pattern?...",},
	{"transform", 1, (Blt_Operation)TransformVirtualOp, 5, 5,
	    "axisName value",},
    };
    static int numAxisOps = sizeof(axisOps) / sizeof(Blt_OpSpec);

    proc = Blt_GetOperation(interp, numAxisOps, axisOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (graphPtr, argc, argv);
    return result;
}

int
Blt_AxisOp(graphPtr, axisPtr, argc, argv)
    Graph *graphPtr;
    Axis *axisPtr;
    int argc;
    char **argv;
{
    int result;
    Blt_Operation proc;
    static Blt_OpSpec axisOps[] =
    {
	{"cget", 2, (Blt_Operation)CgetOp, 4, 4, "option",},
	{"configure", 2, (Blt_Operation)ConfigureOp, 3, 0, "?option value?...",},
	{"invtransform", 1, (Blt_Operation)InvTransformOp, 4, 4, "value",},
	{"limits", 1, (Blt_Operation)LimitsOp, 3, 3, "",},
	{"transform", 1, (Blt_Operation)TransformOp, 4, 4, "value",},
	{"use", 1, (Blt_Operation)UseOp, 3, 4, "?axisName?",},
    };
    static int numAxisOps = sizeof(axisOps) / sizeof(Blt_OpSpec);

    proc = Blt_GetOperation(graphPtr->interp, numAxisOps, axisOps,
	BLT_OPER_ARG2, argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (graphPtr, axisPtr->virtAxisPtr, argc - 3, argv + 3);
    return result;
}

void
Blt_DrawAxes(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;
{
    Axis *axisPtr;
    register int i;

    for (axisPtr = graphPtr->axisArr, i = 0; i < 4; i++, axisPtr++) {
	if (!axisPtr->virtAxisPtr->hidden) {
	    DrawAxis(graphPtr, drawable, axisPtr);
	}
    }
}

void
Blt_PrintAxes(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    Axis *axisPtr;
    register int i;

    for (axisPtr = graphPtr->axisArr, i = 0; i < 4; i++, axisPtr++) {
	if (!axisPtr->virtAxisPtr->hidden) {
	    PrintAxis(printable, axisPtr);
	}
    }
}


void
Blt_DrawAxisLimits(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;
{
    VirtualAxis *axisPtr;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    Limits *limitsPtr;
    Dimension textDim;
    int horizontal;
    char *minPtr, *maxPtr;
    char *minFormat, *maxFormat;
    char minString[200], maxString[200];
    int vMin, hMin, vMax, hMax;

#define SPACING 8
    vMin = vMax = graphPtr->xMin + graphPtr->padLeft + 2;
    hMin = hMax = graphPtr->yMax - graphPtr->padBottom - 2;	/* Offsets */

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);

	if (axisPtr->numFormats == 0) {
	    continue;
	}
	if ((axisPtr->logScale) || (axisPtr->loose) ||
	    (axisPtr->dataLimits.min == axisPtr->dataLimits.max)) {
	    limitsPtr = &(axisPtr->tickLimits);
	} else {
	    limitsPtr = &(axisPtr->dataLimits);
	}
	horizontal = ((axisPtr->flags & AXIS_TYPE_X) != 0);
	if (graphPtr->inverted) {
	    horizontal = (horizontal == 0);
	}
	minPtr = maxPtr = NULL;
	minFormat = maxFormat = axisPtr->limitFormats[0];
	if (axisPtr->numFormats > 1) {
	    maxFormat = axisPtr->limitFormats[1];
	}
	if (minFormat[0] != '\0') {
	    minPtr = minString;
	    sprintf(minString, minFormat, limitsPtr->min);
	}
	if (maxFormat[0] != '\0') {
	    maxPtr = maxString;
	    sprintf(maxString, maxFormat, limitsPtr->max);
	}
	if (axisPtr->descending) {
	    char *tmp;

	    tmp = minPtr, minPtr = maxPtr, maxPtr = tmp;
	}
	if (maxPtr != NULL) {
	    if (horizontal) {
		axisPtr->limitAttr.theta = 90.0;
		axisPtr->limitAttr.anchor = TK_ANCHOR_SE;
		Blt_DrawText2(graphPtr->tkwin, drawable, maxPtr,
		    &(axisPtr->limitAttr), graphPtr->xMax, hMax, &textDim);
		hMax -= (textDim.height + SPACING);
	    } else {
		axisPtr->limitAttr.theta = 0.0;
		axisPtr->limitAttr.anchor = TK_ANCHOR_NW;
		Blt_DrawText2(graphPtr->tkwin, drawable, maxPtr,
		    &(axisPtr->limitAttr), vMax, graphPtr->yMin, &textDim);
		vMax += (textDim.width + SPACING);
	    }
	}
	if (minPtr != NULL) {
	    axisPtr->limitAttr.anchor = TK_ANCHOR_SW;
	    if (horizontal) {
		axisPtr->limitAttr.theta = 90.0;
		Blt_DrawText2(graphPtr->tkwin, drawable, minPtr,
		    &(axisPtr->limitAttr), graphPtr->xMin, hMin, &textDim);
		hMin -= (textDim.height + SPACING);
	    } else {
		axisPtr->limitAttr.theta = 0.0;
		Blt_DrawText2(graphPtr->tkwin, drawable, minPtr,
		    &(axisPtr->limitAttr), vMin, graphPtr->yMax, &textDim);
		vMin += (textDim.width + SPACING);
	    }
	}
    }				/* Loop on axes */
}

void
Blt_PrintAxisLimits(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    VirtualAxis *axisPtr;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    int vMin, hMin, vMax, hMax;
    char string[200];
    int textWidth, textHeight;
    Limits *limitsPtr;
    char *minFmt, *maxFmt;

#define SPACING 8
    vMin = vMax = graphPtr->xMin + graphPtr->padLeft + 2;
    hMin = hMax = graphPtr->yMax - graphPtr->padBottom - 2;	/* Offsets */
    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->axisTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	axisPtr = (VirtualAxis *)Tcl_GetHashValue(hPtr);

	if (axisPtr->numFormats == 0) {
	    continue;
	}
	if (axisPtr->logScale || axisPtr->loose ||
	    (axisPtr->dataLimits.min == axisPtr->dataLimits.max)) {
	    limitsPtr = &(axisPtr->tickLimits);
	} else {
	    limitsPtr = &(axisPtr->dataLimits);
	}
	minFmt = maxFmt = axisPtr->limitFormats[0];
	if (axisPtr->numFormats > 1) {
	    maxFmt = axisPtr->limitFormats[1];
	}
	if (*maxFmt != '\0') {
	    sprintf(string, maxFmt, limitsPtr->max);
	    Blt_GetTextExtents(&(axisPtr->tickAttr), string, &textWidth,
		&textHeight);
	    if ((textWidth > 0) && (textHeight > 0)) {
		if ((axisPtr->flags & AXIS_TYPE_MASK) & AXIS_TYPE_X) {
		    axisPtr->limitAttr.theta = 90.0;
		    axisPtr->limitAttr.anchor = TK_ANCHOR_SE;
		    Blt_PrintText(printable, string, &(axisPtr->limitAttr),
			graphPtr->xMax, hMax);
		    hMax -= (textWidth + SPACING);
		} else {
		    axisPtr->limitAttr.theta = 0.0;
		    axisPtr->limitAttr.anchor = TK_ANCHOR_NW;
		    Blt_PrintText(printable, string, &(axisPtr->limitAttr), vMax,
			graphPtr->yMin);
		    vMax += (textWidth + SPACING);
		}
	    }
	}
	if (*minFmt != '\0') {
	    sprintf(string, minFmt, limitsPtr->min);
	    Blt_GetTextExtents(&(axisPtr->tickAttr), string, &textWidth,
		&textHeight);
	    if ((textWidth > 0) && (textHeight > 0)) {
		axisPtr->limitAttr.anchor = TK_ANCHOR_SW;
		if ((axisPtr->flags & AXIS_TYPE_MASK) & AXIS_TYPE_X) {
		    axisPtr->limitAttr.theta = 90.0;
		    Blt_PrintText(printable, string, &(axisPtr->limitAttr),
			graphPtr->xMin, hMin);
		    hMin -= (textWidth + SPACING);
		} else {
		    axisPtr->limitAttr.theta = 0.0;
		    Blt_PrintText(printable, string, &(axisPtr->limitAttr), vMin,
			graphPtr->yMax);
		    vMin += (textWidth + SPACING);
		}
	    }
	}
    }
}
