
/*
 * bltGrLegd.c --
 *
 *	This module implements the legend for the BLT graph widget.
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

#define padLeft  	padX.side1
#define padRight  	padX.side2
#define padTop		padY.side1
#define padBottom	padY.side2
#define PADDING(x)	((x).side1 + (x).side2)

#define DEF_LEGEND_ACTIVE_BG_COLOR 	STD_COLOR_ACTIVE_BG
#define DEF_LEGEND_ACTIVE_BG_MONO	STD_MONO_ACTIVE_BG
#define DEF_LEGEND_ACTIVE_BORDER_WIDTH  "2"
#define DEF_LEGEND_ACTIVE_FG_COLOR	STD_COLOR_ACTIVE_FG
#define DEF_LEGEND_ACTIVE_FG_MONO	STD_MONO_ACTIVE_FG
#define DEF_LEGEND_ACTIVE_RELIEF	"flat"
#define DEF_LEGEND_ANCHOR	   	"n"
#define DEF_LEGEND_BG_COLOR	   	(char *)NULL
#define DEF_LEGEND_BG_MONO		(char *)NULL
#define DEF_LEGEND_BORDER_WIDTH 	STD_BORDERWIDTH
#define DEF_LEGEND_FG_COLOR		STD_COLOR_NORMAL_FG
#define DEF_LEGEND_FG_MONO		STD_MONO_NORMAL_FG
#define DEF_LEGEND_FONT			STD_FONT_SMALL
#define DEF_LEGEND_HIDE			"no"
#define DEF_LEGEND_IPAD_X		"1"
#define DEF_LEGEND_IPAD_Y		"1"
#define DEF_LEGEND_PAD_X		"4"
#define DEF_LEGEND_PAD_Y		"4"
#define DEF_LEGEND_POSITION		"rightmargin"
#define DEF_LEGEND_RAISED       	"no"
#define DEF_LEGEND_RELIEF		"sunken"
#define DEF_LEGEND_SHADOW_COLOR		(char *)NULL
#define DEF_LEGEND_SHADOW_MONO		(char *)NULL

static int StringToPosition _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *PositionToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

static Tk_CustomOption legendPositionOption =
{
    StringToPosition, PositionToString, (ClientData)0
};

extern Tk_CustomOption bltPositionOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltPadOption;
extern Tk_CustomOption bltShadowOption;

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"ActiveBackground", DEF_LEGEND_ACTIVE_BG_COLOR,
	Tk_Offset(Legend, activeBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-activebackground", "activeBackground",
	"ActiveBackground", DEF_LEGEND_ACTIVE_BG_MONO,
	Tk_Offset(Legend, activeBorder), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-activeborderwidth", "activeBorderWidth",
	"BorderWidth", DEF_LEGEND_BORDER_WIDTH,
	Tk_Offset(Legend, entryBorderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"ActiveForeground", DEF_LEGEND_ACTIVE_FG_COLOR,
	Tk_Offset(Legend, entryAttr.activeColor), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-activeforeground", "activeForeground",
	"ActiveForeground", DEF_LEGEND_ACTIVE_FG_MONO,
	Tk_Offset(Legend, entryAttr.activeColor), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_RELIEF, "-activerelief", "activeRelief", "Relief",
	DEF_LEGEND_ACTIVE_RELIEF, Tk_Offset(Legend, activeRelief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_LEGEND_ANCHOR, Tk_Offset(Legend, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-background", "background", "Background",
	DEF_LEGEND_BG_MONO, Tk_Offset(Legend, fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-background", "background", "Background",
	DEF_LEGEND_BG_COLOR, Tk_Offset(Legend, fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_LEGEND_BORDER_WIDTH, Tk_Offset(Legend, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_LEGEND_FONT, Tk_Offset(Legend, entryAttr.font), 0},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_LEGEND_FG_COLOR, Tk_Offset(Legend, entryAttr.color),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_LEGEND_FG_MONO, Tk_Offset(Legend, entryAttr.color),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_LEGEND_HIDE, Tk_Offset(Legend, hidden), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-ipadx", "iPadX", "Pad",
	DEF_LEGEND_IPAD_X, Tk_Offset(Legend, ipadX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-ipady", "iPadY", "Pad",
	DEF_LEGEND_IPAD_Y, Tk_Offset(Legend, ipadY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-padx", "padX", "Pad",
	DEF_LEGEND_PAD_X, Tk_Offset(Legend, padX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-pady", "padY", "Pad",
	DEF_LEGEND_PAD_Y, Tk_Offset(Legend, padY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-position", "position", "Position",
	DEF_LEGEND_POSITION, Tk_Offset(Legend, anchorPos),
	TK_CONFIG_DONT_SET_DEFAULT, &legendPositionOption},
    {TK_CONFIG_BOOLEAN, "-raised", "raised", "Raised",
	DEF_LEGEND_RAISED, Tk_Offset(Legend, raised),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_LEGEND_RELIEF, Tk_Offset(Legend, relief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_LEGEND_SHADOW_COLOR, Tk_Offset(Legend, entryAttr.shadow),
	TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_LEGEND_SHADOW_MONO, Tk_Offset(Legend, entryAttr.shadow),
	TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

#ifdef __STDC__
static Tcl_IdleProc DrawLegend;
static BindPickProc PickLegendEntry;
#endif

static int
GetLegendPosition(interp, string, posPtr)
    Tcl_Interp *interp;
    char *string;
    LegendPosition *posPtr;
{
    char c;
    unsigned int length;

    if ((string == NULL) || (*string == '\0')) {
	posPtr->site = LEGEND_SITE_RIGHT;
	return TCL_OK;		/* Position marked as default */
    }
    c = string[0];
    length = strlen(string);
    if (c == '@') {
	long x, y;
	int result;
	char *comma;

	string++;
	comma = strchr(string, ',');
	if (comma == NULL) {
	    goto badFormat;
	}
	*comma = '\0';
	result = ((Tcl_ExprLong(interp, string, &x) == TCL_OK) &&
	    (Tcl_ExprLong(interp, comma + 1, &y) == TCL_OK));
	*comma = ',';
	if (!result) {
	    return TCL_ERROR;
	}
	posPtr->x = (int)x, posPtr->y = (int)y;
	posPtr->site = LEGEND_SITE_XY;
    } else if ((c == 'l') && (strncmp(string, "leftmargin", length) == 0)) {
	posPtr->site = LEGEND_SITE_LEFT;
    } else if ((c == 'r') && (strncmp(string, "rightmargin", length) == 0)) {
	posPtr->site = LEGEND_SITE_RIGHT;
    } else if ((c == 't') && (strncmp(string, "topmargin", length) == 0)) {
	posPtr->site = LEGEND_SITE_TOP;
    } else if ((c == 'b') && (strncmp(string, "bottommargin", length) == 0)) {
	posPtr->site = LEGEND_SITE_BOTTOM;
    } else if ((c == 'p') && (strncmp(string, "plotarea", length) == 0)) {
	posPtr->site = LEGEND_SITE_PLOT;
    } else {
      badFormat:
	Tcl_AppendResult(interp, "bad position \"", string, "\": should be  \
\"leftmargin\", \"rightmargin\", \"topmargin\", \"bottommargin\", \
\"plotarea\", or @x,y", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToPosition --
 *
 *	Convert the string representation of a legend XY position into
 *	window coordinates.  The form of the string must be "@x,y" or
 *	none.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToPosition(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to XPoint structure */
{
    LegendPosition *posPtr = (LegendPosition *)(widgRec + offset);

    if (GetLegendPosition(interp, string, posPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

static char *
NameOfPosition(posPtr)
    LegendPosition *posPtr;
{
    static char position[200];

    switch (posPtr->site) {
    case LEGEND_SITE_LEFT:
	return "leftmargin";
    case LEGEND_SITE_RIGHT:
	return "rightmargin";
    case LEGEND_SITE_TOP:
	return "topmargin";
    case LEGEND_SITE_BOTTOM:
	return "bottommargin";
    case LEGEND_SITE_PLOT:
	return "plotarea";
    case LEGEND_SITE_XY:
	sprintf(position, "@%d,%d", posPtr->x, posPtr->y);
	return (position);
    default:
	return "unknown legend position";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PositionToString --
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
PositionToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of XPoint in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    LegendPosition *posPtr = (LegendPosition *)(widgRec + offset);

    return (NameOfPosition(posPtr));
}


static void
SetLegendOrigin(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr = graphPtr->legendPtr;
    int x, y;
    Tk_Anchor anchor;
    int borderWidths;
    int margin;

    x = y = 0;			/* Suppress compiler warning */
    anchor = TK_ANCHOR_CENTER;
    borderWidths = graphPtr->borderWidth + graphPtr->plotBW;
    switch (legendPtr->anchorPos.site) {
    case LEGEND_SITE_RIGHT:
	margin = graphPtr->rightMargin - borderWidths;
	margin -= Blt_GetAxisMargin(&(graphPtr->axisArr[3]));
	x = graphPtr->width - graphPtr->borderWidth - (margin / 2);
	break;
    case LEGEND_SITE_LEFT:
	margin = graphPtr->leftMargin - borderWidths;
	margin -= Blt_GetAxisMargin(&(graphPtr->axisArr[1]));
	x = graphPtr->borderWidth + (margin / 2);
	break;
    case LEGEND_SITE_TOP:
	margin = graphPtr->topMargin - borderWidths;
	margin -= graphPtr->titleAttr.height +
	    Blt_GetAxisMargin(&(graphPtr->axisArr[2]));
	y = graphPtr->borderWidth + (margin / 2);
	y += graphPtr->titleAttr.height;
	break;
    case LEGEND_SITE_BOTTOM:
	margin = graphPtr->bottomMargin - borderWidths;
	margin -= Blt_GetAxisMargin(&(graphPtr->axisArr[0]));
	y = graphPtr->height - graphPtr->borderWidth - (margin / 2);
	break;
    case LEGEND_SITE_PLOT:
    case LEGEND_SITE_XY:
	break;
    }

    switch (legendPtr->anchorPos.site) {
    case LEGEND_SITE_RIGHT:
    case LEGEND_SITE_LEFT:
	switch (legendPtr->anchor) {
	case TK_ANCHOR_N:
	case TK_ANCHOR_NE:
	case TK_ANCHOR_NW:
	    y = graphPtr->yMin;
	    anchor = TK_ANCHOR_N;
	    break;
	case TK_ANCHOR_CENTER:
	case TK_ANCHOR_E:
	case TK_ANCHOR_W:
	    y = (graphPtr->yMax + graphPtr->yMin) / 2;
	    anchor = TK_ANCHOR_CENTER;
	    break;
	case TK_ANCHOR_S:
	case TK_ANCHOR_SE:
	case TK_ANCHOR_SW:
	    y = graphPtr->yMax;
	    anchor = TK_ANCHOR_S;
	    break;
	}
	break;

    case LEGEND_SITE_TOP:
    case LEGEND_SITE_BOTTOM:
	switch (legendPtr->anchor) {
	case TK_ANCHOR_E:
	case TK_ANCHOR_NE:
	case TK_ANCHOR_SE:
	    x = graphPtr->xMax;
	    anchor = TK_ANCHOR_E;
	    break;
	case TK_ANCHOR_CENTER:
	case TK_ANCHOR_N:
	case TK_ANCHOR_S:
	    x = (graphPtr->xMin + graphPtr->xMax) / 2;
	    anchor = TK_ANCHOR_CENTER;
	    break;
	case TK_ANCHOR_W:
	case TK_ANCHOR_SW:
	case TK_ANCHOR_NW:
	    x = graphPtr->xMin;
	    anchor = TK_ANCHOR_W;
	    break;
	}
	break;

    case LEGEND_SITE_PLOT:
	switch (legendPtr->anchor) {
	case TK_ANCHOR_N:
	    x = (graphPtr->xMin + graphPtr->xMax) / 2;
	    y = graphPtr->yMin;
	    break;

	case TK_ANCHOR_NE:
	    x = graphPtr->width - graphPtr->rightMargin;
	    y = graphPtr->yMin;
	    break;

	case TK_ANCHOR_NW:
	    x = graphPtr->leftMargin;
	    y = graphPtr->yMin;
	    break;

	case TK_ANCHOR_CENTER:
	    x = (graphPtr->xMin + graphPtr->xMax) / 2;
	    y = (graphPtr->yMax + graphPtr->yMin) / 2;
	    break;

	case TK_ANCHOR_E:
	    x = graphPtr->width - graphPtr->rightMargin;
	    y = (graphPtr->yMax + graphPtr->yMin) / 2;
	    break;

	case TK_ANCHOR_W:
	    x = graphPtr->leftMargin;
	    y = (graphPtr->yMax + graphPtr->yMin) / 2;
	    break;

	case TK_ANCHOR_S:
	    x = (graphPtr->xMin + graphPtr->xMax) / 2;
	    y = graphPtr->height - graphPtr->bottomMargin;
	    break;

	case TK_ANCHOR_SE:
	    x = graphPtr->width - graphPtr->rightMargin;
	    y = graphPtr->height - graphPtr->bottomMargin;
	    break;

	case TK_ANCHOR_SW:
	    x = graphPtr->leftMargin;
	    y = graphPtr->height - graphPtr->bottomMargin;
	    break;
	}
	anchor = legendPtr->anchor;
	break;

    case LEGEND_SITE_XY:
	if ((graphPtr->width != Tk_Width(graphPtr->tkwin)) ||
	    (graphPtr->height != Tk_Height(graphPtr->tkwin))) {
	    double scale;

	    /*
	     * Legend position was given in window coordinates so we have to
	     * scale using the current page coordinates.
	     */
	    scale = (double)graphPtr->width / Tk_Width(graphPtr->tkwin);
	    x = (int)(legendPtr->anchorPos.x * scale);
	    scale = (double)graphPtr->height / Tk_Height(graphPtr->tkwin);
	    y = (int)(legendPtr->anchorPos.y * scale);
	} else {
	    x = legendPtr->anchorPos.x;
	    y = legendPtr->anchorPos.y;
	}
	/*
	 * If negative coordinates, assume they mean offset from the
	 * opposite side of the window.
	 */
	if (x < 0) {
	    x += graphPtr->width;
	}
	if (y < 0) {
	    y += graphPtr->height;
	}
	anchor = legendPtr->anchor;
	break;
    }
    Blt_TranslateAnchor(x, y, legendPtr->width, legendPtr->height, anchor,
	&x, &y);
    legendPtr->x = x + legendPtr->padLeft;
    legendPtr->y = y + legendPtr->padTop;
}


static ClientData
PickLegendEntry(clientData, x, y)
    ClientData clientData;
    int x, y;			/* Point to be tested */
{
    Graph *graphPtr = (Graph *)clientData;
    Legend *legendPtr;
    int n;
    int width, height;
    int row, column;
    Blt_ListItem item;
    Element *elemPtr;

    legendPtr = graphPtr->legendPtr;
    width = legendPtr->width;
    height = legendPtr->height;
    x -= (legendPtr->x + legendPtr->borderWidth);
    y -= (legendPtr->y + legendPtr->borderWidth);
    width -= (2 * legendPtr->borderWidth + PADDING(legendPtr->padX));
    height -= (2 * legendPtr->borderWidth + PADDING(legendPtr->padY));

    if ((x < 0) || (x >= width) || (y < 0) || (y >= height)) {
	return NULL;
    }
    /*
     * It's in the bounding box, so compute the index.
     */
    row = y / legendPtr->entryAttr.height;
    column = x / legendPtr->entryAttr.width;
    n = (column * legendPtr->numRows) + row;
    if (n >= legendPtr->numEntries) {
	return NULL;
    }
    elemPtr = NULL;
    for (item = Blt_ListFirstItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (elemPtr->label == NULL) {
	    continue;		/* Skip this label */
	}
	if (n == 0) {
	    break;
	}
	n--;
    }
    return (ClientData) elemPtr;
}

static Element *
LocateElement(graphPtr, string)
    Graph *graphPtr;
    char *string;
{
    if (string[0] == '@') {
	int x, y;

	if (Blt_GetXYPosition(graphPtr->interp, graphPtr->tkwin,
		string, &x, &y)
	    != TCL_OK) {
	    return NULL;
	}
	return (Element *) PickLegendEntry(graphPtr, x, y);
    }
    return NULL;
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_LayoutLegend --
 *
 * 	Calculates the dimensions (width and height) needed for
 *	the legend.  Also determines the number of rows and columns
 *	necessary to list all the valid element labels.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *   	The following fields of the legend are calculated and set.
 *
 * 	numEntries   - number of valid labels of elements in the
 *		      display list.
 * 	numRows	    - number of rows of entries
 * 	numCols	    - number of columns of entries
 * 	entryAttr.height - height of each entry
 * 	entryAttr.width  - width of each entry
 * 	height	    - width of legend (includes borders and padding)
 * 	width	    - height of legend (includes borders and padding)
 *
 * -----------------------------------------------------------------
 */
void
Blt_LayoutLegend(graphPtr, maxWidth, maxHeight)
    Graph *graphPtr;
    int maxWidth;		/* Maximum width available in window
				 * to draw the legend. Will calculate number
				 * of columns from this. */
    int maxHeight;		/* Maximum height available in window
				 * to draw the legend. Will calculate number
				 * of rows from this. */
{
    Legend *legendPtr = graphPtr->legendPtr;
    Blt_ListItem item;
    Element *elemPtr;
    int numRows, numCols;
    int maxLabelWidth, maxLabelHeight;
    int numEntries;
    int twiceBW;
    int textWidth, textHeight;
    int symbolWidth;
    Tk_FontMetrics fontMetrics;

    /* Initialize legend values to default (no legend displayed) */

    legendPtr->entryAttr.width = legendPtr->entryAttr.height = 0;
    legendPtr->numRows = legendPtr->numCols = 0;
    legendPtr->numEntries = 0;
    legendPtr->height = legendPtr->width = 0;

    if (legendPtr->hidden) {
	return;			/* Legend is not being displayed */
    }
    /* Determine the number of labels and the width of the width label */

    numEntries = 0;
    maxLabelWidth = maxLabelHeight = 0;
    for (item = Blt_ListFirstItem(&(graphPtr->elemList));
	item != NULL; item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (elemPtr->label == NULL) {
	    continue;		/* Skip this label */
	}
	Blt_GetTextExtents(&(legendPtr->entryAttr), elemPtr->label, &textWidth,
	    &textHeight);
	if (textWidth > maxLabelWidth) {
	    maxLabelWidth = textWidth;
	}
	if (textHeight > maxLabelHeight) {
	    maxLabelHeight = textHeight;
	}
	numEntries++;
    }

    if (numEntries == 0) {
	return;			/* No labels to display in legend */
    }
    /*
     * Calculate the space need to for the legend based upon the size
     * of a label entry and the number of rows and columns needed.
     * Bound the height of the area by *maxHeight* which is the size
     * of the plotting area.
     */
    twiceBW = (2 * legendPtr->entryBorderWidth);

    Tk_GetFontMetrics(legendPtr->entryAttr.font, &fontMetrics);
    symbolWidth = 2 * fontMetrics.ascent;
    legendPtr->numEntries = numEntries;
    legendPtr->entryAttr.height = maxLabelHeight + twiceBW +
	PADDING(legendPtr->ipadY);
    legendPtr->entryAttr.width = maxLabelWidth + symbolWidth + twiceBW +
	PADDING(legendPtr->ipadX) + 5;

    maxHeight -= (2 * legendPtr->borderWidth) + PADDING(legendPtr->padY);
    maxWidth -= (2 * legendPtr->borderWidth) + PADDING(legendPtr->padX);
    numRows = maxHeight / legendPtr->entryAttr.height;
    if (numRows < 1) {
	numRows = 1;
    }
    numCols = maxWidth / legendPtr->entryAttr.width;
    if (numCols < 1) {
	numCols = 1;
    }
    switch (legendPtr->anchorPos.site) {
    case LEGEND_SITE_XY:
    case LEGEND_SITE_PLOT:
    case LEGEND_SITE_LEFT:
    case LEGEND_SITE_RIGHT:
	if (numRows > 0) {
	    numCols = ((numEntries - 1) / numRows) + 1;
	    if (numRows > numEntries) {
		numRows = numEntries;
	    }
	}
	break;

    case LEGEND_SITE_TOP:
    case LEGEND_SITE_BOTTOM:
	if (numCols > 0) {
	    numRows = ((numEntries - 1) / numCols) + 1;
	    if (numCols > numEntries) {
		numCols = numEntries;
	    } else {
		numCols = ((numEntries - 1) / numRows) + 1;
	    }
	}
	break;
    }
    legendPtr->height = (2 * legendPtr->borderWidth) +
	PADDING(legendPtr->padY) + (numRows * legendPtr->entryAttr.height);
    legendPtr->width = (2 * legendPtr->borderWidth) +
	PADDING(legendPtr->padX) + (numCols * legendPtr->entryAttr.width);
    legendPtr->numRows = numRows;
    legendPtr->numCols = numCols;
}

void
Blt_DrawLegend(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    Legend *legendPtr = graphPtr->legendPtr;
    Blt_ListItem item;
    int x, y;
    int width, height;
    int labelX, startY, symbolX, symbolY;
    int counter;
    int symSize, midX, midY;
    int redraw;
    register Element *elemPtr;
    GC fillGC;
    Tk_FontMetrics fontMetrics;

    graphPtr->flags &= ~DRAW_LEGEND;
    if ((legendPtr->hidden) || (legendPtr->numEntries == 0) ||
	(legendPtr->numRows == 0) || (legendPtr->numCols == 0)) {
	return;
    }
    SetLegendOrigin(graphPtr);

    width = legendPtr->width - PADDING(legendPtr->padX);
    height = legendPtr->height - PADDING(legendPtr->padY);
    Tk_GetFontMetrics(legendPtr->entryAttr.font, &fontMetrics);
    symSize = fontMetrics.ascent;
    midX = symSize + 1 + legendPtr->entryBorderWidth;
    midY = (symSize / 2) + 1 + legendPtr->entryBorderWidth;
    labelX = 2 * symSize +
	legendPtr->entryBorderWidth + legendPtr->ipadX.side1 + 5;
    symbolY = midY + legendPtr->ipadY.side1;
    symbolX = midX + legendPtr->ipadX.side1;

    x = legendPtr->x, y = legendPtr->y;

    redraw = FALSE;
    fillGC = legendPtr->fillGC;
    if (legendPtr->fillColor == NULL) {
	fillGC = graphPtr->fillGC;
    }
    if (drawable == None) {
	/*
	 * If there's no pixmap already to draw into, then this routine was
	 * called from Tk_DoWhenIdle instead of DisplayGraph.  Create a
	 * temporary pixmap and reset the x,y coordinates to do a quick
	 * redraw of just the legend.
	 */
	drawable = Tk_GetPixmap(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	    width, height, Tk_Depth(graphPtr->tkwin));

	/* Clear the background of the pixmap */
	if (legendPtr->anchorPos.site > LEGEND_SITE_TOP) {
	    fillGC = graphPtr->plotFillGC;
	} else if (graphPtr->tile != NULL) {
	    fillGC = graphPtr->fillGC;
	    Blt_SetTileOrigin(graphPtr->tkwin, fillGC,
		legendPtr->x, legendPtr->y);
	}
	XFillRectangle(graphPtr->display, drawable, fillGC, 0, 0, width,
	    height);
	x = y = 0;
	redraw = TRUE;
    }
    /*
     * Draw the background of the legend.
     */
    if (legendPtr->fillColor != NULL) {
	XFillRectangle(graphPtr->display, drawable, fillGC, x, y, width,
	    height);
    }
    if (legendPtr->borderWidth > 0) {
	Tk_Draw3DRectangle(graphPtr->tkwin, drawable, graphPtr->border,
	    x, y, width, height, legendPtr->borderWidth, legendPtr->relief);
    }
    x += legendPtr->borderWidth;
    y += legendPtr->borderWidth;

    counter = 0;
    startY = y;
    for (item = Blt_ListFirstItem(&(graphPtr->elemList));
	item != NULL; item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (elemPtr->label == NULL) {
	    continue;		/* Skip this entry */
	}
	if (elemPtr->flags & LABEL_ACTIVE) {
	    legendPtr->entryAttr.state |= STATE_ACTIVE;
	} else {
	    legendPtr->entryAttr.state &= ~STATE_ACTIVE;
	}
	if (legendPtr->entryAttr.state & STATE_ACTIVE) {
	    Tk_Fill3DRectangle(graphPtr->tkwin, drawable,
		legendPtr->activeBorder, x, y,
		legendPtr->entryAttr.width, legendPtr->entryAttr.height,
		legendPtr->entryBorderWidth, elemPtr->labelRelief);
	} else if (elemPtr->labelRelief != TK_RELIEF_FLAT) {
	    Tk_Draw3DRectangle(graphPtr->tkwin, drawable, graphPtr->border,
		x, y, legendPtr->entryAttr.width, legendPtr->entryAttr.height,
		legendPtr->entryBorderWidth, elemPtr->labelRelief);
	}
	(*elemPtr->infoPtr->drawSymbolProc) (graphPtr, drawable, elemPtr,
	    x + symbolX, y + symbolY, symSize);
	Blt_DrawText(graphPtr->tkwin, drawable, elemPtr->label,
	    &(legendPtr->entryAttr), x + labelX,
	    y + legendPtr->entryBorderWidth + legendPtr->ipadY.side1);
	counter++;

	/* Check when to move to the next column */
	if ((counter % legendPtr->numRows) > 0) {
	    y += legendPtr->entryAttr.height;
	} else {
	    x += legendPtr->entryAttr.width;
	    y = startY;
	}
    }
    if (redraw) {
	Blt_DisableCrosshairs(graphPtr);
	XCopyArea(graphPtr->display, drawable, Tk_WindowId(graphPtr->tkwin),
	    graphPtr->drawGC, 0, 0, width, height, legendPtr->x, legendPtr->y);
	Blt_EnableCrosshairs(graphPtr);
	Tk_FreePixmap(graphPtr->display, drawable);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintLegend --
 *
 * -----------------------------------------------------------------
 */
void
Blt_PrintLegend(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    Legend *legendPtr = graphPtr->legendPtr;
    int x, y, startY;
    Element *elemPtr;
    int labelX, symbolX, symbolY;
    int counter;
    Blt_ListItem item;
    int symSize, midX, midY;
    int width, height;
    Tk_FontMetrics fontMetrics;

    if ((legendPtr->hidden) || (legendPtr->numEntries == 0) ||
	(legendPtr->numRows == 0) || (legendPtr->numCols == 0)) {
	return;
    }
    SetLegendOrigin(graphPtr);

    x = legendPtr->x, y = legendPtr->y;
    width = legendPtr->width - PADDING(legendPtr->padX);
    height = legendPtr->height - PADDING(legendPtr->padY);

    if (legendPtr->fillColor != NULL) {
	Blt_ClearBackgroundToPostScript(printable);
	Blt_RectangleToPostScript(printable, x, y, width, height);
    }
    if ((graphPtr->postscript->decorations) && (legendPtr->borderWidth > 0)) {
	Blt_3DRectangleToPostScript(printable, graphPtr->border, x, y, width,
	    height, legendPtr->borderWidth, legendPtr->relief);
    }
    x += legendPtr->borderWidth;
    y += legendPtr->borderWidth;

    Tk_GetFontMetrics(legendPtr->entryAttr.font, &fontMetrics);
    symSize = fontMetrics.ascent;
    midX = symSize + 1 + legendPtr->entryBorderWidth;
    midY = (symSize / 2) + 1 + legendPtr->entryBorderWidth;
    labelX = 2 * symSize + legendPtr->entryBorderWidth +
	legendPtr->ipadX.side1 + 5;
    symbolY = midY + legendPtr->ipadY.side1;
    symbolX = midX + legendPtr->ipadX.side1;

    counter = 0;
    startY = y;
    for (item = Blt_ListFirstItem(&(graphPtr->elemList));
	item != NULL; item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (elemPtr->label == NULL) {
	    continue;		/* Skip this label */
	}
	if (elemPtr->flags & LABEL_ACTIVE) {
	    legendPtr->entryAttr.state |= STATE_ACTIVE;
	} else {
	    legendPtr->entryAttr.state &= ~STATE_ACTIVE;
	}
	if (legendPtr->entryAttr.state & STATE_ACTIVE) {
	    Blt_3DRectangleToPostScript(printable, legendPtr->activeBorder,
		x, y, legendPtr->entryAttr.width, legendPtr->entryAttr.height,
		legendPtr->entryBorderWidth, legendPtr->activeRelief);
	}
	(*elemPtr->infoPtr->printSymbolProc) (graphPtr, printable, elemPtr,
	    x + symbolX, y + symbolY, symSize);
	Blt_PrintText(printable, elemPtr->label, &(legendPtr->entryAttr),
	    x + labelX,
	    y + legendPtr->entryBorderWidth + legendPtr->ipadY.side1);
	counter++;
	if ((counter % legendPtr->numRows) > 0) {
	    y += legendPtr->entryAttr.height;
	} else {
	    x += legendPtr->entryAttr.width;
	    y = startY;
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DrawLegend --
 *
 * -----------------------------------------------------------------
 */
static void
DrawLegend(clientData)
    ClientData clientData;
{
    Graph *graphPtr = (Graph *)clientData;
    Blt_DrawLegend(graphPtr, None);
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureLegend --
 *
 * 	Routine to configure the legend.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static void
ConfigureLegend(graphPtr, legendPtr)
    Graph *graphPtr;
    Legend *legendPtr;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    Blt_ResetTextAttributes(graphPtr->tkwin, &(legendPtr->entryAttr));

    newGC = NULL;
    if (legendPtr->fillColor != NULL) {
	gcMask = GCForeground;
	gcValues.foreground = legendPtr->fillColor->pixel;
	newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
	if (legendPtr->fillGC != NULL) {
	    Tk_FreeGC(graphPtr->display, legendPtr->fillGC);
	}
    }
    legendPtr->fillGC = newGC;
    /*
     *  Update the layout of the graph (and redraw the elements) if
     *  any of the following legend options (all of which affect the
     *	size of the legend) have changed.
     *
     *		-activeborderwidth, -borderwidth
     *		-font
     *		-hide
     *		-ipadx, -ipady, -padx, -pady
     *		-rows
     *
     *  If the position of the legend changed to/from the default
     *  position, also indicate that a new layout is needed.
     *
     */
    if (Blt_ConfigModified(configSpecs, "-*borderwidth", "-*pad?",
	    "-position", "-hide", "-font", "-rows", (char *)NULL)) {
	graphPtr->flags |= COORDS_WORLD;
    }
    graphPtr->flags |= (REDRAW_WORLD | REDRAW_BACKING_STORE);
    Blt_EventuallyRedrawGraph(graphPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DestroyLegend --
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with the legend are freed.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DestroyLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr = graphPtr->legendPtr;

    Tk_FreeOptions(configSpecs, (char *)legendPtr, graphPtr->display, 0);
    Blt_FreeTextAttributes(graphPtr->display, &(legendPtr->entryAttr));
    Blt_DestroyBindingTable(legendPtr->bindTable);
    free((char *)legendPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateLegend --
 *
 * 	Creates and initializes a legend structure with default settings
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
int
Blt_CreateLegend(graphPtr)
    Graph *graphPtr;
{
    Legend *legendPtr;

    legendPtr = (Legend *)calloc(1, sizeof(Legend));
    assert(legendPtr);
    legendPtr->hidden = FALSE;
    legendPtr->anchorPos.x = legendPtr->anchorPos.y = -SHRT_MAX;
    legendPtr->relief = TK_RELIEF_SUNKEN;
    legendPtr->activeRelief = TK_RELIEF_FLAT;
    legendPtr->entryBorderWidth = legendPtr->borderWidth = 2;
    legendPtr->ipadX.side1 = legendPtr->ipadX.side2 = 1;
    legendPtr->ipadY.side1 = legendPtr->ipadY.side2 = 1;
    legendPtr->padX.side1 = legendPtr->padX.side2 = 4;
    legendPtr->padY.side1 = legendPtr->padY.side2 = 4;
    legendPtr->anchor = TK_ANCHOR_N;
    legendPtr->anchorPos.site = LEGEND_SITE_RIGHT;
    graphPtr->legendPtr = legendPtr;
    Blt_InitTextAttributes(&(legendPtr->entryAttr));
    legendPtr->entryAttr.justify = TK_JUSTIFY_LEFT;
    legendPtr->entryAttr.anchor = TK_ANCHOR_NW;
    legendPtr->bindTable = Blt_CreateBindingTable(graphPtr->interp,
	graphPtr->tkwin, graphPtr, PickLegendEntry, Blt_GraphTags);

    if (Blt_ConfigureWidgetComponent(graphPtr->interp, graphPtr->tkwin,
	    "legend", "Legend", configSpecs, 0, (char **)NULL,
	    (char *)legendPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    ConfigureLegend(graphPtr, legendPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOp --
 *
 * 	Find the legend entry from the given argument.  The argument
 *	can be either a screen position "@x,y" or the name of an
 *	element.
 *
 *	I don't know how useful it is to test with the name of an
 *	element.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
GetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char *argv[];
{
    register Element *elemPtr;
    Legend *legendPtr = graphPtr->legendPtr;

    if ((legendPtr->hidden) || (legendPtr->numEntries == 0)) {
	return TCL_OK;
    }
    if ((argv[3][0] == 'c') && (strcmp(argv[3], "current") == 0)) {
	elemPtr = (Element *)Blt_GetCurrentItem(legendPtr->bindTable);
    } else {
	elemPtr = LocateElement(graphPtr, argv[3]);
    }
    if (elemPtr != NULL) {
	Tcl_SetResult(interp, elemPtr->name, TCL_STATIC);
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ActivateOp --
 *
 * 	Activates a particular label in the legend.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static int
ActivateOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    Legend *legendPtr = graphPtr->legendPtr;
    Element *elemPtr;
    unsigned int active, redraw;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    register int i;

    active = (argv[2][0] == 'a') ? LABEL_ACTIVE : 0;
    redraw = 0;
    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	for (i = 3; i < argc; i++) {
	    if (Tcl_StringMatch(elemPtr->name, argv[i])) {
		break;
	    }
	}
	if ((i < argc) &&
	    (active != (elemPtr->flags & LABEL_ACTIVE))) {
	    elemPtr->flags ^= LABEL_ACTIVE;
	    if (elemPtr->label != NULL) {
		redraw++;
	    }
	}
    }
    if ((redraw) && (!legendPtr->hidden)) {
	/*
	 * We need to redraw the legend. If there isn't a redraw already
	 * pending for the whole graph, we can redraw just the legend,
	 * calling the legend's display routine rather than the graph's.
	 * The window must be hidden though.
	 */
	if (legendPtr->anchorPos.site > LEGEND_SITE_TOP) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	if (graphPtr->flags & REDRAW_PENDING) {
	    graphPtr->flags |= REDRAW_WORLD;	/* Upgrade to entire graph */
	} else if (!(graphPtr->flags & DRAW_LEGEND)) {
	    if ((graphPtr->tkwin != NULL) && (Tk_IsMapped(graphPtr->tkwin))) {
		Tk_DoWhenIdle(DrawLegend, (ClientData)graphPtr);
		graphPtr->flags |= DRAW_LEGEND;
	    }
	}
    }
    /* Return the names of all the active legend entries */
    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->elemTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	elemPtr = (Element *)Tcl_GetHashValue(hPtr);
	if (elemPtr->flags & LABEL_ACTIVE) {
	    Tcl_AppendElement(interp, elemPtr->name);
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * BindOp --
 *
 *	  .t bind index sequence command
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BindOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return Blt_ConfigureBindings(interp, graphPtr->legendPtr->bindTable,
	Blt_MakeElementTag(graphPtr, argv[3]), argc - 4, argv + 4);
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOp --
 *
 * 	Queries or resets options for the legend.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return (Tk_ConfigureValue(interp, graphPtr->tkwin, configSpecs,
	    (char *)graphPtr->legendPtr, argv[3], 0));
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 * 	Queries or resets options for the legend.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int flags = TK_CONFIG_ARGV_ONLY;
    Legend *legendPtr;

    legendPtr = graphPtr->legendPtr;
    if (argc == 3) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)legendPtr, (char *)NULL, flags));
    } else if (argc == 4) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)legendPtr, argv[3], flags));
    }
    if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 3,
	    argv + 3, (char *)legendPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    ConfigureLegend(graphPtr, legendPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LegendOp --
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Legend is possibly redrawn.
 *
 *----------------------------------------------------------------------
 */

static Blt_OpSpec legendOps[] =
{
    {"activate", 1, (Blt_Operation)ActivateOp, 3, 0, "?pattern?...",},
    {"bind", 1, (Blt_Operation)BindOp, 4, 6, "elemName sequence command",},
    {"cget", 2, (Blt_Operation)CgetOp, 4, 4, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 3, 0, "?option value?...",},
    {"deactivate", 1, (Blt_Operation)ActivateOp, 3, 0, "?pattern?...",},
    {"get", 1, (Blt_Operation)GetOp, 4, 4, "index",},
};
static int numLegendOps = sizeof(legendOps) / sizeof(Blt_OpSpec);

int
Blt_LegendOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numLegendOps, legendOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (graphPtr, interp, argc, argv);
    return (result);
}
