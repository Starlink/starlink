/*
 * tixDiImgTxt.c --
 *
 *	This file implements one of the "Display Items" in the Tix library :
 *	Image-text display items.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tixPort.h>
#include <tixInt.h>
#include <tixDef.h>

#define DEF_IMAGEITEM_BITMAP	""
#define DEF_IMAGEITEM_IMAGE		""
#define DEF_IMAGEITEM_TYPE		"image"
#define DEF_IMAGEITEM_SHOWIMAGE		"1"
#define DEF_IMAGEITEM_SHOWTEXT		"1"
#define DEF_IMAGEITEM_STYLE		""
#define DEF_IMAGEITEM_TEXT		""
#define DEF_IMAGEITEM_UNDERLINE		"-1"

static Tk_ConfigSpec imageItemConfigSpecs[] = {

    {TK_CONFIG_STRING, "-image", "image", "Image",
       DEF_IMAGEITEM_IMAGE, Tk_Offset(TixImageItem, imageString),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_CUSTOM, "-itemtype", "itemType", "ItemType", 
       DEF_IMAGEITEM_TYPE, Tk_Offset(TixImageItem, diTypePtr),
       0, &tixConfigItemType},

    {TK_CONFIG_CUSTOM, "-style", "imageStyle", "ImageStyle",
       DEF_IMAGEITEM_STYLE, Tk_Offset(TixImageItem, stylePtr),
       TK_CONFIG_NULL_OK, &tixConfigItemStyle},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

/*----------------------------------------------------------------------
 *
 *		Configuration options for Text Styles
 *
 *----------------------------------------------------------------------
 */


#define SELECTED_BG SELECT_BG 
#define DISABLED_BG DISABLED  

#define DEF_IMAGESTYLE_NORMAL_FG_COLOR		BLACK
#define DEF_IMAGESTYLE_NORMAL_FG_MONO		BLACK
#define DEF_IMAGESTYLE_NORMAL_BG_COLOR		NORMAL_BG
#define DEF_IMAGESTYLE_NORMAL_BG_MONO		WHITE

#define DEF_IMAGESTYLE_ACTIVE_FG_COLOR		BLACK
#define DEF_IMAGESTYLE_ACTIVE_FG_MONO		WHITE
#define DEF_IMAGESTYLE_ACTIVE_BG_COLOR		ACTIVE_BG
#define DEF_IMAGESTYLE_ACTIVE_BG_MONO		BLACK

#define DEF_IMAGESTYLE_SELECTED_FG_COLOR	BLACK
#define DEF_IMAGESTYLE_SELECTED_FG_MONO		WHITE
#define DEF_IMAGESTYLE_SELECTED_BG_COLOR	SELECTED_BG
#define DEF_IMAGESTYLE_SELECTED_BG_MONO		BLACK

#define DEF_IMAGESTYLE_DISABLED_FG_COLOR	BLACK
#define DEF_IMAGESTYLE_DISABLED_FG_MONO		BLACK
#define DEF_IMAGESTYLE_DISABLED_BG_COLOR	DISABLED_BG
#define DEF_IMAGESTYLE_DISABLED_BG_MONO		WHITE

#define DEF_IMAGESTYLE_PADX		"0"
#define DEF_IMAGESTYLE_PADY		"0"
#define DEF_IMAGESTYLE_ANCHOR		"w"


static Tk_ConfigSpec imageStyleConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
       DEF_IMAGESTYLE_ANCHOR, Tk_Offset(TixImageStyle, anchor), 0},

    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
       (char *) NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
       (char *) NULL, 0, 0},
 
    {TK_CONFIG_PIXELS, "-padx", "padX", "Pad",
       DEF_IMAGESTYLE_PADX, Tk_Offset(TixImageStyle, pad[0]), 0},

    {TK_CONFIG_PIXELS, "-pady", "padY", "Pad",
       DEF_IMAGESTYLE_PADY, Tk_Offset(TixImageStyle, pad[1]), 0},

/* The following is automatically generated */
	{TK_CONFIG_COLOR,"-background","background","Background",
	DEF_IMAGESTYLE_NORMAL_BG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_NORMAL].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-background","background","Background",
	DEF_IMAGESTYLE_NORMAL_BG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_NORMAL].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-foreground","foreground","Foreground",
	DEF_IMAGESTYLE_NORMAL_FG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_NORMAL].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-foreground","foreground","Foreground",
	DEF_IMAGESTYLE_NORMAL_FG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_NORMAL].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-activebackground","activeBackground","ActiveBackground",
	DEF_IMAGESTYLE_ACTIVE_BG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_ACTIVE].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-activebackground","activeBackground","ActiveBackground",
	DEF_IMAGESTYLE_ACTIVE_BG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_ACTIVE].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-activeforeground","activeForeground","ActiveForeground",
	DEF_IMAGESTYLE_ACTIVE_FG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_ACTIVE].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-activeforeground","activeForeground","ActiveForeground",
	DEF_IMAGESTYLE_ACTIVE_FG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_ACTIVE].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-selectbackground","selectBackground","SelectBackground",
	DEF_IMAGESTYLE_SELECTED_BG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_SELECTED].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-selectbackground","selectBackground","SelectBackground",
	DEF_IMAGESTYLE_SELECTED_BG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_SELECTED].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-selectforeground","selectForeground","SelectForeground",
	DEF_IMAGESTYLE_SELECTED_FG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_SELECTED].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-selectforeground","selectForeground","SelectForeground",
	DEF_IMAGESTYLE_SELECTED_FG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_SELECTED].fg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-disabledbackground","disabledBackground","DisabledBackground",
	DEF_IMAGESTYLE_DISABLED_BG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_DISABLED].bg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-disabledbackground","disabledBackground","DisabledBackground",
	DEF_IMAGESTYLE_DISABLED_BG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_DISABLED].bg),
	TK_CONFIG_MONO_ONLY},
	{TK_CONFIG_COLOR,"-disabledforeground","disabledForeground","DisabledForeground",
	DEF_IMAGESTYLE_DISABLED_FG_COLOR,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_DISABLED].fg),
	TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,"-disabledforeground","disabledForeground","DisabledForeground",
	DEF_IMAGESTYLE_DISABLED_FG_MONO,
	Tk_Offset(TixImageStyle,colors[TIX_DITEM_DISABLED].fg),
	TK_CONFIG_MONO_ONLY},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

/*----------------------------------------------------------------------
 * Forward declarations for procedures defined later in this file:
 *----------------------------------------------------------------------
 */
static void		ImageProc _ANSI_ARGS_((ClientData clientData,
			    int x, int y, int width, int height,
			    int imgWidth, int imgHeight));
static void		Tix_ImageItemCalculateSize  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static char *		Tix_ImageItemComponent	_ANSI_ARGS_((
			    Tix_DItem * iPtr, int x, int y));
static int		Tix_ImageItemConfigure _ANSI_ARGS_((
			    Tix_DItem * iPtr, int argc, char ** argv,
			    int flags));
static Tix_DItem *	Tix_ImageItemCreate _ANSI_ARGS_((
			    Tix_DispData * ddPtr, Tix_DItemInfo * diTypePtr));
static void		Tix_ImageItemDisplay  _ANSI_ARGS_((
			    Pixmap pixmap, GC gc, Tix_DItem * iPtr,
			    int x, int y, int width, int height, int flag));
static void		Tix_ImageItemFree  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static void		Tix_ImageItemLostStyle	_ANSI_ARGS_((
			    Tix_DItem * iPtr));
static void		Tix_ImageItemStyleChanged  _ANSI_ARGS_((
			    Tix_DItem * iPtr));
static int		Tix_ImageStyleConfigure _ANSI_ARGS_((
			    Tix_DItemStyle* style, int argc, char ** argv,
			    int flags));
static Tix_DItemStyle *	Tix_ImageStyleCreate _ANSI_ARGS_((
			    Tcl_Interp *interp, Tk_Window tkwin,
			    Tix_DItemInfo * diTypePtr, char * name));
static void		Tix_ImageStyleFree _ANSI_ARGS_((
			    Tix_DItemStyle* style));
static void		Tix_ImageStyleSetTemplate _ANSI_ARGS_((
			    Tix_DItemStyle* style,
			    Tix_StyleTemplate * tmplPtr));

Tix_DItemInfo tix_ImageItemType = {
    "image",			/* type */
    TIX_DITEM_IMAGE,
    Tix_ImageItemCreate,		/* createProc */
    Tix_ImageItemConfigure,
    Tix_ImageItemCalculateSize,
    Tix_ImageItemComponent,
    Tix_ImageItemDisplay,
    Tix_ImageItemFree,
    Tix_ImageItemStyleChanged,
    Tix_ImageItemLostStyle,

    Tix_ImageStyleCreate,
    Tix_ImageStyleConfigure,
    Tix_ImageStyleFree,
    Tix_ImageStyleSetTemplate,

    imageItemConfigSpecs,
    imageStyleConfigSpecs,
    NULL,				/*next */
};


/*----------------------------------------------------------------------
 * Tix_Image --
 *
 *
 *----------------------------------------------------------------------
 */
static Tix_DItem * Tix_ImageItemCreate(ddPtr, diTypePtr)
    Tix_DispData * ddPtr;
    Tix_DItemInfo * diTypePtr;
{
    TixImageItem * itPtr;

    itPtr = (TixImageItem*) ckalloc(sizeof(TixImageItem));

    itPtr->diTypePtr	= diTypePtr;
    itPtr->ddPtr	= ddPtr;
    itPtr->stylePtr	= NULL;
    itPtr->clientData	= 0;
    itPtr->size[0]	= 0;
    itPtr->size[1]	= 0;

    itPtr->imageString	= NULL;
    itPtr->image	= NULL;
    itPtr->imageW	= 0;
    itPtr->imageH	= 0;

    return (Tix_DItem *)itPtr;
}

static void Tix_ImageItemFree(iPtr)
    Tix_DItem * iPtr;
{
    TixImageItem * itPtr = (TixImageItem *) iPtr;

    if (itPtr->image) {
	Tk_FreeImage(itPtr->image);
    }
    if (itPtr->stylePtr) {
	TixDItemStyleFree(iPtr, (Tix_DItemStyle*)itPtr->stylePtr);
    }

    Tk_FreeOptions(imageItemConfigSpecs, (char *)itPtr,
	itPtr->ddPtr->display, 0);
    ckfree((char*)itPtr);
}

static int Tix_ImageItemConfigure(iPtr, argc, argv, flags)
    Tix_DItem * iPtr;
    int argc;
    char ** argv;
    int flags;
{
    TixImageItem * itPtr = (TixImageItem *) iPtr;
    TixImageStyle * oldStyle = itPtr->stylePtr;

    if (Tk_ConfigureWidget(itPtr->ddPtr->interp, itPtr->ddPtr->tkwin,
	imageItemConfigSpecs,
	argc, argv, (char *)itPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    if (itPtr->stylePtr == NULL) {
	itPtr->stylePtr = (TixImageStyle*)TixGetDefaultDItemStyle(
	    itPtr->ddPtr, &tix_ImageItemType, iPtr, NULL);
    }

    /*
     * Free the old images for the widget, if there were any.
     */
    if (itPtr->image != NULL) {
	Tk_FreeImage(itPtr->image);
	itPtr->image = NULL;
    }

    if (itPtr->imageString != NULL) {
	itPtr->image = Tk_GetImage(itPtr->ddPtr->interp, itPtr->ddPtr->tkwin,
	    itPtr->imageString, ImageProc, (ClientData) itPtr);
	if (itPtr->image == NULL) {
	    return TCL_ERROR;
	}
    }

    if (oldStyle != NULL && itPtr->stylePtr != oldStyle) {
	Tix_ImageItemStyleChanged(iPtr);
    }
    else {
	Tix_ImageItemCalculateSize((Tix_DItem*)itPtr);
    }

    return TCL_OK;
}

static void Tix_ImageItemDisplay(pixmap, gc, iPtr, x, y, width, height, flags)
    Pixmap pixmap;
    GC gc;
    Tix_DItem * iPtr;
    int x;
    int y;
    int width;
    int height;
    int flags;
{
    TixImageItem *itPtr = (TixImageItem *)iPtr;
    GC foreGC, backGC;
    TixpSubRegion subReg;

    if ((width <= 0) || (height <= 0)) {
	return;
    }

    TixGetColorDItemGC(iPtr, &backGC, &foreGC, flags);
    TixpStartSubRegionDraw(itPtr->ddPtr->display, pixmap, foreGC,
	    &subReg, 0, 0, x, y, width, height,
	    itPtr->size[0], itPtr->size[1]);
    TixDItemGetAnchor(itPtr->stylePtr->anchor, x, y, width, height,
	itPtr->size[0], itPtr->size[1], &x, &y);

    if (backGC != None) {
	TixpSubRegFillRectangle(itPtr->ddPtr->display, pixmap,
		backGC, &subReg, x, y, width, height);
    }

    if (itPtr->image != NULL) {
	int bitY;

	bitY = itPtr->size[1] - itPtr->imageH - 2*itPtr->stylePtr->pad[1];

	if (bitY > 0) {
	    bitY = bitY / 2;
	} else {
	    bitY = 0;
	}
	TixpSubRegDrawImage(&subReg, itPtr->image, 0, 0, itPtr->imageW,
		itPtr->imageH, pixmap,
		x + itPtr->stylePtr->pad[0],
		y + itPtr->stylePtr->pad[1] + bitY);
    }

    TixpEndSubRegionDraw(itPtr->ddPtr->display, pixmap, foreGC,
	    &subReg);
}

static void Tix_ImageItemCalculateSize(iPtr)
    Tix_DItem * iPtr;
{
    TixImageItem *itPtr = (TixImageItem *)iPtr;

    itPtr->size[0] = 0;
    itPtr->size[1] = 0;

    if (itPtr->image != NULL) {
	Tk_SizeOfImage(itPtr->image, &itPtr->imageW, &itPtr->imageH);

	itPtr->size[0] = itPtr->imageW;
	itPtr->size[1] = itPtr->imageH;
    }

    itPtr->size[0] += 2*itPtr->stylePtr->pad[0];
    itPtr->size[1] += 2*itPtr->stylePtr->pad[1];
}

static char * Tix_ImageItemComponent(iPtr, x, y)
    Tix_DItem * iPtr;
    int x;
    int y;
{
#if 0
    TixImageItem *itPtr = (TixImageItem *)iPtr;
#endif
    static char * body = "body";

    return body;
}


static void Tix_ImageItemStyleChanged(iPtr)
    Tix_DItem * iPtr;
{
    TixImageItem *itPtr = (TixImageItem *)iPtr;

    if (itPtr->stylePtr == NULL) {
	/* Maybe we haven't set the style to default style yet */
	return;
    }
    Tix_ImageItemCalculateSize(iPtr);
    if (itPtr->ddPtr->sizeChangedProc != NULL) {
	itPtr->ddPtr->sizeChangedProc(iPtr);
    }
}
static void Tix_ImageItemLostStyle(iPtr)
    Tix_DItem * iPtr;
{
    TixImageItem *itPtr = (TixImageItem *)iPtr;

    itPtr->stylePtr = (TixImageStyle*)TixGetDefaultDItemStyle(
	itPtr->ddPtr, &tix_ImageItemType, iPtr, NULL);

    Tix_ImageItemStyleChanged(iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImageProc --
 *
 *	This procedure is invoked by the image code whenever the manager
 *	for an image does something that affects the size of contents
 *	of an image displayed in this widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the HList to get redisplayed.
 *
 *----------------------------------------------------------------------
 */
static void
ImageProc(clientData, x, y, width, height, imgWidth, imgHeight)
    ClientData clientData;		/* Pointer to widget record. */
    int x, y;				/* Upper left pixel (within image)
					 * that must be redisplayed. */
    int width, height;			/* Dimensions of area to redisplay
					 * (may be <= 0). */
    int imgWidth, imgHeight;		/* New dimensions of image. */
{
    TixImageItem *itPtr = (TixImageItem *)clientData;

    Tix_ImageItemCalculateSize((Tix_DItem *)itPtr);
    if (itPtr->ddPtr->sizeChangedProc != NULL) {
	itPtr->ddPtr->sizeChangedProc((Tix_DItem *)itPtr);
    }
}

/*----------------------------------------------------------------------
 *
 *
 *			Display styles
 *
 *
 *----------------------------------------------------------------------
 */
static Tix_DItemStyle *
Tix_ImageStyleCreate(interp, tkwin, diTypePtr, name)
    Tcl_Interp * interp;
    Tk_Window tkwin;
    char * name;
    Tix_DItemInfo * diTypePtr;
{
    int i;
    TixImageStyle * stylePtr =
      (TixImageStyle *)ckalloc(sizeof(TixImageStyle));

    stylePtr->pad[0]	 = 0;
    stylePtr->pad[1]	 = 0;
    stylePtr->anchor	 = TK_ANCHOR_CENTER;

    for (i=0; i<4; i++) {
	stylePtr->colors[i].bg = NULL;
	stylePtr->colors[i].fg = NULL;
	stylePtr->colors[i].backGC = None;
	stylePtr->colors[i].foreGC = NULL;
    }

    return (Tix_DItemStyle *)stylePtr;
}

static int
Tix_ImageStyleConfigure(style, argc, argv, flags)
    Tix_DItemStyle *style;
    int argc;
    char ** argv;
    int flags;
{
    TixImageStyle * stylePtr = (TixImageStyle *)style;
    XGCValues gcValues;
    GC newGC;
    int i;

    if (!(flags &TIX_DONT_CALL_CONFIG)) {
	if (Tk_ConfigureWidget(stylePtr->interp, stylePtr->tkwin,
	    imageStyleConfigSpecs,
	    argc, argv, (char *)stylePtr, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    gcValues.graphics_exposures = False;
    for (i=0; i<4; i++) {
	/* Foreground */
	gcValues.background = stylePtr->colors[i].bg->pixel;
	gcValues.foreground = stylePtr->colors[i].fg->pixel;
	newGC = Tk_GetGC(stylePtr->tkwin,
	    GCForeground|GCBackground|GCGraphicsExposures, &gcValues);

	if (stylePtr->colors[i].foreGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin),
		stylePtr->colors[i].foreGC);
	}
	stylePtr->colors[i].foreGC = newGC;

	/* Background */
	gcValues.foreground = stylePtr->colors[i].bg->pixel;
	newGC = Tk_GetGC(stylePtr->tkwin,
	    GCForeground|GCGraphicsExposures, &gcValues);

	if (stylePtr->colors[i].backGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin),
		stylePtr->colors[i].backGC);
	}
	stylePtr->colors[i].backGC = newGC;
    }

    return TCL_OK;
}

static void Tix_ImageStyleFree(style)
    Tix_DItemStyle *style;
{
    TixImageStyle * stylePtr = (TixImageStyle *)style;
    int i;

    for (i=0; i<4; i++) {
	if (stylePtr->colors[i].backGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin), stylePtr->colors[i].backGC);
	}
	if (stylePtr->colors[i].foreGC != None) {
	    Tk_FreeGC(Tk_Display(stylePtr->tkwin), stylePtr->colors[i].foreGC);
	}
    }

    Tk_FreeOptions(imageStyleConfigSpecs, (char *)stylePtr,
	Tk_Display(stylePtr->tkwin), 0);
    ckfree((char *)stylePtr);
}

static int bg_flags [4] = {
    TIX_DITEM_NORMAL_BG,
    TIX_DITEM_ACTIVE_BG,
    TIX_DITEM_SELECTED_BG,
    TIX_DITEM_DISABLED_BG
};
static int fg_flags [4] = {
    TIX_DITEM_NORMAL_FG,
    TIX_DITEM_ACTIVE_FG,
    TIX_DITEM_SELECTED_FG,
    TIX_DITEM_DISABLED_FG
};

static void
Tix_ImageStyleSetTemplate(style, tmplPtr)
    Tix_DItemStyle* style;
    Tix_StyleTemplate * tmplPtr;
{
    TixImageStyle * stylePtr = (TixImageStyle *)style;
    int i;

    if (tmplPtr->flags & TIX_DITEM_PADX) {
	stylePtr->pad[0] = tmplPtr->pad[0];
    }
    if (tmplPtr->flags & TIX_DITEM_PADY) {
	stylePtr->pad[1] = tmplPtr->pad[1];
    }

    for (i=0; i<4; i++) {
	if (tmplPtr->flags & bg_flags[i]) {
	    if (stylePtr->colors[i].bg != NULL) {
		Tk_FreeColor(stylePtr->colors[i].bg);
	    }
	    stylePtr->colors[i].bg = Tk_GetColor(
		stylePtr->interp, stylePtr->tkwin,
		Tk_NameOfColor(tmplPtr->colors[i].bg));
	}
    }
    for (i=0; i<4; i++) {
	if (tmplPtr->flags & fg_flags[i]) {
	    if (stylePtr->colors[i].fg != NULL) {
		Tk_FreeColor(stylePtr->colors[i].fg);
	    }
	    stylePtr->colors[i].fg = Tk_GetColor(
		stylePtr->interp, stylePtr->tkwin,
		Tk_NameOfColor(tmplPtr->colors[i].fg));
	}
    }

    Tix_ImageStyleConfigure(style, 0, 0, TIX_DONT_CALL_CONFIG);
}
