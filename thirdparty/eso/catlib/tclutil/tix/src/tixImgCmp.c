/* 
 * tkImgCmp.c --
 *
 *	This procedure implements images of type "compound" for Tix.
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

/*
 * ToDo: 
 *	- lineconfig and itemconfig command
 */

/*
 * The following data structure represents the master for a bitmap
 * image:
 */
typedef struct CmpMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter for application that is
				 * using image. */
    Tcl_Command imageCmd;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    Display * display;		/* Display of the the window associated with
				 * this image. We need to keep it
				 * because Tk_Display(CmpMaster.tkwin) may
				 * be invalid. */
    Tk_Window tkwin;		/* default options are taken from this window.
				 * If undefined, will use the main window
				 * of this application */
    int width, height;		/* Dimensions of image. */
    int padX, padY;
    struct CmpLine * lineHead;
    struct CmpLine * lineTail;

    /* Thde default options, etc */
    int borderWidth;		/* Width of 3-D borders. */
    Tk_3DBorder background;	/* Used for drawing background. */
    int relief;			/* Indicates whether window as a whole is
				 * raised, sunken, or flat. */

    TixFont font;		/* Information about text font.*/
    XColor *foreground;		/* Color for drawing text and bitmaps */
    GC gc;			/* default GC for drawing text. */

    int showBackground;		/* whether the background should be drawn */
    unsigned int changing;	/* is this image going to call Tk_ImageChanged
				 * in an idle event? */
    unsigned int isDeleted;
} CmpMaster;

#define TYPE_TEXT	0
#define TYPE_SPACE	1
#define TYPE_IMAGE	2
#define TYPE_BITMAP	3
#define TYPE_WIDGET	4

typedef struct CmpLine {
    struct CmpMaster *masterPtr;
    struct CmpLine * next;
    struct CmpItem * itemHead;
    struct CmpItem * itemTail;
    int padX, padY;
    Tk_Anchor anchor;
    int width, height;		/* Dimensions of this line. */

} CmpLine;

/* abstract type */

#define COMMON_MEMBERS \
    struct CmpLine * line; \
    struct CmpItem * next; \
    Tk_Anchor anchor; \
    char type; \
    int width; \
    int height; \
    int padX, padY

typedef struct CmpItem {
    COMMON_MEMBERS;
} CmpItem;

typedef struct CmpBitmapItem {
    COMMON_MEMBERS;

    Pixmap bitmap;
    XColor *foreground;
    XColor *background;
    GC gc;			/* GC for drawing the bitmap. */
} CmpBitmapItem;

typedef struct CmpImageItem {
    COMMON_MEMBERS;

    Tk_Image image;
    char * imageString;
} CmpImageItem;

typedef struct CmpSpaceItem {
    COMMON_MEMBERS;

} CmpSpaceItem;

typedef struct CmpTextItem {
    COMMON_MEMBERS;

    char * text;
    int numChars;
    Tk_Justify justify;		/* Justification to use for multi-line text. */
    int wrapLength;
    int underline;		/* Index of character to underline.  < 0 means
				 * don't underline anything. */
    XColor *foreground;
    TixFont font;		/* Information about text font, or NULL. */
    GC gc;			/* GC for drawing the bitmap. */
} CmpTextItem;

typedef union CmpItemPtr {
    CmpItem	  * item;
    CmpBitmapItem * bitmap;
    CmpImageItem  * image;
    CmpSpaceItem  * space;
    CmpTextItem  * text;
} CmpItemPtr;

/*
 * The type record for bitmap images:
 */
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
static int		ImgCmpCreate _ANSI_ARGS_((Tcl_Interp *interp,
						  char *name, int argc, 
						  Tcl_Obj *CONST objv[],
						  Tk_ImageType *typePtr, Tk_ImageMaster master,
						  ClientData *clientDataPtr));
#else
static int		ImgCmpCreate _ANSI_ARGS_((Tcl_Interp *interp,
						  char *name, int argc, 
						  char **argv,
						  Tk_ImageType *typePtr, Tk_ImageMaster master,
						  ClientData *clientDataPtr));
#endif

static ClientData	ImgCmpGet _ANSI_ARGS_((Tk_Window tkwin,
			    ClientData clientData));
static void		ImgCmpDisplay _ANSI_ARGS_((ClientData clientData,
			    Display *display, Drawable drawable, 
			    int imageX, int imageY, int width, int height,
			    int drawableX, int drawableY));
static void		ImgCmpFree _ANSI_ARGS_((ClientData clientData,
			    Display *display));
static void		ImgCmpDelete _ANSI_ARGS_((ClientData clientData));

Tk_ImageType tixCompoundImageType = {
    "compound",			/* name */
    ImgCmpCreate,		/* createProc */
    ImgCmpGet,			/* getProc */
    ImgCmpDisplay,		/* displayProc */
    ImgCmpFree,			/* freeProc */
    ImgCmpDelete,		/* deleteProc */

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    (Tk_ImagePostscriptProc *) NULL,    /* postscriptProc */
#endif

    (Tk_ImageType *) NULL	/* nextPtr */
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_BORDER, "-background", "background", "Background",
       DEF_CMPIMAGE_BG_COLOR, Tk_Offset(CmpMaster, background),
       TK_CONFIG_COLOR_ONLY},

    {TK_CONFIG_BORDER, "-background", "background", "Background",
       DEF_CMPIMAGE_BG_MONO, Tk_Offset(CmpMaster, background),
       TK_CONFIG_MONO_ONLY},

    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_PIXELS, "-borderwidth", "borderWidth", (char *) NULL,
       "0", Tk_Offset(CmpMaster, borderWidth), 0},

    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_FONT, "-font", "font", "Font",
       DEF_CMPIMAGE_FONT, Tk_Offset(CmpMaster, font), 0},

    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
       DEF_CMPIMAGE_FG_COLOR, Tk_Offset(CmpMaster, foreground),
       TK_CONFIG_COLOR_ONLY},

    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
       DEF_CMPIMAGE_FG_MONO, Tk_Offset(CmpMaster, foreground),
       TK_CONFIG_MONO_ONLY},

    {TK_CONFIG_PIXELS, "-padx", (char *) NULL, (char *) NULL,
	"0", Tk_Offset(CmpMaster, padX), 0},

    {TK_CONFIG_PIXELS, "-pady", (char *) NULL, (char *) NULL,
	"0", Tk_Offset(CmpMaster, padY), 0},

    {TK_CONFIG_RELIEF, "-relief", (char *) NULL, (char *) NULL,
	"flat", Tk_Offset(CmpMaster, relief), 0},

    {TK_CONFIG_BOOLEAN, "-showbackground", (char *) NULL, (char *) NULL,
	"0", Tk_Offset(CmpMaster, showBackground), 0},

    {TK_CONFIG_WINDOW, "-window", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(CmpMaster, tkwin), TK_CONFIG_NULL_OK},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

static Tk_ConfigSpec lineConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
       "c", Tk_Offset(CmpLine, anchor), 0},
    {TK_CONFIG_PIXELS, "-padx", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpLine, padX), 0},
    {TK_CONFIG_PIXELS, "-pady", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpLine, padY), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

static Tk_ConfigSpec bitmapConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
       "c", Tk_Offset(CmpBitmapItem, anchor), 0},

    {TK_CONFIG_COLOR, "-background", "background", "Background",
       "", Tk_Offset(CmpBitmapItem, background),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_BITMAP, "-bitmap", (char *) NULL, (char *) NULL,
       "",  Tk_Offset(CmpBitmapItem, bitmap), TK_CONFIG_NULL_OK},

    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
       "", Tk_Offset(CmpBitmapItem, foreground),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_PIXELS, "-padx", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpBitmapItem, padX), 0},

    {TK_CONFIG_PIXELS, "-pady", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpBitmapItem, padY), 0},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
        (char *) NULL, 0, 0}
};

static Tk_ConfigSpec imageConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
       "c", Tk_Offset(CmpImageItem, anchor), 0},
    {TK_CONFIG_STRING, "-image", (char *) NULL, (char *) NULL,
       (char *) NULL, Tk_Offset(CmpImageItem, imageString), TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-padx", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpImageItem, padX), 0},
    {TK_CONFIG_PIXELS, "-pady", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpImageItem, padY), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
        (char *) NULL, 0, 0}
};

static Tk_ConfigSpec spaceConfigSpecs[] = {
    {TK_CONFIG_PIXELS, "-height", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpSpaceItem, height), 0},
    {TK_CONFIG_PIXELS, "-width", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpSpaceItem, width), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

static Tk_ConfigSpec textConfigSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
       "c", Tk_Offset(CmpTextItem, anchor), 0},

    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
       (char *) NULL, 0, 0},

    {TK_CONFIG_FONT, "-font", (char *) NULL, (char *) NULL,
       "", Tk_Offset(CmpTextItem, font), TK_CONFIG_NULL_OK},

    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
       "", Tk_Offset(CmpTextItem, foreground),
       TK_CONFIG_NULL_OK},

    {TK_CONFIG_JUSTIFY, "-justify", (char *) NULL, (char *) NULL,
       "left", Tk_Offset(CmpTextItem, justify), 0},

    {TK_CONFIG_PIXELS, "-padx", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpTextItem, padX), 0},

    {TK_CONFIG_PIXELS, "-pady", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpTextItem, padY), 0},

    {TK_CONFIG_STRING, "-text", (char *) NULL, (char *) NULL,
       "",  Tk_Offset(CmpTextItem, text), TK_CONFIG_NULL_OK},

    {TK_CONFIG_INT, "-underline", (char *) NULL, (char *) NULL,
       "-1", Tk_Offset(CmpTextItem, underline), 0},

    {TK_CONFIG_PIXELS, "-wraplength", (char *) NULL, (char *) NULL,
       "0", Tk_Offset(CmpTextItem, wrapLength), 0},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
        (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures used only locally in this file:
 */

static int		ImgCmpCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
static void		ImgCmpCmdDeletedProc _ANSI_ARGS_((
			    ClientData clientData));
static int		ImgCmpConfigureMaster _ANSI_ARGS_((
			    CmpMaster *masterPtr, int argc, char **argv,
			    int flags));
CmpBitmapItem *		AddNewBitmap _ANSI_ARGS_((CmpMaster *masterPtr,
			    CmpLine *line,
			    int argc, char **argv));
CmpImageItem * 		AddNewImage _ANSI_ARGS_((CmpMaster *masterPtr,
			    CmpLine *line,
			    int argc, char **argv));
CmpSpaceItem * 		AddNewSpace _ANSI_ARGS_((CmpMaster *masterPtr,
			    CmpLine *line,
			    int argc, char **argv));
CmpTextItem * 		AddNewText _ANSI_ARGS_((CmpMaster *masterPtr,
			    CmpLine *line,
			    int argc, char **argv));
CmpLine* 		AddNewLine _ANSI_ARGS_((CmpMaster *masterPtr,
			    int argc, char **argv));
static void		CalculateMasterSize _ANSI_ARGS_((
			    ClientData clientData));
static void		ChangeImageWhenIdle _ANSI_ARGS_((
			    CmpMaster *masterPtr));
static void		ImageProc _ANSI_ARGS_((ClientData clientData,
			    int x, int y, int width, int height,
			    int imgWidth, int imgHeight));
static void 		FreeLine _ANSI_ARGS_((CmpLine * lPtr));
static void 		FreeItem _ANSI_ARGS_((CmpItemPtr p));
static void		CmpEventProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpCreate --
 *
 *	This procedure is called by the Tk image code to create "test"
 *	images.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The data structure for a new image is allocated.
 *
 *----------------------------------------------------------------------
 */

/* ARGSUSED */
static int
ImgCmpCreate(interp, name, argc, 
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
	     objv, /* allan: 05/01: added #ifdefs for tk8.3 */
#else
	     argv, 
#endif
	     typePtr, master, clientDataPtr)
    Tcl_Interp *interp;		/* Interpreter for application containing
				 * image. */
    char *name;			/* Name to use for image. */
    int argc;			/* Number of arguments. */
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    Tcl_Obj *CONST objv[];      /* Argument objects for options (not including image name or type) */
#else
    char **argv;		/* Argument strings for options (not including image name or type) */
#endif
    Tk_ImageType *typePtr;	/* Pointer to our type record (not used). */
    Tk_ImageMaster master;	/* Token for image, to be used by us in
				 * later callbacks. */
    ClientData *clientDataPtr;	/* Store manager's token for image here;
				 * it will be returned in later callbacks. */
{
    CmpMaster *masterPtr;

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    /* just generate an argv from the objv argument */
    char* argv[64];  /* there shouldn't be more than a few options... */
    int i;
    for(i = 0; i < argc; i++)
	argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;
#endif

    masterPtr = (CmpMaster *) ckalloc(sizeof(CmpMaster));
    masterPtr->tkMaster = master;
    masterPtr->interp = interp;
    masterPtr->imageCmd = Tcl_CreateCommand(interp, name, ImgCmpCmd,
	(ClientData)masterPtr, ImgCmpCmdDeletedProc);
    masterPtr->tkwin = NULL;
    masterPtr->display = NULL;
    masterPtr->width = 0;
    masterPtr->height = 0;
    masterPtr->padX = 0;
    masterPtr->padY = 0;
    masterPtr->lineHead = NULL;
    masterPtr->lineTail = NULL;
    masterPtr->borderWidth = 0;
    masterPtr->background = NULL;
    masterPtr->relief = 0;
    masterPtr->font = NULL;
    masterPtr->foreground = NULL;
    masterPtr->gc = None;
    masterPtr->showBackground = 0;
    masterPtr->changing = 0;
    masterPtr->isDeleted = 0;
	
    if (ImgCmpConfigureMaster(masterPtr, argc, argv, 0) != TCL_OK) {
	ImgCmpDelete((ClientData) masterPtr);
	return TCL_ERROR;
    }
    *clientDataPtr = (ClientData) masterPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpConfigureMaster --
 *
 *	This procedure is called when a bitmap image is created or
 *	reconfigured.  It process configuration options and resets
 *	any instances of the image.
 *
 * Results:
 *	A standard Tcl return value.  If TCL_ERROR is returned then
 *	an error message is left in masterPtr->interp->result.
 *
 * Side effects:
 *	Existing instances of the image will be redisplayed to match
 *	the new configuration options.
 *
 *----------------------------------------------------------------------
 */
static int
ImgCmpConfigureMaster(masterPtr, argc, argv, flags)
    CmpMaster *masterPtr;	/* Pointer to data structure describing
				 * overall bitmap image to (reconfigure). */
    int argc;			/* Number of entries in argv. */
    char **argv;		/* Pairs of configuration options for image. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget,
				 * such as TK_CONFIG_ARGV_ONLY. */
{
    XGCValues gcValues;
    GC newGC;
    int i;

    if (argc %2) {
	Tcl_AppendResult(masterPtr->interp, "value missing for option \"",
	    argv[argc-1], "\"", NULL);
	return TCL_ERROR;
    }
    for (i=0; i<argc; i+=2) {
	size_t length = strlen(argv[i]);
	if (strncmp(argv[i], "-window", length) == 0) {
	    masterPtr->tkwin = Tk_NameToWindow(masterPtr->interp, argv[i+1],
		Tk_MainWindow(masterPtr->interp));
	    if (masterPtr->tkwin == NULL) {
		return TCL_ERROR;
	    }
	}
    }
    if (masterPtr->tkwin == NULL) {
	Tcl_AppendResult(masterPtr->interp, 
	    "no value given for -window option.", NULL);
	return TCL_ERROR;
    }
    masterPtr->display = Tk_Display(masterPtr->tkwin);

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	    configSpecs, argc, argv, (char *) masterPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }

    Tk_CreateEventHandler(masterPtr->tkwin,
	StructureNotifyMask, CmpEventProc, (ClientData)masterPtr);
    /*
     * Get the default GC for text and bitmaps
     */
    gcValues.foreground = masterPtr->foreground->pixel;
    gcValues.background = Tk_3DBorderColor(masterPtr->background)->pixel;
    gcValues.font = TixFontId(masterPtr->font);
    gcValues.graphics_exposures = False;
    newGC = Tk_GetGC(masterPtr->tkwin,
	 GCBackground|GCForeground|GCFont|GCGraphicsExposures,
	 &gcValues);
    if (masterPtr->gc != None) {
	Tk_FreeGC(Tk_Display(masterPtr->tkwin), masterPtr->gc);
    }
    masterPtr->gc = newGC;

    ChangeImageWhenIdle(masterPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ImgCmpCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to an image managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
ImgCmpCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about button widget. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpMaster *masterPtr = (CmpMaster *) clientData;
    int c, code;
    size_t length;

    if (argc < 2) {
	sprintf(interp->result,
	    "wrong # args: should be \"%.50s option ?arg arg ...?\"",
	    argv[0]);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'a') && (strncmp(argv[1], "add", length) == 0)) {
	if (argc < 3) {
	    return Tix_ArgcError(interp, argc, argv, 2, 
		"type ?option value? ...");
	}
	c = argv[2][0];
	length = strlen(argv[2]);

	if ((c == 'l') && (strncmp(argv[2], "line", length) == 0)) {
	    CmpLine * newLine;

	    newLine = AddNewLine(masterPtr, argc-3, argv+3);
	    if (newLine == NULL) {
		return TCL_ERROR;
	    }
	}
	else {
	    CmpItemPtr p;

	    if (masterPtr->lineTail == 0) {
		if (AddNewLine(masterPtr, 0, 0) == NULL) {
		    return TCL_ERROR;
		}
	    }
	    if ((c == 'b') && (strncmp(argv[2], "bitmap", length) == 0)) {
		p.bitmap = AddNewBitmap(masterPtr, masterPtr->lineTail, 
		    argc-3, argv+3);
		if (p.bitmap == NULL) {
		    return TCL_ERROR;
		}
	    }
	    else if ((c == 'i') && (strncmp(argv[2], "image", length) == 0)) {
		p.image = AddNewImage(masterPtr, masterPtr->lineTail, 
		    argc-3, argv+3);
		if (p.image == NULL) {
		    return TCL_ERROR;
		}
	    }
	    else if ((c == 's') && (strncmp(argv[2], "space", length) == 0)) {
		p.space = AddNewSpace(masterPtr, masterPtr->lineTail, 
		    argc-3, argv+3);
		if (p.space == NULL) {
		    return TCL_ERROR;
		}
	    }
	    else if ((c == 't') && (strncmp(argv[2], "text", length) == 0)) {
		p.text = AddNewText(masterPtr, masterPtr->lineTail, 
		    argc-3, argv+3);
		if (p.text == NULL) {
		    return TCL_ERROR;
		}
	    }
	    else {
		Tcl_AppendResult(interp, "unknown option \"",
		    argv[2], "\", must be bitmap, image, line, ",
		    "space, text or widget", NULL);
		return TCL_ERROR;
	    }

	    /* append to the end of the line */
	    if (masterPtr->lineTail->itemHead == NULL) {
		masterPtr->lineTail->itemHead =  p.item;
		masterPtr->lineTail->itemTail = p.item;
	    } else {
		masterPtr->lineTail->itemTail->next = p.item;
		masterPtr->lineTail->itemTail = p.item;
	    }
	}
	ChangeImageWhenIdle(masterPtr);
	return TCL_OK;
    } else if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	    && (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	return Tk_ConfigureValue(interp, Tk_MainWindow(interp), configSpecs,
		(char *) masterPtr, argv[2], 0);
    } else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	    && (length >= 2)) {
	if (argc == 2) {
	    code = Tk_ConfigureInfo(interp, Tk_MainWindow(interp),
		configSpecs, (char *) masterPtr, (char *) NULL, 0);
	} else if (argc == 3) {
	    code = Tk_ConfigureInfo(interp, Tk_MainWindow(interp),
		configSpecs, (char *) masterPtr, argv[2], 0);
	} else {
	    int i;
	    for (i=2; i<argc-2; i++) {
		length = strlen(argv[i]);
		if (strncmp(argv[i], "-window", length) == 0) {
		    Tcl_AppendResult(interp, "The -window option cannot ",
			"be changed.", (char *) NULL);
		    return TCL_ERROR;
		}
	    }
	    code = ImgCmpConfigureMaster(masterPtr, argc-2, argv+2,
		TK_CONFIG_ARGV_ONLY);
	}
	return code;
    } else if ((c == 'i') && (strncmp(argv[1], "itemconfigure", length)== 0)) {
	Tcl_AppendResult(interp, "unimplemented", NULL);
	return TCL_ERROR;
    } else if ((c == 'l') && (strncmp(argv[1], "lineconfigure", length)== 0)) {
	Tcl_AppendResult(interp, "unimplemented", NULL);
	return TCL_ERROR;
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": must be cget or configure", (char *) NULL);
	return TCL_ERROR;
    }
    /*return TCL_OK; allan: 27.8.01: stmt not reached */
}

/*----------------------------------------------------------------------
 *
 *
 *----------------------------------------------------------------------
 */
CmpLine * 
AddNewLine(masterPtr, argc, argv)
    CmpMaster *masterPtr;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpLine * lPtr = (CmpLine *)ckalloc(sizeof(CmpLine));

    lPtr->masterPtr = masterPtr;
    lPtr->next = NULL;
    lPtr->itemHead = NULL;
    lPtr->itemTail = NULL;
    lPtr->padX   = 0;
    lPtr->padY   = 0;
    lPtr->width  = 1;
    lPtr->height = 1;

    lPtr->anchor = TK_ANCHOR_CENTER;

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	    lineConfigSpecs, argc, argv, (char *) lPtr, 
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	FreeLine(lPtr);
	return NULL;
    }

    /*
     * Append to the end of the master's lines
     */
    if (masterPtr->lineHead == NULL) {
	masterPtr->lineHead = masterPtr->lineTail = lPtr;
    } else {
	masterPtr->lineTail->next = lPtr;
	masterPtr->lineTail = lPtr;
    }

    return lPtr;
}

/*----------------------------------------------------------------------
 *
 *
 *----------------------------------------------------------------------
 */
CmpBitmapItem *
AddNewBitmap(masterPtr, line, argc, argv)
    CmpMaster *masterPtr;
    CmpLine *line;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpItemPtr p;
    XGCValues gcValues;

    p.bitmap = (CmpBitmapItem*) ckalloc(sizeof(CmpBitmapItem));
    p.bitmap->line = line;
    p.bitmap->next = NULL;
    p.bitmap->anchor = TK_ANCHOR_CENTER;
    p.bitmap->type = TYPE_BITMAP;
    p.bitmap->padX = 0;
    p.bitmap->padY = 0;
    p.bitmap->width = 0;
    p.bitmap->height = 0;
   
    p.bitmap->bitmap = None;
    p.bitmap->foreground = NULL;
    p.bitmap->background = NULL;
    p.bitmap->gc = None;

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	    bitmapConfigSpecs, argc, argv, (char *) p.bitmap, 
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	goto error;
    }

    /* Get the GC for the bitmap */
    if (p.bitmap->background) {
	gcValues.background = p.bitmap->background->pixel;
    } else {
	gcValues.background = Tk_3DBorderColor(masterPtr->background)->pixel;
    }
    if (p.bitmap->foreground) {
	gcValues.foreground = p.bitmap->foreground->pixel;
    } else {
	gcValues.foreground = masterPtr->foreground->pixel;
    }
    gcValues.graphics_exposures = False;
    p.bitmap->gc = Tk_GetGC(masterPtr->tkwin,
	 GCBackground|GCForeground|GCGraphicsExposures,
	 &gcValues);

    return p.bitmap;

  error:

    FreeItem(p);
    return NULL;
}

/*----------------------------------------------------------------------
 *
 *
 *----------------------------------------------------------------------
 */
CmpImageItem *
AddNewImage(masterPtr, line, argc, argv)
    CmpMaster *masterPtr;
    CmpLine *line;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpItemPtr p;


    p.image = (CmpImageItem*) ckalloc(sizeof(CmpImageItem));
    p.image->line = line;
    p.image->next = NULL;
    p.image->anchor = TK_ANCHOR_CENTER;
    p.image->type = TYPE_IMAGE;
    p.image->padX = 0;
    p.image->padY = 0;
    p.image->width = 0;
    p.image->height = 0;
   
    p.image->imageString = NULL;
    p.image->image = NULL;

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	    imageConfigSpecs, argc, argv, (char *) p.image, 
	    TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	goto error;
    }

    /* Get the image */
    if (p.image->imageString != NULL) {
	p.image->image = Tk_GetImage(masterPtr->interp, masterPtr->tkwin,
	    p.image->imageString, ImageProc, (ClientData)p.image);
	if (p.image->image == NULL) {
	    goto error;
	}
    }

    return p.image;

  error:

    FreeItem(p);
    return NULL;
}

/*----------------------------------------------------------------------
 *
 *
 *----------------------------------------------------------------------
 */
CmpSpaceItem *
AddNewSpace(masterPtr, line, argc, argv)
    CmpMaster *masterPtr;
    CmpLine *line;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpItemPtr p;

    p.space = (CmpSpaceItem*) ckalloc(sizeof(CmpSpaceItem));
    p.space->line = line;
    p.space->next = NULL;
    p.space->anchor = TK_ANCHOR_CENTER;
    p.space->type = TYPE_SPACE;
    p.space->padX = 0;
    p.space->padY = 0;
    p.space->width = 0;
    p.space->height = 0;

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	spaceConfigSpecs, argc, argv, (char *)p.space, 
	TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	goto error;
    }

    return p.space;

  error:

    FreeItem(p);
    return NULL;
}

/*----------------------------------------------------------------------
 *
 *
 *----------------------------------------------------------------------
 */
CmpTextItem *
AddNewText(masterPtr, line, argc, argv)
    CmpMaster *masterPtr;
    CmpLine *line;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    CmpItemPtr p;
    XGCValues gcValues;

    p.text = (CmpTextItem*) ckalloc(sizeof(CmpTextItem));
    p.text->line = line;
    p.text->next = NULL;
    p.text->anchor = TK_ANCHOR_CENTER;
    p.text->type = TYPE_TEXT;
    p.text->padX = 0;
    p.text->padY = 0;
    p.text->width = 0;
    p.text->height = 0;
   
    p.text->text = NULL;
    p.text->numChars = 0;
    p.text->justify = TK_JUSTIFY_CENTER;
    p.text->underline = -1;
    p.text->wrapLength = 0;

    p.text->foreground = NULL;
    p.text->font = NULL;
    p.text->gc = None;

    if (Tk_ConfigureWidget(masterPtr->interp, masterPtr->tkwin,
	textConfigSpecs, argc, argv, (char *) p.text, 
	TK_CONFIG_ARGV_ONLY) != TCL_OK) {

	goto error;
    }

    /* Get the GC for the text */
    if (p.text->foreground) {
	gcValues.foreground = p.text->foreground->pixel;
    } else {
	gcValues.foreground = masterPtr->foreground->pixel;
    }
    if (p.text->font) {
	gcValues.font = TixFontId(p.text->font);
    } else {
	gcValues.font = TixFontId(masterPtr->font);
    }
    gcValues.graphics_exposures = False;
    p.text->gc = Tk_GetGC(masterPtr->tkwin,
	 GCFont|GCForeground|GCGraphicsExposures,
	 &gcValues);

    return p.text;

  error:

    FreeItem(p);
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpGet --
 *
 *	This procedure is called for each use of a bitmap image in a
 *	widget.
 *
 * Results:
 *	The return value is a token for the instance, which is passed
 *	back to us in calls to ImgCmpDisplay and ImgCmpFree.
 *
 * Side effects:
 *	A data structure is set up for the instance (or, an existing
 *	instance is re-used for the new one).
 *
 *----------------------------------------------------------------------
 */
static ClientData
ImgCmpGet(tkwin, masterData)
    Tk_Window tkwin;		/* Window in which the instance will be
				 * used. */
    ClientData masterData;	/* Pointer to our master structure for the
				 * image. */
{
    CmpMaster *masterPtr = (CmpMaster *)masterData;
    
    if (tkwin == masterPtr->tkwin) {
	return masterData;
    }

    Tcl_AppendResult(masterPtr->interp, 
	"Image \"",
	Tk_NameOfImage(masterPtr->tkMaster),
	"\" can only be assigned to window \"",
	Tk_PathName(masterPtr->tkwin), "\"", NULL);
    Tcl_AddErrorInfo(masterPtr->interp, "\n    (while configuring image \"");
    Tcl_AddErrorInfo(masterPtr->interp, Tk_NameOfImage(masterPtr->tkMaster));
    Tcl_AddErrorInfo(masterPtr->interp, "\")");
    Tk_BackgroundError(masterPtr->interp);

    return NULL;
}

static void
CalculateMasterSize(clientData)
    ClientData clientData;
{
    CmpMaster *masterPtr = (CmpMaster *)clientData;
    CmpLine *lPtr;
    CmpItemPtr p;

    masterPtr->width  = 0;
    masterPtr->height = 0;

    for (lPtr = masterPtr->lineHead; lPtr; lPtr=lPtr->next) {
	lPtr->width  = 0;
	lPtr->height = 0;
	for (p.item = lPtr->itemHead; p.item; p.item=p.item->next) {

	    switch (p.item->type) {
	      case TYPE_IMAGE:
		Tk_SizeOfImage(p.image->image, 
		    &p.image->width, &p.image->height);
		break;

	      case TYPE_SPACE:
		/* Do nothing */
		break;

	      case TYPE_TEXT:
		{
		    TixFont font;

		    if (p.text->text == NULL) {
			break;
		    }
		  
		    if (p.text->font) {
			font = p.text->font;
		    } else {
			font = masterPtr->font;
		    }
		    
		    p.text->numChars = strlen(p.text->text);
		    TixComputeTextGeometry(font, p.text->text,
			p.text->numChars,
			p.text->wrapLength,
			&p.text->width, &p.text->height);
	        }
		break;
		
	      case TYPE_BITMAP:
		Tk_SizeOfBitmap(Tk_Display(masterPtr->tkwin),
		    p.bitmap->bitmap, &p.bitmap->width,
		    &p.bitmap->height);
		break;

	      case TYPE_WIDGET:

		
		break;
	    }
	    p.item->width  += 2*p.item->padX;
	    p.item->height += 2*p.item->padY;

	    lPtr->width += p.item->width;
	    if (lPtr->height < p.item->height) {
		lPtr->height = p.item->height;
	    }
	}
	lPtr->width  += 2*lPtr->padX;
	lPtr->height += 2*lPtr->padY;

	if (masterPtr->width < lPtr->width) {
	    masterPtr->width = lPtr->width;
	}
	masterPtr->height += lPtr->height;
    }
    masterPtr->width  += 2*masterPtr->padX + 2*masterPtr->borderWidth;
    masterPtr->height += 2*masterPtr->padY + 2*masterPtr->borderWidth;

    Tk_ImageChanged(masterPtr->tkMaster, 0, 0, masterPtr->width,
	masterPtr->height, masterPtr->width, masterPtr->height);
    masterPtr->changing = 0;
}

static void
ChangeImageWhenIdle(masterPtr)
    CmpMaster *masterPtr;
{
    if (!masterPtr->changing) {
	masterPtr->changing = 1;
	Tk_DoWhenIdle(CalculateMasterSize, (ClientData)masterPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpDisplay --
 *
 *	This procedure is invoked to draw a bitmap image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A portion of the image gets rendered in a pixmap or window.
 *
 *----------------------------------------------------------------------
 */
static void
ImgCmpDisplay(clientData, display, drawable, imageX, imageY, width,
	height, drawableX, drawableY)
    ClientData clientData;	/* Pointer to CmpInstance structure for
				 * for instance to be displayed. */
    Display *display;		/* Display on which to draw image. */
    Drawable drawable;		/* Pixmap or window in which to draw image. */
    int imageX, imageY;		/* Upper-left corner of region within image
				 * to draw. */
    int width, height;		/* Dimensions of region within image to draw.*/
    int drawableX, drawableY;	/* Coordinates within drawable that
				 * correspond to imageX and imageY. */
{
    CmpMaster * masterPtr = (CmpMaster*)clientData;
    CmpLine *lPtr;
    CmpItemPtr p;
    int dx, dy, extraX;

    if (masterPtr == NULL) {
	/* attempting to draw into a invalid window (can only be drawn into
	 * the original window set by the -window option */
	return;
    }

    if (masterPtr->showBackground) {
	Tk_Fill3DRectangle(masterPtr->tkwin, drawable, 
	    masterPtr->background,
	    drawableX + masterPtr->padX - imageX,
	    drawableY + masterPtr->padY - imageY,
	    masterPtr->width  - 2*masterPtr->padX,
	    masterPtr->height - 2*masterPtr->padY,
	    masterPtr->borderWidth, masterPtr->relief);
    }

    /* ToDo: Set the clipping region according to the imageX,Y, and 
     * width, height */
    dy = drawableY + masterPtr->padY + masterPtr->borderWidth - imageY;

    for (lPtr = masterPtr->lineHead; lPtr; lPtr=lPtr->next) {
	dx = drawableX + masterPtr->padX - imageX;
	dx += lPtr->padX;
	dy += lPtr->padY;

	extraX = masterPtr->width - 2*masterPtr->padX - lPtr->width;
	switch (lPtr->anchor) {
	  case TK_ANCHOR_SW: case TK_ANCHOR_W: case TK_ANCHOR_NW:
	    extraX = 0;
	    break;
	  case TK_ANCHOR_N: case TK_ANCHOR_CENTER: case TK_ANCHOR_S:
	    extraX /= 2;
	    break;
	  case TK_ANCHOR_SE: case TK_ANCHOR_E: case TK_ANCHOR_NE:
	    break;
	}
	dx += extraX;

	for (p.item = lPtr->itemHead; p.item; p.item=p.item->next) {
	    int extraY;
	    dx += p.item->padX;

	    extraY = lPtr->height - 2*lPtr->padY - p.item->height;
	    switch (p.item->anchor) {
	      case TK_ANCHOR_SW: case TK_ANCHOR_S: case TK_ANCHOR_SE:
		break;
	      case TK_ANCHOR_W: case TK_ANCHOR_CENTER: case TK_ANCHOR_E:
		extraY /= 2;
		break;
	      case TK_ANCHOR_NW: case TK_ANCHOR_N: case TK_ANCHOR_NE:
		extraY = 0;
		break;
	    }

	    switch (p.item->type) {
	      case TYPE_IMAGE:
		Tk_RedrawImage(p.image->image, 0, 0, 
		    p.image->width  - 2*p.item->padX,
		    p.image->height - 2*p.item->padY,
		    drawable, dx, dy+extraY);
		break;

	      case TYPE_SPACE:
		/* Do nothing */
		break;

	      case TYPE_TEXT:
		{
		    TixFont font;

		    if (p.text->text == NULL) {
			break;
		    }
		  
		    if (p.text->font) {
			font = p.text->font;
		    } else {
			font = masterPtr->font;
		    }

		    TixDisplayText(Tk_Display(masterPtr->tkwin), drawable,
			font, p.text->text, p.text->numChars,
			dx, dy+extraY,	    
			p.text->width - 2*p.item->padX,
			p.text->justify,
			p.text->underline,
			p.text->gc);
	        }
		break;
		
	      case TYPE_BITMAP:
		XCopyPlane(Tk_Display(masterPtr->tkwin), p.bitmap->bitmap,
		    drawable, p.bitmap->gc, 0, 0,
		    p.bitmap->width  - 2*p.item->padX,
		    p.bitmap->height - 2*p.item->padY,
		    dx, dy+extraY, 1);

		break;

	      case TYPE_WIDGET:

		
		break;
	    }
	    dx += p.item->width - p.item->padX;
	}
	dy += lPtr->height - lPtr->padY;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpFree --
 *
 *	This procedure is called when a widget ceases to use a
 *	particular instance of an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Internal data structures get cleaned up.
 *
 *----------------------------------------------------------------------
 */

static void
ImgCmpFree(clientData, display)
    ClientData clientData;	/* Pointer to CmpInstance structure for
				 * for instance to be displayed. */
    Display *display;		/* Display containing window that used image.*/
{
    /*
     * Since one compound image can only be used in one window, when that 
     * window is deleted, this image is now useless and should be deleted as
     * well
     */
}

static void FreeLine(lPtr)
    CmpLine * lPtr;
{
    Tk_FreeOptions(lineConfigSpecs, (char *)lPtr,
	Tk_Display(lPtr->masterPtr->tkwin), 0);
    ckfree((char *) lPtr);
}

static void FreeItem(p)
    CmpItemPtr p;
{
    switch (p.item->type) {
      case TYPE_IMAGE:
	if (p.image->image) {
	    Tk_FreeImage(p.image->image);
	}
	Tk_FreeOptions(imageConfigSpecs, (char *)p.image, 
	    Tk_Display(p.item->line->masterPtr->tkwin), 0);
	break;

      case TYPE_SPACE:
	Tk_FreeOptions(spaceConfigSpecs, (char *)p.space, 
	    Tk_Display(p.item->line->masterPtr->tkwin), 0);
	break;

      case TYPE_TEXT:
	if (p.text->gc != None) {
	    Tk_FreeGC(Tk_Display(p.text->line->masterPtr->tkwin),
		p.text->gc);
	}
	Tk_FreeOptions(textConfigSpecs, (char *)p.text, 
	    Tk_Display(p.item->line->masterPtr->tkwin), 0);
	break;

      case TYPE_BITMAP:
	if (p.bitmap->gc != None) {
	    Tk_FreeGC(Tk_Display(p.bitmap->line->masterPtr->tkwin),
		p.bitmap->gc);
	}
	Tk_FreeOptions(bitmapConfigSpecs, (char *)p.bitmap, 
	    Tk_Display(p.item->line->masterPtr->tkwin), 0);
	break;

      case TYPE_WIDGET:
	break;
    }
    ckfree((char *) p.item);
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpDelete --
 *
 *	This procedure is called by the image code to delete the
 *	master structure for an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with the image get freed.
 *
 *----------------------------------------------------------------------
 */
static void
ImgCmpDelete(masterData)
    ClientData masterData;	/* Pointer to CmpMaster structure for
				 * image.  Must not have any more instances. */
{
    CmpMaster *masterPtr = (CmpMaster *) masterData;
    CmpLine * lPtr;
    CmpItemPtr p;

    if (masterPtr->tkwin == NULL) {
	goto done;
    }

    Tk_Preserve((ClientData) masterPtr);

    Tk_DeleteEventHandler(masterPtr->tkwin,
	StructureNotifyMask, CmpEventProc, (ClientData)masterPtr);

    if (masterPtr->isDeleted) {
	Tk_Release((ClientData) masterPtr);
	return;
    }
    masterPtr->isDeleted = 1;

    for (lPtr=masterPtr->lineHead; lPtr;) {
	CmpLine * toDelete = lPtr;
	lPtr = lPtr->next;

	for (p.item=toDelete->itemHead; p.item;) {
	    CmpItemPtr toDelete;

	    toDelete.item = p.item;
	    p.item=p.item->next;

	    FreeItem(toDelete);
	}
	FreeLine(toDelete);
    }

    if (masterPtr->changing) {
	Tk_CancelIdleCall(CalculateMasterSize, (ClientData)masterPtr);
    }
    masterPtr->tkMaster = NULL;
    if (masterPtr->imageCmd != NULL) {
	char * cmd = (char*)Tcl_GetCommandName(masterPtr->interp,masterPtr->imageCmd);
	masterPtr->imageCmd = NULL;
	Tcl_DeleteCommand(masterPtr->interp, cmd);
    }
    if (masterPtr->gc != None) {
	Tk_FreeGC(masterPtr->display, masterPtr->gc);
    }

    Tk_FreeOptions(configSpecs, (char *) masterPtr, masterPtr->display, 0);
    Tk_Release((ClientData) masterPtr);

  done:
    ckfree((char *) masterPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImgCmpCmdDeletedProc --
 *
 *	This procedure is invoked when the image command for an image
 *	is deleted.  It deletes the image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The image is deleted.
 *
 *----------------------------------------------------------------------
 */
static void
ImgCmpCmdDeletedProc(clientData)
    ClientData clientData;	/* Pointer to CmpMaster structure for
				 * image. */
{
    CmpMaster *masterPtr = (CmpMaster *) clientData;

    Tk_Preserve((ClientData) masterPtr);
    if (masterPtr->isDeleted == 1) {
	Tk_Release((ClientData) masterPtr);
	return;
    } else {
	if (masterPtr->tkMaster != NULL) {
	    if (Tk_MainWindow(masterPtr->interp) != NULL) {
		Tk_DeleteImage(masterPtr->interp, 
		    Tk_NameOfImage(masterPtr->tkMaster));
	    }
	}
	Tk_Release((ClientData) masterPtr);
    }
}
/*
 *----------------------------------------------------------------------
 *
 * ImageProc --
 *
 *	This procedure is invoked by the image code whenever the manager
 *	for an image does something that affects the size of contents
 *	of an image displayed in a button.
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
    CmpItemPtr p;

    p.image = (CmpImageItem *)clientData;

    ChangeImageWhenIdle(p.item->line->masterPtr);
}

/*
 *--------------------------------------------------------------
 *
 * CmpEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for various
 *	events on the window that employs this compound image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, it is redisplayed.
 *
 *--------------------------------------------------------------
 */

static void
CmpEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    CmpMaster *masterPtr = (CmpMaster *)clientData;
    char * cmd;

    if (eventPtr->type == DestroyNotify) {
	if (masterPtr->imageCmd != NULL) {
	    cmd = (char*)Tcl_GetCommandName(masterPtr->interp,masterPtr->imageCmd);
	    masterPtr->imageCmd = NULL;
	    Tcl_DeleteCommand(masterPtr->interp, cmd);
	}
    }
}
