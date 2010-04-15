/* This file was automatically generated.  Do not edit! */
#define HtmlAlloc(A)      ((void*)Tcl_Alloc(A))
#define HtmlFree(A)       Tcl_Free((char*)(A))
typedef struct HtmlWidget HtmlWidget;
int HtmlUnlock(HtmlWidget *htmlPtr);
char *HtmlResolveUri(HtmlWidget *htmlPtr,char *zUri);
void HtmlLock(HtmlWidget *htmlPtr);
void HtmlTPCantHappen(const char *zFile,int line);
#if defined(COVERAGE_TEST)
# define CANT_HAPPEN       HtmlTPCantHappen(__FILE__,__LINE__)
#endif
#if !(defined(COVERAGE_TEST))
# define CANT_HAPPEN
#endif
#define Html_IMG             76
typedef struct HtmlImage HtmlImage;
typedef union HtmlElement HtmlElement;
HtmlImage *HtmlGetImage(HtmlWidget *htmlPtr,HtmlElement *p);
void HtmlAppendArglist(Tcl_DString *str,HtmlElement *pElem);
void HtmlScheduleRedraw(HtmlWidget *htmlPtr);
#define REDRAW_IMAGES        0x002000
void HtmlRedrawEverything(HtmlWidget *htmlPtr);
#define RELAYOUT             0x000010
typedef struct HtmlBlock HtmlBlock;
typedef struct HtmlIndex HtmlIndex;
struct HtmlIndex {
  HtmlElement *p;      /* The token containing the character */
  int i;               /* Index of the character */
};
typedef short Html_16;
typedef struct HtmlScript HtmlScript;
#define Html_TypeCount       151
typedef struct HtmlStyleStack HtmlStyleStack;
typedef struct HtmlLayoutContext HtmlLayoutContext;
typedef struct HtmlMargin HtmlMargin;
struct HtmlLayoutContext {
  HtmlWidget *htmlPtr;          /* The html widget undergoing layout */
  HtmlElement *pStart;          /* Start of elements to layout */
  HtmlElement *pEnd;            /* Stop when reaching this element */
  int headRoom;                 /* Extra space wanted above this line */
  int top;                      /* Absolute top of drawing area */
  int bottom;                   /* Bottom of previous line */
  int left, right;              /* Left and right extremes of drawing area */
  int pageWidth;                /* Width of the layout field, including
                                ** the margins */
  int maxX, maxY;               /* Maximum X and Y values of paint */
  HtmlMargin *leftMargin;       /* Stack of left margins */
  HtmlMargin *rightMargin;      /* Stack of right margins */
};
#define N_FONT_FAMILY     8
#define N_FONT_SIZE       7
#define N_FONT            (N_FONT_FAMILY*N_FONT_SIZE)
#define N_COLOR             16      /* Total number of colors */
typedef struct GcCache GcCache;
typedef unsigned char Html_u8;
struct GcCache {
  GC gc;                /* The graphics context */
  Html_u8 font;         /* Font used for this context */
  Html_u8 color;        /* Color used for this context */
  Html_u8 index;        /* Index used for LRU replacement */
};
#define N_CACHE_GC 16
struct HtmlWidget {
  Tk_Window tkwin;              /* The main window for this widget */
  Tk_Window clipwin;            /* The clipping window in which all text is
                                ** rendered. */
  char *zClipwin;               /* Name of the clipping window. */
  Display *display;             /* The X11 Server that contains tkwin */
  Tcl_Interp *interp;           /* The interpreter in which the widget lives */
  char *zCmdName;               /* Name of the command */
  HtmlElement *pFirst;          /* First HTML token on a list of them all */
  HtmlElement *pLast;           /* Last HTML token on the list */
  int nToken;                   /* Number of HTML tokens on the list.
                                 * Html_Block tokens don't count. */
  HtmlElement *lastSized;       /* Last HTML element that has been sized */
  HtmlElement *nextPlaced;      /* Next HTML element that needs to be
                                 * positioned on canvas. */
  HtmlBlock *firstBlock;        /* List of all HtmlBlock tokens */
  HtmlBlock *lastBlock;         /* Last HtmlBlock in the list */
  HtmlElement *firstInput;      /* First <INPUT> element */
  HtmlElement *lastInput;       /* Last <INPUT> element */
  int nInput;                   /* The number of <INPUT> elements */
  int nForm;                    /* The number of <FORM> elements */
  int varId;                    /* Used to construct a unique name for a
                                ** global array used by <INPUT> elements */

  /*
   * Information about the selected region of text
   */
  HtmlIndex selBegin;           /* Start of the selection */
  HtmlIndex selEnd;             /* End of the selection */
  HtmlBlock *pSelStartBlock;    /* Block in which selection starts */
  Html_16 selStartIndex;        /* Index in pSelStartBlock of first selected
                                 * character */
  Html_16 selEndIndex;          /* Index of last selecte char in pSelEndBlock */
  HtmlBlock *pSelEndBlock;      /* Block in which selection ends */

  /*
   * Information about the insertion cursor
   */
  int insOnTime;                /* How long the cursor states one (millisec) */
  int insOffTime;               /* How long it is off (milliseconds) */
  int insStatus;                /* Is it visible? */
  Tcl_TimerToken insTimer;      /* Timer used to flash the insertion cursor */
  HtmlIndex ins;                /* The insertion cursor position */
  HtmlBlock *pInsBlock;         /* The HtmlBlock containing the cursor */
  int insIndex;                 /* Index in pInsBlock of the cursor */

  /*
   * The following fields hold state information used by
   * the tokenizer.
   */
  char *zText;                  /* Complete text of the unparsed HTML */
  int nText;                    /* Number of characters in zText */
  int nAlloc;                   /* Space allocated for zText */
  int nComplete;                /* How much of zText has actually been
                                 * converted into tokens */
  int iCol;                     /* The column in which zText[nComplete]
                                 * occurs.  Used to resolve tabs in input */
  int iPlaintext;               /* If not zero, this is the token type that
                                 * caused us to go into plaintext mode.  One
                                 * of Html_PLAINTEXT, Html_LISTING or
                                 * Html_XMP */
  HtmlScript *pScript;            /* <SCRIPT> currently being parsed */
  char *zHandler[Html_TypeCount]; /* If not NULL, this is a TCL routine that
                                 * is used to process tokens of the given
                                 * type */
  /*
   * These fields hold state information used by the HtmlAddStyle routine.
   * We have to store this state information here since HtmlAddStyle
   * operates incrementally.  This information must be carried from
   * one incremental execution to the next.
   */
  HtmlStyleStack *styleStack;   /* The style stack */
  int paraAlignment;            /* Justification associated with <p> */
  int rowAlignment;             /* Justification associated with <tr> */
  int anchorFlags;              /* Style flags associated with <A>...</A> */
  int inDt;                     /* Style flags associated with <DT>...</DT> */
  int inTr;                     /* True if within <tr>..</tr> */
  int inTd;                     /* True if within <td>..</td> or <th>..</th> */
  HtmlElement *anchorStart;     /* Most recent <a href=...> */
  HtmlElement *formStart;       /* Most recent <form> */
  HtmlElement *formElemStart;   /* Most recent <textarea> or <select> */
  HtmlElement *innerList;       /* The inner most <OL> or <UL> */

  /*
   * These fields are used to hold the state of the layout engine.
   * Because the layout is incremental, this state must be held for
   * the life of the widget.
   */
  HtmlLayoutContext layoutContext;

  /*
   * Information used when displaying the widget:
   */
  Tk_3DBorder border;		/* Background color */
  int borderWidth;		/* Width of the border. */
  int relief;			/* 3-D effect: TK_RELIEF_RAISED, etc. */
  int highlightWidth;		/* Width in pixels of highlight to draw
				 * around widget when it has the focus.
				 * <= 0 means don't draw a highlight. */
  XColor *highlightBgColorPtr;  /* Color for drawing traversal highlight
				 * area when highlight is off. */
  XColor *highlightColorPtr;	/* Color for drawing traversal highlight. */
  int inset;			/* Total width of highlight and 3-D border */
  Tk_Font aFont[N_FONT];	/* Information about all screen fonts */
  char fontValid[(N_FONT+7)/8]; /* If bit N%8 of work N/8 of this field is 0
                                 * if aFont[N] needs to be reallocated before
                                 * being used. */
  XColor *apColor[N_COLOR];     /* Information about all colors */
  int colorUsed;                /* bit N is 1 if color N is in use.  Only
                                ** applies to colors that aren't predefined */
  int iDark[N_COLOR];           /* Dark 3D shadow of color K is iDark[K] */
  int iLight[N_COLOR];          /* Light 3D shadow of color K is iLight[K] */
  XColor *fgColor;              /* Color of normal text. apColor[0] */
  XColor *newLinkColor;         /* Color of unvisitied links. apColor[1] */
  XColor *oldLinkColor;         /* Color of visitied links. apColor[2] */
  XColor *selectionColor;       /* Background color for selections */
  GcCache aGcCache[N_CACHE_GC]; /* A cache of GCs for general use */
  int lastGC;                   /* Index of recently used GC */
  HtmlImage *imageList;         /* A list of all images */
  int width, height;		/* User-requested size of the usable drawing
                                 * area, in pixels.   Borders and padding
                                 * make the actual window a little larger */
  int realWidth, realHeight;    /* The actual physical size of tkwin as
                                 * reported in the most recent ConfigureNotify
                                 * event. */
  int padx, pady;               /* Separation between the edge of the window
                                 * and rendered HTML.  */
  int underlineLinks;           /* TRUE if we should underline hyperlinks */

  /* Information about the selection
  */
  int exportSelection;          /* True if the selection is automatically
                                 * exported to the clipboard */

  /* Callback commands.  The HTML parser will invoke callbacks from time
  ** to time to find out information it needs to complete formatting of
  ** the document.  The following fields define the callback commands.
  */
  char *zIsVisited;             /* Command to tell if a hyperlink has already
                                ** been visited */
  char *zGetImage;              /* Command to get an image from a URL */
  char *zFrameCommand;          /* Command for handling <frameset> markup */
  char *zAppletCommand;         /* Command to process applets */
  char *zResolverCommand;       /* Command to resolve URIs */
  char *zFormCommand;           /* When user presses Submit */
  char *zHyperlinkCommand;      /* Invoked when a hyperlink is clicked */
  char *zFontCommand;           /* Invoked to find font names */
  char *zScriptCommand;         /* Invoked for each <SCRIPT> markup */

   /*
    * Miscellaneous information:
    */
  int tableRelief;              /* 3d effects on <TABLE> */
  int ruleRelief;               /* 3d effects on <HR> */
  char *zBase;                  /* The base URI */
  char *zBaseHref;              /* zBase as modified by <BASE HREF=..> markup */
  Tk_Cursor cursor;		/* Current cursor for window, or None. */
  char *takeFocus;		/* Value of -takefocus option;  not used in
				 * the C code, but used by keyboard traversal
				 * scripts.  Malloc'ed, but may be NULL. */
  char *yScrollCmd;		/* Command prefix for communicating with
				 * vertical scrollbar.  NULL means no command
				 * to issue.  Malloc'ed. */
  char *xScrollCmd;		/* Command prefix for communicating with
				 * horizontal scrollbar.  NULL means no command
				 * to issue.  Malloc'ed. */
  int xOffset, yOffset;         /* Current scroll position.  These form the
                                 * coordinate in the virtual canvas that
                                 * corresponds to (0,0) on the physical screen
                                 * in window tkwin */
  int maxX, maxY;               /* Maximum extent of any "paint" that appears
                                 * on the virtual canvas.  Used to compute
                                 * scrollbar positions. */
  int dirtyLeft, dirtyTop;      /* Top left corner of region to redraw.  These
                                 * are physical screen coordinates relative to
                                 * clipwin, not tkwin. */
  int dirtyRight, dirtyBottom;  /* Bottom right corner of region to redraw */
  int locked;                   /* Number of locks on this structure. Don't
                                ** delete until it reaches zero. */
  int flags;			/* Various flags;  see below for
				 * definitions. */
};
typedef int Html_32;
struct HtmlImage {
  HtmlWidget *htmlPtr;     /* The owner of this image */
  Tk_Image image;          /* The Tk image token */
  Html_32 w;               /* Requested width of this image (0 if none) */
  Html_32 h;               /* Requested height of this image (0 if none) */
  char *zUrl;              /* The URL for this image. */
  char *zWidth, *zHeight;  /* Width and height in the <img> markup. */
  HtmlImage *pNext;        /* Next image on the list */
  HtmlElement *pList;      /* List of all <IMG> markups that use this
                           ** same image */
};
#if defined(COVERAGE_TEST)
extern int HtmlTPArray[2000];
# define TestPoint(X)      {extern int HtmlTPArray[]; HtmlTPArray[X]++;}
#endif
#if !(defined(COVERAGE_TEST))
# define TestPoint(X)
#endif
#if !defined(HAVE_STRICMP)
# define stricmp strcasecmp
#endif
char *HtmlMarkupArg(HtmlElement *p,const char *tag,char *zDefault);
#define IMAGE_ALIGN_Right         7
#define IMAGE_ALIGN_Left          6
#define IMAGE_ALIGN_TextTop       3
#define IMAGE_ALIGN_AbsMiddle     4
#define IMAGE_ALIGN_AbsBottom     5
#define IMAGE_ALIGN_Top           2
#define IMAGE_ALIGN_Middle        1
#define IMAGE_ALIGN_Bottom        0
typedef struct HtmlBaseElement HtmlBaseElement;
typedef struct HtmlStyle HtmlStyle;
struct HtmlStyle {
  unsigned int font    : 6;      /* Font to use for display */
  unsigned int color   : 4;      /* Foreground color */
  signed int subscript : 4;      /* Positive for <sup>, negative for <sub> */
  unsigned int align   : 2;      /* Horizontal alignment */
  unsigned int bgcolor : 4;      /* Background color */
  unsigned int flags   : 12;     /* the STY_ flags below */
};
struct HtmlBaseElement {
  HtmlElement *pNext;         /* Next input token in a list of them all */
  HtmlElement *pPrev;         /* Previous token in a list of them all */
  HtmlStyle style;            /* The rendering style for this token */
  Html_u8 type;               /* The token type. */
  Html_u8 flags;              /* The HTML_ flags below */
  Html_16 count;              /* Various uses, depending on "type" */
};
typedef struct HtmlTextElement HtmlTextElement;
struct HtmlTextElement {
  HtmlBaseElement base;       /* All the base information */
  Html_32 y;                  /* y coordinate where text should be rendered */
  Html_16 x;                  /* x coordinate where text should be rendered */
  Html_16 w;                  /* width of this token in pixels */
  Html_u8 ascent;             /* height above the baseline */
  Html_u8 descent;            /* depth below the baseline */
  Html_u8 spaceWidth;         /* Width of one space in the current font */
  char zText[1];              /* Text for this element.  Null terminated */
};
typedef struct HtmlSpaceElement HtmlSpaceElement;
struct HtmlSpaceElement {
  HtmlBaseElement base;       /* All the base information */
  Html_16 w;                  /* Width of a single space in current font */
  Html_u8 ascent;             /* height above the baseline */
  Html_u8 descent;            /* depth below the baseline */
};
typedef struct HtmlMarkupElement HtmlMarkupElement;
struct HtmlMarkupElement {
  HtmlBaseElement base;
  char **argv;
};
typedef struct HtmlCell HtmlCell;
struct HtmlCell {
  HtmlMarkupElement markup;
  Html_16 rowspan;          /* Number of rows spanned by this cell */
  Html_16 colspan;          /* Number of columns spanned by this cell */
  Html_16 x;                /* X coordinate of left edge of border */
  Html_16 w;                /* Width of the border */
  Html_32 y;                /* Y coordinate of top of border indentation */
  Html_32 h;                /* Height of the border */
  HtmlElement *pTable;      /* Pointer back to the <table> */
  HtmlElement *pEnd;        /* Element that ends this cell */
};
typedef struct HtmlTable HtmlTable;
typedef unsigned short Html_u16;
#define HTML_MAX_COLUMNS 40
struct HtmlTable {
  HtmlMarkupElement markup;
  Html_u8 borderWidth;           /* Width of the border */
  Html_u8 nCol;                  /* Number of columns */
  Html_u16 nRow;                 /* Number of rows */
  Html_32 y;                     /* top edge of table border */
  Html_32 h;                     /* height of the table border */
  Html_16 x;                     /* left edge of table border */
  Html_16 w;                     /* width of the table border */
  int minW[HTML_MAX_COLUMNS+1];  /* minimum width of each column */
  int maxW[HTML_MAX_COLUMNS+1];  /* maximum width of each column */
};
typedef struct HtmlRef HtmlRef;
struct HtmlRef {
  HtmlMarkupElement markup;
  HtmlElement *pOther;         /* Pointer to some other Html element */
};
typedef struct HtmlLi HtmlLi;
struct HtmlLi {
  HtmlMarkupElement markup;
  Html_u8 type;     /* What type of list is this? */
  Html_u8 ascent;   /* height above the baseline */
  Html_u8 descent;  /* depth below the baseline */
  Html_16 cnt;      /* Value for this element (if inside <OL>) */
  Html_16 x;        /* X coordinate of the bullet */
  Html_32 y;        /* Y coordinate of the bullet */
};
typedef struct HtmlListStart HtmlListStart;
struct HtmlListStart {
  HtmlMarkupElement markup;
  Html_u8 type;            /* One of the LI_TYPE_ defines above */
  Html_u8 compact;         /* True if the COMPACT flag is present */
  Html_u16 cnt;            /* Next value for <OL> */
  Html_u16 width;          /* How much space to allow for indentation */
  HtmlElement *pPrev;      /* Next higher level list, or NULL */
};
typedef struct HtmlImageMarkup HtmlImageMarkup;
struct HtmlImageMarkup {
  HtmlMarkupElement markup;
  Html_u8 align;          /* Alignment.  See IMAGE_ALIGN_ defines below */
  Html_u8 textAscent;     /* Ascent of text font in force at the <IMG> */
  Html_u8 textDescent;    /* Descent of text font in force at the <IMG> */
  Html_u8 redrawNeeded;   /* Need to redraw this image because the image
                          ** content changed. */
  Html_16 h;              /* Actual height of the image */
  Html_16 w;              /* Actual width of the image */
  Html_16 ascent;         /* How far image extends above "y" */
  Html_16 descent;        /* How far image extends below "y" */
  Html_16 x;              /* X coordinate of left edge of the image */
  Html_32 y;              /* Y coordinate of image baseline */
  char *zAlt;             /* Alternative text */
  HtmlImage *pImage;      /* Corresponding HtmlImage structure */
  HtmlElement *pNext;     /* Next markup using the same HtmlImage structure */
};
typedef struct HtmlInput HtmlInput;
struct HtmlInput {
  HtmlMarkupElement markup;
  HtmlElement *pForm;      /* The <FORM> to which this belongs */
  HtmlElement *pNext;      /* Next element in a list of all input elements */
  Tk_Window tkwin;         /* The window that implements this control */
  HtmlWidget *htmlPtr;     /* The whole widget.  Needed by geometry callbacks */
  HtmlElement *pEnd;       /* End tag for <TEXTAREA>, etc. */
  Html_32  y;              /* Baseline for this input element */
  Html_u16 x;              /* Left edge */
  Html_u16 w, h;           /* Width and height of this control */
  Html_u8 padLeft;         /* Extra padding on left side of the control */
  Html_u8 align;           /* One of the IMAGE_ALIGN_xxx  types */
  Html_u8 textAscent;      /* Ascent for the current font */
  Html_u8 textDescent;     /* descent for the current font */
  Html_u8 type;            /* What type of input is this? */
  Html_u8 sized;           /* True if this input has been sized already */
  Html_u16 cnt;            /* Used to derive widget name. 0 if no widget */
};
typedef struct HtmlForm HtmlForm;
struct HtmlForm {
  HtmlMarkupElement markup;
  Html_u16 id;             /* Unique number assigned to this form */
};
typedef struct HtmlHr HtmlHr;
struct HtmlHr {
  HtmlMarkupElement markup;
  Html_32  y;              /* Baseline for this input element */
  Html_u16 x;              /* Left edge */
  Html_u16 w, h;           /* Width and height of this control */
  Html_u8 is3D;            /* Is it drawn 3D? */
};
typedef struct HtmlAnchor HtmlAnchor;
struct HtmlAnchor {
  HtmlMarkupElement markup;
  Html_32  y;              /* Top edge for this element */
};
struct HtmlScript {
  HtmlMarkupElement markup;
  char *zScript;           /* Complete text of this script */
  int nScript;             /* Number of characters of text */
};
struct HtmlBlock {
  HtmlBaseElement base;      /* Superclass.  Must be first */
  char *z;                   /* Space to hold text when n>0 */
  int top, bottom;           /* Extremes of y coordinates */
  Html_u16 left, right;      /* Left and right boundry of this object */
  Html_u16 n;                /* Number of characters in z[] */
  HtmlBlock *pPrev, *pNext;  /* Linked list of all Blocks */
};
union HtmlElement {
  HtmlElement *pNext;
  HtmlBaseElement base;
  HtmlTextElement text;
  HtmlSpaceElement space;
  HtmlMarkupElement markup;
  HtmlCell cell;
  HtmlTable table;
  HtmlRef ref;
  HtmlLi li;
  HtmlListStart list;
  HtmlImageMarkup image;
  HtmlInput input;
  HtmlForm form;
  HtmlHr hr;
  HtmlAnchor anchor;
  HtmlScript script;
  HtmlBlock block;
};
int HtmlGetImageAlignment(HtmlElement *p);
struct HtmlStyleStack {
  HtmlStyleStack *pNext;   /* Next style on the stack */
  int type;                /* A markup that ends this style. Ex: Html_EndEM */
  HtmlStyle style;         /* The currently active style. */
};
struct HtmlMargin {
  int indent;          /* Size of the current margin */
  int bottom;          /* Y value at which this margin expires */
  int tag;             /* Markup that will cancel this margin */
  HtmlMargin *pNext;   /* Previous margin */
};
