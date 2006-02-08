/* This file was automatically generated.  Do not edit! */
typedef struct HtmlCell HtmlCell;
typedef struct HtmlMarkupElement HtmlMarkupElement;
typedef struct HtmlBaseElement HtmlBaseElement;
typedef union HtmlElement HtmlElement;
typedef struct HtmlStyle HtmlStyle;
struct HtmlStyle {
  unsigned int font    : 6;      /* Font to use for display */
  unsigned int color   : 4;      /* Foreground color */
  signed int subscript : 4;      /* Positive for <sup>, negative for <sub> */
  unsigned int align   : 2;      /* Horizontal alignment */
  unsigned int bgcolor : 4;      /* Background color */
  unsigned int flags   : 12;     /* the STY_ flags below */
};
typedef unsigned char Html_u8;
typedef short Html_16;
struct HtmlBaseElement {
  HtmlElement *pNext;         /* Next input token in a list of them all */
  HtmlElement *pPrev;         /* Previous token in a list of them all */
  HtmlStyle style;            /* The rendering style for this token */
  Html_u8 type;               /* The token type. */
  Html_u8 flags;              /* The HTML_ flags below */
  Html_16 count;              /* Various uses, depending on "type" */
};
struct HtmlMarkupElement {
  HtmlBaseElement base;
  char **argv;
};
typedef int Html_32;
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
typedef struct HtmlScript HtmlScript;
struct HtmlScript {
  HtmlMarkupElement markup;
  char *zScript;           /* Complete text of this script */
  int nScript;             /* Number of characters of text */
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
typedef struct HtmlImageMarkup HtmlImageMarkup;
typedef struct HtmlImage HtmlImage;
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
typedef struct HtmlHr HtmlHr;
struct HtmlHr {
  HtmlMarkupElement markup;
  Html_32  y;              /* Baseline for this input element */
  Html_u16 x;              /* Left edge */
  Html_u16 w, h;           /* Width and height of this control */
  Html_u8 is3D;            /* Is it drawn 3D? */
};
typedef struct HtmlForm HtmlForm;
struct HtmlForm {
  HtmlMarkupElement markup;
  Html_u16 id;             /* Unique number assigned to this form */
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
typedef struct HtmlInput HtmlInput;
typedef struct HtmlWidget HtmlWidget;
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
typedef struct HtmlRef HtmlRef;
struct HtmlRef {
  HtmlMarkupElement markup;
  HtmlElement *pOther;         /* Pointer to some other Html element */
};
typedef struct HtmlAnchor HtmlAnchor;
struct HtmlAnchor {
  HtmlMarkupElement markup;
  Html_32  y;              /* Top edge for this element */
};
typedef struct HtmlTokenMap HtmlTokenMap;
struct HtmlTokenMap {
  char *zName;                /* Name of a markup */
  Html_16 type;               /* Markup type code */
  Html_16 extra;              /* Extra space needed above HtmlBaseElement */
  HtmlTokenMap *pCollide;     /* Hash table collision chain */
};
extern HtmlTokenMap HtmlMarkupMap[];
#define HTML_MARKUP_COUNT 147
#define HTML_MARKUP_HASH_SIZE 163
#define Html_TypeCount       151
#define Html_EndXMP          151
#define Html_XMP             150
#define Html_WBR             149
#define Html_EndVAR          148
#define Html_VAR             147
#define Html_EndUL           146
#define Html_UL              145
#define Html_EndU            144
#define Html_U               143
#define Html_EndTT           142
#define Html_TT              141
#define Html_EndTR           140
#define Html_TR              139
#define Html_EndTITLE        138
#define Html_TITLE           137
#define Html_EndTH           136
#define Html_TH              135
#define Html_EndTEXTAREA     134
#define Html_TEXTAREA        133
#define Html_EndTD           132
#define Html_TD              131
#define Html_EndTABLE        130
#define Html_TABLE           129
#define Html_EndSUP          128
#define Html_SUP             127
#define Html_EndSUB          126
#define Html_SUB             125
#define Html_STYLE           124
#define Html_EndSTRONG       123
#define Html_STRONG          122
#define Html_EndSTRIKE       121
#define Html_STRIKE          120
#define Html_EndSMALL        119
#define Html_SMALL           118
#define Html_EndSELECT       117
#define Html_SELECT          116
#define Html_SCRIPT          115
#define Html_EndSAMP         114
#define Html_SAMP            113
#define Html_EndS            112
#define Html_S               111
#define Html_EndPRE          110
#define Html_PRE             109
#define Html_PLAINTEXT       108
#define Html_EndPARAM        107
#define Html_PARAM           106
#define Html_EndP            105
#define Html_P               104
#define Html_EndOPTION       103
#define Html_OPTION          102
#define Html_EndOL           101
#define Html_OL              100
#define Html_EndNOSCRIPT     99
#define Html_NOSCRIPT        98
#define Html_EndNOFRAME      97
#define Html_NOFRAME         96
#define Html_EndNOBR         95
#define Html_NOBR            94
#define Html_NEXTID          93
#define Html_META            92
#define Html_EndMENU         91
#define Html_MENU            90
#define Html_EndMARQUEE      89
#define Html_MARQUEE         88
#define Html_EndMAP          87
#define Html_MAP             86
#define Html_EndLISTING      85
#define Html_LISTING         84
#define Html_LINK            83
#define Html_EndLI           82
#define Html_LI              81
#define Html_EndKBD          80
#define Html_KBD             79
#define Html_ISINDEX         78
#define Html_INPUT           77
#define Html_IMG             76
#define Html_IFRAME          75
#define Html_EndI            74
#define Html_I               73
#define Html_EndHTML         72
#define Html_HTML            71
#define Html_HR              70
#define Html_EndH6           69
#define Html_H6              68
#define Html_EndH5           67
#define Html_H5              66
#define Html_EndH4           65
#define Html_H4              64
#define Html_EndH3           63
#define Html_H3              62
#define Html_EndH2           61
#define Html_H2              60
#define Html_EndH1           59
#define Html_H1              58
#define Html_EndFRAMESET     57
#define Html_FRAMESET        56
#define Html_EndFRAME        55
#define Html_FRAME           54
#define Html_EndFORM         53
#define Html_FORM            52
#define Html_EndFONT         51
#define Html_FONT            50
#define Html_EMBED           49
#define Html_EndEM           48
#define Html_EM              47
#define Html_EndDT           46
#define Html_DT              45
#define Html_EndDL           44
#define Html_DL              43
#define Html_EndDIV          42
#define Html_DIV             41
#define Html_EndDIR          40
#define Html_DIR             39
#define Html_EndDFN          38
#define Html_DFN             37
#define Html_EndDD           36
#define Html_DD              35
#define Html_EndCOMMENT      34
#define Html_COMMENT         33
#define Html_EndCODE         32
#define Html_CODE            31
#define Html_EndCITE         30
#define Html_CITE            29
#define Html_EndCENTER       28
#define Html_CENTER          27
#define Html_EndCAPTION      26
#define Html_CAPTION         25
#define Html_BR              24
#define Html_EndBODY         23
#define Html_BODY            22
#define Html_EndBLOCKQUOTE   21
#define Html_BLOCKQUOTE      20
#define Html_EndBIG          19
#define Html_BIG             18
#define Html_BGSOUND         17
#define Html_EndBASEFONT     16
#define Html_BASEFONT        15
#define Html_BASE            14
#define Html_EndB            13
#define Html_B               12
#define Html_AREA            11
#define Html_EndAPPLET       10
#define Html_APPLET          9
#define Html_EndADDRESS      8
#define Html_ADDRESS         7
#define Html_EndA            6
#define Html_A               5
#define Html_Block   4
#define HtmlIsMarkup(X) ((X)->base.type>Html_Block)
#define Html_Unknown 3
#define Html_Space   2
#define Html_Text    1
#define INTERFACE 0
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
typedef struct HtmlBlock HtmlBlock;
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
typedef struct HtmlIndex HtmlIndex;
struct HtmlIndex {
  HtmlElement *p;      /* The token containing the character */
  int i;               /* Index of the character */
};
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
