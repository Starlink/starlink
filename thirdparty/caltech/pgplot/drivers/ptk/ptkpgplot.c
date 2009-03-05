#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include <tk.h>
#include "Lang.h"
#include "tkPort.h"
#include "default.h"
#include "tkInt.h"
#include "tkVMacro.h"
#include "ptkpgplot.h"

#include "pgxwin.h"

#define NODEBUG
#ifdef DODEBUG
#define SAY(x) printf(x)
#else
#define SAY(x) 
#endif

/*
 * VAX VMS includes etc..
 */
#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
typedef struct dsc$descriptor_s VMS_string;
#define VMS_STRING(dsc, string) \
  dsc.dsc$w_length = strlen(string); \
  dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
  dsc.dsc$b_class = DSC$K_CLASS_S; \
  dsc.dsc$a_pointer = string;
#endif

/*
 * Compose the pgplot-callable driver function name.
 * Allow tkdriv to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call tkdriv() or tkdriv_().
 */
#ifdef PG_PPU
#define DRIV pkdriv_    /* Perl Tk with PG_PPU defined */
#else
#define DRIV pkdriv     /* Perl Tk with PG_PPU undefined */
#endif

/*
 * List widget defaults. Note that the macros that are prefixed
 * TKPG_STR_ are for use in the configSpecs resource database. These
 * have to be strings.
 */
#define TKPG_MIN_WIDTH 64           /* Minimum width (pixels) */

#define TKPG_MIN_HEIGHT 64          /* Minimum height (pixels) */

#define TKPG_DEF_WIDTH 256          /* Default width (pixels) */
#define TKPG_STR_DEF_WIDTH "256"    /* String version of TKPG_DEF_WIDTH */

#define TKPG_DEF_HEIGHT 256         /* Default height (pixels) */
#define TKPG_STR_DEF_HEIGHT "256"   /* String version of TKPG_DEF_HEIGHT */

#define TKPG_MIN_COLORS 2           /* Min number of colors per colormap */
#define TKPG_STR_MIN_COLORS "2"     /* String version of TKPG_MIN_COLORS */

#define TKPG_DEF_COLORS 100         /* Default number of colors to try for */
#define TKPG_STR_DEF_COLORS "100"   /* String version of TKPG_DEF_COLORS */

#define TKPG_MAX_COLORS 255         /* Max number of colors per colormap */

#define TKPG_DEF_HIGHLIGHT_WIDTH 2  /* Default width of traversal highlight */
#define TKPG_STR_DEF_HIGHLIGHT_WIDTH "2"/* String ver of TKPG_DEF_HIGHLIGHT_WIDTH */
#define TKPG_STR_MARGIN_DEF "20"    /* The default number of pixels of */
                                    /*  extra space to allocate around the */
                                    /*  edge of the plot area. */
/*
 * Specify the name to prefix errors with.
 */
#define TKPG_IDENT "PgplotWidget"

typedef struct TkPgplot TkPgplot;

/*
 * Declare a container for a list of PGPLOT widgets.
 */
typedef struct {
  TkPgplot *head;       /* The head of the list of widgets */
} TkPgplotList;

/*
 * A context descriptor for managing parent ScrolledWindow scroll-bars.
 */
typedef struct {
  LangCallback *xScrollCmd;  /* pTk X-axis update-scrollbar callback */
  LangCallback *yScrollCmd;  /* pTk Y-axis update-scrollbar callback */
  unsigned x;           /* Pixmap X coordinate of top left corner of window */
  unsigned y;           /* Pixmap Y coordinate of top left corner of window */
} TkpgScroll;

/*
 * This container records state-values that are modified by X events.
 */
typedef struct {
  unsigned long mask;  /* Event mask registered to tkpg_EventHandler() */
  int focus_acquired;  /* True when we have keyboard-input focus */
  int cursor_active;   /* True when cursor augmentation is active */
} TkpgEvents;

struct TkPgplot {
                      /* Widget context */
  Tk_Window tkwin;          /* Tk's window object */
  Display *display;         /* The X display of the window */
  Tcl_Interp *interp;       /* The application's TCL interpreter */
  Tcl_Command widgetCmd;    /* Token for widget's command. */
  char buffer[81];          /* A work buffer for constructing result strings */

                      /* Public resource attributes */
  int max_colors;           /* The max number of colors needed */
  int min_colors;           /* The min number of colors needed */
  int req_width;            /* The requested widget width (pixels) */
  int req_height;           /* The requested widget height (pixels) */
  int highlight_thickness;  /* The width of the highlight border */
  XColor *highlightBgColor; /* The inactive traversal highlight color */
  XColor *highlightColor;   /* The active traversal highlight color */
  XColor *normalFg;         /* Normal foreground color (color index 1) */
  Tk_3DBorder border;       /* 3D border structure */
  int borderWidth;          /* The width of the 3D border */
  int relief;               /* Relief of the 3D border */
  char *takeFocus;          /* "1" to allow focus traversal, "0" to disallow */
  char *name;               /* Name of pgplot widget */
  Cursor cursor;            /* The active cursor of the window */
                      /* Private attributes */
  int share;                /* True if shared colors are desired */
  int padx,pady;            /* Extra padding margin widths (pixels) */
  TkPgplot *next;           /* The next widget of a list of PGPLOT Xt widgets */
  int tkslct_id;            /* The device ID returned to PGPLOT by the */
                            /* open-workstation driver opcode, and used for */
                            /* subsequent device selection via the */
                            /* select-plot driver opcode */
  int pgslct_id;            /* The device ID returned to the application by */
                            /* pgopen() for subsequent device selection with */
                            /* the pgslct() function */
  char *device;             /* A possible PGPLOT cpgbeg() file string */
  TkpgScroll scroll;        /* Used to maintain parent scroll bars */
  TkpgEvents events;        /* X event context */
  PgxWin *pgx;              /* PGPLOT generic X-window context descriptor */
};

static TkPgplot *new_TkPgplot(Tcl_Interp *interp, Tk_Window main_w, char *name,
			      int objc, Tcl_Obj *CONST objv[]);
static TkPgplot *del_TkPgplot(TkPgplot *tkpg);


/*
 * Describe all recognized widget resources.
 */
static Tk_ConfigSpec configSpecs[] = {

  {TK_CONFIG_BORDER,
     "-background", "background", "Background",
     "Black", Tk_Offset(TkPgplot, border), 0},
  {TK_CONFIG_SYNONYM,
     "-bg", "background", (char *) NULL, NULL, 0, 0},

  {TK_CONFIG_COLOR,
     "-foreground", "foreground", "Foreground",
     "White", Tk_Offset(TkPgplot, normalFg), 0},
  {TK_CONFIG_SYNONYM,
     "-fg", "foreground", (char *) NULL, NULL, 0, 0},

  {TK_CONFIG_ACTIVE_CURSOR,
     "-cursor", "cursor", "Cursor",
     "", Tk_Offset(TkPgplot, cursor), TK_CONFIG_NULL_OK},

  {TK_CONFIG_PIXELS,
     "-borderwidth", "borderWidth", "BorderWidth",
     "0", Tk_Offset(TkPgplot, borderWidth), 0},
  {TK_CONFIG_SYNONYM,
     "-bd", "borderWidth", (char *) NULL, NULL, 0, 0},
  
  {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
     "raised", Tk_Offset(TkPgplot, relief), 0},

  {TK_CONFIG_PIXELS,
     "-height", "height", "Height",
     TKPG_STR_DEF_HEIGHT, Tk_Offset(TkPgplot, req_height), 0},

  {TK_CONFIG_PIXELS,
     "-width", "width", "Width",
     TKPG_STR_DEF_WIDTH, Tk_Offset(TkPgplot, req_width), 0},

  {TK_CONFIG_COLOR,
     "-highlightbackground", "highlightBackground", "HighlightBackground",
     "grey", Tk_Offset(TkPgplot, highlightBgColor), 0},

  {TK_CONFIG_COLOR,
     "-highlightcolor", "highlightColor", "HighlightColor",
     "White", Tk_Offset(TkPgplot, highlightColor), 0},

  {TK_CONFIG_PIXELS,
     "-highlightthickness", "highlightThickness", "HighlightThickness",
     TKPG_STR_DEF_HIGHLIGHT_WIDTH, Tk_Offset(TkPgplot, highlight_thickness), 0},

  {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
     "0", Tk_Offset(TkPgplot, takeFocus), TK_CONFIG_NULL_OK},

  {TK_CONFIG_CALLBACK,
     "-xscrollcommand", "xScrollCommand", "ScrollCommand",
     "", Tk_Offset(TkPgplot, scroll.xScrollCmd),
     TK_CONFIG_NULL_OK},

  {TK_CONFIG_CALLBACK,
     "-yscrollcommand", "yScrollCommand", "ScrollCommand",
     "", Tk_Offset(TkPgplot, scroll.yScrollCmd),
     TK_CONFIG_NULL_OK},

  {TK_CONFIG_INT,
     "-mincolors", "minColors", "MinColors",
     TKPG_STR_MIN_COLORS, Tk_Offset(TkPgplot, min_colors), 0},

  {TK_CONFIG_INT,
     "-maxcolors", "maxColors", "MaxColors",
     TKPG_STR_DEF_COLORS, Tk_Offset(TkPgplot, max_colors), 0},

  {TK_CONFIG_BOOLEAN,
     "-share", "share", "Share",
     0, Tk_Offset(TkPgplot, share), 0},

  {TK_CONFIG_PIXELS,
     "-padx", "padX", "Pad",
     TKPG_STR_MARGIN_DEF, Tk_Offset(TkPgplot, padx), 0},

  {TK_CONFIG_PIXELS,
     "-pady", "padY", "Pad",
     TKPG_STR_MARGIN_DEF, Tk_Offset(TkPgplot, pady), 0},

  {TK_CONFIG_STRING, "-name", "name", "Name",
   "0", Tk_Offset(TkPgplot, name), 0},

  {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/* Enumerate the PGPLOT class widget lists */

#define TKPG_ACTIVE_WIDGETS 1
#define TKPG_FREE_WIDGETS 2

static TkPgplotList *tkpg_WidgetList(int type);

static TkPgplot *tkpg_FindWidgetByName(char *name, int type, TkPgplot **prev);
static TkPgplot *tkpg_FindWidgetByID(int tkslct_id, int type, TkPgplot **prev);

static TkPgplot *tkpg_RemoveWidget(char *name, int type);
static TkPgplot *tkpg_PrependWidget(TkPgplot *tkpg, int type);
static TkPgplot *tkpg_CurrentWidget(char *context);

static TkPgplot *tkpg_open_widget(char *name);
static TkPgplot *tkpg_close_widget(char *name);

static void tkpg_NewPixmap(PgxWin *pgx, unsigned width, unsigned height);
static void tkpg_update_scroll_bars(TkPgplot *tkpg);
static void tkpg_update_clip(TkPgplot *tkpg);
static void tkpg_update_border(TkPgplot *tkpg);

/*static int PgplotCmd(ClientData context, Tcl_Interp *interp, int argc,
		     Tcl_Obj *CONST objv[]);
*/

static int tkpg_InstanceCommand(ClientData context, Tcl_Interp *interp,
				int objc, Tcl_Obj *CONST objv[]);

static int tkpg_InstanceCommand_return(ClientData context, int iret);

static int tkpg_Configure(TkPgplot *tkpg, Tcl_Interp *interp,
			  int objc, Tcl_Obj *CONST objv[], int flags);
static void tkpg_expose_handler(TkPgplot *tkpg, XEvent *event);
static void tkpg_draw_focus_highlight(TkPgplot *tkpg);
static void tkpg_draw_3d_border(TkPgplot *tkpg);
static int tkpg_refresh_window(TkPgplot *tkpg);

static void tkpg_ClrCursor(TkPgplot *tkpg);
static void tkpg_EventHandler(ClientData context, XEvent *event);
static void tkpg_CursorHandler(ClientData context, XEvent *event);

static Tk_Window tkpg_toplevel_of_path(Tcl_Interp *interp, Tk_Window main_w,
				       char *path);

/*
 * Enumerate supported pgband() cursor types.
 */
typedef enum {
  TKPG_NORM_CURSOR  = 0, /* Un-augmented X cursor */
  TKPG_LINE_CURSOR  = 1, /* Line cursor between ref and pointer */
  TKPG_RECT_CURSOR  = 2, /* Rectangular cursor between ref and pointer */
  TKPG_YRNG_CURSOR  = 3, /* Two horizontal lines, at ref.x and pointer.x */
  TKPG_XRNG_CURSOR  = 4, /* Two vertical lines, at ref.y and pointer.y */
  TKPG_HLINE_CURSOR = 6, /* Horizontal line cursor at y=ref.y */
  TKPG_VLINE_CURSOR = 5, /* Vertical line cursor at x=ref.x */
  TKPG_CROSS_CURSOR = 7  /* Cross-hair cursor centered on the pointer */
} TkpgCursorMode;

static int tkpg_SetCursor(TkPgplot *tkpg, TkpgCursorMode mode,
		 	  float xref, float yref, int ci);
static void tkpg_FreeProc(char *context);

static int tkpg_scrollbar_callback(TkPgplot *tkpg, Tcl_Interp *interp, 
				   char *widget, char *view, int objc,
				   Tcl_Obj *CONST objv[]);
static int tkpg_scrollbar_error(TkPgplot *tkpg, Tcl_Interp *interp, 
				char *widget, char *view, int objc,
				Tcl_Obj *CONST objv[]);

static int tkpg_tcl_setcursor(TkPgplot *tkpg, Tcl_Interp *interp,
			      int objc, Tcl_Obj *CONST objv[]);
static int tkpg_tcl_world(TkPgplot *tkpg, Tcl_Interp *interp, 
			  char *widget, int objc, Tcl_Obj *CONST objv[]);
static int tkpg_tcl_pixel(TkPgplot *tkpg, Tcl_Interp *interp, 
			  char *widget, int objc, Tcl_Obj *CONST objv[]);
static int tkpg_tcl_id(TkPgplot *tkpg, Tcl_Interp *interp, 
		       char *widget, int objc, Tcl_Obj *CONST objv[]);
static int tkpg_tcl_device(TkPgplot *tkpg, Tcl_Interp *interp, 
			   char *widget, int objc, Tcl_Obj *CONST objv[]);

/*
 * The following file-scope container records the list of active and
 * inactive PGPLOT widgets.
 */
static struct {
  int id_counter;              /* Used to give widgets unique identifiers */
  TkPgplotList active_widgets; /* List of active widgets */
  TkPgplotList free_widgets;   /* List of unnassigned widgets */
} tkPgplotClassRec = {
  0,         /* id_counter */
  {NULL},    /* active_widgets */
  {NULL},    /* free_widgets */
};

/*
 * The following macro defines the event mask used by the cursor event
 * handler. It is here to ensure that Tk_CreateEventHandler() and
 * Tk_DeleteEventHandler() are presented with identical event masks.
 */
#define CURSOR_EVENT_MASK ((unsigned long)(EnterWindowMask | LeaveWindowMask | \
					   PointerMotionMask))
/*
 * The following macro defines the event mask normally used by the widget.
 */
#define NORMAL_EVENT_MASK ((unsigned long)(StructureNotifyMask | \
					   ExposureMask | FocusChangeMask))

/*.......................................................................
 * This function provides the TCL command that creates a PGPLOT widget.
 *
 * Input:
 *  context   ClientData    The client_data argument specified in
 *                          TkPgplot_Init() when PgplotCmd was registered.
 *                          This is the main window cast to (ClientData).
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  objc             int    The number of command arguments.
 *  objv            char ** The array of 'objc' command arguments.
 *                          objv[0] = "pgplot"
 *                          objv[1] = the name to give the new widget.
 *                          objv[2..objc-1] = attribute settings.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
int PgplotCmd(ClientData context, Tcl_Interp *interp, int objc,
	      Tcl_Obj *CONST objv[])
{
  Tk_Window main_tkw = Tk_MainWindow(interp); /* The application main window */
  TkPgplot *tkpg;                          /* The new widget instance object */

  SAY("PgplotCmd\n");

/*
 * Make sure that a name for the new widget has been provided.
 */

#ifdef DODEBUG
  {int i;
  printf(" Args(%d):\n", objc);
  for (i=0; i<objc; i++) 
    printf("  %d %p %s\n", i, objv[i], Tcl_GetString(objv[i]));
  //printf("   %d\n", i);
  }
#endif

  if(objc < 2) {
    SAY(" Wrong args\n");
    Tcl_WrongNumArgs(interp, 1,
		     objv, " pathName ?options?");
    return TCL_ERROR;
  };
/*
 * Allocate the widget-instance object.
 */

  tkpg = new_TkPgplot(interp, main_tkw, Tcl_GetString(objv[1]), objc-2, objv+2);
  if(!tkpg)
    return TCL_ERROR;
  return TCL_OK;
}

/*.......................................................................
 * Create a new widget instance object.
 *
 * Input:
 *  interp   Tcl_Interp *  The TCL interpreter object.
 *  main_w    Tk_Window    The main window of the application.
 *  name           char *  The name to give the new widget.
 *  objc            int    The number of argument in objv[]
 *  objv           char ** Any configuration arguments.
 * Output:
 *  return     TkPgplot *  The new PGPLOT widget, or NULL on error.
 *                         If NULL is returned then the context of the
 *                         error will have been recorded in the result
 *                         field of the interpreter.
 */
static TkPgplot *new_TkPgplot(Tcl_Interp *interp, Tk_Window main_w, char *name,
			      int objc, Tcl_Obj *CONST objv[])
{
  TkPgplot *tkpg;  /* The new widget object */
  PgxWin *pgx;     /* The PGPLOT X window object of the widget */
  Tk_Window top_w; /* The top-level window parent of 'name' */

  SAY("new_TkPgplot\n");

/*
 * Get the toplevel window associated with the pathname in 'name'.
 */
  top_w = tkpg_toplevel_of_path(interp, main_w, name);
  if(!top_w)
    return NULL;

  SAY("Got top_w\n");
/*
 * Allocate the container.
 */
  tkpg = (TkPgplot *) malloc(sizeof(TkPgplot));
  if(!tkpg) {
    Tcl_AppendResult(interp, "Insufficient memory to create ", name, NULL);
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can safely be passed to
 * del_TkPgplot().
 */
  tkpg->tkwin = NULL;
  tkpg->display = Tk_Display(main_w);
  tkpg->interp = interp;
  tkpg->max_colors = TKPG_DEF_COLORS;
  tkpg->min_colors = TKPG_MIN_COLORS;
  tkpg->req_width = TKPG_DEF_WIDTH;
  tkpg->req_height = TKPG_DEF_HEIGHT;
  tkpg->highlight_thickness = TKPG_DEF_HIGHLIGHT_WIDTH;
  tkpg->highlightBgColor = NULL;
  tkpg->highlightColor = NULL;
  tkpg->normalFg = NULL;
  tkpg->border = NULL;
  tkpg->borderWidth = 0;
  tkpg->relief = TK_RELIEF_RAISED;
  tkpg->takeFocus = NULL;
  tkpg->name = NULL;
  tkpg->cursor = None;
  tkpg->share = 0;
  tkpg->padx = 0;
  tkpg->pady = 0;
  tkpg->next = NULL;
  tkpg->tkslct_id = tkPgplotClassRec.id_counter++;
  tkpg->pgslct_id = 0;
  tkpg->device = NULL;
  tkpg->scroll.xScrollCmd = NULL;
  tkpg->scroll.yScrollCmd = NULL;
  tkpg->scroll.x = 0;
  tkpg->scroll.y = 0;
  tkpg->events.mask = NoEventMask;
  tkpg->events.focus_acquired = 0;
  tkpg->events.cursor_active = 0;
  tkpg->pgx = NULL;
/*
 * Allocate the PGPLOT-window context descriptor.
 */
  pgx = tkpg->pgx = new_PgxWin(tkpg->display, Tk_ScreenNumber(top_w),
			(void *) tkpg, name, 0, tkpg_NewPixmap);
  if(!pgx) {
    Tcl_AppendResult(interp, "Unable to create Pgplot window object for: ",
		     name, NULL);
    return NULL;
  };
/*
 * Compose a sample PGPLOT device-specification for use in opening this
 * widget to PGPLOT.
 */
  tkpg->device = (char *) malloc(sizeof(char) *
				 (strlen(name)+1+strlen(TK_PGPLOT_DEVICE)+1));
  if(!tkpg->device) {
    Tcl_AppendResult(interp, "Insufficient memory for ", name, NULL);
    return NULL;
  };
  sprintf(tkpg->device, "%s/%s", name, TK_PGPLOT_DEVICE);
/*
 * Ensure that the toplevel window parent of the new window exists,
 * before attempting to determine its visual.
 */
  Tk_MakeWindowExist(top_w);
/*
 * Create the widget window from the specified path.
 */
  tkpg->tkwin = Tk_CreateWindowFromPath(interp, main_w, name, NULL);
  if(!tkpg->tkwin)
    return del_TkPgplot(tkpg);
/*
 * Give the widget a class name.
 */
  Tk_SetClass(tkpg->tkwin, "Pgplot");
/*
 * Register an event handler.
 */
  tkpg->events.mask = NORMAL_EVENT_MASK;
  Tk_CreateEventHandler(tkpg->tkwin, tkpg->events.mask, tkpg_EventHandler,
			(ClientData) tkpg);
/*
 * Create the TCL command that will allow users to configure the widget.
 */
  tkpg->widgetCmd = Lang_CreateWidget(interp, tkpg->tkwin, tkpg_InstanceCommand, 
				      (ClientData) tkpg, 0);
/*
 * Parse command line defaults into tkpg so that tkpg->min_colors,
 * tkpg->max_colors and tkpg->share are known.
 */
  if(Tk_ConfigureWidget(interp, tkpg->tkwin, configSpecs, objc, objv,
			(char *) tkpg, 0) == TCL_ERROR)
    return del_TkPgplot(tkpg);
/*
 * If requested, try to allocate read/write colors.
 * If this fails arrange to try shared colors.
 */
  if(!tkpg->share && !pgx_window_visual(pgx, Tk_WindowId(top_w),
				       tkpg->min_colors, tkpg->max_colors, 0))
    tkpg->share = 1;
/*
 * Allocate shared colors?
 */
  if(tkpg->share) {
    if(!pgx_window_visual(pgx, Tk_WindowId(top_w), tkpg->min_colors,
			tkpg->max_colors, 1)) {
      Tcl_AppendResult(interp, "Unable to allocate any colors for ",name,NULL);
      return del_TkPgplot(tkpg);
    };
  };
/*
 * Force Tk to create the window.
 */
  Tk_MakeWindowExist(tkpg->tkwin);
/*
 * Fill in details about the window in pgx.
 */
  pgx->window = Tk_WindowId(tkpg->tkwin);
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
  {
    XGCValues gcv;
    gcv.graphics_exposures = False;
    pgx_start_error_watch(pgx);
    pgx->expose_gc = XCreateGC(pgx->display, pgx->window, (unsigned long)
			       (GCGraphicsExposures), &gcv);
    if(pgx_end_error_watch(pgx) || pgx->expose_gc==NULL) {
      Tcl_AppendResult(interp,
	       "Failed to allocate a graphical context for ", name, NULL);
      return del_TkPgplot(tkpg);
    };
  };
/*
 * Parse the command-line arguments again and install the relevant
 * defaults into the color descriptor created by pgx_window_visual().
 */
  if(tkpg_Configure(tkpg, interp, objc, objv, 0))
    return del_TkPgplot(tkpg);
/*
 * If the widget has scroll-bars make sure that they agree with the
 * window.
 */
  tkpg_update_scroll_bars(tkpg);
  tkpg_update_clip(tkpg);
/*
 * Replace the share configuration attribute with the actual
 * value that was acheived.
 */
  tkpg->share = pgx->color->readonly;
/*
 * Prepend the new widget to the list of unassigned widgets to be
 * used by pgbeg().
 */
  tkpg_PrependWidget(tkpg, TKPG_FREE_WIDGETS);
/*
 * Return the widget name.
 */
#ifdef OLDTK
  Tcl_ArgResult(interp, LangWidgetArg(interp,tkpg->tkwin));
#else
  Tcl_SetObjResult(interp, LangWidgetObj(interp,tkpg->tkwin));
#endif
  return tkpg;
}

/*.......................................................................
 * Delete a TkPgplot widget.
 *
 * Input:
 *  tkpg    TkPgplot *   The widget to be deleted.
 * Output:
 *  return  TkPgplot *   Always NULL.
 */
static TkPgplot *del_TkPgplot(TkPgplot *tkpg)
{

  SAY("del_TkPgplot\n");

  if(tkpg) {
    if(tkpg->pgx) {
      PgxWin *pgx = tkpg->pgx;
/*
 * Remove the device from the appropriate list of PGPLOT widgets.
 */
      /* tkpg_RemoveWidget(pgx->name, pgx->state ? TKPG_ACTIVE_WIDGETS :*/
      tkpg_RemoveWidget(tkpg->name, pgx->state ? TKPG_ACTIVE_WIDGETS :
		                                TKPG_FREE_WIDGETS);
/*
 * Delete the Tcl command attached to the widget.
 */

      Lang_DeleteWidget(tkpg->interp, tkpg->widgetCmd);
/*
 * Delete the window context descriptor.
 */ 
      tkpg->pgx = del_PgxWin(tkpg->pgx);
    };
/*
 * Delete the device name string.
 */
    if(tkpg->device)
      free(tkpg->device);
    tkpg->device = NULL;
/*
 * Clear the cursor.
 */
    tkpg_ClrCursor(tkpg);
/*
 * Delete resource values.
 */
    if(tkpg->display)
      Tk_FreeOptions(configSpecs, (char *) tkpg, tkpg->display, 0);
/*
 * Remove the DestroyNotify event handler before destroying the
 * window. Otherwise this function would call itself recursively
 * and pgx would be free'd twice.
 */
    if(tkpg->events.mask != NoEventMask) {
      Tk_DeleteEventHandler(tkpg->tkwin, tkpg->events.mask,
			    tkpg_EventHandler, (ClientData) tkpg);
      tkpg->events.mask = NoEventMask;
    };
/*
 * Zap the window.
 */
    if(tkpg->tkwin) {
      Tk_DestroyWindow(tkpg->tkwin);
      tkpg->tkwin = NULL;
    };
/*
 * Delete the container.
 */
    free(tkpg);
  };
  return NULL;
}

/*.......................................................................
 * This function is called upon by the pgxwin toolkit whenever the
 * pixmap used as backing store needs to be resized.
 *
 * Input:
 *  pgx      PgxWin *   The pgxwin toolkit context descriptor.
 *  width  unsigned     The desired new pixmap width.
 *  height unsigned     The desired new pixmap height.
 */
static void tkpg_NewPixmap(PgxWin *pgx, unsigned width, unsigned height)
{
  TkPgplot *tkpg = (TkPgplot *) pgx->context;
/*
 * Reset the scrollbars then hand the job of allocating the
 * pixmap back to the pgxwin toolkit.
 */
  tkpg->scroll.x = 0;
  tkpg->scroll.y = 0;
  tkpg_update_scroll_bars(tkpg);
  pgx_new_pixmap(pgx, width, height);
  return;
}

/*.......................................................................
 * Whenever the size of a pixmap and/or window of a PGPLOT winget are
 * changed, this function should be called to adjust scroll bars.
 *
 * Input:
 *  tkpg     TkPgplot *  The pgplot widget instance.
 */
static void tkpg_update_scroll_bars(TkPgplot *tkpg)
{
  TkpgScroll *scroll = &tkpg->scroll;
/*
 * Block widget deletion, so that if one of the scroll-bar callbacks
 * deletes the widget we won't end up using a deleted tkpg pointer.
 */
  Tcl_Preserve((ClientData)tkpg);
/*
 * Update the horizontal scroll-bar if there is one.
 */

  SAY("tkpg_update_scroll_bars\n");

  if(scroll->xScrollCmd) {
    int result;
    Tcl_Interp *interp = tkpg->interp;

    double pixmap_width = pgx_pixmap_width(tkpg->pgx);
    double first, last;
#ifdef DODEBUG
    printf("  pixmap_width=%.2f\n", pixmap_width);
#endif
    if(pixmap_width < 1.0) {
      first = 0.0;
      last = 1.0;
    } else {
      first = scroll->x / pixmap_width;
      last = (scroll->x + Tk_Width(tkpg->tkwin)) / pixmap_width;
    };
#ifdef DODEBUG
    printf("  first = %.2f  last=%.2f=\n", first, last);
#endif

    result = LangDoCallback(interp, scroll->xScrollCmd, 0, 2, " %f %f", first, last);
    if (result != TCL_OK) {
      Tcl_BackgroundError(interp);
    }
    Tcl_ResetResult(interp);  
  };
/*
 * Update the vertical scroll-bar if there is one.
 */
  if(scroll->yScrollCmd) {
    int result;
    Tcl_Interp *interp = tkpg->interp;
    double pixmap_height = pgx_pixmap_height(tkpg->pgx);
    double first, last;
    if(pixmap_height < 1.0) {
      first = 0.0;
      last = 1.0;
    } else {
      first = scroll->y / pixmap_height;
      last = (scroll->y + Tk_Height(tkpg->tkwin)) / pixmap_height;
    };

    result = LangDoCallback(interp, scroll->yScrollCmd, 0, 2, " %f %f", first, last);
    if (result != TCL_OK) {
      Tcl_BackgroundError(interp);
    }
    Tcl_ResetResult(interp);  
  };
/*
 * Tell pgplot about the current scroll and pan values.
 */
  pgx_scroll(tkpg->pgx, scroll->x, scroll->y);
/*
 * Unblock widget deletion.
 */
  Tcl_Release((ClientData)tkpg);

  SAY("tkpg_update_scroll_bars FINISHED\n");
  return;
}

/*.......................................................................
 * Update the clip-area of the window to prevent pgxwin functions from
 * drawing over the highlight-borders.
 *
 * Input:
 *  tkpg    TkPgplot *  The pgplot widget instance.
 */
static void tkpg_update_clip(TkPgplot *tkpg)
{
  (void) pgx_update_clip(tkpg->pgx, 1, Tk_Width(tkpg->tkwin),
			 Tk_Height(tkpg->tkwin),
			 tkpg->highlight_thickness + tkpg->borderWidth);
}

/*.......................................................................
 * Find an inactive PGPLOT widget of a given name, open it to PGPLOT,
 * and move it to the head of the active list of widgets.
 *
 * Input:
 *  name         char *  The name of the widget to be opened.
 * Output:
 *  tkpg     TkPgplot *  The selected widget, or NULL on error.
 */
static TkPgplot *tkpg_open_widget(char *name)
{
  TkPgplot *tkpg;
/*
 * Remove the named widget from the free-widget list.
 */

  tkpg = tkpg_RemoveWidget(name, TKPG_FREE_WIDGETS);
  if(!tkpg) {
    if(tkpg_FindWidgetByName(name, TKPG_ACTIVE_WIDGETS, NULL)) {
      fprintf(stderr, "%s: Widget %s is already open.\n", TKPG_IDENT, name);
    } else {
      fprintf(stderr, "%s: Can't open non-existent widget (%s).\n",
	      TKPG_IDENT, name ? name : "(null)");
    };
    return NULL;
  };
/*
 * Pre-pend the widget to the active list.
 */
  tkpg_PrependWidget(tkpg, TKPG_ACTIVE_WIDGETS);
/*
 * Open the connection to the PgxWin library.
 */
  pgx_open(tkpg->pgx);
  if(!tkpg->pgx->state)
    tkpg_close_widget(name);
/*
 * Reset the background and foreground colors to match the current
 * configuration options.
 */
  pgx_set_background(tkpg->pgx, Tk_3DBorderColor(tkpg->border));
  pgx_set_foreground(tkpg->pgx, tkpg->normalFg);
/*
 * Reset its scroll-bars.
 */
  tkpg_update_scroll_bars(tkpg);

  SAY("tkpg_open_widget FINISHED\n");
  return tkpg;
}

/*.......................................................................
 * Find an active PGPLOT widget of a given name, close it to PGPLOT and
 * move it to the head of the inactive list of widgets.
 *
 * Input:
 *  name       char *  The name of the widget.
 * Output:
 *  return TkPgplot *  The selected widget, or NULL if not found.
 */
static TkPgplot *tkpg_close_widget(char *name)
{
  TkPgplot *tkpg;
/*
 * Remove the widget from the active list.
 */
  tkpg = tkpg_RemoveWidget(name, TKPG_ACTIVE_WIDGETS);
  if(!tkpg) {
    fprintf(stderr, "%s: Request to close non-existent widget (%s).\n",
	    TKPG_IDENT, name ? name : "(null)");
    return NULL;
  };
/*
 * Remove cursor handler.
 */
  tkpg_ClrCursor(tkpg);
/*
 * Close the connection to the PgxWin library.
 */
  pgx_close(tkpg->pgx);
/*
 * Invalidate the pgslct() id. The next time that the widget is opened
 * to PGPLOT a different value will likely be used.
 */
  tkpg->pgslct_id = 0;
/*
 * Prepend the widget to the free list.
 */
  tkpg_PrependWidget(tkpg, TKPG_FREE_WIDGETS);
  return tkpg;
}

/*.......................................................................
 * Lookup a widget by name from a given list of widgets.
 *
 * Input:
 *  name       char *  The name of the widget.
 *  type        int    The enumerated name of the list to search,
 *                     from:
 *                       TKPG_ACTIVE_WIDGETS
 *                       TKPG_FREE_WIDGETS
 * Output:
 *  prev   TkPgplot ** *prev will either be NULL if the widget
 *                     was at the head of the list, or be the
 *                     widget in the list that immediately precedes
 *                     the specified widget.
 *  return TkPgplot *  The located widget, or NULL if not found.
 */
static TkPgplot *tkpg_FindWidgetByName(char *name, int type, TkPgplot **prev)
{
  TkPgplotList *widget_list; /* The list to be searched */

  SAY("tkpg_FindWidgetByName\n");
  widget_list = tkpg_WidgetList(type);
  if(widget_list && name) {
    TkPgplot *last = NULL;
    TkPgplot *node = widget_list->head;
    for( ; node; last = node, node = node->next) {
      if(strcmp(node->name, name)==0) {
	if(prev)
	  *prev = last;
	return node;
      };
    };
  };
/*
 * Widget not found.
 */
  if(prev)
    *prev = NULL;
  return NULL;
}

/*.......................................................................
 * Lookup a widget by its PGPLOT id from a given list of widgets.
 *
 * Input:
 *  tkslct_id   int    The number used by PGPLOT to select the
 *                     device.
 *  type        int    The enumerated name of the list to search,
 *                     from:
 *                       TKPG_ACTIVE_WIDGETS
 *                       TKPG_FREE_WIDGETS
 * Output:
 *  prev   TkPgplot ** *prev will either be NULL if the widget
 *                     was at the head of the list, or be the
 *                     widget in the list that immediately precedes
 *                     the specified widget.
 *  return TkPgplot *  The located widget, or NULL if not found.
 */
static TkPgplot *tkpg_FindWidgetByID(int tkslct_id, int type, TkPgplot **prev)
{
  TkPgplotList *widget_list; /* The list to be searched */
  widget_list = tkpg_WidgetList(type);
  if(widget_list) {
    TkPgplot *last = NULL;
    TkPgplot *node = widget_list->head;
    for( ; node; last = node, node = node->next) {
      if(tkslct_id == node->tkslct_id) {
	if(prev)
	  *prev = last;
	return node;
      };
    };
  };
/*
 * Widget not found.
 */
  if(prev)
    *prev = NULL;
  return NULL;
}

/*.......................................................................
 * Lookup one of the PGPLOT class widget lists by its enumerated type.
 *
 * Input:
 *  type            int   The enumerated name of the list, from:
 *                          TKPG_ACTIVE_WIDGETS
 *                          TKPG_FREE_WIDGETS
 * Output:
 *  return TkPgplotList * The widget list, or NULL if not recognized.
 */
static TkPgplotList *tkpg_WidgetList(int type)
{
  switch(type) {
  case TKPG_ACTIVE_WIDGETS:
    return &tkPgplotClassRec.active_widgets;
  case TKPG_FREE_WIDGETS:
    return &tkPgplotClassRec.free_widgets;
  default:
    fprintf(stderr, "tkpg_WidgetList: No such list.\n");
  };
  return NULL;
}

/*.......................................................................
 * Remove a given widget from one of the PGPLOT class widget lists.
 *
 * Input:
 *  name       char *  The name of the widget to be removed from
 *                     the list.
 *  type        int    The enumerated name of the list from which to
 *                     remove the widget, from:
 *                       TKPG_ACTIVE_WIDGETS
 *                       TKPG_FREE_WIDGETS
 * Output:
 *  return TkPgplot *  The removed widget, or NULL if not found.
 */
static TkPgplot *tkpg_RemoveWidget(char *name, int type)
{
  TkPgplotList *widget_list; /* The list to remove the widget from */
  TkPgplot *tkpg = NULL;     /* The widget being removed */
  TkPgplot *prev;            /* The widget preceding tkpg in the list */
/*
 * Get the widget list.
 */

  SAY("tkpg_RemoveWidget\n");

#ifdef DODEBUG
  printf("  Remove Widget %s\n", name);
#endif
  widget_list = tkpg_WidgetList(type);
  if(widget_list) {
    tkpg = tkpg_FindWidgetByName(name, type, &prev);
    if(tkpg) {
      if(prev) {
	prev->next = tkpg->next;
      } else {
	widget_list->head = tkpg->next;
      };
      tkpg->next = NULL;
    };
  };
  return tkpg;
}

/*.......................................................................
 * Prepend a PGPLOT widget to a given PGPLOT class widget list.
 *
 * Input:
 *  tkpg   TkPgplot *  The widget to add to the list.
 *  type        int    The enumerated name of the list to add to, from:
 *                       TKPG_ACTIVE_WIDGETS
 *                       TKPG_FREE_WIDGETS
 * Output:
 *  return TkPgplot *  The added widget (the same as tkpg), or NULL
 *                     on error.
 */
static TkPgplot *tkpg_PrependWidget(TkPgplot *tkpg, int type)
{
  TkPgplotList *widget_list;  /* The list to prepend the widget to */
/*
 * Get the widget list.
 */
  widget_list = tkpg_WidgetList(type);
  if(widget_list) {
    tkpg->next = widget_list->head;
    widget_list->head = tkpg;
  };
  return tkpg;
}

/*.......................................................................
 * Return the currently selected PGPLOT device.
 *
 * Input:
 *  context     char *  If no TkPgplot device is currently selected
 *                      and context!=NULL then, an error message of
 *                      the form printf("%s: ...\n", context) will
 *                      be written to stderr reporting that no
 *                      device is open.
 * Output:
 *  return  TkPgplot *  The currently selected PGPLOT device, or
 *                      NULL if no device is currently selected.
 */
static TkPgplot *tkpg_CurrentWidget(char *context)
{
  TkPgplot *tkpg = tkPgplotClassRec.active_widgets.head;
  if(!tkpg && context)
    fprintf(stderr, "%s: No /%s device is currently selected.\n", context,
	    TK_PGPLOT_DEVICE);
  return tkpg;
}

/*.......................................................................
 * This is the only external entry point to the tk device driver.
 * It is called by PGPLOT to open, perform operations on, return
 * information about and close tk windows.
 *
 * Input:
 *  ifunc   int *  The PGPLOT operation code to be executed.
 * Input/output:
 *  rbuf  float *  A general buffer for input/output of float values.
 *  nbuf    int *  Where relevant this is used to return the number of
 *                 elements in rbuf[]. Also used on input to specify
 *                 number of pixels in the line-of-pixels primitive.
 *  chr    char *  A general buffer for string I/O.
 *  lchr    int *  Where relevant this is used to send and return the
 *                 number of significant characters in chr.
 * Input:
 *  len     int    Added to the call line by the FORTRAN compiler.
 *                 This contains the declared size of chr[].  
 */
#ifdef VMS
void DRIV(ifunc, rbuf, nbuf, chrdsc, lchr)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
{
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void DRIV(ifunc, rbuf, nbuf, chr, lchr, len)
 int   *ifunc, *nbuf, *lchr;
 int   len;
 float rbuf[];
 char  *chr;
{
#endif
/*
 * Get the active widget if there is one.
 */
  TkPgplot *tkpg = tkpg_CurrentWidget(NULL);
  PgxWin *pgx = tkpg ? tkpg->pgx : NULL;
  int i;
/*
 * Flush buffered opcodes.
 */
  pgx_pre_opcode(pgx, *ifunc);
/*
 * Branch on the specified PGPLOT opcode.
 */

  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
    {
      char *dev_name = TK_PGPLOT_DEVICE " (widget_path/" TK_PGPLOT_DEVICE ")";
      strncpy(chr, dev_name, len);
      *lchr = strlen(dev_name);
      for(i = *lchr; i < len; i++)
	chr[i] = ' ';
    };
    break;

/*--- IFUNC=2, Return physical min and max for plot device, and range
               of color indices -----------------------------------------*/
  case 2:
    rbuf[0] = 0.0;
    rbuf[1] = -1.0;  /* Report no effective max plot width */
    rbuf[2] = 0.0;
    rbuf[3] = -1.0;  /* Report no effective max plot height */
    rbuf[4] = 0.0;
    rbuf[5] = (pgx && !pgx->bad_device) ? pgx->color->ncol-1 : 1;
    *nbuf = 6;
    break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

  case 3:
    pgx_get_resolution(pgx, &rbuf[0], &rbuf[1]);
    rbuf[2] = 1.0;		/* Device coordinates per pixel */
    *nbuf = 3;
    break;

/*--- IFUNC=4, Return misc device info ----------------------------------*/

  case 4:
    chr[0] = 'I'; /* Interactive device */
    chr[1] = 'X'; /* Cursor is available and opcode 27 is desired */
    chr[2] = 'N'; /* No dashed lines */
    chr[3] = 'A'; /* Area fill available */
    chr[4] = 'T'; /* Thick lines */
    chr[5] = 'R'; /* Rectangle fill available */
    chr[6] = 'P'; /* Line of pixels available */
    chr[7] = 'N'; /* Don't prompt on pgend */
    chr[8] = 'Y'; /* Can return color representation */
    chr[9] = 'N'; /* Not used */
    chr[10]= 'S'; /* Area-scroll available */
    *lchr = 11;
    break;

/*--- IFUNC=5, Return default file name ---------------------------------*/

  case 5:
    chr[0] = '\0';  /* Default name is "" */
    *lchr = 0;
    break;

/*--- IFUNC=6, Return default physical size of plot ---------------------*/

  case 6:
    pgx_def_size(pgx, Tk_Width(tkpg->tkwin), Tk_Height(tkpg->tkwin), rbuf, nbuf);
    break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

  case 7:
    rbuf[0] = 1.0;
    *nbuf = 1;
    break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

  case 8:
    {
      TkPgplot *new_tkpg = tkpg_FindWidgetByID((int)(rbuf[1]+0.5),
					   TKPG_ACTIVE_WIDGETS, NULL);
      if(new_tkpg) {
	new_tkpg->pgslct_id = (int) (rbuf[0]+0.5);
	/*tkpg_RemoveWidget(new_tkpg->pgx->name, TKPG_ACTIVE_WIDGETS);*/
	tkpg_RemoveWidget(new_tkpg->name, TKPG_ACTIVE_WIDGETS);
	tkpg_PrependWidget(new_tkpg, TKPG_ACTIVE_WIDGETS);
      } else {
	fprintf(stderr, "%s: [Select plot] No such open device.\n", TKPG_IDENT);
      };
    };
    break;

/*--- IFUNC=9, Open workstation -----------------------------------------*/

  case 9:
/*
 * Assign the returned device unit number and success indicator.
 * Assume failure to open until the workstation is open.
 */
    rbuf[0] = rbuf[1] = 0.0;
    *nbuf = 2;
/*
 * Prepare the display name.
 */
    if(*lchr >= len) {
      fprintf(stderr, "%s: Widget name too long.\n", TKPG_IDENT);
      return;
    } else {
      chr[*lchr] = '\0';
    };
/*
 * Get the requested widget from the free widget list.
 */
    tkpg = tkpg_open_widget(chr);
    if(!tkpg)
      return;
    SAY("** Opened the widget\n");
    rbuf[0] = tkpg->tkslct_id; /* The number used to select this device */
    rbuf[1] = 1.0;
    *nbuf = 2;
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

  case 10:
/*
 * Remove the device from the list of open devices.
 */
    if(pgx)
      /*tkpg_close_widget(pgx->name);*/
      tkpg_close_widget(tkpg->name);
    break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

  case 11:
    pgx_begin_picture(pgx, rbuf);
    break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

  case 12:
    pgx_draw_line(pgx, rbuf);
    break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

  case 13:
    pgx_draw_dot(pgx, rbuf);
    break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

  case 14:
    break;

/*--- IFUNC=15, Select color index --------------------------------------*/

  case 15:
    pgx_set_ci(pgx, (int) (rbuf[0] + 0.5));
    break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

  case 16:
    pgx_flush(pgx);
    break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

  case 17:
    if(tkpg)
      tkpg_ClrCursor(tkpg);
    pgx_read_cursor(pgx, rbuf, chr, nbuf, lchr);
    break;

/*--- IFUNC=18, Erase alpha screen. -------------------------------------*/
  /* (Not implemented: no alpha screen) */
  case 18:
    break;

/*--- IFUNC=19, Set line style. -----------------------------------------*/
  /* (Not implemented: should not be called) */
  case 19:
    break;

/*--- IFUNC=20, Polygon fill. -------------------------------------------*/

  case 20:
    pgx_poly_fill(pgx, rbuf);
    break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

  case 21:
    {
      int ci = (int)(rbuf[0]+0.5);
      pgx_set_rgb(pgx, ci, rbuf[1],rbuf[2],rbuf[3]);
      if(ci==0)
	tkpg_update_border(tkpg);
    };
    break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/

  case 22:
    pgx_set_lw(pgx, rbuf[0]);
    break;

/*--- IFUNC=23, Escape --------------------------------------------------*/
    /* (Not implemented: ignored) */
  case 23:
    break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

  case 24:
    pgx_rect_fill(pgx, rbuf);
    break;

/*--- IFUNC=25, ---------------------------------------------------------*/
  /* (Not implemented: ignored) */
  case 25:
    break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

  case 26:
    pgx_pix_line(pgx, rbuf, nbuf);
    break;

/*--- IFUNC=27, World-coordinate scaling --------------------------------*/

  case 27:
    pgx_set_world(pgx, rbuf);
    break;

/*--- IFUNC=29, Query color representation ------------------------------*/
  case 29:
    pgx_get_rgb(pgx, rbuf, nbuf);
    break;

/*--- IFUNC=30, Scroll rectangle ----------------------------------------*/
  case 30:
    pgx_scroll_rect(pgx, rbuf);
    break;

/*--- IFUNC=?, ----------------------------------------------------------*/

  default:
    fprintf(stderr, "%s: Ignoring unimplemented opcode=%d.\n",
	    TKPG_IDENT, *ifunc);
    *nbuf = -1;
    break;
  };
  return;
}

/*.......................................................................
 * This function services TCL commands for a given widget.
 *
 * Input:
 *  context   ClientData    The tkpg widget cast to (ClientData).
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  objc             int    The number of command arguments.
 *  objv            char ** The array of 'objc' command arguments.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_InstanceCommand(ClientData context, Tcl_Interp *interp,
			       int objc, Tcl_Obj *CONST objv[])
{
  TkPgplot *tkpg = (TkPgplot *) context;
  char *widget;     /* The name of the widget */
  char *command;    /* The name of the command */

  SAY("tkpg_InstanceCommand\n");

/*
 * Get the name of the widget.
 */
  widget = Tcl_GetString(objv[0]);
/*
 * Get the name of the command.
 */
  if(objc < 2) {
    Tcl_AppendResult(interp, "Missing arguments to ", widget, " command.",
		     NULL);
    return TCL_ERROR;
  };
  command = Tcl_GetString(objv[1]);
#ifdef DODEBUG
  printf("  command=%s\n", command);
#endif
/*
 * Prevent untimely deletion of the widget while this function runs.
 * Note that following this statement you must return via
 * tkpg_InstanceCommand_return() to ensure that Tcl_Release() gets called.
 */
  Tcl_Preserve(context);
/*
 * Check for recognized command names.
 */
  if(strcmp(command, "xview") == 0) {             /* X-axis scroll-bar update */
    return tkpg_InstanceCommand_return(context,
		      tkpg_scrollbar_callback(tkpg, interp, widget, command,
					      objc-2, objv+2));
  } else if(strcmp(command, "yview") == 0) {      /* Y-axis scroll-bar update */
    return tkpg_InstanceCommand_return(context,
		      tkpg_scrollbar_callback(tkpg, interp, widget, command,
					      objc-2, objv+2));
  } else if(strcmp(command, "configure") == 0) {  /* Configure widget */
/*
 * Check the number of configure arguments.
 */
    switch(objc - 2) {
    case 0:   /* Return the values of all configuration options */
      return tkpg_InstanceCommand_return(context,
			Tk_ConfigureInfo(interp, tkpg->tkwin, configSpecs,
					 (char *) tkpg, NULL, 0));
      break;
    case 1:   /* Return the value of a single given configuration option */
      return tkpg_InstanceCommand_return(context,
		        Tk_ConfigureInfo(interp, tkpg->tkwin, configSpecs,
					 (char *) tkpg, Tcl_GetString(objv[2]), 0));
      break;
    default:  /* Change one of more of the configuration options */
      return tkpg_InstanceCommand_return(context,
			tkpg_Configure(tkpg, interp, objc-2, objv+2,
				       TK_CONFIG_ARGV_ONLY));
      break;
    };
  } else if(strcmp(command, "cget") == 0) {  /* Get a configuration value */
    if(objc != 3) {
      Tcl_AppendResult(interp, "Wrong number of arguments to \"", widget,
		       " cget\" command", NULL);
      return tkpg_InstanceCommand_return(context, TCL_ERROR);
    } else {
      return tkpg_InstanceCommand_return(context,
			Tk_ConfigureValue(interp, tkpg->tkwin, configSpecs,
					  (char *) tkpg, Tcl_GetString(objv[2]), 0));
    };
  } else if(strcmp(command, "setcursor") == 0) { /* Augment the cursor */
    return tkpg_InstanceCommand_return(context,
			tkpg_tcl_setcursor(tkpg, interp, objc - 2, objv + 2));
  } else if(strcmp(command, "clrcursor") == 0) { /* Clear cursor augmentation */
    tkpg_ClrCursor(tkpg);
    return tkpg_InstanceCommand_return(context, TCL_OK);
  } else if(strcmp(command, "world") == 0) {  /* Pixel to world coordinates */
    return tkpg_InstanceCommand_return(context,
				       tkpg_tcl_world(tkpg, interp, widget,
						      objc-2, objv+2));
  } else if(strcmp(command, "pixel") == 0) {  /* World to pixel coordinates */
    return tkpg_InstanceCommand_return(context,
				       tkpg_tcl_pixel(tkpg, interp, widget,
						      objc-2, objv+2));
  } else if(strcmp(command, "id") == 0) {     /* PGPLOT id of widget */
    return tkpg_InstanceCommand_return(context,
				       tkpg_tcl_id(tkpg, interp, widget,
						   objc-2, objv+2));
  } else if(strcmp(command, "device") == 0) { /* PGPLOT name for the widget */
    return tkpg_InstanceCommand_return(context,
				       tkpg_tcl_device(tkpg, interp, widget,
						       objc-2, objv+2));
  };
/*
 * Unknown command name.
 */
  Tcl_AppendResult(interp, "Unknown command \"", widget, " ", command, "\"",
		   NULL);
  return tkpg_InstanceCommand_return(context, TCL_ERROR);
}

/*.......................................................................
 * This is a private cleanup-return function of tkpg_InstanceCommand().
 * It should be used to return from said function after Tcl_Preserve() has
 * been called. It calls Tcl_Release() on the widget to unblock deletion
 * and returns the specified error code.
 *
 * Input:
 *  context   ClientData    The tkpg widget cast to (ClientData).
 *  iret             int    TCL_OK or TCL_ERROR.
 * Output:
 *  return           int    The value of iret.
 */
static int tkpg_InstanceCommand_return(ClientData context, int iret)
{
  Tcl_Release(context);
  return iret;
}

/*.......................................................................
 * This function is services TCL commands for a given widget.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record to be configured.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *  flags            int    The flags argument of Tk_ConfigureWidget():
 *                           0                - No flags.
 *                           TK_CONFIG_ARGV   - Override the X defaults
 *                                              database and the configSpecs
 *                                              defaults.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_Configure(TkPgplot *tkpg, Tcl_Interp *interp,
			  int objc, Tcl_Obj *CONST objv[], int flags)
{
/*
 * Get the X-window pgplot object.
 */
  PgxWin *pgx = tkpg->pgx;
/*
 * Install the new defaults in tkpg.
 */
  SAY("tkpg_Configure\n");

#ifdef DODEBUG
  { int i;
  for (i=0; i<objc; i++) {
    printf(" %s", Tcl_GetString(objv[i]));
  }
  printf("\n");
  }
#endif

  if(Tk_ConfigureWidget(interp, tkpg->tkwin, configSpecs, objc, objv,
			(char *) tkpg, flags) == TCL_ERROR)
    return TCL_ERROR;
/*
 * Install the background color in PGPLOT color-index 0.
 */
  pgx_set_background(pgx, Tk_3DBorderColor(tkpg->border));
/*
 * Install the foreground color in PGPLOT color-index 1.
 */
  pgx_set_foreground(pgx, tkpg->normalFg);
/*
 * Install changes to window attributes.
 */
  {
    XSetWindowAttributes attr;  /* The attribute-value container */
    unsigned long mask = 0;     /* The set of attributes that have changed */
    attr.background_pixel = pgx->color->pixel[0];
    mask |= CWBackPixel;
    attr.colormap = pgx->color->cmap;
    mask |= CWColormap;
    attr.border_pixel = pgx->color->pixel[0];
    mask |= CWBorderPixel;
    attr.do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask |
      KeyPressMask | KeyReleaseMask;
    mask |= CWDontPropagate;
    Tk_ChangeWindowAttributes(tkpg->tkwin, mask, &attr);
  };
/*
 * Tell Tk what window size we want.
 */
  Tk_GeometryRequest(tkpg->tkwin, tkpg->req_width, tkpg->req_height);
/*
 * Tell pgxwin that the clip margin may have changed.
 */
  tkpg_update_clip(tkpg);
/*
 * Update the optional window margins.
 */
  pgx_set_margin(pgx, tkpg->padx, tkpg->pady);
/*
 * Refresh the window.
 */
  tkpg_refresh_window(tkpg);
  SAY("tkpg_Configure FINISH\n");
  return TCL_OK;
}

/*.......................................................................
 * This is the main X event callback for Pgplot widgets.
 * 
 * Input:
 *  context   ClientData    The tkpg widget cast to (ClientData).
 *  event         XEvent *  The event that triggered the callback.
 */
static void tkpg_EventHandler(ClientData context, XEvent *event)
{
  TkPgplot *tkpg = (TkPgplot *) context;
/*
 * Determine what type of event triggered this call.
 */
  switch(event->type) {
  case ConfigureNotify:   /* The window has been resized */
    SAY("ConfigureNotify\n");
    tkpg->scroll.x = 0;
    tkpg->scroll.y = 0;
    tkpg_update_clip(tkpg);
    tkpg_update_scroll_bars(tkpg);
    tkpg_refresh_window(tkpg);
    break;
  case DestroyNotify:     /* The window has been destroyed */
/*
 * Delete the cursor event handler to prevent further use by user.
 */
    tkpg_ClrCursor(tkpg);
/*
 * Delete the main event handler to prevent prolonged use.
 */
    Tk_DeleteEventHandler(tkpg->tkwin, tkpg->events.mask, tkpg_EventHandler,
			  (ClientData) tkpg);
/*
 * Tell del_TkPgplot() that we have already deleted the event mask.
 */
    tkpg->events.mask = NoEventMask;
/*
 * Force the functions in pgxwin.c to discard subsequent graphics.
 */
    if(tkpg->pgx)
      tkpg->pgx->window = None;
/*
 * Queue deletion of tkpg until all references to the widget have been
 * completed.
 */
    Tcl_EventuallyFree(context, tkpg_FreeProc);
    break;
  case FocusIn:           /* Keyboard-input focus has been acquired */
    tkpg->events.focus_acquired = 1;
    tkpg_draw_focus_highlight(tkpg);
    break;
  case FocusOut:          /* Keyboard-input focus has been lost */
    tkpg->events.focus_acquired = 0;
    tkpg_draw_focus_highlight(tkpg);
    break;
  case Expose:            /* Redraw the specified area */
    tkpg_expose_handler(tkpg, event);
    break;
  };
  return;
}

/*.......................................................................
 * The expose-event handler for PGPLOT widgets.
 *
 * Input:
 *  tkpg   TkPgplot * The Tk Pgplot widget.
 *  event    XEvent   The expose event that invoked the callback.
 */
static void tkpg_expose_handler(TkPgplot *tkpg, XEvent *event)
{
/*
 * Re-draw the focus-highlight border.
 */
  tkpg_draw_focus_highlight(tkpg);
/*
 * Re-draw the 3D borders.
 */
  tkpg_draw_3d_border(tkpg);
/*
 * Re-draw the damaged area.
 */
  pgx_expose(tkpg->pgx, event);
  return;
}

/*.......................................................................
 * Re-draw the focus highlight border if it has a finite size.
 *
 * Input:
 *  tkpg    TkPgplot * The Tk Pgplot widget.
 */
static void tkpg_draw_focus_highlight(TkPgplot *tkpg)
{
  Window w = Tk_WindowId(tkpg->tkwin);
/*
 * Re-draw the focus-highlight border.
 */
  if(tkpg->highlight_thickness != 0) {
    GC gc = Tk_GCForColor(tkpg->events.focus_acquired ?
			  tkpg->highlightColor : tkpg->highlightBgColor,
			  w);
    Tk_DrawFocusHighlight(tkpg->tkwin, gc, tkpg->highlight_thickness, w);
  };
  return;
}

/*.......................................................................
 * Re-draw the 3D border if necessary.
 *
 * Input:
 *  tkpg    TkPgplot * The Tk Pgplot widget.
 */
static void tkpg_draw_3d_border(TkPgplot *tkpg)
{
  Tk_Window tkwin = tkpg->tkwin;
  Window w = Tk_WindowId(tkwin);
/*
 * Re-draw the focus-highlight border.
 */
  if(tkpg->border && tkpg->borderWidth > 0) {
    int margin = tkpg->highlight_thickness;
    Tk_Draw3DRectangle(tkwin, w, tkpg->border, margin, margin,
		       Tk_Width(tkwin) - 2*margin, Tk_Height(tkwin) - 2*margin,
		       tkpg->borderWidth, tkpg->relief);
  };
  return;
}

/*.......................................................................
 * Augment the cursor of a given widget.
 *
 * Input:
 *  tkpg       TkPgplot * The PGPLOT widget to connect a cursor to.
 *  mode TkpgCursorMode   The type of cursor augmentation.
 *  xref,yref     float   The world-coordinate reference point for band-type
 *                        cursors.
 *  ci              int   The color index with which to plot the cursor,
 *                        or -1 to select the current foreground color.
 * Output:
 *  return          int   TCL_OK or TCL_ERROR.
 */
static int tkpg_SetCursor(TkPgplot *tkpg, TkpgCursorMode mode,
			  float xref, float yref, int ci)
{
  PgxWin *pgx = tkpg->pgx;
  float rbuf[2];
/*
 * Remove any existing cursor augmentation.
 */
  tkpg_ClrCursor(tkpg);
/*
 * Mark the cursor as active.
 */
  tkpg->events.cursor_active = 1;
/*
 * Convert xref, yref from world coordinates to device coordinates.
 */
  rbuf[0] = xref;
  rbuf[1] = yref;
  pgx_world2dev(pgx, rbuf);
/*
 * Raise the cursor.
 */
  if(pgx_set_cursor(pgx, ci, (int)mode, 0, rbuf, rbuf)) {
    Tcl_AppendResult(tkpg->interp, "Unable to display cursor.\n", NULL);
    tkpg_ClrCursor(tkpg);
    return TCL_ERROR;
  };
/*
 * If the pointer is currently in the window, record its position
 * and draw the cursor.
 */
  if(pgx_locate_cursor(pgx))
    pgx_draw_cursor(pgx);
/*
 * Create an event handler to handle asychronous cursor input.
 */
  Tk_CreateEventHandler(tkpg->tkwin, CURSOR_EVENT_MASK,
			tkpg_CursorHandler, (ClientData) tkpg);
  return TCL_OK;
}

/*.......................................................................
 * This is the X event callback for Pgplot cursor events. It is called
 * only when the cursor augmentation has been established by
 * tkpg_SetCursor() and not cleared by tkpg_ClrCursor().
 * 
 * Input:
 *  context   ClientData    The tkpg widget cast to (ClientData).
 *  event         XEvent *  The event that triggered the callback.
 */
static void tkpg_CursorHandler(ClientData context, XEvent *event)
{
  TkPgplot *tkpg = (TkPgplot *) context;
  PgxWin *pgx = tkpg->pgx;
  float rbuf[2];
  char key;
/*
 * Handle the event. Note that button-press and keyboard events
 * have not been selected so the return values are irrelevent.
 */
  (void) pgx_cursor_event(pgx, event, rbuf, &key);
/*
 * Handle errors.
 */
  if(pgx->bad_device)
    tkpg_ClrCursor(tkpg);
  return;
}

/*.......................................................................
 * Clear the cursor of a given widget.
 *
 *  tkpg  TkPgplot * The widget to disconnect the cursor from.
 */
static void tkpg_ClrCursor(TkPgplot *tkpg)
{
  if(tkpg) {
    PgxWin *pgx = tkpg->pgx;
/*
 * Do nothing if the cursor is inactive.
 */
    if(tkpg->events.cursor_active) {
/*
 * Remove the current event handler.
 */
      Tk_DeleteEventHandler(tkpg->tkwin, CURSOR_EVENT_MASK,
			    tkpg_CursorHandler, (ClientData) tkpg);
/*
 * Reset the cursor context to its inactive state.
 */
      tkpg->events.cursor_active = 0;
/*
 * Erase the cursor.
 */
      pgx_erase_cursor(pgx);
      pgx_set_cursor(pgx, 0, TKPG_NORM_CURSOR, 0, NULL, NULL);
    };
  };
  return;
}

/*.......................................................................
 * Augment the cursor as specified in the arguments of the setcursor
 * widget command.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record to be configured.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *                           [0] The type of cursor augmentation, from:
 *                                norm  - Un-augmented X cursor
 *                                line  - Line cursor between ref and pointer
 *                                rect  - Rectangle between ref and pointer
 *                                yrng  - Horizontal lines at ref.x & pointer.x
 *                                xrng  - Vertical lines at ref.y & pointer.y
 *                                hline - Horizontal line cursor at y=ref.y
 *                                vline - Vertical line cursor at x=ref.x
 *                                cross - Pointer centered cross-hair
 *                           [1] The X-axis world coordinate at which
 *                               to anchor rect,yrng and xrng cursors.
 *                           [2] The Y-axis world coordinate at which
 *                               to anchor rect,yrng and xrng cursors.
 *                           [3] The color index of the cursor.
 *  flags            int    The flags argument of Tk_ConfigureWidget():
 *                           0                - No flags.
 *                           TK_CONFIG_ARGV   - Override the X defaults
 *                                              database and the configSpecs
 *                                              defaults.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_tcl_setcursor(TkPgplot *tkpg, Tcl_Interp *interp,
			      int objc, Tcl_Obj *CONST objv[])
{
  TkpgCursorMode mode;  /* Cursor augmentation mode */
  double xref,yref;    /* The X and Y reference positions of the cursor */
  int ci;              /* The color index used to draw the cursor */
  int found = 0;       /* True once the mode has been identified */
  int i;
/*
 * List the correspondence between cursor-mode names and pgband() mode
 * enumerators.
 */
  struct {
    TkpgCursorMode mode;
    char *name;
  } modes[] = {
    {TKPG_NORM_CURSOR, "norm"},  /* Un-augmented X cursor */
    {TKPG_LINE_CURSOR, "line"},  /* Line cursor between ref and pointer */
    {TKPG_RECT_CURSOR, "rect"},  /* Rectangle between ref and pointer */
    {TKPG_YRNG_CURSOR, "yrng"},  /* Horizontal lines at ref.x & pointer.x */
    {TKPG_XRNG_CURSOR, "xrng"},  /* Vertical lines at ref.y & pointer.y */
    {TKPG_HLINE_CURSOR, "hline"},/* Horizontal line cursor at y=ref.y */
    {TKPG_VLINE_CURSOR, "vline"},/* Vertical line cursor at x=ref.x */
    {TKPG_CROSS_CURSOR, "cross"},/* Pointer centered cross-hair */
  };
/*
 * Check that we have the expected number of arguments.
 */
  if(objc != 4) {
    Tcl_AppendResult(interp, "Wrong number of arguments. Should be: \"",
		     tkpg->name, " setcursor mode x y ci",
		     /*tkpg->pgx->name, " setcursor mode x y ci",*/
		     NULL);
    return TCL_ERROR;
  };
/*
 * Make sure that the widget is currently open to PGPLOT.
 */
  if(tkpg->pgslct_id == 0) {
    Tcl_AppendResult(interp, tkpg->name,
		     " setcursor: Widget not open to PGPLOT.", NULL);
    return TCL_ERROR;
  };
/*
 * Lookup the cursor mode.
 */
  mode = TKPG_NORM_CURSOR;
  for(i=0; !found && i<sizeof(modes)/sizeof(modes[0]); i++) {
    if(strcmp(modes[i].name, Tcl_GetString(objv[0])) == 0) {
      found = 1;
      mode = modes[i].mode;
    };
  };
/*
 * Mode not found?
 */
  if(!found) {
    Tcl_AppendResult(interp, "Unknown PGPLOT cursor mode \"", Tcl_GetString(objv[0]),
		     "\". Should be one of:", NULL);
    for(i=0; i<sizeof(modes)/sizeof(modes[0]); i++)
      Tcl_AppendResult(interp, " ", modes[i].name, NULL);
    return TCL_ERROR;
  };
/*
 * Read the cursor X and Y coordinate.
 */
  if(Tcl_GetDouble(interp, Tcl_GetString(objv[1]), &xref) == TCL_ERROR ||
     Tcl_GetDouble(interp, Tcl_GetString(objv[2]), &yref) == TCL_ERROR)
    return TCL_ERROR;
/*
 * Get the color index to use when drawing the cursor.
 */
  if(Tcl_GetInt(interp, Tcl_GetString(objv[3]), &ci) == TCL_ERROR)
    return TCL_ERROR;
/*
 * Delegate the rest of the work to tkpg_SetCursor().
 */
  return tkpg_SetCursor(tkpg, mode, xref, yref, ci);
}

/*.......................................................................
 * This is a Tk_FreeProc() wrapper function around del_TkPgplot(),
 * suitable for use with Tcl_EventuallyFree().
 *
 * Input:
 *  context   ClientData  The tkpg widget to be deleted, cast to
 *                        ClientData.
 */
static void tkpg_FreeProc(char *context)
{
  (void) del_TkPgplot((TkPgplot *) context);
}

/*.......................................................................
 * Refresh the contents of the window.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record to be configured.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int tkpg_refresh_window(TkPgplot *tkpg)
{
  if(Tk_IsMapped(tkpg->tkwin)) {
    tkpg_draw_focus_highlight(tkpg);
    tkpg_draw_3d_border(tkpg);
    return pgx_scroll(tkpg->pgx, tkpg->scroll.x, tkpg->scroll.y);
  };
  return 0;
}

/*.......................................................................
 * Whenever the color representation of the background color is changed
 * via PGPLOT, this function is called to update the Tk 3D border.
 *
 * Input:
 *  tkpg     TkPgplot *   The associated PGPLOT widget.
 */
static void tkpg_update_border(TkPgplot *tkpg)
{
  XColor *bg;     /* The new background color */
  char cname[20]; /* The color as a string of the form #rrrrggggbbbb */
  Tk_3DBorder bd; /* The new Tk border */
/*
 * Get the PGPLOT background color.
 */
  bg = &tkpg->pgx->color->xcolor[0];
/*
 * Tk_Get3DBorder requires a standard X color resource string.
 */
  sprintf(cname, "#%4.4hx%4.4hx%4.4hx", bg->red, bg->green, bg->blue);
  bd = Tk_Get3DBorder(tkpg->interp, tkpg->tkwin, cname);
  if(bd) {
/*
 * Replace the previous border with the new one.
 */
    if(tkpg->border)
      Tk_Free3DBorder(tkpg->border);
    tkpg->border = bd;
    tkpg_draw_3d_border(tkpg);
  } else {
    fprintf(stderr, "Tk_Get3DBorder failed: %s\n", Tcl_GetString(Tcl_GetObjResult(tkpg->interp)));
  };
}

/*.......................................................................
 * Respond to an xview or yview scrollbar command.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record to be configured.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  view            char *  "xview" or "yview".
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_scrollbar_callback(TkPgplot *tkpg, Tcl_Interp *interp, 
				  char *widget, char *view, int objc,
				  Tcl_Obj *CONST objv[])
{
  int window_size;  /* The size of the window along the direction of motion */
  int pixmap_size;  /* The size of the pixmap along the direction of motion */
  int new_start_pos;/* The new pixmap coord of the top|left of the window */
  int old_start_pos;/* The old pixmap coord of the top|left of the window */
/*
 * Fill in the current scroll-statistics along the requested direction.
 */
  if(*view == 'x') {
    window_size = Tk_Width(tkpg->tkwin);
    pixmap_size = pgx_pixmap_width(tkpg->pgx);
    old_start_pos = tkpg->scroll.x;
  } else {
    window_size = Tk_Height(tkpg->tkwin);
    pixmap_size = pgx_pixmap_height(tkpg->pgx);
    old_start_pos = tkpg->scroll.y;
  };
/*
 * The first argument specifies what form of scrollbar command has
 * been received (see 'man scrollbar' for details).
 */
  if(objc < 1) {
    return tkpg_scrollbar_error(tkpg, interp, widget, view, objc, objv);
/*
 * The moveto command requests a new start position as a
 * fraction of the pixmap size.
 */
  } else if(strcmp(Tcl_GetString(objv[0]), "moveto")==0) {
    double fractional_position;
    if(objc != 2)
      return tkpg_scrollbar_error(tkpg, interp, widget, view, objc, objv);
/*
 * Read the fractional position.
 */
    if(Tcl_GetDouble(interp, Tcl_GetString(objv[1]), &fractional_position) == TCL_ERROR)
      return TCL_ERROR;
    new_start_pos = fractional_position * pixmap_size;
/*
 * The "scroll" command specifies an increment to move the pixmap by
 * and the units to which the increment refers.
 */
  } else if(strcmp(Tcl_GetString(objv[0]), "scroll")==0) {
    int scroll_increment;
    if(objc != 3)
      return tkpg_scrollbar_error(tkpg, interp, widget, view, objc, objv);
/*
 * Read the scroll-increment.
 */
    if(Tcl_GetInt(interp, Tcl_GetString(objv[1]), &scroll_increment) == TCL_ERROR)
      return TCL_ERROR;
/*
 * The unit of the increment can either be "units", which in our case
 * translates to a single pixel, or "pages", which corresponds to the
 * width/height of the window.
 */
    if(strcmp(Tcl_GetString(objv[2]), "units")==0) {
      new_start_pos = old_start_pos + scroll_increment;
    } else if(strcmp(Tcl_GetString(objv[2]), "pages")==0) {
      int page_size = window_size - 2 *
	(tkpg->highlight_thickness + tkpg->borderWidth);
      if(page_size < 0)
	page_size = 0;
      new_start_pos = old_start_pos + scroll_increment * page_size;
    } else {
      return tkpg_scrollbar_error(tkpg, interp, widget, view, objc, objv);
    };
  } else {
    Tcl_AppendResult(interp, "Unknown xview command \"", Tcl_GetString(objv[0]), "\"", NULL);
    return TCL_ERROR;
  };
/*
 * Keep the pixmap visible.
 */
  if(new_start_pos < 0 || window_size > pixmap_size) {
    new_start_pos = 0;
  } else if(new_start_pos + window_size > pixmap_size) {
    new_start_pos = pixmap_size - window_size;
  };
/*
 * Record the top left corner of the new scrolling-area.
 */
  if(*view == 'x')
    tkpg->scroll.x = new_start_pos;
  else
    tkpg->scroll.y = new_start_pos;
/*
 * Update the scrolled area and the scrollbar slider.
 */
  tkpg_update_scroll_bars(tkpg);
  return TCL_OK;
}

/*.......................................................................
 * This is a private error-return function of tkpg_scrollbar_callback().
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  view            char *  "xview" or "yview".
 *  objc             int    The number of arguments in objv.
 *  objv            char ** The array of 'objc' configuration arguments.
 * Output:
 *  return           int    TCL_ERROR and the context of the error
 *                          is recorded in interp->result.
 */
static int tkpg_scrollbar_error(TkPgplot *tkpg, Tcl_Interp *interp, 
			       char *widget, char *view, int objc,
			       Tcl_Obj *CONST objv[])
{
  int i;
  Tcl_AppendResult(interp, "Bad command: ", widget, " ", view, NULL);
  for(i=0; i<objc; i++)
    Tcl_AppendResult(interp, " ", Tcl_GetString(objv[i]), NULL);
  Tcl_AppendResult(interp, "\nAfter \"widget [xy]view\", use one of:\n \"moveto <fraction>\" or \"scroll -1|1 units|pages\"", NULL);
  return TCL_ERROR;
}


/*.......................................................................
 * Implement the Tcl world function. This converts an X-window
 * pixel coordinate to the corresponding PGPLOT world coordinate.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *                           [0] The coordinate axes to convert, from:
 *                                "x"  - Convert an X-axis coord.
 *                                "y"  - Convert a Y-axis coord.
 *                                "xy" - Convert a an X Y axis pair.
 *                           [1] An X-axis pixel coordinate if [0][0] is
 *                               'x'.
 *                               A Y-axis pixel coordinate if [0][0] is
 *                               'y'.
 *                           [2] This is only expected if [0]=="xy". It
 *                               should then contain the Y-axis
 *                               coordinate to be converted.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_tcl_world(TkPgplot *tkpg, Tcl_Interp *interp, 
			 char *widget, int objc, Tcl_Obj *CONST objv[])
{
  int xpix, ypix;     /* The input X window coordinate */
  float rbuf[2];      /* The conversion buffer */
  char *axis;         /* The axis specification string */
  enum {BAD_AXIS, X_AXIS, Y_AXIS, XY_AXIS}; /* Enumerated axis type */
  int axtype;         /* The decoded axis type */
  char *usage = " world [x <xpix>]|[y <xpix>]|[xy <xpix> <ypix>]";
/*
 * Check that an axis specification argument has been provided.
 */
  if(objc < 1) {
    Tcl_AppendResult(interp, "Usage: ", widget, usage, NULL);
    return TCL_ERROR;
  };
/*
 * Decode the axis type and check the expected argument count.
 */
  axis = Tcl_GetString(objv[0]);
  axtype = BAD_AXIS;
  switch(*axis++) {
  case 'x':
    switch(*axis++) {
    case 'y':
      if(*axis == '\0' && objc == 3)
	axtype = XY_AXIS;
      break;
    case '\0':
      if(objc == 2)
	axtype = X_AXIS;
      break;
    };
    break;
  case 'y':
    if(*axis == '\0' && objc == 2)
      axtype = Y_AXIS;
    break;
  };
/*
 * Unrecognised axis description?
 */
  if(axtype == BAD_AXIS) {
    Tcl_AppendResult(interp, "Usage: ", widget, usage, NULL);
    return TCL_ERROR;
  };
/*
 * Get the pixel coordinates to be converted.
 */
  switch(axtype) {
  case X_AXIS:
    if(Tcl_GetInt(interp, Tcl_GetString(objv[1]), &xpix) == TCL_ERROR)
      return TCL_ERROR;
    ypix = 0;
    break;
  case Y_AXIS:
    xpix = 0;
    if(Tcl_GetInt(interp, Tcl_GetString(objv[1]), &ypix) == TCL_ERROR)
      return TCL_ERROR;
    break;
  case XY_AXIS:
    if(Tcl_GetInt(interp, Tcl_GetString(objv[1]), &xpix) == TCL_ERROR ||
       Tcl_GetInt(interp, Tcl_GetString(objv[2]), &ypix) == TCL_ERROR)
      return TCL_ERROR;
    break;
  };
/*
 * Convert the pixel coordinates to world coordinates.
 */
  pgx_win2dev(tkpg->pgx, xpix, ypix, rbuf);
  pgx_dev2world(tkpg->pgx, rbuf);
/*
 * Write the world coordinate(s) into the reply string.
 */
  switch(axtype) {
  case X_AXIS:
    Tcl_DoubleResults(interp, 1, 0, rbuf[0]);
    break;
  case Y_AXIS:
    Tcl_DoubleResults(interp, 1, 0, rbuf[1]);
    break;
  case XY_AXIS:
    Tcl_DoubleResults(interp, 2, 0, rbuf[0], rbuf[1]);
    break;
  };
  return TCL_OK;
}

/*.......................................................................
 * Implement the Tcl pixel function. This converts PGPLOT world
 * coordinates to X-window pixel coordinates.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *                           [0] The coordinate axes to convert, from:
 *                                "x"  - Convert an X-axis coord.
 *                                "y"  - Convert a Y-axis coord.
 *                                "xy" - Convert a an X Y axis pair.
 *                           [1] An X-axis world coordinate if [0][0] is
 *                               'x'.
 *                               A Y-axis world coordinate if [0][0] is
 *                               'y'.
 *                           [2] This is only expected if [0]=="xy". It
 *                               should then contain the Y-axis
 *                               coordinate to be converted.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_tcl_pixel(TkPgplot *tkpg, Tcl_Interp *interp, 
			 char *widget, int objc, Tcl_Obj *CONST objv[])
{
  double wx, wy;      /* The world X and Y coordinates to be converted */
  int xpix, ypix;     /* The output X window coordinate */
  float rbuf[2];      /* The conversion buffer */
  char *axis;         /* The axis specification string */
  enum {BAD_AXIS, X_AXIS, Y_AXIS, XY_AXIS}; /* Enumerated axis type */
  int axtype;         /* The decoded axis type */
  char *usage = " pixel [x <x>]|[y <x>]|[xy <x> <y>]";
/*
 * Check that an axis specification argument has been provided.
 */
  if(objc < 1) {
    Tcl_AppendResult(interp, "Usage: ", widget, usage, NULL);
    return TCL_ERROR;
  };
/*
 * Decode the axis type and check the expected argument count.
 */
  axis = Tcl_GetString(objv[0]);
  axtype = BAD_AXIS;
  switch(*axis++) {
  case 'x':
    switch(*axis++) {
    case 'y':
      if(*axis == '\0' && objc == 3)
	axtype = XY_AXIS;
      break;
    case '\0':
      if(objc == 2)
	axtype = X_AXIS;
      break;
    };
    break;
  case 'y':
    if(*axis == '\0' && objc == 2)
      axtype = Y_AXIS;
    break;
  };
/*
 * Unrecognised axis description?
 */
  if(axtype == BAD_AXIS) {
    Tcl_AppendResult(interp, "Usage: ", widget, usage, NULL);
    return TCL_ERROR;
  };
/*
 * Get the pixel coordinates to be converted.
 */
  switch(axtype) {
  case X_AXIS:
    if(Tcl_GetDouble(interp, Tcl_GetString(objv[1]), &wx) == TCL_ERROR)
      return TCL_ERROR;
    wy = 0;
    break;
  case Y_AXIS:
    wx = 0;
    if(Tcl_GetDouble(interp, Tcl_GetString(objv[1]), &wy) == TCL_ERROR)
      return TCL_ERROR;
    break;
  case XY_AXIS:
    if(Tcl_GetDouble(interp, Tcl_GetString(objv[1]), &wx) == TCL_ERROR ||
       Tcl_GetDouble(interp, Tcl_GetString(objv[2]), &wy) == TCL_ERROR)
      return TCL_ERROR;
    break;
  };
/*
 * Convert the world coordinate to pixel coordinates.
 */
  rbuf[0] = wx;
  rbuf[1] = wy;
  pgx_world2dev(tkpg->pgx, rbuf);
  pgx_dev2win(tkpg->pgx, rbuf, &xpix, &ypix);
/*
 * Write the pixel coordinate(s) into the reply string.
 */
  switch(axtype) {
  case X_AXIS:
    /* Note: Tcl_IntResults(interp, count, append, ...)  requires
       append to be 1 when count is 1. See tkGlue.c in Perl/Tk code */
    Tcl_IntResults(interp, 1, 1, xpix);
    break;
  case Y_AXIS:
    /* Note: Tcl_IntResults(interp, count, append, ...)  requires
       append to be 1 when count is 1. See tkGlue.c in Perl/Tk code */
    Tcl_IntResults(interp, 1, 1, ypix);
    break;
  case XY_AXIS:
    Tcl_IntResults(interp, 2, 0, xpix, ypix);
    break;
  };
  return TCL_OK;
}

/*.......................................................................
 * Implement the Tcl "return PGPLOT id" function.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *                          (None are expected).
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_tcl_id(TkPgplot *tkpg, Tcl_Interp *interp, 
		      char *widget, int objc, Tcl_Obj *CONST objv[])
{
/*
 * There shouldn't be any arguments.
 */
  if(objc != 0) {
    Tcl_AppendResult(interp, "Usage: ", widget, " id", NULL);
    return TCL_ERROR;
  };
/*
 * Return the id in the Tcl result string.
 */
  sprintf(tkpg->buffer, "%d", tkpg->pgslct_id);
  Tcl_AppendResult(interp, tkpg->buffer, NULL);
  return TCL_OK;
}

/*.......................................................................
 * Implement the Tcl "return PGPLOT device specifier" function.
 *
 * Input:
 *  tkpg        TkPgplot *  The widget record.
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  widget          char *  The name of the PGPLOT widget.
 *  objc             int    The number of configuration arguments.
 *  objv            char ** The array of 'objc' configuration arguments.
 *                          (None are expected).
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int tkpg_tcl_device(TkPgplot *tkpg, Tcl_Interp *interp, 
			   char *widget, int objc, Tcl_Obj *CONST objv[])
{
/*
 * There shouldn't be any arguments.
 */
  if(objc != 0) {
    Tcl_AppendResult(interp, "Usage: ", widget, " device", NULL);
    return TCL_ERROR;
  };
/*
 * Return the device specifier in the Tcl result string.
 */
  Tcl_AppendResult(interp, tkpg->device, NULL);
  return TCL_OK;
}

/*.......................................................................
 * Return the toplevel window ID of a given tk pathname.
 *
 * Input:
 *  interp  Tcl_Interp *  The TCL intrepreter.
 *  main_w   Tk_Window    The main window of the application.
 *  path          char *  The tk path name of a window.
 * Output:
 *  return   Tk_Window    The top-level window of the path, or NULL if
 *                        it doesn't exist. In the latter case an error
 *                        message will have been appended to interp->result.
 */
static Tk_Window tkpg_toplevel_of_path(Tcl_Interp *interp, Tk_Window main_w,
				       char *path)
{
  char *endp;   /* The element in path[] following the first path component */
  char *first;  /* A copy of the first component of the pathname */
  int length;   /* The length of the first component of the pathname */
  Tk_Window w;  /* The Tk window of the first component of the pathname */
/*
 * The first character of the path should be a dot.
 */

  SAY("tkpg_toplevel_of_path\n");
  if(!path || *path == '\0' || *path != '.') {
    Tcl_AppendResult(interp, "Unknown window: ", path ? path : "(null)",
		     NULL);
    return NULL;
  };

  SAY("1\n");
/*
 * Find the end of the first component of the pathname.
 */
  for(endp=path+1; *endp && *endp != '.'; endp++)
    ;
  length = endp - path;
/*
 * Make a copy of the name of the first component of the path name.
 */
  first = malloc(length + 1);
  if(!first) {
    Tcl_AppendResult(interp, "Ran out of memory while finding toplevel window.",
		     NULL);
    return NULL;
  };
  strncpy(first, path, length);
  first[length] = '\0';

  SAY("2\n");
#ifdef DODEBUG
  printf("first=%s\n", first);
#endif
/*
 * Lookup the corresponding window.
 */
  w = Tk_NameToWindow(interp, first, main_w);
/*
 * Discard the copy.
 */
  free(first);

  SAY("3\n");

/*
 * If the window doesn't exist, Tk_NameToWindow() is documented to place
 * an error message in interp->result, so just return the error condition.
 */
  if(!w)
    return NULL;
/*
 * If the looked up window is a toplevel window return it, otherwise
 * the toplevel for the specified path must be the main window.
 */
  SAY("Got to end\n");
  return Tk_IsTopLevel(w) ? w : main_w;
}
