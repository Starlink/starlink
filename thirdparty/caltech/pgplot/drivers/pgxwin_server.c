#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

/*
 * The following headers are included to define the setpgid() prototype.
 */
#ifndef VMS
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#endif

/*
 * Client/server communication protocol revision number.
 */
#define PGXWIN_REVISION 0

/*
 * Allow the expose-event handler name to be changed by compile
 * time pre-definition of PGXWIN_SERVER. If pgxwin_server is modified in
 * such a way as to become incompatible with an earlier version of xwdriv.c,
 * its name should be changed by postfixing a small increasing integer
 * to the name of the executable. New and old pgplot programs can then
 * coexist as long as both versions of the server are
 * retained. In order not to clutter up system directories, don't change
 * this name unless absolutely necessary. This is also the name given to
 * the server selection atom, so be sure that it remains valid for this
 * purpose.
 */
#ifndef PGXWIN_SERVER
#define PGXWIN_SERVER "pgxwin_server"
#endif

#define XW_MAX_COLORS 256     /* Max number of colors to allocate */
                              /* Note that the line_of_pixels opcode */
                              /* assumes that there are at most 256 colors */
#define XW_MIN_COLORS 2       /* Min number of colors per colormap */
#define XW_DEF_COLORS 100     /* Default number of colors to allocate */
#define NCOLORS 16            /* Number of pre-defined PGPLOT colors */
#define XW_IMAGE_LEN 1280     /* Length of the line-of-pixels buffer */
#define COLORMULT 65535       /* Normalized color intensity multiplier */

#define XW_DEV_NAME "XWINDOW (X Window display)" /* PGPLOT device name */
#define XW_WINDOW_NAME "PGPLOT Window" /* Window title */
#define XW_ICON_NAME "PGPLOT"          /* Name to place under the icon */
#define XW_BORDER ((unsigned int)4)    /* Window border width (pixels) */

#define XW_DEF_ASPECT (8.5/11.0)   /* Default aspect (height/width) of window */
#define XW_DEF_WIDTH 867           /* Default width (pixels) */
#define XW_DEF_HEIGHT ((int) XW_DEF_WIDTH * XW_DEF_ASPECT)
                                   /* Default height (pixels) */
#define XW_MIN_WIDTH 64            /* Minimum width (pixels) */
#define XW_MIN_HEIGHT 64           /* Minimum height (pixels) */

/*
 * Define equivalence values for the XParseGeometry bitmask bits, using
 * values agreed upon by xwdriv.c and pgxwin_server.c, for use in
 * communicating geometries between client and server.
 */
#define XW_WidthValue 1
#define XW_HeightValue 2
#define XW_XValue 4
#define XW_YValue 8
#define XW_XNegative 16
#define XW_YNegative 32

/*
 * Enumerate the supported window close-down modes.
 */
#define XW_DELETE 1
#define XW_PERSIST 2
#define XW_ICONIZE 3

/*
 * Enumerate property-data formats, named after the internal types that
 * are used to communicate them with XChangeProperty() and
 * XGetWindowProperty().
 */
#define XW_CHAR_PROP 8
#define XW_SHORT_PROP 16
#define XW_LONG_PROP 32

/*
 * ANSI defines two exit value to indicate succesful and unsuccesful program
 * termination. We should use these. On pre-ANSI machines we will assume
 * the old K&R convention.
 */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/* A container used to record the geometry of the X-window */

typedef struct {
  int x,y;             /* Locus of top left corner of window (pixels) */
  unsigned int width;  /* Width of window (pixels) */
  unsigned int height; /* Height of window (pixels) */
  int xpix_per_inch;   /* Number of pixels per inch along X */
  int ypix_per_inch;   /* Number of pixels per inch along Y */
  int xmargin;         /* X-axis 1/4" margin in pixels */
  int ymargin;         /* Y-axis 1/4" margin in pixels */
  int xmin,xmax;       /* Min/max X-axis pixels excluding 1/4" margins */
  int ymin,ymax;       /* Min/max X-axis pixels excluding 1/4" margins */
} XWgeom;

/*
 * Declare a colormap descriptor.
 */
typedef struct {
  XVisualInfo *vi;      /* The colormap visual information descriptor */
  Colormap cmap;        /* Colormap ID */
  int ncol;             /* The number of colors available. ci = [0...ncol-1] */
  unsigned long *pixel; /* Color pixels allocated */
  int monochrome;       /* True we have to use a monochrome screen */
  int default_class;    /* The class of the default visual of the screen */
} XWcolor;

/* Declare a container for cursor IDs */

typedef struct {
  Cursor norm;   /* ID of cursor to use when cursor input is not expected */
  Cursor live;   /* ID of cursor to use when cursor input is expected */
  Cursor idle;   /* ID of cursor to use when the window is un-assigned */
} XWCursor;

/* Declare a container in which to record resource database quarks */

typedef struct { /* A Quark name/class resource pair */
  XrmQuark name;
  XrmQuark class;
} XWQPair;

typedef struct {
  XWQPair prog;       /* Program name/class quarks */
  XWQPair display;    /* .display name/class quarks */
  XWQPair server;     /* .server. name/class quark */
  XWQPair geom;       /* PGWin geometry name/class quarks */
  XWQPair iconize;    /* PGWin iconize name/class quarks */
  XWQPair acceptquit; /* PGWin acceptQuit name/class quarks */
  XWQPair mincolors;  /* PGWin minColors name/class quarks */
  XWQPair maxcolors;  /* PGWin maxColors name/class quarks */
  XWQPair visual;     /* PGWin visual name/class quarks */
  XWQPair crosshair;  /* PGWin crosshair name/class quarks */
  XWQPair visible;    /* Server visible name/class quarks */
  XWQPair icongeom;   /* iconGeometry name/class quarks */
  XrmQuark win_class; /* PGWin window-specification class quark */
} XWQuarks;

/* Declare a container to record details of a given client window */

typedef struct PGwin {
  Window window;     /* PGPLOT /xw window ID */
  Window parent;     /* Parent window of 'window' */
  Window client;     /* Window ID of client using the window */
  Pixmap pixmap;     /* Pixmap ID for buffering and expose-event handling */
  GC gc;             /* Graphical context descriptor */
  int protocol;      /* Client/server protocol revision to use */
  int id;            /* PGPLOT selection ID */
  int screen;        /* The screen on which the window appears */
  int mapped;        /* True only after the window is first mapped */
  int disposition;   /* Close-down mode: XW_PERSIST, XW_ICONIZE, XW_DELETE */
  int acceptquit;     /* True if WM_DELETE_WINDOW events are to be obeyed on */
                      /* Active windows */
  int iconize;        /* If true, iconize inactive windows if persistent */
  int mincol;         /* Min number of colors per colormap */
  int maxcol;         /* Max number of colors per colormap */
  int crosshair;      /* If true show crosshair cursor */
  int visual_class;   /* If != -1, then this is the visual class to try for. */
  XWgeom geom;       /* The size and position of the window */
  XWcolor color;     /* Colormap descriptor */
  XWCursor *curs;    /* Pointer to xw->col_cursor or xw->gry_cursor */
  struct PGwin *next;/* Pointer to next window in list */
} PGwin;

/* Declare a container for server data */

typedef struct {
  Display *display;   /* Connection to the display */
  XrmDatabase xrdb;   /* X resource database */
  int screen;         /* Screen number of windows */
  Window pgxwin;      /* ID of top-level window of server */
  Atom server_atom;   /* PGXWIN_SERVER selection atom */
  Atom client_data;   /* PGXWIN_CLIENT_DATA property atom */
  PGwin *active_list; /* LIFO list of active PGPLOT /xw windows */
  PGwin *closed_list; /* Sorted list of un-connected PGPLOT /xw windows */
  XWCursor col_cursor;/* Cursors for color visuals */
  XWCursor gry_cursor;/* Cursors for gray and monochrome visuals */
  XWQuarks quarks;    /* Resource database quarks */
  Pixmap icon;        /* ID of icon pixmap */
  Atom wm_protocols;  /* WM_PROTOCOLS atom */
  Atom wm_delete_win; /* WM_DELETE_WINDOW atom */
  Atom geom_atom;     /* Client/server geometry transaction atom */
} XWServer;

#ifdef __STDC__
#define ARGS(args) args
#else
#define ARGS(args) ()
#endif

static XWServer *new_XWServer ARGS((XrmDatabase xrdb));
static XWServer *del_XWServer ARGS((XWServer *xw));
static PGwin *new_PGwin ARGS((XWServer *xw, int id, int screen, Window client,
			      int disposition));
static PGwin *add_PGwin ARGS((PGwin **pglist, int sort, PGwin *pgw));
static PGwin *rem_PGwin ARGS((PGwin **pglist, PGwin *pgw));
static PGwin *del_PGwin ARGS((XWServer *xw, PGwin *pgw));
static Window xw_server_window ARGS((XWServer *xw));
static int xw_set_signals ARGS((void));
static int xw_handle_error ARGS((Display *display, XErrorEvent *event));
static int xw_event_loop ARGS((XWServer *xw));
static Bool xw_parse_bool ARGS((char *str, Bool def));
static int xw_parse_visual ARGS((char *str));
static int xw_same_string ARGS((char *s1, char *s2));
static int xw_client_message ARGS((XWServer *xw, XEvent *event));
static int xw_ret_cursor ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_ret_colors ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_ret_error ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_new_window ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_new_pixmap ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_set_geom ARGS((XWServer *xw, PGwin *pgw, int x, int y,
		       unsigned int width, unsigned int height, int mask));
static int xw_new_geom ARGS((XWServer *xw, XEvent *event, PGwin *pgw));
static int xw_get_geom ARGS((XWServer *xw, PGwin *pgw));
static int xw_wm_message ARGS((XWServer *xw, XEvent *event));
static int xw_check_destroy ARGS((XWServer *xw, XEvent *event));
static int xw_expose_win ARGS((XWServer *xw, XEvent *event));
static int xw_get_visual ARGS((XWServer *xw, PGwin *pgw));
static int xw_find_visual ARGS((XWServer *xw, PGwin *pgw, int class));
static int xw_get_colorcells ARGS((XWServer *xw, PGwin *pgw, XVisualInfo *vi,
				   Colormap cmap));
static int xw_del_Colormap ARGS((XWServer *xw, PGwin *pgw, XVisualInfo *vi,
			   Colormap cmap, int ncol));
static XVisualInfo *xw_visual_info ARGS((Display *display, int screen,
					 Visual *visual));
static int xw_prep_window ARGS((XWServer *xw, PGwin *pgw, Window client,
				int protocol));
static unsigned long xw_send_data ARGS((XWServer *xw, PGwin *pgw,
		   unsigned char *data, int form, unsigned long n, Atom type));
static int xw_ini_cursors ARGS((XWServer *xw, XWCursor *curs));
static int xw_new_cursors ARGS((XWServer *xw, int usecolor, XWCursor *curs));
static int xw_del_cursors ARGS((XWServer *xw, XWCursor *curs));
static char *xw_home_dir ARGS((void));
static XrmDatabase xw_get_xrdb ARGS((Display *display, XrmDatabase cmd_xrdb));
static int xw_get_quarks ARGS((XWServer *xw));
static char *xw_get_default ARGS((XWServer *xw, int window_id, XWQPair *pair));
static int xw_get_config ARGS((XWServer *xw, PGwin *pgw));
static int xw_setwmhints ARGS((XWServer *xw, int screen, Window window,int id));
static int xw_name_window ARGS((XWServer *xw, Window window, char *w_name,
			  char *i_name));
static int xw_sync_error ARGS((XWServer *xw));


/* List resource command-line arguments */

static XrmOptionDescRec cmd_opt[] = {
  {"-display",            ".server.display",  XrmoptionSepArg, NULL},
  {"-win_visual",         ".Win.visual",      XrmoptionSepArg, NULL},
  {"-win_iconize",        ".Win.iconize",     XrmoptionSepArg, NULL},
  {"-win_geometry",       ".Win.geometry",    XrmoptionSepArg, NULL},
  {"-win_minColors",      ".Win.minColors",   XrmoptionSepArg, NULL},
  {"-win_maxColors",      ".Win.maxColors",   XrmoptionSepArg, NULL},
  {"-win_crosshair",      ".Win.crosshair",   XrmoptionSepArg, NULL},
  {"-win_acceptQuit",     ".Win.acceptQuit",  XrmoptionSepArg, NULL},
  {"-win_iconGeometry",   ".Win.iconGeometry",XrmoptionSepArg, NULL},
  {"-server_visible",     ".server.visible",  XrmoptionSepArg, NULL},
  {"-server_iconGeometry",".server.iconGeometry", XrmoptionSepArg, NULL},
  {"-xrm",                NULL,               XrmoptionResArg, NULL}
};

/* List usage of command-line arguments */

static struct {
  char *opt;
  char *arg;
} cmd_usage[] = {
  {"-help",               ""},
  {"-display",            "display_name"},
  {"-win_visual",         "default|monochrome|pseudocolor|directcolor|staticcolor|truecolor|grayscale|staticgray"},
  {"-win_iconize",        "True|False"},
  {"-win_geometry",       "WIDTHxHEIGHT+X+Y"},
  {"-win_minColors",      "integer"},
  {"-win_maxColors",      "integer"},
  {"-win_crosshair",      "True|False"},
  {"-win_acceptQuit",     "True|False"},
  {"-win_iconGeometry",   "+X+Y"},
  {"-server_visible",     "True|False"},
  {"-server_iconGeometry","+X+Y"},
  {"-xrm",       "Resource manager string, eg. \"pgxwin.win2.maxColors: 16\""}
};

/*.......................................................................
 * PGPLOT /xw driver multi-client window server.
 *
 * Input:
 *  argv[1]  char *   The name of the display to connect to.
 */
#ifdef __STDC__
int main(int argc, char *argv[])
#else
int main(argc, argv)
  int argc; char *argv[];
#endif
{
  XWServer *xw;                /* PGPLOT /xw server descriptor */
  XrmDatabase cmd_xrdb=NULL;   /* Command-line X resource database */
  int i;
/*
 * Close stdin and stdout since we aren't going to use them.
 */
  fclose(stdin);
  fclose(stdout);
/*
 * Under UNIX make sure that we are not in the same process group as
 * the process that spawned us. Otherwise, signals sent to the parent
 * will be sent to us as well.
 */
#ifndef VMS
  setpgid(0,0);
#endif
/*
 * Reset signal handlers.
 */
  xw_set_signals();
/*
 * Get command-line X resource options.
 */
  XrmInitialize();
  XrmParseCommand(&cmd_xrdb, cmd_opt,
		  (int)(sizeof(cmd_opt)/sizeof(XrmOptionDescRec)), "pgxwin",
			 &argc, argv);
/*
 * The only legal remaining argument is -help.
 */
  if(argc > 1) {
    if(strcmp(argv[1],"-help")!=0) {
      fprintf(stderr,
	      "%s: Unknown command-line option \"%s\". Try the -help option.\n",
	      PGXWIN_SERVER, argv[1]);
    } else {
      fprintf(stderr, "Usage:\n\t %s [options]\n\n", PGXWIN_SERVER);
      fprintf(stderr, "Where legal options and their arguments include:\n");
      for(i=0; i<sizeof(cmd_usage)/sizeof(cmd_usage[0]); i++)
	fprintf(stderr, " %s\t %s\n", cmd_usage[i].opt, cmd_usage[i].arg);
      fprintf(stderr, "\n");
    };
    return EXIT_FAILURE;
  };
/*
 * Start the server.
 */
  if((xw=new_XWServer(cmd_xrdb)) == NULL)
    return EXIT_FAILURE;
/*
 * Enter the event loop.
 */
  xw_event_loop(xw);
/*
 * Clean up.
 */
  xw = del_XWServer(xw);
  return EXIT_SUCCESS;
}

/*.......................................................................
 * Open a connection to the display, check whether another server is
 * already running, and if not, create a simple unmapped window for
 * communication, install its ID in a root-window property named PGXWIN,
 * and return a descriptor for use with the new server.
 *
 * Input:
 *  xrdb XrmDatabase    The X resource database initialized from command
 *                      line arguments.
 * Output:
 *  xw      XWServer *  The descriptor of the server, or NULL on error.
 */
#ifdef __STDC__
static XWServer *new_XWServer(XrmDatabase xrdb)
#else
static XWServer *new_XWServer(xrdb)
     XrmDatabase xrdb;
#endif
{
  XWServer *xw;       /* The return descriptor */
  char *display_name; /* The name of the display */
/*
 * Allocate the container.
 */
  xw = (XWServer *) malloc(sizeof(XWServer));
  if(xw==NULL) {
    fprintf(stderr, "%s: Insufficient memory for server descriptor.\n",
	    PGXWIN_SERVER);
    return NULL;
  };
/*
 * Initialize the container, at least to the point at which it can safely be
 * passed to del_XWServer().
 */
  xw->display = NULL;
  xw->xrdb = xrdb;
  xw->screen = 0;
  xw->pgxwin = None;
  xw->server_atom = None;
  xw->client_data = None;
  xw->active_list = NULL;
  xw->closed_list = NULL;
  
  xw_ini_cursors(xw, &xw->col_cursor);
  xw_ini_cursors(xw, &xw->gry_cursor);
  xw->icon = None;
  xw->wm_protocols  = None;
  xw->wm_delete_win = None;
  xw->geom_atom = None;
/*
 * Initialize the list of resource database quarks required by xw_get_config().
 */
  if(xw_get_quarks(xw))
    return del_XWServer(xw);
/*
 * Determine the display name.
 */
  display_name = xw_get_default(xw, 0, &xw->quarks.display);
/*
 * Open a connection to the display.
 */
  if((xw->display = XOpenDisplay(display_name)) == NULL) {
    fprintf(stderr, "%s: cannot connect to X server [%s]\n", PGXWIN_SERVER,
	    XDisplayName(display_name));
    return del_XWServer(xw);
  };
/*
 * Get the X resource database for the display, combined with the
 * command-line database.
 */
  xw->xrdb = xw_get_xrdb(xw->display, xw->xrdb);
/*
 * Get the screen number referenced in the display name.
 */
  xw->screen = DefaultScreen(xw->display);
/*
 * Install an error handler for non-fatal errors. If we don't do this then
 * Xlib will do its own error handling, which includes killing the program.
 */
  XSetErrorHandler(xw_handle_error);
/*
 * Get selected window-manager atoms.
 */
  xw->wm_protocols = XInternAtom(xw->display, "WM_PROTOCOLS", False);
  xw->wm_delete_win = XInternAtom(xw->display, "WM_DELETE_WINDOW", False);
/*
 * Get the window geometry client/server transaction atom.
 */
  xw->geom_atom = XInternAtom(xw->display, "PGXWIN_GEOMETRY", False);
/*
 * Create a simple unmapped window to receive events on.
 */
  xw->pgxwin = xw_server_window(xw);
  if(xw->pgxwin==None)
    return del_XWServer(xw);
/*
 * Get the server selection atom.
 */
  xw->server_atom = XInternAtom(xw->display, PGXWIN_SERVER, False);
  if(xw->server_atom == None) {
    fprintf(stderr, "%s: Failed to obtain %s selection atom.\n",
	    PGXWIN_SERVER, PGXWIN_SERVER);
    return del_XWServer(xw);
  };
/*
 * See if another server already exists by checking if the PGXWIN_SERVER
 * selection is currently owned by another window.
 */
  if(XGetSelectionOwner(xw->display, xw->server_atom) != None) {
    fprintf(stderr, "%s: Another server is already active.\n", PGXWIN_SERVER);
    return del_XWServer(xw);
  };
/*
 * Grab ownership of the PGXWIN_SERVER selection.
 */
  XSetSelectionOwner(xw->display, xw->server_atom, xw->pgxwin, CurrentTime);
  XFlush(xw->display);
/*
 * Did another server beat us to it?
 */
  if(XGetSelectionOwner(xw->display, xw->server_atom) != xw->pgxwin) {
    fprintf(stderr, "%s: Another server is already active.\n", PGXWIN_SERVER);
    return del_XWServer(xw);
  };
/*
 * Get the client data property atom.
 */
  xw->client_data = XInternAtom(xw->display, "PGXWIN_CLIENT_DATA", False);
  if(xw->client_data == None) {
    fprintf(stderr, "%s: Failed to obtain PGXWIN_CLIENT_DATA selection atom.\n",
	    PGXWIN_SERVER);
    return del_XWServer(xw);
  };
/*
 * Create two cursors to be used by clients. The normal cursor is used
 * when cursor input is not expected, and the live cursor is used when
 * input is expected. Allocate two versions or these cursors - one for
 * color visuals and another for gray/monochrome displays.
 */
  if(xw_new_cursors(xw, 1, &xw->col_cursor) ||
     xw_new_cursors(xw, 0, &xw->gry_cursor))
    return del_XWServer(xw);
  return xw;
}

/*.......................................................................
 * Clean up, and delete a XWServer descriptor.
 *
 * Input:
 *  xw     XWServer *  The server descriptor to be deleted.
 * Output:
 *  return XWServer *  Allways NULL.
 */
#ifdef __STDC__
static XWServer *del_XWServer(XWServer *xw)
#else
static XWServer *del_XWServer(xw)
     XWServer *xw;
#endif
{
  if(xw) {
/*
 * Display connection acquired?
 */
    if(xw->display) {
/*
 * Delete the communication window.
 * This will also clear the ownership of the PGXWIN_SERVER selection.
 */
      if(xw->pgxwin != None)
	XDestroyWindow(xw->display, xw->pgxwin);
/*
 * Delete all client windows.
 */
      while(xw->active_list)
	del_PGwin(xw, rem_PGwin(&xw->active_list, xw->active_list));
      while(xw->closed_list)
	del_PGwin(xw, rem_PGwin(&xw->closed_list, xw->closed_list));
/*
 * Delete the cursors.
 */
      xw_del_cursors(xw, &xw->col_cursor);
      xw_del_cursors(xw, &xw->gry_cursor);
/*
 * Delete the icon pixmap.
 */
    if(xw->icon != None)
      XFreePixmap(xw->display, xw->icon);
/*
 * Close the connection to the display.
 */
      XCloseDisplay(xw->display);
    };
/*
 * Delete the empty container.
 */
    free((char *)xw);
  };
  return NULL;
}

/*.......................................................................
 * Create and display the server window in its iconic state.
 *
 * Input:
 *  xw    XWServer *  The descriptor of the server.
 *                    Only the 'display', 'screen' and 'wm_delete_window'
 *                    members are used.
 * Output:
 *  return  Window    The ID of the server window, or None on error.
 */
#ifdef __STDC__
static Window xw_server_window(XWServer *xw)
#else
static Window xw_server_window(xw)
     XWServer *xw;
#endif
{
  Window window = None;     /* The new server window */
  unsigned int width = 200; /* The width of the window when mapped */
  unsigned int height = 20; /* The height of the window when mapped */
  int x = 0;                /* The X-position of the window when mapped */
  int y = 0;                /* The Y-position of the window when mapped */
/*
 * Create the window. Bracket the window acquisition with
 * xw_sync_error() calls, to determine whether any window creation
 * errors occur.
 */
  xw_sync_error(xw);
  window = XCreateSimpleWindow(xw->display, DefaultRootWindow(xw->display),
				x, y, width, height, (unsigned)1,
				WhitePixel(xw->display, xw->screen),
				BlackPixel(xw->display, xw->screen));
  if(xw_sync_error(xw) || window==None) {
    fprintf(stderr, "%s: Failed to create the PGPLOT server window.\n",
	    PGXWIN_SERVER);
    return None;
  };
/*
 * Name the server window and its icon.
 */
  if(xw_name_window(xw, window, "PGPLOT Server", "pgxwin")) {
    XDestroyWindow(xw->display, window);
    return None;
  };
/*
 * Tell the window manager how to dimension and locate the window.
 */
  {
    XSizeHints *hints = XAllocSizeHints();
    if(!hints) {
      fprintf(stderr, "%s: Insufficient memory.\n", PGXWIN_SERVER);
      XDestroyWindow(xw->display, window);
      return None;
    };
    hints->flags = PPosition | PSize;
    hints->x = x;
    hints->y = y;
    hints->width  = width;
    hints->height = height;
    XSetWMNormalHints(xw->display, window, hints);
    XFree((char *)hints);
  };
/*
 * Set window manager hints to tell the window manager that the
 * initial state of the window when mapped, should be iconic.
 */
  if(xw_setwmhints(xw, xw->screen, window, 0)) {
    XDestroyWindow(xw->display, window);
    return None;
  };
/*
 * Arrange to be informed of window manager "delete window" actions.
 */
  XSetWMProtocols(xw->display, window, &xw->wm_delete_win, 1);
/*
 * Display the server window if requested.
 */
  {
    char *def = xw_get_default(xw, 0, &xw->quarks.visible);
    if(xw_parse_bool(def, True) == True)
      XMapWindow(xw->display, window);
  };
  XFlush(xw->display);
  return window;
}

/*.......................................................................
 * Remove a PGPLOT /xw window from a given list of windows.
 *
 * Input:
 *  pglist  Pgwins ** Pointer to the head of the window list.  
 *  pgw      PGwin *  The descriptor of the window to be deleted.
 * Output:
 *  return   PGwin *  The descriptor of the removed window, or NULL
 *                    if not found.
 */
#ifdef __STDC__
static PGwin *rem_PGwin(PGwin **pglist, PGwin *pgw)
#else
static PGwin *rem_PGwin(pglist, pgw)
     PGwin **pglist; PGwin *pgw;
#endif
{
  PGwin *prev;  /* Descriptor of window before current position in list */
  PGwin *next;  /* Descriptor of next window to be checked */
/*
 * Search for the location of the window on the xw->pgwins list.
 */
  prev = NULL;
  next = *pglist;
  while(next!=NULL && next!=pgw) {
    prev = next;
    next = next->next;
  };
/*
 * Window not found?
 */
  if(next==NULL) {
    fprintf(stderr, "%s(rem_PGwin): No such window.\n", PGXWIN_SERVER);
    return NULL;
  };
/*
 * Relink around the window.
 */
  if(prev==NULL)
    *pglist = next->next;
  else
    prev->next = next->next;
/*
 * The descriptor is no longer in a list.
 */
  next->next = NULL;
/*
 * Return the window descriptor.
 */
  return next;
}

/*.......................................................................
 * Add a PGPLOT /xw window to a given list of windows. The window is
 * is inserted such that the list is maintained in order of window
 * number.
 *
 * Input:
 *  pglist  Pgwins ** Pointer to the head of the window list.  
 *  sort       int    If true, insert the window such that a list sorted
 *                    in order of increasing pgw->number is maintained
 *                    in that order. Otherwise insert at the head of
 *                    the list to implement a LIFO list.
 *  pgw      PGwin *  The descriptor of the window to be added.
 * Output:
 *  return   PGwin *  The descriptor of the added window.
 */
#ifdef __STDC__
static PGwin *add_PGwin(PGwin **pglist, int sort, PGwin *pgw)
#else
static PGwin *add_PGwin(pglist, sort, pgw)
     PGwin **pglist; int sort; PGwin *pgw;
#endif
{
  PGwin *prev;   /* Pointer to previous window in list */
  PGwin *next;   /* Pointer to next window in list */
  if(pgw==NULL) {
    fprintf(stderr, "%s(add_PGwin): NULL window descriptor.\n", PGXWIN_SERVER);
    return NULL;
  };
/*
 * Maintain a sorted list?
 */
  if(sort) {
/*
 * Find the correct position for the window in the window list.
 */
    prev = NULL;
    next = *pglist;
    while(next && next->id < pgw->id) {
      prev = next;
      next = next->next;
    };
/*
 * Insert the window between 'prev' and 'next'.
 */
    pgw->next = next;
    if(prev==NULL)
      *pglist = pgw;
    else
      prev->next = pgw;
/*
 * To implement a LIFO list, insert the window at the head of the list.
 */
  } else {
    pgw->next = *pglist;
    *pglist = pgw;
  };
  return pgw;
}

/*.......................................................................
 * Delete a PGPLOT /xw window. Note that if the descriptor comes from
 * a list of windows, it must first be removed from the list by
 * rem_PGwin().
 *
 * Input:
 *  xw    XWServer *  The descriptor of the server.
 *  pgw      PGwin *  The descriptor of the window to be deleted.
 * Output:
 *  return   PGwin *  Allways NULL.
 */
#ifdef __STDC__
static PGwin *del_PGwin(XWServer *xw, PGwin *pgw)
#else
static PGwin *del_PGwin(xw, pgw)
     XWServer *xw; PGwin *pgw;
#endif
{
  if(pgw) {
/*
 * Remove the PGPLOT window from the display.
 */
    if(pgw->window != None)
      XUnmapWindow(xw->display, pgw->window);
/*
 * Destroy the graphical context descriptor.
 */
    if(pgw->gc)
      XFreeGC(xw->display, pgw->gc);
/*
 * Delete the colormap, any private color cells and the visual info descriptor.
 */
    if(pgw->color.vi) {
      xw_del_Colormap(xw, pgw, pgw->color.vi, pgw->color.cmap,
		      pgw->color.ncol);
      XFree((char *)pgw->color.vi);
      pgw->color.vi = NULL;
    };
/*
 * Delete the array of pixel indexes.
 */
    if(pgw->color.pixel)
      free((char *)pgw->color.pixel);
/*
 * Destroy the PGPLOT /xw window.
 */
    if(pgw->window != None)
      XDestroyWindow(xw->display, pgw->window);
/*
 * Destroy its pixmap.
 */
    if(pgw->pixmap != None)
      XFreePixmap(xw->display, pgw->pixmap);
/*
 * Delete the container.
 */
    free((char *)pgw);
  };
  return NULL;
}

/*.......................................................................
 * Set up signal handlers.
 */
#ifdef __STDC__
static int xw_set_signals(void)
#else
static int xw_set_signals()
#endif
{
  signal(SIGINT, SIG_DFL);
/*
 * We shouldn't be receiving any alarms, but just in case we do, arrange
 * to ignore them.
 */
#ifdef SIGALRM
  signal(SIGALRM, SIG_IGN);
#endif
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_DFL);
#endif
  signal(SIGTERM, SIG_DFL);
  signal(SIGFPE, SIG_DFL);
#ifdef SIGABRT
  signal(SIGABRT, SIG_DFL);
#endif
#ifdef SIGQUIT
  signal(SIGQUIT, SIG_DFL);
#endif
/*
 * We have arranged for this process to be a process group leader,
 * and the only process in its group (to avoid receiving signals sent to
 * the process that created it). POSIX.1 says that if such a process is
 * orphaned when in a stopped state, the process will be sent SIGHUP
 * followed by SIGCONT. Arrange to ignore the SIGHUP.
 */
#ifdef SIGHUP
  signal(SIGHUP, SIG_IGN);
#endif
#ifdef SIGPOLL
  signal(SIGPOLL, SIG_DFL);
#endif
  return 0;
}

/*.......................................................................
 * This function is called by X whenever a non-fatal error occurs
 * on a given display connection. For the moment it does nothing but
 * count such errors in an internal static error counter. This counter
 * can then be queried and reset by sending a NULL error event pointer.
 *
 * Input:
 *  display    Display *  The display connection on which the error occured.
 *  event  XErrorEvent *  The descriptor of the error event, or NULL to
 *                        request that the error counter be queried and reset.
 * Output:
 *  return         int    The return value is not specified by Xlib, so
 *                        for Xlib calls we will simply return 0. For
 *                        none Xlib calls (distinguishable by sending
 *                        event==NULL), the value of the error counter
 *                        is returned.
 */
#ifdef __STDC__
static int xw_handle_error(Display *display, XErrorEvent *event)
#else
static int xw_handle_error(display, event)
     Display *display; XErrorEvent *event;
#endif
{
  static int error_count = 0;
/*
 * To query and reset the error counter, this program calls xw_handle_error()
 * with a NULL error event pointer. This distinguishes it from a call
 * from Xlib.
 */
  if(!event) {
    int ret_count = error_count;
    error_count = 0;   /* Reset the error counter */
    return ret_count;  /* Return the pre-reset value of the error counter */
#ifdef DEBUG
  } else {
    char errtxt[81]; /* Buffer to receive error message in */
/*
 * Get a message describing the error.
 */
    XGetErrorText(display, (int)event->error_code, errtxt, (int)sizeof(errtxt));
    fprintf(stderr, "%s: XErrorEvent: %s\n", PGXWIN_SERVER, errtxt);
/*
 * Report the operation that caused it. These opcode numbers are listed in
 * <X11/Xproto.h>.
 */
    fprintf(stderr, "%s: Major opcode: %d, Resource ID: 0x%lx%s.\n",
	 PGXWIN_SERVER, (int) event->request_code,
	 (unsigned long) event->resourceid,
         (event->resourceid==DefaultRootWindow(display)?" (Root window)":""));
#endif
  };
/*
 * Keep a record of the number of errors that have occurred since the
 * error counter was last cleared.
 */
  error_count++;
  return 0;
}

/*.......................................................................
 * This is the main server event loop, which listens for client events
 * on client windows and the main event window. It returns only on
 * server shutdown.
 *
 * Input:
 *  xw   XWServer *   The server descriptor.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_event_loop(XWServer *xw)
#else
static int xw_event_loop(xw)
     XWServer *xw;
#endif
{
  XEvent event;          /* The descriptor of the lastest X-event */
  int have_selection=1;  /* If the server selection is stolen set this to 0 */
/*
 * Enter the event loop.
 */
  do {
    XNextEvent(xw->display, &event);
    switch(event.type) {
/*
 * Handle client-server messages.
 */
    case ClientMessage:
      if(event.xclient.message_type == xw->wm_protocols) {
	if(xw_wm_message(xw, &event))
	  return 0;
      } else {
	if(xw_client_message(xw, &event))
	  return 0;
      };
      break;
/*
 * Has somebody else grabbed the selection atom?
 */
    case SelectionClear:
      fprintf(stderr, "%s: Server selection usurped.\n", PGXWIN_SERVER);
      have_selection = 0;
      break;
/*
 * Refuse all selection requests.
 */
    case SelectionRequest:
      {
	XEvent reply;
	reply.xselection.type = SelectionNotify;
	reply.xselection.requestor = event.xselectionrequest.requestor;
	reply.xselection.selection = event.xselectionrequest.selection;
	reply.xselection.target = event.xselectionrequest.target;
	reply.xselection.time = event.xselectionrequest.time;
	reply.xselection.property = None;
	XSendEvent(xw->display, event.xselection.requestor, False, (long)0,
		   &reply);
      };
      break;
/*
 * Handle window exposure events.
 */
    case Expose:
      if(xw_expose_win(xw, &event))
	return 0;
      break;
/*
 * Watch for client windows being destroyed.
 */
    case DestroyNotify:
      if(xw_check_destroy(xw, &event))
	return 0;
      break;
    default:
      break;
    };
/*
 * Quit if the selection atom has been stolen and no clients are connected.
 */
  } while(have_selection || xw->closed_list!=NULL);
  return 0;
}

/*.......................................................................
 * Handle messages sent to the server by PGPLOT /xw clients.
 *
 * Input:
 *  xw               XWServer *  The server descriptor.
 *  event  XEvent *  The descriptor of the message.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Close server.
 */
#ifdef __STDC__
static int xw_client_message(XWServer *xw, XEvent *event)
#else
static int xw_client_message(xw, event)
     XWServer *xw; XEvent *event;
#endif
{
  Window client;  /* Source client window */
  PGwin *pgw;     /* Descriptor of the associated PGPLOT window */
/*
 * The window argument contains the window of the client that sent the
 * event.
 */
  client = event->xclient.window;
/*
 * New window required?
 */
  if(event->xclient.message_type == XA_WINDOW) {
    xw_new_window(xw, event, (PGwin *)0);
  } else {
/*
 * Locate the descriptor of the PGPLOT /xw window that is connected to
 * the client (or NULL if not yet connected).
 */
    for(pgw=xw->active_list; pgw && pgw->client!=client; pgw=pgw->next);
/*
 * Handle the specified message type.
 */
    if(pgw) {
      Atom type = event->xclient.message_type;
      switch(type) {
      case XA_PIXMAP:
	xw_new_pixmap(xw, event, pgw);
	break;
      case XA_CURSOR:
	xw_ret_cursor(xw, event, pgw);
	break;
      case XA_COLORMAP:
	xw_ret_colors(xw, event, pgw);
	break;
      default:
	if(type == xw->geom_atom)
	  xw_new_geom(xw, event, pgw);
	else
	  xw_ret_error(xw, event, pgw);  /* Unknown message type */
	break;
      };
    };
  };
  return 0;
}

/*.......................................................................
 * Reply to a client that has requested the IDs of the normal and active
 * cursors.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_ret_cursor(XWServer *xw, XEvent *event, PGwin *pgw)
#else
static int xw_ret_cursor(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
/*
 * Assign the cursor IDs to the return message descriptor.
 */
  event->xclient.data.l[0] = pgw->curs->norm;
  event->xclient.data.l[1] = pgw->curs->live;
  event->xclient.data.l[2] = pgw->crosshair;
  if(!XSendEvent(xw->display, pgw->client, False, (long)0, event))
    return 1;
  XFlush(xw->display);
  return 0;
}

/*.......................................................................
 * Reply to a client that has requested an unknown message type by
 * returning a message with the message_type field set to None.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window (or NULL
 *                    if this is not known).
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_ret_error(XWServer *xw, XEvent *event, PGwin *pgw)
#else
static int xw_ret_error(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
  event->xclient.message_type = None;
  if(!XSendEvent(xw->display, event->xclient.window, False, (long)0, event))
    return 1;
  XFlush(xw->display);
  return 0;
}

/*.......................................................................
 * Reply to a client that has requested colormap details.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_ret_colors(XWServer *xw, XEvent *event,
			 PGwin *pgw)
#else
static int xw_ret_colors(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
/*
 * Assign the colormap attributes to the return message descriptor.
 */
  event->xclient.data.l[0] = pgw->color.cmap;
  event->xclient.data.l[1] = pgw->color.ncol;
  if(!XSendEvent(xw->display, pgw->client, False, (long)0, event))
    return 1;
  XFlush(xw->display);
/*
 * If any colors were allocated, send the client the colormap pixel
 * indexes by placing them in the PGXWIN_CLIENT_DATA property on the
 * client's communication window.
 */
  if(pgw->color.cmap != None && xw_send_data(xw, pgw,
					 (unsigned char *)&pgw->color.pixel[0],
					 XW_LONG_PROP,
					 (unsigned long)pgw->color.ncol,
					 XA_INTEGER)==0)
    return 1;
  return 0;
}

/*.......................................................................
 * Respond to a client request for a new window.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_new_window(XWServer *xw, XEvent *event, PGwin *pgw)
#else
static int xw_new_window(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
  int protocol = event->xclient.data.l[0];/* Protocol revision */
  int id = event->xclient.data.l[1];      /* Window number requested */
  int screen = event->xclient.data.l[2];  /* Screen to put window on */
  int disposition = event->xclient.data.l[3]; /* Close-down disposition */
  Window client = event->xclient.window;  /* Client communication window */
/*
 * Limit the requested communication protocol to one that we can handle.
 */
  if(protocol > PGXWIN_REVISION)
    protocol = PGXWIN_REVISION;
/*
 * Treat -ve window ids as equivalent to the 0 wildcard.
 */
  if(id<0)
    id = 0;
/*
 * New window required?
 */
  if(pgw==NULL) {
/*
 * See if the request can be satisfied by a currently unassigned
 * window.
 */
    if(xw->closed_list != NULL) {
/*
 * Use the first window if no particular number has been requested.
 */
      if(id==0)
	id = xw->closed_list->id;
/*
 * Search for a window that has the required numeric id.
 */
      for(pgw=xw->closed_list; pgw && pgw->id != id; pgw=pgw->next);
/*
 * If found, remove the window from the inactive list and register
 * it to the new client.
 */
      if(pgw) {
	rem_PGwin(&xw->closed_list, pgw);
	pgw->client = client;
      };
    };
/*
 * If a window with the required ID was found, but it is displayed on
 * a different screen than the client now wants, destroy it so that a
 * new window of the requested ID can be created on the specified screen.
 */
    if(pgw && pgw->screen != screen)
      pgw = del_PGwin(xw, pgw);
/*
 * Create a new window?
 */
    if(pgw==NULL) {
/*
 * Determine an unused window number if no number was specified.
 * Note that the window list is arranged in increasing order
 * of window number.
 */
      if(id==0) {
	PGwin *tmpwin = xw->active_list;
	for(id=1; tmpwin && tmpwin->id==id; tmpwin=tmpwin->next,id++);
      };
/*
 * See if the requested ID is already in use.
 */
      for(pgw=xw->active_list; pgw && pgw->id != id; pgw=pgw->next);
/*
 * If the required id is not in use create a new window.
 */
      pgw = pgw ? NULL : new_PGwin(xw, id, screen, client, disposition);
    };
/*
 * If a new window was acquired, add it to the active list and prepare it
 * for use. If during preparing the new client window, it turns out to have
 * been destroyed, return the PGPLOT window to the inactive list.
 */
    if(pgw) {
      if(xw_prep_window(xw, pgw, client, protocol)) {
	add_PGwin(&xw->closed_list, 0, pgw);
	pgw = NULL;
      };
    };
  };
/*
 * If a window was assigned to the new client record its details in
 * the return client-message descriptor.
 */
  if(pgw && pgw->client == client) {
    event->xclient.data.l[0] = pgw->protocol;    
    event->xclient.data.l[1] = pgw->id;
    event->xclient.data.l[2] = pgw->window;
    event->xclient.data.l[3] = pgw->disposition;
  } else {
    event->xclient.data.l[0] = protocol;
    event->xclient.data.l[1] = 0;
    event->xclient.data.l[2] = None;  /* No window available */
    event->xclient.data.l[3] = 0;
  };
  if(!XSendEvent(xw->display, client, False, (long)0, event))
    return 1;
  XFlush(xw->display);
  return 0;
}

/*.......................................................................
 * Set and return the window geometry.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_new_geom(XWServer *xw, XEvent *event, PGwin *pgw)
#else
static int xw_new_geom(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
  int x = event->xclient.data.l[0];
  int y = event->xclient.data.l[1];
  unsigned int width = event->xclient.data.l[2];
  unsigned int height = event->xclient.data.l[3];
  int xw_mask = event->xclient.data.l[4];
  int mask = 0; /* XParseGeometry() bit-mask */
/*
 * Translate from the PGXWIN defined bitmap values to the local
 * XParseGeometry() bitmap values.
 */
  if(xw_mask & XW_WidthValue)
    mask |= WidthValue;
  if(xw_mask & XW_HeightValue)
    mask |= HeightValue;
  if(xw_mask & XW_XValue)
    mask |= XValue;
  if(xw_mask & XW_YValue)
    mask |= YValue;
  if(xw_mask & XW_XNegative)
    mask |= XNegative;
  if(xw_mask & XW_YNegative)
    mask |= YNegative;  
/*
 * Install the new geometry in the pgw->geom descriptor.
 */
  xw_set_geom(xw, pgw, x,y, width,height, mask);
/*
 * Update the window-manager size hints for the current window.
 */
  {
    XSizeHints *hints = XAllocSizeHints();
    if(hints) {
      hints->flags = USPosition | USSize | PMinSize;
      hints->x = pgw->geom.x;
      hints->y = pgw->geom.y;
      hints->width = pgw->geom.width;
      hints->height = pgw->geom.height;
      hints->min_width = XW_MIN_WIDTH;
      hints->min_height = XW_MIN_HEIGHT;
/*
 * Instate the new size hints in the WM_NORMAL_HINTS property.
 */
      XSetWMNormalHints(xw->display, pgw->window, hints);
      XFree((char *)hints);
    };
  };
/*
 * Resize the window if requested.
 */
  if(mask & (WidthValue | HeightValue))
    XResizeWindow(xw->display, pgw->window, pgw->geom.width, pgw->geom.height);
/*
 * Move the window if requested.
 */
  if(mask & (XValue | YValue))
    XMoveWindow(xw->display, pgw->window, pgw->geom.x, pgw->geom.y);
/*
 * If the window has not previously been mapped, map it now.
 */
  if(!pgw->mapped) {
    XMapRaised(xw->display, pgw->window);
    pgw->mapped = 1;
  };
/*
 * Return details of the new geometry to the client.
 */
  event->xclient.data.l[0] = pgw->geom.x;
  event->xclient.data.l[1] = pgw->geom.y;
  event->xclient.data.l[2] = pgw->geom.width;
  event->xclient.data.l[3] = pgw->geom.height;
  if(!XSendEvent(xw->display, pgw->client, False, (long)0, event))
    return 1;
  XFlush(xw->display);
  return 0;
}

/*.......................................................................
 * Respond to a client request for a new pixmap.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 *  pgw      PGwin *  Descriptor of the client PGPLOT window.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_new_pixmap(XWServer *xw, XEvent *event,
			 PGwin *pgw)
#else
static int xw_new_pixmap(xw, event, pgw)
     XWServer *xw; XEvent *event; PGwin *pgw;
#endif
{
  unsigned long fill_pixel = event->xclient.data.l[0];
/*
 * Delete the current pixmap if it doesn't have the required size.
 */
  if(pgw->pixmap != None) {
    Window root;
    int x, y;
    unsigned width, height, border, depth;
/*
 * Determine the size of the existing pixmap.
 */
    XGetGeometry(xw->display, pgw->pixmap, &root, &x, &y, &width, &height,
		 &border, &depth);
/*
 * If the pixmap doesn't have a size equal to that requested in the last
 * PGXWIN_GEOMETRY transaction, delete it.
 */
    if(width != pgw->geom.width || height != pgw->geom.height) {
      XFreePixmap(xw->display, pgw->pixmap);
      pgw->pixmap = None;
    };
  };
/*
 * Create a new pixmap if necessary.
 */
  if(pgw->pixmap == None) {
/*
 * Bracket the pixmap acquisition with xw_sync_error() calls, to
 * determine whether any errors occur.
 */
    xw_sync_error(xw);
    pgw->pixmap = XCreatePixmap(xw->display, pgw->window, pgw->geom.width,
			   pgw->geom.height, (unsigned) pgw->color.vi->depth);
    if(xw_sync_error(xw) || pgw->pixmap==None) {
      fprintf(stderr, "%s: Failed to allocate %dx%d pixmap.\n", PGXWIN_SERVER,
	      pgw->geom.width, pgw->geom.height);
      pgw->pixmap = None;
    };
  };
/*
 * Set the fill-color to that specified by the client.
 */
  XSetForeground(xw->display, pgw->gc, fill_pixel);
/*
 * Clear the pixmap.
 */
  if(pgw->pixmap != None)
    XFillRectangle(xw->display, pgw->pixmap, pgw->gc, 0, 0,
		   pgw->geom.width, pgw->geom.height);
/*
 * Clear the window.
 */
  XClearWindow(xw->display, pgw->window);
  XFlush(xw->display);
/*
 * Return the ID of the new pixmap and its size.
 */
  event->xclient.data.l[0] = pgw->pixmap;
  if(!XSendEvent(xw->display, pgw->client, False, (long)0, event))
    return 1;
  XFlush(xw->display);
  return 0;
}

/*.......................................................................
 * Check a resource string value against boolean values.
 *
 * Input:
 *  str       char *  The string value to be tested (NULL is ok).
 *  def       Bool    The default boolean value to take if the string
 *                    matches none of the recognized boolean strings.
 * Output:
 *  return    Bool    The boolean value of the string.
 */
#ifdef __STDC__
static Bool xw_parse_bool(char *str, Bool def)
#else
static Bool xw_parse_bool(str, def)
     char *str; Bool def;
#endif
{
/*
 * Check for truth values.
 */
  if(xw_same_string(str, "true") || xw_same_string(str, "yes") ||
     xw_same_string(str, "t")    || xw_same_string(str, "on")  ||
     xw_same_string(str, "1"))
    def = True;
/*
 * Check for false values.
 */
  else if(xw_same_string(str, "false") || xw_same_string(str, "no") ||
     xw_same_string(str, "f")    || xw_same_string(str, "off")  ||
     xw_same_string(str, "0"))
    def = False;
  return def;
}

/*.......................................................................
 * Check a resource string value against visual class names.
 *
 * Input:
 *  str       char *  The string value to be tested (NULL is ok).
 * Output:
 *  return     int    The Visual class parsed, or -1 to select the default.
 */
#ifdef __STDC__
static int xw_parse_visual(char *str)
#else
static int xw_parse_visual(str)
     char *str;
#endif
{
/*
 * Create a lookup table of recognised visual classes.
 */
  static struct {
    char *name;  /* Name of visual class */
    int class;   /* Enumerated identifier of visual class */
  } classes[] = {
    {"monochrome",  -2},
    {"default",     -1},
    {"pseudocolor", PseudoColor},
    {"directcolor", TrueColor},    /* We can't handle DirectColor */
    {"staticcolor", StaticColor},
    {"truecolor",   TrueColor},
    {"grayscale",   GrayScale},
    {"staticgray",  StaticGray}
  };
  int i;
/*
 * Lookup the given class name.
 */
  if(str) {
    for(i=0; i<sizeof(classes)/sizeof(classes[0]); i++) {
      if(xw_same_string(str, classes[i].name))
	return classes[i].class;
    };
/*
 * Class name not recognised.
 */
    fprintf(stderr, "%s: Unrecognised visual type: \"%s\".\n",
	    PGXWIN_SERVER, str);
  };
  return -1;
}

/*.......................................................................
 * Perform a case-insensitive string comparison. Leading and trailing
 * white-space are not significant.
 *
 * Input:
 *  s1,s2  char *   The strings to be compared.
 * Output:
 *  return  int     0 - The strings are not the same.
 *                  1 - The strings are the same.
 */
#ifdef __STDC__
static int xw_same_string(char *s1, char *s2)
#else
static int xw_same_string(s1, s2)
     char *s1; char *s2;
#endif
{
/*
 * If either of the strings are NULL pointers, report that they are equal
 * only if both are NULL.
 */
  if(s1==NULL || s2==NULL)
    return s1==NULL && s2==NULL;
/*
 * Skip leading white-space.
 */
  while(*s1 && isspace(*s1))
    s1++;
  while(*s2 && isspace(*s2))
    s2++;
/*
 * Find the section of the strings that are identical.
 */
  while(*s1 && *s2 &&
   (islower(*s1) ? toupper(*s1) : *s1) == (islower(*s2) ? toupper(*s2) : *s2)) {
    s1++;
    s2++;
  };
/*
 * Skip trailing white-space.
 */
  while(*s1 && isspace(*s1))
    s1++;
  while(*s2 && isspace(*s2))
    s2++;
/*
 * Are the strings equal?
 */
  return (*s1=='\0' && *s2=='\0');
}

/*.......................................................................
 * Record a given window geometry. As much or as little information may
 * be provided as is known and defaults will be substituted as necessary.
 * The values and mask returned by the XParseGeometry() Xlib function
 * may be presented directly to this function. If pgw->window!=None the
 * defaults will be those of the current window. The other defaults
 * use the appropriate combination of XW_DEF_WIDTH and XW_DEF_ASPECT
 * macros, the optional PGPLOT_XW_WIDTH environment variable and where
 * positions are not given, the size of the display, used to center the plot.
 *
 * Input:
 *  xw      XWServer *  The PGPLOT /xw server descriptor. Only the display
 *                      and screen members are required.
 *  pgw        PGwin *  The PGPLOT window descriptor.
 *  mask         int    A bit mask to specify which values have been provided
 *                      and how they should be interpretted. The mask is the
 *                      union of the following:
 *                        WidthValue  -  Use the given width value.
 *                       HeightValue  -  Use the given height value.
 *                            XValue  -  Use the given value of 'x'.
 *                            YValue  -  Use the given value of 'y'.
 *                         XNegative  -  x is wrt the right of the display.
 *                         YNegative  -  y is wrt the left of the display.
 *  x            int    The left edge of the window.
 *  y            int    The top edge of the window.
 *  width   unsigned    The width of the window.
 *  height  unsigned    The height of the window.
 * Output:
 *  pgw->geom XWgeom    The new window geometry.
 *  return       int    0 - OK.
 */
#ifdef __STDC__
static int xw_set_geom(XWServer *xw, PGwin *pgw, int x, int y,
		       unsigned int width, unsigned int height, int mask)
#else
static int xw_set_geom(xw, pgw, x, y, width, height, mask)
    XWServer *xw; PGwin *pgw; int x; int y;
     unsigned int width; unsigned int height; int mask;
#endif
{
  unsigned int d_pix_width;   /* Display width in pixels */
  unsigned int d_pix_height;  /* Display height in pixels */
  unsigned int d_mm_width;    /* Display width in mm */
  unsigned int d_mm_height;   /* DIsplay height in mm */
  unsigned int w_width=0;     /* Current window width (pixels) */
  unsigned int w_height=0;    /* Current window height (pixels) */
  int w_x;                    /* Current window x offset (pixels) */
  int w_y;                    /* Current window y offset (pixels) */
/*
 * Determine the current display width and height in mm and pixels.
 */
  d_pix_width = DisplayWidth(xw->display, pgw->screen);
  d_mm_width = DisplayWidthMM(xw->display, pgw->screen);
  d_pix_height = DisplayHeight(xw->display, pgw->screen);
  d_mm_height = DisplayHeightMM(xw->display, pgw->screen);
/*
 * If the window is already open, get its attributes.
 */
  if(pgw->window != None) {
    XWindowAttributes attr;     /* Current window attributes */
    Window child;
    XGetWindowAttributes(xw->display, pgw->window, &attr);
/*
 * Translate from parent-relative to absolute X and Y window offsets.
 */
    XTranslateCoordinates(xw->display, pgw->window, attr.root,
			  0, 0, &w_x, &w_y, &child);
    w_x -= XW_BORDER;
    w_y -= XW_BORDER;
    w_width = attr.width;
    w_height = attr.height;
  };
/*
 * Ensure that all given values are positive.
 */
  if((mask & XValue) && x < 0) {
    mask |= XNegative;
    x = -x;
  };
  if((mask & YValue) && y < 0) {
    mask |= YNegative;
    y = -y;
  };
/*
 * Is either the width or height unspecified?
 */
  if(!(mask & WidthValue) || !(mask & HeightValue)) {
/*
 * Width given but not height?
 */
    if(mask & WidthValue) {
      height = (pgw->window!=None) ? w_height : (int)(width * XW_DEF_ASPECT);
/*
 * Height given but not width?
 */
    } else if(mask & HeightValue) {
      width = (pgw->window!=None) ? w_width : (int)(height / XW_DEF_ASPECT);
    }
/*
 * Neither width nor height given?
 */
    else {
      if(pgw->window != None) {
	width = w_width;
	height = w_height;
      } else {
	char *envptr;
/*
 * Use the XW_DEF_WIDTH macro to set the default width.
 */
	width = XW_DEF_WIDTH;
/*
 * The user PGPLOT_XW_WIDTH environment variable overrides the default width.
 */
	if((envptr=getenv("PGPLOT_XW_WIDTH")) != NULL) {
	  float frac_width;
	  if(sscanf(envptr, "%f", &frac_width) != 1 || frac_width <= 0.0 ||
	     frac_width > 2.0) {
	    fprintf(stderr, "%s: Ignoring bad PGPLOT_XW_WIDTH=\"%s\"\n",
		    PGXWIN_SERVER, envptr);
	  } else {
	    width = (unsigned int) (frac_width * d_pix_width);
	  };
	};
/*
 * Use a hieght given by the default aspect ratio and the default width.
 */
	height = width * XW_DEF_ASPECT;
      };
    };
  };
/*
 * Apply width and height bounds.
 */
  if(width < XW_MIN_WIDTH)
    width = XW_MIN_WIDTH;
  if(height < XW_MIN_HEIGHT)
    height = XW_MIN_HEIGHT;
/*
 * Assign the return width and height values.
 */
  pgw->geom.width = width;
  pgw->geom.height = height;
/*
 * Determine the horizontal offset of the left edge of the window.
 */
  if(mask & XValue) {
    pgw->geom.x = (mask & XNegative) ? (d_pix_width - x - pgw->geom.width) : x;
    if(pgw->geom.x < 0)
      pgw->geom.x = 0;
  } else if(pgw->window != None) {
    pgw->geom.x = w_x;
  } else {
    pgw->geom.x = (d_pix_width - pgw->geom.width) / 2;  /* Center the window */
  };
/*
 * Determine the vertical offset of the top edge of the window.
 */
  if(mask & YValue) {
    pgw->geom.y = (mask & YNegative) ?(d_pix_height - y - pgw->geom.height) : y;
    if(pgw->geom.y < 0)
      pgw->geom.y = 0;
  } else if(pgw->window != None) {
    pgw->geom.y = w_y;
  } else {
    pgw->geom.y = (d_pix_height - pgw->geom.height) / 2; /* Center the window */
  };
/*
 * Determine the device resolution in pixels per inch.
 */
  pgw->geom.xpix_per_inch = 25.4 * ((double)d_pix_width / (double)d_mm_width);
  pgw->geom.ypix_per_inch = 25.4 * ((double)d_pix_height / (double)d_mm_height);
/*
 * Determine the number of pixels needed to form a 1/4" margin around the
 * the plot area.
 */
  pgw->geom.xmargin = (int) (0.25 * pgw->geom.xpix_per_inch + 0.5);
  pgw->geom.ymargin = (int) (0.25 * pgw->geom.ypix_per_inch + 0.5);
/*
 * Determine the pixel indexes that enclose an area bounded by 1/4" margins.
 */
  pgw->geom.xmin = pgw->geom.xmargin;
  pgw->geom.xmax = pgw->geom.width - pgw->geom.xmargin;
  pgw->geom.ymin = pgw->geom.ymargin;
  pgw->geom.ymax = pgw->geom.height - pgw->geom.ymargin;
  return 0;
}

/*.......................................................................
 * Determine the default geometry for a new window.
 *
 * Input:
 *  xw    XWServer *   The PGPLOT /xw server descriptor.
 *  pgw      PGwin *   The PGPLOT window descriptor.
 * Output:
 *  return  int     0 - OK.
 *                  1 - Error.
 */
#ifdef __STDC__
static int xw_get_geom(XWServer *xw, PGwin *pgw)
#else
static int xw_get_geom(xw, pgw)
     XWServer *xw; PGwin *pgw;
#endif
{
  int x,y;             /* The offset of the window on the screen (pixels) */
  unsigned int width;  /* The width of the window (pixels) */
  unsigned int height; /* The height of the window (pixels) */
  int mask;            /* A bit mask to specify which values are known */
  char *geometry;      /* The X-default geometry string */
/*
 * Get the optional pgxwin.geometry: resource string.
 */
  geometry = xw_get_default(xw, pgw->id, &xw->quarks.geom);
/*
 * XParseGeometry() returns values specified in the geometry string and
 * a mask used to specify which values were found and how they should be
 * interpretted. If no geometry string was found, specify the mask for
 * an empty set.
 */
  mask = geometry ? XParseGeometry(geometry, &x, &y, &width, &height) : 0;
/*
 * Send the values and the selection mask to xw_set_geom(). It will fill
 * in default values for members not marked in the mask.
 */
  return xw_set_geom(xw, pgw, x, y, width, height, mask);
}

/*.......................................................................
 * Handle expose-events on client windows.
 *
 * Input:
 *  xw     XWServer *  The server descriptor.
 *  event    XEvent *  The descriptor of the expose event.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Close server.
 */
#ifdef __STDC__
static int xw_expose_win(XWServer *xw, XEvent *event)
#else
static int xw_expose_win(xw, event)
     XWServer *xw; XEvent *event;
#endif
{
  Window window;  /* The window on which the expose event was generated */
  PGwin *pgw;     /* Descriptor of the connected PGPLOT /xw window */
/*
 * The window argument contains the window of the client that sent the
 * event.
 */
  window = event->xexpose.window;
/*
 * Locate the descriptor of the PGPLOT /xw window requiring an update.
 */
  for(pgw=xw->active_list; pgw && pgw->window!=window; pgw=pgw->next);
  if(pgw==NULL)
    for(pgw=xw->closed_list; pgw && pgw->window!=window; pgw=pgw->next);
/*
 * If it is one of our windows we have a backing pixmap to repair
 * the damaged part of the window, copy the exposed area from the
 * pixmap to the window.
 */
  if(pgw && pgw->pixmap != None) {
    XCopyArea(xw->display, pgw->pixmap, pgw->window, pgw->gc,
	      event->xexpose.x, event->xexpose.y,
	      (unsigned) event->xexpose.width, (unsigned) event->xexpose.height,
	      event->xexpose.x, event->xexpose.y);
    XFlush(xw->display);
  };
  return 0;
}

/*.......................................................................
 * Handle messages sent to the server by the window manager.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Close server.
 */
#ifdef __STDC__
static int xw_wm_message(XWServer *xw, XEvent *event)
#else
static int xw_wm_message(xw, event)
     XWServer *xw; XEvent *event;
#endif
{
  PGwin *pgw;     /* Descriptor of a PGPLOT window */
  Window window;  /* Window to which the message was sent */
/*
 * Hopefully the window argument contains the window of the client that
 * sent the event. My book doesn't say.
 */
  window = event->xclient.window;
/*
 * Handle the specific window-manager message.
 */
  if(event->xclient.data.l[0] == xw->wm_delete_win) {  /* Delete window */
/*
 * Was the message sent to the server window?
 */
    if(window == xw->pgxwin) {
/*
 * Delete inactive windows.
 */
      while(xw->closed_list)
	del_PGwin(xw, rem_PGwin(&xw->closed_list, xw->closed_list));
/*
 * If there are no remaining windows the server should be closed down.
 */
      return xw->active_list == NULL;
    } else {
/*
 * Find out if it is a window from the active list.
 */
      for(pgw=xw->active_list; pgw && pgw->window!=window; pgw=pgw->next);
      if(pgw) {
	if(pgw->acceptquit) {
	  XKillClient(xw->display, pgw->client);
	  rem_PGwin(&xw->active_list, pgw);
	  del_PGwin(xw, pgw);
	} else {
	  pgw->disposition = XW_DELETE; /* Defer deletion until inactive */
	};
      };
/*
 * Find out if it is a window from the inactive list.
 */
      for(pgw=xw->closed_list; pgw && pgw->window!=window; pgw=pgw->next);
      if(pgw) {
	rem_PGwin(&xw->closed_list, pgw);
	del_PGwin(xw, pgw);
      };
    };
  };
  return 0;
}

/*.......................................................................
 * Check a DestroyNotify event to see if it came from one of our
 * client windows. If it did, close the connection to that client.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  event   XEvent *  The descriptor of the message.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Close server.
 */
#ifdef __STDC__
static int xw_check_destroy(XWServer *xw, XEvent *event)
#else
static int xw_check_destroy(xw, event)
     XWServer *xw; XEvent *event;
#endif
{
  PGwin *pgw;     /* Descriptor of the PGPLOT window */
  Window window;  /* The window wo which the event was sent */
/*
 * The destroyed window is recorded in the 'window' member.
 */
  window = event->xdestroywindow.window;
/*
 * See if the window is a client of one of the windows managed by us.
 */
  for(pgw=xw->active_list; pgw && pgw->client!=window; pgw=pgw->next);
/*
 * If it was one of our clients, remove its PGPLOT window from the active
 * list.
 */
  if(pgw) {
    rem_PGwin(&xw->active_list, pgw);
/*
 * Mark the window as unused by changing the cursor.
 */
    XDefineCursor(xw->display, pgw->window, pgw->curs->idle);
/*
 * If the window should be retained mapped, move it to the inactive
 * window list. Otherwise delete it.
 */
    if(pgw->disposition == XW_DELETE) {
      del_PGwin(xw, pgw);
    } else {
      add_PGwin(&xw->closed_list, 0, pgw);
      if(pgw->disposition == XW_ICONIZE)
	XIconifyWindow(xw->display, pgw->window, pgw->screen);
    };
/*
 * See if one of our PGPLOT windows got destroyed by another program.
 * In principle this shouldn't happen because other programs shouldn't
 * be deleting resources that we created. Unfortunately there is at
 * least one program that does do this. The TkSteal program steals
 * windows from other applications and adds them to the window
 * hierarchy of a given Tk application. When the Tk program exits
 * the window then gets destroyed - arrgh!
 */
  } else {
/*
 * Check the list of active windows first.
 */
    for(pgw=xw->active_list; pgw && pgw->window!=window; pgw=pgw->next);
    if(pgw) {
      fprintf(stderr,
	      "\n%s: Active PGPLOT window %d destroyed by another program!\n",
	      PGXWIN_SERVER, pgw->id);
      del_PGwin(xw, rem_PGwin(&xw->active_list, pgw));
/*
 * Check the list of inactive windows if not found in the active list.
 */ 
    } else {
      for(pgw=xw->closed_list; pgw && pgw->window!=window; pgw=pgw->next);
      if(pgw)
	del_PGwin(xw, rem_PGwin(&xw->closed_list, pgw));
    };
  };
  return 0;
}

/*.......................................................................
 * Create a new PGPLOT window.
 *
 * Input:
 *  xw     XWServer * The PGPLOT /xw server descriptor.
 *  id          int   The numeric id used by PGPLOT users to refer to the
 *                    window.
 *  screen      int   The screen on which to create the window.
 *  client   Window   The window ID of the client to which the new window
 *                    is to be assigned.
 *  disposition int   The close-down mode desired for the window when
 *                    the client disconnects:
 *                      XW_PERSIST - Keep the window mapped.
 *                      XW_ICONIZE - Iconize the window.
 *                      XW_DELETE  - Delete window.
 * Output:
 *  return  PGwin * The new PGPLOT window descriptor, or NULL on error.
 */
#ifdef __STDC__
static PGwin *new_PGwin(XWServer *xw, int id, int screen, Window client,
			int disposition)
#else
static PGwin *new_PGwin(xw, id, screen, client, disposition)
     XWServer *xw; int id; int screen; Window client; int disposition;
#endif
{
  PGwin *pgw;   /* The return descriptor */
/*
 * Allocate the descriptor.
 */
  pgw = (PGwin *) malloc(sizeof(PGwin));
  if(pgw==NULL) {
    fprintf(stderr, "%s: Insufficient memory for new PGPLOT window.\n",
	    PGXWIN_SERVER);
    return del_PGwin(xw, pgw);
  };
/*
 * Initialize all members of the descriptor at least to the point at which
 * the descriptor can safely be sent to del_PGwin(). All pointers must
 * be assigned NULL and XIDs assigned None, so that del_PGwin() knows what
 * hasn't been allocated yet.
 */
  pgw->window = None;
  pgw->parent = None;
  pgw->client = client;
  pgw->protocol = PGXWIN_REVISION;
  pgw->pixmap = None;
  pgw->gc = NULL;
  pgw->id = id;
  pgw->screen = screen;
  pgw->mapped = 0;
  pgw->disposition = disposition;
  pgw->color.vi = NULL;
  pgw->color.cmap = None;
  pgw->color.ncol = 0;
  pgw->color.pixel = NULL;
  pgw->color.monochrome = 1;
  pgw->color.default_class = 0;
  pgw->curs = NULL;
  pgw->next = NULL;
/*
 * Get the configuration defaults for the window.
 */
  if(xw_get_config(xw, pgw))
    return del_PGwin(xw, pgw);
/*
 * If a persistent window has been requested, see if it should be iconized
 * when inactive.
 */
  if(pgw->disposition == XW_PERSIST && pgw->iconize)
    pgw->disposition = XW_ICONIZE;
/*
 * Record the parent window ID.
 */
  pgw->parent = RootWindow(xw->display, pgw->screen);
/*
 * Get a visual and colormap for the pending window.
 */
  if(xw_get_visual(xw, pgw))
    return del_PGwin(xw, pgw);
/*
 * Get the default geometry for the window.
 */
  if(xw_get_geom(xw, pgw))
    return del_PGwin(xw, pgw);
/*
 * Get color or black-and-white cursors for the window.
 */
  if(DisplayCells(xw->display, pgw->screen) < 10 ||
     pgw->color.default_class == GrayScale ||
     pgw->color.default_class == StaticGray) {
    pgw->curs = &xw->gry_cursor;
  } else {
    pgw->curs = &xw->col_cursor;
  };
/*
 * Create the window.
 */
  {
    XSetWindowAttributes attr;
    unsigned long mask = CWEventMask | CWDontPropagate |
                         CWBorderPixel | CWBackPixel | CWCursor;
    attr.event_mask    = ExposureMask | StructureNotifyMask;
    attr.do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask |
                            KeyPressMask | KeyReleaseMask;
    attr.border_pixel  = WhitePixel(xw->display, pgw->screen);
    attr.background_pixel = BlackPixel(xw->display, pgw->screen);
    attr.cursor = pgw->curs->idle;
    if(!pgw->color.monochrome) {
      mask |= CWColormap;
      attr.colormap = pgw->color.cmap;
    };
/*
 * Bracket the window acquisition with xw_sync_error() calls, to
 * determine whether any window creation errors occur.
 */
    xw_sync_error(xw);
    pgw->window = XCreateWindow(xw->display, pgw->parent, pgw->geom.x,
		      pgw->geom.y, pgw->geom.width, pgw->geom.height,
		      XW_BORDER, pgw->color.vi->depth, InputOutput,
		      pgw->color.vi->visual, mask, &attr);
    if(xw_sync_error(xw) || pgw->window == None) {
      fprintf(stderr,
       "%s: Failed to create window with visual: id=0x%lx class=%d depth=%u.\n",
	      PGXWIN_SERVER, (unsigned long)pgw->color.vi->visualid,
	      pgw->color.vi->class, pgw->color.vi->depth);
      fprintf(stderr, "%s: Colormap id=0x%lx.\n", PGXWIN_SERVER,
	      (unsigned long) pgw->color.cmap);
      pgw->window = None;
      return del_PGwin(xw, pgw);
    };
  };
/*
 * Arrange to be informed of window manager "delete window" actions.
 */
  XSetWMProtocols(xw->display, pgw->window, &xw->wm_delete_win, 1);
/*
 * Give the window and icon names.
 */
  {
    char window_name[sizeof(XW_WINDOW_NAME)+10];
    char icon_name[sizeof(XW_ICON_NAME)+10];
    sprintf(window_name, "%s %d", XW_WINDOW_NAME, pgw->id);
    sprintf(icon_name, "%s%d", XW_ICON_NAME, pgw->id);
    if(xw_name_window(xw, pgw->window, window_name, icon_name))
      return del_PGwin(xw, pgw);
  };
/*
 * Specify window-state hints to the window manager.
 */
  if(xw_setwmhints(xw, pgw->screen, pgw->window, pgw->id))
    return del_PGwin(xw, pgw);
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
  {
    XGCValues gcv;
    gcv.graphics_exposures = False;
    xw_sync_error(xw);
    pgw->gc = XCreateGC(xw->display, pgw->window, (unsigned long)
			(GCGraphicsExposures), &gcv);
  };
  if(xw_sync_error(xw) || pgw->gc==NULL) {
    fprintf(stderr,
	    "%s: Failed to allocate graphical context for window 0x%lx.\n",
	    PGXWIN_SERVER, (unsigned long) pgw->window);
    return del_PGwin(xw, pgw);
  };
/*
 * Return the initialized descriptor for use.
 */
  return pgw;
}

/*.......................................................................
 * Set up the visual and colormap for the /xw window.
 *
 * Input:
 *  xw  XWServer *  The PGPLOT /xw server descriptor.
 *  pgw    PGwin *  The PGPLOT window descriptor.
 * Output:
 *  pgw->color.vi             The info descriptor of the visual to be used.
 *  pgw->color.cmap           The ID of the colormap to use.
 *  pgw->color.ncol           The number of colors available.
 *  pgw->color.pixel[0..ncol] The color cell pixel indexes.
 *  pgw->color.monochrome     If true, use black and white instead of the above
 *                           values.
 *
 *  return   int     0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int xw_get_visual(XWServer *xw, PGwin *pgw)
#else
static int xw_get_visual(xw, pgw)
     XWServer *xw; PGwin *pgw;
#endif
{
/*
 * Initialize the color descriptor.
 */
  pgw->color.vi = NULL;
  pgw->color.cmap = None;
  pgw->color.ncol = 2;
  pgw->color.pixel = NULL;
  pgw->color.monochrome = 1;
  pgw->color.default_class = 0;
/*
 * Get the XVisualInfo structure for the default visual.
 */
  pgw->color.vi = xw_visual_info(xw->display, pgw->screen,
				 DefaultVisual(xw->display, pgw->screen));
  if(!pgw->color.vi)
    return 1;
/*
 * Allocate an array to store pixel indexes in.
 */
  pgw->color.pixel=(unsigned long *)malloc(sizeof(unsigned long) * pgw->maxcol);
  if(pgw->color.pixel==NULL) {
    fprintf(stderr, "%s: Insufficient memory for new PGPLOT window.\n",
	    PGXWIN_SERVER);
    return 1;
  };
/*
 * Record the class of the default colormap.
 */
  pgw->color.default_class = pgw->color.vi->class;
/*
 * Check for user preferences before starting the default visual search.
 */
  if(pgw->visual_class != -2 &&
     (pgw->visual_class<0 || !xw_find_visual(xw, pgw, pgw->visual_class))) {
/*
 * Color display?
 */
    switch(pgw->color.default_class) {
    case PseudoColor:
    case StaticColor:
    case DirectColor:
    case TrueColor:
      if(xw_find_visual(xw, pgw, PseudoColor) ||
	 xw_find_visual(xw, pgw, StaticColor) ||
	 xw_find_visual(xw, pgw, TrueColor))
	return 0;
      break;
/*
 * Gray-scale display?
 */
    case GrayScale:
    case StaticGray:
      if(xw_find_visual(xw, pgw, GrayScale) ||
	 xw_find_visual(xw, pgw, StaticGray))
	return 0;
      break;
    };
  };
/*
 * Use the monochrome default if no usable colormap was found.
 */
  return 0;
}

/*.......................................................................
 * Private function of xw_get_visual(), used to find a visual of a given
 * class and at least pgw->mincol colors. The pgw->color structure will
 * be left untouched unless a good colormap is located. It is assumed
 * that pgw->color.vi is initialized with a info structure for the
 * default visual returned by xw_visual_info().
 *
 * Input:
 *  xw   XWServer *  The PGPLOT /xw server descriptor.
 *  pgw     PGwin *  The PGPLOT window descriptor.
 *  class     int    The type of colormap required, chosen from:
 *                      PseudoColor,StaticColor,GrayScale,StaticGray.
 * Input/Output:
 *
 *  return    int    0 - No colormap was found - pgw->color.* remain
 *                       at the values that they had on input.
 *                   1 - A suitable colormap was found. The following
 *                       members of pgw->color will be set as indicated.
 *                        vi    -  The info descriptor of the located visual.
 *                        cmap  -  The ID of the located colormap.
 *                        pixel -  The color cell pixel indexes.
 *                        ncol  -  The number of colors available.
 *                       Note that the returned colormap and pixels must
 *                       be deleted via xw_del_Colormap() and the vi structure
 *                       via XFree() when the window is destroyed.
 */
#ifdef __STDC__
static int xw_find_visual(XWServer *xw, PGwin *pgw, int class)
#else
static int xw_find_visual(xw, pgw, class)
     XWServer *xw; PGwin *pgw; int class;
#endif
{
  Colormap cmap=None;   /* The new colormap */
  int ncol=0;           /* The number of colors allocated */
  XVisualInfo *vi=NULL; /* The visual info of a private colormap */
/*
 * See if the default (shared) visual supports the required class of
 * colormap.
 */
  if(class == pgw->color.vi->class) {
    cmap = DefaultColormap(xw->display, pgw->screen);
    ncol = xw_get_colorcells(xw, pgw, pgw->color.vi, cmap);
    if(ncol >= pgw->maxcol) {
      pgw->color.cmap = cmap;
      pgw->color.ncol = ncol;
      pgw->color.monochrome = 0;
      return 1;
    };
    xw_del_Colormap(xw, pgw, pgw->color.vi, cmap, ncol);
  };
/*
 * Acquire a private colormap.
 */
  {
    XVisualInfo vi_template;     /* Visual search template */
    XVisualInfo *vi_list = NULL; /* List of matching visuals */
    int nmatch;                  /* Number of matching visuals in vi_list[] */
/*
 * Get a list of all visuals of the requested class.
 */
    vi_template.class = class;
    vi_list = XGetVisualInfo(xw->display, (long)VisualClassMask, &vi_template,
			     &nmatch);
/*
 * Search the list for a visual that has a colormap size that
 * best matches pgw->maxcol. Note that the colormap_size memeber of
 * the visual info structure effectively provides the number of
 * "independant" color table entries. Thus the following algorithm
 * works even for colormaps of TrueColor and DirectColor where the
 * colormap_size attribute refers to the size of a single primary color
 * table.
 */
    if(vi_list) {
      XVisualInfo *vi_below = NULL;
      XVisualInfo *vi_above = NULL;
      for(vi=vi_list; vi<vi_list+nmatch; vi++) {
	if(vi->colormap_size < pgw->maxcol) {
	  if(!vi_below || vi->colormap_size > vi_below->colormap_size)
	    vi_below = vi;
	} else {
	  if(!vi_above || vi->colormap_size < vi_above->colormap_size)
	    vi_above = vi;
	};
      };
/*
 * If available, use a visual that has at least pgw->maxcol independant
 * colors.
 */
      if(vi_above)
	vi = vi_above;
      else if(vi_below)
	vi = vi_below;
      else
	vi = NULL;
/*
 * Did we get a usable visual?
 */
      if(vi && vi->colormap_size > 2)
	vi = xw_visual_info(xw->display, pgw->screen, vi->visual);
      XFree((char *) vi_list);
    };
  };
/*
 * Bracket the colormap acquisition with xw_sync_error() calls, to
 * determine whether any allocation errors occur.
 */
  if(vi) {
    xw_sync_error(xw);
    cmap = XCreateColormap(xw->display, pgw->parent, vi->visual, AllocNone);
    if(xw_sync_error(xw) || cmap == None) {
      fprintf(stderr,
         "%s: XCreateColormap failed for visual: id=0x%lx class=%d depth=%u.\n",
	 PGXWIN_SERVER, (unsigned long)vi->visualid, vi->class, vi->depth);
/*
 * Allocate color-cells in the new colormap.
 */
    } else if((ncol = xw_get_colorcells(xw, pgw, vi, cmap)) >= pgw->mincol) {
      XFree((char *) pgw->color.vi);
      pgw->color.vi   = vi;
      pgw->color.cmap = cmap;
      pgw->color.ncol = ncol;
      pgw->color.monochrome = 0;
#ifdef DEBUG
      fprintf(stderr, "%s: Got %d colors in colormap 0x%lx, visual id=0x%lx class=%d depth=%u.\n", PGXWIN_SERVER, ncol, (unsigned long)cmap, (unsigned long) vi->visualid, vi->class, vi->depth);
#endif
      return 1;
    } else {
      xw_del_Colormap(xw, pgw, vi, cmap, ncol);
    };
    XFree((char *) vi);
  };
/*
 * Failed to get a colormap of the requested class.
 */
  return 0;
}

/*.......................................................................
 * Private function of xw_find_visual(), used to allocate color cells for a
 * given colormap and return a count of the number allocated.
 *
 * Input:
 *  xw     XWServer *  The PGPLOT /xw server descriptor.
 *  pgw       PGwin *  The PGPLOT window descriptor.
 *  vi  XVisualInfo *  The info descripto of the visual containing the colormap.
 *  cmap   Colormap    The colormap ID to associate the cells with.
 * Output:
 *  pgw->color.pixel[] The colorcell indexes.
 *  return      int    The number of colors allocated.
 */
#ifdef __STDC__
static int xw_get_colorcells(XWServer *xw, PGwin *pgw, XVisualInfo *vi,
			     Colormap cmap)
#else
static int xw_get_colorcells(xw, pgw, vi, cmap)
     XWServer *xw; PGwin *pgw; XVisualInfo *vi; Colormap cmap;
#endif
{
  unsigned long maxcol; /* The max number of cells to attempt to allocate */
  int ncol;             /* The number of color-cells allocated */
/*
 * Determine the number of color cells in the colormap.
 */
  switch(vi->class) {
  case PseudoColor:
  case GrayScale:
  case StaticColor:
  case StaticGray:
    maxcol = vi->colormap_size;
    break;
  case TrueColor:
  case DirectColor:
/*
 * Determine the maximum number of significant colors available
 * by looking at the total number of bits set in the pixel bit-masks.
 */
    maxcol = 1;
    {
      unsigned long rgb_mask = (vi->red_mask | vi->green_mask | vi->blue_mask);
      do {
	if(rgb_mask & (unsigned long)0x1)
	  maxcol <<= (unsigned long)1;
      } while(maxcol < pgw->maxcol && (rgb_mask >>= (unsigned long)1) != 0);
    };
    break;
  default:
    maxcol = 0;
    break;
  };
/*
 * Limit the number of colorcells to the size of the pgw->color.pixel[] array.
 */
  if(maxcol > pgw->maxcol)
    maxcol = pgw->maxcol;
/*
 * Don't try to allocate anything if there are too few colors available.
 */
  if(maxcol < pgw->mincol) {
    ncol = 0;
  } else {
    unsigned long planes[1];
    unsigned int nplanes = 0;
/*
 * Dynamic colormaps require one to allocate cells explicitly.
 * Allocate up to maxcol color cells.
 */
    switch(vi->class) {
    case PseudoColor:
    case GrayScale:
    case DirectColor:
/*
 * See if we can get all of the colors requested.
 */
      if(XAllocColorCells(xw->display, cmap, False, planes, nplanes,
			  pgw->color.pixel, (unsigned) maxcol)) {
	ncol = maxcol;
/*
 * If there aren't at least pgw->mincol color cells available, then
 * give up on this colormap.
 */
      } else if(!XAllocColorCells(xw->display, cmap, False, planes, nplanes,
				  pgw->color.pixel, (unsigned) pgw->mincol)) {
	ncol = 0;
      } else {
/*
 * Since we were able to allocate pgw->mincol cells, we may be able to
 * allocate more. First discard the pgw->mincol cells, so that we can
 * try for a bigger number.
 */
	XFreeColors(xw->display, cmap, pgw->color.pixel, (int) pgw->mincol,
		    (unsigned long)0);
/*
 * Since there is no direct method to determine the number of allocatable
 * color cells available in a colormap, perform a binary search for the
 * max number that can be allocated. Note that it is possible that another
 * client may allocate colors from the same colormap while we search. This
 * invalidates the result of the search and is the reason for the outer
 * while loop.
 */
	ncol = 0;
	do {
	  int lo = pgw->mincol;
	  int hi = maxcol;
	  while(lo<=hi) {
	    int mid = (lo+hi)/2;
	    if(XAllocColorCells(xw->display, cmap, False, planes, nplanes,
				pgw->color.pixel, (unsigned) mid)) {
	      ncol = mid;
	      lo = mid + 1;
	      XFreeColors(xw->display, cmap, pgw->color.pixel, mid,
			  (unsigned long)0);
	    } else {
	      hi = mid - 1;
	    };
	  };
	} while(ncol >= pgw->mincol &&
		!XAllocColorCells(xw->display, cmap, False, planes, nplanes,
				  pgw->color.pixel, (unsigned) ncol));
      };
      break;
/*
 * For static color maps, color-cell pixel indexes will be assigned later
 * with XAllocColor() in xw_set_rgb(). For now simply assign 0 to all
 * pixels.
 */
    case StaticColor:
    case TrueColor:
    case StaticGray:
      for(ncol=0; ncol<maxcol; ncol++)
	pgw->color.pixel[ncol] = 0;
      ncol = maxcol;
      break;
    default:
      ncol = 0;
      break;
    };
  };
  return (ncol >= pgw->mincol) ? ncol : 0;
}

/*.......................................................................
 * Delete the color-cells of a colormap if pertinent and the colormap itself.
 *
 * Input:
 *  xw    XWServer *  The PGPLOT /xw server descriptor.
 *  pgw      PGwin *  The PGPLOT window descriptor.
 *  vi XVisualInfo *  The info descriptor of the visual containing the colormap.
 *  cmap  Colormap    The colormap ID to be deleted.
 *  ncol       int    The number of color cells allocated.
 *                    If <= 0 cell de-allocation will not be performed.
 * Output:
 *  return     int    0.
 */
#ifdef __STDC__
static int xw_del_Colormap(XWServer *xw, PGwin *pgw, XVisualInfo *vi,
			   Colormap cmap, int ncol)
#else
static int xw_del_Colormap(xw, pgw, vi, cmap, ncol)
     XWServer *xw; PGwin *pgw; XVisualInfo *vi; Colormap cmap; int ncol;
#endif
{
/*
 * Is there a colormap to be deleted?
 */
  if(cmap != None) {
/*
 * Delete colorcells if necessary.
 */
    switch(vi->class) {
    case PseudoColor:
    case GrayScale:
    case DirectColor:
      if(ncol > 0)
	XFreeColors(xw->display, cmap, pgw->color.pixel, ncol,(unsigned long)0);
      break;
    };
/*
 * Delete the colormap if necessary.
 */
    if(cmap != DefaultColormap(xw->display, pgw->screen))
      XFreeColormap(xw->display, cmap);
  };
  return 0;
}

/*.......................................................................
 * Prepare an existing PGPLOT window for active duty with a new PGPLOT
 * client. This involves installing the window on the active window list,
 * associating it with the new client, and arranging to detect pertinent
 * events on the client and PGPLOT window.
 * 
 *
 * Input:
 *  xw     XWServer *  The PGPLOT /xw server descriptor.
 *  pgw       PGwin *  The PGPLOT window descriptor.
 *  client   Window    The client being assigned to this window.
 *  protocol    int    The client/server communication protocol to use.
 * Output:
 *  return     int    0 - OK.
 */
#ifdef __STDC__
static int xw_prep_window(XWServer *xw, PGwin *pgw, Window client, int protocol)
#else
static int xw_prep_window(xw, pgw, client, protocol)
     XWServer *xw; PGwin *pgw; Window client; int protocol;
#endif
{
/*
 * Associate the window with the new client.
 */
  pgw->client = client;
  pgw->protocol = protocol;
  pgw->mapped = 0;
/*
 * Select the events that we want to detect on the client's communication
 * window. Bracket the call with xw_sync_error() calls to determine
 * whether it generates any errors. An error here would mean that
 * the client communication window ID was invalid. Note that the
 * second xw_sync_error() call also has the effect of flushing the
 * event mask to the display so that hereafter if the window id becomes
 * invalid we will be informed of it through a DestroyNotify event.
 */
  xw_sync_error(xw);
  XSelectInput(xw->display, pgw->client, (long)
	       (StructureNotifyMask|PropertyChangeMask));
  if(xw_sync_error(xw)) {
    fprintf(stderr,
      "%s: Failed to select events on client communication window (0x%lx).\n",
	    PGXWIN_SERVER, (unsigned long) pgw->client);
    return 1;
  };
/*
 * Install the window in the active list.
 */
  add_PGwin(&xw->active_list, 1, pgw);
  return 0;
}

/*.......................................................................
 * Send data to a client by placing data in the client's xw->client_data
 * property.
 *
 * Input:
 *  xw        XWServer * The PGPLOT /xw server descriptor.
 *  pgw          PGwin * The PGPLOT window to send to.
 *  data unsigned char * The data to be sent, cast to (char *).
 *  form           int   The format for the property. Recognised values and
 *                       the data types used to send them in data[] are:
 *                         XW_CHAR_PROP  -  (char)
 *                         XW_SHORT_PROP -  (short)
 *                         XW_LONG_PROP  -  (long)
 *  n    unsigned long   The number of items to be sent, in multiples of
 *                       'size'.
 *  type          Atom   The output property type (eg. XA_INTEGER).
 * Output:
 *  return unsigned long The number of items sent==n, or 0 on error.
 */
#ifdef __STDC__
static unsigned long xw_send_data(XWServer *xw, PGwin *pgw, unsigned char *data,
				  int form, unsigned long n, Atom type)
#else
static unsigned long xw_send_data(xw, pgw, data, form, n, type)
     XWServer *xw; PGwin *pgw; unsigned char *data; int form; unsigned long n;
     Atom type;
#endif
{
  XEvent event;         /* Used to check for property-notify events */
  long max_item;        /* Max number of items transfereable in one go */
  long ndone;           /* The number of items sent so far */
  long nnew;            /* The number of items sent in the latest iteration */
  unsigned long size;   /* Size of property data element */
  int waserr=0;         /* True after an error */
/*
 * The property data expected by XChangeProperty is arranged as an array of
 * (char) if form=8, (short) if form=16, and (long) if form=32,
 * irrespective of the sizes of these types. Get the size of one such
 * element in bytes.
 */
  switch(form) {
  case XW_CHAR_PROP:
    size = sizeof(char);
    break;
  case XW_SHORT_PROP:
    size = sizeof(short);
    break;
  case XW_LONG_PROP:
    size = sizeof(long);
    break;
  default:
    fprintf(stderr, "%s: Unknown property format: %d\n", PGXWIN_SERVER, form);
    return 0;
    break;
  };
/*
 * Determine the current maximum number of items that can be transfered
 * in one go. (Note that the units of XMaxRequestSize are mutliples of
 * 4 bytes).
 */
  max_item = (4*XMaxRequestSize(xw->display))/size;
/*
 * Send the data in one or more chunks of up to max_item items.
 */
  for(ndone=0; !waserr && ndone<n; ndone+=nnew) {
/*
 * How many complete elements can be sent this time around?
 */
    nnew = (n-ndone > max_item) ? max_item : (n-ndone);
/*
 * Place the latest set of nnew items in the client's data property.
 */
    XChangeProperty(xw->display, pgw->client, xw->client_data, type, form,
		    PropModeReplace, data+ndone, (int)nnew);
    XFlush(xw->display);
/*
 * Wait for the property to be deleted before sending more data.
 */
    do {
      XWindowEvent(xw->display, pgw->client, (long)
		   (StructureNotifyMask|PropertyChangeMask), &event);
/*
 * If the client window is destroyed, rather than blocking forever waiting
 * for a property notify event, put the event back to be handled by
 * xw_event_loop(), cleanup and return the error status.
 */
      if(event.type == DestroyNotify) {
	XPutBackEvent(xw->display, &event);
	return 0;
      };
    } while(!waserr && !(event.type == PropertyNotify &&
			 event.xproperty.window == pgw->client &&
			 event.xproperty.atom   == xw->client_data &&
			 event.xproperty.state  == PropertyDelete));
  };
/*
 * Terminate the transaction by sending a zero-length property value.
 */
  XChangeProperty(xw->display, pgw->client, xw->client_data, type, form,
		  PropModeReplace, data, 0);
  XFlush(xw->display);
  return ndone;
}

/*.......................................................................
 * Create two cursors for use by clients. One cursor is for use when
 * cursor input is expected, and the other for when it is not expected.
 * Also create a cursor for use by the server, to show when a window
 * is unassigned to any client.
 *
 *  xw    XWServer * The PGPLOT /xw server descriptor.
 *  usecolor   int   0 - Create a black and white cursor.
 *                   1 - Create a color cursor.
 * Input/Output:
 *  curs  XWCursor * The cursor container to be filled.
 * Output:
 *  return     int   0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int xw_new_cursors(XWServer *xw, int usecolor, XWCursor *curs)
#else
static int xw_new_cursors(xw, usecolor, curs)
     XWServer *xw; int usecolor; XWCursor *curs;
#endif
{
  XColor bg,fg;   /* Background and foreground colors */
/*
 * Create the new cursors.
 */
  curs->norm = XCreateFontCursor(xw->display, XC_spider);
  curs->live = XCreateFontCursor(xw->display, XC_crosshair);
  curs->idle = XCreateFontCursor(xw->display, XC_pirate);
  if(curs->norm==None || curs->live==None || curs->idle==None) {
    fprintf(stderr, "%s: Error creating cursor.\n", PGXWIN_SERVER);
    return 1;
  };
/*
 * Initialize the common parts of the color descriptors.
 */
  bg.pixel = fg.pixel = 0;
  bg.flags = fg.flags = DoRed | DoGreen | DoBlue;
  bg.pad = fg.pad = 0;
/*
 * A black background will be used for all cursors.
 */
  bg.red = bg.green = bg.blue = 0;
/*
 * Give the normal cursor a yellow foreground color.
 */
  fg.red   = fg.green = COLORMULT;
  fg.blue  = usecolor ? 0 : COLORMULT;
  XRecolorCursor(xw->display, curs->norm, &fg, &bg);
/*
 * Give the active cursor a red foreground color.
 */
  fg.red   = COLORMULT;
  fg.green = fg.blue = usecolor ? 0:COLORMULT;
  XRecolorCursor(xw->display, curs->live, &fg, &bg);
/*
 * Give the idle cursor a white foreground color.
 */
  fg.red = fg.green = fg.blue = COLORMULT;
  XRecolorCursor(xw->display, curs->idle, &fg, &bg);
  return 0;
}

/*.......................................................................
 * Delete cursors created by xw_new_cursors().
 *
 *  xw    XWServer * The PGPLOT /xw server descriptor.
 *  curs  XWCursor * The cursor container to be emptied.
 * Output:
 *  return     int   0 - OK.
 */
#ifdef __STDC__
static int xw_del_cursors(XWServer *xw, XWCursor *curs)
#else
static int xw_del_cursors(xw, curs)
     XWServer *xw; XWCursor *curs;
#endif
{
  if(curs->norm != None)
    XFreeCursor(xw->display, curs->norm);
  curs->norm = None;
  if(curs->live != None)
    XFreeCursor(xw->display, curs->live);
  curs->live = None;
  if(curs->idle != None)
    XFreeCursor(xw->display, curs->idle);
  curs->idle = None;
  return 0;
}

/*.......................................................................
 * Initialize a new cursor container to be empty. This must be called
 * before xw_new_cursors or xw_del_cursors so that it is clear which
 * cursors have been created and need to be deleted.
 *
 *  xw    XWServer * The PGPLOT /xw server descriptor.
 *  curs  XWCursor * The cursor container to be initialized.
 * Output:
 *  return     int   0 - OK.
 */
#ifdef __STDC__
static int xw_ini_cursors(XWServer *xw, XWCursor *curs)
#else
static int xw_ini_cursors(xw, curs)
     XWServer *xw; XWCursor *curs;
#endif
{
  curs->norm = None;
  curs->live = None;
  curs->idle = None;
  return 0;
}

/*.......................................................................
 * Return a dynamically allocated visual info structure for a given
 * visual. This is simply a more convenient interface to XGetVisualInfo()
 * and XVisualIDFromVisual().
 *
 * Input:
 *  display    Display *   The display connection to which the visual
 *                         belongs.
 *  screen         int     The screen to which the visual belongs.
 *  visual      Visual *   The visual for which information is required.
 * Output:
 *  return XVisualInfo *   The required information descriptor, or NULL
 *                         on error.
 */
#ifdef __STDC__
static XVisualInfo *xw_visual_info(Display *display, int screen, Visual *visual)
#else
static XVisualInfo *xw_visual_info(display, screen, visual)
     Display *display; int screen; Visual *visual;
#endif
{
  XVisualInfo *vi=NULL;  /* The return descriptor */
  XVisualInfo template;  /* The search template */
  int nret = 0;          /* The number of descriptors returned */
/*
 * Using the visual ID and the screen should unambiguously select the
 * information for the specified visual.
 */
  template.visualid = XVisualIDFromVisual(visual);
  template.screen = screen;
  vi = XGetVisualInfo(display, (long)(VisualIDMask | VisualScreenMask),
		      &template, &nret);
  if(vi == NULL || nret < 1) {
    fprintf(stderr,
      "%s: Error getting visual information for visual ID 0x%lx, screen %d.\n",
      PGXWIN_SERVER, (unsigned long)template.visualid, screen);
    vi = NULL;
  };
  return vi;
}

/*.......................................................................
 * Get X-resource configurable PGPLOT window attributes for a new window.
 *
 * Input:
 *  display
 */
#ifdef __STDC__
static int xw_get_config(XWServer *xw, PGwin *pgw)
#else
static int xw_get_config(xw, pgw)
     XWServer *xw; PGwin *pgw;
#endif
{
  char *def = NULL;  /* X resource value */
  XWQuarks *qrk = &xw->quarks;
/*
 * Initialize with server resource defaults.
 */
  pgw->acceptquit = 0;
  pgw->iconize = 0;
  pgw->mincol = NCOLORS;
  pgw->maxcol = XW_DEF_COLORS<XW_MAX_COLORS ? XW_DEF_COLORS:XW_MAX_COLORS;
  pgw->crosshair = 0;
  pgw->visual_class = -1;
/*
 * Override current defaults.
 *
 * Record whether WM_DELETE_WINDOW actions are to be accepted on
 * active windows.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->acceptquit)))
    pgw->acceptquit = xw_parse_bool(def, False) == True;
/*
 * Record whether persistent windows should be iconized when inactive.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->iconize)))
    pgw->iconize = xw_parse_bool(def, False) == True;
/*
 * Override the default minimum number of colors per colormap.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->mincolors))) {
    pgw->mincol = atoi(def);
    if(pgw->mincol < XW_MIN_COLORS)
      pgw->mincol = XW_MIN_COLORS;
    if(pgw->mincol > XW_MAX_COLORS)
      pgw->mincol = XW_MAX_COLORS;
  };
/*
 * Override the default max number of colors per window.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->maxcolors))) {
    pgw->maxcol = atoi(def);
    if(pgw->maxcol < XW_MIN_COLORS)
      pgw->maxcol = XW_MIN_COLORS;
    if(pgw->maxcol > XW_MAX_COLORS)
      pgw->maxcol = XW_MAX_COLORS;
  };
/*
 * Ensure that maxcol >= mincol.
 */
  if(pgw->mincol > pgw->maxcol)
    pgw->maxcol = pgw->mincol;
/*
 * See if a crosshair cursor should be used.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->crosshair)))
    pgw->crosshair = xw_parse_bool(def, False) == True;
/*
 * See what the default visual type should be.
 */
  if((def = xw_get_default(xw, pgw->id, &qrk->visual)))
    pgw->visual_class = xw_parse_visual(def);
  return 0;
}

/*.......................................................................
 * Lookup a resource value from the X resource database.
 * This function must not be called before xw_get_xrdb() or xw_get_quarks().
 *
 * Input:
 *  xw        XWServer *   The server descriptor.
 *  window_id      int     The numeric ID of the window to look up a
 *                         resource for, or 0 for a server resource.
 *  pair       XWQPair *   Name/class quark pair of the resource to be
 *                         looked up.
 * Output:
 *  return        char *   The resource value, or NULL if no value is
 *                         available. This is a pointer to an internal
 *                         static buffer.
 */
#ifdef __STDC__
static char *xw_get_default(XWServer *xw, int window_id, XWQPair *pair)
#else
static char *xw_get_default(xw, window_id, pair)
     XWServer *xw; int window_id; XWQPair *pair;
#endif
{
  static char resource[80];   /* Buffer for returned value */
  XrmName win_name;           /* Window specification quark */
  XrmName name_list[4];       /* List of resource name components */
  XrmClass class_list[4];     /* List of resource class components */
  XrmRepresentation rep_type; /* Returned representation of resource */
  XrmValue value;             /* Resource value */
  int have_value = 0;         /* True once a resource value has been acquired */
/*
 * All resources share the initial program component.
 */
  name_list[0]  = xw->quarks.prog.name;
  class_list[0] = xw->quarks.prog.class;  
/*
 * If a specific window has been specified determine the window component
 * name to use and convert it to a quark.
 */
  if(window_id) {
    sprintf(resource, "win%d", window_id);
    win_name = XrmStringToName(resource);
    name_list[1]  = win_name;
    name_list[2]  = pair->name;
    name_list[3]  = NULLQUARK;
    class_list[1] = xw->quarks.win_class;
    class_list[2] = pair->class;
    class_list[3] = NULLQUARK;
    have_value = XrmQGetResource(xw->xrdb, name_list, class_list, &rep_type,
			&value)==True && value.size + 1 <= sizeof(resource);
/*
 * For compatibility with the first version of pgxwin_server, allow
 * pgxwin.resource_name to be an alias for pgxwin.Win.resource_name.
 */
    if(!have_value) {
      name_list[1]  = pair->name;
      name_list[2]  = NULLQUARK;
      class_list[1] = pair->class;
      class_list[2] = NULLQUARK;
      have_value = XrmQGetResource(xw->xrdb, name_list, class_list, &rep_type,
			  &value)==True && value.size + 1 <= sizeof(resource);
    };
/*
 * Get server resource.
 */
  } else {
    name_list[1]  = xw->quarks.server.name;
    name_list[2]  = pair->name;
    name_list[3]  = NULLQUARK;
    class_list[1] = xw->quarks.server.class;
    class_list[2] = pair->class;
    class_list[3] = NULLQUARK;
    have_value = XrmQGetResource(xw->xrdb, name_list, class_list, &rep_type,
			&value)==True && value.size + 1 <= sizeof(resource);
  };
/*
 * Return a '\0' terminated copy of the resource value, or NULL if not
 * available.
 */
  if(have_value) {
    strncpy(resource, (char *)value.addr, (int)value.size);
    resource[(int)value.size] = '\0';
    return resource;
  };
  return NULL;
}

/*.......................................................................
 * Create the X resource database.
 *
 * Input:
 *  display    Display *  The display connection over which to check for the
 *                        RESOURCE_MANAGER root window property.
 *  cmd_db XrmDatabase    The command line resource database.
 * Output:
 *  return XrmDatabase    The initialized database.
 */
#ifdef __STDC__
static XrmDatabase xw_get_xrdb(Display *display, XrmDatabase cmd_db)
#else
static XrmDatabase xw_get_xrdb(display, cmd_db)
     Display *display; XrmDatabase cmd_db;
#endif
{
  XrmDatabase user_db = NULL;  /* User specified database */
  XrmDatabase env_db = NULL;   /* Environment-specific database */
/*
 * Initialize the database manager.
 */
  XrmInitialize();
/*
 * Get the XA_RESOURCE_MANAGER property from the root window.
 */
  if(XResourceManagerString(display)) {
    user_db = XrmGetStringDatabase(XResourceManagerString(display));
  } else {
/*
 * If there was nothing on the root window, attempt to read the users
 * .Xdefaults file.
 */
#ifdef VMS
    user_db = XrmGetFileDatabase("DECW$USER_DEFAULTS:DECW$XDEFAULTS.DAT");
#else
    char *dir = xw_home_dir();
    char *sep = "/";
    char *file = ".Xdefaults";
    if(dir) {
      char *path = (char *) malloc(sizeof(char) *
				   (strlen(dir)+strlen(sep)+strlen(file) + 1));
      if(path) {
	sprintf(path, "%s%s%s", dir, sep, file);
	user_db = XrmGetFileDatabase(path);
	free(path);
      };
    };
#endif
  };
/*
 * See if an environment-specific database exists.
 */
  {
    char *env = getenv("XENVIRONMENT");
    if(env)
      env_db = XrmGetFileDatabase(env);
  };
/*
 * Merge the databases.
 */
  XrmMergeDatabases(env_db, &user_db);
/*
 * Override selected resources from the command-line resource database.
 */
  XrmMergeDatabases(cmd_db, &user_db);
  return user_db;
}

/*.......................................................................
 * Get the user's home directory.
 *
 * Output:
 *  return   char *  A statically allocated string containing the user's
 *                   home directory name, or NULL if not available.
 */
#ifdef __STDC__
static char *xw_home_dir(void)
#else
static char *xw_home_dir()
#endif
{
  char *home = NULL;
#ifdef VMS
  home = "SYS$LOGIN";
#else
  if((home=getenv("HOME")) == NULL) {
    struct passwd *pwd = getpwuid(getuid());
    if(pwd)
      home = pwd->pw_dir;
  };
#endif
  return home;
}

/*.......................................................................
 * Record quarks for frequently used resource-database components, in
 * xw->quarks. This must be called before xw_get_config().
 *
 * Input:
 *  xw     XWServer *  The server who's quark database is to be initialized.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int xw_get_quarks(XWServer *xw)
#else
static int xw_get_quarks(xw)
     XWServer *xw;
#endif
{
  XWQuarks *q = &xw->quarks;
  q->prog.name       = XrmStringToName("pgxwin");
  q->prog.class      = XrmStringToClass("Pgxwin");
  q->display.name    = XrmStringToName("display");
  q->display.class   = XrmStringToClass("Display");
  q->server.name     = XrmStringToName("server");
  q->server.class    = XrmStringToClass("Server");
  q->geom.name       = XrmStringToName("geometry");
  q->geom.class      = XrmStringToClass("Geometry");
  q->iconize.name    = XrmStringToName("iconize");
  q->iconize.class   = XrmStringToClass("Iconize");
  q->acceptquit.name = XrmStringToName("acceptQuit");
  q->acceptquit.class= XrmStringToClass("AcceptQuit");
  q->mincolors.name  = XrmStringToName("minColors");
  q->mincolors.class = XrmStringToClass("MinColors");
  q->maxcolors.name  = XrmStringToName("maxColors");
  q->maxcolors.class = XrmStringToClass("MaxColors");
  q->visual.name     = XrmStringToName("visual");
  q->visual.class    = XrmStringToClass("Visual");
  q->crosshair.name  = XrmStringToName("crosshair");
  q->crosshair.class = XrmStringToClass("Crosshair");
  q->visible.name    = XrmStringToName("visible");
  q->visible.class   = XrmStringToClass("Visible");
  q->icongeom.name    = XrmStringToName("iconGeometry");
  q->icongeom.class   = XrmStringToClass("IconGeometry");
  q->win_class = XrmStringToClass("Win");
  return 0;
}

/*.......................................................................
 * Specify window state hints to the window manager.
 *
 * Input:
 *  xw    XWServer *  The server descriptor.
 *  screen     int    The screen on which the window resides.
 *  window  Window    The window to which the hints apply.
 *  id         int    The PGwin window number, or 0 for the server window.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_setwmhints(XWServer *xw, int screen, Window window, int id)
#else
static int xw_setwmhints(xw, screen, window, id)
     XWServer *xw; int screen; Window window; int id;
#endif
{
  XWMHints *hints;
  char *icongeom;         /* Icon geometry resource value */
/*
 * Allocate the hints structure as recommended.
 */
  hints = XAllocWMHints();
/*
 * Start with no hints specified.
 */
  if(hints) {
    hints->flags = 0;
/*
 * Server-specific hints.
 */
    if(id==0) {
      hints->flags |= StateHint;  /* The server window should start iconified */
      hints->initial_state = IconicState;
/*
 * PGPLOT window specific hints.
 */
    } else {
      hints->flags |= InputHint;   /* Register interest in keyboard input */
      hints->input = True;
    };
/*
 * See if a geometry has been specified for the window icon.
 */
    icongeom = xw_get_default(xw, id, &xw->quarks.icongeom);
    if(icongeom) {
      int x,y;
      unsigned int width;
      unsigned int height;
      int mask = XParseGeometry(icongeom, &x, &y, &width, &height);
      hints->flags |= IconPositionHint;
      if(mask & XValue)
	x = (mask & XNegative) ? DisplayWidth(xw->display, screen) - x : x;
      else
	x = 0;
      if(mask & YValue)
	y = (mask & YNegative) ? DisplayHeight(xw->display, screen) - y : y;
      else
	y = 0;
      hints->icon_x = (mask & XValue) ? x:0;
      hints->icon_y = (mask & YValue) ? y:0;
    };
/*
 * Install the hints if any were provided.
 */
    if(hints->flags)
      XSetWMHints(xw->display, window, hints);
    XFree((char *)hints);
  } else {
    fprintf(stderr, "%s: Insufficient memory for position and state hints.\n",
	    PGXWIN_SERVER);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Set the window name and icon names for a window.
 *
 * Input:
 *  xw      XWServer *   The server descriptor.
 *  window    Window     The window to name.
 *  w_name      char *   The name for the window.
 *  i_name      char *   The name for the icon.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
#ifdef __STDC__
static int xw_name_window(XWServer *xw, Window window, char *w_name,
			  char *i_name)
#else
static int xw_name_window(xw, window, w_name, i_name)
     XWServer *xw; Window window; char *w_name; char *i_name;
#endif
{
/*
 * Get text-property version of the window name and assign it to the window.
 */
  {
    XTextProperty window_name;
    if(XStringListToTextProperty(&w_name, 1, &window_name) == 0) {
      fprintf(stderr, "%s: Error allocating window name.\n", PGXWIN_SERVER);
      return 1;
    };
    XSetWMName(xw->display, window, &window_name);
    XFree((char *)window_name.value);
  };
/*
 * Get text-property versions of the icon name and assign it to the window.
 */
  {
    XTextProperty icon_name;
    if(XStringListToTextProperty(&i_name, 1, &icon_name) == 0) {
      fprintf(stderr, "%s: Error allocating icon name.\n", PGXWIN_SERVER);
      return 1;
    };
    XSetWMIconName(xw->display, window, &icon_name);
    XFree((char *)icon_name.value);
  };
  return 0;
}

/*.......................................................................
 * Acquire an up to date count of the number of error events generated
 * from all previous Xlib calls since the last time that this function
 * was called. This involves calling XSync() to ensure that all pending
 * requests have been processed, and clears the xw_handle_error() error
 * counter before returning.
 *
 * Thus, to determine whether a given function call causes any errors,
 * bracket it with two calls to xw_sync_error().
 *
 * Input:
 *  xw     XWServer * The PGPLOT server descriptor.
 * Output:
 *  return      int   The number of error events.
 */
#ifdef __STDC__
static int xw_sync_error(XWServer *xw)
#else
static int xw_sync_error(xw)
     XWServer *xw;
#endif
{
/*
 * Force queued X requests to the server and wait for them to be processed.
 */
  XSync(xw->display, False);
/*
 * Ask the error handler for the current count of error events and to
 * clear its error counter by sending it a NULL XErrorEvent pointer.
 */
  return xw_handle_error(xw->display, (XErrorEvent *) 0);
}
