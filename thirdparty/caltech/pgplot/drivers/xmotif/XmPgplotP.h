#ifndef XmPgplotP_h
#define XmPgplotP_h

#if XmVersion == 1001
#include <Xm/XmP.h>
#else
#include <Xm/PrimitiveP.h>
#endif

#include "XmPgplot.h"
#include "pgxwin.h"

/*
 * Declare a container for a list of widgets.
 */
typedef struct {
  XmPgplotWidget head;  /* The head of the list of widgets */
} XmPgplotList;

/*
 * Declare the structure that will contain class-specific
 * attributes. These are effectively shared by all instances
 * of the class widgets.
 */
typedef struct XmPgplotClassPart {
  int widget_id_counter; /* Used to give widgets unique numeric identifiers */
  XmPgplotList active_widgets; /* List of active widgets */
  XmPgplotList free_widgets;   /* List of unnassigned widgets */
  XtPointer *extension;  /* Unused extension field */
} XmPgplotClassPart;

/*
 * Collect all class-specific parts from superclasses and the current
 * new class.
 */
typedef struct XmPgplotClassRec {
  CoreClassPart core_class;
  XmPrimitiveClassPart primitive_class;
  XmPgplotClassPart pgplot_class;
} XmPgplotClassRec;

externalref XmPgplotClassRec xmPgplotClassRec;

/*
 * A context descriptor for managing parent ScrolledWindow scroll-bars.
 */
typedef struct {
  int is_scrolled;      /* True if the parent is a scrolled window */
  Widget w_hbar;        /* Widget id of horizontal scroll-bar */
  Widget w_vbar;        /* Widget id of vertical scroll-bar */  
  unsigned width;       /* The width of the pixmap being scrolled */
  unsigned height;      /* The height of the pixmap being scrolled */
  unsigned x;           /* Pixmap X coordinate of top left corner of window */
  unsigned y;           /* Pixmap Y coordinate of top left corner of window */
} XmpScroll;

/*
 * A context descriptor for dispatching pointer input events.
 */
typedef struct {
  unsigned long mask;       /* The current cursor event-mask */
  XtCallbackProc callback;  /* The cursor-event client callback, or 0 */
  void *client_data;        /* Client data to be sent to the callback */
} XmpInput;

/*
 * Now declare a structure to contain the instance specific parts of
 * the class. This contains members that are different from one
 * instance of the widget class to the next.
 */
typedef struct XmPgplotPart {
/* Public resource attributes */
  int max_colors;       /* The max number of colors needed */
  int min_colors;       /* The min number of colors needed */
  Colormap colormap;    /* The default colormap to use */
  Visual *visual;       /* The default visual to use */  
  XtCallbackList resize_callback; /* User resize-window callbacks */
  Dimension pad_x;      /* The number of pixels to assign to the optional */
                        /*  margin either side of the viewsurface */
  Dimension pad_y;      /* The number of pixels to assign to the optional */
                        /*  margin above and below the viewsurface */
  Boolean share;        /* Non-zero to allocate shared color cells */
/* Private attributes */
  XmPgplotWidget next;  /* The next widget of a list of PGPLOT Motif widgets */
  int xmslct_id;        /* The device ID returned to PGPLOT by the */
                        /* open-workstation driver opcode, and used for */
                        /* subsequent device selection via the */
                        /* select-plot driver opcode */
  int pgslct_id;        /* The device ID returned to the application by */
                        /* pgopen() for subsequent device selection with */
                        /* the pgslct() function */
  char *device;         /* A possible PGPLOT cpgbeg() file string */
  XtAppContext app;     /* The application context */
  XmpScroll scroll;     /* Used to maintain parent ScrolledWindow scroll bars */
  XmpInput input;       /* Cursor input callback and client data container */
  XColor bg, fg;        /* The RGB values of the current foreground and */
                        /* background colors. The pixel member is ignored. */
  PgxWin *pgx;          /* PGPLOT generic X-window context descriptor */
} XmPgplotPart;

/*
 * Collect the instance structures of the super-classes and the
 * PGPLOT class.
 */
typedef struct XmPgplotRec {
  CorePart core;
  XmPrimitivePart primitive;
  XmPgplotPart pgplot;
} XmPgplotRec;

#endif
