#ifndef XaPgplotP_h
#define XaPgplotP_h

#include <X11/Xaw/SimpleP.h>

#define XtNresizeCallback "resizeCallback"
#define XtCResizeCallback "ResizeCallback"

#include "XaPgplot.h"
#include "pgxwin.h"

/*
 * Declare a container for a list of widgets.
 */
typedef struct {
  XaPgplotWidget head;  /* The head of the list of widgets */
} XaPgplotList;

/*
 * Declare the structure that will contain class-specific
 * attributes. These are effectively shared by all instances
 * of the class widgets.
 */
typedef struct XaPgplotClassPart {
  int widget_id_counter; /* Used to give widgets unique numeric identifiers */
  XaPgplotList active_widgets; /* List of active widgets */
  XaPgplotList free_widgets;   /* List of unnassigned widgets */
  XtPointer *extension;  /* Unused extension field */
} XaPgplotClassPart;

/*
 * Collect all class-specific parts from superclasses and the current
 * new class.
 */
typedef struct XaPgplotClassRec {
  CoreClassPart core_class;
  SimpleClassPart simple_class;
  XaPgplotClassPart pgplot_class;
} XaPgplotClassRec;

externalref XaPgplotClassRec xaPgplotClassRec;

/*
 * A context descriptor for dispatching pointer input events.
 */
typedef struct {
  unsigned long mask;       /* The current cursor event-mask */
  XtCallbackProc callback;  /* The cursor-event client callback, or 0 */
  void *client_data;        /* Client data to be sent to the callback */
} XapInput;

/*
 * Now declare a structure to contain the instance specific parts of
 * the class. This contains members that are different from one
 * instance of the widget class to the next.
 */
typedef struct XaPgplotPart {
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
  XaPgplotWidget next;  /* The next widget of a list of PGPLOT Motif widgets */
  int xaslct_id;        /* The device ID returned to PGPLOT by the */
                        /* open-workstation driver opcode, and used for */
                        /* subsequent device selection via the */
                        /* select-plot driver opcode */
  int pgslct_id;        /* The device ID returned to the application by */
                        /* pgopen() for subsequent device selection with */
                        /* the pgslct() function */
  char *device;         /* A possible PGPLOT cpgbeg() file string */
  XtAppContext app;     /* The application context */
  XapInput input;       /* Cursor input callback and client data container */
  Pixel fgpixel;        /* In the athena port, we have nowhere to store this otherwise */
  XColor bg, fg;        /* The RGB values of the current foreground and */
                        /* background colors. The pixel member is ignored. */
  PgxWin *pgx;          /* PGPLOT generic X-window context descriptor */
} XaPgplotPart;

/*
 * Collect the instance structures of the super-classes and the
 * PGPLOT class.
 */
typedef struct XaPgplotRec {
  CorePart core;
  SimplePart      simple;
  XaPgplotPart pgplot;
} XaPgplotRec;

#endif
