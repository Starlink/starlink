/*
**  Parameters used by the ink jet printing code
*/
#define NPLANES 3
#define NINKS   8
#define NBITS   8
#define MAXX       1440    /* "paper  width/dots (8") */
#define MAXY       1800    /*   "   length/dots (10") */
#define MAXCOLOURS 256
#define MAXDOTS    8
#define MAXX_BYTES (MAXX/8)
#define MAXRLE     360

#include <config.h>

/* This gets rid of a compiler warning because bith tcl.h and jpeglib.h
   define EXTERN with different arguments. */
#undef EXTERN

typedef enum { false, true } boolean;
#define HAVE_BOOLEAN			/* Prevents conflicting definition of boolean in jmorecfg.h */

#if HAVE_JPEGLIB_H
#include <jpeglib.h>
#endif

/*
 * A data structure of the following type is kept for each gwm
 * widget.
 */

typedef struct {
    Tk_Window tkwin;            /* Window that embodies the gwm.  NULL
                                 * means window has been deleted but
                                 * widget record hasn't been cleaned up yet. */
    Display *display;           /* X's token for the window's display. */
    Tcl_Interp *interp;         /* Interpreter associated with widget. */
    Tcl_Command widgetCmd;

    XColor *bg;                 /* The widgets resources */
    XColor *fg;
    XColor *ovcolour;
    XColor *crosscolour;
    char *name;
    char *printformat;
    XColor *printbg;
    char *printvar;
    char *takeFocus;
    Cursor cursor;
    int xoffset;
    int yoffset;
    int xovoffset;
    int yovoffset;
    int crossx;
    int crossy;
    double paperwidth;
    double paperheight;
    unsigned int width;
    unsigned int height;
    int cols;
    int mincols;
    int crosshair;
    int printing;
    int overlay;
    int quality;
    int progressive;
    XVisualInfo *vinfo;
    struct wininfo *info;       /* The structure used by the gwm event */
                                /* processing routine   */

    /* The following items are used to communicate between the print    */
    /* start routine and the idle processing routines                   */
    FILE *out;
    XColor *ct;
    XColor *ctr;
    XColor *ctg;
    XColor *ctb;
    long rbase;
    long gbase;
    long bbase;
    char *buffer;
    XImage *image;
    int j;
    int colour;
#if HAVE_JPEGLIB_H
    struct jpeg_compress_struct *cinfo;
    struct jpeg_error_mgr *jerr;
#endif

    /* The following items are used by the ink jet printing routines */
    int mask[NINKS][NPLANES][NBITS];
    int inks[MAXDOTS][MAXCOLOURS];
    int locx[MAXX];
    int nyi, nxi, nxo, nyo, yo;
    float expand, dimin;

} Gwm;

/*
 * A data structure of the following type is kept for each gwm
 * canvas item.
 */

typedef struct {
    Tk_Item header;
    Tk_Window tkwin;
    Tcl_Interp *interp;
    Tcl_Command itemCmd;
    struct wininfo *info;
    XColor *bg;
    XColor *fg;
    XColor *ovcolour;
    char *name;
    char *command;
    GC gc;
    int cols;
    int mincols;
    unsigned int width;
    unsigned int height;
#if 0
    int overlay;
    GC ovgc;
    int xovoffset;
    int yovoffset;
#endif
} GwmItem;

/*
  Prototype for Gwm window print routine.
*/
int tkgwmStartPrint(Tcl_Interp*, Gwm*, char*);

/*
  Prototypes for gwm canvas item procedures.
*/
int tkgwmItemCreate(Tcl_Interp*, Tk_Canvas, Tk_Item*, int, char**);
int tkgwmItemConfigure(Tcl_Interp*, Tk_Canvas, Tk_Item*, int, char**, int);
int tkgwmItemCoord(Tcl_Interp*, Tk_Canvas, Tk_Item*, int, char**);
void tkgwmItemDelete(Tk_Canvas, Tk_Item*, Display*);
void tkgwmItemDisplay(Tk_Canvas, Tk_Item*, Display*, Drawable, int, int, int,
    int);
double tkgwmItemPoint(Tk_Canvas, Tk_Item*, double*);
int tkgwmItemArea(Tk_Canvas, Tk_Item*, double*);
void tkgwmItemScale(Tk_Canvas, Tk_Item*, double, double, double, double);
void tkgwmItemTranslate(Tk_Canvas, Tk_Item*, double, double);
int tkgwmItemPostScript(Tcl_Interp*, Tk_Canvas, Tk_Item*, int);
