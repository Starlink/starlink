/*
 * bltInt.h --
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

#ifndef _BLT_INT_H
#define _BLT_INT_H

#include <tcl.h>
#ifdef WIN32
#include <tkWin.h>
#else
#include <tk.h>
#endif /* WIN32 */

#include "bltTkInt.h"

#include <stdio.h>
#include <assert.h>
#include <math.h>

#ifdef WIN32
#include "bltWinConfig.h"
#else
#include "bltConfig.h"
#endif
#include "blt.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif /* HAVE_STDLIB_H */

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif /* HAVE_ERRNO_H */

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif /* HAVE_MEMORY_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif /* HAVE_MALLOC_H */

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#include "bltList.h"

#ifndef M_PI
#define M_PI    	3.14159265358979323846
#endif /* M_PI */

#ifndef M_PI_2
#define M_PI_2		1.57079632679489661923
#endif

#ifndef M_SQRT2
#define M_SQRT2		1.41421356237309504880
#endif /* M_SQRT2 */

#ifndef M_SQRT1_2
#define M_SQRT1_2	0.70710678118654752440
#endif /* M_SQRT1_2 */

#ifndef SHRT_MAX
#define SHRT_MAX	0x7FFF
#endif /* SHRT_MAX */

#ifndef SHRT_MIN
#define SHRT_MIN	-(SHRT_MAX)
#endif /* SHRT_MAX */

#ifndef USHRT_MAX
#define	USHRT_MAX	0xFFFF
#endif /* USHRT_MAX */

#ifndef INT_MAX
#define INT_MAX		2147483647
#endif /* INT_MAX */

#ifndef HAVE_FLOAT_H
/*
 * ----------------------------------------------------------------------
 *
 * DBL_MIN, DBL_MAX --
 *
 * 	DBL_MAX and DBL_MIN are the largest and smaller double
 * 	precision numbers that can be represented by the floating
 * 	point hardware. If the compiler is ANSI, they can be found in
 * 	float.h.  Otherwise, we use HUGE_VAL or HUGE to determine
 * 	them.
 *
 * ----------------------------------------------------------------------
 */
/*
 * Don't want to include __infinity (definition of HUGE_VAL (SC1.x))
 */
#ifdef sun
#define DBL_MAX		1.7976931348623157E+308
#define DBL_MIN		2.2250738585072014E-308
#define DBL_EPSILON	2.2204460492503131e-16
#else
#ifndef DBL_EPSILON
#define DBL_EPSILON	BLT_DBL_EPSILON
#endif
#ifdef HUGE_VAL
#define DBL_MAX		HUGE_VAL
#define DBL_MIN		(1/HUGE_VAL)
#else
#ifdef HUGE
#define DBL_MAX		HUGE
#define DBL_MIN		(1/HUGE)
#else
/*
 * Punt: Assume values simple and relatively small
 */
#define DBL_MAX		3.40282347E+38
#define DBL_MIN		1.17549435E-38
#endif /*HUGE*/
#endif /*HUGE_VAL*/
#endif /*sun*/
#endif /*!HAVE_FLOAT_H*/

#undef INLINE
#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif
#undef EXPORT
#define EXPORT

#undef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))

#undef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))

/*
 * ----------------------------------------------------------------------
 *
 *  	The following are macros replacing math library functions:
 *  	"fabs", "fmod", "abs", "rint", and "exp10".
 *
 *  	Although many of these routines may be in your math library,
 *  	they aren't used in libtcl.a or libtk.a.  This makes it
 *  	difficult to dynamically load the BLT library as a shared
 *  	object unless the math library is also shared (which isn't
 *  	true on several systems).  We can avoid the problem by
 *  	replacing the "exotic" math routines with macros.
 *
 * ----------------------------------------------------------------------
 */
#undef ABS
#define ABS(x)		(((x)<0)?(-(x)):(x))

#undef EXP10
#define EXP10(x)	(pow(10.0,(x)))

#undef FABS
#define FABS(x) 	(((x)<0.0)?(-(x)):(x))

#undef SIGN
#define SIGN(x)		(((x) < 0.0) ? -1 : 1)

/* 
 * Be careful when using the next two macros.  They both assume the floating
 * point number is less than the size of an int.  That means, for example, you 
 * can't use these macros with numbers bigger than than 2^31-1.
 */
#undef FMOD
#define FMOD(x,y) 	((x)-(((int)((x)/(y)))*y))

#undef ROUND
#define ROUND(x) 	((int)((x) + (((x)<0.0) ? -0.5 : 0.5)))

/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */

#define UCHAR(c) ((unsigned char) (c))

#define TRUE 	1
#define FALSE 	0

#define SCROLL_MODE_CANVAS	(1<<0)
#define SCROLL_MODE_LISTBOX	(1<<1)

#define RGB_COLOR_ANTIQUEWHITE1	"#ffefdb"
#define RGB_COLOR_BISQUE1	"#ffe4c4"
#define RGB_COLOR_BISQUE2	"#eed5b7"
#define RGB_COLOR_BISQUE3     	"#cdb79e"
#define RGB_COLOR_BLACK		"#000000"
#define RGB_COLOR_BLUE		"#0000ff"
#define RGB_COLOR_GREEN		"#00ff00"
#define RGB_COLOR_GREY		"#b0b0b0"
#define RGB_COLOR_GREY15	"#262626"
#define RGB_COLOR_GREY50	"#7f7f7f"
#define RGB_COLOR_GREY64	"#a3a3a3"
#define RGB_COLOR_GREY70	"#b3b3b3"
#define RGB_COLOR_GREY75	"#bfbfbf"
#define RGB_COLOR_GREY77	"#c3c3c3"
#define RGB_COLOR_GREY82	"#d1d1d1"
#define RGB_COLOR_GREY85	"#d9d9d9"
#define RGB_COLOR_GREY90	"#e5e5e5"
#define RGB_COLOR_GREY95	"#f2f2f2"
#define RGB_COLOR_LIGHTBLUE1	"#bfefff"
#define RGB_COLOR_LIGHTBLUE2	"#b2dfee"
#define RGB_COLOR_LIGHTSKYBLUE1	"#b0e2ff"
#define RGB_COLOR_MAROON	"#b03060"
#define RGB_COLOR_NAVYBLUE	"#000080"
#define RGB_COLOR_PINK		"#ffc0cb"
#define RGB_COLOR_RED		"#ff0000"
#define RGB_COLOR_WHITE		"#ffffff"
#define RGB_COLOR_YELLOW	"#ffff00"

#ifdef OLD_TK_COLORS
#define STD_COLOR_NORMAL_BG	RGB_COLOR_BISQUE1
#define STD_COLOR_ACTIVE_BG	RGB_COLOR_BISQUE2
#define STD_COLOR_SELECT_BG	RGB_COLOR_LIGHTBLUE2
#define STD_COLOR_DISABLE_FG	RGB_COLOR_GREY64
#else
#define STD_COLOR_NORMAL_BG	RGB_COLOR_GREY85
#define STD_COLOR_ACTIVE_BG	RGB_COLOR_GREY64
#define STD_COLOR_SELECT_BG	RGB_COLOR_GREY77
#define STD_COLOR_DISABLE_FG	RGB_COLOR_GREY64
#endif

#define STD_COLOR_INDICATOR	RGB_COLOR_MAROON

#define STD_COLOR_ACTIVE_FG	RGB_COLOR_BLACK
#define STD_COLOR_NORMAL_FG	RGB_COLOR_BLACK
#define STD_COLOR_SELECT_FG	RGB_COLOR_BLACK
#define STD_COLOR_SHADOW	RGB_COLOR_GREY64
#define STD_MONO_ACTIVE_BG	RGB_COLOR_BLACK
#define STD_MONO_ACTIVE_FG	RGB_COLOR_WHITE
#define STD_MONO_NORMAL_BG	RGB_COLOR_WHITE
#define STD_MONO_NORMAL_FG	RGB_COLOR_BLACK
#define STD_MONO_SELECT_BG	RGB_COLOR_BLACK
#define STD_MONO_SELECT_FG	RGB_COLOR_WHITE
#define STD_MONO_SHADOW		RGB_COLOR_BLACK

#define STD_SELECT_BORDERWIDTH	"2"
#define STD_BORDERWIDTH 	"2"

#define STD_FONT_HUGE		"*-Helvetica-Bold-R-Normal-*-18-180-*"
#define STD_FONT_LARGE		"*-Helvetica-Bold-R-Normal-*-14-140-*"
#define STD_FONT		"*-Helvetica-Bold-R-Normal-*-12-120-*"
#define STD_FONT_SMALL		"*-Helvetica-Bold-R-Normal-*-10-100-*"

#ifdef WIN32
#undef STD_FONT
#undef STD_FONT_SMALL
#undef STD_FONT_LARGE
#undef STD_COLOR_NORMAL_BG
#undef STD_COLOR_NORMAL_FG
#undef STD_COLOR_TEXT_FG	
#undef STD_COLOR_SELECT_BG
#undef STD_COLOR_SELECT_FG

#define STD_FONT		"{MS Sans Serif} 10"
#define STD_FONT_SMALL		"{MS Sans Serif} 8"
#define STD_FONT_LARGE		"{MS Sans Serif} 12"
#define STD_COLOR_NORMAL_BG	"SystemButtonFace"
#define STD_COLOR_NORMAL_FG	"SystemButtonText"
#define STD_COLOR_TEXT_FG	"SystemWindowText"
#define STD_COLOR_SELECT_BG	"SystemHighlight"
#define STD_COLOR_SELECT_FG	"SystemHighlightText"
#endif

#define LineWidth(w)	(((w) > 1) ? (w) : 0)

#undef VARARGS
#ifdef __cplusplus
#define ANYARGS (...)
#define VARARGS(first)  (first, ...)
#define VARARGS2(first, second)  (first, second, ...)
#else
#define ANYARGS ()
#define VARARGS(first) ()
#define VARARGS2(first, second) ()
#endif

#if defined(ITCL_NAMESPACES) || (TCL_MAJOR_VERSION >= 8)
#define HAVE_NAMESPACES 1
#else
#define HAVE_NAMESPACES 0
#endif 

extern Tk_Window bltMainWindow;


/*
 * Tcl/Tk Backward compatibility section.
 */
#if (TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION == 0)
typedef ClientData DestroyData;
#define Tk_Cursor Cursor
#else
typedef char *DestroyData;
#endif

#if (TK_MAJOR_VERSION >= 8) 

#define NO_FLAGS			0
#define Blt_FindPhoto(interp, name)	Tk_FindPhoto(interp, name)

#else
#define Tcl_GetStringResult(interp)	((interp)->result)

typedef struct Tcl_Namespace Tcl_Namespace;
typedef struct Tcl_CallFrame *Tcl_CallFrame;

#define Blt_FindPhoto(interp, name)	Tk_FindPhoto(name)

/*
 * The following structure is used by Tk_GetFontMetrics() to return
 * information about the properties of a Tk_Font.  
 */

typedef struct Tk_FontMetrics {
    int ascent;			/* The amount in pixels that the tallest
				 * letter sticks up above the baseline, plus
				 * any extra blank space added by the designer
				 * of the font. */
    int descent;		/* The largest amount in pixels that any
				 * letter sticks below the baseline, plus any
				 * extra blank space added by the designer of
				 * the font. */
    int linespace;		/* The sum of the ascent and descent.  How
				 * far apart two lines of text in the same
				 * font should be placed so that none of the
				 * characters in one line overlap any of the
				 * characters in the other line. */
} Tk_FontMetrics;

typedef XFontStruct *Tk_Font;

#define Tk_FontId(font)			((font)->fid)
#define Tk_TextWidth(font, str, len)	(XTextWidth((font),(str),(len)))
#define Tk_GetFontMetrics(font, fmPtr)  \
	((fmPtr)->ascent = (font)->ascent, (fmPtr)->descent = (font)->descent, \
	 (fmPtr)->linespace = (font)->ascent + (font)->descent)

#define Tk_NameOfFont(font)		(Tk_NameOfFontStruct(font))
#define Tk_DrawChars(dpy, draw, gc, font, str, len, x, y) \
    TkDisplayChars((dpy),(draw),(gc),(font),(str),(len),(x),(y), 0, DEF_TEXT_FLAGS)

#define Tk_MeasureChars(font, text, len, maxPixels, flags, lenPtr) \
    TkMeasureChars((font),(text), (len), 0, maxPixels, 0,(flags), (lenPtr))

extern int TkMeasureChars _ANSI_ARGS_((Tk_Font font, char *source, 
	int maxChars, int startX, int maxX, int tabOrigin, int flags, 
	int *nextXPtr));
extern void TkDisplayChars _ANSI_ARGS_((Display *display, Drawable drawable,
	GC gc, Tk_Font font, char *string, int numChars, int x, int y, 
	int tabOrigin, int flags));

/*
 * FLAGS passed to TkMeasureChars:
 */
#define TK_WHOLE_WORDS			1
#define TK_AT_LEAST_ONE			2
#define TK_PARTIAL_OK			4
#define TK_IGNORE_NEWLINES		8
#define TK_IGNORE_TABS			16
#define NO_FLAGS			0

#endif


#define DEF_TEXT_FLAGS			(TK_PARTIAL_OK | TK_IGNORE_NEWLINES)


#define Panic(mesg)	panic("%s:%d %s", __FILE__, __LINE__, (mesg))

/*
 * Since the Tcl/Tk distribution doesn't perform any asserts, dynamic loading
 * can fail to find the __assert function.  As a workaround, we'll include
 * our own.
 */
#undef	assert
#ifdef	NDEBUG
#define	assert(EX) ((void)0)
#else
extern void Blt_Assert _ANSI_ARGS_((char *testExpr, char *fileName, int lineNum));
#if defined(__STDC__)
#define	assert(EX) (void)((EX) || (Blt_Assert(#EX, __FILE__, __LINE__), 0))
#else
#define	assert(EX) (void)((EX) || (Blt_Assert("EX", __FILE__, __LINE__), 0))
#endif	/* __STDC__ */
#endif	/* NDEBUG */

typedef int *Blt_Tile;		/* Opaque type for tiles */

typedef int (QSortCompareProc) _ANSI_ARGS_((const void *, const void *));


/*
 * ----------------------------------------------------------------------
 *
 * Blt_CmdSpec --
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *name;			/* Name of command */
    Tcl_CmdProc *cmdProc;
    Tcl_CmdDeleteProc *cmdDeleteProc;
    ClientData clientData;
} Blt_CmdSpec;

/*
 * ----------------------------------------------------------------------
 *
 * Blt_Operation --
 *
 * 	Generic function prototype of CmdOptions.
 *
 * ----------------------------------------------------------------------
 */
typedef int (*Blt_Operation) _ANSI_ARGS_(ANYARGS);

/*
 * ----------------------------------------------------------------------
 *
 * Blt_OpSpec --
 *
 * 	Structure to specify a set of operations for a Tcl command.
 *      This is passed to the Blt_GetOperation procedure to look
 *      for a function pointer associated with the operation name.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *name;			/* Name of operation */
    int minChars;		/* Minimum # characters to disambiguate */
    Blt_Operation proc;
    int minArgs;		/* Minimum # args required */
    int maxArgs;		/* Maximum # args required */
    char *usage;		/* Usage message */

} Blt_OpSpec;

typedef enum {
    BLT_OPER_ARG0,		/* Op name is the first argument */
    BLT_OPER_ARG1,		/* Op name is the second argument */
    BLT_OPER_ARG2,		/* Op name is the third argument */
    BLT_OPER_ARG3,		/* Op name is the fourth argument */
    BLT_OPER_ARG4		/* Op name is the fifth argument */

} Blt_OpIndex;

extern Blt_Operation Blt_GetOperation _ANSI_ARGS_((Tcl_Interp *interp,
	int numSpecs, Blt_OpSpec * specArr, Blt_OpIndex argIndex,
	int numArgs, char **argArr));

extern int Blt_LookupOperation _ANSI_ARGS_((Blt_OpSpec *specArr, int numSpecs,  
     char *string));

/*
 * ----------------------------------------------------------------------
 *
 * Pad --
 *
 * 	Structure to specify vertical and horizontal padding.
 *
 *	This allows padding to be specified on a per side basis.
 *	Vertically, side1 and side2 refer to the top and bottom sides.
 *	Horizontally, they refer to the left and right sides.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    short int side1, side2;
} Pad;

#define padLeft  	padX.side1
#define padRight  	padX.side2
#define padTop		padY.side1
#define padBottom	padY.side2
#define PADDING(x)	((x).side1 + (x).side2)

/*
 * ----------------------------------------------------------------------
 *
 * The following enumerated values are used as bit flags.
 *
 *
 * ----------------------------------------------------------------------
 */
typedef enum {
    FILL_NONE,			/* Neither coordinate plane is specified */
    FILL_X,			/* Horizontal plane */
    FILL_Y,			/* Vertical plane */
    FILL_BOTH			/* Both vertical and horizontal planes */
} Fill;

/*
 * ----------------------------------------------------------------------
 *
 * Dashes --
 *
 * 	List of dash values (maximum 11 based upon PostScript limit).
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char valueArr[12];
    int numValues;
    int offset;
} Dashes;

extern void Blt_SetDashes _ANSI_ARGS_((Display *display, GC gc, 
	Dashes *dashesPtr));
extern Dashes *Blt_GetDashes _ANSI_ARGS_((GC gc));

/*
 * ----------------------------------------------------------------------
 *
 * TextSegment --
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    int x, y;			/* Coordinates of the baseline */
    char *text;			/* Text to be displayed */
    int numChars;		/* Number of characters in text */
    int width;			/* Width of line in pixels. This information
				 * is used to draw PostScript strings the
				 * same width as X */
} TextSegment;

/*
 * ----------------------------------------------------------------------
 *
 * CompoundText --
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    int numSegments;		/* # segments of text */
    short int width, height;	/* Dimensions of text bounding box */
    TextSegment segArr[1];	/* Information about each segment of text */
} CompoundText;

typedef struct Shadow {
    XColor *color;
    int offset;
} Shadow;

/*
 * ----------------------------------------------------------------------
 *
 * TextAttributes --
 *
 * 	Represents a convenient structure to hold text attributes
 *	which determine how a text string is to be displayed on the
 *	window, or drawn with PostScript commands.  The alternative
 *	is to pass lots of parameters to the drawing and printing
 *	routines. This seems like a more efficient and less cumbersome
 *	way of passing parameters.
 *
 * ----------------------------------------------------------------------
 */
typedef struct TextAttributes {
    unsigned int state;		/* If non-zero, indicates to draw text
				 * in the active color */
    short int width, height;	/* Extents of text */

    XColor *color;		/* Normal color */
    XColor *activeColor;	/* Active color */
    Tk_Font font;		/* Font to use to draw text */
    Tk_3DBorder border;		/* Background color of text.  This is also
				 * used for drawing disabled text. */
    Shadow shadow;		/* Drop shadow color and offset */
    Tk_Justify justify;		/* Justification of the text string. This
				 * only matters if the text is composed
				 * of multiple lines. */
    GC gc;			/* GC used to draw the text */
    double theta;		/* Rotation of text in degrees. */
    Tk_Anchor anchor;		/* Indicates how the text is anchored around
				 * its x and y coordinates. */
    Pad padX, padY;		/* # pixels padding of around text region */
    short int leader;		/* # pixels spacing between lines of text */
    
} TextAttributes;

/*
 * -------------------------------------------------------------------
 *
 * Point2D --
 *
 *	Represents a single coordinate in 2D space.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    double x, y;
} Point2D;

/*
 * -------------------------------------------------------------------
 *
 * Point3D --
 *
 *	Represents a single coordinate in 3D space.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    double x, y, z;
} Point3D;

/*
 * -------------------------------------------------------------------
 *
 * Dimension --
 *
 *	Represents the size of a rectangle.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    int width, height;
} Dimension;


/*
 *----------------------------------------------------------------------
 *
 * ImageRegion --
 *
 *      Designates a rectangular region of an image.  Used to copy
 *	parts of images.
 *
 *----------------------------------------------------------------------
 */
typedef struct ImageRegion {
    int x, y;			/* Upper left corner coordinates of
				 * the rectangle defining the
				 * region. */
    int width, height;		/* Dimensions of the rectangular region. */
} ImageRegion;

/*
 * -------------------------------------------------------------------
 *
 * ColorPair --
 *
 *	Holds a pair of foreground, background colors.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    XColor *fgColor, *bgColor;
} ColorPair;

#define COLOR_NONE		(XColor *)0
#define COLOR_DEFAULT		(XColor *)1
#define COLOR_ALLOW_DEFAULTS	1

extern int Blt_GetColorPair _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *fgColor, char *bgColor, ColorPair * pairPtr, int colorFlag));
extern void Blt_FreeColorPair _ANSI_ARGS_((ColorPair * pairPtr));

#define STATE_NORMAL	0
#define STATE_ACTIVE	(1<<0)
#define STATE_DISABLED	(1<<1)
#define STATE_EMPHASIS	(1<<2)



typedef struct BindTable *BindTable;
typedef ClientData (BindPickProc) _ANSI_ARGS_((ClientData clientData,
	int x, int y));

typedef void (BindTagProc) _ANSI_ARGS_((BindTable bindTable, ClientData object, 
	ClientData *tagArr, int *numTagsPtr));

/*
 *  Binding structure information:
 */

struct BindTable {
    int flags;
    Tk_BindingTable bindingTable;
				/* Table of all bindings currently defined.
				 * NULL means that no bindings exist, so the 
				 * table hasn't been created.  Each "object" 
				 * used for this table is either a Tk_Uid for 
				 * a tag or the address of an item named by 
				 * id. */

    ClientData currentItem;	/* The item currently containing the mouse
				 * pointer, or NULL if none. */

    ClientData newItem;		/* The item that is about to become the
				 * current one, or NULL.  This field is
				 * used to detect deletions of the new
				 * current item pointer that occur during
				 * Leave processing of the previous current
				 * tab.  */
    
    ClientData focusItem;

    XEvent pickEvent;		/* The event upon which the current choice
				 * of the current tab is based.  Must be saved
				 * so that if the current item is deleted,
				 * we can pick another. */
    int activePick;		/* The pick event has been initialized so 
				 * that we can repick it */

    int state;			/* Last known modifier state.  Used to
				 * defer picking a new current object
				 * while buttons are down. */

    ClientData clientData;
    Tk_Window tkwin;
    BindPickProc *pickProc;	/* Routine called to report the item the
				 * mouse is currently over. */
    BindTagProc *tagProc;	/* Routine called to report tags picked items. */

};

extern void Blt_DestroyBindingTable _ANSI_ARGS_((BindTable table));

extern BindTable Blt_CreateBindingTable _ANSI_ARGS_((Tcl_Interp *interp,
    Tk_Window tkwin, ClientData clientData, BindPickProc *pickProc,
    BindTagProc *tagProc));

extern int Blt_ConfigureBindings _ANSI_ARGS_((Tcl_Interp *interp, BindTable table, 
	ClientData item, int argc, char **argv));

extern void Blt_PickCurrentItem _ANSI_ARGS_((BindTable table));

extern void Blt_DeleteAllBindings _ANSI_ARGS_((BindTable table, ClientData object));

#define Blt_SetFocusItem(bindPtr, object) \
	((bindPtr)->focusItem = (ClientData)(object))

#define Blt_GetCurrentItem(bindPtr)  ((bindPtr)->currentItem)
#define Blt_GetLatestItem(bindPtr)  ((bindPtr)->newItem)

#define Blt_GetBindingData(bindPtr)  ((bindPtr)->clientData)

/*
 * ----------------------------------------------------------------------
 *
 * 	X11/Xosdefs.h requires XNOSTDHDRS be set for some systems.
 *	This is a guess.  If I can't find STDC headers or unistd.h,
 *	assume that this is non-POSIX and non-STDC environment.
 *	(needed for Encore Umax 3.4 ?)
 *
 * ----------------------------------------------------------------------
 */
#if !defined(STDC_HEADERS) && !defined(HAVE_UNISTD_H)
#define XNOSTDHDRS 	1
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	Assume we need to declare free if there's no stdlib.h or malloc.h
 *
 * ----------------------------------------------------------------------
 */
#if !defined(HAVE_STDLIB_H) && !defined(HAVE_MALLOC_H)
extern void free _ANSI_ARGS_((void *));
#endif

/*
 * ----------------------------------------------------------------------
 *
 *	On some systems "strdup" and "strcasecmp" are in the C library,
 *      but have no declarations in the C header files. Make sure we
 *      supply them here.
 *
 * ----------------------------------------------------------------------
 */
#ifdef NO_DECL_STRDUP
extern char *strdup _ANSI_ARGS_((CONST char *s));
#endif

#ifdef NO_DECL_DRAND48
extern double drand48 _ANSI_ARGS_((void));
extern void srand48 _ANSI_ARGS_((long seed));
#endif

#ifdef NO_DECL_STRCASECMP
extern int strcasecmp _ANSI_ARGS_((CONST char *s1, CONST char *s2));
#endif

extern int Blt_StringToFlag _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int flags));
extern char *Blt_FlagToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin, 
	char *string, int offset, Tcl_FreeProc **freeProc));

extern Tk_Uid Blt_GetUid _ANSI_ARGS_((char *string));
extern void Blt_FreeUid _ANSI_ARGS_((Tk_Uid uid));
extern Tk_Uid Blt_FindUid _ANSI_ARGS_((char *string));

extern char *Blt_Int _ANSI_ARGS_((int value));
extern char *Blt_Double _ANSI_ARGS_((Tcl_Interp *interp, double value));

extern void Blt_InitHexTable _ANSI_ARGS_((char *table));

extern GC Blt_GetPrivateGC _ANSI_ARGS_((Tk_Window tkwin, unsigned long gcMask, 
	XGCValues *valuePtr));

extern GC Blt_GetPrivateGCFromDrawable _ANSI_ARGS_((Tk_Window tkwin, 
	Drawable drawable, unsigned long gcMask, XGCValues *valuePtr));

extern void Blt_FreePrivateGC _ANSI_ARGS_((Display *display, GC gc));

extern Tk_Window Blt_FindChild _ANSI_ARGS_((Tk_Window parent, char *childName));

extern Tk_Window Blt_Toplevel _ANSI_ARGS_((Tk_Window tkwin));

extern int Blt_GetLength _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *string, int *valuePtr));

extern char *Blt_NameOfFill _ANSI_ARGS_((Fill fill));

extern int Blt_GetXYPosition _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *string, int *x, int *y));

extern Tcl_Command Blt_InitCmd _ANSI_ARGS_((Tcl_Interp *interp, 
	char *nameSpace, Blt_CmdSpec *specPtr));

extern int Blt_InitCmds _ANSI_ARGS_((Tcl_Interp *interp, char *nameSpace,
	Blt_CmdSpec *specPtr, int numCmds));

extern int Blt_ConfigModified _ANSI_ARGS_(TCL_VARARGS(Tk_ConfigSpec *, specs));

extern void Blt_DStringAppendElements _ANSI_ARGS_(TCL_VARARGS(Tcl_DString *, args));

extern void Blt_MakeInputOnlyWindowExist _ANSI_ARGS_((Tk_Window tkwin));

extern void Blt_GetBoundingBox _ANSI_ARGS_((int width, int height,
	double theta, int *widthPtr, int *heightPtr, XPoint *pointArr));

extern CompoundText *Blt_GetCompoundText _ANSI_ARGS_((char *string, 
	TextAttributes *attrPtr));

extern void Blt_GetTextExtents _ANSI_ARGS_((TextAttributes *attrPtr, char *text,
	int *widthPtr, int *heightPtr));

extern void Blt_InitTextAttributes _ANSI_ARGS_((TextAttributes *attrPtr));

extern void Blt_ResetTextAttributes _ANSI_ARGS_((Tk_Window tkwin, 
	TextAttributes *attrPtr));

extern void Blt_FreeTextAttributes _ANSI_ARGS_((Display *display, 
	TextAttributes *attrPtr));

extern void Blt_SetTextDrawAttributes _ANSI_ARGS_((TextAttributes *attrPtr, 
	Tk_Font font, GC gc, XColor *normalColor, XColor *activeColor, 
	XColor *shadowColor, double theta, Tk_Anchor anchor, Tk_Justify justify, 
	int leader, int shadowOffset));

extern void Blt_SetTextPrintAttributes _ANSI_ARGS_((TextAttributes *attrPtr, 
	Tk_Font font, XColor *fgColor, XColor *bgColor, XColor *shadowColor, 
	double theta, Tk_Anchor anchor, Tk_Justify justify, int leader, 
	int shadowOffset));

extern void Blt_DrawText _ANSI_ARGS_((Tk_Window tkwin, Drawable drawable,
	char *string, TextAttributes *attrPtr, int x, int y));

extern void Blt_DrawCompoundText _ANSI_ARGS_((Tk_Window tkwin, Drawable drawable,
	CompoundText *textPtr, TextAttributes *attrPtr, int x, int y));

extern void Blt_DrawText2 _ANSI_ARGS_((Tk_Window tkwin, Drawable drawable,
	char *string, TextAttributes *attrPtr, int x, int y, 
	Dimension *dimPtr));

extern Pixmap Blt_CreateTextBitmap _ANSI_ARGS_((Tk_Window tkwin, 
	CompoundText *textPtr, TextAttributes *attrPtr, int *widthPtr, 
	int *heightPtr, int isMask));

extern void Blt_InitBitmapGC _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin));
extern void Blt_InitEpsCanvasItem _ANSI_ARGS_((Tcl_Interp *interp));

extern Pixmap Blt_RotateBitmap _ANSI_ARGS_((Tk_Window tkwin, Pixmap bitmap, 
	int width, int height, double theta, int *widthPtr, int *heightPtr));

extern Pixmap Blt_ScaleBitmap _ANSI_ARGS_((Tk_Window tkwin, Pixmap srcBitmap, 
	int srcWidth, int srcHeight, int scaledWidth, int scaledHeight));

extern Pixmap Blt_ScaleBitmapRegion _ANSI_ARGS_((Tk_Window tkwin, 
	Pixmap srcBitmap, int srcWidth, int srcHeight, 
	int scaledWidth, int scaledHeight, ImageRegion *regionPtr));

extern void Blt_TranslateAnchor _ANSI_ARGS_((int x, int y, int width, 
	int height, Tk_Anchor anchor, int *transXPtr, int *transYPtr));

typedef void (Blt_TileChangedProc) _ANSI_ARGS_((ClientData clientData,
	Blt_Tile tile));

extern Blt_Tile Blt_GetTile _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	char *imageName));

extern void Blt_FreeTile _ANSI_ARGS_((Blt_Tile tile));

extern char *Blt_NameOfTile _ANSI_ARGS_((Blt_Tile tile));

extern void Blt_SetTileChangedProc _ANSI_ARGS_((Blt_Tile tile,
	Blt_TileChangedProc * changeProc, ClientData clientData));

extern Pixmap Blt_PixmapOfTile _ANSI_ARGS_((Blt_Tile tile));

extern void Blt_SizeOfTile _ANSI_ARGS_((Blt_Tile tile, int *widthPtr,
	int *heightPtr));

extern void Blt_SetTileOrigin _ANSI_ARGS_((Tk_Window tkwin, GC gc, int x,
	int y));

extern int Blt_ConfigureWidgetComponent _ANSI_ARGS_((Tcl_Interp *interp, 
	Tk_Window tkwin, char *name, char *class, Tk_ConfigSpec *specs, 
	int argc, char **argv, char *widgRec, int flags));

extern void Blt_HSV _ANSI_ARGS_((XColor *colorPtr, double *huePtr, 
	double *valPtr, double *satPtr));

extern void Blt_RGB _ANSI_ARGS_((double hue, double sat, double val, 
	XColor *colorPtr));

extern int Blt_NaturalSpline _ANSI_ARGS_((double *xCntl, double *yCtrl, 
	int numCtrls, double *X, double *Y, int numPoints));

extern int Blt_QuadraticSpline _ANSI_ARGS_((double *xCntl, double *yCtrl, 
	int numCtrls, double *X, double *Y, int numPoints, double epsilon));

extern int Blt_ParseFlag _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
extern char *Blt_FlagPrint _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

extern void Blt_GraphTags _ANSI_ARGS_((BindTable table, ClientData object, 
    ClientData *tagArr, int *numTagsPtr));

extern Window Blt_WindowId _ANSI_ARGS_((Tk_Window tkwin));
extern void Blt_MapTopLevelWindow _ANSI_ARGS_((Tk_Window tkwin));
extern void Blt_UnmapTopLevelWindow _ANSI_ARGS_((Tk_Window tkwin));
extern void Blt_RaiseTopLevelWindow _ANSI_ARGS_((Tk_Window tkwin));

extern int Blt_AdjustViewport _ANSI_ARGS_((int offset, int worldSize, 
	int windowSize, int scrollUnits, int scrollMode));

extern int Blt_GetScrollInfo _ANSI_ARGS_((Tcl_Interp *interp, int argc, 
	char **argv, int *offsetPtr, int worldSize, int windowSize, 
	int scrollUnits, int scrollMode));

extern void Blt_UpdateScrollbar _ANSI_ARGS_((Tcl_Interp *interp, 
	char *scrollCmd, double firstFract, double lastFract));

extern int Blt_ReparentWindow _ANSI_ARGS_((Display *display, Window window,
	Window newParent, int x, int y));

extern void panic _ANSI_ARGS_(VARARGS(char *fmt));

#ifndef TCL_NAMESPACE_ONLY
#define TCL_NAMESPACE_ONLY TCL_GLOBAL_ONLY
#endif

extern Tcl_Namespace *Tcl_GetCurrentNamespace _ANSI_ARGS_((Tcl_Interp *interp));
extern Tcl_Namespace *Tcl_GetGlobalNamespace _ANSI_ARGS_((Tcl_Interp *interp));
extern Tcl_Command Tcl_FindCommand _ANSI_ARGS_((Tcl_Interp *interp, 
	char *name, Tcl_Namespace *nsPtr, int flags));

#if (TCL_MAJOR_VERSION >= 8)

extern Tcl_Namespace *Tcl_CreateNamespace _ANSI_ARGS_((Tcl_Interp *interp,
	char *name, ClientData clientData, 
	Tcl_NamespaceDeleteProc * nsDelProc));

extern void Tcl_DeleteNamespace _ANSI_ARGS_((Tcl_Namespace *nsPtr));

extern Tcl_Namespace *Tcl_FindNamespace _ANSI_ARGS_((Tcl_Interp *interp, 
	char *name, Tcl_Namespace *context, int flags));

extern int Tcl_Export _ANSI_ARGS_((Tcl_Interp *interp, Tcl_Namespace *nsPtr, 
	char *name, int resetFlag));

extern Tcl_Var Tcl_FindNamespaceVar _ANSI_ARGS_((Tcl_Interp *interp, char *name,
    Tcl_Namespace *contextNsPtr, int flags));

extern void Tcl_PopCallFrame _ANSI_ARGS_((Tcl_Interp* interp));

extern int Tcl_PushCallFrame _ANSI_ARGS_((Tcl_Interp* interp, 
	Tcl_CallFrame *framePtr, Tcl_Namespace *nsPtr, int isProcCallFrame)); 

#endif /* TCL_MAJOR_VERSION >= 8 */

extern Tcl_Namespace *Blt_NamespaceOfVariable _ANSI_ARGS_((Tcl_Interp *interp,
	char *varName));

extern Tcl_CallFrame *Blt_EnterNamespace _ANSI_ARGS_((Tcl_Interp *interp,
	Tcl_Namespace *nsPtr));

extern void Blt_LeaveNamespace _ANSI_ARGS_((Tcl_Interp *interp, 
	Tcl_CallFrame *framePtr));

extern int Blt_ParseQualifiedName _ANSI_ARGS_((Tcl_Interp *interp, char *name, 
	Tcl_Namespace **nsPtrPtr, char **namePtr));

extern Tcl_Command Blt_CreateCommand _ANSI_ARGS_((Tcl_Interp *interp, 
	char *cmdName, Tcl_CmdProc *proc, ClientData clientData,
	Tcl_CmdDeleteProc *deleteProc));

#if HAVE_JPEGLIB_H
extern int Blt_JPEGToPhoto _ANSI_ARGS_((Tcl_Interp *interp, char *fileName,
	char *photoName));
#endif  /* HAVE_JPEGLIB_H */

/* 
 * Define this if you want to be able to tile to the main window "."
 * This will cause a conflict with Tk if you try to compile and link
 * statically.  
 */
#undef TILE_MAINWINDOW	

#ifndef NO_BEEP
extern Tcl_AppInitProc Blt_BeepInit;
#endif
#ifndef NO_BGEXEC
extern Tcl_AppInitProc Blt_BgexecInit;
#endif
#ifndef NO_BITMAP
extern Tcl_AppInitProc Blt_BitmapInit;
#endif
#ifndef NO_BITMAP
extern Tcl_AppInitProc Blt_BitmapInit;
#endif
#ifndef NO_BUSY
extern Tcl_AppInitProc Blt_BusyInit;
#endif
#ifndef NO_CONTAINER
extern Tcl_AppInitProc Blt_ContainerInit;
#endif
#ifndef NO_CUTBUFFER
extern Tcl_AppInitProc Blt_CutbufferInit;
#endif
#ifndef NO_DEBUG
extern Tcl_AppInitProc Blt_DebugInit;
#endif
#ifndef NO_DRAGDROP
extern Tcl_AppInitProc Blt_DndInit;
#endif
#ifndef NO_GRAPH
extern Tcl_AppInitProc Blt_GraphInit;
#endif
#ifndef NO_HIERBOX
extern Tcl_AppInitProc Blt_HierboxInit;
#endif
#ifndef NO_HTEXT
extern Tcl_AppInitProc Blt_HtextInit;
#endif
#ifdef WIN32
#ifndef NO_PRINTER
extern Tcl_AppInitProc Blt_PrinterInit;
#endif
#endif
#ifndef NO_TABLE
extern Tcl_AppInitProc Blt_TableInit;
#endif
#ifndef NO_VECTOR
extern Tcl_AppInitProc Blt_VectorInit;
#endif
#ifndef NO_WINOP
extern Tcl_AppInitProc Blt_WinopInit;
#endif
#ifndef NO_WATCH
extern Tcl_AppInitProc Blt_WatchInit;
#endif
#ifndef NO_SPLINE
extern Tcl_AppInitProc Blt_SplineInit;
#endif
#ifndef NO_TABSET
extern Tcl_AppInitProc Blt_TabsetInit;
#endif
#ifndef NO_TILEFRAME
extern Tcl_AppInitProc Blt_FrameInit;
#endif
#ifndef NO_TILEBUTTON
extern Tcl_AppInitProc Blt_ButtonInit;
#endif
#ifndef NO_TILESCROLLBAR
extern Tcl_AppInitProc Blt_ScrollbarInit;
#endif

#if (BLT_MAJOR_VERSION == 3)
#ifndef NO_MOUNTAIN
extern Tcl_AppInitProc Blt_MountainInit;
#endif
#ifndef NO_TED
extern Tcl_AppInitProc Blt_TedInit;
#endif
#endif



#ifdef WIN32

#ifdef CHECK_UNICODE_CALLS

#define _UNICODE
#define UNICODE

#define __TCHAR_DEFINED
typedef float *_TCHAR;

#define _TCHAR_DEFINED
typedef float *TCHAR;

#endif /* CHECK_UNICODE_CALLS */

/*
 * Define EINPROGRESS in terms of WSAEINPROGRESS.
 */

#ifndef	EINPROGRESS
#define EINPROGRESS WSAEINPROGRESS
#endif

/*
 * If ENOTSUP is not defined, define it to a value that will never occur.
 */

#ifndef ENOTSUP
#define	ENOTSUP		-1030507
#endif

/*
 * The following defines redefine the Windows Socket errors as
 * BSD errors so Tcl_PosixError can do the right thing.
 */

#ifndef EWOULDBLOCK
#define EWOULDBLOCK             EAGAIN
#endif
#ifndef EALREADY
#define EALREADY	149	/* operation already in progress */
#endif
#ifndef ENOTSOCK
#define ENOTSOCK	95	/* Socket operation on non-socket */
#endif
#ifndef EDESTADDRREQ
#define EDESTADDRREQ	96	/* Destination address required */
#endif
#ifndef EMSGSIZE
#define EMSGSIZE	97	/* Message too long */
#endif
#ifndef EPROTOTYPE
#define EPROTOTYPE	98	/* Protocol wrong type for socket */
#endif
#ifndef ENOPROTOOPT
#define ENOPROTOOPT	99	/* Protocol not available */
#endif
#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT	120	/* Protocol not supported */
#endif
#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT	121	/* Socket type not supported */
#endif
#ifndef EOPNOTSUPP
#define EOPNOTSUPP	122	/* Operation not supported on socket */
#endif
#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT	123	/* Protocol family not supported */
#endif
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT	124	/* Address family not supported */
#endif
#ifndef EADDRINUSE
#define EADDRINUSE	125	/* Address already in use */
#endif
#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL	126	/* Can't assign requested address */
#endif
#ifndef ENETDOWN
#define ENETDOWN	127	/* Network is down */
#endif
#ifndef ENETUNREACH
#define ENETUNREACH	128	/* Network is unreachable */
#endif
#ifndef ENETRESET
#define ENETRESET	129	/* Network dropped connection on reset */
#endif
#ifndef ECONNABORTED
#define ECONNABORTED	130	/* Software caused connection abort */
#endif
#ifndef ECONNRESET
#define ECONNRESET	131	/* Connection reset by peer */
#endif
#ifndef ENOBUFS
#define ENOBUFS		132	/* No buffer space available */
#endif
#ifndef EISCONN
#define EISCONN		133	/* Socket is already connected */
#endif
#ifndef ENOTCONN
#define ENOTCONN	134	/* Socket is not connected */
#endif
#ifndef ESHUTDOWN
#define ESHUTDOWN	143	/* Can't send after socket shutdown */
#endif
#ifndef ETOOMANYREFS
#define ETOOMANYREFS	144	/* Too many references: can't splice */
#endif
#ifndef ETIMEDOUT
#define ETIMEDOUT	145	/* Connection timed out */
#endif
#ifndef ECONNREFUSED
#define ECONNREFUSED	146	/* Connection refused */
#endif
#ifndef ELOOP
#define ELOOP		90	/* Symbolic link loop */
#endif
#ifndef EHOSTDOWN
#define EHOSTDOWN	147	/* Host is down */
#endif
#ifndef EHOSTUNREACH
#define EHOSTUNREACH	148	/* No route to host */
#endif
#ifndef ENOTEMPTY
#define ENOTEMPTY 	93	/* directory not empty */
#endif
#ifndef EUSERS
#define EUSERS		94	/* Too many users (for UFS) */
#endif
#ifndef EDQUOT
#define EDQUOT		49	/* Disc quota exceeded */
#endif
#ifndef ESTALE
#define ESTALE		151	/* Stale NFS file handle */
#endif
#ifndef EREMOTE
#define EREMOTE		66	/* The object is remote */
#endif

/*
 * Supply definitions for macros to query wait status, if not already
 * defined in header files above.
 */

#if TCL_UNION_WAIT
#   define WAIT_STATUS_TYPE union wait
#else
#   define WAIT_STATUS_TYPE int
#endif

#ifndef WIFEXITED
#   define WIFEXITED(stat)  (((*((int *) &(stat))) & 0xff) == 0)
#endif

#ifndef WEXITSTATUS
#   define WEXITSTATUS(stat) (((*((int *) &(stat))) >> 8) & 0xff)
#endif

#ifndef WIFSIGNALED
#   define WIFSIGNALED(stat) (((*((int *) &(stat)))) && ((*((int *) &(stat))) == ((*((int *) &(stat))) & 0x00ff)))
#endif

#ifndef WTERMSIG
#   define WTERMSIG(stat)    ((*((int *) &(stat))) & 0x7f)
#endif

#ifndef WIFSTOPPED
#   define WIFSTOPPED(stat)  (((*((int *) &(stat))) & 0xff) == 0177)
#endif

#ifndef WSTOPSIG
#   define WSTOPSIG(stat)    (((*((int *) &(stat))) >> 8) & 0xff)
#endif

/*
 * Define constants for waitpid() system call if they aren't defined
 * by a system header file.
 */

#ifndef WNOHANG
#   define WNOHANG 1
#endif
#ifndef WUNTRACED
#   define WUNTRACED 2
#endif


extern double hypot (double x, double y);

extern char *Blt_StrDup(const char *string);
extern void *Blt_Calloc(unsigned int numElems, unsigned int sizeOfElem);
extern int Blt_AsyncRead(int fd, char *buffer, unsigned int size);
extern int Blt_AsyncWrite(int fd, char *buffer, unsigned int size);
extern void Blt_CreateFileHandler(int fd, int flags, Tcl_FileProc *proc, 
    ClientData clientData);
extern void Blt_DeleteFileHandler(int fd);
extern int Blt_GetPlatformId(void);
extern char *Blt_Win32Error(void);
extern int Blt_GetOpenPrinter(Tcl_Interp *interp, const char *id, 
	Drawable *drawablePtr);
extern int Blt_OpenPrinterDoc(Tcl_Interp *interp, const char *id);
extern int Blt_ClosePrinterDoc(Tcl_Interp *interp, const char *id);

#define strdup(s)		Blt_StrDup(s)
#define calloc(s,n)		Blt_Calloc((s),(n))
#define malloc(s)		Tcl_Alloc(s)
#define realloc(p, s)		Tcl_Realloc((p), (s))
#define free			Tcl_Free
#define strncasecmp(s1,s2,n)	_strnicmp(s1,s2,n)
#define strcasecmp(s1,s2)	_stricmp(s1,s2)

#undef EXPORT 
#define EXPORT __declspec(dllexport)

#ifdef __MINGW32__
#ifndef DeleteBitmap
#define DeleteBitmap(hbm)       DeleteObject((HGDIOBJ)(HBITMAP)(hbm))
#endif
#ifndef DeleteBrush
#define DeleteBrush(hbr)	DeleteObject((HGDIOBJ)(HBRUSH)(hbr))
#endif
#ifndef DeleteFont
#define DeleteFont(hfont)	DeleteObject((HGDIOBJ)(HFONT)(hfont))
#endif
#ifndef DeletePalette
#define DeletePalette(hpal)     DeleteObject((HGDIOBJ)(HPALETTE)(hpal))
#endif
#ifndef DeletePen
#define DeletePen(hpen)		DeleteObject((HGDIOBJ)(HPEN)(hpen))
#endif
#ifndef SelectBitmap
#define SelectBitmap(hdc, hbm)  ((HBITMAP)SelectObject((hdc), (HGDIOBJ)(HBITMAP)(hbm)))
#endif
#ifndef SelectBrush
#define SelectBrush(hdc, hbr)   ((HBRUSH)SelectObject((hdc), (HGDIOBJ)(HBRUSH)(hbr)))
#endif
#ifndef SelectFont
#define SelectFont(hdc, hfont)  ((HFONT)SelectObject((hdc), (HGDIOBJ)(HFONT)(hfont)))
#endif
#ifndef SelectPen
#define SelectPen(hdc, hpen)    ((HPEN)SelectObject((hdc), (HGDIOBJ)(HPEN)(hpen)))
#endif
#ifndef GetNextWindow
#define GetNextWindow(hWnd,wCmd) GetWindow((hWnd),(wCmd))
#endif
#endif /* __MINWIN32__ */

extern GC Blt_CreateGC _ANSI_ARGS_((Display* display, Drawable drawable, 
	unsigned long mask, XGCValues* valuesPtr));

#endif /* WIN32 */

#endif /*_BLT_INT_H*/

