/*
 * bltTkInt.h --
 *
 *	Contains copies of internal Tk structures.
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

#ifndef _BLT_TKINT_H
#define _BLT_TKINT_H

/*
 * The Border structure used internally by the Tk_3D* routines.
 * The following is a copy of it from tk3d.c.
 */

typedef struct {
    Screen *screen;		/* Screen on which the border will be used. */
    Visual *visual;		/* Visual for all windows and pixmaps using
				 * the border. */
    int depth;			/* Number of bits per pixel of drawables where
				 * the border will be used. */
    Colormap colormap;		/* Colormap out of which pixels are
				 * allocated. */
    int refCount;		/* Number of different users of
				 * this border.  */
    XColor *bgColor;		/* Background color (intensity
				 * between lightColorPtr and
				 * darkColorPtr). */
    XColor *darkColor;		/* Color for darker areas (must free when
				 * deleting structure). NULL means shadows
				 * haven't been allocated yet.*/
    XColor *lightColor;		/* Color used for lighter areas of border
				 * (must free this when deleting structure).
				 * NULL means shadows haven't been allocated
				 * yet. */
    Pixmap shadow;		/* Stipple pattern to use for drawing
				 * shadows areas.  Used for displays with
				 * <= 64 colors or where colormap has filled
				 * up. */
    GC bgGC;			/* Used (if necessary) to draw areas in
				 * the background color. */
    GC darkGC;			/* Used to draw darker parts of the
				 * border. None means the shadow colors
				 * haven't been allocated yet.*/
    GC lightGC;			/* Used to draw lighter parts of
				 * the border. None means the shadow colors
				 * haven't been allocated yet. */
    Tcl_HashEntry *hashPtr;	/* Entry in borderTable (needed in
				 * order to delete structure). */
} TkBorder;


typedef struct TkWindow *TkWindow;
typedef struct TkFontAttributes {
    Tk_Uid family;		/* Font family. The most important field. */
    int pointsize;		/* Pointsize of font, 0 for default size, or
				 * negative number meaning pixel size. */
    int weight;			/* Weight flag; see below for def'n. */
    int slant;			/* Slant flag; see below for def'n. */
    int underline;		/* Non-zero for underline font. */
    int overstrike;		/* Non-zero for overstrike font. */
} TkFontAttributes;

typedef struct TkFontMetrics {
    int	ascent;			/* From baseline to top of font. */
    int	descent;		/* From baseline to bottom of font. */
    int maxWidth;		/* Width of widest character in font. */
    int fixed;			/* Non-zero if this is a fixed-width font,
				 * 0 otherwise. */
} TkFontMetrics;

typedef struct TkFont {
    /*
     * Fields used and maintained exclusively by generic code.
     */

    int refCount;		/* Number of users of the TkFont. */
    Tcl_HashEntry *cacheHashPtr;/* Entry in font cache for this structure,
				 * used when deleting it. */
    Tcl_HashEntry *namedHashPtr;/* Pointer to hash table entry that
				 * corresponds to the named font that the
				 * tkfont was based on, or NULL if the tkfont
				 * was not based on a named font. */
    int tabWidth;		/* Width of tabs in this font (pixels). */
    int	underlinePos;		/* Offset from baseline to origin of
				 * underline bar (used for drawing underlines
				 * on a non-underlined font). */
    int underlineHeight;	/* Height of underline bar (used for drawing
				 * underlines on a non-underlined font). */

    /*
     * Fields in the generic font structure that are filled in by
     * platform-specific code.
     */

    Font fid;			/* For backwards compatibility with XGCValues
				 * structures.  Remove when TkGCValues is
				 * implemented.  */
    TkFontAttributes fa;	/* Actual font attributes obtained when the
				 * the font was created, as opposed to the
				 * desired attributes passed in to
				 * TkpGetFontFromAttributes().  The desired
				 * metrics can be determined from the string
				 * that was used to create this font. */
    TkFontMetrics fm;		/* Font metrics determined when font was
				 * created. */
} TkFont;


#ifdef WIN32
/*
 * The TkWinDrawable is the internal implementation of an X Drawable (either
 * a Window or a Pixmap).  The following constants define the valid Drawable
 * types.
 */

#define TWD_BITMAP	1
#define TWD_WINDOW	2
#define TWD_WINDC	3

typedef struct {
    int type;
    HWND handle;
    TkWindow *winPtr;
} TkWinWindow;

typedef struct {
    int type;
    HBITMAP handle;
    Colormap colormap;
    int depth;
} TkWinBitmap;

typedef struct {
    int type;
    HDC hdc;
}TkWinDC;

typedef union {
    int type;
    TkWinWindow window;
    TkWinBitmap bitmap;
    TkWinDC winDC;
} TkWinDrawable;

/*
 * The TkWinDCState is used to save the state of a device context
 * so that it can be restored later.
 */

typedef struct TkWinDCState {
    HPALETTE palette;
} TkWinDCState;

extern HDC TkWinGetDrawableDC (Display *display, Drawable drawable, 
	TkWinDCState *state);
extern HDC TkWinReleaseDrawableDC (Drawable drawable, HDC dc, TkWinDCState *state);

#endif /*WIN32*/

#endif /* BLT_TKINT_H */
