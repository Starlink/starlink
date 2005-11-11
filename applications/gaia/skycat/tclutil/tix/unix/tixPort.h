/*
 * tixPort.h --
 *
 *	This header file handles porting issues that occur because of
 *	differences between systems.  It reads in platform specific
 *	portability files.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _TIX_PORT_H_
#define _TIX_PORT_H_

#ifndef _TCL
#include "config.h"
#include "tcl.h"
#endif

#ifndef _TK
#include "tk.h"
#endif

#if (!defined(__WIN32__)) && (!defined(_WIN32)) && (!defined(MAC_TCL))
    /*
     * The Tcl/Tk porting stuff is needed only in Unix.
     */
#if !defined(_TCLPORT) && !defined(_TKPORT)
#  ifdef _TKINT
#    include "tkPort.h"
#  else
#    include "tclPort.h"
#  endif
#endif
#endif

#ifndef _TIX_H_
#include <tix.h>
#endif

#if defined(__WIN32__) || defined(_WIN32)
#   include "tixWinPort.h"
#else
#   if defined(MAC_TCL)
#	include "tixMacPort.h"
#   else
#	include "../unix/tixUnixPort.h"
#   endif
#endif

#ifdef TK_4_1_OR_LATER

EXTERN Tcl_HashTable *	TixGetHashTable _ANSI_ARGS_((Tcl_Interp * interp,
			    char * name, Tcl_InterpDeleteProc *deleteProc));
#define _TixGetHashTable(i,n,p) TixGetHashTable(i,n,p)

#else

EXTERN Tcl_HashTable *	TixGetHashTable _ANSI_ARGS_((Tcl_Interp * interp,
			    char * name));
#define _TixGetHashTable(i,n,p) TixGetHashTable(i,n)

#endif

#if (TK_MAJOR_VERSION > 4)

/*
 * The font handling is changed in Tk 8.0 and later
 */

typedef Tk_Font TixFont;
#define TixFontId(font) Tk_FontId(font)

EXTERN void		TixComputeTextGeometry _ANSI_ARGS_((
			    TixFont fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr));
EXTERN void		TixDisplayText _ANSI_ARGS_((Display *display,
			    Drawable drawable, TixFont font,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc));

#define TixFreeFont Tk_FreeFont
#define TixNameOfFont Tk_NameOfFont
#define TixGetFont Tk_GetFont

#else

typedef XFontStruct* TixFont;
#define TixFontId(font) ((font)->fid)
#define TixComputeTextGeometry TkComputeTextGeometry
#define TixDisplayText TkDisplayText
#define TixFreeFont Tk_FreeFontStruct
#define TixNameOfFont Tk_NameOfFontStruct
#define TixGetFont Tk_GetFontStruct

EXTERN void		TkDisplayText _ANSI_ARGS_((Display *display,
			    Drawable drawable, XFontStruct *fontStructPtr,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc));
EXTERN void		TkComputeTextGeometry _ANSI_ARGS_((
			    XFontStruct *fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr));


#endif

#endif /* _TIX_PORT_H_ */
