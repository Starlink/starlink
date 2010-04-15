/* ************************************************************************ */


/* Header file for the `xvertext 5.0' routines.

   Copyright (c) 1993 Alan Richardson (mppa3@uk.ac.sussex.syma) */


/* ************************************************************************ */

#ifndef _XVERTEXT_INCLUDED_
#define _XVERTEXT_INCLUDED_


#define XV_VERSION      5.0
#define XV_COPYRIGHT \
      "xvertext routines Copyright (c) 1993 Alan Richardson"


/* ---------------------------------------------------------------------- */


/* text alignment */

#define NONE             0
#define TLEFT            1
#define TCENTRE          2
#define TRIGHT           3
#define MLEFT            4
#define MCENTRE          5
#define MRIGHT           6
#define BLEFT            7
#define BCENTRE          8
#define BRIGHT           9


/* ---------------------------------------------------------------------- */

/* this shoulf be C++ compliant, thanks to
     vlp@latina.inesc.pt (Vasco Lopes Paulo) */

#ifdef EXTERN
#undef EXTERN
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

EXTERN double   XRotVersion(char*, int);
EXTERN void    XRotSetMagnification(double);
EXTERN void    XRotSetBoundingBoxPad(int);
EXTERN int     XRotDrawString(Display*, XFontStruct*, double,
                              Drawable, GC, int, int, char*);
EXTERN int     XRotDrawImageString(Display*, XFontStruct*, double,
                                   Drawable, GC, int, int, char*);
EXTERN int     XRotDrawAlignedString(Display*, XFontStruct*, double,
                                     Drawable, GC, int, int, char*, int);
EXTERN int     XRotDrawAlignedImageString(Display*, XFontStruct*, double,
                                          Drawable, GC, int, int, char*, int);
EXTERN XPoint *XRotTextExtents(Display*, XFontStruct*, double, int,
                               int, char*, int);

/* ---------------------------------------------------------------------- */


#endif /* _XVERTEXT_INCLUDED_ */



