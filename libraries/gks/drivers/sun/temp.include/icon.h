/*	@(#)icon.h 10.5 88/02/08 SMI	*/

/*****************************************************************************/
/*                                 icon.h                                    */
/*                   Copyright (c) 1985 by Sun Microsystems, Inc.            */
/*****************************************************************************/

#ifndef icon_DEFINED
#define icon_DEFINED

#include <sunwindow/attr.h>

/*****************************************************************************/
/*                            Attributes                                     */
/*****************************************************************************/

#define ICON_ATTR(type, ordinal)       ATTR(ATTR_PKG_ICON, type, ordinal)
#define ICON_ATTR_LIST(ltype, type, ordinal) \
        ICON_ATTR(ATTR_LIST_INLINE((ltype), (type)), (ordinal))

typedef enum {
   ICON_X		= ICON_ATTR(ATTR_X, 1),
   ICON_Y		= ICON_ATTR(ATTR_Y, 2),
   ICON_WIDTH		= ICON_ATTR(ATTR_X, 3),
   ICON_HEIGHT		= ICON_ATTR(ATTR_Y, 4),
   ICON_IMAGE_RECT	= ICON_ATTR(ATTR_RECT_PTR, 5),
   ICON_LABEL_RECT	= ICON_ATTR(ATTR_RECT_PTR, 6),
   ICON_IMAGE		= ICON_ATTR(ATTR_PIXRECT_PTR, 7),
   ICON_LABEL		= ICON_ATTR(ATTR_STRING, 8),
   ICON_FONT		= ICON_ATTR(ATTR_PIXFONT_PTR, 9)
} Icon_attribute;

#define icon_attr_next(attr) (Icon_attribute *)attr_next((caddr_t *)attr)

/*****************************************************************************/
/* icon struct                                                               */
/*****************************************************************************/

struct icon {
	short	 	ic_width, ic_height;	/* overall icon dimensions */
	struct pixrect *ic_background;	/* background pattern (mem pixrect) */
	Rect		ic_gfxrect;	/* where the graphic goes */
	struct pixrect *ic_mpr;		/* the graphic (a memory pixrect) */
	Rect		ic_textrect;	/* where text goes */
	char	       *ic_text;	/* the text */
	struct pixfont *ic_font;	/* Font with which to display text */
	int		ic_flags;
};

/* flag values */
#define	ICON_BKGRDPAT	0x02		/* use ic_background to prepare image*/
#define	ICON_BKGRDGRY	0x04		/* use std gray to prepare image*/
#define	ICON_BKGRDCLR	0x08		/* clear to prepare image*/
#define	ICON_BKGRDSET	0x10		/* set to prepare image*/
#define	ICON_FIRSTPRIV	0x0100		/* start of private flags range */
#define	ICON_LASTPRIV	0x8000		/* end of private flags range */

#define	ICON_DEFAULT_WIDTH	64
#define	ICON_DEFAULT_HEIGHT	64

/*****************************************************************************/
/* typedefs                                                                  */
/*****************************************************************************/

typedef caddr_t      Icon;
typedef struct icon *icon_handle;

/*****************************************************************************/
/* external functions for attribute/value interface                          */
/*****************************************************************************/

extern Icon	icon_create();
extern caddr_t	icon_get();
extern int	icon_set();
extern int	icon_destroy();
extern void	icon_display(); /* how are icons undisplayed? */

/*****************************************************************************/
/* DEFINE_ICON_FROM_IMAGE macro                                              */
/*****************************************************************************/
#ifdef i386
#define	DEFINE_ICON_FROM_IMAGE(name, image)				\
	static struct mpr_data CAT(name,_data) = {			\
	    mpr_linebytes(ICON_DEFAULT_WIDTH,1), (short *)(image),	\
	    {0, 0}, 0, MP_STATIC};						\
	extern struct pixrectops mem_ops;				\
	static struct pixrect CAT(name,_mpr) = {			\
	    &mem_ops, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT, 1,	\
	    (caddr_t)&CAT(name,_data)};					\
	static struct icon name = {					\
	    ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,			\
	    (struct pixrect *)0,					\
	    0, 0, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,		\
	    &CAT(name,_mpr),						\
	    0, 0, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,		\
	    NULL, (struct pixfont *)0, ICON_BKGRDGRY};

#else

#define	DEFINE_ICON_FROM_IMAGE(name, image)				\
	static struct mpr_data CAT(name,_data) = {			\
	    mpr_linebytes(ICON_DEFAULT_WIDTH,1), (short *)(image),	\
	    {0, 0}, 0, 0};						\
	extern struct pixrectops mem_ops;				\
	static struct pixrect CAT(name,_mpr) = {			\
	    &mem_ops, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT, 1,	\
	    (caddr_t)&CAT(name,_data)};					\
	static struct icon name = {					\
	    ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,			\
	    (struct pixrect *)0,					\
	    0, 0, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,		\
	    &CAT(name,_mpr),						\
	    0, 0, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT,		\
	    NULL, (struct pixfont *)0, ICON_BKGRDGRY};
#endif
#endif ~icon_DEFINED
