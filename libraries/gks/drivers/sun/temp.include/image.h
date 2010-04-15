/*	@(#)image.h 10.7 88/02/08 SMI	*/

/***********************************************************************/
/*	                      image.h  				       */
/*          Copyright (c) 1985 by Sun Microsystems, Inc.               */
/***********************************************************************/

#ifndef image_DEFINED
#define image_DEFINED

#include <sunwindow/attr.h>

/***********************************************************************/
/*	        	Attributes 				       */
/***********************************************************************/

#define	IMAGE_ATTR(type, ordinal)	ATTR(ATTR_PKG_IMAGE, type, ordinal)

#define	IMAGE_ATTRIBUTE_LIST		ATTR_LIST

/* Fake types -- This should be resolved someday */
#define ATTR_IMAGE_PTR			ATTR_OPAQUE
#define ATTR_FONT_PTR			ATTR_OPAQUE
#define ATTR_PIXRECT_INT_INT		ATTR_INT_TRIPLE

/* Reserved for future use */
#define	IMAGE_ATTR_UNUSED_FIRST		 0
#define	IMAGE_ATTR_UNUSED_LAST		31


typedef enum {

    IMAGE_ACTIVE		= IMAGE_ATTR(ATTR_NO_VALUE, 32),
    IMAGE_BOXED			= IMAGE_ATTR(ATTR_BOOLEAN, 33),
    IMAGE_CENTER		= IMAGE_ATTR(ATTR_BOOLEAN, 133),
    IMAGE_FONT			= IMAGE_ATTR(ATTR_FONT_PTR, 34),
    IMAGE_HEIGHT		= IMAGE_ATTR(ATTR_INT, 35),
    IMAGE_INACTIVE		= IMAGE_ATTR(ATTR_NO_VALUE, 36),
    IMAGE_INVERT		= IMAGE_ATTR(ATTR_BOOLEAN, 37),
    IMAGE_LEFT_PIXRECT		= IMAGE_ATTR(ATTR_PIXRECT_PTR, 38),
    IMAGE_LEFT_MARGIN		= IMAGE_ATTR(ATTR_INT, 39),
    IMAGE_MARGIN		= IMAGE_ATTR(ATTR_INT, 40),
    IMAGE_PIXRECT		= IMAGE_ATTR(ATTR_PIXRECT_PTR, 41),
    IMAGE_RELEASE		= IMAGE_ATTR(ATTR_NO_VALUE, 42),
    IMAGE_RELEASE_STRING	= IMAGE_ATTR(ATTR_NO_VALUE, 43),
    IMAGE_RELEASE_PR		= IMAGE_ATTR(ATTR_NO_VALUE, 44),
    IMAGE_RIGHT_PIXRECT		= IMAGE_ATTR(ATTR_PIXRECT_PTR, 45),
    IMAGE_RIGHT_MARGIN		= IMAGE_ATTR(ATTR_INT, 46),
    IMAGE_STRING		= IMAGE_ATTR(ATTR_STRING, 47),
    IMAGE_WIDTH			= IMAGE_ATTR(ATTR_INT, 48),


/* Only used for named parameters */

    IMAGE_ERROR_PROC		= IMAGE_ATTR(ATTR_FUNCTION_PTR, 49), /* Call */
    IMAGE_REGION		= IMAGE_ATTR(ATTR_PIXRECT_INT_INT, 50),

    IMAGE_NOP			= IMAGE_ATTR(ATTR_NO_VALUE, 51)

} Image_attribute;


/***********************************************************************/
/* opaque types for images	                                       */
/***********************************************************************/

typedef	caddr_t 	Image;

#define	IMAGE_NULL	((Image)0)

/***********************************************************************/
/* external declarations                                               */
/***********************************************************************/

/*
   extern Image		 image_create(av_list);
   extern caddr_t	 image_get(image|image_item, attr{, data});
   extern int		 image_set(image|image_item, attr{, data}, value);
   extern void		 image_destroy(image|image_item);
   extern void		 image_paint();
 */

   extern Image		 image_create();
   extern caddr_t	 image_get();
   extern int		 image_set();
   extern void		 image_destroy();
   extern void		 image_render();

/***********************************************************************/
/* various utility macros                                              */
/***********************************************************************/

/***********************************************************************/
#endif not image_DEFINED
