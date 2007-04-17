#ifndef _astroImage_h_
#define _astroImage_h_

/*
 * E.S.O. - VLT project 
 * $Id: astro_image.h,v 1.1.1.1 2006/01/12 16:36:40 abrighto Exp $
 *
 * astro_image.h - C interface to C++ class AstroImage
 *
 * (Note: C applications must have at least a dummy C++ main and link
 * with C++)
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *                 02/01/06  Renamed astroImage.h to astro_image.h to avoid
 *                           name conflict on file systems that ignore case
 */


/* handle for an open image server */
typedef void* AiHandle;


/*
 * open the named image server and return a handle for it or NULL if
 * there were errors
 */
AiHandle aiOpen(char* name);


/* close the image server connection and free its resources */
void aiClose(AiHandle);

/*
 * pass a request to the catalog and return the name of a FITS file
 * containing the resulting image, or NULL if not found
 *
 * Args:
 *
 * handle - handle returned from ai_open()
 *
 * ra, dec - world coordinates position
 *
 * width, height - dimensions of image to return.
 *
 * The return filename is the name of a temporary file that will
 * be reused on the next call to this routine.
 */
char* aiGetImage(AiHandle handle, double ra, double dec, 
		  double width, double height);

#endif /* _astroImage_h_ */
