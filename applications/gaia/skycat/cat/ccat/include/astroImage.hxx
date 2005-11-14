#ifndef _astroImage_h_
#define _astroImage_h_

/*
 * E.S.O. - VLT project 
 * $Id: astroImage.h,v 1.1.1.1 2002/04/04 20:11:46 brighton Exp $
 *
 * astroImage.h - C interface to C++ class AstroImage
 *
 * (Note: C applications must have at least a dummy C++ main and link
 * with C++)
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
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
