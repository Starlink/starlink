/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: astro_image.C,v 1.1.1.1 2006/01/12 16:36:40 abrighto Exp $
 *
 * astroImage.C - C interface implementation for C++ class AstroImage
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Oct 95  Created
 *                 02/01/06  Renamed astroImage.C to astro_image.C to avoid
 *                           name conflict on file systems that ignore case
 */
static const char* const rcsId="@(#) $Id: astro_image.C,v 1.1.1.1 2006/01/12 16:36:40 abrighto Exp $";



// include the C++ and C interfaces
#include <errno.h>
#include "error.h"
#include "AstroImage.h"
extern "C" {
#include "astro_image.h"
#include "astro_catalog.h"
}


/* 
 * check that the given handle is not null and return its
 * error status
 */
static int aiCheckHandle(AiHandle handle)
{
    if (handle)
	return ((AstroImage*)handle)->status();
    return error("internal error: ", "bad image server handle", EINVAL);
}


/*
 * open the named image server and return a handle for it or NULL if
 * there were errors
 */
AiHandle aiOpen(char* name)
{
    return (AiHandle)AstroImage::open(name);
}


/* close the image server connection and free its resources */
void aiClose(AiHandle handle)
{
    if (handle)
	delete (AstroImage*)handle;
}


/*
 * pass a request to the image server and return the name of a FITS file
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
 *
 * XXX note: this routine should probably return the status instead
 * of the filename (the C++ method returns the status).
 */
char* aiGetImage(AiHandle handle, double ra, double dec, 
		  double width, double height)
{
    if (aiCheckHandle(handle) != OK)
	return NULL;

    if (((AstroImage*)handle)->getImage(WorldCoords(ra, dec), width, height) != 0)
	return NULL;
    return (char*)(((AstroImage*)handle)->tmpfile());
}

