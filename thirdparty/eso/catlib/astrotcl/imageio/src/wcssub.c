#include <stdlib.h>
#include "fitsio2.h"
void * wcsinit(char *header);  /* prototype for libwcs routine */

/*--------------------------------------------------------------------------*/
/* int fits_init_wcs_img */

int ffiwcs(fitsfile *fptr,  /* I - FITS file pointer                    */
           void **wcs, /* O - pointer to the WCS structure  */
                            /*     created and returned by wcsinit.     */
           int *status)     /* IO - error status                        */
/*
  initialize the structure full of WCS parameters, by reading the 
  image header keywords.
*/
{
    char *header;

    if (*status > 0)
        return(*status);

    /* read header keywords into a long string of chars */
    if (ffh2st(fptr, &header, status) > 0)
    {
        ffpmsg("error reading entire header into a long string (ffiwcs)");
        return(*status);
    }

    /* extract WCS parameters from header into the structure */
    *wcs = wcsinit(header);
    free(header);        /* free the memory allocated by fits_header2string */

    if (!wcs)
    {
        ffpmsg("error constructing the WCS structure (ffiwcs)");
        *status = WCS_ERROR;
    }

    return(*status);
}
