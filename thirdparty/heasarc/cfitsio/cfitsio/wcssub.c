#include <stdlib.h>
#include <string.h>
#include "fitsio2.h"

/*--------------------------------------------------------------------------*/
int ffgiwcs(fitsfile *fptr,  /* I - FITS file pointer                    */
           char **header,   /* O - pointer to the WCS related keywords  */
           int *status)     /* IO - error status                        */
/*
  int fits_get_image_wcs_keys 
  return a string containing all the image WCS header keywords.
  This string is then used as input to the wcsinit WCSlib routine.
*/
{
    int hdutype;

    if (*status > 0)
        return(*status);

    fits_get_hdu_type(fptr, &hdutype, status);
    if (hdutype != IMAGE_HDU)
    {
      ffpmsg(
     "Error in ffgiwcs. This HDU is not an image. Can't read WCS keywords");
      return(*status = NOT_IMAGE);
    }

    /* read header keywords into a long string of chars */
    if (ffh2st(fptr, header, status) > 0)
    {
        ffpmsg("error creating string of image WCS keywords (ffgiwcs)");
        return(*status);
    }

    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffgtwcs(fitsfile *fptr,  /* I - FITS file pointer              */
           int xcol,        /* I - column number for the X column  */
           int ycol,        /* I - column number for the Y column  */
           char **header,   /* O - string of all the WCS keywords  */
           int *status)     /* IO - error status                   */
/*
  int fits_get_table_wcs_keys
  Return string containing all the WCS keywords appropriate for the 
  pair of X and Y columns containing the coordinate
  of each event in an event list table.  This string may then be passed
  to Doug Mink's WCS library wcsinit routine, to create and initialize the
  WCS structure.  The calling routine must free the header character string
  when it is no longer needed. 
*/
{
    int hdutype, ncols, tstatus, length;
    int naxis1 = 1, naxis2 = 1;
    long tlmin, tlmax;
    char keyname[FLEN_KEYWORD];
    char valstring[FLEN_VALUE];
    char comm[2];
    char *cptr;
    /*  construct a string of 80 blanks, for adding fill to the keywords */
                 /*  12345678901234567890123456789012345678901234567890123456789012345678901234567890 */
    char blanks[] = "                                                                                ";

    if (*status > 0)
        return(*status);

    fits_get_hdu_type(fptr, &hdutype, status);
    if (hdutype == IMAGE_HDU)
    {
        ffpmsg("Can't read table WSC keywords. This HDU is not a table");
        return(*status = NOT_TABLE);
    }

    fits_get_num_cols(fptr, &ncols, status);
    
    if (xcol < 1 || xcol > ncols)
    {
        ffpmsg("illegal X axis column number in fftwcs");
        return(*status = BAD_COL_NUM);
    }

    if (ycol < 1 || ycol > ncols)
    {
        ffpmsg("illegal Y axis column number in fftwcs");
        return(*status = BAD_COL_NUM);
    }

    /* allocate character string for all the WCS keywords */
    *header = calloc(1, 2401);  /* room for up to 30 keywords */
    if (*header == 0)
    {
        ffpmsg("error allocating memory for WCS header keywords (fftwcs)");
        return(*status = MEMORY_ALLOCATION);
    }

    cptr = *header;
    comm[0] = '\0';
    
    tstatus = 0;
    ffkeyn("TLMIN",xcol,keyname,status);
    ffgkyj(fptr,keyname, &tlmin,NULL,&tstatus);

    if (!tstatus)
    {
        ffkeyn("TLMAX",xcol,keyname,status);
        ffgkyj(fptr,keyname, &tlmax,NULL,&tstatus);
    }

    if (!tstatus)
    {
        naxis1 = tlmax - tlmin + 1;
    }

    tstatus = 0;
    ffkeyn("TLMIN",ycol,keyname,status);
    ffgkyj(fptr,keyname, &tlmin,NULL,&tstatus);

    if (!tstatus)
    {
        ffkeyn("TLMAX",ycol,keyname,status);
        ffgkyj(fptr,keyname, &tlmax,NULL,&tstatus);
    }

    if (!tstatus)
    {
        naxis2 = tlmax - tlmin + 1;
    }

    /*            123456789012345678901234567890    */
    strcat(cptr, "NAXIS   =                    2");
    strncat(cptr, blanks, 50);
    cptr += 80;

    ffi2c(naxis1, valstring, status);   /* convert to formatted string */
    ffmkky("NAXIS1", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    strcpy(keyname, "NAXIS2");
    ffi2c(naxis2, valstring, status);   /* convert to formatted string */
    ffmkky(keyname, valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /* read the required header keywords (use defaults if not found) */

    /*  CTYPE1 keyword */
    tstatus = 0;
    ffkeyn("TCTYP",xcol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       valstring[0] =  '\0';
    ffmkky("CTYPE1", valstring, comm, cptr, status);  /* construct the keyword*/
    length = strlen(cptr);
    strncat(cptr, blanks, 80 - length);  /* pad with blanks */
    cptr += 80;

    /*  CTYPE2 keyword */
    tstatus = 0;
    ffkeyn("TCTYP",ycol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       valstring[0] =  '\0';
    ffmkky("CTYPE2", valstring, comm, cptr, status);  /* construct the keyword*/
    length = strlen(cptr);
    strncat(cptr, blanks, 80 - length);  /* pad with blanks */
    cptr += 80;

    /*  CRPIX1 keyword */
    tstatus = 0;
    ffkeyn("TCRPX",xcol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CRPIX1", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /*  CRPIX2 keyword */
    tstatus = 0;
    ffkeyn("TCRPX",ycol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CRPIX2", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /*  CRVAL1 keyword */
    tstatus = 0;
    ffkeyn("TCRVL",xcol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CRVAL1", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /*  CRVAL2 keyword */
    tstatus = 0;
    ffkeyn("TCRVL",ycol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CRVAL2", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /*  CDELT1 keyword */
    tstatus = 0;
    ffkeyn("TCDLT",xcol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CDELT1", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /*  CDELT2 keyword */
    tstatus = 0;
    ffkeyn("TCDLT",ycol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) )
       strcpy(valstring, "1");
    ffmkky("CDELT2", valstring, comm, cptr, status);  /* construct the keyword*/
    strncat(cptr, blanks, 50);  /* pad with blanks */
    cptr += 80;

    /* the following keywords may not exist */

    /*  CROTA2 keyword */
    tstatus = 0;
    ffkeyn("TCROT",ycol,keyname,status);
    if (ffgkey(fptr, keyname, valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("CROTA2", valstring, comm, cptr, status);  /* construct keyword*/
        strncat(cptr, blanks, 50);  /* pad with blanks */
        cptr += 80;
    }

    /*  EPOCH keyword */
    tstatus = 0;
    if (ffgkey(fptr, "EPOCH", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("EPOCH", valstring, comm, cptr, status);  /* construct keyword*/
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  EQUINOX keyword */
    tstatus = 0;
    if (ffgkey(fptr, "EQUINOX", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("EQUINOX", valstring, comm, cptr, status); /* construct keyword*/
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  RADECSYS keyword */
    tstatus = 0;
    if (ffgkey(fptr, "RADECSYS", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("RADECSYS", valstring, comm, cptr, status); /*construct keyword*/
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  TELESCOPE keyword */
    tstatus = 0;
    if (ffgkey(fptr, "TELESCOP", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("TELESCOP", valstring, comm, cptr, status); 
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  INSTRUME keyword */
    tstatus = 0;
    if (ffgkey(fptr, "INSTRUME", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("INSTRUME", valstring, comm, cptr, status);  
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  DETECTOR keyword */
    tstatus = 0;
    if (ffgkey(fptr, "DETECTOR", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("DETECTOR", valstring, comm, cptr, status);  
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  MJD-OBS keyword */
    tstatus = 0;
    if (ffgkey(fptr, "MJD-OBS", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("MJD-OBS", valstring, comm, cptr, status);  
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  DATE-OBS keyword */
    tstatus = 0;
    if (ffgkey(fptr, "DATE-OBS", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("DATE-OBS", valstring, comm, cptr, status);  
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    /*  DATE keyword */
    tstatus = 0;
    if (ffgkey(fptr, "DATE", valstring, NULL, &tstatus) == 0 )
    {
        ffmkky("DATE", valstring, comm, cptr, status);  
        length = strlen(cptr);
        strncat(cptr, blanks, 80 - length);  /* pad with blanks */
        cptr += 80;
    }

    strcat(cptr, "END");
    strncat(cptr, blanks, 77);

    return(*status);
}
