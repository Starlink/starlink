/*  This file, putcolu.c, contains routines that write data elements to    */
/*  a FITS image or table.  Writes null values.                            */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.  Users shall not, without prior written   */
/*  permission of the U.S. Government,  establish a claim to statutory     */
/*  copyright.  The Government and others acting on its behalf, shall have */
/*  a royalty-free, non-exclusive, irrevocable,  worldwide license for     */
/*  Government purposes to publish, distribute, translate, copy, exhibit,  */
/*  and perform such material.                                             */

#include <stdlib.h>
#include <string.h>
#include "fitsio2.h"
/*--------------------------------------------------------------------------*/
int ffppru( fitsfile *fptr,  /* I - FITS file pointer                       */
            long  group,      /* I - group to write(1 = 1st group)          */
            long  firstelem,  /* I - first vector element to write(1 = 1st) */
            long  nelem,      /* I - number of values to write              */
            int  *status)     /* IO - error status                          */
/*
  Write null values to the primary array.
*/
{
    long row;

    /*
      the primary array is represented as a binary table:
      each group of the primary array is a row in the table,
      where the first column contains the group parameters
      and the second column contains the image itself.
    */

    row=maxvalue(1,group);

    ffpclu(fptr, 2, row, firstelem, nelem, status);
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffpprn( fitsfile *fptr,  /* I - FITS file pointer                       */
            long  firstelem,  /* I - first vector element to write(1 = 1st) */
            long  nelem,      /* I - number of values to write              */
            int  *status)     /* IO - error status                          */
/*
  Write null values to the primary array. (Doesn't support groups).
*/
{
    long row = 1;

    /*
      the primary array is represented as a binary table:
      each group of the primary array is a row in the table,
      where the first column contains the group parameters
      and the second column contains the image itself.
    */

    ffpclu(fptr, 2, row, firstelem, nelem, status);
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffpclu( fitsfile *fptr,  /* I - FITS file pointer                       */
            int  colnum,     /* I - number of column to write (1 = 1st col) */
            long  firstrow,  /* I - first row to write (1 = 1st row)        */
            long  firstelem, /* I - first vector element to write (1 = 1st) */
            long  nelem,     /* I - number of values to write               */
            int  *status)    /* IO - error status                           */
/*
  Set elements of a table column to the appropriate null value for the column
  The column number may refer to a real column in an ASCII or binary table, 
  or it may refer to a virtual column in a 1 or more grouped FITS primary
  array.  FITSIO treats a primary array as a binary table
  with 2 vector columns: the first column contains the group parameters (often
  with length = 0) and the second column contains the array of image pixels.
  Each row of the table represents a group in the case of multigroup FITS
  images.
*/
{
    int tcode, maxelem, hdutype, lennull, nwrite = 0;
    short i2null;
    INT32BIT i4null;
    long twidth, incre, repeat, rowlen, rownum, elemnum, remain, next, ntodo;
    long tnull, startpos, wrtptr, ii;
    double scale, zero;
    unsigned char i1null, lognul = 0;
    char tform[20], cstring[50];
    char message[FLEN_ERRMSG];
    char snull[20];   /*  the FITS null value  */
    long   jbuff[2] = { -1, -1};  /* all bits set is equivalent to a NaN */

    if (*status > 0)           /* inherit input status value if > 0 */
        return(*status);

    /*---------------------------------------------------*/
    /*  Check input and get parameters about the column: */
    /*---------------------------------------------------*/

    /* note that the 5th parameter = 1, not nelem, so that the returned */
    /* repeat and incre values will be the actual values for this column */

    /* note that 6th parameter = 0 because if writing nulls to a variable   */
    /* length column then dummy data values must have already been written  */
    /* to the heap.  We now just have to overwrite the previous values with */
    /* null values.                                                         */

    if (ffgcpr( fptr, colnum, firstrow, firstelem, 1, 0, &scale, &zero,
        tform, &twidth, &tcode, &maxelem, &startpos,  &elemnum, &incre,
        &repeat, &rowlen, &hdutype, &tnull, snull, status) > 0)
        return(*status);

    if (tcode == TSTRING)
    {
      if (snull[0] == ASCII_NULL_UNDEFINED)
      {
        ffpmsg(
        "Null value string for ASCII table column is not defined (FTPCLU).");
        return(*status = NO_NULL);
      }

      strcpy(cstring, snull);          /* copy null string to temp buffer */
      lennull = strlen(cstring);

      if (hdutype == BINARY_TBL)
      {  /* write up to and including null terminator into BINTABLE column */
         nwrite = minvalue(twidth, lennull + 1);
      }
      else
      {  /* write up to 20 chars to ASCII table column; pad with blanks */
         nwrite = minvalue(twidth, 20);

         for (ii = lennull; ii < nwrite; ii++)
            cstring[ii] = ' ';  
      }
    }
    else if ( tcode == TBYTE ||
              tcode == TSHORT ||
              tcode == TLONG ) 
    {
      if (tnull == NULL_UNDEFINED)
      {
        ffpmsg(
        "Null value for integer table column is not defined (FTPCLU).");
        return(*status = NO_NULL);
      }

      if (tcode == TBYTE)
         i1null = tnull;
      else if (tcode == TSHORT)
      {
         i2null = tnull;
#if BYTESWAPPED
         ffswap2(&i2null, 1); /* reverse order of bytes */
#endif
      }
      else
      {
         i4null = tnull;
#if BYTESWAPPED
         ffswap4(&i4null, 1); /* reverse order of bytes */
#endif
      }
    }

    /*---------------------------------------------------------------------*/
    /*  Now write the pixels to the FITS column.                           */
    /*---------------------------------------------------------------------*/
    remain = nelem;           /* remaining number of values to write  */
    next = 0;                 /* next element in array to be written  */
    rownum = 0;               /* row number, relative to firstrow     */
    ntodo = remain;           /* number of elements to write at one time */

    while (ntodo)
    {
        /* limit the number of pixels to process a one time to the number that
           will fit in the buffer space or to the number of pixels that remain
           in the current vector, which ever is smaller.
        */
        ntodo = minvalue(ntodo, (repeat - elemnum));
        wrtptr = startpos + (rownum * rowlen) + (elemnum * incre);

        ffmbyt(fptr, wrtptr, IGNORE_EOF, status); /* move to write position */

        switch (tcode) 
        {
            case (TBYTE):
 
                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 1,  &i1null, status);
                break;

            case (TSHORT):

                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 2, &i2null, status);
                break;

            case (TLONG):

                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 4, &i4null, status);
                break;

            case (TFLOAT):

                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 4, jbuff, status);
                break;

            case (TDOUBLE):

                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 8, jbuff, status);
                break;

            case (TLOGICAL):
 
                for (ii = 0; ii < ntodo; ii++)
                  ffpbyt(fptr, 1, &lognul, status);
                break;

            case (TSTRING):  /* an ASCII table column */
                /* repeat always = 1, so ntodo is also guaranteed to = 1 */
                ffpbyt(fptr, nwrite, cstring, status);
                break;

            default:  /*  error trap  */
                sprintf(message, 
                   "Cannot write null value to column %d which has format %s",
                     colnum,tform);
                ffpmsg(message);
                return(*status);

        } /* End of switch block */

        /*-------------------------*/
        /*  Check for fatal error  */
        /*-------------------------*/
        if (*status > 0)  /* test for error during previous write operation */
        {
           sprintf(message,
             "Error writing %ld thru %ld of null values (ffpclu).",
              next+1, next+ntodo);
           ffpmsg(message);
           return(*status);
        }

        /*--------------------------------------------*/
        /*  increment the counters for the next loop  */
        /*--------------------------------------------*/
        remain -= ntodo;
        if (remain)
        {
            next += ntodo;
            elemnum += ntodo;
            if (elemnum == repeat)  /* completed a row; start on next row */
            {
                elemnum = 0;
                rownum++;
            }
        }
        ntodo = remain;  /* this is the maximum number to do in next loop */

    }  /*  End of main while Loop  */

    return(*status);
}

