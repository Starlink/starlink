/*  This file, getcols.c, contains routines that read data elements from   */
/*  a FITS image or table, with a character string datatype.               */

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
/* stddef.h is apparently needed to define size_t */
#include <stddef.h>
#include "fitsio2.h"
/*--------------------------------------------------------------------------*/
int ffgcvs( fitsfile *fptr,   /* I - FITS file pointer                       */
            int  colnum,      /* I - number of column to read (1 = 1st col)  */
            long  firstrow,   /* I - first row to read (1 = 1st row)         */
            long  firstelem,  /* I - first vector element to read (1 = 1st)  */
            long  nelem,      /* I - number of strings to read               */
            char *nulval,     /* I - string for null pixels                  */
            char **array,     /* O - array of values that are read           */
            int  *anynul,     /* O - set to 1 if any values are null; else 0 */
            int  *status)     /* IO - error status                           */
/*
  Read an array of string values from a column in the current FITS HDU.
  Any undefined pixels will be set equal to the value of 'nulval' unless
  nulval = null in which case no checks for undefined pixels will be made.
*/
{
    char cdummy[2];

    ffgcls(fptr, colnum, firstrow, firstelem, nelem, 1, nulval,
           array, cdummy, anynul, status);
    return(*status);
}
/*--------------------------------------------------------------------------*/
int ffgcfs( fitsfile *fptr,   /* I - FITS file pointer                       */
            int  colnum,      /* I - number of column to write (1 = 1st col) */
            long  firstrow,   /* I - first row to write (1 = 1st row)        */
            long  firstelem,  /* I - first vector element to write (1 = 1st) */
            long  nelem,      /* I - number of strings to write              */
            char **array,     /* O - array of values that are read           */
            char *nularray,   /* O - array of flags = 1 if nultyp = 2        */
            int  *anynul,     /* O - set to 1 if any values are null; else 0 */
            int  *status)     /* IO - error status                           */
/*
  Read an array of string values from a column in the current FITS HDU.
  Nularray will be set = 1 if the corresponding array pixel is undefined, 
  otherwise nularray will = 0.
*/
{
    char dummy[2];

    ffgcls(fptr, colnum, firstrow, firstelem, nelem, 2, dummy,
           array, nularray, anynul, status);
    return(*status);
}

/*--------------------------------------------------------------------------*/
int ffgcls( fitsfile *fptr,   /* I - FITS file pointer                       */
            int  colnum,      /* I - number of column to write (1 = 1st col) */
            long  firstrow,   /* I - first row to write (1 = 1st row)        */
            long  firstelem,  /* I - first vector element to write (1 = 1st) */
            long  nelem,      /* I - number of strings to write              */
            int   nultyp,     /* I - null value handling code:               */
                              /*     1: set undefined pixels = nulval        */
                              /*     2: set nularray=1 for undefined pixels  */
            char  *nulval,    /* I - value for null pixels if nultyp = 1     */
            char **array,     /* O - array of values that are read           */
            char *nularray,   /* O - array of flags = 1 if nultyp = 2        */
            int  *anynul,     /* O - set to 1 if any values are null; else 0 */
            int  *status)     /* IO - error status                           */
/*
  Read an array of string values from a column in the current FITS HDU.
*/
{
    long nullen; 
    int tcode, maxelem, hdutype, nulcheck;
    long twidth, incre, repeat, rowlen, rownum, elemnum;
    long ii, jj, ntodo, tnull, startpos, readptr, remain, next;
    double scale, zero;
    char tform[20];
    char message[FLEN_ERRMSG];
    char snull[20];   /*  the FITS null value  */
    tcolumn *colptr;

    double cbuff[DBUFFSIZE / sizeof(double)]; /* align cbuff on word boundary */
    char *buffer, *arrayptr;

    if (*status > 0 || nelem == 0)  /* inherit input status value if > 0 */
        return(*status);

    if (fptr->HDUposition != (fptr->Fptr)->curhdu)
        ffmahd(fptr, (fptr->HDUposition) + 1, NULL, status);

    if (anynul)
        *anynul = 0;

    if (nultyp == 2)
        memset(nularray, 0, nelem);   /* initialize nullarray */

    /*---------------------------------------------------*/
    /*  Check input and get parameters about the column: */
    /*---------------------------------------------------*/
    if (colnum < 1 || colnum > (fptr->Fptr)->tfield)
    {
        sprintf(message, "Specified column number is out of range: %d",
                colnum);
        ffpmsg(message);
        return(*status = BAD_COL_NUM);
    }

    colptr  = (fptr->Fptr)->tableptr;   /* point to first column */
    colptr += (colnum - 1);     /* offset to correct column structure */
    tcode = colptr->tdatatype;

    if (tcode == -TSTRING) /* variable length column in a binary table? */
    {
      /* only read a single string; ignore value of firstelem */

      if (ffgcpr( fptr, colnum, firstrow, 1, 1, 0, &scale, &zero,
        tform, &twidth, &tcode, &maxelem, &startpos,  &elemnum, &incre,
        &repeat, &rowlen, &hdutype, &tnull, snull, status) > 0)
        return(*status);

      remain = 1;
      twidth = repeat;  
    }
    else if (tcode == TSTRING)
    {
      if (ffgcpr( fptr, colnum, firstrow, firstelem, nelem, 0, &scale, &zero,
        tform, &twidth, &tcode, &maxelem, &startpos,  &elemnum, &incre,
        &repeat, &rowlen, &hdutype, &tnull, snull, status) > 0)
        return(*status);

      remain = nelem;
    }
    else
        return(*status = NOT_ASCII_COL);

    nullen = strlen(snull);   /* length of the undefined pixel string */
    if (nullen == 0)
        nullen = 1;
 
    /*------------------------------------------------------------------*/
    /*  Decide whether to check for null values in the input FITS file: */
    /*------------------------------------------------------------------*/
    nulcheck = nultyp; /* by default check for null values in the FITS file */

    if (nultyp == 1 && nulval[0] == 0)
       nulcheck = 0;    /* calling routine does not want to check for nulls */

    else if (snull[0] == ASCII_NULL_UNDEFINED)
       nulcheck = 0;   /* null value string in ASCII table not defined */

    else if (nullen > twidth)
       nulcheck = 0;   /* null value string is longer than width of column  */
                       /* thus impossible for any column elements to = null */

    /*---------------------------------------------------------------------*/
    /*  Now read the strings one at a time from the FITS column.           */
    /*---------------------------------------------------------------------*/
    next = 0;                 /* next element in array to be read  */
    rownum = 0;               /* row number, relative to firstrow     */

    while (remain)
    {
      /* limit the number of pixels to process a one time to the number that
         will fit in the buffer space or to the number of pixels that remain
         in the current vector, which ever is smaller.
      */
      ntodo = minvalue(remain, maxelem);      
      ntodo = minvalue(ntodo, (repeat - elemnum));

      readptr = startpos + (rownum * rowlen) + (elemnum * incre);
      ffmbyt(fptr, readptr, REPORT_EOF, status);  /* move to read position */

      /* read the array of strings from the FITS file into the buffer */

      if (incre == twidth)
         ffgbyt(fptr, ntodo * twidth, cbuff, status);
      else
         ffgbytoff(fptr, twidth, ntodo, incre - twidth, cbuff, status);

      /* copy from the buffer into the user's array of strings */
      /* work backwards from last char of last string to 1st char of 1st */

      buffer = ((char *) cbuff) + (ntodo * twidth) - 1;

      for (ii = next + ntodo - 1; ii >= next; ii--)
      {
         arrayptr = array[ii] + twidth - 1;

         for (jj = twidth - 1; jj > 0; jj--)  /* ignore trailing blanks */
         {
            if (*buffer == ' ')
            {
              buffer--;
              arrayptr--;
            }
            else
              break;
         }
         *(arrayptr + 1) = 0;  /* write the string terminator */
         
         for (; jj >= 0; jj--)    /* copy the string itself */
         {
           *arrayptr = *buffer;
           buffer--;
           arrayptr--;
         }

         /* check if null value is defined, and if the   */
         /* column string is identical to the null string */
         if (nulcheck && !strncmp(snull, array[ii], nullen) )
         {
           *anynul = 1;   /* this is a null value */
           if (nultyp == 1)
             strcpy(array[ii], nulval);
           else
             nularray[ii] = 1;
         }
      }
    
      if (*status > 0)  /* test for error during previous read operation */
      {
         sprintf(message,
          "Error reading elements %ld thru %ld of data array (ffpcls).",
             next+1, next+ntodo);

         ffpmsg(message);
         return(*status);
      }

      /*--------------------------------------------*/
      /*  increment the counters for the next loop  */
      /*--------------------------------------------*/
      next += ntodo;
      remain -= ntodo;
      if (remain)
      {
          elemnum += ntodo;
          if (elemnum == repeat)  /* completed a row; start on next row */
          {
              elemnum = 0;
              rownum++;
          }
      }
    }  /*  End of main while Loop  */

    return(*status);
}

