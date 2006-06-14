/* fhead.c - construct FITS headers and return as strings */

/*
   History :
    29Sep2004: abstracted with slight modification from the CFITSIO
               system (bdk)
    08Jul2005: add ability to accumulate a set of FITS records (bdk)
*/


/*** The original file header follows ***/


/*  This file, putkey.c, contains routines that write keywords to          */
/*  a FITS header.                                                         */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.                                           */


/*** CFITSIO includes the following notice ***/

/*

Copyright (Unpublished--all rights reserved under the copyright laws of
the United States), U.S. Government as represented by the Administrator
of the National Aeronautics and Space Administration.  No copyright is
claimed in the United States under Title 17, U.S. Code.

Permission to freely use, copy, modify, and distribute this software
and its documentation without fee is hereby granted, provided that this
copyright notice and disclaimer of warranty appears in all copies.

DISCLAIMER:

THE SOFTWARE IS PROVIDED 'AS IS' WITHOUT ANY WARRANTY OF ANY KIND,
EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO,
ANY WARRANTY THAT THE SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY
IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, AND FREEDOM FROM INFRINGEMENT, AND ANY WARRANTY THAT THE
DOCUMENTATION WILL CONFORM TO THE SOFTWARE, OR ANY WARRANTY THAT THE
SOFTWARE WILL BE ERROR FREE.  IN NO EVENT SHALL NASA BE LIABLE FOR ANY
DAMAGES, INCLUDING, BUT NOT LIMITED TO, DIRECT, INDIRECT, SPECIAL OR
CONSEQUENTIAL DAMAGES, ARISING OUT OF, RESULTING FROM, OR IN ANY WAY
CONNECTED WITH THIS SOFTWARE, WHETHER OR NOT BASED UPON WARRANTY,
CONTRACT, TORT , OR OTHERWISE, WHETHER OR NOT INJURY WAS SUSTAINED BY
PERSONS OR PROPERTY OR OTHERWISE, AND WHETHER OR NOT LOSS WAS SUSTAINED
FROM, OR AROSE OUT OF THE RESULTS OF, OR USE OF, THE SOFTWARE OR
SERVICES PROVIDED HEREUNDER."

*/






#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <stddef.h>

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"


#include "fitsio2.h"

#include "libsim/fhead_par.h"
#include "libsim/fhead.h"


static char fhead__rec[FHEAD__MXREC][81]; /* store for headers */
static int fhead__next = 0;            /* count of stored headers */


/*+ fhead_getfits - get the list of FITS records */

void fhead_getfits
( 
int *nrec,                /* number of header records (returned) */
char records[][81],       /* header records (returned) */
int  *status              /* global status (given and returned) */
)
/*  Method :
     Return the stored records.

    Authors :
     B.D.Kelly (ROE)

    History :
     08Jul2005: original (bdk)

*/
{
   int j;       /* loop counter */

   if (*status != 0) return;

   *nrec = fhead__next;

   for ( j=0; j<fhead__next; j++ )
   {
      strcpy ( records[j], fhead__rec[j] );
   }
}



/*+ fhead_init - initialise the list of FITS records */

void fhead_init
( 
int  *status        /* global status (given and returned */
)
/*  Method :
     Initialise the internal counter to zero.
    Authors :
     B.D.Kelly (ROE)

    History :
     08Jul2005: original (bdk)

*/
{

   if (*status != 0) return;

   fhead__next = 0;
}



/*+ fhead_make - make a FITS header card */

void fhead_make
( 
int  datatype,      /* I - datatype of the value    */
char *keyname,      /* I - name of keyword to write */
void *value,        /* I - keyword value            */
char *comm,         /* I - keyword comment          */
char *card,         /* O - constructed card         */
int  *status        /* IO - error status            */
)
/*
  Write (put) the keyword, value and comment into the FITS header card.
  Writes a keyword value with the datatype specified by the 1st argument.
*/
{
    char errmsg[81];
    char valstring[FLEN_VALUE];


    if (*status != 0) return;

    if (datatype == TSTRING)
    {

       ffs2c ( (char *) value, valstring, status );
    }
    else if (datatype == TBYTE)
    {
       ffi2c ( (long) *(unsigned char *) value, valstring, status );
    }
    else if (datatype == TUSHORT)
    {
       ffi2c ( (long) *(unsigned short *) value, valstring, status );
    }
    else if (datatype == TSHORT)
    {
       ffi2c ( (long) *(short *) value, valstring, status );
    }
    else if (datatype == TUINT)
    {
       ffd2f ( (double) *(unsigned int *) value, 0, valstring, status );
    }
    else if (datatype == TINT)
    {
       ffi2c ( (long) *(int *) value, valstring, status );
    }
    else if (datatype == TLOGICAL)
    {
       ffl2c ( *(int *)value, valstring, status );
    }
    else if (datatype == TULONG)
    {
       ffd2f ( (double) *(unsigned long *) value, 0, valstring, status );
    }
    else if (datatype == TLONG)
    {
       ffi2c ( *(long *) value, valstring, status );
    }
    else if (datatype == TFLOAT)
    {
       ffr2e ( *(float *) value, -7, valstring, status );
    }
    else if (datatype == TDOUBLE)
    {
       ffd2e ( *(double *) value, -15, valstring, status );
    }
    else if (datatype == TCOMPLEX)
    {
        fhead_pkyc ( (float *) value, -7, valstring, status );
    }
    else if (datatype == TDBLCOMPLEX)
    {
        fhead_pkym ( (double *) value, -15, valstring, status );
    }
    else
    {
        sprintf ( errmsg, 
          "Bad datatype code: %d : for keyword %s (fhead_make)", 
          datatype, keyname );
        ffpmsg(errmsg);
        *status = BAD_DATATYPE;
    }

/* Assemble the card string */

    ffmkky ( keyname, valstring, comm, card, status );

/* Convert any error messages */

   if ( *status != 0 )
   {
      for ( ; ; )
      {
         if ( fits_read_errmsg ( errmsg ) == 0 ) break;
         *status = DITS__APP_ERROR;
         strcat ( errmsg, "  keyname " );
         strcat ( errmsg, keyname );
         ErsRep ( 0, status, errmsg );
      }
   }


} 


/*+ fhead_pkyc - put complex float */

void fhead_pkyc
(
float *value,        /* I - keyword value (real, imaginary)     */
int   decim,         /* I - number of decimal places to display */
char *valstring,     /* O - value as string                     */
int   *status        /* IO - error status                       */
)
/*
  Writes an complex float keyword value. Format = (realvalue, imagvalue)
*/
{
    char tmpstring[FLEN_VALUE];

   if ( *status != 0 ) return;


    strcpy(valstring, "(" );
    ffr2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffr2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

}


/*+ fhead_pkym - put complex double */

void fhead_pkym
(
double *value,       /* I - keyword value (real, imaginary)     */
int   decim,         /* I - number of decimal places to display */
char *valstring,     /* O - value as string                     */
int   *status        /* IO - error status                       */
)
/*
  Writes an complex double keyword value. Format = (realvalue, imagvalue)
*/
{
    char tmpstring[FLEN_VALUE];

   if ( *status != 0 ) return;

    strcpy(valstring, "(" );
    ffd2e(value[0], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ", ");
    ffd2e(value[1], decim, tmpstring, status); /* convert to string */
    strcat(valstring, tmpstring);
    strcat(valstring, ")");

}



/*+ fhead_putfits - add a fits record to a list of them */

void fhead_putfits
( 
int  datatype,      /* I - datatype of the value    */
char *keyname,      /* I - name of keyword to write */
void *value,        /* I - keyword value            */
char *comm,         /* I - keyword comment          */
int  *status        /* IO - error status            */
)
/*  Method :
     Put the keyword, value and comment into an empty internal
     FITS header card.

    Authors :
     B.D.Kelly (ROE)

    History :
     08Jul2005: original (bdk)

*/
{

   if (*status != 0) return;

   if ( fhead__next < FHEAD__MXREC )
   {
      fhead_make ( datatype, keyname, value, comm,
        fhead__rec[fhead__next], status );
      fhead__next++;
   }
   else
   {
      *status = DITS__APP_ERROR;
      ErsRep ( 0, status, 
        "fhead library: tried to write too many FITS headers" );
   }

}


/*+ fhead_putfitscom - add a fits comment record to a list of them */

void fhead_putfitscom
( 
char *comm,         /* I - keyword comment          */
int  *status        /* IO - error status            */
)
/*  Method :
     Construct a COMMENT FITS header with the given string and put it
     into an empty card.

    Authors :
     B.D.Kelly (ROE)

    History :
     24Aug2005: original (bdk)

*/
{

   if (*status != 0) return;

   if ( fhead__next < FHEAD__MXREC )
   {
      sprintf ( fhead__rec[fhead__next], "COMMENT  %s", comm );
      fhead__next++;
   }
   else
   {
      *status = DITS__APP_ERROR;
      ErsRep ( 0, status, 
        "fhead library: tried to write too many FITS headers" );
   }

}


