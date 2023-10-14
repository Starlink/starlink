#include "dat_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "ndf1_types.h"
#include "sae_par.h"
#include <stdlib.h>

#define sztext ( NDF__SZAST - 1 )*( NDF__MXACL + 1 ) /* Size of text buffer */

void ndf1Wrast( const char *text ){
/*
*+
*  Name:
*     ndf1Wrast

*  Purpose:
*     Write AST_ data as text to an HDS object.

*  Synopsis:
*     void ndf1Wrast( const char *text )

*  Description:
*     This is a service function to be provided as a "sink" function for
*     the "astChannel" function. It takes data in the form of text (in
*     response to writing an AST_ object to a Channel) and delivers it to
*     an HDS object for storage.
*
*     This function communicates with other NDF_ functions via global
*     variables. These are described below under "Global Variables used
*     as Arguments".
*

*  Parameters:
*     text
*        Pointer to the line of text to be written out to HDS.

*  Thread Safety:
*     This function is not thread safe. Therefore this function should
*     not be called simulatentously from more than one thread. In a
*     multi-threaded context, the function that calls astWrite should
*     lock the mutex that serialises access to the global variables.

*  Global Variables used as Arguments:
*     Ndf_DCB_astlc = HDSLoc * (Given)
*        A locator for the HDS object which is to store the data. This must
*        be a 1-dimensional _CHAR array, whose initial size and character
*        string length will be determined via this locator. Write access to
*        the object must be available via this locator, but the locator
*        itself is not altered by this function.
*     Ndf_DCB_astln = int (Given and Returned)
*        This must initially be set to the value 0, to indicate that data
*        will be written starting at the first element of the HDS array
*        (note the function will not operate correctly unless 0 is the
*        initial value - you cannot start writing at another point in the
*        array if you have previously written to a different array). On
*        exit it will be incremented by the number of elements used to
*        store data, so that it identifies the first element to be used on
*        the next invocation.
*     Ndf_DCB_astpt = int (Given and Returned)
*        A pointer to the contents of the HDS object, initially mapped in
*        "WRITE" mode. This pointer may be modified by the function (and
*        re-mapped in "UPDATE" mode) if it needs to extend the size of the
*        object to accommodate the data written.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *pout;           /* Pointer to next output character */
   char *pout_last;      /* Pointer to last character in output line */
   const char *pin;      /* Pointer to next input character */
   int ndim;             /* Number of HDS object dimensions */
   int started;          /* Has first non-blank character been found? */
   int status;           /* Local status argument */
   size_t i;             /* Index of next input character */
   size_t nc;            /* No. of input characters to use */
   static hdsdim dim;    /* Dimension size of HDS object */
   static size_t length; /* Length of HDS object in characters */

   NDF__DCB_ASSERT_ASTMUTEX;

/* Start a new error reporting context. */
   status = SAI__OK;
   errMark();

/* Get the length of the supplied input string excluding trailing spaces.
   Skip if there is nothing to write. */
   nc = astChrLen( text );
   if( nc ) {

/* Before writing the first line, obtain the initial number of elements
   in the HDS _CHAR array being written to, and the length of each
   element in characters. Each element is a Fortran-style fixed-length
   space-padded string without a trailing null character. All elements
   are contiguous in memory.  */
      if( Ndf_DCB_astln == 0 ) {
         datShape( Ndf_DCB_astlc, 1, &dim, &ndim, &status );
         datClen( Ndf_DCB_astlc, &length, &status );
      }

/* Check that the HDS array is large enough to accomodate the number of
   required lines. If not, unmap the array and double its size. Then
   re-map it. We do not know exactly how many lines will be needed as we
   do not yet know how many continuation lines will be needed. But
   doubling the array size should be good enough. */
      if( Ndf_DCB_astln >= dim ) {
         datUnmap( Ndf_DCB_astlc, &status );
         dim = 2*dim;
         datAlter( Ndf_DCB_astlc, 1, &dim, &status );
         datMap( Ndf_DCB_astlc, "_CHAR", "UPDATE", 1, &dim,
                 (void **) &Ndf_DCB_astpt, &status );
      }
      if( status == SAI__OK ) {

/* Get a pointer to the first character in the next element of
   the HDS array */
         pout = Ndf_DCB_astpt + length*Ndf_DCB_astln;

/* The input text is broken up into lines of no more than "length-1"
   characters (this includes an initial flag character for each line
   indicating if it is a continuation line). Get a pointer to the last
   character in the first output line and store a space as the
   first character in the first line and */
         pout_last = pout + length - 1;
         *pout = ' ';

/* Loop over all characters in the supplied text, excluding trailing
   spaces. */
         started = 0;
         pin = text;
         for( i = 0; i < nc; i++,pin++ ) {

/* Skip any leading spaces (this removes any indentation). */
            if( !started && *pin != ' ' ) started = 1;
            if( started ) {

/* Increment the output pointer and then copy the next input character
   to the output. */
               *(++pout) = *pin;

/* If we have just written the final character in the current output
   line, then prepare to write the next, if there are any input
   characters left to write out. */
               if( pout == pout_last && i < nc - 1 ) {

/* Increment the index of the current output line. Check that the HDS
   array is large enough to accomodate it. If not, unmap the array and
   double its size. Then re-map it. */
                  if( ++Ndf_DCB_astln >= dim ) {
                     datUnmap( Ndf_DCB_astlc, &status );
                     dim = 2*dim;
                     datAlter( Ndf_DCB_astlc, 1, &dim, &status );
                     datMap( Ndf_DCB_astlc, "_CHAR", "UPDATE", 1, &dim,
                             (void **) &Ndf_DCB_astpt, &status );
                     if( status != SAI__OK ) break;

/* Update the pointer to the start and end of the next output line. */
                     pout = Ndf_DCB_astpt + length*Ndf_DCB_astln - 1;
                     pout_last = pout + length - 1;

                  } else {
                     pout_last += length;
                  }

/* Store a continuation character as the first character  in the
   subline. */
                  *(++pout) = '+';
               }
            }
         }

/* Pad the HDS element with spaces. */
         while( pout < pout_last ) *(++pout) = ' ';

/* Increment the number of lines stored. */
         Ndf_DCB_astln++;
      }
   }

/* Pass any error to AST. */
   if( status != SAI__OK ) astError( NDF__WRAST, "ndf1Wrast: Error writing "
                                     "out a line of WCS information." );

/* End the error reporting context. */
   errRlse();

}

