#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ndf_ast.h"
#include "mers.h"
#include "star/util.h"
#include <string.h>

#define SZTEXT ( NDF__SZAST - 1 )*( NDF__MXACL + 1 ) /* Size of text buffer */

const char *ndf1Rdast( void ){
/*
*+
*  Name:
*     ndf1Rdast

*  Purpose:
*     Read AST_ data as text from an HDS object.

*  Synopsis:
*     const char *ndf1Rdast( void )

*  Description:
*     This is a service function to be provided as a "source" function for
*     the "astChannel" function. It reads data from an HDS object (in
*     response to reading from an AST_ Channel) and delivers it to the AST_
*     library for interpretation.
*
*     This function communicates with other NDF_ functions via global
*     variables. These are described below under "Global Variables used as
*     Arguments".

*  Thread Safety:
*     This function is not thread safe. Therefore this function should
*     not be called simulatentously from more than one thread. In a
*     multi-threaded context, the function that calls astread or astWrite
*     should lock the mutex that serialises access to the global variables.

*  Global Variables used as Arguments:
*     Ndf_DCB_astlc = HDSLoc * (Given)
*        A locator for the HDS object which holds the data. This must be a
*        1-dimensional _CHAR array, whose size and character string length
*        will be determined via this locator.
*     Ndf_DCB_astln = int
*        This must initially be set to the value 0, to indicate that data
*        will be read starting at the first element of the HDS array (note
*        the function will not operate correctly unless 0 is the initial
*        value - you cannot start reading at another point in the array if
*        you have previously read from a different array). On exit it will
*        be incremented by the number of elements used to obtain data, so
*        that it identifies the first element to be used on the next
*        invocation.
*     Ndf_DCB_astpt = int
*        A pointer to the contents of the HDS object, mapped in "READ"
*        mode.

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
   const char *p;        /* Pointer to start of next HDS line */
   const char *result = NULL; /* Returned pointer */
   int again;            /* Loop to read another line? */
   int ndim;             /* Number of HDS object dimensions */
   int status;           /* Local status argument */
   size_t l;             /* Number of characters in AST_ text */
   static char text[ SZTEXT + 1 ]; /* Buffer for AST_ text */
   static hdsdim dim;    /* Dimension size of HDS object */
   static size_t length; /* Length of HDS object in characters */

/* Start a new error reporting context. */
   status = SAI__OK;
   errMark();

/* Before reading the first line, obtain the number of elements in the
   HDS _CHAR array being written to, and the length of each element in
   characters. Each element is a Fortran-style fixed-length space-padded
   string without a trailing null character. All elements are contiguous
   in memory.  */
   if( Ndf_DCB_astln == 0 ) {
      datShape( Ndf_DCB_astlc, 1, &dim, &ndim, &status );
      datClen( Ndf_DCB_astlc, &length, &status );

/* If the character string length is too long to be read, then report
   an error. */
      if( status == SAI__OK ) {
         if( length > sizeof( text ) - 1 ) {
            status = NDF__TRUNC;
            msgSeti( "LENGTH", length );
            msgSeti( "LEN", sizeof( text ) - 1 );
            errRep( " ", "Length of HDS object (_CHAR*^LENGTH) exceeds "
                    "internal buffer length of ^LEN characters.", &status );
         }
      }
   }

/* Get a pointer to the first character in the next element of the HDS
   array. */
   p = Ndf_DCB_astpt + Ndf_DCB_astln*length;

/* Loop to extract lines (including continuation lines) from the
   character array and to re-assemble them into a single line. */
   l = 0;
   if( status == SAI__OK && Ndf_DCB_astln < dim ) {
      again = 1;
      while( again && ( status == SAI__OK ) ){

/* Check that we have not reached the end of the array. */
         if( Ndf_DCB_astln < dim ) {

/* If this is the first element read, insert it at the start of the
   text buffer (minus the first character, which is a flag) and update
   the count of characters in this buffer. Also update the character
   array element number. */
            if( status == SAI__OK ) {
               if( l == 0 ) {
                  memcpy( text, p + 1, length - 1 );
                  l = length - 1;

/* If it is a continuation line, check whether its contents can be
   appended to the text buffer without exceeding its length. If not,
   then report an error. */
               } else if( *p == '+' ) {
                  if( l + length >= sizeof( text ) ) {
                     status = NDF__TRUNC;
                     msgSeti( "LEN", sizeof( text ) - 1 );
                     errRep( " ", "Too many input continuation lines; "
                             "internal buffer length of ^LEN characters "
                             "exceeded.", &status );

/* Otherwise, append its contents to the text buffer and advance the
   input character array element. */
                  } else {
                     memcpy( text + l, p + 1, length - 1 );
                     l += length - 1;
                  }

/* Quit looping if we have read all the continuation lines. */
               } else {
                  again = 0;
               }

/* Move on to the next element of the HDS array. */
               if( again ) {
                  Ndf_DCB_astln++;
                  p += length;
               }
            }

/* Also quit looping if the input character array is exhausted. */
         } else {
            again = 0;
         }
      }

/* Remove any trailing blanks from the text buffer. */
      if( l > 0 ) {
         text[ l ] = 0;
         l = astChrLen( text );
         text[ l ] = 0;
      }

/* Get the value to return. */
      if( status == SAI__OK ) result = text;
   }

/* End the error reporting context. */
   errRlse();

/* Return a pointer to the line of text to be interpreted by AST. */
   return result;
}

