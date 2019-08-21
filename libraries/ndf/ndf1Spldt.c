#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Spldt( const char *str, size_t sbeg, size_t send,
                const char *delim, int mxfld, int fbeg[], int fend[],
                int *nfield, int *status ){
/*
*+
*  Name:
*     ndf1Spldt

*  Purpose:
*     Split the fields in a date/time string.

*  Synopsis:
*     void ndf1Spldt( const char *str, size_t sbeg, size_t send,
*                     const char *delim, int mxfld, int fbeg[], int fend[],
*                     int *nfield, int *status )

*  Description:
*     This function identifies a series of fields within a string (or sub-
*     string) which are separated by one of a set of delimiters and returns
*     the character positions identifying the start and end of each field.
*     Surrounding blank characters are stripped, both from the original
*     string (or sub-string) and from each field. An error results if the
*     expected number of fields is exceeded or (optionally) if too few
*     fields are found. An error also results if any field is empty or
*     entirely blank.
*
*     This function is intended for identifying the separate date and time
*     fields within a date/time string.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        analysed.
*     sbeg
*        Zero based index at which to start analysing "str".
*     send
*        Zero based index at which to finish analysing "str".
*     delim
*        Pointer to a null terminated string holding a sequence of
*        delimiter characters, any one of which will be taken as a field
*        separator if it is found within "str" (a blank may also be used,
*        although multiple adjacent blanks will only count as a single
*        occurrence of a delimiter).
*     mxfld
*        The maximum number of fields expected. The absolute value of this
*        parameter is used. If it is negative, then a smaller number of
*        fields will be accepted. If it is positive, then the number of
*        fields found must match the value given exactly or an error will
*        result.
*     fbeg
*        Returned holding the zero-based index of the start of each
*        field (this array should have at least "abs(mxfld)" elements).
*     fend
*        Returned holding the zero-based index of the end of each field
*        (this array should have at least "abs(mxfld)" elements).
*     *nfield
*        Returned holding the actual number of fields found.
*     *status
*        The global status.

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
   char  *substr;        /* Local copy of substring to be analysed */
   size_t i1;            /* Character position of field start */
   size_t i1next;        /* Start of next field */
   size_t i2;            /* Character position of field end */
   size_t lstr;          /* Length of substr */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the field count. */
   *nfield = 0;

/* Make a null terminated local copy of the section of the supplied string
   to be analysed, excluding leading or trailing spaces. */
   substr = ndf1Strip( NULL, str, sbeg, send, &lstr, NULL, status );

/* If the whole string is being used, ensure "sbeg" gives the correct
   offset to theh start (i.e. zero). */
   if( sbeg > send ) sbeg = 0;

/* Loop to identify each field until an error occurs or we reach the end
   of "substr". */
   i1 = 0;
   while( ( *status == SAI__OK ) && ( i1 < lstr ) ){

/* Count each field and report an error if we have found too many. */
      if( *nfield >= abs( mxfld ) ) {
         *status = NDF__DTMIN;
         errRep( " ", "Invalid date/time specification; too many fields "
                 "found.", status );
      } else {

/* Advance the character pointer to the first non-blank character in
   the field. */
         while( substr[i1] == ' ' ) i1++;

/* Find the final character in the field (the last character before the
   next delimiter or end of string). */
         for( i2 = i1; i2 < lstr; i2++ ){
            if( strchr( delim, substr[ i2 ] ) ) break;
         }
         i2--;

/* Remember the index at the start of the next field. */
         i1next = i2 + 2;

/* Find the preceeding non-space character. */
         while( i2 >= i1 && substr[i2] == ' ' ) i2--;

/* Check if two adjacent delimiters were found and report an error if
   so. */
         if( i2 < i1 ) {
            *status = NDF__DTMIN;
            errRep( " ", "Invalid date/time specification; field value is "
                    "missing.", status );

/* Report an error if we have found too many fields. */
         } else if( *nfield >= abs( mxfld ) ) {
            *status = NDF__DTMIN;
            errRep( " ", "Invalid date/time specification; too many "
                    "fields found.", status );

/* Store the first and last character positions, making them relative to
   the start of the whole string. */
         } else {
            fbeg[ *nfield ] = i1 + sbeg;
            fend[ *nfield ] = i2 + sbeg;

/* Increment the number of fields found. */
            (*nfield)++;

         }
      }

/* Update the start of the next field and return to process it. */
      i1 = i1next;
   }

/* If OK, but insufficient fields have been found, then report an
   error. */
   if( *status == SAI__OK ) {
      if( mxfld > 0 && *nfield < mxfld ) {
         *status = NDF__DTMIN;
         errRep( " ", "Invalid date/time specification; too few fields "
                 "found.", status );
      }
   }

/* Free resources. */
   substr = astFree( substr );

}

