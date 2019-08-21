#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "mers.h"
#include "ndf1.h"

void ndf1Pshdt( const char *str, int ymdhm[], float *sec, int *status ){
/*
*+
*  Name:
*     ndf1Pshdt

*  Purpose:
*     Parse a history date/time string.

*  Synopsis:
*     void ndf1Pshdt( const char *str, int ymdhm[], float *sec, int *status )

*  Description:
*     This function parses a string containing a history date/time
*     specification read from the history component of an NDF data
*     structure. This is normally expected to be in one of the standard
*     history formats (either of the forms "1993-JUN-01 14:51:4.143" or
*     "1993/JUN/01 14:51:4.143"), but since actual data files may contain
*     other formats, this function is designed to be flexible; it will
*     accept most reasonable variations on the above forms. The resulting
*     date/time is checked for validity.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the date/time string
*        to be parsed.
*     ymdhm
*       Returned holding the year, month, day, hour and minute fields of
*       the date/time (in that order), stored as integers. The supplied
*       "ymdhm" array should have at least "5" elements.
*     *sec
*        Returned holding the seconds (and fractions of a second) field.
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
*     MERCHANTABILITY or FITNESS FOR "a" PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R."f". Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *substring = NULL;/* Dynamically allocated substring */
   char month[ 12 ][ 10 ] = { "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER" };  /* Month names */
   int f[ 10 ];          /* Character position of sub-field start */
   int i;                /* Loop counter for fields */
   int ii;               /* Counter for field delimiters */
   int im;               /* Loop counter for months */
   int l[ 10 ];          /* Character position of sub-field end */
   int nc;               /* Number of characters read */
   int nfield;           /* Number of fields found */
   int ok;               /* Date/time valid? */
   int tmp;              /* Temporary store for year field value */
   size_t a[ 2 ];        /* Starting positions of main fields */
   size_t b[ 2 ];        /* Ending positions of main fields */
   size_t slen;          /* String length */

/* Local Data: */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* We start by splitting the whole string into separate fields
   separated by one or more blanks (if we allow blanks around sub-field
   delimiters then we may find up to 10 such fields in a valid
   date/time string - an error results if more than this number are
   found). */
   ndf1Spldt( str, 1, 0, " ", -10, f, l, &nfield, status );
   if( *status == SAI__OK ) {

/* We actually want to split the string initially into only two main
   fields containing the (Y/M/D) and (H:M:S) values. To detect which of
   the field separators found is significant, we inspect each in turn
   and reject those which lie adjacent to one of the sub-field
   delimiters: "/-:". */
      ii = 1;
      a[ 0 ] = f[ 0 ];
      for( i = 0; i < nfield - 1; i++ ){
         if( !strchr("/-:", str[ l[ i ] ] ) &&
             !strchr("/-:", str[ f[ i + 1 ] ] ) ) {

/* Check that only one suitable main field delimiter is found.
   Otherwise there may be an extra field present or a sub-field
   delimiter missing, so report an error. */
            if( ii > 1 ) {
               *status = NDF__DTMIN;
               errRep( " ", "Invalid data/time specification; possible "
                       "extra field or missing delimiter.", status );
               break;
            }

/* Store the extent of the two main fields we want in the "a" and "b"
   arrays. */
            b[ ii - 1 ] = l[ i ];
            a[ ii ] = f[ i + 1 ];
            ii++;
         }
      }
      b[ ii - 1 ] = l[ i ];

/* If exactly two main fields cannot be found, then there is probably a
   missing field or delimiter, so report an error. */
      if( ii < 2 && *status == SAI__OK ) {
         *status = NDF__DTMIN;
         errRep( " ", "Invalid data/time specification; possible missing "
                 "field or delimiter.", status );
      }

/* Decompose the two main fields found above into exactly three
   sub-fields each, using the appropriate delimiters. Store the
   resulting field extents consecutively in the "f" and "l" arrays. */
      ndf1Spldt( str, a[ 0 ], b[ 0 ], "/-", 3, f, l, &nfield, status );
      ndf1Spldt( str, a[ 1 ], b[ 1 ], ":", 3, f + 3, l + 3, &nfield, status );
   }

/* If an error occurred above, then report contextual information. */
   if( *status != SAI__OK ) {
      msgSetc( "STR", str );
      errRep( " ", "Error occurred while reading the history date/time "
              "string '^STR'.", status );
   }

/* Attempt to read the year field, reporting an error if necessary. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 0 ], l[ 0 ], &slen, NULL,
                             status );
      if( nc = 0, sscanf( substring, "%d%n", ymdhm, &nc ) < 1 || nc < slen ){
         *status = NDF__DTMIN;
         msgSetc( "STR", str );
         msgSeti( "F", f[ 0 ] );
         msgSeti( "L", l[ 0 ] );
         errRep( " ", "Invalid year field encountered (characters ^F:^L) "
                 "in the history date/time string '^STR'.", status );
      }
   }

/* Attempt to read the month field, first by comparing it with the name
   of each month in turn, allowing abbreviation to no less than 3
   characters. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 1 ], l[ 1 ], &slen, NULL,
                             status );
      for( im = 0; im < 12; im++ ){
         if( ndf1Simlr( substring, 1, 0, month[ im ], 3 ) ) break;
      }

/* Accept the month number if "ok". */
      if( im < 12 ) {
         ymdhm[ 1 ] = im + 1;

/* Otherwise, attempt to read the field as a decimal string. If this
   also fails, then report an error. */
      } else {
         if( nc = 0, sscanf( substring, "%d%n", ymdhm + 1, &nc ) < 1 || nc < slen ){
            *status = NDF__DTMIN;
            msgSetc( "STR", str );
            msgSeti( "F", f[ 1 ] );
            msgSeti( "L", l[ 1 ] );
            errRep( " ", "Invalid month field encountered (characters "
                    "^F:^L) in the history date/time string '^STR'.", status );
         }
      }
   }

/* Attempt to read the day field, reporting an error if necessary. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 2 ], l[ 2 ], &slen, NULL,
                             status );
      if( nc = 0, sscanf( substring, "%d%n", ymdhm + 2, &nc ) < 1 || nc < slen ){
         *status = NDF__DTMIN;
         msgSetc( "STR", str );
         msgSeti( "F", f[ 2 ] );
         msgSeti( "L", l[ 2 ] );
         errRep( " ", "Invalid day field encountered (characters ^F:^L) in "
                 "the history date/time string '^STR'.", status );
      }
   }

/* Attempt to read the hour field, reporting an error if necessary. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 3 ], l[ 3 ], &slen, NULL,
                             status );
      if( nc = 0, sscanf( substring, "%d%n", ymdhm + 3, &nc ) < 1 || nc < slen ){
         *status = NDF__DTMIN;
         msgSetc( "STR", str );
         msgSeti( "F", f[ 3 ] );
         msgSeti( "L", l[ 3 ] );
         errRep( " ", "Invalid hour field encountered (characters ^F:^L) "
                 "in the history date/time string '^STR'.", status );
      }
   }

/* Attempt to read the minute field, reporting an error if necessary. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 4 ], l[ 4 ], &slen, NULL,
                             status );
      if( nc = 0, sscanf( substring, "%d%n", ymdhm + 4, &nc ) < 1 || nc < slen ){
         *status = NDF__DTMIN;
         msgSetc( "STR", str );
         msgSeti( "F", f[ 4 ] );
         msgSeti( "L", l[ 4 ] );
         errRep( " ", "Invalid minute field encountered (characters ^F:^L) "
                 "in the history date/time string '^STR'.", status );
      }
   }

/* Attempt to read the seconds field, reporting an error if necessary. */
   if( *status == SAI__OK ) {
      substring = ndf1Strip( substring, str, f[ 5 ], l[ 5 ], &slen, NULL,
                             status );
      if( nc = 0, sscanf( substring, "%f%n", sec, &nc ) < 1 || nc < slen ){
         *status = NDF__DTMIN;
         msgSetc( "STR", str );
         msgSeti( "F", f[ 5 ] );
         msgSeti( "L", l[ 5 ] );
         errRep( " ", "Invalid seconds field encountered (characters "
                 "^F:^L) in the history date/time string '^STR'.", status );
      }
   }

/* If "ok", then save the original year field value and adjust this field
   to give the full year number by adding the century if necessary.
   Note that 80 (i.e. 1980) is chosen to pre-date the writing of any
   NDF history records. */
   if( *status == SAI__OK ) {
      tmp = ymdhm[ 0 ];
      if( ymdhm[ 0 ] < 80 ) {
         ymdhm[ 0 ] += 2000;
      } else if( ymdhm[ 0 ] < 100 ) {
         ymdhm[ 0 ] += 1900;
      }

/* Mark the error stack and check the date/time fields for validity. */
      errMark();
      ndf1Vdat( ymdhm, *sec, status );

/* If this check detects an error, then it may be that the year and day
   fields have been swapped (a common variation). Swap these values,
   restoring the original uncorrected year field value, and correct the
   new year value by adding the century if necessary. */
      if( *status != SAI__OK ) {
         ymdhm[ 0 ] = ymdhm[ 2 ];
         ymdhm[ 2 ] = tmp;
         if( ymdhm[ 0 ] < 80 ) {
            ymdhm[ 0 ] += 2000;
         } else if( ymdhm[ 0 ] < 100 ) {
            ymdhm[ 0 ] += 1900;
         }

/* Begin a new error reporting environment and check the new field
   values for validity. Note if they are now "ok" and end the error
   reporting environment. */
         errBegin( status );
         ndf1Vdat( ymdhm, *sec, status );
         ok = ( *status == SAI__OK );
         errEnd( status );

/* If the second check was successful, then annul the error from the
   first check. Release the error stack. */
         if( ok ) errAnnul( status );
      }
      errRlse();

/* If the validity check failed, then report contextual information. */
      if( *status != SAI__OK ) {
         msgSetc( "STR", str );
         errRep( " ", "Error occurred while reading the history date/time "
                 "string '^STR'.", status );
      }
   }

/* Free the memory uised to hold a substring */
   substring = astFree( substring );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Pshdt", status );

}

