#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Fsplt( const char *fname, size_t start, size_t end, size_t *d1,
                size_t *d2, size_t *n1, size_t *n2, size_t *t1, size_t *t2,
                size_t *v1, size_t *v2, int *status ){
/*
*+
*  Name:
*     ndf1Fsplt

*  Purpose:
*     Split a file name into directory, name, type and version fields.

*  Synopsis:
*     void ndf1Fsplt( const char *fname, size_t start, size_t end, size_t *d1,
*                     size_t *d2, size_t *n1, size_t *n2, size_t *t1, size_t *t2,
*                     size_t *v1, size_t *v2, int *status )

*  Description:
*     This function splits a full file name into a directory field, a name
*     field, a type field (which contains a leading ".") and a version
*     field and returns the character positions of the start and end of
*     each field.

*  Parameters:
*     fname
*        Pointer to a null terminated string holding the file
*        specification.
*     start
*        The zero-based index of the first character to consider in "fname".
*        The whole string is used if "start" > "end".
*     end
*        The zero-based index of the last character to consider in "fname".
*        The whole string is used if "start" > "end".
*     *d1
*        Returned holding the zero-based position of the first character
*        in the directory field.
*     *d2
*        Returned holding the zero-based position of the last character
*        in the directory field.
*     *n1
*        Returned holding the zero-based position of the first character
*        in the file name field.
*     *n2
*        Returned holding the zero-based position of the last character in
*        the file name field.
*     *t1
*        Returned holding the zero-based position of the first character in
*        the type field.
*     *t2
*        Returned holding the zero-based position of the last character in
*        the type field.
*     *v1
*        Returned holding the zero-based position of the first character
*        in the version field.
*     *v2
*        Returned holding the zero-based position of the last character in
*        the version field.
*     *status
*        The global status.

*  Notes:
*     -  If the supplied file name contains no directory field, then "d2"
*     is returned less than "d1", if it contains no name field, then "n2"
*     is returned less than "n1", if it contains no type field, then "t2"
*     is returned less than "t1", and if it contains no version field, then
*     "v2" is returned less than "v1".
*     -  The directory field ends at the last "/" and the type
*     field begins at the last "." (if any) which follows this. As there is
*     no version field, "v2" will always be returned less than "v1".

*  Machine-specific features used:
*     This function unavoidably has to make assumptions about the form of
*     POSIX file names.

*  Implementation Deficiencies:
*     The file name supplied should be in a fully expanded form. If it
*     contains environment variables, then it is not guaranteed that the
*     file name syntax can be correctly analysed.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.
*        Support for VMS has been removed.

*-
*/

/* Local Variables: */
   char *lfname;         /* Copy of fname without leading/trailing spaces */
   char *p;              /* Pointer to delimter charactrer */
   size_t istart;        /* Index of first supplied non-blank character */
   size_t nc;            /* Length excluding leading/trailing spaces */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get a copy of the supplied string excluding leading or trailing
   blanks. This also gets the index of the first non-space character in
   fname. All other indices used below are relative to the start of this
   copy. The returned indices are correct to make them relative to the
   start of hte supplied string just before returning. */
   lfname = ndf1Strip( NULL, fname, start, end, &nc, &istart, status );
   if( *status == SAI__OK && lfname ) {

/* If the file name is blank, then report an error. */
      if( *lfname == 0 ) {
         *status = NDF__NAMIN;
         errRep( " ", "Blank file specification supplied.", status );

/* Search for the last "/" character which delimits the final field of the
   file's path name. */
      } else {
         p = strrchr( lfname, '/' );
         if( p ) {
            *d1 = 0;
            *d2 = p - lfname;
            *n1 = *d2 + 1;
         } else {
            *d1 = 1;
            *d2 = 0;
            *n1 = 0;
         }

/* If there is nothing left, then there cannot be a name field or a
   type field, so return null values. */
         if( lfname[*n1] == 0 ) {
            *n1 = 1;
            *n2 = 0;
            *t1 = 1;
            *t2 = 0;

/* Otherwise, return the last character position of the type field. */
         } else {
            *t2 = nc - 1;

/* Search backwards for the last "." which marks the start of the type
   field. */
            p = strrchr( lfname, '.' );

/* If not found, then there is no type field. Otherwise, return
   the final character position of the name field. */
            if( !p ) {
               *n2 = *t2;
               *t1 = 1;
               *t2 = 0;
            } else {
               *t1 = p - lfname;
               *n2 = *t1 - 1;
            }
         }

/* Return a null version field. */
         *v1 = 1;
         *v2 = 0;

      }

/* Adjust the returned indices to make them relative to the first
   used character within the supplied string (which may be blank). */
     if( *d2 >= *d1 ) {
         *d1 += istart;
         *d2 += istart;
      }
      if( *n2 >= *n1 ) {
         *n1 += istart;
         *n2 += istart;
      }
      if( *t2 >= *t1 ) {
         *t1 += istart;
         *t2 += istart;
      }
      if( *v2 >= *v1 ) {
         *v1 += istart;
         *v2 += istart;
      }

/* Adjust the returned indices to make them relative to the first
   character within the whole supplied string. */
      if( start <= end ) {
         if( *d2 >= *d1 ) {
            d1 += start;
            d2 += start;
         }
         if( *n2 >= *n1 ) {
            n1 += start;
            n2 += start;
         }
         if( *t2 >= *t1 ) {
            t1 += start;
            t2 += start;
         }
         if( *v2 >= *v1 ) {
            v1 += start;
            v2 += start;
         }
      }
   }

/* Free resources */
   lfname = astFree( lfname );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Fsplt", status );

}

