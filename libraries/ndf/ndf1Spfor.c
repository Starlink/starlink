#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"

void ndf1Spfor( const char *fname, NdfFCB *fcb, size_t *d1, size_t *d2,
                size_t *n1, size_t *n2, size_t *t1, size_t *t2, size_t *v1,
                size_t *v2, size_t *x1, size_t *x2, int *status ){
/*
*+
*  Name:
*     ndf1Spfor

*  Purpose:
*     Split a foreign format file name into its components.

*  Synopsis:
*     void ndf1Spfor( const char *fname, NdfFCB *fcb, size_t *d1,
*                     size_t *d2, size_t *n1, size_t *n2, size_t *t1,
*                     size_t *t2, size_t *v1, size_t *v2, size_t *x1,
*                     size_t *x2, int *status )

*  Description:
*     This function splits a full foreign format file name into a directory
*     field, a name field, a type field (which contains a leading "."), a
*     version field and a foreign extension field and returns the character
*     positions of the start and end of each field.

*  Parameters:
*     fname
*        Pointer to a null terminated string holding the file
*        specification.
*     fcb
*        Pointer to an object describing the foreign data format (must be
*        non-NULL).
*     *d1
*        Returned holding the position of the first character in the
*        directory field.
*     *d2
*        Returned holding the position of the last character in the
*        directory field.
*     *n1
*        Returned holding the position of the first character in the file
*        name field.
*     *n2
*        Returned holding the position of the last character in the file
*        name field.
*     *t1
*        Returned holding the position of the first character in the type
*        field.
*     *t2
*        Returned holding the position of the last character in the type
*        field.
*     *v1
*        Returned holding the position of the first character in the
*        version field.
*     *v2
*        Returned holding the position of the last character in the version
*        field.
*     *x1
*        Returned holding the position of the first character in the
*        foreign extension field.
*     *x2
*        Returned holding the position of the last character in the foreign
*        extension field.
*     *status
*        The global status.

*  Notes:
*     -  This function should be used in preference to ndf1Fsplt whenever a
*     foreign format file name is being processed as it will permit the "."
*     character to occur in the file extension field if necessary (and so
*     long as the actual type field present in the file name matches that
*     expected for the specified foreign format).
*     -  If the supplied file name contains no directory field, then "d2"
*     is returned less than "d1", if it contains no name field, then "n2"
*     is returned less than "n1", if it contains no type field, then "t2"
*     is returned less than "t1", if it contains no version field, then
*     "v2" is returned less than "v1", and if it contains no foreign
*     extension field, then "x2" is returned less than "x1".
*     -  The restrictions that apply to the function ndf1Fsplt relating to
*     the expansion of file name components also apply to this function.

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
   int found;            /* Expected file extension present? */
   size_t flen;          /* Length of expected file extension */
   size_t tmin;          /* Anticipated start of type field */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Locate any foreign extension specifier. "x1" is returned equal to one
   more than the used length of the string if no foreign extension specifier
   is present. */
   ndf1Forxt( fname, 1, 0, x1, x2, status );

/* Split the preceeding file name into its directory, name, type and
   version fields, using the file name syntax rules for the host machine. */
   ndf1Fsplt( fname, 0, *x1 - 2, d1, d2, n1, n2, t1, t2, v1, v2, status );
   if( *status == SAI__OK ) {

/* If a file type extension appears to be present, then obtain the
   length of the expected file extension in the FCB. */
      if( *t2 >= *t1 ) {
         flen = astChrLen( fcb->ext );

/* Since the file extension may contain a "." character, it may actually
   be longer than identified above (i.e. the end of the name field may
   still contain the first part of the file extension). Find the first
   character position at which the full file extension field could
   start (allowing it to extend into the name field, if present, but not
   into the directory field). */
         tmin = *t1;
         if( *n2 >= *n1 ) tmin = *n1;

/* Adjust the anticipated starting position for the expected file
   extension, given by the file's format code. */
         tmin = NDF_MIN( NDF_MAX( tmin, *t2 - flen + 1 ), *t1 );

/* Test if the expected file extension is present (extending into the
   name field if necessary). */
         ndf1Cmpfl( fname, tmin, *t2, fcb->ext, &found, status );
         if( *status == SAI__OK ) {

/* If so, then correct the name and type field limits to identify it. */
            if( found ) {
               *t1 = tmin;
               if( *n2 >= *n1 ) *n2 = NDF_MIN( *n2, *t1 );
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Spfor", status );

}

