#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"
#include <string.h>

void ndf1Delob( HDSLoc **loc, int *status ){
/*
*+
*  Name:
*     ndf1Delob

*  Purpose:
*     Delete an HDS object (and all sub-objects).

*  Synopsis:
*     void ndf1Delob( HDSLoc **loc, int *status )

*  Description:
*     This function recursively deletes an HDS object (and all sub-objects)
*     passed by locator, and also annuls the locator. The object may be a
*     top-level object, in which case the container file itself will be
*     deleted. If the object is a cell or slice from a structure array,
*     then it will simply be emptied (i.e. all its components will be
*     erased).

*  Parameters:
*     *loc
*        Locator to the HDS object to be deleted. This will be annulled,
*        and a value of DAT__NOLOC will be returned.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if it is called with
*     "status" set, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  This function will handle top-level HDS objects and should always
*     be used if the object may be a top level object. In cases where the
*     object is known to be a sub-component, the ndf1Antmp function may be
*     called (and will be more efficient).
*     -  An error will result and a "status" value of NDF__BNDIN will be
*     returned (unless "status" was set on entry) if an attempt is made to
*     delete a cell or slice of a primitive HDS array.

*  Implementation Deficiencies:
*     -  The method by which this function determines if it has been passed
*     a cell from a structure array is rather unsatisfactory. This should
*     be improved when facilities for obtaining this information become
*     available in HDS.

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

*-
*/

/* Local Variables: */
   char file[ NDF__SZFIL + 1 ];    /* Container file name */
   char path[ NDF__SZPTH + 1 ];    /* Object path name */
   hdsbool_t prim;       /* Whether object is primitive */
   int nlev;             /* Depth of object nesting */
   size_t nc;            /* Character count in object path name */

/* Begin a new error reporting environment. */
   errBegin( status );

/* Obtain the depth of HDS object nesting, together with its container
   file and path name. */
   hdsTrace( *loc, &nlev, path, file, status, sizeof( path ),
             sizeof( file ) );
   if( *status == SAI__OK ) {

/* See if the object is a cell (by seeing whether its path name ends in
   ")"). If it is, then it cannot be deleted because other elements of
   the array from which it is drawn must continue to exist. In this
   case, we will simply reset the object. */
      nc = NDF_MAX( 1, astChrLen( path ) );
      if( path[ nc - 1 ] == ')' ) {

/* Before resetting the object, determine if it is primitive (we cannot
   reset it if it is, as other elements of the same array would also be
   reset). Report an error if necessary. */
         datPrim( *loc, &prim, status );
         if( *status == SAI__OK ) {
            if( prim ) {
               *status = NDF__BNDIN;
               datMsg( "OBJECT", *loc );
               errRep( " ", "^OBJECT is a subset of a primitive HDS object "
                       "and cannot be deleted independently of the rest of "
                       "the array.", status );

/* If OK, we have a cell or slice of a structure array, so reset each
   element. */
            } else {
               ndf1Hrst( *loc, status );
            }
         }

/* Annul the object locator. */
         datAnnul( loc, status );

/* If it is a top level object, then mark it for deletion through HDS. */
      } else if( nlev <= 1 ) {
         hdsErase( loc, status );

/* If it is not a top level object, then call ndf1Antmp to erase it. */
      } else {
         ndf1Antmp( loc, status );
      }

/* If an error occurred while obtaining information about the object,
   then simply annul its locator. */
   } else {
      datAnnul( loc, status );
   }

/* Reset the returned locator value. */
   *loc = NULL;

/* Call error tracing function if appropriate. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Delob", status );

/* End the error reporting environment. */
   errEnd( status );

}

