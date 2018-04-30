#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Cmtmp( const char *type, int ndim, const hdsdim *dim, HDSLoc **loc,
                void **pntr, int *status ) {
/*
*+
*  Name:
*     ary1Cmtmp

*  Purpose:
*     Create and map a temporary workspace array.

*  Synopsis:
*     void ary1Cmtmp( const char *type, int ndim, const hdsdim *dim,
*                     HDSLoc **loc, void **pntr, int *status ) {

*  Description:
*     The routine creates a temporary HDS object of the type and shape
*     specified and maps it for use as workspace. A pointer to the
*     workspace is returned.  The type specified must be a primitive
*     numeric data type, otherwise an error will be reported.

*  Parameters:
*     type
*        An HDS primitive data type string specifying the type of object
*        to be created (case insensitive).
*     ndim
*        Number of object dimensions.
*     dim
*        Pointer to array holding object dimensions.
*     loc
*        Address at which to return HDS locator to the temporary object.
*     pntr
*        Address at which to return a pointer to the mapped array. The
*        array is not initialised by this routine.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check that the data type specified is numeric and report an error if it
   is not. */
   if( !ary1Intyp( type, status ) ){
      *status = ARY__FATIN;
      msgSetc( "BADTYPE", type );
      errRep( "ARY1_CMTMP_TYPE",
              "Routine ary1Cmtmp called with an invalid TYPE argument of "
              "'^BADTYPE' (internal programming error).", status );

/* Create the temporary object and map it as workspace. */
   } else {
      ary1Temp( type, ndim, dim, loc, status );
      datMap( *loc, type, "WRITE", ndim, dim, pntr, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cmtmp", status );

}
