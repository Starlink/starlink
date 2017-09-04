#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void aryFind( HDSLoc *loc, const char *name, Ary **ary, int *status ) {
/*
*+
*  Name:
*     aryFind

*  Purpose:
*     Find an array in an HDS structure and import it into the ARY_
*     system.

*  Synopsis:
*     void aryFind( HDSLoc *loc, const char *name, Ary **ary, int *status )

*  Description:
*     This function finds a named array within an HDS structure, imports
*     it into the ARY_ system and issues an identifier for it. The
*     imported array may then be manipulated by the ARY_ routines.

*  Parameters:
*     loc
*        Locator to the enclosing HDS structure.
*     name
*        Name of the HDS structure component to be imported.
*     ary
*        Address of variable in which to return the Array identifier.
*     status
*        The global status.

*  Notes:
*     -  If this routine is called with "status" set, then a NNULL pointer
*     will be returned for the "ary" argument, although no further
*     processing will occur. The same value will also be returned if the
*     routine should fail for any reason.

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

/* Local variables: */
   HDSLoc *locary;            /* Locator to array structure */
   AryACB *acb;               /* The ACB structure describing the array */

/* Set an initial value for the returned "ary" value. */
   *ary = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check that a standard data component name has been supplied. */
   ary1Chscn( name, status );

/* Locate the named HDS array structure. */
   locary = NULL;
   datFind( loc, name, &locary, status );

/* Import the array structure into the ACB. */
   ary1Imp( locary, &acb, status );

/* Export an array identifier. */
   *ary = ary1Expid( (AryObject *) acb, status );

/* Annul the locator to the array structure. */
   datAnnul( &locary, status );

/* If an error occurred, then reset the "ary" argument and report context
   information. */
   if( *status != SAI__OK ){
      ary = NULL;
      errRep( " ", "ARY_FIND: Error finding an array in an HDS structure.",
              status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "aryFind", status );

}
