#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"

void ary1Cpync( HDSLoc *loc1, const char *name, HDSLoc *loc2,
                int *status ) {
/*
*+
*  Name:
*     ary1Cpync

*  Purpose:
*     Copy a named HDS component from one structure to another.

*  Synopsis:
*     void ary1Cpync( HDSLoc *loc1, const char *name, HDSLoc *loc2,
*                     int *status )

*  Description:
*     This function copies a named HDS component (if it exists) from one
*     structure to another, preserving its name in the process. If the
*     component to be copied does not exist, then the routine completes
*     without action, but no error results. An error will be reported,
*     however, if a component exists to be copied but a component of
*     the same name already exists in the output structure.

*  Parameters:
*     loc1
*        Locator to input HDS structure.
*     name
*        HDS name of the component to be copied.
*     loc2
*        Locator to the HDS structure which is to receive the copied
*        component.
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

/* Local variables: */
   int there;                 /* Whether the component exists */
   HDSLoc *tloc = NULL;       /* Temporary locator */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   there = 1;

/* See if the component to be copied exists. */
   datThere( loc1, name, &there, status );
   if( ( *status == SAI__OK ) && there ){

/* If so, then locate it and copy it. */
      datFind( loc1, name, &tloc, status );
      datCopy( tloc, loc2, name, status );

/* Annul the locator when done. */
      datAnnul( &tloc, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cpync", status );

}
