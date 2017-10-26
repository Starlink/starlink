#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dvfy( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dvfy

*  Purpose:
*     Verify that a data object is correctly constructed.

*  Synopsis:
*     void ary1Dvfy( AryDCB *dcb, int *status )

*  Description:
*     This function checks that a data object, identified by its
*     DCB, is correctly constructed and contains no "rogue"
*     components. An error is reported if a problem is found,
*     otherwise the routine returns without further action.

*  Parameters:
*     dcb
*        The data object.
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
   HDSLoc *loc=NULL;          /* Component locator */
   char name[DAT__SZNAM+1];   /* Component name */
   int icomp;                 /* Loop counter for array components */
   int ncomp;                 /* Number of array components */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Clear the DCB flag and obtain fresh form information for the array. */
   dcb->kform = 0;
   ary1Dfrm( dcb, status );

/* Handle each form of array in turn. */
   if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Clear all relevant DCB flags and annul associated locators. */
         if( dcb->ktype ){

/* Annul the non-imaginary component locator. */
            datAnnul( &dcb->dloc, status );
            dcb->ktype = 0;
         }
         dcb->kbnd = 0;
         dcb->kbad = 0;
         dcb->kstate = 0;
         dcb->kmode = 0;

/* Get new information about the data object, causing all associated
   checking to be performed. If this stage completes successfully, then
   the data object is correctly constructed. */
         ary1Dtyp( dcb, status );
         ary1Dbnd( dcb, status );
         ary1Dbad( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dmod( dcb, status );

/* Simple and scaled arrays.
   ========================= */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "DELTA" ) ||
                 !strcmp( dcb->form, "SCALED" ) ){

/* Clear all relevant DCB flags and annul associated locators. */
         if( dcb->ktype ){

/* Annul the non-imaginary component locator (and the imaginary component
   locator, if present). */
            datAnnul( &dcb->dloc, status );
            if( dcb->complex ) datAnnul( &dcb->iloc, status );
            dcb->ktype = 0;
         }
         dcb->kbnd = 0;
         dcb->kbad = 0;
         dcb->kstate = 0;
         dcb->kmode = 0;
         dcb->kscl = 0;
         if( dcb->scloc ) datAnnul( &dcb->scloc, status );

/* Get new information about the data object, causing all associated
   checking to be performed. If this stage completes successfully, then
   the data object is correctly constructed, although it may still contain
   rogue components. */
         ary1Dtyp( dcb, status );
         ary1Dbnd( dcb, status );
         ary1Dbad( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dmod( dcb, status );
         ary1Dscl( dcb, status );

/* Obtain the number of components in the array structure. */
         datNcomp( dcb->loc, &ncomp, status );
         if( *status == SAI__OK ){

/* Loop to examine each component. */
            for( icomp = 0; icomp < ncomp; icomp++ ){

/* Locate the component and obtain its name, then annul its locator. */
               datIndex( dcb->loc, icomp + 1, &loc, status );
               datName( loc, name, status );
               datAnnul( &loc, status );
               if( *status == SAI__OK ){

/* Test the name against all the permitted component names. */
                  if( strcmp( name, "VARIANT" ) &&
                      strcmp( name, "DATA" ) &&
                      strcmp( name, "SCALE" ) &&
                      strcmp( name, "ZERO" ) &&
                      strcmp( name, "ORIGIN" ) &&
                      strcmp( name, "IMAGINARY_DATA" ) &&
                      strcmp( name, "BAD_PIXEL" ) && (
                         strcmp( dcb->form, "DELTA" ) || (
                            strcmp( name, "ZAXIS" ) &&
                            strcmp( name, "ZDIM" ) &&
                            strcmp( name, "ZRATIO" ) &&
                            strcmp( name, "VALUE" ) &&
                            strcmp( name, "REPEAT" ) &&
                            strcmp( name, "FIRST_DATA" ) &&
                            strcmp( name, "FIRST_VALUE" ) &&
                            strcmp( name, "FIRST_REPEAT" ) ) ) ){

/* Report an error if a rogue component is found. */
                     *status = ARY__ROGUE;
                     datMsg( "ARRAY", dcb->loc );
                     msgSetc( "NAME", name );
                     errRep( " ", "The array structure ^ARRAY contains a rogue "
                             "^NAME component.", status );
                  }
               }

/* Quit examining components as soon as an error occurs. */
               break;
            }
         }

/* If the DCB form entry was not recognised, then report an error. */
      } else if( *status == SAI__OK ) {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( " ", "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dvfy", status );
}
