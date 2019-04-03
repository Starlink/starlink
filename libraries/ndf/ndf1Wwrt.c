#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Wwrt( AstFrameSet *iwcs, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Wwrt

*  Purpose:
*     Write WCS information to an entry in the DCB.

*  Synopsis:
*     void ndf1Wwrt( AstFrameSet *iwcs, NdfDCB *dcb, int *status )

*  Description:
*     This function stores new WCS (World Coordinate System) information in
*     a data object with an entry in the DCB, over-writing any information
*     that may already be present.

*  Parameters:
*     iwcs
*        A pointer to an AST_ object which contains the coordinate system
*        information to be written. This should be a pointer to a FrameSet
*        satisfying all the requirements imposed by the NDF_ library, but
*        this function makes no check on this and any valid AST_ pointer
*        will be accepted and used. The AST_ object itself is not modified
*        by this function.
*     dcb
*        Pointer to the data object entry in the DCB.
*     *status
*        The global status.

*  Notes:
*     - This function stores a cloned pointer to the AST_ object supplied
*     (not a copy) in the DCB.

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
   AstChannel *chan;     /* Pointer to AST_ Channel */
   HDSLoc *wcsloc = NULL;/* Locator to WCS component */
   hdsbool_t there;      /* Component present? */
   hdsdim dim;           /* Dimension size of HDS object */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that WCS information is available in the DCB. */
   ndf1Dw( dcb, status );
   if( *status == SAI__OK ) {

/* If a pointer to WCS information is held in the DCB, then annul it. */
      if( dcb->iwcs ) dcb->iwcs = astAnnul( dcb->iwcs );

/* If a "WCS" component exists in the NDF data structure, then erase it. */
      there = 0;
      datThere( dcb->loc, "WCS", &there, status );
      if( there ) datErase( dcb->loc, "WCS", status );

/* Clone an AST_ pointer to the WCS information and store it in the
   DCB. Exempt the new pointer from AST_ context handling (so it is not
   annulled if "astEnd" is called). */
      dcb->iwcs = astClone( iwcs );
      astExempt( dcb->iwcs );

/* Create a new WCS component (a scalar structure). */
      dim = 0;
      datNew( dcb->loc, "WCS", "WCS", 0, NULL, status );

/* Obtain a locator to this structure and create a DATA component (a
   1-dimensional _CHAR array) within it to hold the information. */
      wcsloc = NULL;
      datFind( dcb->loc, "WCS", &wcsloc, status );
      datNew1C( wcsloc, "DATA", NDF__SZAST, NDF__INAST, status );

/* Obtain a locator to the DATA component, storing this in the DCB and
   annul the WCS structure locator. */
      NDF__DCB_LOCK_ASTMUTEX;
      Ndf_DCB_astlc = NULL;
      datFind( wcsloc, "DATA", &Ndf_DCB_astlc, status );
      datAnnul( &wcsloc, status );

/* Map the new object for write access and store the resulting pointer
   in the DCB. */
      dim = NDF__INAST;
      datMap( Ndf_DCB_astlc, "_CHAR", "WRITE", 1, &dim,
              (void **) &Ndf_DCB_astpt, status );

/* Create an AST_ Channel to write the supplied data to the new
   component.  Supply the "ndf1Wrast" function as the "sink" function for
   storing the data, and specify that only essential information be
   included. */
      if( *status == SAI__OK ) {
         chan = astChannel( NULL, ndf1Wrast, "Full=-1,Comment=0" );

/* Initialise the zero-based index of the first element in the _CHAR array
   to be used by the sink function. */
         Ndf_DCB_astln = 0;

/* Write the copy of the supplied AST_ object to the Channel, thus
   transferring the data to the HDS component. */
         (void) astWrite( chan, dcb->iwcs );

/* If an error occurred during data transfer, report a contextual error
   message. */
         if( *status != SAI__OK ) {
            datMsg( "OBJECT", Ndf_DCB_astlc );
            errRep( " ", "Error while writing AST_ data to the HDS object "
                    "^OBJECT.", status );
         }

/* Annul the Channel pointer, thus deleting the Channel. */
         chan = astAnnul( chan );
      }

/* Unmap the DATA component. */
      datUnmap( Ndf_DCB_astlc, status );

/* Obtain the number of elements used in the component and alter its
   size to eliminate the unused elements. */
      dim = Ndf_DCB_astln;
      datAlter( Ndf_DCB_astlc, 1, &dim, status );

/* Annul the DATA component locator. */
      datAnnul( &Ndf_DCB_astlc, status );
      NDF__DCB_UNLOCK_ASTMUTEX;

/* If an error occurred, clean up by erasing any new HDS component that
   was created. */
      if( *status != SAI__OK ) {
         errBegin( status );
         datErase( dcb->loc, "WCS", status );
         errEnd( status );

/* Also annul the pointer to any WCS information held in the DCB. */
         dcb->iwcs = astAnnul( dcb->iwcs );
      }

/* Note whether WCS informatiomn is up to date in the DCB. */
      dcb->kw = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wwrt", status );

}

