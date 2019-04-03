#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "ary_err.h"
#include "ndf_ast.h"
#include "cnf.h"
#include "mers.h"

void ndf1Danl( int dispos, NdfDCB **dcb, int *status ){
/*
*+
*  Name:
*     ndf1Danl

*  Purpose:
*     Perform an "annul" operation on a data object.

*  Synopsis:
*     void ndf1Danl( int dispos, NdfDCB **dcb, int *status )

*  Description:
*     This function performs an "annul" operation on a DCB entry and
*     optionally disposes of the associated data object. This operation is
*     normally required when an ACB entry is annulled.  The reference count
*     for the data object is decremented and if this is still non-zero,
*     then no further action is taken. However, if the reference count
*     reaches zero, then all locators and identifiers contained in the DCB
*     entry are disposed of (thereby removing any reference to the data
*     object) and the DCB entry is released. If the "dispos" parameter is
*     set to non-zero, the data object will also be disposed of according
*     to the disposal mode specified in the DCB (it is either kept or
*     deleted). If the reference count reaches zero and the "dispos"
*     parameter is zero, then the DCB entry is released, but the data
*     object is not disposed of.

*  Parameters:
*     dispos
*        Whether to dispose of the data object. A value of zero indicates
*        that the data object will remain in use by the NDF_ system; the
*        intention being simply to release the specified DCB entry.
*     *dcb
*        Pointer to the DCB entry to be anulled. If the data object
*        reference count falls to zero, then the DCB entry will be released
*        and a value of zero will be returned for this parameter (if the
*        "dispos" parameter is set to non-zero, the data object will also
*        be disposed of). Otherwise this parameter will be unchanged on
*        exit.
*     *status
*        The global status.

*  Notes:
*     This function attempts to execute even if "status" is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

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
   hdsdim lbnd[ NDF__MXDIM ];      /* Lower bounds of data object */
   hdsdim ubnd[ NDF__MXDIM ];      /* Upper bounds of data object */
   int del;              /* Whether data object is to be deleted */
   int iax;              /* Loop counter for axes */
   int iccomp;           /* Loop counter for character components */
   int ndim;             /* Number of data object dimensions */
   double *ipw1;         /* Work space */
   int *ipw2;            /* Work space */

/* Begin a new error reporting environment. */
   errBegin( status );

/* Decrement the data object reference count. */
   *status = SAI__OK;
   (*dcb)->refct--;

/* If the reference count falls to zero, then the DCB entry must be
   released. */
   if( (*dcb)->refct <= 0 ) {

/* Assign the name of the data file to the MSG token "NDF_EVENT" */
      ndf1Evmsg( "NDF_EVENT", *dcb );

/* Raise an NDF event, describing the closing of a new NDF. */
      ndf1Event( "CLOSE_NDF", status );

/* Ensure that data array information is available in the DCB and derive
   the data object bounds and number of dimensions from the ARY_ system
   identifier for the data array. */
      ndf1Dd( *dcb, status );
      aryBound( (*dcb)->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
      if( *status == SAI__OK ) {

/* See if the data object is to be deleted, i.e. if it is being
   disposed of with a disposal mode other than "KEEP". */
         del = ( dispos && strcmp( (*dcb)->dsp, "KEEP" ) );

/* EXTENSION component.
   ===================
   If an extension (MORE) component locator has been acquired, then
   annul it. */
         if( (*dcb)->kx ) {
            if( (*dcb)->xloc ) datAnnul( &(*dcb)->xloc, status );
            (*dcb)->kx = 0;
         }

/* Character components.
   ====================
   If any character component locators have been acquired, then annul
   them (check each in turn). */
         for( iccomp = 0; iccomp < NDF__MXCCN; iccomp++ ){
            if( (*dcb)->kc[ iccomp ] ) {
               if( (*dcb)->cloc[ iccomp ] ) datAnnul( (*dcb)->cloc + iccomp,
                                                            status );
               (*dcb)->kc[ iccomp ] = 0;
            }
         }

/* DATA component.
   ==============
   Dispose of the data array identifier by deletion if required. */
         if( del ) {
            aryDelet( &(*dcb)->did, status );

/* If the array identifier is being annulled, then open a new error
   context to hold any errors which may result. */
         } else {
            errMark();
            aryAnnul( &(*dcb)->did, status );

/* If the ARY_ system complains that the array is being released in an
   undefined state, then annul the error and make a new error report
   appropriate to the NDF_ system. */
            if( *status == ARY__UNDEF ) {
               errAnnul( status );
               *status = NDF__DUDEF;
               ndf1Dmsg( "NDF", *dcb );
               errRep( " ", "The NDF structure ^NDF has been released from "
                       "the NDF_ system with its data component in an "
                       "undefined state (possible programming error).", status );
            }

/* End the error context. */
            errRlse();
         }

/* Note that DCB data array information is no longer available. */
         (*dcb)->kd = 0;

/* QUALITY component.
   ==================
   Annul the quality component of the data object. */
         ndf1Dqanl( *dcb, del, status );

/* VARIANCE component.
   ==================
   Annul the variance component of the data object. */
         ndf1Dvanl( *dcb, del, status );

/* AXIS component.
   ==============
   If axis structure locators have been acquired, then loop to dispose
   of locators and identifiers for each axis in turn. Obtain the number
   of axes from the number of NDF data array dimensions. */
         if( (*dcb)->ka ) {
            for( iax = 0; iax < ndim; iax++ ){

/* Annul the individual axis structure locators. */
               if( (*dcb)->aloc[ iax ] ) datAnnul( (*dcb)->aloc + iax, status );

/* Axis extensions.
   ===============
   If axis extension locators have been acquired, then annul them. */
               if( (*dcb)->kax[ iax ] ) {
                  if( (*dcb)->axloc[ iax ] ) datAnnul( (*dcb)->axloc + iax,
                                                             status );
                  (*dcb)->kax[ iax ] = 0;
               }

/* Axis character components.
   =========================
   If axis character component locators have been acquired, then annul
   them. */
               for( iccomp = 0; iccomp < NDF__MXACN; iccomp++ ){
                  if( (*dcb)->kac[ iax ][ iccomp ] ) {
                     if( (*dcb)->acloc[ iax ][ iccomp ] ) {
                        datAnnul( (*dcb)->acloc[ iax ] + iccomp,
                                  status );
                     }
                     (*dcb)->kac[ iax ][ iccomp ] = 0;
                  }
               }

/* Axis data arrays.
   ================
   If ARY_ system identifiers for the axis data arrays have been
   acquired, then dispose of them. */
               if( (*dcb)->kad[ iax ] ) {
                  if( (*dcb)->adid[ iax ] ) {
                     if( del ) {
                        aryDelet( (*dcb)->adid + iax, status );
                     } else {
                        aryAnnul( (*dcb)->adid + iax, status );
                     }
                  }
                  (*dcb)->kad[ iax ] = 0;
               }

/* Axis variance arrays.
   ====================
   If ARY_ system identifiers for axis variance arrays have been
   acquired, then dispose of them. */
               if( (*dcb)->kav[ iax ] ) {
                  if( (*dcb)->avid[ iax ] ) {
                     if( del ) {
                        aryDelet( (*dcb)->avid + iax, status );
                     } else {
                        aryAnnul( (*dcb)->avid + iax, status );
                     }
                  }
                  (*dcb)->kav[ iax ] = 0;
               }

/* Axis width arrays.
   =================
   If ARY_ system identifiers for axis width arrays have been acquired,
   then dispose of them. */
               if( (*dcb)->kaw[ iax ] ) {
                  if( (*dcb)->awid[ iax ] ) {
                     if( del ) {
                        aryDelet( (*dcb)->awid + iax, status );
                     } else {
                        aryAnnul( (*dcb)->awid + iax, status );
                     }
                  }
                  (*dcb)->kaw[ iax ] = 0;
               }
            }

/* Note that DCB axis structure information is no longer available. */
            (*dcb)->ka = 0;
         }

/* HISTORY component.
   =================
   If the data object is being disposed of without deletion, then
   ensure that a default history record is written to it, if required. */
         if( dispos && ( !del ) ) {
            errBegin( status );
            ndf1Hwdef( *dcb, " ", status );
            errEnd( status );

/* Dump any logged error message information to the history record. */
            ndf1Hderr( *dcb, 1, status );
         }

/* If there is any chance that the history records are not in chronological
   order, sort them. */
         errBegin( status );
         if( (*dcb)->hsort ) {
            ipw1 = astMalloc( (*dcb)->hnrec*sizeof(*ipw1) );
            ipw2 = astMalloc( (*dcb)->hnrec*sizeof(*ipw2) );
            ndf1Hsrt( *dcb, (*dcb)->hnrec, ipw1, ipw2, status );
            ipw1 = astFree( ipw1 );
            ipw2 = astFree( ipw2 );
            (*dcb)->hsort = 0;
         }
         errEnd( status );

/* If history component locators have been acquired, then annul them. */
         if( (*dcb)->kh ) {
            if( (*dcb)->hloc ) {
               datAnnul( &(*dcb)->hrloc, status );
               datAnnul( &(*dcb)->hloc, status );
            }
            (*dcb)->kh = 0;
         }

/* WCS component.
   ==============
   If an AST_ pointer for WCS information has been acquired, then annul
   it. */
         if( (*dcb)->kw ) {
            if( (*dcb)->iwcs ) (*dcb)->iwcs = astAnnul( (*dcb)->iwcs );
            (*dcb)->kw = 0;
         }
      }

/* Whole data object.
   =================
   Release the NDF data object as a whole, taking account of any
   associated foreign format file. */
      ndf1Clfor( dispos, *dcb, status );

/* Release the DCB slot associated with the data object. */
      *dcb = ndf1Rls( ( NdfObject * ) *dcb, status );
   }

/* Call the error tracing function if appropriate. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Danl", status );

/* End the error reporting environment. */
   errEnd( status );

}

