#include "sae_par.h"
#include "dat_par.h"
#include "ary.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>

void ndfSbnd_( int ndim, const hdsdim lbnd[], const hdsdim ubnd[], int indf,
              int *status ){
/*
*+
*  Name:
*     ndfSbnd

*  Purpose:
*     Set new pixel-index bounds for an NDF.

*  Synopsis:
*     void ndfSbnd( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
*                   int indf, int *status )

*  Description:
*     This function sets new pixel-index bounds for an NDF (or NDF
*     section). The number of NDF dimensions may also be changed. If a base
*     NDF is specified, then a permanent change is made to the actual data
*     object and this will be apparent through any other NDF identifiers
*     which refer to it. However, if an identifier for an NDF section is
*     specified, then its bounds are altered without affecting other
*     identifiers.

*  Parameters:
*     ndim
*        New number of NDF dimensions.
*     lbnd
*        New lower pixel-index bounds of the NDF.
*     ubnd
*        New upper pixel-index bounds of the NDF.
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  The bounds of an NDF section cannot be altered while any of its
*     array components (or any of its axis arrays) is mapped for access
*     through the identifier supplied to this function.
*     -  The bounds of a base NDF cannot be altered while any part of any
*     of its array components (or any of its axis arrays) is mapped for
*     access, even through another identifier.
*     -  The pixel values of any defined NDF array component will be
*     retained if those pixels lie within both the old and new bounds. Any
*     pixels lying outside the new bounds will be lost and cannot later be
*     recovered by further changes to the NDF's bounds. Any new pixels
*     introduced where the new bounds extend beyond the old ones will be
*     assigned the "bad" value, and subsequent enquiries about the presence
*     of bad pixels will reflect this.
*     -  If the new NDF bounds extend beyond the bounds of the associated
*     base NDF and any of the NDF's axis arrays have defined values, then
*     these values will be extrapolated as necessary.
*     -  If the bounds of a base NDF are to be altered and retention of the
*     pixel values of any of its components is not required, then a call to
*     ndfReset should be made before calling this function. This will
*     eliminate any unnecessary processing which might be needed to retain
*     the existing values. This step is not necessary with an NDF section,
*     as no processing of pixel values takes place in this case.

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
   AstFrameSet *iwcs;    /* AST_ pointer to WCS information */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char form[ ARY__SZFRM + 1 ];    /* Array storage form */
   int iax;              /* Loop counter for dimensions */
   int mapped;           /* Is an NDF component mapped? */
   int ndimi;            /* Initial number of NDF dimensions */
   int there;            /* Whether component exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check the new bounds for validity. */
   ndf1Vbnd( ndim, lbnd, ubnd, status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that BOUNDS access to the NDF is available. */
      ndf1Chacc( acb, "BOUNDS", status );

/* Determine the initial number of NDF dimensions from the ARY_ system
   identifier for the data array, held in the ACB. */
      aryNdim( acb->did, &ndimi, status );

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that quality and variance information is available in the DCB
   and ACB (note that this must be done at the start because once one
   component has had new bounds set, the bounds of subsequent
   components will no longer match it, so an error would result due to
   the checks which take place when information is obtained about these
   subsequent components). */
      ndf1Qimp( acb, status );
      ndf1Vimp( acb, status );

/* Similarly, ensure that information is available about all of the
   NDF's axis array components. */
      if( *status == SAI__OK ) {
         for( iax = 0; iax < ndimi; iax++ ){
            ndf1Dad( iax, dcb, status );
            ndf1Dav( iax, dcb, status );
            ndf1Daw( iax, dcb, status );
         }
      }
      if( *status == SAI__OK ) {

/* Check that none of the NDF's components is mapped for access through
   the current ACB entry. */
         mapped = ( acb->dmap || acb->qmap || acb->vmap );

/* Check all the axis arrays for current mappings as well if necessary. */
         if( !mapped ) {
            for( iax = 0; iax < ndimi; iax++ ){
               if( acb->admap[ iax ] || acb->avmap[ iax ] || acb->awmap[ iax ] ) {
                  mapped = 1;
                  break;
               }
            }
         }

/* If any component is mapped, then report an error. */
         if( mapped ) {
            *status = NDF__ISMAP;
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The NDF structure ^NDF is already mapped for "
                    "access through the specified identifier (possible "
                    "programming error).", status );

/* If this is a base NDF, then also check that no part of any component
   is mapped. Report an error if it is. */
         } else if( ( !acb->cut ) && ( dcb->nmap != 0 ) ) {
            *status = NDF__ISMAP;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "The NDF structure ^NDF is already mapped for "
                    "access through another identifier (possible "
                    "programming error).", status );
         }
      }

/* WCS component:
   ==============
   We first obtain the new WCS information which will apply to the
   modified NDF, since this depends on the AXIS component and the NDF"s
   main data array having their original shape. Initialise the AST
   pointer to this information. */
      iwcs = NULL;

/* We can omit this stage if the NDF is a section, since the WCS
   information does not then need changing. Also check if the WCS
   component exists and do nothing if it does not. */
      if( !acb->cut ) {
         ndf1Wsta( acb, &there, status );
         if( *status == SAI__OK ) {
            if( there ) {

/* If we have a base NDF with WCS information present, then obtain a
   copy of the WCS information which will apply to the NDF once its
   bounds have been changed. */
               ndf1Wsbnd( ndim, lbnd, ubnd, acb, &iwcs, status );
            }
         }
      }

/* AXIS component:
   ==============
   The bounds of the axis component must be handled next, since
   extrapolation of axis array values depends on the NDF's main data
   array having its original shape. */
      ndf1Asbnd( ndim, lbnd, ubnd, acb, status );

/* DATA component:
   ==============
   Set the new bounds for the data component. */
      arySbnd( ndim, lbnd, ubnd, acb->did, status );
      ndf1Cmpac( acb->dcb, "DATA", status );

/* QUALITY component:
   =================
   See if the ARY_ system identifier for the quality component is valid.
   If not, then the component is not defined. */
      there = aryValid( acb->qid, status );
      if( *status == SAI__OK ) {

/* If it is defined, then set new bounds for it. */
         if( there ) {
            arySbnd( ndim, lbnd, ubnd, acb->qid, status );
            ndf1Cmpac( acb->dcb, "QUALITY", status );

/* Since this may have caused the array's bad pixel flag to be set, a
   value of zero must be re-established. This is not done if the
   array has a primitive storage form. Otherwise, the zero value is
   set via the base array in the DCB (since it will not affect the
   actual data object if set via an array section). */
            aryForm( acb->qid, form, status );
            if( *status == SAI__OK ) {
               if( strcmp( form, "PRIMITIVE" ) ) arySbad( 0, dcb->qid, status );
            }

/* If the array is not defined, and this is a base NDF, then convert its
   default storage form to take account of the new bounds, if necessary. */
         } else if( !acb->cut ) {
            ndf1Cbfrm( ndim, lbnd, ubnd, dcb->qfrm, sizeof( dcb->qfrm ),
                       status );
         }
      }

/* VARIANCE component:
   ==================
   See if the ARY_ system identifier for the variance component is
   valid. If not, then the component is not defined. */
      there = aryValid( acb->vid, status );
      if( *status == SAI__OK ) {

/* If it is defined, then set new bounds for it. */
         if( there ) {
            arySbnd( ndim, lbnd, ubnd, acb->vid, status );
            ndf1Cmpac( acb->dcb, "VARIANCE", status );

/* If the array is not defined, and this is a base NDF, then convert its
   default storage form to take account of the new bounds, if necessary. */
         } else if( !acb->cut ) {
            ndf1Cbfrm( ndim, lbnd, ubnd, dcb->vfrm, sizeof( dcb->vfrm ),
                       status );
         }
      }

/* WCS component:
   =============
   We now return to the WCS component and write the modified WCS
   information back to the NDF, if necessary, to over-write the
   original information. */
      if( iwcs ) {
         ndf1Wrwcs( iwcs, acb, status );

/* Annul the AST_ pointer to the WCS information. */
         iwcs = astAnnul( iwcs );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfSbnd: Error setting new pixel-index bounds for an "
              "NDF.", status );
      ndf1Trace( "ndfSbnd", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

