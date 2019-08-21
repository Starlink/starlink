#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"
#include "ndf_ast.h"

void ndf1Da( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Da

*  Purpose:
*     Ensure that axis structure information is available for a data
*     object.

*  Synopsis:
*     void ndf1Da( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that axis structure information (including HDS
*     locators to elements of the axis structure array) is available in the
*     DCB for an NDF identified by its DCB entry. If this information is
*     already available, then the function returns without action.
*     Otherwise, it examines the actual data object to obtain this
*     information. Only those checks on the data object"s validity which
*     are necessary to obtain this information are performed.

*  Parameters:
*     dcb
*        Pointer to the data object entry in the DCB.
*     *status
*        The global status.

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
   HDSLoc *aloc = NULL;  /* Locator to axis structure array */
   HDSLoc *locv = NULL;  /* Axis variant component locator */
   char type[ DAT__SZTYP + 1 ];    /* HDS type string */
   hdsbool_t there;      /* Whether axis structure is present */
   hdsdim cell;          /* Cell indices for axis structure */
   hdsdim dima[ DAT__MXDIM ];      /* Dimension sizes of axis structure */
   hdsdim dimv[ DAT__MXDIM ];      /* Dimensions of variant component */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */
   int ndima;            /* Number of axis structure dimensions */
   int ndimv;            /* No. of variant component dimensions */
   char *pntr;           /* Pointer to mapped Variant string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check whether axis structure information is already available. There
   is nothing to do if it is. */
   if( !dcb->ka ) {

/* Ensure that information about the NDF's data array is available for
   the DCB slot. */
      ndf1Dd( dcb, status );

/* Obtain the NDF bounds from the ARY_ system identifier for the data
   array. */
      aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* See if an axis component is present in the NDF. */
      datThere( dcb->loc, "AXIS", &there, status );
      if( *status == SAI__OK ) {

/* Set initial null values for the axis element locators. */
         for( iax = 0; iax < NDF__MXDIM; iax++ ){
            dcb->aloc[ iax ] = NULL;
         }

/* If an axis component is present, then obtain a locator to it and
   determine its type and shape. */
         if( there ) {
            datFind( dcb->loc, "AXIS", &aloc, status );
            datType( aloc, type, status );
            datShape( aloc, DAT__MXDIM, dima, &ndima, status );
            if( *status == SAI__OK ) {

/* Check that its type is "AXIS". Report an error if it is not. */
               if( strcmp( type, "AXIS" ) ) {
                  *status = NDF__TYPIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The AXIS component in the NDF structure "
                          "^NDF has an invalid type of '^BADTYPE'; it "
                          "should be of type 'AXIS'.", status );

/* Check it is 1-dimensional. Report an error if it it not. */
               } else if( ndima != 1 ) {
                  *status = NDF__NDMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndima );
                  errRep( " ", "The AXIS component in the NDF structure "
                          "^NDF is ^BADNDIM-dimensional; it should be "
                          "1-dimensional.", status );

/* Check its dimension size matches the NDF's dimensionality. Report an
   error if it does not. */
               } else if( dima[ 0 ] != ndim ) {
                  *status = NDF__DIMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADDIM", dima[ 0 ] );
                  msgSeti( "NDIM", ndim );
                  errRep( " ", "The AXIS component in the NDF structure "
                          "^NDF has ^BADDIM element(s); this number should "
                          "match the number of NDF dimensions (^NDIM).",
                          status );
               }
            }

/* Obtain a locator to each element of the axis structure array for
   storage in the DCB. */
            if( *status == SAI__OK ) {
               for( iax = 0; iax < ndim; iax++ ){
                  cell = iax + 1;
                  datCell( aloc, 1, &cell, dcb->aloc + iax, status );

/* See if the axis structure element contains a VARIANT component. */
                  datThere( dcb->aloc[ iax ], "VARIANT", &there, status );
                  if( *status == SAI__OK ) {

/* If so, then obtain its type and shape. */
                     if( there ) {
                        datFind( dcb->aloc[ iax ], "VARIANT", &locv, status );
                        datType( locv, type, status );
                        datShape( locv, DAT__MXDIM, dimv, &ndimv, status );
                        if( *status == SAI__OK ) {

/* Check that it is a character object. Report an error if it is not. */
                           if( strncmp( type, "_CHAR*", 6 ) ) {
                              *status = NDF__TYPIN;
                              datMsg( "AXIS", dcb->aloc[ iax ] );
                              msgSetc( "BADTYPE", type );
                              errRep( " ", "The VARIANT component in the "
                                      "NDF axis structure ^AXIS has an "
                                      "invalid data type of '^BADTYPE'; it "
                                      "should be of type '_CHAR'.", status );

/* Check that it is scalar. Report an error if it is not. */
                           } else if( ndimv != 0 ) {
                              *status = NDF__NDMIN;
                              datMsg( "AXIS", dcb->aloc[ iax ] );
                              msgSeti( "BADNDIM", ndimv );
                              errRep( " ", "The VARIANT component in the "
                                      "NDF axis structure ^AXIS is "
                                      "^BADNDIM-dimensional; it should be "
                                      "scalar.", status );
                           }
                        }

/* Map the VARIANT component. */
                        pntr = ndf1Hmp0C( locv, status );
                        if( pntr ) {

/* Test its value. Report an error if it is not "SIMPLE". */
                           if( !astChrMatch( pntr, "SIMPLE" ) ) {
                              *status = NDF__VARIN;
                              datMsg( "AXIS", dcb->aloc[ iax ] );
                              msgSetc( "BADVAR", pntr );
                              errRep( " ", "The VARIANT component in the "
                                      "NDF axis structure ^AXIS has an "
                                      "invalid value of '^BADVAR'; only "
                                      "the value 'SIMPLE' is defined.", status );
                           }

/* Free the memory holding the null-terminated copy */
                           pntr = astFree( pntr );
                        }

/* Annul the locator to the VARIANT component. */
                        datAnnul( &locv, status );
                     }
                  }

/* Quit considering axis structure elements if an error occurs. */
                  if( *status != SAI__OK ) break;
               }
            }

/* Annul the locator to the axis structure array itself. */
            datAnnul( &aloc, status );

/* If an error occurred, then annul any locators which may have been
   allocated. */
            if( *status != SAI__OK ) {
               for( iax = 0; iax < ndim; iax++ ){
                  datAnnul( dcb->aloc + iax, status );
               }
            }
         }
      }

/* Note whether axis structure information is now available in the DCB. */
      dcb->ka = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Da", status );

}
