#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Dw( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dw

*  Purpose:
*     Ensure that WCS information is available in the DCB.

*  Synopsis:
*     void ndf1Dw( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about a data object's WCS
*     (World Coordinate System) component is available in the DCB. It does
*     nothing if this information is already available. Otherwise, it
*     obtains the information by inspecting the actual data object,
*     performing necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which WCS information is required.
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
   AstChannel *chan;     /* Pointer to AST_ Channel */
   AstFrameSet *iwcs;    /* Pointer for original NDF */
   HDSLoc *wcsloc = NULL;/* Locator to WCS structure */
   char type[ DAT__SZTYP + 1 ];    /* HDS component type string */
   hdsbool_t there;      /* HDS component exists? */
   hdsdim dim[ DAT__MXDIM ];       /* HDS object dimensions */
   int ndim;             /* Number of HDS object dimensions */
   size_t clen;          /* Character string length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if WCS information is already available. There is nothing to do
   if it is. */
   if( !dcb->kw ) {

/* Initialise the DCB pointer for the AST_ Object which will contain
   the WCS information. */
      dcb->iwcs = NULL;

/* WCS structure.
   ==============
   See if a WCS component is present in the NDF structure. */
      datThere( dcb->loc, "WCS", &there, status );
      if( *status == SAI__OK ) {

/* If a WCS component is present, obtain a locator for it and determine
   its type and shape. */
         if( there ) {
            wcsloc = NULL;
            datFind( dcb->loc, "WCS", &wcsloc, status );
            datType( wcsloc, type, status );
            datShape( wcsloc, DAT__MXDIM, dim, &ndim, status );

/* Check that the component is of type "WCS" and report an error if it
   is not. */
            if( *status == SAI__OK ) {
               if( strcmp( type, "WCS" ) ) {
                  *status = NDF__TYPIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The WCS component in the NDF structure "
                          "^NDF has an invalid type of '^BADTYPE'; it "
                          "should be of type 'WCS'.", status );

/* Also check that the component is scalar and report an error if it is
   not. */
               } else if( ndim != 0 ) {
                  *status = NDF__NDMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndim );
                  errRep( " ", "The WCS component in the NDF structure "
                          "^NDF is ^BADNDIM-dimensional; it should be "
                          "scalar.", status );
               }
            }

/* DATA component.
   ===============
   See if the WCS structure contains the mandatory DATA component. */
            datThere( wcsloc, "DATA", &there, status );
            if( *status == SAI__OK ) {

/* If it does not, then report an error. */
               if( !there ) {
                  *status = NDF__NOWDT;
                  datMsg( "WCS", wcsloc );
                  errRep( " ", "The DATA component is missing from the NDF "
                          "WCS structure ^WCS", status );

/* Otherwise, obtain a locator for it (storing this locator in the DCB)
   and determine its type and shape. */
               } else {
                  NDF__DCB_LOCK_ASTMUTEX;
                  Ndf_DCB_astlc = NULL;
                  datFind( wcsloc, "DATA", &Ndf_DCB_astlc, status );
                  datType( Ndf_DCB_astlc, type, status );
                  datShape( Ndf_DCB_astlc, DAT__MXDIM, dim, &ndim, status );

/* Check that the DATA component has type _CHAR and report an error if
   it does not. */
                  if( *status == SAI__OK ) {
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "WCS", wcsloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The DATA component in the NDF WCS "
                                "structure ^WCS has an invalid type of "
                                "'^BADTYPE'; it should be of type "
                                "'_CHAR'.", status );

/* Check that the DATA component is 1-dimensional and report an error
   if it is not. */
                     } else if( ndim != 1 ) {
                        *status = NDF__NDMIN;
                        datMsg( "WCS", wcsloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The DATA component in the NDF WCS "
                                "structure ^WCS is ^BADNDIM-dimensional; "
                                "it should be 1-dimensional.", status );

                     }
                  }

/* Determine the DATA component's character string length and check
   that it is not too short to hold AST_ data. Report an error if it
   is. */
                  datClen( Ndf_DCB_astlc, &clen, status );
                  if( *status == SAI__OK ) {
                     if( clen < NDF__MLAST ) {
                        *status = NDF__WCDTS;
                        datMsg( "WCS", wcsloc );
                        msgSeti( "CLEN", clen );
                        msgSeti( "MINLEN", NDF__MLAST );
                        errRep( " ", "The DATA component in the NDF WCS "
                                "structure ^WCS has a character string "
                                "length of ^CLEN; it should have a length "
                                "of at least ^MINLEN.", status );
                     }
                  }

/* Map the DATA component for READ access and store the resulting
   pointer in the DCB. */
                  datMap( Ndf_DCB_astlc, "_CHAR", "READ", ndim, dim,
                          (void **) &Ndf_DCB_astpt, status );

/* Create an AST_ Channel to read from the DATA component. Supply the
   "ndf1Rdast" function as the "source" function for extracting the data. */
                  if( *status == SAI__OK ) {
                     chan = astChannel( ndf1Rdast, NULL, " " );

/* Initialise the index of the first element in the _CHAR array to be
   used by the source function. */
                     Ndf_DCB_astln = 0;

/* Read an Object from the Channel, thus transferring the data, and
   store the resulting AST_ pointer in the DCB. Exempt this pointer
   from AST_ context handling (so it is not annulled if "astEnd" is
   called). */
                     iwcs = astRead( chan );

/* If an error occurred during data transfer, report a contextual error
   message. */
                     if( *status != SAI__OK || !iwcs ) {
                        if( *status == SAI__OK ) *status = NDF__WCSIN;
                        datMsg( "OBJECT", Ndf_DCB_astlc );
                        errRep( " ", "Error while reading AST_ data from "
                                "the HDS object ^OBJECT.", status );
                     }

/* Validate the FrameSet and ensure it contains all the standard dummy
   Frames expected by the NDF library. */
                     ndf1Vwcs( NULL, iwcs, &dcb->iwcs, status );

/* Annul the original FrameSet pointer. */
                     iwcs = astAnnul( iwcs );

/* Exempt this pointer from AST_ context handling (so it is not annulled if
   "astEnd" is called). */
                     astExempt( dcb->iwcs );

/* Annul the Channel pointer, thus deleting the Channel. */
                     chan = astAnnul( chan );
                  }

/* Annul the DATA component locator. */
                  datAnnul( &Ndf_DCB_astlc, status );
                  NDF__DCB_UNLOCK_ASTMUTEX;
               }
            }

/* Annul the WCS structure locator. */
            datAnnul( &wcsloc, status );
         }
      }

/* If an error occurred, annul any DCB pointer that may have been
   allocated. */
      if( *status != SAI__OK ) dcb->iwcs = astAnnul( dcb->iwcs );

/* Note whether WCS information is now available in the DCB. */
      dcb->kw = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dw", status );

}

