#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"
#include "star/util.h"

void ndf1Dq( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dq

*  Purpose:
*     Ensure that quality information is available in the DCB.

*  Synopsis:
*     void ndf1Dq( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about a data object"s quality
*     component is available in the DCB. It does nothing if this
*     information is already available. Otherwise, it obtains this
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which quality information is
*        required.
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
   HDSLoc *locbb = NULL; /* Locator to BADBITS object */
   HDSLoc *locv = NULL;  /* Locator to VARIANT component */
   char ftypeq[ NDF__SZFTP + 1 ];  /* Full type of the quality array */
   char type[ DAT__SZTYP + 1 ];    /* HDS data type string */
   hdsbool_t there;      /* Whether quality component exists */
   hdsdim dim[ NDF__MXDIM ];       /* HDS object dimensions */
   hdsdim lbndd[ NDF__MXDIM ];     /* Data component lower bounds */
   hdsdim lbndq[ NDF__MXDIM ];     /* Quality component lower bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Data component upper bounds */
   hdsdim ubndq[ NDF__MXDIM ];     /* Quality component upper bounds */
   int i;                /* Loop counter for dimensions */
   int ndim;             /* Number of HDS object dimensions */
   int ndimd;            /* Number of data component dimensions */
   int ndimq;            /* Number of quality dimensions */
   char *pntr;           /* Pointer to mapped VARIANT value */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if quality information is already available. There is nothing to
   do if it is. */
   if( !dcb->kq ) {

/* Ensure that information about the data array is available in the DCB. */
      ndf1Dd( dcb, status );

/* See if the QUALITY component is present. If not, then signify this
   by storing null quality values in the DCB. */
      datThere( dcb->loc, "QUALITY", &there, status );
      if( *status == SAI__OK ) {
         dcb->qloc = NULL;
         dcb->qid = NULL;

/* If it is present, then obtain a locator to it, storing this in the
   DCB. */
         if( there ) {
            datFind( dcb->loc, "QUALITY", &dcb->qloc, status );

/* Obtain the type and shape of the QUALITY component. */
            datType( dcb->qloc, type, status );
            datShape( dcb->qloc, DAT__MXDIM, dim, &ndim, status );
            if( *status == SAI__OK ) {

/* Check that the type is "QUALITY". Report an error if it is not. */
               if( strcmp( type, "QUALITY" ) ) {
                  *status = NDF__TYPIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The QUALITY component in the NDF structure "
                          "^NDF has an invalid data type of '^BADTYPE'; it "
                          "should be of type 'QUALITY'.", status );

/* Check that the component is scalar. Report an error if it is not. */
               } else if( ndim != 0 ) {
                  *status = NDF__NDMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndim );
                  errRep( " ", "The QUALITY component in the NDF structure "
                          "^NDF is ^BADNDIM-dimensional; it should be "
                          "scalar.", status );
               }
            }

/* See if the VARIANT component of the QUALITY structure is present. */
            datThere( dcb->qloc, "VARIANT", &there, status );
            if( *status == SAI__OK ) {

/* If so, then obtain a locator to it and determine its type and shape. */
               if( there ) {
                  datFind( dcb->qloc, "VARIANT", &locv, status );
                  datType( locv, type, status );
                  datShape( locv, DAT__MXDIM, dim, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that it is a character object. Report an error if it is not. */
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The VARIANT component in the NDF "
                                "quality structure ^QUAL has an invalid "
                                "data type of '^BADTYPE'; it should be of "
                                "type '_CHAR'.", status );

/* Check that it is scalar. Report an error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The VARIANT component in the NDF "
                                "quality structure ^QUAL is "
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
                        datMsg( "QUAL", dcb->qloc );
                        msgSetc( "BADVAR", pntr );
                        errRep( " ", "The VARIANT component in the NDF "
                                "quality structure ^QUAL has an invalid "
                                "value of '^BADVAR'; only the value "
                                "'SIMPLE' is defined.", status );
                     }
                     pntr = astFree( pntr );
                  }

/* Annul the locator to the VARIANT component. */
                  datAnnul( &locv, status );
               }
            }

/* See if there is a BADBITS component in the QUALITY structure. */
            datThere( dcb->qloc, "BADBITS", &there, status );
            if( *status == SAI__OK ) {

/* If not, then the badbits value defaults to zero.  Otherwise, obtain
   a locator to the BADBITS component and determine its type and shape. */
               if( there ) {
                  datFind( dcb->qloc, "BADBITS", &locbb, status );
                  datType( locbb, type, status );
                  datShape( locbb, DAT__MXDIM, dim, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check it is of type "_UBYTE". Report an error if it is not. */
                     if( strcmp( type, "_UBYTE" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The BADBITS component in the NDF "
                                "quality structure ^QUAL has an invalid "
                                "data type of '^BADTYPE'; it should be of "
                                "type '_UBYTE'.", status );

/* Check it is scalar. Report an error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The BADBITS component in the NDF "
                                "quality structure ^QUAL is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Read the badbits value and store it in the DCB. */
                  dim[ 0 ] = 0;
                  datGet( locbb, "_UBYTE", 0, dim, &dcb->qbb, status );

/* Annul the BADBITS locator. */
                  datAnnul( &locbb, status );
               }
            }

/* See if the QUALITY array is present in the QUALITY structure. Report
   an error if it is not. */
            datThere( dcb->qloc, "QUALITY", &there, status );
            if( *status == SAI__OK ) {
               if( !there ) {
                  *status = NDF__NOQLY;
                  datMsg( "QUAL", dcb->qloc );
                  errRep( " ", "The QUALITY array is missing from the NDF "
                          "quality structure ^QUAL", status );
               } else {

/* Import the QUALITY array component (of the QUALITY structure) into
   the ARY_ system, storing the resulting identifier in the DCB. */
                  aryFind( dcb->qloc, "QUALITY", &dcb->qid, status );

/* Obtain the number of dimensions and the pixel index bounds of the
   NDF's data array and quality array. Also determine the full data
   type of the quality array. */
                  aryBound( dcb->did, NDF__MXDIM, lbndd, ubndd, &ndimd, status );
                  aryBound( dcb->qid, NDF__MXDIM, lbndq, ubndq, &ndimq, status );
                  aryFtype( dcb->qid, ftypeq, status );
                  if( *status == SAI__OK ) {

/* Report an error if the number of quality dimensions does not match
   that of the data array. */
                     if( ndimq != ndimd ) {
                        *status = NDF__NDMIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSeti( "BADNDIM", ndimq );
                        msgSeti( "NDIM", ndimd );
                        errRep( " ", "The QUALITY array in the NDF quality "
                                "structure ^QUAL has an invalid number of "
                                "dimensions (^BADNDIM); it should be "
                                "^NDIM-dimensional.", status );

/* Check that the array has a data type of "_UBYTE" and report an error
   if it does not. */
                     } else if( strcmp( ftypeq, "_UBYTE" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "QUAL", dcb->qloc );
                        msgSetc( "BADTYPE", ftypeq );
                        errRep( " ", "The QUALITY array in the NDF quality "
                                "structure ^QUAL has an invalid type of "
                                "'^BADTYPE'; it should be of type "
                                "'_UBYTE'.", status );

/* Check that the quality array pixel index bounds in each dimension
   match those of the data array. */
                     } else {
                        for( i = 0; i < ndimd; i++ ){
                           if( ( lbndq[ i ] != lbndd[ i ] ) ||
                               ( ubndq[ i ] != ubndd[ i ] ) ) {

/* Report an error if a discrepancy is found. */
                              *status = NDF__BNDIN;
                              msgSeti( "DIM", i + 1 );
                              datMsg( "QUAL", dcb->qloc );
                              errRep( " ", "The pixel-index bounds of "
                                      "dimension ^DIM of the NDF quality "
                                      "structure ^QUAL do not match those "
                                      "of the NDF's DATA_ARRAY component.",
                                      status );
                              break;
                           }
                        }
                     }
                  }
               }
            }
         }

/* Set the default storage form of the quality component using the
   values initially derived from the NDF's data array component. */
         star_strlcpy( dcb->qfrm, dcb->defrm, sizeof( dcb->qfrm ) );

/* If the component is not suitable, then annul any identifiers and
   locators which may have been allocated. */
         if( *status != SAI__OK ) {
            aryAnnul( &dcb->qid, status );
            datAnnul( &dcb->qloc, status );
         }
      }

/* Note whether quality information is now available in the DCB. */
      dcb->kq = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dq", status );

}
