#include "star/util.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"
#include "ndf_ast.h"

void ndf1Prp( NdfACB *acb1, int nextn, char extn[][ DAT__SZNAM + 1 ],
              const int cpf[], NdfPCB *pcb, NdfACB **acb2, int *status ){
/*
*+
*  Name:
*     ndf1Prp

*  Purpose:
*     Selectively propagate an NDF's components to form a new NDF.

*  Synopsis:
*     void ndf1Prp( NdfACB *acb1, int nextn, char extn[][ DAT__SZNAM + 1 ],
*                   const int cpf[], NdfPCB *pcb, NdfACB **acb2, int *status )

*  Description:
*     This function selectively propagates the components of an existing
*     NDF (identified by its ACB entry) to form a new data object and
*     creates an ACB entry to describe the resulting new base NDF. The
*     location of the new object is identified by means of an index to a
*     placeholder entry in the PCB. This placeholder should later be
*     annulled.

*  Parameters:
*     acb1
*        Pointer to the ACB entry of the input NDF.
*     nextn
*        Number of extension component names supplied in the "extn" array
*        (may be zero).
*     extn
*        List of extension component names to be omitted from the
*        propagation operation.
*     cpf
*        List of component propagation flags; symbolic constants are
*        defined in the header file "ndf1.h" to identify the elements of
*        this array. The supplied "cpf" array should have at least
*        "NDF__NCPF" elements.
*     pcb
*        Pointer to a placeholder entry in the PCB which specifies the
*        location (and certain properties) of the new NDF.
*     *acb2
*        Pointer to the ACB entry of the new base NDF.
*     *status
*        The global status.

*  Notes:
*     The order in which the NDF components are processed by this function
*     is arbitrary, but is intended to produce an easily understood
*     component order in the output structure.

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
   AryPlace *place;      /* ARY_ placeholder for data array */
   AstFrameSet *iwcs;    /* Pointer to AST_ WCS Object */
   AstFrameSet *iwcsv;   /* Pointer to stripped AST_ WCS Object */
   NdfDCB *dcb1;         /* Pointer to DCB for input data object */
   NdfDCB *dcb2;         /* Pointer to DCB for output data object */
   hdsdim lbnd[ NDF__MXDIM ];      /* Lower input NDF bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* Upper input NDF bounds */
   int ndim;             /* Number of input NDF dimensions */
   int state;            /* Component defined? */
   int valid;            /* Whether array identifier is valid */

/* Set an initial value for the "acb2" parameter. */
   *acb2 = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the DCB entry of the input data object. */
   dcb1 = acb1->dcb;

/* Obtain a free slot in the DCB for the output data object. */
   NDF__DCB_LOCK_MUTEX;
   dcb2 = ndf1Ffs( NDF__DCBTYPE, status );
   NDF__DCB_UNLOCK_MUTEX;
   if( *status == SAI__OK ) {

/* Propagate foreign format information from the input NDF to the
   placeholder and then initialise the new (output) DCB entry with
   information derived from the placeholder. */
      ndf1Prfor( acb1, pcb, status );
      ndf1Pldcb( pcb, dcb2, status );

/* Obtain the input NDF bounds from the ARY_ system identifier for its
   data array, held in the ACB. */
      aryBound( acb1->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* DATA component.
   ==============
   If the DATA component is being propagated, then copy it into the
   DATA_ARRAY component of the new NDF and store an ARY_ system
   identifier for the new array in the new DCB entry. */
      aryPlace( dcb2->loc, "DATA_ARRAY", &place, status );
      if( cpf[ NDF__DCPF ] ) {
         aryCopy( acb1->did, &place, &dcb2->did, status );

/* If no error has occurred, we use ndf1Event to flag a "data array
   read" event. If the caller has registered a handler for this type of
   event (using ndfHndlr), it will be called. */
         if( *status == SAI__OK ) {
            ndf1Evmsg( "NDF_EVENT", dcb1 );
            ndf1Event( "READ_DATA", status );
         }

/* If the DATA component is not being propagated, then create a new
   (undefined) data array with the same attributes, storing an ARY_
   system identifier for it in the new DCB entry. */
      } else {
         aryDupe( acb1->did, &place, &dcb2->did, status );
      }

/* Propagate the default array attributes for the NDF to the new DCB
   entry and update the storage form to take account of the new NDF
   bounds if necessary. */
      star_strlcpy( dcb2->detyp, dcb1->detyp, sizeof( dcb2->detyp ) );
      dcb2->decpx = dcb1->decpx;
      star_strlcpy( dcb2->defrm, dcb1->defrm, sizeof( dcb2->defrm ) );
      ndf1Cbfrm( ndim, lbnd, ubnd, dcb2->defrm, sizeof( dcb2->defrm ), status );

/* Store the storage form of the new data array. */
      aryForm( dcb2->did, dcb2->dfrm, status );

/* Note if the DCB data array information is correct. */
      dcb2->kd = ( *status == SAI__OK );

/* TITLE component.
   ===============
   If the TITLE component is being propagated, then ensure that
   information about it is available in the input DCB entry. */
      if( cpf[ NDF__TCPF ] ) {
         ndf1Dc( dcb1, NDF__TITLE, status );
         if( *status == SAI__OK ) {

/* If a title is present in the input object, then copy it to the
   output object. */
            if( dcb1->cloc[ NDF__TITLE ] ) {
               datCopy( dcb1->cloc[ NDF__TITLE ], dcb2->loc, "TITLE",
                        status );
            }
         }
      }

/* LABEL component.
   ===============
   If the LABEL component is being propagated, then ensure that
   information about it is available in the input DCB entry. */
      if( cpf[ NDF__LCPF ] ) {
         ndf1Dc( dcb1, NDF__LABEL, status );
         if( *status == SAI__OK ) {

/* If a label is present in the input object, then copy it to the
   output object. */
            if( dcb1->cloc[ NDF__LABEL ] ) {
               datCopy( dcb1->cloc[ NDF__LABEL ], dcb2->loc, "LABEL",
                        status );
            }
         }
      }

/* UNITS component.
   ===============
   If the UNITS component is being propagated, then ensure that
   information about it is available in the input DCB entry. */
      if( cpf[ NDF__UCPF ] ) {
         ndf1Dc( dcb1, NDF__UNITS, status );
         if( *status == SAI__OK ) {

/* If a units component is present in the input object, then copy it to
   the output object. */
            if( dcb1->cloc[ NDF__UNITS ] ) {
               datCopy( dcb1->cloc[ NDF__UNITS ], dcb2->loc, "UNITS",
                        status );
            }
         }
      }

/* QUALITY component.
   ==================
   Set initial null values for the new DCB entry's quality structure
   locator and ARY_ system quality array identifier. */
      dcb2->qloc = NULL;
      dcb2->qid = NULL;

/* Propagate the default quality storage form to the output data object
   and convert it to take account of the new NDF bounds if necessary. */
      ndf1Qfrm( acb1, dcb2->qfrm, sizeof( dcb2->qfrm ), status );
      ndf1Cbfrm( ndim, lbnd, ubnd, dcb2->qfrm, sizeof( dcb2->qfrm ), status );
      if( *status == SAI__OK ) {

/* If the quality component is being propagated, then check that the
   old DCB quality structure locator is valid (if not, then there is no
   quality structure in the old data object). If so, then create a new
   quality structure in the new data object and obtain a locator to it
   for storage in the new DCB entry. */
         if( cpf[ NDF__QCPF ] ) {
            if( dcb1->qloc ) {
               datNew( dcb2->loc, "QUALITY", "QUALITY", 0, NULL, status );
               datFind( dcb2->loc, "QUALITY", &dcb2->qloc, status );

/* Copy the old BADBITS component into the new quality structure, if
   available. Also propagate the badbits value to the new DCB entry. */
               ndf1Cpync( dcb1->qloc, "BADBITS", dcb2->qloc, status );
               dcb2->qbb = dcb1->qbb;
            }

/* See if the old ACB ARY_ system identifier for the quality array is
   valid. If not, then the quality array does not exist. */
            valid = aryValid( acb1->qid, status );
            if( *status == SAI__OK ) {

/* If it exists, then copy it into the new quality structure. Store the
   resulting identifier in the new DCB entry. */
               if( valid ) {
                  aryPlace( dcb2->qloc, "QUALITY", &place, status );
                  aryCopy( acb1->qid, &place, &dcb2->qid, status );
               }
            }
         }
      }

/* Note whether DCB quality information is correct. */
      dcb2->kq = ( *status == SAI__OK );

/* VARIANCE component.
   ==================
   Set an initial null ARY_ system identifier for the new DCB entry"s
   variance array. */
      dcb2->vid = NULL;

/* Propagate the default variance attributes to the output data object
   and convert the storage form to take account of the new NDF bounds
   if necessary. */
      ndf1Vtyp( acb1, dcb2->vtyp, sizeof( dcb2->vtyp ), status );
      ndf1Vcpx( acb1, &dcb2->vcpx, status );
      ndf1Vfrm( acb1, dcb2->vfrm, sizeof( dcb2->vfrm ), status );
      ndf1Cbfrm( ndim, lbnd, ubnd, dcb2->vfrm, sizeof( dcb2->vfrm ), status );
      if( *status == SAI__OK ) {

/* If the variance component is being propagated, then see if the old
   ACB ARY_ system identifier for the variance array is valid. If not,
   then the variance array does not exist, so there is nothing more to
   do. */
         if( cpf[ NDF__VCPF ] ) {
            valid = aryValid( acb1->vid, status );
            if( *status == SAI__OK ) {

/* If it exists, then copy it into the VARIANCE component of the new
   data object. Store the resulting identifier in the new DCB entry. */
               if( valid ) {
                  aryPlace( dcb2->loc, "VARIANCE", &place, status );
                  aryCopy( acb1->vid, &place, &dcb2->vid, status );
               }
            }
         }
      }

/* Note whether DCB variance information is correct. */
      dcb2->kv = ( *status == SAI__OK );

/* AXIS component.
   ==============
   Propagate the axis component. */
      ndf1Aprp( acb1, cpf[ NDF__ACPF ], dcb2, status );

/* WCS component.
   ==============
   If the WCS component is being propagated, determine if WCS
   information is defined for the input NDF. */
      if( cpf[ NDF__WCPF ] ) {
         ndf1Wsta( acb1, &state, status );
         if( *status == SAI__OK ) {

/* If it is, then read the WCS information from the input NDF and
   validate it using the input NDF's ACB entry. The validation should
   always succeed, but this process also strips out the information we
   do not want to store (e.g. associated with the pixel and axis
   coordinate systems). Annul the original AST_ pointer. */
            if( state ) {
               ndf1Rdwcs( acb1, &iwcs, status );
               ndf1Vwcs( acb1, iwcs, &iwcsv, status );
               iwcs = astAnnul( iwcs );

/* Write the stripped information to the output data object.  This also
   causes the output DCB entry to be updated with the WCS information. */
               ndf1Wwrt( iwcsv, dcb2, status );

/* Annul the AST_ pointer to the stripped information. */
               iwcsv = astAnnul( iwcsv );
            }
         }
      }

/* HISTORY component.
   =================
   Propagate the history component. */
      ndf1Hprp( dcb1, cpf[ NDF__HCPF ], dcb2, status );

/* Extension (MORE) component.
   ==========================
   Ensure that extension information is available in the input DCB
   entry. */
      ndf1Dx( dcb1, status );

/* Copy the extension (MORE) structure to the output data object,
   omitting any of its components which aren't wanted. */
      ndf1Xcpy( dcb1->xloc, nextn, extn, dcb2->loc, &dcb2->xloc, status );

/* Note whether extension information is available in the new DCB entry. */
      dcb2->kx = ( *status == SAI__OK );

/* Create a new base NDF entry in the ACB to describe the new data
   object. */
      ndf1Crnbn( dcb2, acb2, status );

/* Assign the name of the data file to the MSG token "NDF_EVENT" */
      ndf1Evmsg( "NDF_EVENT", dcb2 );

/* Raise an NDF event, describing the opening of a new NDF. */
      ndf1Event( "OPEN_NEW_NDF", status );

/* Propagate any bad-bits override value for the NDF's quality
   component to the new ACB entry. */
      if( *status == SAI__OK ) {
         (*acb2)->qbb = acb1->qbb;
         (*acb2)->isqbb = acb1->isqbb;

/* If there was an error, then annul the new DCB entry. */
      } else {
         ndf1Danl( 1, &dcb2, status );
         dcb2 = NULL;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Prp", status );

}

