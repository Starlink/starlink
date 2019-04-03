#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Rst( NdfACB *acb, const char *comp, int *status ){
/*
*+
*  Name:
*     ndf1Rst

*  Purpose:
*     Reset an NDF component to an undefined state.

*  Synopsis:
*     void ndf1Rst( NdfACB *acb, const char *comp, int *status )

*  Description:
*     This function resets a component of an NDF so that its value becomes
*     undefined. It may be used to remove unwanted optional NDF components.
*     The NDF is identified by its ACB entry.

*  Parameters:
*     acb
*        Pointer to ACB for the NDF to be reset.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        component to be reset; any NDF component name is valid. No error
*        will result if the component is already undefined.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied in
*     which case each component will be reset in turn.
*     -  Specifying a component name of "*" will cause all components,
*     except for HISTORY and extensions, to be reset.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char **comps;         /* Array of component name pointers */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */
   int recog;            /* Whether component name is recognised */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Initialise the component recognition flag. */
         recog = 0;

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component.
   ==============
   Reset the axis component, along with all its sub-components. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "AXIS",
                                                     NDF__MINAB ) ) {
            recog = 1;
            ndf1Arst( acb, status );
         }

/* DATA component.
   ============== */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "DATA",
                                                     NDF__MINAB ) ) {
            recog = 1;

/* If the DATA component is to be reset, then check that it is not
   mapped for access through the current ACB entry. Report an error if
   it is. */
            if( acb->dmap ) {
               *status = NDF__ISMAP;
               ndf1Amsg( "NDF", acb );
               errRep( " ", "The data component in the NDF structure ^NDF "
                       "is already mapped for access through the specified "
                       "identifier (possible programming error).", status );

/* Take no further action unless this is a base NDF. Check that the data
   component is not mapped at all. Report an error if it is. */
            } else if( !acb->cut ) {
               if( dcb->ndmap != 0 ) {
                  *status = NDF__ISMAP;
                  ndf1Dmsg( "NDF", dcb );
                  errRep( " ", "The data component in the NDF structure "
                          "^NDF is already mapped for access through "
                          "another identifier (possible programming "
                          "error).", status );

/* Reset the ARY_ system identifier for the NDF's data array held in
   the ACB. */
               } else {
                  aryReset( acb->did, status );
                  ndf1Cmpac( acb->dcb, "DATA", status );
               }
            }
         }

/* EXTENSION (MORE) component.
   ==========================
   If the EXTENSION component is to be reset, then ensure that
   information about it is available in the DCB. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            recog = 1;
            ndf1Dx( dcb, status );
            if( *status == SAI__OK ) {

/* If an extension (MORE) component exists, then erase it along with all
   its contents. Note whether information is still available in the DCB. */
               if( dcb->xloc ) {
                  ndf1Antmp( &dcb->xloc, status );
                  dcb->kx = ( *status == SAI__OK );
               }
            }
         }

/* HISTORY component.
   =================
   If the HISTORY component is to be reset, then ensure that history
   structure information is available in the DCB. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            recog = 1;
            ndf1Dh( dcb, status );
            if( *status == SAI__OK ) {

/* If a history component exists, then annul the associated DCB
   locators and erase the structure. */
               if( dcb->hloc ) {
                  datAnnul( &dcb->hrloc, status );
                  datAnnul( &dcb->hloc, status );
                  datErase( dcb->loc, "HISTORY", status );

/* Clear the DCB history information. */
                  dcb->hdef = 1;
                  dcb->hext = 5;
                  dcb->hsort = 0;
                  dcb->hnrec = 0;
                  dcb->htlen = 0;
                  dcb->humod = NDF__HNORM;
                  dcb->htime = -1.0;

/* Note whether information is still available in the DCB. */
                  dcb->kh = ( *status == SAI__OK );
               }
            }
         }

/* LABEL component.
   ===============
   If the LABEL component is to be reset, then ensure that information
   about it is available in the DCB. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "LABEL",
                                                     NDF__MINAB ) ) {
            recog = 1;
            ndf1Dc( dcb, NDF__LABEL, status );
            if( *status == SAI__OK ) {

/* If the component exists, then erase it. Note whether information is
   still available in the DCB. */
               if( dcb->cloc[ NDF__LABEL ] ) {
                  ndf1Antmp( dcb->cloc + NDF__LABEL, status );
                  dcb->kc[ NDF__LABEL ] = ( *status == SAI__OK );
               }
            }
         }

/* QUALITY component.
   ==================
   Reset the component. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0,
                                                     "QUALITY", NDF__MINAB ) ) {
            recog = 1;
            ndf1Qrst( acb, status );
         }

/* TITLE component.
   ===============
   If the TITLE component is to be reset, then ensure that information
   about it is available in the DCB. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "TITLE",
                                                     NDF__MINAB ) ) {
            recog = 1;
            ndf1Dc( dcb, NDF__TITLE, status );
            if( *status == SAI__OK ) {

/* If the component exists, than erase it. Note whether information is
   still available in the DCB. */
               if( dcb->cloc[ NDF__TITLE ] ) {
                  ndf1Antmp( dcb->cloc + NDF__TITLE, status );
                  dcb->kc[ NDF__TITLE ] = ( *status == SAI__OK );
               }
            }
         }

/* UNITS component.
   ===============
   If the UNITS component is to be reset, then ensure that information
   about it is available in the DCB. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "UNITS",
                                                     NDF__MINAB ) ) {
            recog = 1;
            ndf1Dc( dcb, NDF__UNITS, status );
            if( *status == SAI__OK ) {

/* If the component exists, then erase it. Note whether information is
   still available in the DCB. */
               if( dcb->cloc[ NDF__UNITS ] ) {
                  ndf1Antmp( dcb->cloc + NDF__UNITS, status );
                  dcb->kc[ NDF__UNITS ] = ( *status == SAI__OK );
               }
            }
         }

/* VARIANCE component.
   ==================
   Reset the component. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0,
                                                     "VARIANCE", NDF__MINAB ) ) {
            recog = 1;
            ndf1Vrst( acb, status );
         }

/* WCS component.
   ==============
   If the WCS component is to be reset, then ensure that information
   about it is available in the DCB. */
         if( !strcmp( comps[ icomp ], "*" ) || ndf1Simlr( comps[ icomp ], 1, 0, "WCS",
                                                     NDF__MINAB ) ) {
            recog = 1;
            ndf1Dw( dcb, status );
            if( *status == SAI__OK ) {

/* If a pointer to WCS information is held in the DCB, then annul it
   and erase the associated data structure component. */
               if( dcb->iwcs ) {
                  dcb->iwcs = astAnnul( dcb->iwcs );
                  datErase( dcb->loc, "WCS", status );
               }
            }
         }

/* If the NDF component name was not recognised, then report an error. */
         if( !recog ) {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid component name '^BADCOMP' specified "
                    "(possible programming error).", status );
         }
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
   if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
      *status = NDF__NOCMP;
      errRep( " ", "No component name specified (possible programming "
              "error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Rst", status );

}

