#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "star/util.h"
#include "star/cmp.h"
#include "mers.h"

void ndfHsmod_( const char *hmode, int indf, int *status ){
/*
*+
*  Name:
*     ndfHsmod

*  Purpose:
*     Set the history update mode for an NDF.

*  Synopsis:
*     void ndfHsmod( const char *hmode, int indf, int *status )

*  Description:
*     This function sets the mode to be used for updating the history
*     component of an NDF. This allows control over the amount of history
*     information subsequently recorded. It also allows history recording
*     to be disabled completely.

*  Parameters:
*     hmode
*        Pointer to a null terminated string holding the history update
*        mode required: "DISABLED", "QUIET", "NORMAL" or "VERBOSE". This
*        value may be abbreviated, to no less than three characters. In
*        addition, "SKIP" may be supplied. This is similar to "DISABLED",
*        in that no history record will be added to the NDF when the NDF is
*        closed. However, "SKIP" makes no permanent change to the update
*        mode - the next time the NDF is accessed it will have its original
*        update mode.
*     indf
*        NDF identifier.
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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char mode[ NDF__SZHUM + 1 ];    /* Required mode */
   char text[ NDF__SZHUM + 1 ];    /* UPDATE_MODE value */
   hdsdim dim;           /* Dummy dimension size array */
   int hum;              /* History update mode code */
   int skip;             /* Was SKIP supplied? */
   size_t nc;            /* Number of characters in TEXT */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* If the history update mode is "skip", set a flag and use DISABLED instead. */
      skip = ndf1Simlr( hmode, 1, 0, "SKIP", 3 );
      if( skip ) {
         star_strlcpy( mode, "DISABLED", sizeof( mode ) );
      } else {
         star_strlcpy( mode, hmode, sizeof( mode ) );
      }

/* Check that WRITE access to the NDF is available and validate the
   history update mode string. */
      ndf1Chacc( acb, "WRITE", status );
      ndf1Chhum( mode, &hum, status );

/* Obtain an index to the data object entry in the DCB and ensure that
   DCB history information is available. */
      dcb = acb->dcb;
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that a history component is present and report an error if it
   is not. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* Otherwise, test for each update mode value and assign the
   appropriate UPDATE_MODE component string. */
         } else if( hum == NDF__HDISA ) {
            star_strlcpy( text, "DISABLED", sizeof( text ) );
            nc = 8;
         } else if( hum == NDF__HQUIE ) {
            star_strlcpy( text, "QUIET", sizeof( text ) );
            nc = 5;
         } else if( hum == NDF__HNORM ) {
            star_strlcpy( text, "NORMAL", sizeof( text ) );
            nc = 6;
         } else if( hum == NDF__HVERB ) {
            star_strlcpy( text, "VERBOSE", sizeof( text ) );
            nc = 7;

/* If the update mode is not recognised, then report an error. */
         } else {
            *status = NDF__FATIN;
            msgSeti( "HUM", hum );
            errRep( " ", "Invalid history update mode code (^HUM) "
                    "encountered (internal programming error).", status );
         }

/* Ensure that an UPDATE_MODE component of the required type and shape
   exists in the NDF history structure and write the new value to it.
   Skip this bit if "skip" was supplied, so that no permanent change is made
   to the NDF. */
         if( *status == SAI__OK ) {
            if( !skip ) {
               cmpModC( dcb->hloc, "UPDATE_MODE", nc, 0, &dim, status );
               cmpPut0C( dcb->hloc, "UPDATE_MODE", text, status );
            }

/* Modify the corresponding DCB update mode value. */
            if( *status == SAI__OK ) dcb->humod = hum;
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHsmod: Error setting the history update mode for an "
              "NDF.", status );
      ndf1Trace( "ndfHsmod", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

