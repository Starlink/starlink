#include "sae_par.h"
#include "dat_par.h"
#include "star/util.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "star/util.h"
#include <string.h>
#include "mers.h"

void ndfGtune_( const char *tpar, int *value, int *status ){
/*
*+
*  Name:
*     ndfGtune

*  Purpose:
*     Obtain the value of an NDF_ system tuning parameter.

*  Invocation:
*     void ndfGtune( const char *tpar, int *value, int *status )

*  Description:
*     This function returns the current value of an NDF_ system internal
*     tuning parameter.

*  Parameters:
*     tpar
*        Pointer to a null terminated string holding the name of the tuning
*        parameter whose value is required (case insensitive). This name
*        may be abbreviated, to no less than 3 characters.
*     *value
*        Returned holding the value of the parameter.
*     *status
*        The global status.

*  Notes:
*     See the ndfTune function for a list of the tuning parameters
*     currently available.

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
*     26-APR-2019 (DSB):
*        Add the FIXSW tuning parameter.
*     21-APR-2020 (DSB):
*        Add the ROUND tuning parameter.

*-
*/

/* Local Variables: */
   char xname[ DAT__SZNAM + 1 ];   /* An NDF extension name */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );
   if( *status == SAI__OK ) {

/* The TCB values are held in global variabl;es, so use a mutex to ensure
   the current thread has sole access. */
      NDF__TCB_LOCK_MUTEX;

/* Test the tuning parameter name supplied against each permitted value
   in turn, allowing abbreviation... */

/* Error tracing flag.
   ==================
   If TRACE was specified, then return the error tracing flag value. */
      if( ndf1Simlr( tpar, 1, 0, "TRACE", NDF__MINAB ) ) {
         if( Ndf_TCB_etflg ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Do format conversion flag.
   =========================
   If DOCVT was specified, then return the do format conversion flag
   value. */
      } else if( ndf1Simlr( tpar, 1, 0, "DOCVT", NDF__MINAB ) ) {
         if( Ndf_TCB_docvt ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Keep NDF objects flag.
   =====================
   If KEEP was specified, then return the keep NDF objects flag value. */
      } else if( ndf1Simlr( tpar, 1, 0, "KEEP", NDF__MINAB ) ) {
         if( Ndf_TCB_keep ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Create History component automatically flag.
   ============================================ */
      } else if( ndf1Simlr( tpar, 1, 0, "AUTO_HISTORY", NDF__MINAB ) ) {
         if( Ndf_TCB_autohistory ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Show data conversion flag.
   =========================
   If SHCVT was specified, then return the show data conversion flag
   value. */
      } else if( ndf1Simlr( tpar, 1, 0, "SHCVT", NDF__MINAB ) ) {
         if( Ndf_TCB_shcvt ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Warning message flag.
   ====================
   If WARN was specified, then return the warning message flag value. */
      } else if( ndf1Simlr( tpar, 1, 0, "WARN", NDF__MINAB ) ) {
         if( Ndf_TCB_warn ) {
            *value = 1;
         } else {
            *value = 0;
         }

/* Extension propagation.
   ======================
   Any tuning flag that begins with "PXT" is assumed to be terminated
   with the name of an extension. The tuning flag value is non-zero if
   the extension should be propagated by default by ndfProp, etc, and
   is zero if the extension should not be propagated by default. */
      } else if( ndf1Simlr( tpar, 0, 2, "PXT", NDF__MINAB ) ) {

/* copy the extension name, terminate it to remove trailing spaces and
   check it is not blank. */
         star_strlcpy( xname, tpar + 3, sizeof( xname ) );
         astChrTrunc( xname );
         if( *xname != 0 ) {

/* If there is a KeyMap, and it contains a value for the named extension,
   use the stored value. Otherwise use a defualt value of 1 (all
   extensions are propagated by default). */
            if( Ndf_TCB_pxt ) {
               if( !astMapGet0I( Ndf_TCB_pxt, xname, value ) ) *value = 1;
            } else {
               *value = 1;
            }

/* If the extension name was blank, then report an error. */
         } else {
            *status = NDF__TPNIN;
            errRep( " ", "'PXT' is not a valid tuning parameter name - it "
                    "should be followed by an NDF extension name (possible "
                    "programming error).", status );
         }

/* Maximum size of a section.
   ========================== */
      } else if( ndf1Simlr( tpar, 1, 0, "SECMAX", NDF__MINAB ) ) {
         *value = Ndf_TCB_secmax;

/* Fix the date/time strings stored in history records.
   =================================================== */
      } else if( ndf1Simlr( tpar, 1, 0, "FIXDT", NDF__MINAB ) ) {
         *value = Ndf_TCB_fixdt;

/* Fix the software strings stored in history records.
   ================================================== */
      } else if( ndf1Simlr( tpar, 1, 0, "FIXSW", NDF__MINAB ) ) {
         *value = Ndf_TCB_fixsw;

/* Round floating-point values to the nearest integer.
   ================================================== */
      } else if( ndf1Simlr( tpar, 1, 0, "ROUND", NDF__MINAB ) ) {
         *value = Ndf_TCB_round;

/* Unknown tuning parameter.
   ========================
   Report an error if the tuning parameter name is not recognised. */
      } else {
         *status = NDF__TPNIN;
         msgSetc( "TPAR", tpar );
         errRep( " ", "'^TPAR' is not a valid tuning parameter name "
                 "(possible programming error).", status );
      }

/* Release the mutex. */
      NDF__TCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfGtune: Error obtaining the value of an NDF_ system "
              "tuning parameter.", status );
      ndf1Trace( "ndfGtune", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

