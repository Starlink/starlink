#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"
#include <string.h>

void ndfTune_( int value, const char *tpar, int *status ){
/*
*+
*  Name:
*     ndfTune

*  Purpose:
*     Set an NDF_ system tuning parameter.

*  Synopsis:
*     void ndfTune( int value, const char *tpar, int *status )

*  Description:
*     This function sets a new value for an NDF_ system internal tuning
*     parameter.

*  Parameters:
*     value
*        New value for the tuning parameter.
*     tpar
*        Pointer to a null terminated string holding the name of the
*        parameter to be set (case insensitive).  This name may be
*        abbreviated, to no less than 3 characters.
*     *status
*        The global status.

*  Notes:
*     The following tuning parameters are currently available:
*     - "AUTO_HISTORY": Controls whether to include an empty History
*     component in NDFs created using ndfNew or ndfCreat. If the tuning
*     parameter is zet to zero (the default), no History component will be
*     included in the new NDFs. If the tuning parameter is zet non-zero, a
*     History component will be added automatically to the new NDFs.
*     - "DOCVT": Controls whether to convert foreign format data files to
*     and from native NDF format for access (using the facilities described
*     in SSN/20). If DOCVT is set to 1 (the default), and the other
*     necessary steps described in SSN/20 have been taken, then such
*     conversions will be performed whenever they are necessary to gain
*     access to data stored in a foreign format. If DOCVT is set to 0, no
*     such conversions will be attempted and all data will be accessed in
*     native NDF format only. The value of DOCVT may be changed at any
*     time. It is the value current when a dataset is first accessed by the
*     NDF_ library which is significant.
*     -  "KEEP": Controls whether to retain a native format NDF copy of any
*     foreign format data files which are accessed by the NDF_ library (and
*     automatically converted using the facilities described in SSN/20). If
*     KEEP is set to 0 (the default), then the results of converting
*     foreign format data files will be stored in scratch filespace and
*     deleted when no longer required. If KEEP is set to 1, the results of
*     the conversion will instead be stored in permanent NDF data files in
*     the default directory (such files will have the same name as the
*     foreign file from which they are derived and a file type of ".sdf").
*     Setting KEEP to 1 may be useful if the same datasets are to be re-
*     used, as it avoids having to convert them on each occasion. The value
*     of KEEP may be changed at any time. It is the value current when a
*     foreign format file is first accessed by the NDF_ library which is
*     significant.
*     -  "SECMAX": Gives the largest size of an NDF section, in units of
*     maga-pixels. An error is reported if a section is requested that
*     contains more than this number of pixels. The purpose of this
*     parameter is to help guard against accidental use of incorrect units
*     within NDF sections specified by the user. The default value is 1E10.
*     -  "SHCVT": Controls whether diagnostic information is displayed to
*     show the actions being taken to convert to and from foreign data
*     formats (using the facilities described in SSN/20). If SHCVT is set
*     to 1, then this information is displayed to assist in debugging
*     external format conversion software whenever a foreign format file is
*     accessed. If SHCVT is set to 0 (the default), this information does
*     not appear and format conversion proceeds silently unless an error
*     occurs.
*     -  "TRACE": Controls the reporting of additional error messages which
*     may occasionally be useful for diagnosing internal problems within
*     the NDF_ library. If TRACE is set to 1, then any error occurring
*     within the NDF_ system will be accompanied by error messages
*     indicating which internal functions have exited prematurely as a
*     result. If TRACE is set to 0 (the default), this internal diagnostic
*     information will not appear and only standard error messages will be
*     produced.
*     -  "WARN": Controls the issuing of warning messages when certain non-
*     fatal errors in the structure of NDF data objects are detected. If
*     WARN is set to 1 (the default), then a warning message is issued. If
*     WARN is set to 0, then no message is issued.  In both cases normal
*     execution continues and no "status" value is set.
*     -  "PXT...": Controls whether a named NDF extension should be
*     propagated by default when ndfProp or ndfScopy is called. The name of
*     the extension should be appended to the string "PXT" to form the
*     complete tuning parameter name. Thus the tuning parameter PXTFITS
*     would control whether the FITS extension is propagated by default. If
*     the value for the parameter is non-zero, then the extension will be
*     propagated by default. If the value for the parameter is zero, then
*     the extension will not be propagated by default. The default
*     established by this tuning parameter can be over-ridden by specifying
*     the extension explicitly within the CLIST parameter when calling
*     ndfProp or ndfScopy. The default value for all "PXT..." tuning
*     parameters is 1, meaning that all extensions are propagated by
*     default.
*     - 'FIXDT': If FIXDT is set to 1, the date and time string stored
*     in any new History records will be fixed at an arbitrary value
*     ("10-AUG-2018 11:00:00") regardless of the current time. If 0, the
*     real date and time will be used. This facility is intended to allow
*     regression testing, where any change in the date/time within history
*     records could cause a test to fail.

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
   const char *xname;           /* An NDF extension name */

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
   If TRACE was specified, then set the error tracing flag appropriately. */
      if( ndf1Simlr( tpar, 1, 0, "TRACE", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_etflg = 0;
         } else if( value == 1 ) {
            Ndf_TCB_etflg = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter TRACE; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

/* Do format conversions flag.
   ==========================
   If DOCVT was specified, then set the do format conversions flag
   appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "DOCVT", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_docvt = 0;
         } else if( value == 1 ) {
            Ndf_TCB_docvt = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter DOCVT; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

/* Keep NDF objects flag.
   =====================
   If KEEP was specified, the set the keep NDF objects flag
   appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "KEEP", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_keep = 0;
         } else if( value == 1 ) {
            Ndf_TCB_keep = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter KEEP; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

/* Automatic History creation flag.
   ================================
   If AUTO_HISTORY was specified, then set the automatic history creation
   flag appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "AUTO_HISTORY", NDF__MINAB ) ) {
         Ndf_TCB_autohistory = ( value != 0 );

/* Show data conversions flag.
   ==========================
   If SHCVT was specified, then set the show data conversions flag
   appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "SHCVT", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_shcvt = 0;
         } else if( value == 1 ) {
            Ndf_TCB_shcvt = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter SHCVT; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

/* Warning message flag.
   ====================
   If WARN was specified, then set the warning message flag
   appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "WARN", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_warn = 0;
         } else if( value == 1 ) {
            Ndf_TCB_warn = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter WARN; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

/* Extension propagation.
   ======================
   Any tuning flag that begins with "PXT" is assumed to be terminated
   with the name of an extension. The tuning flag value is non-zero if
   the extension should be propagated by default by ndfProp, etc, and
   is zero if the extension should not be propagated by default. */
      } else if( astChrMatchN( tpar, "PXT", 3 ) ) {

/* Get the extension name and check it is not blank. */
         xname = tpar + 3;
         if( astChrLen( xname ) > 0 ) {

/* Ensure an AST KeyMap is available to hold the flags for each extension.
   Enclose its creation in an AST "permanent memory" block so that it is
   excluded from memory leak checking (since it is never released by the
   NDF library it would otherwise appear as a memory leak). */
            if( !Ndf_TCB_pxt ) {
               astBeginPM;
               Ndf_TCB_pxt = astKeyMap( " " );
               astExempt( Ndf_TCB_pxt );
               astEndPM;
            }

/* Store the supplied value flag in the KeyMap, using the supplied
   extenmsion name as the key. */
            astMapPut0I( Ndf_TCB_pxt, xname, value, " " );

/* If the extension name was blank, then report an error. */
         } else {
            *status = NDF__TPNIN;
            errRep( " ", "'PXT' is not a valid tuning parameter name - it "
                    "should be followed by an NDF extension name (possible "
                    "programming error).", status );
         }

/* Maximum section size.
   =====================
   If AUTO_HISTORY was specified, then set the automatic history creation
   flag appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "SECMAX", NDF__MINAB ) ) {
         if( value > 0 ) {
            Ndf_TCB_secmax = value;

         } else if( *status == SAI__OK ) {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter SECMAX; it should be larger than zero "
                    "(possible programming error).", status );
         }

/* Use a fixed date/time wqithin new History records.
   =================================================
   If FIXDT was specified, then set the "use fixed history date/time" flag
   appropriately. */
      } else if( ndf1Simlr( tpar, 1, 0, "FIXDT", NDF__MINAB ) ) {
         if( value == 0 ) {
            Ndf_TCB_fixdt = 0;
         } else if( value == 1 ) {
            Ndf_TCB_fixdt = 1;

/* If the value supplied is not valid, then report an error. */
         } else {
            *status = NDF__TPVIN;
            msgSeti( "VALUE", value );
            errRep( " ", "The value ^VALUE is not valid for the tuning "
                    "parameter FIXDT; it should be 0 or 1 (possible "
                    "programming error).", status );
         }

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
      errRep( " ", "ndfTune: Error setting a new value for an NDF_ system "
              "tuning parameter.", status );
      ndf1Trace( "ndfTune", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

