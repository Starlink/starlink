#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "dat_err.h"
#include "mers.h"
#include <string.h>

void ndfXgt0c_( int indf, const char *xname, const char *cmpt, char *value,
               int value_length, int *status ){
/*
*+
*  Name:
*     ndfXgt0c

*  Purpose:
*     Read a scalar string from a component within a named NDF extension.

*  Synopsis:
*     void ndfXgt0c( int indf, const char *xname, const char *cmpt,
*                    char *value, int value_length, int *status )

*  Description:
*     This function reads a scalar string from a component within a named
*     NDF extension. The extension must already exist, although the
*     component within the extension need not exist (a default value,
*     established beforehand, will be returned if necessary).

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the NDF
*        extension.
*     cmpt
*        Pointer to a null terminated string holding the name of the
*        component within the extension whose value is to be obtained.
*     value
*        A buffer in which to place the string obtained from the extension
*        component.
*     value_length
*        The length of the supplied "value" buffer. This should include
*        room for a terminating null character.
*     *status
*        The global status.

*  Notes:
*     -  The value given for the "cmpt" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an object can
*     be accessed in a sub-component (or a sub-sub-component...) of an NDF
*     extension. Array subscripts may also be included. Thus a string such
*     as "CALIB.FILTER(3).WAVELENGTH" could be used as a valid "cmpt"
*     value.
*     -  If the requested component in the extension does not exist, then
*     the "value" parameter will be returned unchanged.  A suitable default
*     should therefore be established before this function is called.
*     -  If the length of the "value" parameter passed to the ndfXgt0c
*     function is too short to accommodate the returned result without
*     losing significant (non-blank) trailing characters, then this will
*     be indicated by an appended ellipsis, i.e. "...". No error will
*     result.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
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
   HDSLoc *loc = NULL;   /* Extension locator */
   HDSLoc *loc1 = NULL;  /* Component locator */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsbool_t there;      /* Whether component is present */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the extension name for validity. */
   ndf1Chxnm( xname, 1, 0, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If there is no extension (MORE) structure, then the requested
   extension cannot be there, so report an error. */
         if( !dcb->xloc ) {
            *status = NDF__NOEXT;
            msgSetc( "XNAME", xname );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "There is no '^XNAME' extension in the NDF "
                    "structure ^NDF", status );

/* Otherwise, see if the requested extension is present. */
         } else {
            datThere( dcb->xloc, xname, &there, status );
            if( *status == SAI__OK ) {

/* If absent, then report an error. */
               if( !there ) {
                  *status = NDF__NOEXT;
                  msgSetc( "XNAME", xname );
                  ndf1Amsg( "NDF", acb );
                  errRep( " ", "There is no '^XNAME' extension in the NDF "
                          "structure ^NDF", status );

/* If present, obtain a locator to it. */
               } else {
                  datFind( dcb->xloc, xname, &loc, status );

/* Mark the error stack and attempt to obtain a locator to the required
   component within the extension. */
                  if( *status == SAI__OK ) {
                     there = 1;
                     errMark();
                     hdsFind( loc, cmpt, "READ", &loc1, status );

/* If the component is not present, note this fact and annul the error.
   Release the error stack. */
                     if( ndf1Absnt( *status ) ) {
                        there = 0;
                        errAnnul( status );
                     }
                     errRlse();

/* If the component exists, then mark the error stack and read its
   value. */
                     if( *status == SAI__OK ) {
                        if( there ) {
                           errMark();
                           datGet0C( loc1, value, value_length, status );

/* If a character string truncation occurred, then annul the error and
   append an ellipsis to the returned value. */
                           if( *status == DAT__CONER || *status == DAT__TRUNC ) {
                              errAnnul( status );
                              ndf1Ellip( value, value_length );
                           }

/* Release the error stack. */
                           errRlse();

/* Annul the component locator. */
                           datAnnul( &loc1, status );
                        }
                     }
                  }

/* Annul the extension locator. */
                  datAnnul( &loc, status );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXgt0c: Error reading a scalar value from a "
              "component within a named NDF extension.", status );
      ndf1Trace( "ndfXgt0c", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

