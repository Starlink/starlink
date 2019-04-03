#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfForm_( int indf, const char *comp, char *form, size_t form_length,
              int *status ){
/*
*+
*  Name:
*     ndfForm

*  Purpose:
*     Obtain the storage form of an NDF array component.

*  Synopsis:
*     void ndfForm( int indf, const char *comp, char *form,
*                   size_t form_length, int *status )

*  Description:
*     This function returns the storage form of an NDF array component as
*     an upper case character string (e.g. "SIMPLE").

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component whose storage form is required: "DATA", "QUALITY"
*        or "VARIANCE".
*     form
*        Pointer to an array in which to return a null terminated string
*        holding the storage form of the component.
*     form_length
*        The length of the supplied 'form' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZFRM may be used for declaring the
*     length of a character variable to hold the storage form of an NDF
*     array component. This constant is defined in the header file "ndf.h".
*     -  At present, the NDF_ functions only support "primitive", "simple",
*     "delta" and "scaled" arrays, so only the values "PRIMITIVE",
*     "SIMPLE", "DELTA" and "SCALED" can be returned.

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

/* Local variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Compare the component name with each permitted value in turn,
   allowing abbreviation. */

/* AXIS component.
   ==============
   Report an error, since this component has no storage form. */
      if( ndf1Simlr( comp, 1, 0, "AXIS", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "An AXIS component does not have a storage form "
                 "(possible programming error).", status );

/* DATA component.
   ==============
   Obtain the storage form from the ARY_ system data array identifier
   in the ACB. */
      } else if( ndf1Simlr( comp, 1, 0, "DATA", NDF__MINAB ) ) {
         aryForm( acb->did, form, status );

/* EXTENSION.
   =========
   Report an error, since extensions do not have a storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "EXTENSION", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "An EXTENSION does not have a storage form (possible "
                 "programming error).", status );

/* HISTORY component.
   =================
   Report an error, since this component does not have a storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "HISTORY", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "A HISTORY component does not have a storage form "
                 "(possible programming error).", status );

/* LABEL component.
   ===============
   Report an error, since this component does not have a storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "LABEL", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "A LABEL component does not have a storage form "
                 "(possible programming error).", status );

/* QUALITY component.
   =================
   Inspect the quality component to determine its storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "QUALITY", NDF__MINAB ) ) {
         ndf1Qfrm( acb, form, form_length, status );

/* TITLE component.
   ===============
   Report an error, since this component does not have a storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "TITLE", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "A TITLE component does not have a storage form "
                 "(possible programming error).", status );

/* UNITS component.
   ===============
   Report an error, since this component does not have a storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "UNITS", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "A UNITS component does not have a storage form "
                 "(possible programming error).", status );

/* VARIANCE component.
   ==================
   Inspect the variance component to determine its storage form. */
      } else if( ndf1Simlr( comp, 1, 0, "VARIANCE", NDF__MINAB ) ) {
         ndf1Vfrm( acb, form, form_length, status );

/* If the NDF component name was not recognised, then report an error. */
      } else {
         *status = NDF__CNMIN;
         msgSetc( "BADCOMP", comp );
         errRep( " ", "Invalid array component name '^BADCOMP' specified "
                 "(possible programming error).", status );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfForm: Error obtaining the storage form of an NDF "
              "array component.", status );
      ndf1Trace( "ndfForm", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

