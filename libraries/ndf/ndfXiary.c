#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "ndf.h"
#include "mers.h"

void ndfXiary_( int indf, const char *xname, const char *cmpt,
               const char *mode, Ary **ary, int *status ){
/*
*+
*  Name:
*     ndfXiary

*  Purpose:
*     Obtain access to an array stored in an NDF extension.

*  Synopsis:
*     void ndfXiary( int indf, const char *xname, const char *cmpt,
*                    const char *mode, Ary **ary, int *status )

*  Description:
*     This function locates an array stored in an NDF extension and imports
*     it into the ARY_ system, returning an array identifier for it. If
*     necessary, a section of the array will be selected so that it matches
*     pixel-for-pixel with the main data array of the NDF (or NDF section)
*     supplied.  The returned array identifier may be used to manipulate
*     the array using the ARY_ functions (see SUN/11).

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the
*        extension.
*     cmpt
*        Pointer to a null terminated string holding the name of the array
*        component within the extension.
*     mode
*        Pointer to a null terminated string holding the mode of access
*        required: "READ", "UPDATE" or "WRITE".
*     *ary
*        Returned holding the array identifier.
*     *status
*        The global status.

*  Notes:
*     -  The value given for the "cmpt" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an object can
*     be accessed in a sub-component (or a sub-sub-component...) of an NDF
*     extension. Array subscripts may also be included. Thus a string such
*     as "FILTER(3).FLATFIELD" could be used as a valid "cmpt" value.
*     -  This function will normally generate an array section. However, if
*     the input NDF is a base NDF and the requested array has the same
*     pixel-index bounds, then there is no need to generate a section in
*     order to access the required part of the array. In this case, a base
*     array identifier will be issued instead.
*     -  It is the caller's responsibility to annul the ARY_ system
*     identifier returned by this function (e.g. by calling "aryAnnul")
*     when it is no longer required. The NDF_ system will not perform this
*     task itself.
*     -  The array associated with the returned identifier will have the
*     same number of dimensions as the base array from which it is derived.
*     If the input NDF has fewer dimensions than this, then the pixel-index
*     bounds of the extra array dimensions are preserved unchanged. If the
*     NDF has more dimensions, then the extra ones are ignored.
*     -  This function takes account of the transfer window of the NDF
*     supplied and will restrict the transfer window of the new array
*     section so as not to grant access to regions of the base array which
*     are not accessible in the input NDF.
*     -  If this function is called with "status" set, then a value of NULL
*     will be returned for the "ary" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.
*     -  The NULL constant is defined in the header file "ary.h".

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
   Ary *aryb;            /* Base array identifier */
   HDSLoc *loc = NULL;   /* Extension locator */
   HDSLoc *loc1 = NULL;  /* Component locator */
   NdfACB *acb;          /* Pointer to the NDF in the ACB */
   NdfDCB *dcb;          /* Pointer to data object in the DCB */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode string */
   hdsbool_t there;      /* Extension present? */

/* Set an initial null default value for the "ary" parameter. */
   *ary = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the access mode for validity and determine whether the required
   mode of NDF access is available. */
   ndf1Vmod( mode, vmode, sizeof( vmode ), status );
   ndf1Chmod( acb, vmode, status );

/* Check the extension name for validity. */
   ndf1Chxnm( xname, 1, 0, status );

/* If OK, then obtain an index to the data object in the DCB. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;

/* Ensure that extension information is available for the NDF. */
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

/* If the required extension is present, then obtain a locator to it. */
               } else {
                  datFind( dcb->xloc, xname, &loc, status );

/* Locate the required component within the extension. */
                  hdsFind( loc, cmpt, vmode, &loc1, status );
                  datAnnul( &loc, status );

/* Import the component into the ARY_ system, obtaining an identifier
   for the base array. Annul the extension locator. */
                  aryImprt( loc1, &aryb, status );
                  datAnnul( &loc1, status );

/* Obtain a section from the array which matches the NDF's main data
   array. Annul the base array identifier. */
                  arySsect( aryb, acb->did, ary, status );
                  aryAnnul( &aryb, status );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXiary: Error obtaining access to an array stored in "
              "an NDF extension.", status );
      ndf1Trace( "ndfXiary", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

