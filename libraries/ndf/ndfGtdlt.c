#include <stdlib.h>
#include "sae_par.h"
#include "ndf_ast.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "star/util.h"
#include "mers.h"

void ndfGtdlt_( int indf, const char *comp, int *zaxis, char *ztype,
               size_t ztype_length, float *zratio, int *status ){
/*
*+
*  Name:
*     ndfGtdlt

*  Purpose:
*     Get compression details for a DELTA compressed NDF array component

*  Synopsis:
*     void ndfGtdlt( int indf, const char *comp, int *zaxis, char *ztype,
*                    size_t ztype_length, float *zratio, int *status )

*  Description:
*     This function returns the details of the compression used by an NDF
*     array component stored in DELTA form. If the array is not stored in
*     DELTA form, then null values are returned as listed below, but no
*     error is reported.
*
*     A DELTA array is compressed by storing only the differences between
*     adjacent array values along a nominated compression axis, rather than
*     the full array values. The differences are stored using a smaller
*     data type than the original absolute values. The compression is
*     lossless because any differences that will not fit into the smaller
*     data type are stored explicitly in an extra array with a larger data
*     type. Additional compression is achieved by replacing runs of equal
*     values by a single value and a repeat count.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component; "DATA", "QUALITY" or "VARIANCE".
*     *zaxis
*        The index of the pixel axis along which compression occurred. The
*        first axis has index 1. Zero is returned if the array is not
*        stored in DELTA form.
*     ztype
*        Pointer to an array in which to return a null terminated string
*        holding the data type in which the differences between adjacent
*        array values are stored. This will be one of "_BYTE", "_WORD" or
*        "_INTEGER". The data type of the array itself is returned if the
*        supplid array is not stored in DELTA form.
*     ztype_length
*        The length of the supplied 'ztype' array. This should include
*        room for the terminating null.
*     *zratio
*        The compression factor - the ratio of the uncompressed array size
*        to the compressed array size. This is approximate as it does not
*        include the effects of the metadata needed to describe the extra
*        components of a DELTA array (i.e. the space needed to hold the
*        component names, types, dimensions, etc). A value of 1.0 is
*        returned if the supplid array is not stored in DELTA form.
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
   char *lcomp;          /* Local copy of "comp" without spaces */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int there;            /* Whether the variance array exists */

/* Initialise returned values */
   *zaxis = 0;
   star_strlcpy( ztype, " ", ztype_length );
   *zratio = 1.0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Create a copy of the string excluding any leading or trailing spaces. */
   lcomp = ndf1Strip( NULL, comp, 1, 0, NULL, NULL, status );
   if( *status == SAI__OK ) {

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component:
   ==============
   Report an error, since this component has no scaling. */
      if( ndf1Simlr( lcomp, 1, 0, "AXIS", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for an "
                 "AXIS component (possible programming error).", status );

/* DATA component:
   ==============
   Use the ARY_ system to get the delta compression details for the data array. */
      } else if( ndf1Simlr( lcomp, 1, 0, "DATA", NDF__MINAB ) ) {
         aryGtdlt( acb->did, zaxis, ztype, zratio, status );

/* EXTENSION:
   =========
   Report an error, since extensions have no scaling. */
      } else if( ndf1Simlr( lcomp, 1, 0, "EXTENSION", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for an "
                 "EXTENSION (possible programming error).", status );

/* HISTORY component:
   =================
   Report an error, since this component has no scaling. */
      } else if( ndf1Simlr( lcomp, 1, 0, "HISTORY", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for an "
                 "HISTORY component (possible programming error).", status );

/* LABEL component:
   ===============
   Report an error, since this component has no scaling. */
      } else if( ndf1Simlr( lcomp, 1, 0, "LABEL", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for an "
                 "LABEL component (possible programming error).", status );

/* QUALITY component:
   =================
   Use the ARY_ system to get the delta compression details for the data array. */
      } else if( ndf1Simlr( lcomp, 1, 0, "QUALITY", NDF__MINAB ) ) {

/* Ensure that quality information is available in the DCB and ACB. */
         ndf1Qimp( acb, status );

/* See if the ARY_ system identifier for the quality array is valid.
   If not, then the array does not exist. */
         there = aryValid( acb->qid, status );
         if( *status == SAI__OK ) {

/* If it exists, then get the compression details. */
            if( there ) aryGtdlt( acb->qid, zaxis, ztype, zratio, status );
         }

/* TITLE component:
   ===============
   Report an error, since this component has no scaling. */
      } else if( ndf1Simlr( lcomp, 1, 0, "TITLE", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for a "
                 "TITLE component (possible programming error).", status );

/* UNITS component:
   ===============
   Report an error, since this component has no scaling. */
      } else if( ndf1Simlr( lcomp, 1, 0, "UNITS", NDF__MINAB ) ) {
         *status = NDF__CNMIN;
         errRep( " ", "Delta compression details cannot be obtained for a "
                 "UNITS component (possible programming error).", status );

/* VARIANCE component:
   ==================
   Ensure that variance information is available in the DCB and ACB. */
      } else if( ndf1Simlr( lcomp, 1, 0, "VARIANCE", NDF__MINAB ) ) {
         ndf1Vimp( acb, status );

/* See if the ARY_ system identifier for the variance array is valid.
   If not, then the array does not exist. */
         there = aryValid( acb->vid, status );
         if( *status == SAI__OK ) {

/* If it exists, then get the compression details. */
            if( there ) aryGtdlt( acb->vid, zaxis, ztype, zratio, status );
         }

/* If the component name is not recognised, then report an error. */
      } else {
         *status = NDF__CNMIN;
         msgSetc( "BADCOMP", lcomp );
         errRep( " ", "Invalid array component name '^BADCOMP' specified "
                 "(possible programming error).", status );
      }

   }

/* Free the memory holding the local copy of the "comp" string. */
   lcomp = astFree( lcomp );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfGtdlt:  Error getting information about a delta "
              "compressed NDF array component.", status );
      ndf1Trace( "ndfGtdlt", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}


