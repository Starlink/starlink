#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfMbnd_( const char *option, int *indf1, int *indf2, int *status ){
/*
*+
*  Name:
*     ndfMbnd

*  Purpose:
*     Match the pixel-index bounds of a pair of NDFs.

*  Synopsis:
*     void ndfMbnd( const char *option, int *indf1, int *indf2, int *status )

*  Description:
*     This function matches the pixel-index bounds of a pair of NDFs so
*     that their array components may be compared pixel-for-pixel during
*     subsequent processing. Matching is performed by selecting an
*     appropriate section from each NDF, the method used to define this
*     section being determined by the value given for the "option"
*     parameter.

*  Parameters:
*     option
*        Pointer to a null terminated string holding the this parameter
*        determines how the section to be selected from each NDF is
*        defined: "PAD" or "TRIM" (see the Notes section for details). Its
*        value may be abbreviated to 3 characters.
*     *indf1
*        Identifier for the first NDF whose pixel-index bounds are to be
*        matched.
*     *indf2
*        Identifier for the second NDF to be matched.
*     *status
*        The global status.

*  Notes:
*     -  If "option"="PAD" is specified, then the NDF bounds will be
*     matched by "padding"; i.e. each NDF will be extended by selecting the
*     smallest section from it which encompasses all the pixels in both
*     NDFs. In effect, the pixel-index bounds of the two NDFs are
*     "maximised" and the "union" of the two sets of pixels is selected.
*     Any new pixels introduced into either NDF will be padded with the
*     "bad" value.  If the NDFs have different numbers of dimensions, then
*     the dimensionality of both the returned sections will match the NDF
*     with the higher dimensionality.
*     -  If "option"="TRIM" is specified, then the NDF bounds will be
*     matched by "trimming"; i.e. each NDF will be restricted in extent by
*     selecting a section from it which encompasses only those pixels which
*     are present in both NDFs. In effect, the pixel-index bounds of the
*     two NDFs are "minimised" and the "intersection" of the two sets of
*     pixels is selected. An error will result if the two NDFs have no
*     pixels in common. If the NDFs have different numbers of dimensions,
*     then the dimensionality of both the returned sections will match the
*     NDF with the lower dimensionality.
*     -  Note that the initial NDF identifier values will be annulled by
*     this function and replaced with identifiers describing appropriate
*     new sections from the original NDFs. If access to the original data
*     is still required, then the initial identifiers may be cloned with
*     the function ndfClone before calling this function.

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
   int ndfs[ 2 ];        /* List of NDF identifiers to be matched */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Copy the NDF identifiers supplied to the elements of an array. */
   ndfs[ 0 ] = *indf1;
   ndfs[ 1 ] = *indf2;

/* Compare the "option" value with each permitted value in turn (allowing
   abbreviation), calling the appropriate function to match the pixel
   index bounds of the NDFs. */

/* PAD option:
   ========== */
   if( ndf1Simlr( option, 1, 0, "PAD", NDF__MINAB ) ) {
      ndf1Mbndp( 2, ndfs, status );

/* TRIM option:
   =========== */
   } else if( ndf1Simlr( option, 1, 0, "TRIM", NDF__MINAB ) ) {
      ndf1Mbndt( 2, ndfs, status );

/* If the "option" value was not recognised, then report an error. */
   } else {
      *status = NDF__BMOIN;
      msgSetc( "BADOPT", option );
      errRep( " ", "Invalid matching option '^BADOPT' specified (possible "
              "programming error).", status );
   }

/* Return the new identifier values. */
   if( *status == SAI__OK ) {
      *indf1 = ndfs[ 0 ];
      *indf2 = ndfs[ 1 ];
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfMbnd: Error matching the pixel-index bounds of a "
              "pair of NDFs.", status );
      ndf1Trace( "ndfMbnd", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

