#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Dac( int iax, int iccomp, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dac

*  Purpose:
*     Ensure that DCB information about an axis character component is
*     available.

*  Synopsis:
*     void ndf1Dac( int iax, int iccomp, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information is available in the DCB for a
*     specified axis character component of an NDF. It returns without
*     action if this information is already available. Otherwise, it
*     inspects the actual data object to obtain the information, which it
*     stores in the DCB. Only those checks necessary to obtain and validate
*     this information are performed.

*  Parameters:
*     iax
*        Zero-based index of the axis for which information is required.
*     iccomp
*        Number of the axis character component whose value is required.
*        Symbolic constant values for this parameter are defined in the
*        header file "ndf1.h".
*     dcb
*        Pointer to the NDF.
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
   char type[ DAT__SZTYP + 1 ];    /* Component type */
   hdsbool_t there;      /* Whether component is present */
   hdsdim dim[ DAT__MXDIM ];       /* Component dimensions */
   int ndim;             /* Number of component dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if the required axis character component information is already
   available. There is nothing to do if it is. */
   if( !dcb->kac[ iax ][ iccomp ] ) {

/* Ensure that axis structure information is available in the DCB. */
      ndf1Da( dcb, status );
      if( *status == SAI__OK ) {

/* Set an initial null value for the axis character component locator. */
         dcb->acloc[ iax ][ iccomp ] = NULL;

/* See if the axis structure exists. If not, then the character
   component cannot exist, so its locator remains null. */
         if( dcb->aloc[ iax ] ) {

/* See if the required character component exists within the appropriate
   element of the axis structure. */
            datThere( dcb->aloc[ iax ], Ndf_DCB_accn[ iccomp ], &there, status );
            if( *status == SAI__OK ) {
               if( there ) {

/* If so, then obtain a locator for it and determine its type and shape. */
                  datFind( dcb->aloc[ iax ], Ndf_DCB_accn[ iccomp ],
                           dcb->acloc[ iax ] + iccomp, status );
                  datType( dcb->acloc[ iax ][ iccomp ], type, status );
                  datShape( dcb->acloc[ iax ][ iccomp ], DAT__MXDIM, dim,
                            &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that its type is _CHAR. Report an error if it is not. */
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        msgSetc( "CCOMP", Ndf_DCB_accn[ iccomp ] );
                        datMsg( "AXIS", dcb->acloc[ iax ][ iccomp ] );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The ^CCOMP component in the NDF axis "
                                "structure ^AXIS has an invalid type of "
                                "'^BADTYPE'; it should be of type "
                                "'_CHAR'.", status );

/* Check that it is a scalar object. Report an error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        msgSetc( "CCOMP", Ndf_DCB_accn[ iccomp ] );
                        datMsg( "AXIS", dcb->acloc[ iax ][ iccomp ] );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The ^CCOMP component in the NDF axis "
                                "structure ^AXIS is ^BADNDIM-dimensional; "
                                "it should be scalar.", status );
                     }
                  }

/* If an error occurred, then annul the locator which may have been
   acquired. */
                  if( *status != SAI__OK ) datAnnul( dcb->acloc[ iax ] + iccomp, status );
               }
            }
         }
      }

/* Note whether the required axis character component information is now
   available. */
      dcb->kac[ iax ][ iccomp ] = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dac", status );

}

