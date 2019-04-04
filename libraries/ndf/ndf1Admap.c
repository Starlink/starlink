#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ary.h"
#include "mers.h"
#include "ndf_ast.h"
#include "star/util.h"

void ndf1Admap( int iax, NdfACB *acb, const char *type, const char *mode,
                void **pntr, size_t *el, int *status ){
/*
*+
*  Name:
*     ndf1Admap

*  Purpose:
*     Map an NDF's axis data array for access.

*  Synopsis:
*     void ndf1Admap( int iax, NdfACB *acb, const char *type,
*                     const char *mode, void **pntr, size_t *el, int *status )

*  Description:
*     This function maps a specified NDF axis data array (containing the
*     coordinates of the pixel centres in the nominated dimension) for
*     access. The NDF is identified by its ACB entry. Account is taken of
*     the possibility that the required axis data array may not exist
*     (either because it has not yet been created or because the NDF's ACB
*     entry contains dimensions which do not exist in the actual data
*     object described in the DCB) and suitable default values are
*     provided. Extrapolated values may also be returned if access to an
*     NDF section extending outside the bounds of the actual data object is
*     required. A new axis structure (including an axis data array) may be
*     created by this function if necessary.

*  Parameters:
*     iax
*        Zero-based index of the axis whose data array is to be mapped.
*     acb
*        Pointer to the NDF entry in the ACB.
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used to access the mapped values (case insensitive).
*     mode
*        Pointer to a null terminated string holding the mapping mode to be
*        used to access the values: "READ", "UPDATE" or "WRITE" (case
*        insensitive).
*     *pntr
*        Pointer to the mapped array of values. A value of zero will be
*        returned if the function is called with "status" set, or if it
*        should fail for any reason.
*     *el
*        Returned holding the number of axis array elements mapped.
*     *status
*        The global status.

*  Notes:
*     -  The array mapped by this function will be initialised to contain
*     valid axis centre positions, even if WRITE access is specified. This
*     is necessary because mapping of the associated axis width array may
*     itself require use of the mapped axis centre positions for
*     initialisation.

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
   Ary *id;              /* ARY_ identifier for mapped axis array */
   Ary *oldid;           /* Old ARY_ system identifier */
   AryPlace *place;      /* ARY_ system placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char adtype[ NDF__SZTYP + 1 ];  /* Axis data array numeric type */
   double lscale;        /* Lower extrapolation scale factor */
   double lzero;         /* Lower extrapolation zero point */
   double uscale;        /* Upper extrapolation scale factor */
   double uzero;         /* Upper extrapolation zero point */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   hdsdim lbndd[ NDF__MXDIM ];     /* Data object lower bounds */
   hdsdim lbnds;         /* Lower section bounds */
   hdsdim offs[ NDF__MXDIM ];      /* Pixel offset of NDF entry in the ACB */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Data object upper bounds */
   hdsdim ubnds;         /* Upper section bounds */
   int exist;            /* Does axis data array exist? */
   int lower;            /* Extrapolate to lower pixel indices? */
   int modeu;            /* Update access? */
   int modew;            /* Write access? */
   int ndim;             /* Number of NDF dimensions */
   int ndimd;            /* Number of data object dimensions */
   int sect;             /* Is the NDF a section? */
   int upper;            /* Extrapolate to upper pixel indices? */

/* Initialise the returned pointer value. */
   *pntr = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Check that the required axis data array is not already mapped. Report
   an error if it is. */
   if( acb->admap[ iax ] ) {
      *status = NDF__ISMAP;
      msgSeti( "AXIS", iax );
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The centre array for axis ^AXIS of the NDF structure "
              "^NDF is already mapped for access through the specified "
              "identifier (possible programming error).", status );

/* Obtain the pixel index bounds of the NDF's data array and of the
   data array of the actual data object (whose ARY_ system identifier
   is stored in the DCB). */
   } else {
      aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
      aryBound( dcb->did, NDF__MXDIM, lbndd, ubndd, &ndimd, status );

/* Obtain the pixel offsets between the actual data object and the NDF
   which the ACB entry describes. */
      aryOffs( acb->did, dcb->did, NDF__MXDIM, offs, status );

/* Ensure that axis data array information is available in the DCB. */
      ndf1Dad( iax, dcb, status );
      if( *status == SAI__OK ) {

/* Calculate the data object pixel index bounds for the required axis
   array (by transforming from the NDF pixel index system to the data
   object pixel index system). */
         lbnds = lbnd[ iax ] + offs[ iax ];
         ubnds = ubnd[ iax ] + offs[ iax ];

/* See if the required axis data array exists and whether the NDF is a
   section. */
         exist = ( dcb->adid[ iax ] != NULL );
         sect = acb->cut;

/* Set logical read, update or write flags. */
         modeu = 0;
         modew = 0;
         if( astChrMatch( mode, "UPDATE" ) ) {
            modeu = 1;
         } else if( astChrMatch( mode, "WRITE" ) ) {
            modew = 1;
         }

/* Determine the numeric type of the axis data array. */
         ndf1Adtyp( iax, acb, adtype, sizeof( adtype ), status );

/* If the axis data array exists and the NDF is not a section, then
   clone an ARY_ system identifier for it. */
         if( *status == SAI__OK ) {
            if( exist && ( !sect ) ) {
               aryClone( dcb->adid[ iax ], &id, status );

/* Otherwise, if it exists and the NDF is a section, then obtain an
   ARY_ system identifier for the appropriate section of the axis data
   array. */
            } else if( exist && sect ) {
               arySect( dcb->adid[ iax ], 1, &lbnds, &ubnds, &id, status );

/* If modifications may be made to the section's mapped values, then
   make a temporary copy of the section so that the original data
   object will not be affected. */
               if( modeu || modew ) {
                  aryTemp( &place, status );
                  oldid = id;
                  aryCopy( oldid, &place, &id, status );
                  aryAnnul( &oldid, status );
               }

/* Otherwise, if the axis data array does not exist and the NDF is a
   base NDF and values are to be written to the array and the axis can
   be created (i.e. lies within the dimensionality of the actual data
   object), then create an NDF axis structure containing a new axis
   data array. Clone an ARY_ system identifier for the array. */
            } else if( ( !exist ) && ( !sect ) && ( modew || modeu ) &&
                   ( iax <= ndimd ) ) {
               ndf1Acre( dcb, status );
               aryClone( dcb->adid[ iax ], &id, status );

/* In all other cases, the axis data array will not exist and cannot be
   created, so a temporary array must be created for the purposes of
   mapping. This will later be discarded when the axis data array is
   unmapped. */
            } else {
               aryTemp( &place, status );
               aryNew( adtype, 1, &lbnds, &ubnds, &place, &id, status );

/* Map the array, initialise it, and then unmap it. */
               aryMap( id, adtype, "WRITE", pntr, el, status );
               ndf1Adini( adtype, lbnds, ubnds, *pntr, status );
               aryUnmap( id, status );
            }

/* It is possible that a section of an existing axis data array may
   extend beyond the bounds of the associated data object. Test if it
   extends beyond the lower bound and derive extrapolation parameters
   if it does. */
            if( *status == SAI__OK ) {
               lower = 0;
               upper = 0;
               if( exist && sect ) {
                  if( lbnds < lbndd[ iax ] ) {
                     lower = 1;
                     ndf1Gadex( lbndd[ iax ], ubndd[ iax ], dcb->adid[ iax
                                ], 0, &lscale, &lzero, status );
                  }

/* Similarly derive extrapolation parameters if the section extends
   beyond the array's upper bound. */
                  if( ubnds > ubndd[ iax ] ) {
                     upper = 1;
                     ndf1Gadex( lbndd[ iax ], ubndd[ iax ], dcb->adid[ iax
                                ], 1, &uscale, &uzero, status );
                  }
               }
            }

/* Map the array (or derived temporary array) for access. Use UPDATE
   access if WRITE was specified, to ensure that the array's values are
   initialised. Otherwise use the access mode as specified. */
            if( *status == SAI__OK ) {
               if( modew ) {
                  aryMap( id, type, "UPDATE", pntr, el, status );
               } else {
                  aryMap( id, type, mode, pntr, el, status );
               }

/* If necessary, extrapolate the mapped values to lower pixel indices. */
               if( lower ) {
                  ndf1Adext( type, lscale, lzero, 0, NDF_MIN( lbndd[ iax ] - 1, ubnds ),
                             lbnds, ubnds, *pntr, status );
               }

/* Similarly extrapolate to higher pixel indices if necessary. */
               if( upper ) {
                  ndf1Adext( type, uscale, uzero, 1, NDF_MAX( ubndd[ iax ] + 1, lbnds ),
                             lbnds, ubnds, *pntr, status );
               }
            }

/* If an error occurred, then annul any ARY_ system identifier which may
   have been acquired. */
            if( *status != SAI__OK ) aryAnnul( &id, status );
         }
      }
   }

/* Return a null pointer under error conditions. */
   if( *status != SAI__OK ) {
      *pntr = 0;

/* Otherwise, store the ARY_ system identifier and pointer for the
   mapped array, also store the numeric type used for mapping (in upper
   case) and mark the axis data array as mapped. */
   } else {
      acb->admid[ iax ] = id;
      acb->admpt[ iax ] = *pntr;
      star_strlcpy( acb->admtp[ iax ], type, sizeof( acb->admtp[ iax ] ) );
      astChrCase( NULL, acb->admtp[ iax ], 1, 0 );
      acb->admap[ iax ] = 1;

/* Increment the appropriate DCB mapping counts. */
      dcb->nadmp[ iax ]++;
      dcb->nmap++;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Admap", status );

}

