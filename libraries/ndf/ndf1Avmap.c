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

void ndf1Avmap( int iax, NdfACB *acb, const char *type, const char *mode,
                int stdev, void **pntr, size_t *el, int *status ){
/*
*+
*  Name:
*     ndf1Avmap

*  Purpose:
*     Map an NDF's axis variance array for access.

*  Synopsis:
*     void ndf1Avmap( int iax, NdfACB *acb, const char *type,
*                     const char *mode, int stdev, void **pntr, size_t *el,
*                     int *status )

*  Description:
*     This function maps a specified NDF axis variance array for access.
*     The NDF is identified by its ACB entry. Account is taken of the
*     possibility that the required axis variance array may not exist
*     (either because it has not yet been created or because the NDF"s ACB
*     entry contains dimensions which do not exist in the actual data
*     object described in the DCB) and suitable default values are
*     provided. Extrapolated values may also be returned if access to an
*     NDF section extending outside the bounds of the actual data object is
*     required. A new axis structure (including an axis variance array) may
*     be created by this function if necessary.

*  Parameters:
*     iax
*        Zero-based index of the axis whose variance array is to be mapped.
*     acb
*        Pointer to the NDF entry in the ACB.
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used to access the mapped values (case insensitive).
*     mode
*        Pointer to a null terminated string holding the mapping mode to be
*        used to access the values: "READ", "UPDATE" or "WRITE" (case
*        insensitive).
*     stdev
*        Whether conversion of the mapped variance values to standard
*        deviations is required (as opposed to accessing the variance
*        values directly).
*     *pntr
*        Pointer to the mapped array of values. A value of zero will be
*        returned if the function is called with "status" set, or if it
*        should fail for any reason.
*     *el
*        Returned holding the number of axis array elements mapped.
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
   Ary *id;              /* ARY_ identifier for mapped axis array */
   Ary *oldid;           /* Old ARY_ system identifier */
   AryPlace *place;      /* ARY_ system placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char avtype[ NDF__SZTYP + 1 ];  /* Axis variance numeric type */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   hdsdim lbndd[ NDF__MXDIM ];     /* Data object lower bounds */
   hdsdim lbnds;         /* Lower section bounds */
   hdsdim offs[ NDF__MXDIM ];      /* Pixel offset of NDF entry in the ACB */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Data object upper bounds */
   hdsdim ubnds;         /* Upper section bounds */
   int dce;              /* Data conversion error? */
   int exist;            /* Does axis variance array exist? */
   int init;             /* Array must be initialised? */
   int lower;            /* Extrapolate to lower pixel indices? */
   int moder;            /* Read access? */
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

/* Check that the required axis variance array is not already mapped.
   Report an error if it is. */
   if( acb->avmap[ iax ] ) {
      *status = NDF__ISMAP;
      msgSeti( "AXIS", iax );
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The variance array for axis ^AXIS of the NDF structure "
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

/* Ensure that axis variance array information is available in the DCB. */
      ndf1Dav( iax, dcb, status );
      if( *status == SAI__OK ) {

/* Calculate the data object pixel index bounds for the required axis
   array (by transforming from the NDF pixel index system to the data
   object pixel index system). */
         lbnds = lbnd[ iax ] + offs[ iax ];
         ubnds = ubnd[ iax ] + offs[ iax ];

/* See if the required axis variance array exists and whether the NDF
   is a section. */
         exist = ( dcb->avid[ iax ] != NULL );
         sect = acb->cut;

/* Set logical read, update or write flags. */
         moder = 0;
         modeu = 0;
         modew = 0;
         if( astChrMatch( mode, "READ" ) ) {
            moder = 1;
         } else if( astChrMatch( mode, "UPDATE" ) ) {
            modeu = 1;
         } else if( astChrMatch( mode, "WRITE" ) ) {
            modew = 1;
         }

/* Determine the numeric type of the axis variance array. */
         ndf1Avtyp( iax, acb, avtype, sizeof( avtype ), status );
      }

/* If the axis variance array exists and is not a section, then clone
   an ARY_ system identifier for it. */
      if( *status == SAI__OK ) {
         init = 0;
         if( exist && ( !sect ) ) {
            aryClone( dcb->avid[ iax ], &id, status );

/* Otherwise, if it exists and is a section and values must be obtained
   from it, then obtain an ARY_ system identifier for the appropriate
   section of the array. */
         } else if( exist && sect && ( moder || modeu ) ) {
            arySect( dcb->avid[ iax ], 1, &lbnds, &ubnds, &id, status );

/* Otherwise, if the axis variance array does not exist and the NDF is
   a base NDF and values are to be written to the array and the axis
   can be created (i.e. lies within the dimensionality of the actual
   data object), then create a new axis variance array. Clone an ARY_
   system identifier for it. */
         } else if( ( !exist ) && ( !sect ) && ( modew || modeu ) &&
                ( iax <= ndimd ) ) {
            ndf1Avcre( iax, dcb, status );
            aryClone( dcb->avid[ iax ], &id, status );

/* Note if the array's values must be initialised. */
            init = ( !modew );

/* In all other cases the axis variance array will not exist and cannot
   be created (or exists and cannot be modified), so a temporary array
   must be created for the purposes of mapping. This will later be
   discarded when the axis variance array is unmapped. */
         } else {
            aryTemp( &place, status );
            aryNew( avtype, 1, &lbnds, &ubnds, &place, &id, status );

/* Note if the array's values must be initialised. */
            init = ( !modew );
         }

/* The case where update access is required to a section of an existing
   axis variance array needs special attention. In this case values
   must be read from the array, but modifications will later be
   discarded. A similar problem also exists when mapping a pre-existing
   variance array as standard deviation values in READ mode; in this
   case a temporary array is required to allow the mapped values to be
   converted in situ to standard deviations.  Copy the array into a
   temporary array and annul the original identifier. */
         if( *status == SAI__OK ) {
            if( exist && ( ( sect && modeu ) || ( stdev && moder ) ) ) {
               aryTemp( &place, status );
               oldid = id;
               aryCopy( oldid, &place, &id, status );
               aryAnnul( &oldid, status );
            }
         }

/* If values are to be read from a section of an existing axis variance
   array, then it is possible that the section being accessed may
   extend beyond the bounds of the array. Test if it extends beyond the
   lower bound. */
         if( *status == SAI__OK ) {
            lower = 0;
            upper = 0;
            if( exist && sect && ( moder || modeu ) ) {
               lower = ( lbnds < lbndd[ iax ] );

/* Similarly see if the section extends beyond the array's upper bound. */
               upper = ( ubnds > ubndd[ iax ] );
            }

/* Map the array (or derived temporary array) for access. Use READ/ZERO
   access if READ was specified to ensure that the mapped values are
   initialised. */
            if( moder ) {
               aryMap( id, type, "READ/ZERO", pntr, el, status );

/* Otherwise, if initialisation is required, then use WRITE access and
   perform explicit initialisation (this is used instead of /ZERO on
   the access mode to prevent the storage form being implicitly
   changed). */
            } else if( init ) {
               aryMap( id, type, "WRITE", pntr, el, status );
               ndf1Avext( type, 1, lbnds, lbnds, ubnds, *pntr, status );

/* Otherwise, use the access mode as supplied. */
            } else {
               aryMap( id, type, mode, pntr, el, status );
            }

/* If necessary, extrapolate the mapped values to lower pixel indices by
   filling with zeros. */
            if( lower ) {
               ndf1Avext( type, 0, NDF_MIN( lbndd[ iax ] - 1, ubnds ),
                          lbnds, ubnds, *pntr, status );
            }

/* Similarly extrapolate to higher pixel indices if necessary. */
            if( upper ) {
               ndf1Avext( type, 1, NDF_MAX( ubndd[ iax ] + 1, lbnds ),
                          lbnds, ubnds, *pntr, status );
            }
         }

/* If an error occurred, then annul any ARY_ system identifier which may
   have been acquired. */
         if( *status != SAI__OK ) aryAnnul( &id, status );
      }
   }

/* If the variance values were mapped successfully, then see whether
   conversion to standard deviations is required. Before doing so, also
   check that pre-defined values are to be read from the array and that
   the array's contents will not be zero, otherwise conversion is
   superfluous. */
   if( *status == SAI__OK ) {
      if( stdev && exist && ( !modew ) ) {
         ndf1V2s( 1, type, *el, *pntr, &dce, status );

/* If a conversion error occurs, then report context information. */
         if( *status != SAI__OK ) {
            msgSeti( "AXIS", iax );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "Error converting axis variance values into "
                    "standard deviations (errors) for axis ^AXIS of the "
                    "NDF structure ^NDF", status );
         }
      }
   }

/* If no error has occurred (or the only error was to encounter a
   negative standard deviation), then store the ARY_ system identifier
   and pointer for the mapped array. Also store the numeric type used
   for mapping and the access mode (in upper case) along with the
   standard deviation conversion flag.  Mark the axis variance array as
   mapped. */
   if( ( *status == SAI__OK ) || ( *status == NDF__NGVAR ) ) {
      acb->avmid[ iax ] = id;
      acb->avmpt[ iax ] = *pntr;
      astChrCase( type, acb->avmtp[ iax ], 1, sizeof( acb->avmtp[ iax ] ) );
      astChrCase( mode, acb->avmmd[ iax ], 1, sizeof( acb->avmmd[ iax ] ) );
      acb->avmst[ iax ] = stdev;
      acb->avmap[ iax ] = 1;

/* Increment the appropriate DCB mapping counts. */
      dcb->navmp[ iax ]++;
      dcb->nmap++;

/* Return a null pointer under error conditions. */
   } else {
      *pntr = 0;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Avmap", status );

}

