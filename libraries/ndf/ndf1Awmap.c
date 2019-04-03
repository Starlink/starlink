#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include "ndf_ast.h"

void ndf1Awmap( int iax, NdfACB *acb, const char *type, const char *mode,
                void **pntr, size_t *el, int *status ){
/*
*+
*  Name:
*     ndf1Awmap

*  Purpose:
*     Map an NDF's axis width array for access.

*  Synopsis:
*     void ndf1Awmap( int iax, NdfACB *acb, const char *type,
*                     const char *mode, void **pntr, size_t *el, int *status )

*  Description:
*     This function maps a specified NDF axis width array for access. The
*     NDF is identified by its ACB entry. Account is taken of the
*     possibility that the required axis width array may not exist (either
*     because it has not yet been created or because the NDF"s ACB entry
*     contains dimensions which do not exist in the actual data object
*     described in the DCB) and suitable default values are provided.
*     Extrapolated values may also be returned if access to an NDF section
*     extending outside the bounds of the actual data object is required. A
*     new axis structure (including an axis width array) may be created by
*     this function if necessary.

*  Parameters:
*     iax
*        Zero-based index of the axis whose width array is to be mapped.
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
   Ary *id;              /* ARY_ identifier for mapped axis array */
   Ary *idd;             /* Temporary data array identifier */
   Ary *oldid;           /* Old ARY_ system identifier */
   AryPlace *place;      /* ARY_ system placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char awtype[ NDF__SZTYP + 1 ];  /* Axis width numeric type */
   double lwidth;        /* Lower extrapolation width value */
   double uwidth;        /* Upper extrapolation width value */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   hdsdim lbndd[ NDF__MXDIM ];     /* Data object lower bounds */
   hdsdim lbnds;         /* Lower section bounds */
   hdsdim offs[ NDF__MXDIM ];      /* Pixel offset of NDF entry in the ACB */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Data object upper bounds */
   hdsdim ubnds;         /* Upper section bounds */
   int datmap;           /* Axis data array mapped? */
   int dce;              /* Data conversion error? */
   int exist;            /* Does axis width array exist? */
   int lower;            /* Extrapolate to lower pixel indices? */
   int moder;            /* Read access? */
   int modeu;            /* Update access? */
   int modew;            /* Write access? */
   int ndim;             /* Number of NDF dimensions */
   int ndimd;            /* Number of data object dimensions */
   int new;              /* New (or temporary) array created? */
   int sect;             /* Is the NDF a section? */
   int upper;            /* Extrapolate to upper pixel indices? */
   void *dpntr;          /* Temporary data array pointer */

/* Initiallise the returned pointer value. */
   *pntr = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Check that the required axis width array is not already mapped.
   Report an error if it is. */
   if( acb->awmap[ iax ] ) {
      *status = NDF__ISMAP;
      msgSeti( "AXIS", iax );
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The width array for axis ^AXIS of the NDF structure "
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

/* Ensure that axis width array information is available in the DCB. */
      ndf1Daw( iax, dcb, status );
      if( *status == SAI__OK ) {

/* Calculate the data object pixel index bounds for the required axis
   array (by transforming from the NDF pixel index system to the data
   object pixel index system). */
         lbnds = lbnd[ iax ] + offs[ iax ];
         ubnds = ubnd[ iax ] + offs[ iax ];

/* See if the required axis width array exists and whether the NDF is a
   section. */
         exist = ( dcb->awid[ iax ] != NULL );
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

/* Determine the numeric type of the axis width array. */
         ndf1Awtyp( iax, acb, awtype, sizeof( awtype ), status );
      }

/* If the axis width array exists and is not a section, then clone an
   ARY_ system identifier for it. */
      if( *status == SAI__OK ) {
         new = 0;
         if( exist && ( !sect ) ) {
            aryClone( dcb->awid[ iax ], &id, status );

/* Otherwise, if it exists and is a section and values must be obtained
   from it, then obtain an ARY_ system identifier for the appropriate
   section of the array. */
         } else if( exist && sect && ( moder || modeu ) ) {
            arySect( dcb->awid[ iax ], 1, &lbnds, &ubnds, &id, status );

/* If modifications may be made to the section's mapped values, then
   make a temporary copy of the section so that the original data object
   will not be affected. */
            if( modeu ) {
               aryTemp( &place, status );
               oldid = id;
               aryCopy( oldid, &place, &id, status );
               aryAnnul( &oldid, status );
            }

/* Otherwise, if the axis width array does not exist and the NDF is a
   base NDF and values are to be written to the array and the axis can
   be created (i.e. lies within the dimensionality of the actual data
   object), then create a new axis width array. Clone an ARY_ system
   identifier for it. */
         } else if( ( !exist ) && ( !sect ) && ( modew || modeu ) &&
                ( iax <= ndimd ) ) {
            ndf1Awcre( iax, dcb, status );
            aryClone( dcb->awid[ iax ], &id, status );
            new = 1;

/* In all other cases, values will not be written back to the axis
   width array, so a temporary array must be created for the purposes
   of mapping. This will later be discarded when the axis width array
   is unmapped. Create the array and note that a new temporary array
   exists. */
         } else {
            aryTemp( &place, status );
            aryNew( awtype, 1, &lbnds, &ubnds, &place, &id, status );
            new = 1;
         }
      }

/* It is possible that a section of an existing axis width array may
   extend beyond the bounds of the associated data object. Test if it
   extends beyond the lower bound and derive a width value to be used
   for extrapolation if it does. */
      if( *status == SAI__OK ) {
         lower = 0;
         upper = 0;
         if( exist && sect && ( moder || modeu ) ) {
            if( lbnds < lbndd[ iax ] ) {
               lower = 1;
               ndf1Gawex( lbndd[ iax ], ubndd[ iax ], dcb->awid[ iax ], 0,
                          &lwidth, status );
            }

/* Similarly derive a width value to use for extrapolation if it
   extends beyond the array's upper bound. */
            if( ubnds > ubndd[ iax ] ) {
               upper = 1;
               ndf1Gawex( lbndd[ iax ], ubndd[ iax ], dcb->awid[ iax ], 1,
                          &uwidth, status );
            }
         }

/* Map the axis width array (or derived temporary array) for access.
   Use the access mode supplied unless a new (i.e. un-initialised)
   array is being accessed, in which case "WRITE" is needed. */
         if( !new ) {
            aryMap( id, type, mode, pntr, el, status );
         } else {
            aryMap( id, type, "WRITE", pntr, el, status );
         }

/* If necessary, extrapolate the mapped values to lower pixel indices. */
         if( lower ) {
            ndf1Awext( type, 0, NDF_MIN( lbndd[ iax ] - 1, ubnds ), lwidth,
                       lbnds, ubnds, *pntr, status );
         }

/* Similarly, extrapolate to higher pixel indices if necessary. */
         if( upper ) {
            ndf1Awext( type, 1, NDF_MAX( ubndd[ iax ] + 1, lbnds ), uwidth,
                       lbnds, ubnds, *pntr, status );
         }

/* If initialisation of the mapped values is required, then new width
   values must derived from the values in the associated axis data
   array. Test if these are currently mapped and note this fact. */
         if( new && ( moder || modeu ) ) {
            if( acb->admap[ iax ] ) {
               datmap = 1;

/* If so, then create and map a temporary array to hold a double
   precision copy. Transfer the mapped axis data array values to the
   new array, converting them to double precision. */
               aryTemp( &place, status );
               aryNew( "_DOUBLE", 1, &lbnds, &ubnds, &place, &idd, status );
               aryMap( idd, "_DOUBLE", "WRITE", &dpntr, el, status );
               ndf1CvtD( 1, *el, acb->admtp[ iax ], acb->admpt[ iax ],
                         dpntr, &dce, status );

/* If the axis data array is not mapped, then map it as an array of
   double precision values. */
            } else {
               datmap = 0;
               ndf1Admap( iax, acb, "_DOUBLE", "READ", &dpntr, el, status );

/* If an attempt to access the axis data array fails, then report
   context information. */
               if( *status != SAI__OK ) {
                  msgSeti( "AXIS", iax );
                  ndf1Amsg( "NDF", acb );
                  errRep( " ", "Unable to access the axis centre array for "
                          "axis ^AXIS of the NDF structure ^NDF in order "
                          "to initialise the associated width array.", status );
               }
            }

/* Initialise the mapped width values. */
            ndf1Awini( type, lbnds, ubnds, dpntr, *pntr, status );

/* Annul the temporary copy of the axis data array or unmap the axis
   data array itself, as appropriate. */
            if( datmap ) {
               aryAnnul( &idd, status );
            } else {
               ndf1Adump( iax, acb, status );
            }
         }
      }

/* If an error occurred, then annul any ARY_ system identifier which may
   have been acquired. */
      if( *status != SAI__OK ) aryAnnul( &id, status );
   }

/* If no error has occurred, then store the ARY_ system identifier for
   the mapped array and mark the array as mapped. */
   if( *status == SAI__OK ) {
      acb->awmid[ iax ] = id;
      acb->awmap[ iax ] = 1;

/* Increment the appropriate DCB mapping counts. */
      dcb->nawmp[ iax ]++;
      dcb->nmap++;

/* Return a null pointer under error conditions. */
   } else {
      *pntr = 0;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Awmap", status );

}

