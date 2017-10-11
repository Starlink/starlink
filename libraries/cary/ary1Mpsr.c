#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include <string.h>

void ary1Mpsr( AryACB *acb, HDSLoc *loc, const char *type,
               const char *mode, char bad, HDSLoc **mloc, char *copy,
               void **pntr, char *dce, int *status ) {
/*
*+
*  Name:
*     ary1Mpsr

*  Purpose:
*     Map a simple array component for READ (or UPDATE) access.

*  Synopsis:
*     void ary1Mpsr( AryACB *acb, HDSLoc *loc, const char *type,
*                    const char *mode, char bad, HDSLoc **mloc, char *copy,
*                    void **pntr, char *dce, int *status )

*  Description:
*     This function maps a component (non-imaginary or imaginary) of a
*     simple array for READ (or UPDATE) access and returns a locator
*     and a pointer to the mapped data.

*  Parameters:
*     acb
*        The ACB specifying the array whose data are to be mapped.
*     loc
*        Locator to the array component (non-imaginary or imaginary) to
*        be accessed.
*     type
*        The data type for access (case insensitive).
*     mode
*        The mode of access required; either 'READ' or 'UPDATE' (case
*        insensitive).
*     bad
*        Whether it is necessary to check for "bad" values during data
*        type conversion.
*     mloc
*        Address at which to return the locator associated with the data
*        object which is mapped to provide memory locations for the data.
*     copy
*        Returned holding a flag indicating whether the mapped data region
*        is a "copy" of the actual data (as opposed to being a mapped slice
*        of the data object itself).
*     pntr
*        Returned holding a pointer to the mapped data.
*     dce
*        Returned holding as flag indicating whether a data conversion
*        error occurred.
*     status
*        The global status.

*  Notes:
*     -  This routine may also be used to map scaled, delta and primitive
*     arrays.

*  Prior requirements:
*     -  Appropriate entries relating to the region of data to be mapped
*     must already have been set up in the MCB before this routine is
*     called.
*     -  Type, bounds and dimensionality information for the data object
*     being mapped must already be available in the DCB.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   AryDCB *dcb;               /* Entry in DCB */
   AryMCB *mcb;               /* Entry in MCB */
   HDSLoc *tloc = NULL;       /* Locator for temporary uncompressed values */
   char extyp[DAT__SZTYP+1];  /* Data type of externally visible data */
   char newbad;               /* Bad values present in section? */
   hdsdim dim[ARY__MXDIM];    /* Dimensions of mapping region */
   hdsdim diml[ARY__MXDIM];   /* Lower bounds of slice */
   hdsdim dimu[ARY__MXDIM];   /* Upper bounds of slice */
   int i;                     /* Loop counter for dimensions */
   int ndim;                  /* Number of dimensions to process */
   int ndima;                 /* Number of access dimensions */
   int ndimd;                 /* Number of data object dimensions */
   size_t el;                 /* Number of data elements to be mapped */
   void *tpntr;               /* Pointer to temporary uncompressed values */

/* Set an initial value for the MLOC argument. */
   *mloc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain indices into the DCB and MCB. */
   dcb = acb->dcb;
   mcb = acb->mcb;

/* Obtain the number of access dimensions and the number of actual data
   object dimensions. */
   ndima = acb->ndim;
   ndimd = dcb->ndim;

/* Calculate the size of the mapping region in each dimension and the total
   number of data elements to be mapped. */
   el = 1;
   ndim = ( ndima > ndimd ) ? ndima : ndimd;
   for( i = 0; i < ndim; i++ ){
      dim[ i ] = mcb->umrb[ i ] - mcb->lmrb[ i ] + 1;
      el *= dim[ i ];
   }

/* Get the effective type of the stored data. */
   ary1Extyp( dcb, extyp, status );

/* If the mapping region (and mapping transfer region) comprises the whole
   data object and no data type conversion, uncompression or scaling is
   required, then clone a locator to the entire data component and map it
   for access directly using HDS. */
   if( mcb->whole && !strcasecmp( type, extyp ) &&
       strcmp( dcb->form, "SCALED" ) && strcmp( dcb->form, "DELTA" ) ){
      datClone( loc, mloc, status );
      datMap( *mloc, type, mode, ndimd, dim, pntr, status );
      *copy = 0;
      *dce = 0;

/* Otherwise, if the mapping transfer region fills the mapping region and
   the data component dimensionality does not exceed the maximum
   dimensionality of an HDS slice and no data type conversion or scaling
   is required, then a slice of the data can be mapped directly using HDS. */
   } else if( mcb->mrful && ( ndimd <= ARY__MXHSL ) &&
              !strcasecmp( type, extyp ) &&
              strcmp( dcb->form, "SCALED" ) && strcmp( dcb->form, "DELTA" ) ){

/* Calculate the bounds of the data component slice required. */
      for( i = 0; i <  ndimd; i++ ){
         diml[ i ] = mcb->lmrb[ i ] - dcb->lbnd[ i ] + 1;
         dimu[ i ] = mcb->umrb[ i ] - dcb->lbnd[ i ] + 1;
      }

/* Locate the slice and map it. */
      datSlice( loc, ndimd, diml, dimu, mloc, status );
      datMap( *mloc, type, mode, ndimd, dim, pntr, status );
      *copy = 0;
      *dce = 0;

/* Otherwise, if the mapping transfer region fills the mapping region and
   the data is stored as a delta compressed array, and there is no change
   of dimensionality, and no change of data type, then we can uncompress
   the required section. */
   } else if( mcb->mrful && !strcmp( dcb->form, "DELTA" ) &&
              !strcasecmp( type, extyp ) && ( ndima == ndimd ) ){

/* Create and map a temporary object. */
      ary1Cmtmp( type, ndimd, dim, mloc, pntr, status );

/* Get the bounds of the MRB in the grid indices of the whole uncompressed
   array. */
      for( i = 0; i <  ndimd; i++ ){
         diml[ i ] = mcb->lmrb[ i ] - dcb->lbnd[ i ] + 1;
         dimu[ i ] = mcb->umrb[ i ] - dcb->lbnd[ i ] + 1;
      }

/* Uncompress the required section. */
      ary1Undlt( dcb->loc, ndimd, diml, dimu, *pntr, &newbad, status );
      *copy = 1;
      *dce = 0;

/* Otherwise, if the mapping transfer region doesn't fill the mapping
   region, but it nevertheless exists, then the mapped data will be
   surrounded by "bad" values, so a copy must be made. */
   } else if( mcb->mtrex ){

/* Create and map a temporary object to hold the output values. */
      ary1Cmtmp( type, ndima, dim, mloc, pntr, status );

/* If the array is delta compressed, we first uncompress the required
   section into another temporary array before copying it into the
   returned array. */
      if( !strcmp( dcb->form, "DELTA" ) ){

/* Get the bounds of the MTR in the grid indices of the whole uncompressed
   array. */
         for( i = 0; i <  ndimd; i++ ){
            diml[ i ] = mcb->lmtr[ i ] - dcb->lbnd[ i ] + 1;
            dimu[ i ] = mcb->umtr[ i ] - dcb->lbnd[ i ] + 1;
            dim[ i ] = dimu[ i ] - diml[ i ] + 1;
         }

/* Create and map a temporary HDS array to hold the uncompressed values. */
         ary1Cmtmp( extyp, ndimd, dim, &tloc, &tpntr, status );

/* Uncompress the values in the MTR, storing them in the above temporary
   HDS array. */
         ary1Undlt( dcb->loc, ndimd, diml, dimu, tpntr, &newbad, status );

/* Unmap the temporary array so that the values written into it by
   ary1Undlt will be available when the array is mapped again within
   ary1Gtn. */
         datUnmap( tloc, status );

/* Copy the uncompressed values into the returned array, doing type
   conversion and padding with bad values as necessary. */
         ary1Gtn( bad, extyp, tloc, ndim, mcb->lmtr, mcb->umtr, mcb->lmtr,
                  mcb->umtr, type, mcb->lmrb, mcb->umrb, 1, dcb->scloc,
                  *pntr, dce, status );

/* Annul the temporary array holding the uncompressed values. */
         ary1Antmp( &tloc, status );

/* Now handle non DELTA arrays. */
      } else {

/* Ensure any required scale and zero terms are available. */
         ary1Dscl( dcb, status );

/* Read the data in the mapping transfer region into the returned temporary
   object, padding the rest of the mapped array with "bad" values. */
         ary1Gtn( bad, dcb->type, loc, ndim, dcb->lbnd, dcb->ubnd, mcb->lmtr,
                  mcb->umtr, type, mcb->lmrb, mcb->umrb, 1, dcb->scloc, *pntr,
                  dce, status );
      }

      *copy = 1;

/* In all other cases, there is no data to be transferred, so simply create
   and map a temporary object and fill it with "bad" values. */
   } else {
      ary1Cmtmp( type, ndima, dim, mloc, pntr, status );
      ary1Vbad( type, el, *pntr, status );
      *copy = 1;
      *dce = 0;
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Mpsr", status );

}

