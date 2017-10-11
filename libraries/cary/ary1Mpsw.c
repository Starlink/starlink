#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"

void ary1Mpsw( AryACB *acb, HDSLoc *loc, const char *type,
               const char *inopt, HDSLoc **mloc,
               char *copy, void **pntr, int *status ) {
/*
*+
*  Name:
*     ary1Mpsw

*  Purpose:
*     Map a simple array component for WRITE access.

*  Synopsis:
*     void ary1Mpsw( AryACB *acb, HDSLoc *loc, const char *type,
*                    const char *inopt, HDSLoc **mloc,
*                    char *copy, void **pntr, int *status )

*  Description:
*     The routine maps a component (non-imaginary or imaginary) of a
*     simple array for WRITE access and returns a locator and a pointer
*     to the mapped data. The mapped data region may also be
*     initialised to zero or to "bad" values according to the value
*     supplied for the "inopt" argument.

*  Parameters:
*     acb
*        Pointer to the ACB specifying the array whose data
*        are to be mapped.
*     loc
*        Locator to the array data component (non-imaginary or
*        imaginary) to be accessed.
*     type
*        The data type for access; an HDS primitive numeric data type
*        string (case insensitive).
*     inopt
*        The initialisation option; one of ' ', 'ZERO' or 'BAD' (case
*        insensitive).
*     mloc
*        Address at which to return a locator associated with the object
*        that is mapped to provide memory locations for the data.
*     copy
*        Address at which to return a boolean value indicating if the mapped
*        data region is a "copy" of the actual data (as opposed to being a
*        mapped slice of the data object itself).
*     pntr
*        Address at which to return a pointer to the mapped data.
*     status
*        The global status.

*  Notes:
*     -  This routine may also be used to map scaled or primitive arrays.

*  Prior requirements:
*     -  Appropriate entries relating to the region of data to be
*     mapped must already have been set up in the MCB before this
*     routine is called.
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
   AryDCB *dcb;               /* Pointer to DCB */
   AryMCB *mcb;               /* Pointer to MCB */
   hdsdim dim[ARY__MXDIM];    /* Dimensions of mapping region */
   hdsdim diml[ARY__MXDIM];   /* Lower bounds of slice */
   hdsdim dimu[ARY__MXDIM];   /* Upper bounds of slice */
   int i;                     /* Loop counter for dimensions */
   int ndim;                  /* Number of dimensions to use */
   int ndima;                 /* Number of access dimensions */
   int ndimd;                 /* Number of data object dimensions */
   size_t el;                 /* Number of data elements to be mapped */

/* Set an initial value for the returned MLOC argument. */
   *mloc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain pointers for the DCB and MCB. */
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
      el = el * dim[ i ];
   }

/* If the mapping region (and mapping transfer region) comprises the whole
   data object and no data type conversion or scaling is required, then
   clone a locator to the entire data component and map it for WRITE
   access directly using HDS. */
   if( mcb->whole && !strcasecmp( type, dcb->type ) &&
      strcmp( dcb->form, "SCALED" ) && strcmp( dcb->form, "DELTA" ) ){
      datClone( loc, mloc, status );
      datMap( *mloc, type, "WRITE", ndimd, dim, pntr, status );
      *copy = 0;

/* Otherwise, if the mapping transfer region fills the mapping region and
   the data component dimensionality does not exceed the maximum
   dimensionality of an HDS slice and no data type conversion or scaling
   is required, then a slice of the data can be mapped directly using HDS. */
   } else if( mcb->mrful && ( ndimd <= ARY__MXHSL ) &&
              !strcasecmp( type, dcb->type ) &&
              strcmp( dcb->form, "SCALED" ) && strcmp( dcb->form, "DELTA" ) ){

/* Calculate the bounds of the data component slice required. */
      for( i = 0; i < ndimd; i++ ){
         diml[ i ] = mcb->lmrb[ i ] - dcb->lbnd[ i ] + 1;
         dimu[ i ] = mcb->umrb[ i ] - dcb->lbnd[ i ] + 1;
      }

/* Locate the slice and map it for WRITE access. */
      datSlice( loc, ndimd, diml, dimu, mloc, status );
      datMap( *mloc, type, "WRITE", ndimd, dim, pntr, status );
      *copy = 0;

/* In all other cases, a separate copy of the mapped data must be
   maintained, so create and map a temporary object to contain it. */
   } else {
      ary1Cmtmp( type, ndima, dim, mloc, pntr, status );
      *copy = 1;
   }

/* Initialise the mapped data, as required. */
   ary1Iobw( type, inopt, el, *pntr, status );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Mpsw", status );

}
