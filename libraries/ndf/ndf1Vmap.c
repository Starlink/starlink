#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"
#include "star/util.h"

void ndf1Vmap( NdfACB *acb, const char *type, int cmplx, const char *mmod,
               int stdev, int mask, void **dpntr, void **ipntr, int *status ){
/*
*+
*  Name:
*     ndf1Vmap

*  Purpose:
*     Map the variance component of an NDF for access.

*  Synopsis:
*     void ndf1Vmap( NdfACB *acb, const char *type, int cmplx,
*                    const char *mmod, int stdev, int mask, void **dpntr,
*                    void **ipntr, int *status )

*  Description:
*     This function maps the variance component of an NDF for access and
*     returns a pointer (or pointers) to the mapped values.  The NDF is
*     identified by its ACB entry. The mapped ACB entry may subsequently be
*     unmapped by the ndf1Vump function and cannot be re-mapped until this
*     has been done.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     type
*        Pointer to a null terminated string holding the numeric data type
*        required for access to the variance values (case insensitive).
*     cmplx
*        Whether access to complex values is required.
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access (case insensitive).
*     stdev
*        Whether conversion of the mapped variance values to standard
*        deviations is required (as opposed to accessing the variance
*        values directly).
*     mask
*        This parameter specifies whether the mapped variance values may
*        later be masked using quality information. If so, then this
*        function will ensure tht a writeable buffer is used to return the
*        mapped values; this may require that a copy of the mapped values
*        be made. If "mask" is zero, then this function may return a read-
*        only copy of the mapped values (i.e. as obtained from HDS).
*     *dpntr
*        Returned holding the pointer to the mapped non-imaginary values.
*     *ipntr
*        Returned holding the pointer to the mapped imaginary values (not
*        used if "cmplx" is zero).
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
   AryPlace *place;      /* ARY_ system temporary placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char *ctype;          /* Complex data type string */
   char *opt;            /* Mapping options string */
   char inopt[ NDF__SZIOP + 1 ];   /* Initialisation option */
   char mode[ NDF__SZMOD + 1 ];    /* Mapping access mode */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   int bad;              /* Bad pixel flag for mapped values */
   int ddce;             /* Non-imaginary data conversion error? */
   int icstat;           /* Imaginary conversion status */
   int idce;             /* Imaginary data conversion error? */
   int nc;               /* String length */
   int ndim;             /* Number of NDF dimensions */
   int rdonly;           /* Values may be in a read-only buffer? */
   int there;            /* Whether variance array exists */
   size_t el;            /* Number of elements mapped */
   void *dpt;            /* Temporary non-imaginary value pointer */
   void *ipt;            /* Temporary imaginary value pointer */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* If the variance component is already mapped through this ACB entry,
   then report an error. */
   if( acb->vmap ) {
      *status = NDF__ISMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The variance component in the NDF structure ^NDF is "
              "already mapped for access through the specified identifier "
              "(possible programming error).", status );

/* Validate the mapping mode, decomposing it into an access mode and an
   initialisation option. */
   } else {
      ndf1Vmmd( mmod, mode, sizeof( mode ), inopt, sizeof( inopt ), status );

/* Ensure that variance information is available in the DCB and ACB. */
      ndf1Vimp( acb, status );

/* See if the ARY_ system identifier for the variance array is valid. If
   not, then the array does not exist. */
      there = aryValid( acb->vid, status );
      if( *status == SAI__OK ) {

/* Set an initial null value for the temporary mapped variance array
   identifier. */
         acb->vmtid = NULL;

/* Initialise the flag indicating if the mapped values may be in a
   read-only buffer. */
         rdonly = ( !strcmp( mode, "READ" ) );

/* Case 1:
   ======
   If the variance array exists, then map it; either for complex or
   non-complex access, as required. */
         if( there ) {
            if( cmplx ) {
               aryMapz( acb->vid, type, mmod, dpntr, ipntr, &el, status );
            } else {
               aryMap( acb->vid, type, mmod, dpntr, &el, status );
            }

/* Obtain the bad pixel flag for the mapped values. */
            aryBad( acb->vid, 0, &bad, status );

/* Case 2:
   ======
   If the variance array does not exist, then see if the access mode and
   initialisation option require it to be created. */
         } else if( ( !strcmp( mode, "WRITE" ) ) || ( ( !strcmp( mode, "UPDATE" ) )
                                                  && ( astChrLen( inopt ) > 0 ) ) ) {

/* If so, then create it (thereby importing identifiers for it into the
   ACB). */
            ndf1Vcre( acb, status );

/* Map the array, as required. */
            if( cmplx ) {
               aryMapz( acb->vid, type, mmod, dpntr, ipntr, &el, status );
            } else {
               aryMap( acb->vid, type, mmod, dpntr, &el, status );
            }

/* Obtain the bad pixel flag for the mapped values. */
            aryBad( acb->vid, 0, &bad, status );

/* Case 3:
   ======
   If the variance array does not exist and READ access with an
   initialisation option was requested, then a temporary array must be
   created.  Determine its bounds from the NDF's data array identifier
   in the ACB. */
         } else if( ( !strcmp( mode, "READ" ) ) && ( astChrLen( inopt ) > 0 ) ) {
            aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain a placeholder for a temporary array. */
            aryTemp( &place, status );

/* Create the array, storing the temporary ARY_ system identifier in the
   ACB. Then map it as required. */

/* ...Complex data. */
            opt = astAppendStringf( NULL, &nc, "WRITE/%s", inopt );
            if( cmplx ) {
               ctype = astAppendStringf( NULL, &nc, "COMPLEX_%s", type );
               aryNew( ctype, ndim, lbnd, ubnd, &place, &acb->vmtid, status );
               aryMapz( acb->vmtid, type, opt, dpntr, ipntr, &el, status );
               ctype = astFree( ctype );

/* ...Non-complex data. */
            } else {
               aryNew( type, ndim, lbnd, ubnd, &place, &acb->vmtid, status );
               aryMap( acb->vmtid, type, opt, dpntr, &el, status );
            }
            opt = astFree( opt );

/* Note the mapped values are in a writeable buffer. */
            rdonly = 0;

/* Obtain the bad pixel flag for the mapped values. */
            aryBad( acb->vmtid, 0, &bad, status );

/* Case 4:
   ======
   If the array does not exist and the access mode is not WRITE and no
   initialisation option was specified, then report an error. */
         } else {
            *status = NDF__VUDEF;
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The variance component in the NDF structure ^NDF "
                    "is in an undefined state.", status );
         }
      }
   }

/* If mapped values are to be modified, but they may reside in a
   read-only buffer, then a modifiable copy must be made in a temporary
   array. Determine the size of this array from the ARY_ system data
   array identifier in the ACB. */
   if( *status == SAI__OK ) {
      if( ( mask || stdev ) && rdonly ) {
         aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain a placeholder for the temporary array. */
         aryTemp( &place, status );

/* Create the array, storing the temporary ARY_ system identifier in
   the ACB. Then map it as required and move the original mapped values
   into it. Unmap the original values and save the pointer(s) to the
   new copy. */

/* ...Complex data. */
         if( cmplx ) {
            ctype = astAppendStringf( NULL, &nc, "COMPLEX_%s", type );
            aryNew( ctype, ndim, lbnd, ubnd, &place, &acb->vmtid, status );
            aryMapz( acb->vmtid, type, "WRITE", &dpt, &ipt, &el, status );
            ndf1Move( type, el, *dpntr, dpt, status );
            ndf1Move( type, el, *ipntr, ipt, status );
            aryUnmap( acb->vid, status );
            ctype = astFree( ctype );
            *dpntr = dpt;
            *ipntr = ipt;

/* ...Non-complex data. */
         } else {
            aryNew( type, ndim, lbnd, ubnd, &place, &acb->vmtid, status );
            aryMap( acb->vmtid, type, "WRITE", &dpt, &el, status );
            ndf1Move( type, el, *dpntr, dpt, status );
            aryUnmap( acb->vid, status );
            *dpntr = dpt;
         }
      }
   }

/* If the variance values were mapped successfully, then see if
   conversion from variances to standard deviation values was
   specified. It is unnecessary to actually convert the values unless
   they have been read from a pre-existing variance array. */
   ddce = 0;
   idce = 0;
   if( *status == SAI__OK ) {
      if( stdev && there && ( strcmp( mode, "WRITE" ) ) ) {

/* If required, then convert the non-imaginary component, noting if a
   data conversion error occurred. */
         ndf1V2s( bad, type, el, *dpntr, &ddce, status );

/* Similarly convert the imaginary component, if present. Ensure that
   this stage executes if a NDF__NGVAR (negative variance) error was
   produced above, so that the mapped values can still be used if
   required. */
         if( cmplx ) {
            if( ( *status == SAI__OK ) || ( *status == NDF__NGVAR ) ) {
               icstat = SAI__OK;
               ndf1V2s( bad, type, el, *ipntr, &idce, &icstat );

/* Update the "status" value if an error occurred during the second
   conversion. */
               if( icstat != SAI__OK ) *status = icstat;
            }
         }
      }
   }

/* If there were no errors (or the only error was to detect negative
   variance values during conversion to standard deviations), then note
   that the ACB entry is mapped and increment the DCB counts of mappings
   to this variance array and of total mappings to this NDF. */
   if( ( *status == SAI__OK ) || ( *status == NDF__NGVAR ) ) {
      acb->vmap = 1;
      dcb->nvmap++;
      dcb->nmap++;

/* Store the mapping type (and complex value flag) and mapping mode in
   the ACB. Also note if conversion from variance to standard deviation
   was specified. */
      star_strlcpy( acb->vmtyp, type, sizeof( acb->vmtyp ) );
      acb->vmcpx = cmplx;
      star_strlcpy( acb->vmmod, mode, sizeof( acb->vmmod ) );
      acb->vmstd = stdev;

/* Store the bad pixel flag value. If conversion errors occurred during
   conversion to standard deviations, then set the bad pixel flag for
   the mapped values to non-zero and note it has been modified. */
      acb->vmbad = bad;
      acb->vmbmd = 0;
      if( ddce || idce ) {
         acb->vmbad = 1;
         acb->vmbmd = 1;
      }

/* Store pointers to the mapped values. */
      acb->vmdpt = *dpntr;
      if( cmplx ) acb->vmipt = *ipntr;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vmap", status );

}

