#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>
#include <stdlib.h>

void ary1Maps( AryACB *acb, const char *type, int cmplx, const char *mmod,
               const char *inopt, void **dpntr, void **ipntr, int *status ) {
/*
*+
*  Name:
*     ary1Maps

*  Purpose:
*     Map a simple array.

*  Synopsis:
*     void ary1Maps( AryACB *acb, const char *type, int cmplx,
*                    const char *mode, const char *inopt, void **dpntr,
*                    void **ipntr, int *status )

*  Description:
*     This function maps the data in a simple array using the specified
*     data access type and mapping mode.

*  Parameters:
*     acb
*        The ACB describing the array to be mapped.
*     type
*        The data access type required; an HDS primitive numeric data
*        type string (case insensitive).
*     cmplx
*        Whether the data access type is complex (i.e. whether access
*        is required to an imaginary component in addition to the
*        non-imaginary component).
*     mmod
*        The mapping access mode (either "READ", "UPDATE" or "WRITE",
*        in upper case).
*     inopt
*        The initialisation option (either "ZERO", "BAD" or " ") in upper
*        case. If a NULL pointer is supplied, then any initialisation
*        option appended to the end of "mmod" is used.
*     dpntr
*        Returned holding a pointer to the mapped non-imaginary component
*        of the data.
*     ipntr
*        Returned holding a pointer to the mapped imaginary component of
*        the data (this argument is not used unless "cmplx" is non-zero).
*     status
*        The global status.

* Prior Requirements:
*     -  The DCB mutex must be locked.

*  Notes:
*     -  This function may also be used to map a scaled, delta or
*     primitive array.

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
   AryDCB *dcb;               /* Data object entry in the DCB */
   AryMCB *mcb;               /* Mapping entry in the MCB */
   char mode[ARY__SZMOD+1];   /* Mapping mode buffer */
   char tinop[ARY__SZIOP+1];  /* Temporary initialisation option buffer */
   char vtype[DAT__SZTYP+1];  /* Validated data type string */
   hdsdim dim[ARY__MXDIM];    /* Array (ACB) dimension sizes */
   int bad;                   /* Whether to check for "bad" values */
   int dce;                   /* Data conversion error? */
   int entire;                /* Entire object filled with values? */
   int i;                     /* Loop counter for dimensions */
   int idce;                  /* Imaginary data conversion error? */
   size_t el;                 /* Number of array elements to be mapped */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the access data type string. */
   ary1Vtyp( type, vtype, status );

/* Validate the mapping mode string, decomposing it into an access mode
   and an initialisation option. */
   ary1Vmmd( mmod, mode, tinop, status );

/* If a value has been supplied for "inopt", use it. Otherwise, use the
   init option read fomr "mmod". */
   if( !inopt ) inopt = tinop;

   ARY__MCB_LOCK_MUTEX;

/* Check to see if the array is already mapped for access. Report an error
   if it is. */
   if( acb->mcb ){
      *status = ARY__ISMAP;
      dcb = acb->dcb;
      datMsg( "ARRAY", dcb->loc );
      errRep( " ", "The array ^ARRAY is already mapped for access through the"
              " specified identifier (possible programming error).", status );

/* Check the array access mode to ensure the requested access is permitted. */
   } else {
      ary1Chmod( acb, mode, status );

/* Find a free slot in the MCB and store the MCB pointer in the ACB. */
      mcb = ary1Ffs( ARY__MCBTYPE, status );
      if( *status == SAI__OK ){
         acb->mcb = mcb;

/* Obtain mapping region bounds information, entering this into the MCB. */
         ary1Gmrb( acb, &mcb->mtrex, &mcb->mrful, &mcb->whole, mcb->lmrb,
                   mcb->umrb, mcb->lmtr, mcb->umtr, status );

/* Check for possible conflicting mapped access to the same data object. */
         ary1Chcma( acb, mode, status );

/* Obtain the data object (DCB) and ensure that state information and
   a data object is available. */
         dcb = acb->dcb;
         ary1Dsta( dcb, status );
         ary1Dobj( dcb, status );
         if( *status == SAI__OK ){

/* Calculate the dimension sizes of the array to be mapped and the total
   number of values to be mapped. */
            el = 1;
            for( i = 0; i < acb->ndim; i++ ){
               dim[ i ] = acb->ubnd[ i ] - acb->lbnd[ i ] + 1;
               el *= dim[ i ];
            }

/* Case 1. ======= If the mapping mode is READ or UPDATE and the array's
   data values are defined, then data must be read from the data object.
   See whether it is necessary to check for "bad" values, then map the
   non-imaginary array component for reading. */
            if( ( !strcmp( mode, "READ" ) || !strcmp( mode, "UPDATE" ) ) &&
                dcb->state ){
               bad = acb->bad;
               ary1Mpsr( acb, dcb->dloc, vtype, mode, bad, &mcb->dloc,
                         &mcb->dcopy, &mcb->dpntr, &dce, status );

/* If access to the imaginary component is required and such a component
   exists, then map it for reading, noting whether a data conversion error
   occurs. */
               if( *status == SAI__OK ){
                  if( cmplx ){
                     if( dcb->complex ){
                        idce = 0;
                        ary1Mpsr( acb, dcb->iloc, vtype, mode, bad,
                                  &mcb->iloc, &mcb->icopy, &mcb->ipntr, &idce,
                                  status );
                        dce = dce || idce;

/* If access to an imaginary component is required, but such a component
   does not exist, then a "copy" must be used. Create and map a temporary
   object and fill it with zeros. */
                     } else {
                        mcb->icopy = 1;
                        ary1Cmtmp( vtype, acb->ndim, dim, &mcb->iloc,
                                   &mcb->ipntr, status );
                        ary1Vzero( vtype, el, mcb->ipntr, status );
                     }
                  }

/* Note whether the mapped data may contain "bad" values in (a) the mapping
   transfer region and (b) the padding region which surrounds the actual
   mapped data (if this exists). */
                  mcb->bad = ( acb->bad || dce );
                  mcb->pbad = 1;
               }

/* Case 2. ======= If the mapping mode is WRITE, or it is UPDATE with an
   initialisation option where the arrays's values are undefined, then
   data must be written to the data object when it is subsequently
   unmapped, but there is no need to read initial values now. */
            } else if( !strcmp( mode, "WRITE" ) ||
                     ( !strcmp( mode, "UPDATE" ) && inopt[ 0 ] == ' ' &&
                       !dcb->state ) ){

/* If the data object's values have not yet been initialised, then see if
   the region being mapped covers the entire object. */
               if( !dcb->init ){
                  ary1Inbnd( ARY__MXDIM, mcb->lmtr, mcb->umtr, dcb->ndim,
                             dcb->lbnd, dcb->ubnd, &entire, status );

/* If the region to which WRITE access is being obtained does not cover the
   entire data object, then the object must first be initialised by
   filling it with "bad" values so that undefined data elements don't get
   left around the edges. RFWS removed the code to do this from the
   Fortran version of the ARY library on 11-NOV-1991. The History comment
   was "Temporarily removed initialisation of array regions outside of
   sections accessed in write mode for the first time. This is to improve
   efficiency for applications which process entire arrays in pieces. A
   flag to control this behaviour will have to be added later.". Such a
   flag was never added, and subsequent history has shown it was not
   needed. See the Fortran file ary1_maps.f for the code that was
   commented out here by RFWS. */
               }

/* Now obtain the required access to the data object; map the non-imaginary
   array component for writing. */
               ary1Mpsw( acb, dcb->dloc, vtype, inopt, &mcb->dloc,
                         &mcb->dcopy, &mcb->dpntr, status );

/* If access to the imaginary component is required and such a component
   exists, then map it for writing. */
               if( *status == SAI__OK ){
                  if( cmplx ){
                     if( dcb->complex ){
                        ary1Mpsw( acb, dcb->iloc, vtype, inopt, &mcb->iloc,
                                  &mcb->icopy, &mcb->ipntr, status );

/* If access to an imaginary component is required, but such a component
   does not exist, then a "copy" must be used. Create and map a temporary
   object and initialise its values as required by the initialisation
   option. */
                     } else {
                        mcb->icopy = 1;
                        ary1Cmtmp( vtype, acb->ndim, dim, &mcb->iloc,
                                   &mcb->ipntr, status );
                        ary1Iobw( vtype, inopt, el, mcb->ipntr, status );
                     }
                  }

/* Note whether the mapped data may contain "bad" values in (a) the mapping
   transfer region and (b) the padding region which surrounds the actual
   mapped data (if this exists). */
                  mcb->bad = abs( strcmp( inopt, "ZERO" ) );
                  mcb->pbad = mcb->bad;
               }

/* Note if the data object's values have been initialised. */
               dcb->init = ( dcb->init || ( *status == SAI__OK ) );

/* Case 3. ======= If the access mode is read and an initialisation option
   was supplied (and the array's values are undefined), then a temporary
   copy must be made to hold the initialised values. These will not be
   written back when the array is unmapped. */
            } else if( !strcmp( mode, "READ" ) && ( inopt[ 0 ] != ' ' ) ){

/* Create and map a temporary object and initialise its values as required
   by the initialisation option. */
               mcb->dcopy = 1;
               ary1Cmtmp( vtype, acb->ndim, dim, &mcb->dloc, &mcb->dpntr,
                          status );
               ary1Iobw( vtype, inopt, el, mcb->dpntr, status );

/* If access to an imaginary component is also required, then repeat this
   process. */
               if( cmplx ){
                  mcb->icopy = 1;
                  ary1Cmtmp( vtype, acb->ndim, dim, &mcb->iloc, &mcb->ipntr,
                             status );
                  ary1Iobw( vtype, inopt, el, mcb->ipntr, status );
               }

/* Note whether the mapped data may contain "bad" values in (a) the mapping
   transfer region and (b) the padding region which surrounds the actual
   mapped data (if this exists). */
               mcb->bad = abs( strcmp( inopt, "ZERO" ) );
               mcb->pbad = mcb->bad;

/* Case 4. ======= It is necessary to read values from the array, but these
   values are undefined and an initialisation option has not been
   supplied. Report an error. */
            } else {
               *status = ARY__UNDEF;
               datMsg( "ARRAY", dcb->loc );
               msgSetc( "BADMODE", mode );
               errRep( " ", "^BADMODE access to the array ^ARRAY is not "
                       "available; the array's values are undefined "
                       "(possible programming error).", status );
            }
         }

/* If data mapping was successful, then return the data pointer(s). */
         if( *status == SAI__OK ){
            *dpntr = mcb->dpntr;
            if( cmplx ) *ipntr = mcb->ipntr;

/* Increment the counts of current READ and WRITE mapped access to the data
   object. */
            if( !strcmp( mode, "READ" ) || !strcmp( mode, "UPDATE" ) ){
               (dcb->nread)++;
            }
            if( !strcmp( mode, "WRITE" ) || !strcmp( mode, "UPDATE" ) ){
               (dcb->nwrite)++;
            }

/* Store the mapping access type and mode information in the MCB. */
            ary1Ccpy( vtype, sizeof(mcb->type), mcb->type, status );
            mcb->complex = cmplx;
            ary1Ccpy( mode, sizeof(mcb->amm), mcb->amm, status );

/* If mapping was not successful, then clear the MCB index from the ACB
   (indicating that the array is not mapped) and release the MCB slot. */
         } else {
            acb->mcb = NULL;
            mcb = ary1Rls( (AryObject *) mcb, status );
         }
      }
   }

   ARY__MCB_UNLOCK_MUTEX;

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Maps", status );

}
