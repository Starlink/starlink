#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Map( NdfACB *acb, const char *comp, const char *type, int cmplx,
              const char *mmod, void *rpntr[], void *ipntr[], int *status ){
/*
*+
*  Name:
*     ndf1Map

*  Purpose:
*     Map an array component of an NDF specified by its ACB entry.

*  Synopsis:
*     void ndf1Map( NdfACB *acb, const char *comp, const char *type,
*                   int cmplx, const char *mmod, void *rpntr[],
*                   void *ipntr[], int *status )

*  Description:
*     This function obtains mapped access to an array component of an NDF
*     specified by its ACB entry. A comma-separated list of component names
*     may also be supplied, in which case an array of pointers to the
*     components is returned (in the order specified by the list).

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the array component
*        name(s); "DATA", "QUALITY" or "VARIANCE" (or "ERROR") (case
*        insensitive).
*     type
*        Pointer to a null terminated string holding the numeric data type
*        to be used to access the data; an HDS primitive numeric data type
*        string (case insensitive).
*     cmplx
*        Whether access to complex data is required.
*     mmod
*        Pointer to a null terminated string holding the mapping mode to be
*        used to access the data (case insensitive).
*     rpntr
*        Returned holding the pointer(s) to the mapped non-imaginary array
*        component(s).
*     ipntr
*        Returned holding the pointer(s) to the mapped imaginary array
*        component(s) (not used if "cmplx" is zero).
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
   char **comps;         /* Array of component name pointers */
   char inopt[ NDF__SZIOP + 1 ];   /* Initialisation option */
   char mode[ NDF__SZMOD + 1 ];    /* Access mode */
   int bad;              /* Quality masking gives bad pixels? */
   int i;                /* Number of arrays for quality masking */
   int icomp;            /* Index of current component name */
   int idptr;            /* Index to data pointer in R/IPNTR */
   int ivptr;            /* Index to variance pointer in R/IPNTR */
   int mask;             /* Whether to apply quality masking */
   int ncomp;            /* Number non-blank components specified */
   void *ptr[ 4 ];       /* Pointers to arrays to quality mask */
   size_t el;            /* Number of elements to quality mask */
   unsigned char badbit; /* Unsigned byte bad-bits mask value */
   void *qpntr;          /* Pointer to mapped quality array */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the mapping mode specification for validity. */
   ndf1Vmmd( mmod, mode, sizeof( mode ), inopt, sizeof( inopt ), status );

/* Check that the requested mode of NDF access is available. */
   ndf1Chmod( acb, mode, status );
   if( *status == SAI__OK ) {

/* Initialise the component count. */
      ncomp = 0;

/* Initialise indices which locate the mapped data and variance array
   pointers in the returned "rpntr" and "ipntr" arrays. */
      idptr = -1;
      ivptr = -1;

/* See if the data and variance arrays, if mapped, may need to be
   masked with the quality information. Masking may be necessary if the
   quality masking flag is set and the access mode is READ or UPDATE. */
      mask = ( acb->qmf && ( ( !strcmp( mode, "READ" ) ) || ( !strcmp( mode, "UPDATE" ) ) ) );

/* If masking may be necessary, then obtain the effective bad-bits
   value for the quality component. If it is zero, then no "bad" pixels
   can be generated, so masking is not necessary. */
      if( mask ) {
         ndf1Gtbb( acb, &badbit, status );
         if( *status == SAI__OK ) mask = ( badbit != 0 );
      }

/* If masking may still be required, then see if a quality array
   exists.  Masking cannot be performed if it does not. */
      if( *status == SAI__OK ) {
         if( mask ) ndf1Qsta( acb, &mask, status );
      }
   }

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component:
   ==============
   Report an error, as this component cannot be mapped. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An AXIS component cannot be mapped "
                    "(possible programming error).", status );

/* DATA component:
   ==============
   Map the data array and note which elements of the returned pointer
   arrays point at it. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            ndf1Dmap( acb, type, cmplx, mmod, mask, rpntr + icomp, ipntr + icomp,
                      status );
            idptr = icomp;

/* ERROR:
   =====
   Map the variance array with conversion from variance values to
   standard deviations. Note which elements of the returned pointer
   arrays point at it. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "ERRORS", NDF__MINAB ) ) {
            ndf1Vmap( acb, type, cmplx, mmod, 1, mask, rpntr + icomp,
                      ipntr + icomp, status );
            ivptr = icomp;

/* EXTENSION:
   =========
   Report an error, as an extension cannot be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An EXTENSION cannot be mapped (possible "
                    "programming error).", status );

/* HISTORY component:
   =================
   Report an error, as this component cannot be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A HISTORY component cannot be mapped "
                    "(possible programming error).", status );

/* LABEL component:
   ===============
   Report an error, as this component cannot be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A LABEL component cannot be mapped "
                    "(possible programming error).", status );

/* QUALITY component:
   ==================
   Map the quality array and note it has been mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            if( cmplx ) {
               *status = NDF__QMPIN;
               errRep( " ", "The QUALITY component of an NDF cannot "
                       "be mapped as complex values (possible "
                       "programming error).", status );
            } else {
               ndf1Qmap( acb, type, mmod, rpntr + icomp, status );

/* If the quality array has been mapped successfully, then reset the
   quality masking flag and note that masking is not required in this
   function. */
               if( *status == SAI__OK ) {
                  acb->qmf = 0;
                  mask = 0;
               }
            }

/* TITLE component:
   ===============
   Report an error, as this component cannot be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A TITLE component cannot be mapped "
                    "(possible programming error).", status );

/* UNITS component:
   ===============
   Report an error, as this component cannot be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A UNITS component cannot be mapped "
                    "(possible programming error).", status );

/* VARIANCE component:
   ==================
   Map the variance array and note which elements of the returned
   pointer arrays point at it. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vmap( acb, type, cmplx, mmod, 0, mask, rpntr + icomp,
                      ipntr + icomp, status );
            ivptr = icomp;

/* If the array component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }

/* If an error occurred because WRITE or UPDATE mode was requested for a
   compressed array (which are read-only), re-report it in a more
   user-friendly way. */
         ndf1Cmpac( acb->dcb, comps[ icomp ], status );
      }

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No array component name specified (possible "
                 "programming error).", status );
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If no error has occurred, then see whether automatic quality masking
   should be used to introduce "bad" pixels into the mapped data and/or
   variance arrays.  Masking is not necessary unless the data or
   variance arrays have been mapped. */
   if( *status == SAI__OK ) {
      mask = ( mask && ( ( idptr != -1 ) || ( ivptr != -1 ) ) );

/* If masking is required, then map the quality array for read access. */
      if( mask ) {
         ndf1Qmap( acb, "_UBYTE", "READ", &qpntr, status );

/* If the mapping operation failed, then add context information to the
   error report. */
         if( *status != SAI__OK ) {
            errRep( " ", "Unable to apply automatic quality masking to "
                    "mapped NDF array values.", status );

/* If the data array was mapped, then put pointer(s) to its real (and
   imaginary) component(s) into the start of the "ptr" array. */
         } else {
            i = 0;
            if( idptr != -1 ) {
               i++;
               ptr[ i - 1 ] = rpntr[ idptr ];
               if( cmplx ) {
                  i++;
                  ptr[ i - 1 ] = ipntr[ idptr ];
               }
            }

/* Similarly, add pointer(s) to the mapped variance array component(s)
   to the end of the "ptr" array. The value of "i" then records the number
   of mapped arrays to which quality masking must be applied and "ptr"(1)
   to "ptr"("i") hold pointers to these arrays. */
            if( ivptr != -1 ) {
               i++;
               ptr[ i - 1 ] = rpntr[ ivptr ];
               if( cmplx ) {
                  i++;
                  ptr[ i - 1 ] = ipntr[ ivptr ];
               }
            }

/* Determine how many array elements there are to be masked from the
   size of the NDF's data array. Then apply the quality mask to the
   mapped data and/or variance arrays. */
            arySize( acb->did, &el, status );
            ndf1Qma( el, qpntr, badbit, type, i, ptr, &bad, status );

/* If bad pixels were introduced into the mapped DATA component by the
   quality masking process, then ensure that the bad pixel flag for the
   mapped values is set to non-zero and note that it has been modified. */
            if( *status == SAI__OK ) {
               if( bad ) {
                  if( idptr != -1 ) {
                     acb->dmbad = 1;
                     acb->dmbmd = 1;
                  }

/* Similary set the bad pixel flag for the mapped variance values if
   appropriate. */
                  if( ivptr != -1 ) {
                     acb->vmbad = 1;
                     acb->vmbmd = 1;
                  }
               }
            }
         }

/* Unmap the quality array. */
         ndf1Qump( acb, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Map", status );

}

