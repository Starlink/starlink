#include <stdlib.h>
#include "sae_par.h"
#include "star/util.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Nfind( HDSLoc *loc, const char *name, const char *mode,
                NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Nfind

*  Purpose:
*     Find an NDF and import it into the NDF_ system.

*  Synopsis:
*     void ndf1Nfind( HDSLoc *loc, const char *name, const char *mode,
*                     NdfACB **acb, int *status )

*  Description:
*     This function finds an NDF within an HDS structure or container file,
*     imports it into the NDF_ system and returns the index of the ACB
*     entry allocated to it. The object name supplied may include an NDF
*     section specification, in which case an NDF section entry in the ACB
*     will be returned. Otherwise a base NDF entry will be returned.

*  Parameters:
*     loc
*        HDS locator to the structure containing the NDF.
*     name
*        Pointer to a null terminated string holding the name of the
*        structure component (i.e. the NDF).
*     mode
*        Pointer to a null terminated string holding the mode of access
*        required: "READ", "UPDATE" or "WRITE" (this is only used if "loc"
*        is set to NULL, otherwise the mode of access is derived from
*        the input locator).
*     *acb
*        Pointer to the new NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  The value given for the "name" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an NDF can be
*     found in a sub-component (or a sub-sub-component...) of the structure
*     identified by the locator "loc".  Array subscripts may also be used
*     in this component name.  Thus a string such as
*     "MYSTRUC.ZONE(2).IMAGE" could be used as a valid "name" value.
*     -  An NDF can be accessed within an explicitly named container file
*     by supplying the symbolic value NULL for the "loc" parameter,
*     and specifying the container file within the value supplied for the
*     "name" parameter.
*     -  If a blank value is given for the "name" parameter, then the NDF
*     to be imported will be the object identified directly by the locator
*     "loc".
*     -  If this function is called with "status" set, then a value of zero
*     will be returned for the "acb" parameter, although no further
*     processing will occur.  The same value will also be returned if the
*     function should fail for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
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
   HDSLoc *loc1 = NULL;  /* Temporary locator */
   HDSLoc *loc2 = NULL;  /* Temporary locator */
   NdfACB *acb0;         /* Pointer to base ACB entry */
   char *substring = NULL;/* Pointer to dynamic memory holding substring */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode string */
   hdsdim dim[ DAT__MXDIM ];       /* HDS object dimensions */
   int ndim;             /* Number of HDS object dimensions */
   int sect;             /* Final component has to be a section? */
   size_t n1;            /* Start of HDS object name */
   size_t n2;            /* End of HDS object name */
   size_t s1;            /* Start of possible section spec. */
   size_t s2;            /* End of possible section spec. */

/* Set an initial value for the "acb" parameter. */
   *acb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If necessary, check the access mode string for validity. Otherwise,
   check that the supplied locator is not a locator for an array of
   objects. */
   star_strlcpy( vmode, "UPDATE", sizeof( vmode ) );
   if( !loc ) {
      ndf1Vmod( mode, vmode, sizeof( vmode ), status );
   } else {
      datShape( loc, NDF__MXDIM, dim, &ndim, status );
      if( ndim != 0 && *status == SAI__OK ) {
         *status = NDF__DIMIN;
         msgSeti( "ND", ndim );
         errRep( " ", "The supplied HDS object is a ^ND-dimensional array. "
                 "It must be a scalar object (possible programming "
                 "error).", status );
      }
   }

/* Split the NDF name into an HDS object name and a section
   specification. */
   ndf1Nsplt( name, ( loc != NULL ), &n1, &n2, &s1, &s2, status );
   if( *status == SAI__OK ) {

/* Obtain a locator to the required object. */
      if( n1 <= n2 ) {
         substring = ndf1Strip( substring, name, n1, n2, NULL, NULL, status );
         hdsFind( loc, substring, vmode, &loc1, status );
      } else {
         hdsFind( loc, " ", vmode, &loc1, status );
      }

/* See if the HDS name ends in ")". If so, then it has already been
   subscripted, so any further parenthesised expression must be an NDF
   section specification. */
      sect = 0;
      if( n1 <= n2 ) sect = ( name[ n2 - 1 ] == ')' );

/* If there appears to be a section specification on the end of the NDF
   name, then obtain the shape of the HDS object. */
      if( s1 <= s2 ) {
         datShape( loc1, DAT__MXDIM, dim, &ndim, status );
         if( *status == SAI__OK ) {

/* If the final component has to be a section specification (or,
   equivalently, if the object is scalar), then import the object into
   the NDF system and then cut the required NDF section from it. */
            if( sect || ( ndim == 0 ) ) {
               ndf1Imp( loc1, &acb0, status );
               substring = ndf1Strip( substring, name, s1, s2, NULL, NULL, status );
               ndf1Ncut( acb0, substring, acb, status );

/* Annul the original base NDF entry in the ACB. */
               ndf1Anl( &acb0, status );

/* If the final component is not an NDF section specification, then it
   must be an HDS subscript expression. Cut the appropriate slice/cell
   from the HDS object. */
            } else {
               substring = ndf1Strip( substring, name, s1, s2, NULL, NULL,
                                      status );
               hdsFind( loc1, substring, vmode, &loc2, status );

/* Promote the resulting locator to be a primary locator, if necessary,
   before annulling the original and replacing it with the new locator. */
               if( loc ) {
                  int prmry = 1;
                  datPrmry( 1, &loc2, &prmry, status );
               }
               datAnnul( &loc1, status );
               loc1 = loc2;
               loc2 = NULL;

/* Import the object into the NDF system. */
               ndf1Imp( loc1, acb, status );
            }
         }

/* If there does not appear to be a section specification, then simply
   import the object. */
      } else {
         ndf1Imp( loc1, acb, status );
      }

/* Annul the HDS object locator. */
      datAnnul( &loc1, status );
   }

/* Free dynamic strings. */
   substring = astFree( substring );

/* If an error occurred, then annul any ACB entry which may have been
   acquired and call the error tracing function. */
   if( *status != SAI__OK ) {
      ndf1Anl( acb, status );
      ndf1Trace( "ndf1Nfind", status );
   }

}

