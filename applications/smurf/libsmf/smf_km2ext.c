/*
*+
*  Name:
*     smf_km2ext

*  Purpose:
*     Sort and copy the contents of an AST KeyMap into the time-indexed
*     extension items in an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_km2ext( int indf, const char *xname, AstKeyMap *keymap,
*                      int *timeout, int *status )

*  Arguments:
*     indf = int (Given)
*        An NDF identifier for the NDF into which the extension items are
*        to be written. This should be a time series file spanned by
*        (spectrum,receptor,time slice) axes. It should contain ACSIS and
*        JCMTSTATE extensions propagated from an input NDF (the contents
*        of these extensions will be changed by this routine).
*     xname = const char * (Given)
*        The name of the NDF extension to use.
*     keymap = AstKeyMap * (Given)
*        An AST keyMap holding the primitive array values to store in the
*        NDF extension.
*     timeout = int * (Given)
*        An array that defines the order in which the time slices values
*        in the KeyMap are to be stored in the output NDF. Element "j"
*        of this array holds the index of the time slice within the KeyMap
*        arrays that is to be used as time slice "j" in the output NDF.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function copies values from the supplied KeyMap into a named NDF
*     extension. Each entry in the KeyMap has a key that is equal to the
*     name of the extension component, and a value that is a vectorised
*     list of the values to be put into the extension component.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ndf.h"
#include "mers.h"
#include "star/kaplibs.h"

#include "smf.h"

void smf_km2ext( int indf, const char *xname, AstKeyMap *keymap,
                 int *timeout, int *status ){

/* Local Variables */
   HDSLoc *cloc = NULL;
   HDSLoc *xloc = NULL;
   const char *key = NULL;
   dim_t dim[ NDF__MXDIM ];
   dim_t idim[ NDF__MXDIM ];
   dim_t ntime;
   int i;
   int ndim;
   int nentry;
   int prim;
   int there;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get the shape of the NDF, and note the length of the 3rd axis. */
   ndfDim( indf, 3, idim, &ndim, status );
   ntime = idim[ 2 ];

/* Get a locator to the named extension. */
   ndfXloc( indf, xname, "READ", &xloc, status );

/* Loop round every entry in the KeyMap. */
   nentry = astMapSize( keymap );
   for( i = 0; i < nentry; i++ ) {
      key = astMapKey( keymap, i );

/* See if the supplied extension has a component with the same name. */
      datThere( xloc, key, &there, status );

/* If it did, check the component is primitive. */
      if( there && *status == SAI__OK ) {
         datFind( xloc, key, &cloc, status );
         datPrim( cloc, &prim, status );
         if( prim ) {

/* Modify the final axis of the primitive array to be the same as the
   third axis of the NDF. */
            datShape( cloc, NDF__MXDIM, dim, &ndim, status );
            if( ndim > 0 ) {
               dim[ ndim - 1 ] = ntime;
               datAlter( cloc, ndim, dim, status );

/* Copy the values out of the KeyMap into the HDS array, re-ordering the
   time slices (the last axis) according to the map in "timeout". */
               kpg1Kyhds( keymap, timeout, ndim, 1, cloc, status );

/* Report an error if any of the above checks failed. */
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               msgSetc( "X", xname );
               msgSetc( "K", key );
               ndfMsg( "F", indf );
               errRep( "", "The ^X.^K array had an unexpected shape "
                       "in \"^F\".", status );
            }

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "X", xname );
            msgSetc( "K", key );
            ndfMsg( "F", indf );
            errRep( "", "The ^X.^K array had an unexpected data type "
                    "in \"^F\".", status );
         }

         datAnnul( &cloc, status );

      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "X", xname );
         msgSetc( "K", key );
         ndfMsg( "F", indf );
         errRep( "", "The ^X.^K array was not found in \"^F\".", status );
      }
   }

/* Free resources. */
   datAnnul( &xloc, status );
}

