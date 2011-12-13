/*
*+
*  Name:
*     smf_ext2km

*  Purpose:
*     Copy the time-indexed extension items into an AST KeyMap.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_ext2km( int indf, const char *xname, AstKeyMap *keymap,
*                      int mode, int *status )

*  Arguments:
*     indf = int (Given)
*        An NDF identifier for the NDF containing the extension items
*        to be read. This should be a time series file spanned by
*        (spectrum,receptor,time slice) axes. It should contain ACSIS and
*        JCMTSTATE extensions.
*     xname = const char * (Given)
*        The name of the NDF extension to read.
*     mode = int (Given)
*        Determines what data is copied from the NDF, and whether it
*        over-writes any existing data in the KeyMap or is appended to
*        the end of any existing data.
*
*        1 - Copy all suitable arrays from the named extension into the
*        KeyMap, erasing any existing entries with the same name.
*
*        2 - Only copy extension items that already have an entry in the
*        KeyMap, appending the new values to the end of the existing
*        values. An error is reported if new data cannot be found in the
*        NDF for any of the existing entries in the KeyMap.
*     keymap = AstKeyMap * (Given)
*        An AST keyMap to hold the primitive array values read from the
*        NDF extension.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function copies values from a named NDF extension into the
*     supplied KeyMap. Each entry in the KeyMap has a key that is equal
*     to the name of the extension component, and a value that is a
*     vectorised list of the values read from the extension component.
*
*     The named extension is searched for primitive array components
*     that have a final trailing axis length equal to the length of the
*     NDFs 3rd pixel axis. An additional constraint is that if "xname" is
*     ACSIS, only arrays with at least 2 axes are used. Any such arrays are
*     vectorised and stored in the KeyMap, in the manner determined by the
*     "mode" argument.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2008 (DSB):
*        Initial version.
*     14-APR-2008 (DSB):
*        Only copy ACSIS arrays that have at least 2 axes. This guards
*        against problems if the number of time slices is equal to the
*        number of detectors.
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

void smf_ext2km( int indf, const char *xname, AstKeyMap *keymap,
                 int mode, int *status ){

/* Local Variables */
   HDSLoc *cloc = NULL;
   HDSLoc *xloc = NULL;
   const char *key = NULL;
   int dim[ NDF__MXDIM ];
   int i;
   int ncomp;
   int ndim;
   int nentry;
   int ntime;
   int prim;
   int there;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get the shape of the NDF, and noe the length of the 3rd axis. */
   ndfDim( indf, 3, dim, &ndim, status );
   ntime = dim[ 2 ];

/* Get a locator to the named extension. */
   ndfXloc( indf, xname, "READ", &xloc, status );

/* First deal with mode 1... */
/* ========================= */
   if( mode == 1 ) {

/* Loop round every component in the extension. */
      datNcomp( xloc, &ncomp, status );
      for( i = 1; i <= ncomp; i++ ) {
         datIndex( xloc, i, &cloc, status );

/* Check the component has primitive values. */
         datPrim( cloc, &prim, status );
         if( prim ) {

/* Get the shape of the component. */
            datShape( cloc, NDF__MXDIM, dim, &ndim, status );

/* Skip over this component if the length of its final axis is not equal
   to the number of time slices. */
            if( ndim > 0 && dim[ ndim - 1 ] == ntime ) {

/* Also skip if we are dealing with the ACSIS extension and the array has
   only 1 (or zero) axes. */
               if( ndim > 1 || strcmp( xname, "ACSIS" ) ) {

/* If it is, store the values as a new entry in "keymap". */
                  kpg1Hdsky( cloc, keymap, 2, 1, status );
               }
            }
         }
         datAnnul( &cloc, status );
      }

/* Now deal with mode 2... */
/* ========================= */
   } else if( mode == 2 ) {

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

/* Check the final axis of the primitive array has a length equal to the
   number of time slices. */
               datShape( cloc, NDF__MXDIM, dim, &ndim, status );
               if( ndim > 0 && dim[ ndim - 1 ] == ntime ) {

/* Also skip if we are dealing with the ACSIS extension and the array has
   only 1 (or zero) axes. */
                  if( ndim > 1 || strcmp( xname, "ACSIS" ) ) {

/* If it is OK, append the values to the end of the existing KeyMap entry. */
                     kpg1Hdsky( cloc, keymap, 1, 3, status );
                  }

               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  msgSetc( "X", xname );
                  msgSetc( "K", key );
                  ndfMsg( "F", indf );
                  errRep( "", "The ^X.^K array has an unexpected shape in "
                          "\"^F\".", status );
               }

            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               msgSetc( "X", xname );
               msgSetc( "K", key );
               ndfMsg( "F", indf );
               errRep( "", "The ^X.^K array has an unexpected data type in "
                       "\"^F\".", status );
            }

            datAnnul( &cloc, status );

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "X", xname );
            msgSetc( "K", key );
            ndfMsg( "F", indf );
            errRep( "", "The ^X.^K array is missing in \"^F\".", status );
         }

      }

/* Now tidy up. */
/* ============ */

/* Report an error if the "mode" value was illegal. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "MODE", mode );
      errRep( "", "smf_ext2km: Illegal value (^MODE) supplied for "
              "argument MODE (programming error).", status );
   }

/* Free resources. */
   datAnnul( &xloc, status );
}

