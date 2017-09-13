#include "star/hds.h"
#include "star/atl.h"
#include "f77.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "kaplibs.h"
#include "sae_par.h"

void kpg1Kyhds( AstKeyMap *keymap, const int *map, int axis, int mode,
                HDSLoc *loc, int *status ){
/*
*  Name:
*     kpg1Kyhds

*  Purpose:
*     Copies values from an AST KeyMap to a primitive HDS object.

*  Language:
*     C.

*  Invocation:
*     void kpg1Kyhds( AstKeyMap *keymap, const int *map, int axis, int mode,
*                     HDSLoc *loc, int *status )

*  Description:
*     This function fills a specified HDS object with primitive values
*     read from a vector entry in an AST KeyMap. It is the inverse of
*     kpg1Hdsky. The HDS object must already exist and must be a
*     primitive array or scalar. The values to store in the HDS object
*     are read from the KeyMap entry that has a key equal to the name
*     of the HDS object. The vector read from the KeyMap is interpreted
*     as an N-dimension array, where N is the number of dimensions in the
*     HDS object. Array slices can be re-arranged as they are copied from
*     KeyMap to HDS object. The "axis" argument specifies which axis is
*     being re-arranged. Each array slice is perpendicular to this axis.
*     The KeyMap array and the HDS array are assumed to have the same
*     dimensions on all other axes.

*  Arguments:
*     keymap
*        An AST pointer to the KeyMap.
*     map
*        An array which indicates how to map slices in the KeyMap array
*        onto slices in the HDS array. The length of the supplied array
*        should be equal to the HDS array dimension specified by "axis".
*        Element J of this array says where the data for the J'th slice
*        of the HDS array should come from, where J is the index along
*        the axis specified by "axis". The value of element J is a
*        zero-based index along axis "axis" of the array read from the KeyMap.
*     axis
*        The index of the axis to be re-arranged. The first axis is axis 1.
*        Note, the current version of this routine only allows the array
*        to be re-arranged on the last axis. An error is reported if any
*        other axis is specified. This may change in the future.
*     mode
*        Specifies what happens if the supplied KeyMap does not contain
*        an entry with the name of the supplied HDS object.
*
*        1 - Report an error.
*
*        2 - Do nothing
*     loc
*        An HDS locator for a primitive scalar or array object.
*     status
*        The inherited status.

*  Notes:
*     - An error is reported if the supplied HDS object is a structure.

*  Copyright:
*     Copyright (C) 2008,2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-MAR-2008 (DSB):
*        Original version.
*     2010-10-04 (TIMJ):
*        Add support for short/word keymap entries.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   char name[ DAT__SZNAM + 1 ];
   const char *hdstype;
   hdsdim dim[ NDF__MXDIM ];
   int ndim;
   int haskey;
   int hslice;
   int i;
   int kdim;
   int kmtype;
   int kslice;
   int prim;
   int step;
   int veclen;
   size_t elsize;
   size_t el;
   void *data;
   void *hp;
   void *kp;
   void *pntr;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the name for the supplied HDS object. */
   datName( loc, name, status );

/* Check the supplied HDS object is primitive. */
   datPrim( loc, &prim, status );
   if( !prim && *status == SAI__OK ) {
      msgSetc( "N", name );
      *status = SAI__ERROR;
      errRep( "", "KPG1_KYHDS: Supplied HDS Object \"^N\" is not primitive "
              "(programming error).", status );
   }

/* Check the "mode" value is within bounds. */
   if( mode < 1 || mode > 2 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "M", mode );
         errRep( "", "KPG1_KYHDS: Supplied value for MODE (^M) is illegal "
                 "(programming error).", status );
      }
   }

/* See if the keymap has an entry with the same name. */
   haskey = astMapHasKey( keymap, name );

/* If required, report an error. */
   if( !haskey && mode == 1 ){
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "N", name );
         errRep( "", "KPG1_KYHDS: The name \"^N\" does not exist "
                 "(programming error).", status );
      }
   }

/* Get the shape of the HDS array. */
   datShape( loc, NDF__MXDIM, dim, &ndim, status );

/* Check the supplied value of "axis" is OK. */
   if( axis < 1 || axis > ndim ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "A", axis );
         msgSeti( "N", ndim );
         errRep( "", "KPG1_KYHDS: Supplied value for AXIS (^A) is illegal "
                 "- it should be in the range 1 to ^N (programming error).",
                 status );
      }
   }

/* Skip to the end if there is nothing to do. */
   if( haskey && *status == SAI__OK ) {

/* Get the data type of the KeyMap entry. */
      kmtype = astMapType( keymap, name );

/* Get the best data type in which to map the HDS array. */
      if( kmtype == AST__INTTYPE ) {
         hdstype = "_INTEGER";

      } else if( kmtype == AST__SINTTYPE ) {
         hdstype = "_WORD";

      } else if( kmtype == AST__DOUBLETYPE ) {
         hdstype = "_DOUBLE";

      } else if( kmtype == AST__FLOATTYPE ) {
         hdstype = "_REAL";

      } else if( kmtype == AST__STRINGTYPE ) {
         hdstype = "_CHAR";

      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "N", name );
         errRep( "", "KPG1_KYHDS: Cannot copy KeyMap entry \"^N\" - "
                 "KeyMap data type has no HDS equivalent.", status );
      }

/* Get the number of bytes needed to store a single value. */
      if( kmtype == AST__INTTYPE ) {
         elsize = sizeof( int );

      } else if( kmtype == AST__SINTTYPE ) {
         elsize = sizeof( short );

      } else if( kmtype == AST__DOUBLETYPE ) {
         elsize = sizeof( double );

      } else if( kmtype == AST__FLOATTYPE ) {
         elsize = sizeof( float );

      } else {
         elsize = astMapLenC( keymap, name );
      }

/* Allocate memory to hold the values read from the KeyMap. */
      veclen = astMapLength( keymap, name );
      data = astMalloc( elsize*veclen );

/* Get the entire vector from the KeyMap. */
      if( kmtype == AST__INTTYPE ) {
         (void) astMapGet1I( keymap, name, veclen, &veclen, (int *) data );

      } else if( kmtype == AST__SINTTYPE ) {
         (void) astMapGet1S( keymap, name, veclen, &veclen, (short *) data );

      } else if( kmtype == AST__DOUBLETYPE ) {
         (void) astMapGet1D( keymap, name, veclen, &veclen, (double *) data );

      } else if( kmtype == AST__FLOATTYPE ) {
         (void) astMapGet1F( keymap, name, veclen, &veclen, (float *) data );

      } else {
         (void) atlMapGet1C( keymap, name, elsize*veclen, elsize, &veclen,
                             (char *) data, status );
      }

/* Map the HDS object as a vector using the data type selected above. */
      datMapV( loc, hdstype, "WRITE", &pntr, &el, status );

/* Assuming the KeyMap and HDS arrays have the same dimensions on all
   axes other than that specified "axis", find the length of the KeyMap
   array on axis "axis". */
      kdim = ( veclen*dim[ axis - 1 ] )/el;

/* For the moment, we can only re-order the last axis. */
      if( axis != ndim ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSeti( "A", axis );
            msgSeti( "N", ndim );
            errRep( "", "Currently KPG1_KYHDS can only re-order the last "
                    "axis, but axis ^A (of ^N) was specified (programming "
                    "error).", status );
         }
      }

/* Get the vector step between slices. */
      step = 1;
      for( i = 0; i < axis - 1; i++ ) step *= dim[ i ];

/* Loop round each slice in the HDS array. */
      for( hslice = 0; hslice < dim[ axis - 1 ]; hslice++ ) {

/* Get the index of the corresponding slice in the KeyMap array, and pass
   on if it is not within the bounds of the KeyMap array. */
         kslice = map[ hslice ];
         if( kslice >= 0 && kslice < kdim ) {

/* Get pointers to the start of the slice in KeyMap and HDS arrays. */
            hp = ( (char *) pntr ) + hslice*step*elsize;
            kp = ( (char *) data ) + kslice*step*elsize;

/* Copy the slice data. */
            memcpy( hp, kp, elsize*step );
         }
      }

/* Free resources. */
      data = astFree( data );
      datUnmap( loc, status );
   }

}
