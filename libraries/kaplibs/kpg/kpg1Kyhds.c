#include "star/hds.h"
#include "star/atl.h"
#include "f77.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

void kpg1Kyhds( AstKeyMap *keymap, int start, int mode, HDSLoc *loc, 
                int *status ){
/*
*  Name:
*     kpg1Kyhds

*  Purpose:
*     Copy values from an AST KeyMap to a primitive HDS object.

*  Language:
*     C.

*  Invocation:
*     void kpg1Kyhds( AstKeyMap *keymap, int start, int mode, HDSLoc *loc, 
*                     int *status )

*  Description:
*     This function fills a specified HDS object with primitive values
*     read from a vector entry in an AST KeyMap. It is the inverse of 
*     kpg1Hdsky. The HDS object must already exist and must be a 
*     primitive array or scalar. The values to store in the HDS object
*     are read sequentially from the vector entry in the specified KeyMap
*     which has a key equal to the name of the HDS object. The length of
*     the vector in the KeyMap can be greater than the number of elements
*     in the HDS object, in which case the index of the first entry to 
*     transfer is given by "start".

*  Arguments:
*     keymap 
*        An AST pointer to the KeyMap.
*     start 
*        The index of the first element to use in the vector entry in the 
*        KeyMap. The first element has index 1 in the Fortran interface
*        (KPG1_KYHDS), and zero in the C interface.
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
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-MAR-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   char name[ DAT__SZNAM + 1 ];
   const char *hdstype;
   int haskey;
   int i;   
   int j;
   int kmtype;
   int prim;
   int veclen;
   size_t el;
   size_t elsize;
   void *data;            
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

/* Skip to the end if there is nothing to do. */
   if( haskey ) {

/* Get the data type of the KeyMap entry. */
      kmtype = astMapType( keymap, name );

/* Get the best data type in which to map the HDS array. */
      if( kmtype == AST__INTTYPE ) {
         hdstype = "_INTEGER";

      } else if( kmtype == AST__DOUBLETYPE ) {
         hdstype = "_DOUBLE";

      } else if( kmtype == AST__FLOATTYPE ) {
         hdstype = "_FLOAT";

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

/* Map the HDS object as a vector using the data type selected above. */
      datMapV( loc, hdstype, "READ", &pntr, &el, status );

/* "i" is the index into the HDS vector and "j" is the idnex into the
   KeyMap vector. Set the initial value for both. */
      if( start > 0 ) {
         j = start;
         i = 0;
      } else {
         j = 0;
         i = -start;
      }

/* Get the entire vector from the KeyMap, and copy the requested section
   into the HDS array. */
      if( kmtype == AST__INTTYPE ) {
         (void) astMapGet1I( keymap, name, veclen, &veclen, (int *) data );
         for( ; i < el && j < veclen; i++, j++ ) {
            ((int *)pntr)[ i ] = ((int *)data)[ j ];
         }            

      } else if( kmtype == AST__DOUBLETYPE ) {
         (void) astMapGet1D( keymap, name, veclen, &veclen, (double *) data );
         for( ; i < el && j < veclen; i++, j++ ) {
            ((double *)pntr)[ i ] = ((double *)data)[ j ];
         }            

      } else if( kmtype == AST__FLOATTYPE ) {
         (void) astMapGet1F( keymap, name, veclen, &veclen, (float *) data );
         for( ; i < el && j < veclen; i++, j++ ) {
            ((float *)pntr)[ i ] = ((float *)data)[ j ];
         }            

      } else {
         (void) atlMapGet1S( keymap, name, elsize*veclen, elsize, &veclen,
                             (char *) data, status );
         for( ; i < el && j < veclen; i++, j++ ) {
            memcpy( ((char*) pntr) + i*elsize, ((char *) data) + j*elsize, 
                    elsize );
         }            
      }

/* Free resources. */
      data = astFree( data );
      datUnmap( loc, status );
   }

}
