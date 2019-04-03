#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "star/util.h"
#include <string.h>
#include "mers.h"

void ndf1Hnew( HDSLoc *loc1, const char *name, const char *type, int ndim,
               const hdsdim dim[], HDSLoc **loc2, int *status ){
/*
*+
*  Name:
*     ndf1Hnew

*  Purpose:
*     Create a new HDS object.

*  Synopsis:
*     void ndf1Hnew( HDSLoc *loc1, const char *name, const char *type,
*                    int ndim, const hdsdim dim[], HDSLoc **loc2, int *status )

*  Description:
*     This function creates a new HDS object. It performs a similar task to
*     the HDS function "datNew", except that it will accept a compound
*     component name consisting of a sequence of component names separated
*     by "." (array slice/cell subscripts may also be included). A complete
*     object specification including a container file name may also be
*     given if the input locator supplied is set to NULL. All
*     components which appear in the "name" parameter except for the last
*     one must already exist.  The object to be created must not itself
*     exist.  If successful, the function returns a locator to the new
*     object. This will be a primary locator if a new container file was
*     opened or created (i.e. if "loc1" was set to NULL), otherwise it
*     will be a secondary locator.

*  Parameters:
*     loc1
*        Locator to an existing HDS structure (or NULL if the "name"
*        parameter contains a complete object specification).
*     name
*        Pointer to a null terminated string holding the relative HDS path
*        name of the object to be created.
*     type
*        Pointer to a null terminated string holding the HDS object type.
*     ndim
*        Number of object dimensions.
*     dim
*        Object dimensions.
*     *loc2
*        Returned holding the locator to the new object.
*     *status
*        The global status.

*  Notes:
*     If this function is called with "status" set, then a NULL locator
*     will be returned via the "loc2" parameter. The same value will also
*     be returned if the function should fail for any reason.

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
   HDSLoc *loc = NULL;   /* Temporary locator */
   char *objnam;         /* New top-level object name */
   char *path;           /* Container file path */
   const char *pdot;     /* Pointer to final delimiting '.' */
   int done;             /* Finished? */
   int primary;          /* Make locator primary? */
   size_t d1;            /* First character in directory name */
   size_t d2;            /* Last character in directory name */
   size_t f1;            /* First character in file name */
   size_t f2;            /* Last character in file name */
   size_t n1;            /* First character in file name field */
   size_t n2;            /* Last character in file name field */
   size_t p1;            /* First character in HDS path */
   size_t p2;            /* Last character in HDS path */
   size_t t1;            /* First character in file type field */
   size_t t2;            /* Last character in file type field */
   size_t v1;            /* First character in file version field */
   size_t v2;            /* Last character in file version field */

/* Initialise the returned locator. */
   *loc2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !name ) return;

/* If the input locator is NULL, then the "name" value contains a
   full object specification, including a container file name. Split
   this into its file name and HDS path fields. */
   done = 0;
   if( !loc1 ) {
      hdsSplit( name, &f1, &f2, &p1, &p2, status );
      if( *status == SAI__OK ) {

/* If the HDS path is absent, then we must create a new top-level
   object. Obtain a name for this object from the name field of the
   container file specification (truncate it if it is too long). */
         if( p1 > p2 ) {
            ndf1Fsplt( name, f1, f2, &d1, &d2, &n1, &n2, &t1, &t2,
                       &v1, &v2, status );
            if( *status == SAI__OK ) {
               n1 += f1;
               n2 += f1;
               if( n1 <= n2 ) {
                  if( n2 - n1 + 1 > DAT__SZNAM ) n2 = n1 + DAT__SZNAM - 1;
                  objnam = ndf1Substr( name, n1, n2, status );
               } else {
                  objnam = "?";
               }

/* Create the new file (and top-level object) and note there is nothing
   more to do. */
               path = ndf1Strip( NULL, name, f1, f2, NULL, NULL, status );
               hdsNew( path, objnam, type, ndim, dim, loc2, status );
               done = 1;

/* Free dynamic strings. */
               if( n1 <= n2 ) objnam = astFree( objnam );
               path = astFree( path );
            }

/* If both a container file name and an HDS path are present, then open
   the container file in update mode. */
         } else {
            path = ndf1Strip( NULL, name, f1, f2, NULL, NULL, status );
            hdsOpen( path, "UPDATE", loc2, status );
            path = astFree( path );
         }
      }

/* If the input locator is not NULL, then find the first and last
   non-blank characters in the HDS path and report an error if it is
   completely blank. */
   } else {
      astFandl( name, 1, 0, &p1, &p2 );
      if( p1 > p2 ) {
         *status = NDF__CNMIN;
         errRep( " ", "No HDS component name given (possible programming "
                 "error).", status );

/* If OK, clone the input locator. */
      } else {
         datClone( loc1, loc2, status );
      }
   }

/* If OK, and there is still an HDS component to create, then search
   backwards from the end of the HDS path for a "." which delimits the
   final name field. */
   if( ( *status == SAI__OK ) && ( !done ) ) {
      pdot = name + p2;
      while( *pdot != '.' && pdot >= name ) pdot--;

/* If the "." occurs at the end of the path, then there is a field
   missing, so report an error. */
      if( pdot == name + p2 ) {
         *status = NDF__CNMIN;
         msgSetc( "NAME", name + p1 );
         errRep( " ", "Missing field in HDS component name '^NAME'.", status );

/* Otherwise, if a "." was found but it was not at the end of the path,
   then search for the structure in which to create the new object,
   using the part of the path which precedes the ".". Promote the
   resulting locator to be a primary locator if necessary (to hold the
   container file open). */
      } else if( pdot >= name ) {
         path = ndf1Strip( NULL, name, p1, pdot - name - 1, NULL, NULL,
                           status );
         hdsFind( *loc2, path, "UPDATE", &loc, status );
         primary = 1;
         if( !loc1 ) datPrmry( 1, &loc, &primary, status );
         path = astFree( path );

/* Retain the locator to the structure. */
         datAnnul( loc2, status );
         *loc2 = loc;
         loc = NULL;
      }

/* Check that the name of the object to be created is valid and create
   the required new object within this structure. */
      if( *status == SAI__OK ) {
         path = ndf1Strip( NULL, name, pdot - name + 1, p2, NULL, NULL,
                           status );
         datChscn( path, status );
         datNew( *loc2, path, type, ndim, dim, status );

/* Obtain a locator to the new object, promoting it if necessary. */
         datFind( *loc2, path, &loc, status );
         path = astFree( path );

         primary = 1;
         if( !loc1 ) datPrmry( 1, &loc, &primary, status );
         datAnnul( loc2, status );
         *loc2 = loc;
         loc = NULL;
      }
   }

/* If an error occurred, then annul the returned locator. */
   if( *status != SAI__OK ) datAnnul( loc2, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hnew", status );

}

