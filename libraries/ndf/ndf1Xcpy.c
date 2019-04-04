#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include <string.h>

void ndf1Xcpy( HDSLoc *xloc1, int nextn, char extn[][ DAT__SZNAM + 1 ],
               HDSLoc *loc, HDSLoc **xloc2, int *status ){
/*
*+
*  Name:
*     ndf1Xcpy

*  Purpose:
*     Copy an extension structure to a new MORE component.

*  Synopsis:
*     void ndf1Xcpy( HDSLoc *xloc1, int nextn, const char extn[][
*                    DAT__SZNAM + 1 ], HDSLoc *loc, HDSLoc **xloc2,
*                    int *status )

*  Description:
*     This function copies an existing extension (MORE) structure into a
*     new MORE component which it creates (if necessary) in a designated
*     HDS structure.  Selected components of the extension structure may be
*     omitted from the copying operation. The output MORE structure should
*     not exist before this function is called and the function will only
*     create it if it is actually needed to hold extension components (i.e.
*     if the number of components copied is not zero).  A locator to the
*     new structure is returned; this will have the value NULL if
*     creation of an output structure was not necessary.

*  Parameters:
*     xloc1
*        Locator to the input extension (MORE) structure to be copied.
*        A value of NULL may be used to indicate that the input
*        structure does not exist, in which case the "xloc2" parameter
*        will simply be set to the same value.
*     nextn
*        The number of extension components to be excluded from the copying
*        operation (may be zero).
*     extn
*        A list of the names of extension components which are to be
*        excluded from the copying operation. These components need not
*        necessarily exist within the input extension (MORE) structure.
*     loc
*        Locator to an existing HDS structure within which the new output
*        MORE component is to be created.
*     *xloc2
*        Locator to the new MORE structure, if it exists. Otherwise a value
*        of NULL is returned.
*     *status
*        The global status.

*  Notes:
*     -  A value of NULL will be returned for the "xloc2" parameter
*     if this function is called with "status" set, although no further
*     processing will occur.
*     -  A value of NULL will also be returned for the "xloc2"
*     parameter if the function should fail for any reason.

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
   HDSLoc *cloc = NULL;  /* Extension component locator */
   char name[ DAT__SZNAM + 1 ];    /* Extension component name */
   int create;           /* Whether output structure created */
   int exclud;           /* Whether to exclude a component */
   int icomp;            /* Loop counter for extension components */
   int iextn;            /* Loop counter for excluded components */
   int ncomp;            /* Number of extension components */

/* Set an initial value for the "xloc2" parameter. */
   *xloc2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check that the input extension (MORE) structure exists. */
   if( xloc1 ) {

/* See how many components it has. */
      datNcomp( xloc1, &ncomp, status );
      if( *status == SAI__OK ) {

/* There is nothing to do unless it has some components to copy. */
         if( ncomp > 0 ) {

/* If there are no extension components to exclude from the copying
   operation, then copy the entire extension (MORE) structure, including
   all its components. Obtain a locator to the new structure. */
            if( nextn == 0 ) {
               datCopy( xloc1, loc, "MORE", status );
               datFind( loc, "MORE", xloc2, status );

/* Otherwise, each component must be checked before copying. Note
   whether the output extension (MORE) structure has been created yet. */
            } else {
               create = 0;

/* Get a locator to each input extension component in turn and obtain
   its name. */
               for( icomp = 0; icomp < ncomp; icomp++ ){
                  datIndex( xloc1, icomp + 1, &cloc, status );
                  datName( cloc, name, status );
                  if( *status == SAI__OK ) {

/* Search the list of extension components to be excluded to see if
   this name appears. Set the "exclud" flag accordingly. */
                     exclud = 0;
                     for( iextn = 0; iextn < nextn; iextn++ ){
                        if( !strcmp( extn[ iextn ], name ) ) {
                           exclud = 1;
                           break;
                        }
                     }

/* If the extension component is not to be excluded, then an output
   extension (MORE) structure is needed. Check to see if it has been
   created yet. */
                     if( !exclud ) {
                        if( !create ) {

/* If not, then create it and obtain a locator to it. Note it has now
   been created. */
                           datNew( loc, "MORE", "EXT", 0, NULL, status );
                           datFind( loc, "MORE", xloc2, status );
                           create = 1;
                        }

/* Copy the extension component into the output structure. */
                        datCopy( cloc, *xloc2, name, status );
                     }
                  }

/* Annul the input extension component locator. */
                  datAnnul( &cloc, status );

/* Quit the component loop if an error occurs. */
                  if( *status != SAI__OK ) break;
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Xcpy", status );

}

