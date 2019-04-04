#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Plcre( HDSLoc *loc, const char *name, HDSLoc **locpl, int *new,
                int *status ){
/*
*+
*  Name:
*     ndf1Plcre

*  Purpose:
*     Create (or check) an NDF placeholder object.

*  Synopsis:
*     void ndf1Plcre( HDSLoc *loc, const char *name, HDSLoc **locpl,
*                     int *new, int *status )

*  Description:
*     This function may be used to create an NDF placeholder object which
*     identifies a position in the underlying data system where a newly
*     created NDF should be positioned. A pre-existing object may also be
*     used for this purpose so long as it is an empty scalar structure of
*     type NDF, and this function may be used to check such an object for
*     validity. A primary HDS locator for the placeholder object is
*     returned along with an indication of whether a new placeholder object
*     was created.

*  Parameters:
*     loc
*        HDS locator which, in conjunction with the "name" parameter,
*        identifies the structure which is to become a new NDF. A value of
*        NULL may be supplied to indicate that the "name" parameter
*        contains an absolute object name.
*     name
*        Pointer to a null terminated string holding the name to be used
*        together with the "loc" value to identify the placeholder object.
*        If "loc" is set to NULL, this should be the absolute HDS name
*        of the object, otherwise it should be a relative name.
*     *locpl
*        Returned holding the locator to the placeholder object. This will
*        be a primary locator and will be linked into the HDS group
*        "ndf1.h" (to prevent external events from annulling it).
*     *new
*        Returned holding the whether a new placeholder object was created.
*     *status
*        The global status.

*  Notes:
*     -  The object identified by the "loc" and "name" arguments may or may
*     not already exist. If it does not, then it will be created by this
*     function. Otherwise it will be checked for validity (to be valid is
*     should be an empty scalar structure of type NDF).
*     -  If a top-level object is specified, then a new container file will
*     be created (whether or not it already exists). Otherwise, all
*     structures lying above the specified object must already exist.
*     -  If this function is called with "status" set, then an invalid
*     locator will be returned for the "locpl" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.

*  Implementation Deficiencies:
*     There is currently an asymmetry in the way this function works, in
*     that top level objects (but excluding cells of such objects or top
*     level objects referred to directly by the locator "loc") are over-
*     written by the creation of a new container file if they already exist
*     and are never candidates for re-use. In contrast, other objects which
*     already exist are examined to ensure they are empty scalar structures
*     of type NDF and are then re-used if possible (otherwise an error
*     results). This behaviour is historical and due partly to reliance on
*     VMS (version numbers being available on files but not on HDS
*     components) and partly to copying ADAM (which has a similar asymmetry
*     in its parameter system). This should be rationalised at some point.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char type[ DAT__SZTYP + 1 ];    /* Object type */
   hdsdim dim[ DAT__MXDIM ];       /* Object dimension array */
   int ncomp;            /* Number of structure components */
   int ndim;             /* Number of object dimensions */
   int prim;             /* Set to primary ? */
   size_t f1;            /* Index of start of file name */
   size_t f2;            /* Index of end of file name */
   size_t p1;            /* Index of start of HDS path */
   size_t p2;            /* Index of end of HDS path */

/* Set an initial null value for the "locpl" parameter. */
   *locpl = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If a root locator has been supplied, then the "name" value will
   include an HDS container file name. Split it into this file name and
   an HDS path specification. */
   if( !loc ) {
      hdsSplit( name, &f1, &f2, &p1, &p2, status );
      if( *status == SAI__OK ) {

/* If the path name does not exist, then the new NDF will be a top
   level object requiring a new container file. In this case, we can
   pretend that any existing object with the same name does not exist
   (since we will create a new file). */
         if( p1 > p2 ) {
            *new = 1;

/* If a path name exists, then mark the error stack and attempt to find
   the object. */
         } else {
            errMark();
            hdsFind( loc, name, "UPDATE", locpl, status );

/* If no such object was found, then note this fact and annul the
   error. */
            *new = 0;
            if( ndf1Absnt( *status ) ) {
               *new = 1;
               errAnnul( status );
            }
            errRlse();
         }
      }

/* If a root locator was not supplied and "name" is blank, then "loc" must
   identify the new object directly, so it must already exist. Clone a
   locator to it. */
   } else {
      *new = 0;
      if( astChrLen( name ) == 0 ) {
         datClone( loc, locpl, status );

/* Otherwise, defer error reporting and attempt to find the object. */
      } else {
         errMark();
         hdsFind( loc, name, "UPDATE", locpl, status );

/* If the object was not found, then note this fact and annul the
   error. */
         if( ndf1Absnt( *status ) ) {
            *new = 1;
            errAnnul( status );
         }
         errRlse();
      }
   }

/* If the object did not exist, then create it. */
   if( *status == SAI__OK ) {
      if( *new ) {
         ndf1Hnew( loc, name, "NDF", 0, dim, locpl, status );

/* If the object initially existed, then obtain its type and shape. */
      } else {
         datType( *locpl, type, status );
         datShape( *locpl, DAT__MXDIM, dim, &ndim, status );
         if( *status == SAI__OK ) {

/* Check that the type is "NDF" and report an error if it is not. */
            if( strcmp( type, "NDF" ) ) {
               *status = NDF__TYPIN;
               datMsg( "NDF", *locpl );
               msgSetc( "BADTYPE", type );
               errRep( " ", "The object ^NDF has an invalid type of "
                       "'^BADTYPE'; it should be of type 'NDF'.", status );

/* Check that the object is scalar and report an error if it is not. */
            } else if( ndim != 0 ) {
               *status = NDF__NDMIN;
               datMsg( "NDF", *locpl );
               msgSeti( "BADNDIM", ndim );
               errRep( " ", "The object ^NDF is ^BADNDIM-dimensional; it "
                       "should be scalar.", status );
            }

/* Determine the number of existing components in the structure. Report
   an error if this is not zero. */
            datNcomp( *locpl, &ncomp, status );
            if( *status == SAI__OK ) {
               if( ncomp != 0 ) {
                  *status = NDF__INUSE;
                  datMsg( "NDF", *locpl );
                  errRep( " ", "The NDF structure ^NDF is already in use; "
                          "the structure is not empty).", status );
               }
            }
         }
      }

/* Promote the locator to become a primary locator and link it into a
   private group to prevent external events from annulling it. */
      prim = 1;
      datPrmry( 1, locpl, &prim, status );
      hdsLink( *locpl, "NDF_PCB", status );
   }

/* An error occurred, then annul the returned locator. */
   if( *status != SAI__OK ) datAnnul( locpl, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Plcre", status );

}

