#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndfXpt0l_( int value, int indf, const char *xname, const char *cmpt,
                int *status ){
/*
*+
*  Name:
*     ndfXpt0l

*  Purpose:
*     Write a scalar logical value to a component within a named NDF extension.

*  Synopsis:
*     void ndfXpt0l( int value, int indf, const char *xname,
*                    const char *cmpt, int *status )

*  Description:
*     This function writes a scalar logical value to a component within a
*     named NDF extension. The extension must already exist, although the
*     component within the extension need not exist and will be created if
*     necessary.

*  Parameters:
*     value
*        The logical value to be written to the extension component.
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the NDF
*        extension.
*     cmpt
*        Pointer to a null terminated string holding the name of the
*        component within the extension whose value is to be assigned.
*     *status
*        The global status.

*  Notes:
*     -  The value given for the "cmpt" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an object can
*     be accessed in a sub-component (or a sub-sub-component...) of an NDF
*     extension. Array subscripts may also be included. Thus a string such
*     as "CALIB.FILTER(3).WAVELENGTH" could be used as a valid "cmpt"
*     value.
*     -  All HDS structures which lie above the specified component within
*     the extension must already exist, otherwise an error will result.
*     -  If the specified extension component does not already exist, then
*     it will be created by this function. If it already exits, but does
*     not have the correct type or shape, then it will be deleted and a new
*     scalar component with the appropriate type will be created in its
*     place.

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
   HDSLoc *loc = NULL;   /* Extension locator */
   HDSLoc *loc1 = NULL;  /* Component locator */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char rtype[ DAT__SZTYP + 1 ];   /* Required component type */
   char type[ DAT__SZTYP + 1 ];    /* Component type */
   hdsbool_t there;      /* Whether component is present */
   hdsdim dim[ DAT__MXDIM ];       /* Dimension array */
   int ndim;             /* Number of extension dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the extension name for validity. */
   ndf1Chxnm( xname, 1, 0, status );

/* Check that write access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If there is no extension (MORE) structure, then the requested
   extension cannot be there, so report an error. */
         if( !dcb->xloc ) {
            *status = NDF__NOEXT;
            msgSetc( "XNAME", xname );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "There is no '^XNAME' extension in the NDF "
                    "structure ^NDF", status );

/* Otherwise, see if the requested extension is present. */
         } else {
            datThere( dcb->xloc, xname, &there, status );
            if( *status == SAI__OK ) {

/* If absent, then report an error. */
               if( !there ) {
                  *status = NDF__NOEXT;
                  msgSetc( "XNAME", xname );
                  ndf1Amsg( "NDF", acb );
                  errRep( " ", "There is no '^XNAME' extension in the NDF "
                          "structure ^NDF", status );

/* If present, obtain a locator to it. */
               } else {
                  datFind( dcb->xloc, xname, &loc, status );

/* Mark the error stack and attempt to obtain a locator to the requested
   component within the extension. */
                  if( *status == SAI__OK ) {
                     there = 1;
                     errMark();
                     hdsFind( loc, cmpt, "WRITE", &loc1, status );

/* If the component is not present, note this fact and annul the error.
   Release the error stack. */
                     if( ndf1Absnt( *status ) ) {
                        there = 0;
                        errAnnul( status );
                     }
                     errRlse();
                  }

/* Determine the type which the component should have. */
                  if( *status == SAI__OK ) {
                     star_strlcpy( rtype, "_LOGICAL", sizeof( rtype ) );

/* If the component already exists, then determine its actual type and
   shape. */
                     if( there ) {
                        datType( loc1, type, status );
                        datShape( loc1, DAT__MXDIM, dim, &ndim, status );

/* Check whether the component has the required type and is scalar. If
   not, then mark the error stack and attempt to delete it. */
                        if( *status == SAI__OK ) {
                           if( ( strcmp( type, rtype ) ) || ( ndim != 0 ) ) {
                              errMark();
                              ndf1Delob( &loc1, status );

/* If the component could not be deleted because it has invalid bounds
   (e.g. it may be a cell within a primitive array, so cannot be deleted
   independently of the rest of the array), then annul the error. We
   will try and write the value to it anyway, so obtain a new locator
   for it. */
                              if( *status == NDF__BNDIN ) {
                                 errAnnul( status );
                                 hdsFind( loc, cmpt, "WRITE", &loc1, status );

/* Note if the component was deleted successfully. */
                              } else if( *status == SAI__OK ) {
                                 there = 0;
                              }

/* Release the error stack. */
                              errRlse();
                           }
                        }
                     }
                  }

/* If the component is not present (or has been deleted). then create a
   new one with the required type. */
                  if( *status == SAI__OK ) {
                     if( !there ) ndf1Hnew( loc, cmpt, rtype, 0, dim,
                                            &loc1, status );

/* Write the value to it. */
                     datPut( loc1, "_LOGICAL", 0, dim, &value, status );

/* Annul the component locator. */
                     datAnnul( &loc1, status );
                  }

/* Annul the extension locator. */
                  datAnnul( &loc, status );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXpt0l: Error writing a scalar value to a "
              "component within a named NDF extension.", status );
      ndf1Trace( "ndfXpt0l", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

