#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"
#include <string.h>

void ndf1Gthdt( NdfDCB *dcb, int irec, int ymdhm[], float *sec, int *status ){
/*
*+
*  Name:
*     ndf1Gthdt

*  Purpose:
*     Get the date/time value from a history record.

*  Synopsis:
*     void ndf1Gthdt( NdfDCB *dcb, int irec, int ymdhm[], float *sec,
*                     int *status )

*  Description:
*     This function returns the date/time associated with a record in an
*     NDF history component.

*  Parameters:
*     dcb
*        Pointer to the NDF for which the information is required.
*     irec
*        One-based history record number for which the information is
*        required.
*     ymdhm
*        Returned holding the year, month, day, hour and minute fields of
*        the record"s date/time value (in that order), stored as integers.
*        The supplied "ymdhm" array should have at least "5" elements.
*     *sec
*        Returned holding the seconds field of the date/time value.
*     *status
*        The global status.

*  Notes:
*     A history component must be present in the specified NDF. This
*     function does not check for this.

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
   HDSLoc *cell = NULL;  /* Array cell locator */
   HDSLoc *loc = NULL;   /* Locator to DATE component */
   char type[ DAT__SZTYP + 1 ];    /* Object data type */
   hdsbool_t there;      /* Component present? */
   hdsdim dim[ DAT__MXDIM ];       /* Object dimension sizes */
   hdsdim sub;           /* Array cell subscript */
   int ndim;             /* Number of object dimensions */
   char *pntr;           /* Pointer to mapped string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that history information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Obtain a locator to the required cell of the history record array. */
      sub = irec;
      datCell( dcb->hrloc, 1, &sub, &cell, status );

/* Check whether the mandatory DATE component is present within the
   cell. Report an error if it is not. */
      datThere( cell, "DATE", &there, status );
      if( *status == SAI__OK ) {
         if( !there ) {
            *status = NDF__NOHDT;
            datMsg( "STRUCT", cell );
            errRep( " ", "The DATE component is missing from the NDF "
                    "history record structure ^STRUCT", status );

/* Otherwise, obtain a locator to the DATE component and determine its
   type and shape. */
         } else {
            datFind( cell, "DATE", &loc, status );
            datType( loc, type, status );
            datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the DATE component is of type "_CHAR" and report an error
   if it is not. */
            if( *status == SAI__OK ) {
               if( strncmp( type, "_CHAR*", 6 ) ) {
                  *status = NDF__TYPIN;
                  datMsg( "STRUC", cell );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The DATE component in the NDF history "
                          "record structure ^STRUC has an invalid type of "
                          "'^BADTYPE'; it should be of type '_CHAR'.", status );

/* Also check that the DATE component is scalar and report an error if
   it is not. */
               } else if( ndim != 0 ) {
                  *status = NDF__NDMIN;
                  datMsg( "STRUC", cell );
                  msgSeti( "BADNDIM", ndim );
                  errRep( " ", "The DATE component in the NDF history "
                          "record structure ^STRUC is "
                          "^BADNDIM-dimensional; it should be scalar.", status );
               }
            }

/* Map the DATE component. */
            pntr = ndf1Hmp0C( loc, status );

/* Parse the string and report a contextual error message if necessary. */
            if( pntr ) {
               ndf1Pshdt( pntr, ymdhm, sec, status );
               if( *status != SAI__OK ) {
                  msgSeti( "IREC", irec );
                  datMsg( "HIST", dcb->hloc );
                  errRep( " ", "Unable to obtain date/time information for "
                          "record ^IREC in the NDF history structure "
                          "^HIST.", status );
               }
               pntr = astFree( pntr );
            }

/* Annul the DATE locator. */
            datAnnul( &loc, status );
         }
      }

/* Annul the history record cell locator. */
      datAnnul( &cell, status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Gthdt", status );

}
