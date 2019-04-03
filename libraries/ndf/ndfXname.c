#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "dat_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"

void ndfXname_( int indf, int n, char *xname, size_t xname_length, int *status ){
/*
*+
*  Name:
*     ndfXname

*  Purpose:
*     Obtain the name of the N"th extension in an NDF.

*  Synopsis:
*     void ndfXname( int indf, int n, char *xname, size_t xname_length,
*                    int *status )

*  Description:
*     This function returns the name of the N"th extension in an NDF. If
*     the requested extension does not exist, then the name is returned
*     blank. The function may therefore be used to obtain the names of all
*     the extensions present by setting "n" to 1,2... etc.  until a blank
*     name is returned. Note that the order in which these names are
*     obtained is not defined.

*  Parameters:
*     indf
*        NDF identifier.
*     n
*        The number of the extension whose name is required.
*     xname
*        Pointer to an array in which to return a null terminated string
*        holding the extension name (in upper case).
*     xname_length
*        The length of the supplied 'xname' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZXNM is provided to define the length
*     of character variables which are to hold an NDF extension name. This
*     constant is defined in the header file "ndf.h".

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
   HDSLoc *loc = NULL;   /* Locator to HDS component */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char name[ DAT__SZNAM + 1 ];    /* Name of HDS extension component */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check that the extension number specified is valid and report an
   error if it is not. */
   if( n < 1 ) {
      *status = NDF__XNOIN;
      msgSeti( "N", n );
      errRep( " ", "Invalid extension number ^N specified (possible "
              "programming error).", status );

/* Import the NDF identifier. */
   } else {
      ndf1Impid( indf, &acb, status );

/* Obtain an index to the data object entry in the DCB. */
      if( *status == SAI__OK ) {
         dcb = acb->dcb;

/* Ensure that extension (MORE) structure information is available in
   the DCB. */
         ndf1Dx( dcb, status );
         if( *status == SAI__OK ) {

/* If there is no extension (MORE) structure in the NDF, then set a
   blank name. */
            if( !dcb->xloc ) {
               star_strlcpy( xname, " ", xname_length );

/* Otherwise, mark the error stack and obtain a locator to the N"th
   extension structure component. */
            } else {
               errMark();
               datIndex( dcb->xloc, n, &loc, status );

/* If the component was not found, then annul the error and set a blank
   name. */
               if( *status == DAT__OBJNF ) {
                  errAnnul( status );
                  star_strlcpy( xname, " ", xname_length );

/* Otherwise, obtain the name and copy this to the output parameter,
   checking for possible truncation. */
               } else {
                  datName( loc, name, status );
                  ndf1Ccpy( name, xname, xname_length, status );

/* Annul the locator. */
                  datAnnul( &loc, status );
               }
               loc = NULL;

/* Release the error stack. */
               errRlse();
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXname: Error obtaining the name of the N'th "
              "extension in an NDF.", status );
      ndf1Trace( "ndfXname", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

