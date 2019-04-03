#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "prm_par.h"
#include "mers.h"

void ndf1Amsg( const char *token, NdfACB *acb ){
/*
*+
*  Name:
*     ndf1Amsg

*  Purpose:
*     Assign the name of an NDF identified in the ACB to a message token.

*  Synopsis:
*     void ndf1Amsg( const char *token, NdfACB *acb )

*  Description:
*     This function assigns the full name (including the file name) of an
*     NDF to a message token for use with the ERR_ and MSG_ functions
*     (SUN/104). If an NDF section is supplied, then a dimension bound
*     expression is appended to the data object's name. The NDF is
*     identified by the index of its entry in the ACB.

*  Parameters:
*     token
*        Pointer to a null terminated string holding the name of the
*        message token.
*     acb
*        Pointer to the NDF entry in the ACB.

*  Notes:
*     -  This function has no "status" parameter and does not perform
*     normal error checking. If it should fail, then no value will be
*     assigned to the message token and this will be apparent in the final
*     message.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char *buf = NULL;     /*  Buffer for section bounds expression */
   hdsdim lbnds[ NDF__MXDIM ];     /* Section lower bounds */
   hdsdim offs[ NDF__MXDIM ];      /* Pixel offsets for NDF section */
   hdsdim ubnds[ NDF__MXDIM ];     /* Section upper bounds */
   int *old_status;      /* The old error status pointer used by AST */
   int i;                /* Lopp counter for dimensions */
   int lbnd[ NDF__MXDIM ];         /* Lower bound in base NDF pixel system */
   int n;                /* Number of significant dimensions */
   int nc;               /* No. characters in buffer */
   int ndims;            /* Number of section dimensions */
   int ok;               /* No errors occurred? */
   int status;           /* Local status variable */
   int ubnd[ NDF__MXDIM ];         /* Upper bound in base NDF pixel system */

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Assign the name of the data object to the message token. */
   ndf1Dmsg( token, dcb );

/* If this is an NDF section, then section bounds information must be
   appended. Defer error reporting. */
   if( acb->cut ) {
      errMark();
      status = SAI__OK;
      old_status = astWatch( &status );

/* Obtain the section bounds and pixel offsets. */
      aryBound( acb->did, NDF__MXDIM, lbnds, ubnds, &ndims, &status );
      aryOffs( dcb->did, acb->did, NDF__MXDIM, offs, &status );
      if( status == SAI__OK ) {

/* Convert the section bounds for each possible dimension into the
   corresponding bounds in the base NDF by subtracting the pixel
   offsets. */
         n = 0;
         for( i = 0; i < NDF__MXDIM; i++ ){
            lbnd[ i ] = lbnds[ i ] - offs[ i ];
            ubnd[ i ] = ubnds[ i ] - offs[ i ];

/* Note the last dimension for which the lower and upper bounds are
   significant (i.e. not both equal to 1). */
            if( ( lbnd[ i ] != 1 ) || ( ubnd[ i ] != 1 ) ) n = i + 1;
         }

/* Start constructing the section bounds expression. */
         nc = 0;
         buf = astAppendString( NULL, &nc, "(" );

/* Loop to format the bounds for each dimension. Use the number of
   dimensions in the NDF section, or the number of the last dimension
   which has significant bounds, whichever is larger. */
         for( i = 0; i < NDF_MAX( ndims, n ); i++ ){
            if( i > 0 ) buf = astAppendString( buf, &nc, "," );

/* If we are displaying dimensions which lie outside the NDF section,
   then prefix them with an additional parenthesis. */
            if( i == ndims ) buf = astAppendString( buf, &nc, "(" );

/* Format the lower bound. */
            buf = astAppendStringf( buf, &nc, "%d", lbnd[ i ] );

/* Then add the upper bound, if different. */
            if( ubnd[ i ] != lbnd[ i ] ) {
               buf = astAppendString( buf, &nc, ":" );
               buf = astAppendStringf( buf, &nc, "%d", ubnd[ i ] );
            }
         }

/* Add a closing parenthesis (plus an extra one if the number of NDF
   section dimensions was exceeded). */
         if( n > ndims ) buf = astAppendString( buf, &nc, ")" );
         buf = astAppendString( buf, &nc, ")" );
      }

/* Note whether any error has occurred, If so, then annul it. */
      ok = ( status == SAI__OK );
      if( !ok ) errAnnul( &status );

/* End the error context. */
      astWatch( old_status );
      errRlse();

/* If there were no errors, then append the section bounds expression
   to the message token. */
      if( status == SAI__OK ) msgSetc( token, buf );
   }

/* Free the string holding the bounds expression */
   buf = astFree( buf );

}

