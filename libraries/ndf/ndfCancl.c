#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"

void ndfCancl_( const char *param, int *status ){
/*
*+
*  Name:
*     ndfCancl

*  Purpose:
*     Cancel the association of an NDF with an ADAM parameter.

*  Synopsis:
*     void ndfCancl( const char *param, int *status )

*  Description:
*     This function cancels the association of an NDF with an ADAM
*     parameter. A subsequent attempt to get a value for the parameter will
*     result in a new value being obtained by the underlying parameter
*     system.
*
*     By supplying a blank parameter name, all currently active NDF
*     parameters can be cancelled in a single call. However, it is possible
*     to exclude selected parameters from this automatic cancellation if
*     necessary. To do this, the parameter to be excluded should be marked
*     by making a prior call to this function with an asterisk appended to
*     the end of the parameter name. Any subsequent call to this function
*     with a blank parameter name will skip such marked parameters. To mark
*     all currently active NDF parameters in this way, supply the "param"
*     parameter holding just an asterisk.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter to be cancelled or marked.
*     *status
*        The global status.

*  Notes:
*     -  When cancelling a parameter, the behaviour of this function is
*     identical to "parCancl".
*     -  Any remaining NDF identifiers for the associated NDF are
*     unaffected by this function. It's only affect is to cause ndfAssoc or
*     ndfExist to prompt for a new NDF when called subsequently.
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   char *lparam;         /* Copy of str without leading or trailing spaces */
   const char *key;     /* Current parameter name */
   int ikey;             /* Key index */
   int mark;             /* Mark parameters instead of cancelling? */
   int marked;           /* Non-zero if parameter has been marked */
   int nkey;             /* No. of keys left in KeyMap */
   int there;            /* Was the parameter found in the APB? */
   size_t l;             /* Length of parameter name */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Begin a new error reporting context. */
   errBegin( status );

/* Create a mutable copy of the parameter name excluding any leading or
   trailing spaces. */
   lparam = ndf1Strip( NULL, param, 1, 0, &l, NULL, status );
   if( *status == SAI__OK ) {

/* If the last character is an asterisk, set a flag, note the length
   of the preceeding text and replace the asterisk with a terminating null. */
      if( l == 0 ) {
         mark = 0;
      } else if( lparam[ l - 1 ] == '*' ) {
         mark = 1;
         lparam[ --l ] = 0;
      } else {
         mark = 0;
      }

/* Lock the mutex that serialises access to the APB global variables.
   This prevents other threads accessing them. */
      NDF__APB_LOCK_MUTEX;

/* If a parameter name was supplied... */
      if( l > 0 ) {

/* See if the named parameter is in the APB. */
         if( Ndf_APB_pars ) {
            there = astMapHasKey( Ndf_APB_pars, lparam );
         } else {
            there = 0;
         }

/* If it was found, either mark it by giving it a non-zero value in the
   KeyMap, or cancel and remove it from the KeyMap. */
         if( there ) {
            if( mark ) {
               astMapPut0I( Ndf_APB_pars, lparam, 1, " " );
            } else {
               parCancl( lparam, status );
               astMapRemove( Ndf_APB_pars, lparam );
            }

/* If the parameter was not found, report an error. */
         } else if( *status == SAI__OK ) {
            *status = NDF__NOPAR;
            msgSetc( "P", lparam );
            errRep( " ", "The parameter '^P' is not associated with an NDF "
                    "(possible programming error).", status );
         }

/* If no parameter name was supplied, and a list of parameters is
   available... */
      } else if( Ndf_APB_pars ) {

/* Loop round all parameters in the APB, marking each one, or cancelling
   and removing each one that is not marked. */
         nkey = astMapSize( Ndf_APB_pars );
         ikey = 0;
         while( ikey < nkey && *status == SAI__OK ){
            key = astMapKey( Ndf_APB_pars, ikey );
            if( mark ) {
               astMapPut0I( Ndf_APB_pars, key, 1, " " );
               ikey++;

            } else if( astMapGet0I( Ndf_APB_pars, key, &marked ) ) {
               if( marked == 0 ) {
                  parCancl( key, status );
                  astMapRemove( Ndf_APB_pars, key );
                  nkey--;
               } else {
                  ikey++;
               }
            }
         }
      }

/* Unlock the mutex that serialises access to the APB global variables.
   This allows other threads to access them. */
      NDF__APB_UNLOCK_MUTEX;

   }

/* Free resources. */
   lparam = astFree( lparam );

/*If an error occurred, report context information and call the error
  tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfCancl: Error cancelling an NDF parameter.", status );
      ndf1Trace( "ndfCancl", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

