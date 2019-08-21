#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Gtarg( int iarg, char *arg, size_t arg_length, int *there,
                int *status ){
/*
*+
*  Name:
*     ndf1Gtarg

*  Purpose:
*     Translate an environment variable.

*  Synopsis:
*     void ndf1Gtarg( int iarg, char *arg, size_t arg_length, int *there,
*                     int *status )

*  Description:
*     This function returns any of the command line arguments used when
*     invoking the current application. The returned value will be
*     truncated without error if the variable supplied is too short

*  Parameters:
*     iarg
*        The number of the argument required. Argument zero gives the
*        name of the command used to invoke the application. Subsequent
*        arguments return successive tokens from whatever followed this
*        command.
*     arg
*        Pointer to an array in which to return a null terminated string
*        holding the requested command line argument. A null string is
*        stored if the argument does not exist.
*     arg_length
*        The length of the supplied 'arg' array. This should include
*        room for the terminating null.
*     *there
*        Returned holding a flag indicating if the requested argument
*        exists (non-zero if yes, zero if no).
*     *status
*        The global status.

*  Notes:
*     The command line arguments are stored in global variables as part
*     of the initialisation of the NDF library (see function
*     ndf1GetCmdLine in file ndf1Init.c). On some systems, this
*     initialisation of the command line arguments may fail, which will
*     result in null values for "argc" and "argv".

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
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version.

*-
*/

/* Local variables; */
   size_t nc;

/* Initialise. */
   *there = 0;
   arg[0] = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check that the requested argument exists, and get its length. */
   if( iarg < NDF_DCB_argc && NDF_DCB_argv[ iarg ] ) {
      nc = strlen( NDF_DCB_argv[ iarg ] );

/* Indicate it exists. */
      *there = 1;

/* If the argument is shorter than the supplied character string, store
   it. */
      if( nc < arg_length - 1 ) {
         strcpy( arg, NDF_DCB_argv[ iarg ] );

/* Otherwise store as much of it as will fit in the character string and
   then terminate the returned string. Then report an error. */
      } else {
         memcpy( arg, NDF_DCB_argv[ iarg ], arg_length - 1 );
         arg[arg_length - 1] = 0;
         *status = NDF__ARGIN;
         errRepf( " ", "Command line argument was truncated to %zu "
                  "characters: '%s'.", status, arg_length - 1,
                  NDF_DCB_argv[ iarg ] );
      }
   }
}
