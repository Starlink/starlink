#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"

void ndfBegin_( void ){
/*
*+
*  Name:
*     ndfBegin

*  Purpose:
*     Begin a new NDF context.

*  Synopsis:
*     void ndfBegin( void )

*  Description:
*     This function begins a new NDF context. A subsequent call to ndfEnd
*     may then be used to annul all the NDF identifiers (and placeholders)
*     issued since the call to ndfBegin was made.

*  Notes:
*     Matching pairs of calls to ndfBegin and ndfEnd may be nested.

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

/* Ensure the NDF library has been initialised. */
   NDF_INIT( NULL );

/* Get a pointer to thread speciic data */
   NDF_GETTSD;

/* Increment the current identifier context level for the current thread. */
   NDF_TSD(acbIdctx)++;

/* Restablish the original AST status pointer */
   NDF_FINAL

}

