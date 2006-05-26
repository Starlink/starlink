/*
*+
*  Name:
*     ATL_TEST

*  Purpose:
*     Test installation of the stand-alone ATL package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     PROGRAM

*  Invocation:
*     RUN ATL_TEST

*  Description:
*     This program tests the installation of the stand-alone ATL
*     package. Note, it is not an exhaustive test of the ATL_ system
*     itself. 

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "star/atl.h"
#include "sae_par.h"
#include "mers.h"
#include <stdio.h>

main(){

   int status;

/* Initialise the global status */
   status = sai__ok;



/* To be written........ */




/* If an error occurred, then report a contextual message. */
   if( status != sai__ok ) {
      errRep( "", 'ATL_TEST: ATL_ installation test failed.', status );
   } else {
      msgOut( "", 'ATL_TEST: ATL_ installation test passed.', status );
   }
}
