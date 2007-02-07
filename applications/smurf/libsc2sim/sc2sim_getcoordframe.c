/*
*+
*  Name:
*     sc2sim_getcoordframe

*  Purpose:
*     Calculate coordframe enumerated type from string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getcoordframe ( char *name, int *status )

*  Arguments:
*     name = char* (Given)
*        String containing name of coordinate frame (nasmyth, azel, radec)
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Given a string for the observation return the enumerated value
*     as defined in dsim_struct.h. Valid modes:
*     stare, dstare, dream, pong, polspin, heatrun, hits
* 
*     Returns none if the string could not be matched to a valid type

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-09-14 (EC):
*        Original

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>

/* SC2SIM includes */
#include "sc2sim.h"

mapCoordframe sc2sim_getcoordframe( char *name, int *status ) {
   /* Check status */
   if ( !StatusOkP(status) ) return none;

   if( strcmp( name, "NASMYTH" ) == 0 ) return nasmyth;
   else if( strcmp( name, "AZEL" ) == 0 ) return azel;
   else if( strcmp( name, "RADEC" ) == 0 ) return radec;
   else return nocoord;

}
