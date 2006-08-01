/*
*+
*  Name:
*     sc2sim_bolnatcoords

*  Purpose:
*     Get bolometer native coordinates 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_bolnatcoords ( double *xbolo, double *ybolo, int *bol, 
*                           int *status )

*  Arguments:
*     xbolo = double* (Returned)
*        Projected X coords of bolometers
*     ybolo = double* (Returned)
*        Projected Y coords of bolometers
*     bol = int* (Returned)
*        Bolometer counter
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the native coordinates of each bolometer in the array

*  Authors:
*     E.Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-02-28 (EC):
*        Original
*     2006-07-20 (JB):
*        Split from dsim.c

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

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_bolnatcoords 
(
double *xbolo,        /* projected X coords of bolometers (returned) */
double *ybolo,        /* projected Y coords of bolometers (returned) */
int *bol,             /* bolometer counter (returned) */
int *status           /* global status (given and returned) */
)

{
   /* Local variables */
   int i;           /* row counter */
   int j;           /* column counter */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Set the bolometer indices */
   *bol = 0;
   for ( j=0; j<BOLCOL; j++ ) {
      for ( i=0; i<BOLROW; i++ ) {
         xbolo[*bol] = (double)j;
         ybolo[*bol] = (double)i;
         (*bol)++;
      }//for
   }//for

}//sc2sim_bolnatcoords



