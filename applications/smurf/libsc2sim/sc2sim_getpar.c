/*
*+
*  Name:
*     sc2sim_getpar

*  Purpose:
*     Get parameters from arguments

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getpar ( int argc, char **argv, struct dxml_struct *inx, 
*                     struct dxml_sim_struct *sinx, int *rseed, 
*                     int *savebols, int *status )

*  Arguments:
*     argc = int (Given)
*        Argument count
*     argv = char** (Given)
*        Argument list
*     inx = dxml_struct* (Returned)
*        Structure for values from XML file
*     sinx = dxml_sim_struct* (Returned)
*        Structure for values from XML file
*     rseed = int* (Returned)
*        Seed for random number generator
*     savebols = int* (Returned)
*        Flag for writing bolometer details
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Get and interpret parameters.

*  Authors:
*     H.W. van Someren Greve (ASTRON)
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2001-11-08 (Greve):
*        Original
*     2002-08-21 (BDK):
*        C version
*     2003-06-26 (BDK):
*        Introduce XML file specifying simulation
*     2003-08-19 (BDK):
*        Get parameters from arguments
*     2005-05-13 (BDK):
*        Remove DREAM-specific checks
*     2006-07-20 (JB):
*        Split from dsim.c
*     2006-09-05 (JB):
*        Check for obsXML and simXML files

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
#include <stdlib.h>

/* SC2SIM includes */
#include "sc2sim.h"
#include "dream.h"
#include "dxml.h"

#include "ast.h"
#include "fitsio.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "sae_par.h"

#define FUNC_NAME "sc2sim_getpar"

void sc2sim_getpar
( 
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
int *savebols,           /* flag for writing bolometer details (returned) */
int *status              /* global status (given and returned) */
)

{
   /* Local variables */
   char obs_name[132];            /* XML observation file name */
   char sim_name[132];            /* XML simulation file name */

   int dlength;                   /* length of date string */
   int tlength;                   /* length of time string */
   char cur_day[16];              /* date string */ 
   char cur_time[16];             /* time string */ 

   /* Check status */
   if ( !StatusOkP(status) ) return;

   dlength = 16;
   tlength = 16;

   strcpy ( obs_name, argv[1] );

   strcpy ( sim_name, argv[2] );

   *rseed = atoi ( argv[3] );
   *savebols = 0;

   if ( argc == 6 ) {
      if ( ( strcmp ( "t", argv[5] ) == 0 ) ||
           ( strcmp ( "T", argv[5] ) == 0 ) ) {
         *savebols = 1;
      }
   }

   /*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );

   if ( *status != SAI__OK ) {
     msgSetc ( "FILENAME", obs_name );
     msgOut(FUNC_NAME, "Cannot find file ^FILENAME", status);
     return;
   }  
    
   dxml_returnXML ( inx, status );

   dxml_readsimXML ( sim_name, status );

   if ( *status != SAI__OK ) {
     msgSetc ( "FILENAME", obs_name );
     msgOut(FUNC_NAME, "Cannot find file ^FILENAME", status);
     return;
   } 

   dxml_returnsimXML ( sinx, status );

   dream_timenow( dlength, tlength, 0, cur_day, cur_time, NULL, NULL, status );

}
