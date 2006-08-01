/*
*+
*  Name:
*     sc2sim_bolcoords

*  Purpose:
*     Get bolometer Nasmyth coordinates 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_bolcoords ( char *subname, double ra, double dec, double elevation,
*                        double p, char *domain, int *bol, double xbc[],
*                        double ybc[], int *status )

*  Arguments:
*     lambda = double (Given)
*        Wavelength in metres
*     subname = char* (Given)
*        Subarray name, s8a-s4d
*     ra = double (Given)
*        RA of observation in radians
*     dec = double (Given)
*        Dec of observation in radians
*     elevation = double (Given)
*        Telescope elevation in radians
*     p = double (Given)
*        Parallactic angle in radians
*     domain = char* (Given)
*        AST domain name to be used
*     bol = int* (Returned)
*        Bolometer counter
*     xbc = double[] (Returned)
*        Projected X coords of bolometers
*     ybc = double[] (Returned)
*        Projected Y coords of bolometers
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Use sc2ast to set up an AST frameset for the world coordinates, then
*     use the frameset to get a transformation from pixel numbers to Nasmyth
*     coordinates for the given subarray and calculate coordinates for all
*     the pixels.

*  Authors:
*     B.D.Kelly (ROE)
*     E.Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2005-05-10 (BDK):
*        Original 
*     2005-05-13 (BDK):
*        Pass subname as argument and look-up subnum
*     2005-12-05 (EC):
*        Fixed new API for sc2ast
*     2006-02-28 (EC):
*        Call dsim_bolnatcoords
*     2006-07-19 (JB):
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

/* Standard includes */
#include <math.h>

/* Starlink includes */
#include "ast.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "sc2da/sc2ast.h"

void sc2sim_bolcoords 
(
char *subname,       /* subarray name, s8a-s4d (given) */
double ra,           /* RA of observation in radians (given) */
double dec,          /* Dec of observation in radians (given) */
double elevation,    /* telescope elevation in radians (given) */
double p,            /* parallactic angle in radians (given) */
char *domain,        /* AST domain name to be used (given) */
int *bol,            /* bolometer counter (returned) */
double xbc[],        /* projected X coords of bolometers (returned) */
double ybc[],        /* projected Y coords of bolometers (returned) */
int *status          /* global status (given and returned) */
)

{

   /* Local variables */
   static double bindc[BOLCOL*BOLROW]; /* bolometer indices */
   static double bindr[BOLCOL*BOLROW]; /* bolometer indices */
   AstFrameSet *fset;                  /* World coordinate transformations */
   int subnum;                         /* subarray number */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Set the bolometer indices */

   sc2sim_bolnatcoords( bindc, bindr, bol, status );

   /*
     printf("NBOLO: %i\n",*bol );

     for( i=0; i<*bol; i++ )
     printf(" %e %e\n", bindc[i], bindr[i] );
   */

   /**bol = 0;
      for ( j=0; j<BOLCOL; j++ ) {
      for ( i=0; i<BOLROW; i++ ) {
      bindc[*bol] = (double)j;
      bindr[*bol] = (double)i;
      (*bol)++; 
      }
      }
   */

   /* Set up the frameset */

   sc2ast_name2num ( subname, &subnum, status );

   /* Currently broken since we need to put in the Azimuth
      and the time */
   
   /*sc2ast_createwcs ( subnum, 0.0, elevation, 0.0, fset,
     status );*/

   /* I think this is fixed now EC 05Dec2005 */
   sc2ast_createwcs_compat( subnum, ra, dec, elevation, 0.0, &fset,
			    status );
  
   /* Select the required domain */
   
   sc2ast_getdomain ( domain, fset, status );
   
   /* Find transformations for all the pixels */
   
   astTran2 ( fset, *bol, bindr, bindc, 1, xbc, ybc );
   
   /* Set the current frame back to the one with domain=SKY */

   sc2ast_getdomain ( "SKY", fset, status );

}//sc2sim_bolcoords



