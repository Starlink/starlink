/*
*+
*  Name:
*     sc2sim_getobspar.c

*  Purpose:
*     Read observation parameters from keymap file and store in 
*     sc2sim_obs struct.  

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getobspar ( AstKeyMap *keymap, struct sc2sim_obs_struct *inx, 
*                        int *status )

*  Arguments:
*     keymap = AstKeyMap* (Given)
*        Keymap containing obs parameters
*     sinx = sc2sim_obs_struct* (Returned)
*        Structure for values from obs keymap file
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Retrieve obs parameters and store in sc2sim_obs_struct.
*     If a parameter is unspecified, set it to a reasonable default value.

*  Authors:
*     J. Balfour (UBC)
*     A.G. Gibb (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-09-15 (JB):
*        Original
*     2006-10-04 (JB):
*        Replace strcpy with strncpy, replace pong_gridcount with
*        pong_width and pong_height
*     2006-10-12 (AGG):
*        - RA and Dec were not being stored in the inx struct
*        - Delete wt0_name and wt1_name
*     2006-10-13 (AGG):
*        inx.nvert now set to a sensible (i.e non-zero) default value

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
#include <ctype.h>

/* SC2SIM includes */
#include "sc2sim.h"

#include "smurf_par.h"
#include "libsmf/smf.h"

/* Starlink Includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

#define FUNC_NAME "sc2sim_getobspar"

void sc2sim_getobspar ( AstKeyMap *keymap, struct sc2sim_obs_struct *inx, 
                        int *status ) {

   char *convert;         /* String for converting values */
   char *curtok=NULL;     /* current jig vertex being parsed */
   double dec;            /* Double representation of Dec */
   int grid_max_x;        /* The reconstruction grid max X */
   int grid_max_y;        /* The reconstruction grid max Y */ 
   int grid_min_x;        /* The reconstruction grid min X */
   int grid_min_y;        /* The reconstruction grid min Y */
   int i = 0;             /* Loop counter */
   int ix;                /* grid offset */
   int iy;                /* grid offset */
   int j = 0;             /* Array index */
   int nvert_x=0;         /* Number of jig_x vertices */
   int nvert_y=0;         /* Number of jig_Y vertices */
   double ra;             /* Double representation of RA */
   const char *temp;      /* Temporary string for retrieving string values */
   int vert_x[SC2SIM__MXVERT]; /* Temporary array for x-vertices */
   int vert_y[SC2SIM__MXVERT]; /* Temporary array for y-vertices */

   /* Check status */
   if ( !StatusOkP(status) ) return;
 
   temp = smf_malloc ( SC2SIM__FLEN, sizeof (*temp), 1, status );
   convert = smf_malloc ( SC2SIM__FLEN, sizeof (*convert), 1, status );

   if ( !astMapGet0D ( keymap, "BOL_DISTX", &(inx->bol_distx) ) )
      inx->bol_distx = 6.28;

   if ( !astMapGet0D ( keymap, "BOL_DISTY", &(inx->bol_disty) ) )
      inx->bol_disty = 6.28; 

   if ( !astMapGet0C ( keymap, "BOLFILE", &temp ) )
      strncpy ( inx->bolfile, "", SC2SIM__FLEN ); 
   else
      strncpy ( inx->bolfile, temp, SC2SIM__FLEN);

   if ( !astMapGet0D ( keymap, "BOUS_ANGLE", &(inx->bous_angle) ) )
      inx->bous_angle = 0.4636476; 

   if ( !astMapGet0D ( keymap, "BOUS_WIDTH", &(inx->bous_width) ) )
      inx->bous_width = 2000.0;

   if ( !astMapGet0D ( keymap, "BOUS_HEIGHT", &(inx->bous_height) ) )
      inx->bous_height = 2000.0;

   if ( !astMapGet0D ( keymap, "BOUS_SPACING", &(inx->bous_spacing) ) )
      inx->bous_spacing = 240.0;

   if ( !astMapGet0D ( keymap, "BOUS_VMAX", &(inx->bous_vmax) ) )
      inx->bous_vmax = 200.0;

   if ( !astMapGet0I ( keymap, "CONV_SHAPE", &(inx->conv_shape) ) )
      inx->conv_shape = 1;

   if ( !astMapGet0D ( keymap, "CONV_SIG", &(inx->conv_sig) ) )
      inx->conv_sig = 1.0;

   if ( !astMapGet0C ( keymap, "COORDFRAME", &temp ) )
      strncpy ( inx->coordframe, "NASMYTH", 80 ); 
   else {
      strncpy ( convert, temp, 80 );
      /* Convert to uppercase */
      i = 0;
      while ( *convert != '\0' ) {
         *convert = toupper (*convert);
         convert++;
         i++;
      }
      convert = convert - i;
      strncpy ( inx->coordframe, convert, 80 );
   }

   if ( !astMapGet0C ( keymap, "DEC", &temp ) )
      inx->dec = 0.0;
   else {
      /* Get the double representation of the sexagesimal string and
         convert from hours to radians */
      strncpy ( convert, temp, 80 );
      sc2sim_sex2double ( convert, &dec, status );
      dec *= DH2R;
      inx->dec = dec;
   }

   if ( !astMapGet0D ( keymap, "DISTFAC", &(inx->distfac) ) )
      inx->distfac = 0.0;

   if ( !astMapGet0C ( keymap, "FLATNAME", &temp ) )
      strncpy ( inx->flatname, "", SC2SIM__FLEN );
   else
      strncpy ( inx->flatname, temp, SC2SIM__FLEN ); 

   if ( !astMapGet0I ( keymap, "GRID_MAX_X", &grid_max_x ) )
      grid_max_x = 1;

   if ( !astMapGet0I ( keymap, "GRID_MAX_Y", &grid_max_y ) )
      grid_max_y = 1;

   if ( !astMapGet0I ( keymap, "GRID_MIN_X", &grid_min_x ) )
      grid_min_x = 1;

   if ( !astMapGet0I ( keymap, "GRID_MIN_Y", &grid_min_y ) )
      grid_min_y = 1;

   if ( !astMapGet0D ( keymap, "GRID_STEP_X", &(inx->grid_step_x) ) )
      inx->grid_step_x = 6.28;

   if ( !astMapGet0D ( keymap, "GRID_STEP_Y", &(inx->grid_step_y) ) )
      inx->grid_step_y = 6.28;

   if ( !astMapGet0D ( keymap, "HEATNUM", &(inx->heatnum) ) )
      inx->heatnum = 1.0;

   if ( !astMapGet0D ( keymap, "HEATSTART", &(inx->heatstart) ) )
      inx->heatstart = 0.0;

   if ( !astMapGet0D ( keymap, "HEATSTEP", &(inx->heatstep) ) )
      inx->heatstep = 0.0;

   if ( !astMapGet0D ( keymap, "JIG_STEP_X", &(inx->jig_step_x) ) )
      inx->jig_step_x = 6.28;

   if ( !astMapGet0D ( keymap, "JIG_STEP_Y", &(inx->jig_step_y) ) )
      inx->jig_step_y = 6.28;

   /* Retrieve the string representation of the jig_pos.x.  If
      there is no provided string, supply a default. */
   if ( !astMapGet0C ( keymap, "JIG_POS.X", &temp ) ) {
      nvert_x = 8;
      vert_x[0] = 0;
      vert_x[1] = -1;
      vert_x[2] = 1;
      vert_x[3] = -1;
      vert_x[4] = 0;
      vert_x[5] = 1;      
      vert_x[6] = -1;
      vert_x[7] = 1; 
   } else {

      /* Parse the string and retrieve the values */
      strncpy ( convert, temp, 80 );
      curtok = strtok ( convert, ";" );
      while ( curtok != NULL ){
         nvert_x++;
         if ( nvert_x > SC2SIM__MXVERT ) {
            *status = SAI__ERROR;
            msgOut(FUNC_NAME, 
                   "Number of x vertices exceeds maximum", status); 
            return;
         }
         vert_x[nvert_x] = atoi ( curtok );
         curtok = strtok ( NULL, ";" );
      }
   }

   /* Retrieve the string representation of the jig_pos.y.  If
      there is no provided string, supply a default. */
   if ( !astMapGet0C ( keymap, "JIG_POS.Y", &temp ) ) {
      nvert_y = 8;
      vert_y[0] = 1;
      vert_y[1] = -1;
      vert_y[2] = 0;
      vert_y[3] = 1;
      vert_y[4] = -1;
      vert_y[5] = 1;      
      vert_y[6] = 0;
      vert_y[7] = -1; 
   } else {

      /* Parse the string and retrieve the values */
      strncpy ( convert, temp, 80 );
      curtok = strtok ( convert, ";" );
      while ( curtok != NULL ){
         nvert_y++;
         if ( nvert_y > SC2SIM__MXVERT ) {
            *status = SAI__ERROR;
            msgOut(FUNC_NAME, 
                   "Number of y vertices exceeds maximum", status); 
            return;
         }
         vert_y[nvert_y] = atoi ( curtok );
         curtok = strtok ( NULL, ";" );
      }
   }

   /* Check to make sure the number of vertices is equal, and
      store them in the inx structure */
   if ( nvert_x != nvert_y ) {
      *status = SAI__ERROR;
      msgOut(FUNC_NAME, 
             "Number of vertices is not equal", status); 
      return;
   } else  if ( nvert_x == 0 || nvert_y == 0 ) {
      *status = SAI__ERROR;
      msgOut(FUNC_NAME, 
             "Error parsing jig vertices", status); 
      return;
   }

   for ( i = 0; i < nvert_x; i++ ) {
      (inx->jig_vert)[nvert_x][0] = vert_x[i];
      (inx->jig_vert)[nvert_x][1] = vert_y[i];
   }

   if ( !astMapGet0D ( keymap, "LAMBDA", &(inx->lambda) ) )
      inx->lambda = 0.85e-3;

   if ( !astMapGet0D ( keymap, "MJDAYSTART", &(inx->mjdaystart) ) )
      inx->mjdaystart = 53795.0;

   if ( !astMapGet0I ( keymap, "NBOLX", &(inx->nbolx) ) )
      inx->nbolx = 40;

   if ( !astMapGet0I ( keymap, "NBOLY", &(inx->nboly) ) )
      inx->nboly = 32;   

   /* Calculate the relative grid coordinates */
   inx->ngrid = 
     ( 1 + grid_max_y - grid_min_y ) * ( 1 + grid_max_x - grid_min_x );

   if ( inx->ngrid <= SC2SIM__MXGRID )
   {
      for ( iy=grid_min_y; iy<=grid_max_y; iy++ )
      {
         for ( ix=grid_min_x; ix<=grid_max_x; ix++ )
         {
	    (inx->gridpts)[j][0] = ix;
	    (inx->gridpts)[j][1] = iy;
            j++;
         }
      }
   }
   else
   {   
      *status = SAI__ERROR;
      msgOut(FUNC_NAME, 
             "Too many reconstruction grid points requested", status); 
      return;
   }      

   if ( !astMapGet0I ( keymap, "NUMSAMPLES", &(inx->numsamples) ) )
      inx->numsamples = 128; 

   if ( !astMapGet0I ( keymap, "NVERT", &(inx->nvert) ) )
      inx->nvert = nvert_x;

   astMapGet0C ( keymap, "OBSMODE", &temp );

   if ( !astMapGet0C ( keymap, "OBSMODE", &temp ) )
      strncpy ( inx->obsmode, "", 80 ); 
   else {
      /* Convert to uppercase */
      strncpy ( convert, temp, 80 );
      i = 0;
      while ( *convert != '\0' ) {
         *convert = toupper (*convert);
         convert++;
         i++;
      }
      
      convert = convert - i;
      strncpy ( inx->obsmode, convert, 80 );

   }

   if ( !astMapGet0I ( keymap, "PLATENUM", &(inx->platenum) ) )
      inx->platenum = 1;

   if ( !astMapGet0D ( keymap, "PLATEREV", &(inx->platerev) ) )
      inx->platerev = 2.0;

   if ( !astMapGet0D ( keymap, "PONG_ANGLE", &(inx->pong_angle) ) )
      inx->pong_angle = 0.4636476;
 
   if ( !astMapGet0D ( keymap, "PONG_HEIGHT", &(inx->pong_height) ) )
      inx->pong_height = 2000.0;

   if ( !astMapGet0D ( keymap, "PONG_WIDTH", &(inx->pong_width) ) )
   inx->pong_width = 2000.0;

   if ( !astMapGet0D ( keymap, "PONG_SPACING", &(inx->pong_spacing) ) )
      inx->pong_spacing = 240.0;

   if ( !astMapGet0D ( keymap, "PONG_VMAX", &(inx->pong_vmax) ) )
      inx->pong_vmax = 200.0;

   if ( !astMapGet0C ( keymap, "RA", &temp ) )
      inx->ra = 0.0;
   else {
      /* Get the double representation of the sexagesimal string and
         convert from hours to radians */
      strncpy ( convert, temp, 80 );
      sc2sim_sex2double ( convert, &ra, status );
      ra *= DH2R;
      inx->ra = ra;
   }

   if ( !astMapGet0D ( keymap, "SAMPLE_T", &(inx->sample_t) ) )
      inx->sample_t = 5.0;

   if ( !astMapGet0D ( keymap, "SCAN_ANGLE", &(inx->scan_angle) ) )
      inx->scan_angle = 0.4636476;

   if ( !astMapGet0D ( keymap, "SCAN_PATHLENGTH", &(inx->scan_pathlength) ) )
      inx->scan_angle = 2000.0;
  
   if ( !astMapGet0D ( keymap, "SCAN_VMAX", &(inx->scan_vmax) ) )
      inx->scan_vmax = 200.0;

   if ( !astMapGet0I ( keymap, "SMU_MOVE", &(inx->smu_move) ) )
      inx->smu_move = 8;

   if ( !astMapGet0D ( keymap, "SMU_OFFSET", &(inx->smu_offset) ) )
      inx->smu_offset = 0.0;

   if ( !astMapGet0I ( keymap, "SMU_SAMPLES", &(inx->smu_samples) ) )
      inx->smu_samples = 0;

   if ( !astMapGet0I ( keymap, "SUBSYSNR", &(inx->subsysnr) ) )
      inx->subsysnr = 1;

   if ( !astMapGet0D ( keymap, "TARGETPOW", &(inx->targetpow) ) )
      inx->targetpow = 25.0;

   smf_free ( temp, status );
   smf_free ( convert, status );

}

