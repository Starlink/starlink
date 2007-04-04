/*
*+
*  Name:
*     sc2sim_getsimpar.c

*  Purpose:
*     Read simulation parameters from keymap file and store in 
*     sc2sim_sim struct.  

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getsimpar ( AstKeyMap *keymap, struct sc2sim_sim_struct *sinx, 
*                        int *status )

*  Arguments:
*     keymap = AstKeyMap* (Given)
*        Keymap containing sim parameters
*     sinx = sc2sim_sim_struct* (Returned)
*        Structure for values from sim keymap file
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Retrieve sim parameters and store in sc2sim_sim_struct.
*     If a parameter is unspecified, set it to a reasonable default value.

*  Authors:
*     J. Balfour (UBC)
*     E. Chapin (UBC)
*     A.G. Gibb (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-09-15 (JB):
*        Original
*     2006-10-04 (JB):
*        Replace strcpy with strncpy
*     2006-10-23 (EC):
*        Don't free constant memory used by AST
*     2006-11-01 (AGG):
*        Put reasonable default values for ast/atm names
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
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

/* SMURF includes */
#include "libsmf/smf.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* Starlink Includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

void sc2sim_getsimpar ( AstKeyMap *keymap, struct sc2sim_sim_struct *sinx, 
                        int *status ) {

  const char *temp=NULL; /* Pointer to static strings created by ast */
 
   /* Check status */
   if ( !StatusOkP(status) ) return;

   if ( !astMapGet0I ( keymap, "ADD_ATM", &(sinx->add_atm) ) )
      sinx->add_atm = 0;
  
   if ( !astMapGet0I ( keymap, "ADD_FNOISE", &(sinx->add_fnoise) ) )
      sinx->add_fnoise = 0; 

   if ( !astMapGet0I ( keymap, "ADD_HNOISE", &(sinx->add_hnoise) ) )
      sinx->add_hnoise = 0; 

   if ( !astMapGet0I ( keymap, "ADD_PNS", &(sinx->add_pns) ) )
      sinx->add_pns = 0; 

   if ( !astMapGet0D ( keymap, "AIRMASS", &(sinx->airmass) ) )
      sinx->airmass = 1.2; 

   if ( !astMapGet0D ( keymap, "ANANG", &(sinx->anang) ) )
      sinx->anang = 0.0; 

   if ( !astMapGet0D ( keymap, "ANPOL", &(sinx->anpol) ) )
      sinx->anpol = 100.0; 

   if ( !astMapGet0D ( keymap, "ANTRANS", &(sinx->antrans) ) )
      sinx->antrans = 100.0; 

   if ( !astMapGet0D ( keymap, "AOMEGA", &(sinx->aomega) ) )
      sinx->aomega = 0.179;

   if ( !astMapGet0C ( keymap, "ASTNAME", &temp ) )
      strncpy ( sinx->astname, "ast.sdf", SC2SIM__FLEN );
   else 
      strncpy ( sinx->astname, temp, SC2SIM__FLEN );

   if ( !astMapGet0D ( keymap, "ASTPOL", &(sinx->astpol) ) )
      sinx->astpol = 10.0;

   if ( !astMapGet0D ( keymap, "ATEND", &(sinx->atend) ) )
      sinx->atend = 5.0; 

   if ( !astMapGet0C ( keymap, "ATMNAME", &temp ) )
      strncpy ( sinx->atmname, "atm.sdf", SC2SIM__FLEN );
   else 
      strncpy ( sinx->atmname, temp, SC2SIM__FLEN );

   if ( !astMapGet0D ( keymap, "ATMREFNU", &(sinx->atmrefnu) ) )
      sinx->atmrefnu = 0.5;  

   if ( !astMapGet0D ( keymap, "ATMREFVEL", &(sinx->atmrefvel) ) )
      sinx->atmrefvel = 15.0; 

   if ( !astMapGet0D ( keymap, "ATMXVEL", &(sinx->atmxvel) ) )
      sinx->atmxvel = 5000.0; 

   if ( !astMapGet0D ( keymap, "ATMYVEL", &(sinx->atmyvel) ) )
      sinx->atmyvel = 0.0; 

   if ( !astMapGet0D ( keymap, "ATMZEROX", &(sinx->atmzerox) ) )
      sinx->atmzerox = 5000.0;  

   if ( !astMapGet0D ( keymap, "ATMZEROY", &(sinx->atmzeroy) ) )
      sinx->atmzeroy = 50000.0;  

   if ( !astMapGet0D ( keymap, "ATSTART", &(sinx->atstart) ) )
      sinx->atstart = 5.0;  

   if ( !astMapGet0D ( keymap, "BANDGHZ", &(sinx->bandGHz) ) )
      sinx->bandGHz = 35.0;

   if ( !astMapGet0D ( keymap, "BLINDANG", &(sinx->blindang) ) )
      sinx->blindang = 10.0;  

   if ( !astMapGet0D ( keymap, "BLINDPOL", &(sinx->blindpol) ) )
      sinx->blindpol = 1.0;

   if ( !astMapGet0D ( keymap, "BLINDTRANS", &(sinx->blindtrans) ) )
      sinx->blindtrans = 93.0;    

   if ( !astMapGet0D ( keymap, "CASSANG", &(sinx->cassang) ) )
      sinx->cassang = 135.0;    

   if ( !astMapGet0D ( keymap, "CASSPOL", &(sinx->casspol) ) )
      sinx->casspol = 1.0;

   if ( !astMapGet0D ( keymap, "CASSTRANS", &(sinx->casstrans) ) )
      sinx->casstrans = 98.0;

   if ( !astMapGet0I ( keymap, "FLUX2CUR", &(sinx->flux2cur) ) )
      sinx->flux2cur = 1;

   if ( !astMapGet0D ( keymap, "MEANATM", &(sinx->meanatm) ) )
      sinx->meanatm = 7.0;

   if ( !astMapGet0D ( keymap, "NASANG", &(sinx->nasang) ) )
      sinx->nasang = 90.0;

   if ( !astMapGet0D ( keymap, "NASPOL", &(sinx->naspol) ) )
      sinx->naspol = 1.0;

   if ( !astMapGet0D ( keymap, "NASTRANS", &(sinx->nastrans) ) )
      sinx->nastrans = 98.0;

   if ( !astMapGet0I ( keymap, "NCYCLE", &(sinx->ncycle) ) )
      sinx->ncycle = 1;

   if ( !astMapGet0D ( keymap, "SMU_TERR", &(sinx->smu_terr) ) )
      sinx->smu_terr = 0.0;

   if ( !astMapGet0C ( keymap, "SUBNAME", &temp ) ) 
      strncpy ( sinx->subname, "s8a", SC2SIM__FLEN );
   else
      strncpy ( sinx->subname, temp, SC2SIM__FLEN );

   if ( !astMapGet0D ( keymap, "TELEMISSION", &(sinx->telemission) ) )
      sinx->telemission = 4.0;

   if ( !astMapGet0D ( keymap, "TAUZEN", &(sinx->tauzen) ) )
      sinx->tauzen = 0.052583;

   if ( !astMapGet0D ( keymap, "XPOINT", &(sinx->xpoint) ) )
      sinx->xpoint = 20.0;

   if ( !astMapGet0D ( keymap, "YPOINT", &(sinx->ypoint) ) )
      sinx->ypoint = 20.0;

}

