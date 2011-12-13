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
 *     Tim Jenness (JAC, Hawaii)
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
 *     2007-06-29 (EC)
 *        Changed bolometer noise model:
 *        -removed guessatm, aomega, bandGHz
 *        -added jy2pw, refload, refnoise
 *        Modified default values of atmyvalue and atmrefnu
 *     2007-07-04 (EC):
 *        Added cosmic ray spikes, parameters spike_p0/p1/t0/alpha
 *     2007-07-13 (AGG):
 *        Check that tauzen is within a valid range
 *     2007-10-09 (AGG):
 *        Print out value of tauzen if too low, use msgOut for reporting
 *     2008-04-24 (AGG):
 *        Set more appropriate default values for the atmosphere
 *     2009-10-16 (AGG):
 *        Use one_strlcpy rather than strlcpy
 *     2009-11-20 (DSB):
 *        Add sinx->params and sinx->interp.
 *     2010-03-16 (TIMJ):
 *        Use one_strlcpy
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2009-2010 Science & Technology Facilities Council.
 *     Copyright (C) 2005-2009 University of British Columbia. All
 *     Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

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
#include "star/one.h"

void sc2sim_getsimpar ( AstKeyMap *keymap, struct sc2sim_sim_struct *sinx,
                        int *status ) {

  char subnames[SC2SIM__MAXSUBS * SC2SIM__SUBLEN];
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

  if ( !astMapGet0D ( keymap, "ANANG", &(sinx->anang) ) )
    sinx->anang = 0.0;

  if ( !astMapGet0D ( keymap, "ANPOL", &(sinx->anpol) ) )
    sinx->anpol = 100.0;

  if ( !astMapGet0D ( keymap, "ANTRANS", &(sinx->antrans) ) )
    sinx->antrans = 100.0;

  if ( !astMapGet0C ( keymap, "ASTNAME", &temp ) )
    one_strlcpy ( sinx->astname, "ast.sdf", sizeof(sinx->astname), status );
  else
    one_strlcpy ( sinx->astname, temp, sizeof(sinx->astname), status );

  if ( !astMapGet0D ( keymap, "ASTPOL", &(sinx->astpol) ) )
    sinx->astpol = 10.0;

  if ( !astMapGet0D ( keymap, "ATEND", &(sinx->atend) ) )
    sinx->atend = 5.0;

  if ( !astMapGet0C ( keymap, "ATMNAME", &temp ) )
    one_strlcpy ( sinx->atmname, "atm.sdf", sizeof(sinx->atmname), status );
  else
    one_strlcpy ( sinx->atmname, temp, sizeof(sizeof(sinx->atmname)), status );

  if ( !astMapGet0D ( keymap, "ATMREFNU", &(sinx->atmrefnu) ) )
    sinx->atmrefnu = 0.5;

  if ( !astMapGet0D ( keymap, "ATMREFVEL", &(sinx->atmrefvel) ) )
    sinx->atmrefvel = 15.0;

  if ( !astMapGet0D ( keymap, "ATMXVEL", &(sinx->atmxvel) ) )
    sinx->atmxvel = 2500.0;

  if ( !astMapGet0D ( keymap, "ATMYVEL", &(sinx->atmyvel) ) )
    sinx->atmyvel = 2500.0;

  if ( !astMapGet0D ( keymap, "ATMZEROX", &(sinx->atmzerox) ) )
    sinx->atmzerox = 5000.0;

  if ( !astMapGet0D ( keymap, "ATMZEROY", &(sinx->atmzeroy) ) )
    sinx->atmzeroy = 5000.0;

  if ( !astMapGet0D ( keymap, "ATSTART", &(sinx->atstart) ) )
    sinx->atstart = 5.0;

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

  if ( !astMapGet0D ( keymap, "JY2PW", &(sinx->jy2pw) ) )
    sinx->jy2pw = 2.3e-3;

  if ( !astMapGet0D ( keymap, "NASANG", &(sinx->nasang) ) )
    sinx->nasang = 90.0;

  if ( !astMapGet0D ( keymap, "NASPOL", &(sinx->naspol) ) )
    sinx->naspol = 1.0;

  if ( !astMapGet0D ( keymap, "NASTRANS", &(sinx->nastrans) ) )
    sinx->nastrans = 98.0;

  if ( !astMapGet0I ( keymap, "NCYCLE", &(sinx->ncycle) ) )
    sinx->ncycle = 1;

  if ( !astMapGet0D ( keymap, "REFLOAD", &(sinx->refload) ) )
    sinx->refload = 7.4;

  if ( !astMapGet0D ( keymap, "REFNOISE", &(sinx->refnoise) ) )
    sinx->refnoise = 6.5e-5;

  if ( !astMapGet0D ( keymap, "SMU_TERR", &(sinx->smu_terr) ) )
    sinx->smu_terr = 0.0;

  if ( !astMapGet0D ( keymap, "SPIKE_ALPHA", &(sinx->spike_alpha) ) )
    sinx->spike_alpha = -1.5;

  if ( !astMapGet0D ( keymap, "SPIKE_P0", &(sinx->spike_p0) ) )
    sinx->spike_p0 = 1.0;

  if ( !astMapGet0D ( keymap, "SPIKE_P1", &(sinx->spike_p1) ) )
    sinx->spike_p1 = 1000.0;

  if ( !astMapGet0D ( keymap, "SPIKE_T0", &(sinx->spike_t0) ) )
    sinx->spike_t0 = 20.0;

  if ( !astMapGet1C ( keymap, "SUBNAME", SC2SIM__SUBLEN, SC2SIM__MAXSUBS, &(sinx->nsubarrays), subnames ) ) {
    one_strlcpy ( (sinx->subname)[0], "s8a", SC2SIM__SUBLEN, status );
    sinx->nsubarrays = 1;
  } else {
    char *ptr = subnames;
    int i;
    for (i = 0; i < sinx->nsubarrays; i++) {
      one_strlcpy( (sinx->subname)[i], ptr, SC2SIM__SUBLEN, status );
      ptr += SC2SIM__SUBLEN;
    }
  }

  if ( !astMapGet0D ( keymap, "TELEMISSION", &(sinx->telemission) ) )
    sinx->telemission = 4.0;

  if ( !astMapGet0D ( keymap, "TAUZEN", &(sinx->tauzen) ) )
    sinx->tauzen = 0.052583;
  /* Check that tauzen is in range - this should be a
     wavelength-dependent check, but for simplicity we can stick to
     one value since tau should never get this low anyway. */
  if ( sinx->tauzen <= 0.014 ) {
    msgSetd("T",sinx->tauzen);
    msgOut("","TAUZEN too low (value = ^T, must be >0.014), setting to 0.015\n",
           status);
    sinx->tauzen = 0.015;
  }

  if ( !astMapGet0D ( keymap, "XPOINT", &(sinx->xpoint) ) )
    sinx->xpoint = 20.0;

  if ( !astMapGet0D ( keymap, "YPOINT", &(sinx->ypoint) ) )
    sinx->ypoint = 20.0;

  if ( !astMapGet0C ( keymap, "INTERP", &temp ) ) {
    sinx->interp = AST__NEAREST;
  } else if( astChrMatch( temp, "nearest" ) ) {
    sinx->interp = AST__NEAREST;
  } else if( astChrMatch( temp, "linear" ) ) {
    sinx->interp = AST__LINEAR;
  } else if( astChrMatch( temp, "sinc" ) ) {
    sinx->interp = AST__SINC;
  } else if( astChrMatch( temp, "sincsinc" ) ) {
    sinx->interp = AST__SINCSINC;
  } else if( astChrMatch( temp, "sinccos" ) ) {
    sinx->interp = AST__SINCCOS;
  } else if( astChrMatch( temp, "sincgauss" ) ) {
    sinx->interp = AST__SINCGAUSS;
  } else if( astChrMatch( temp, "somb" ) ) {
    sinx->interp = AST__SOMB;
  } else if( astChrMatch( temp, "sombcos" ) ) {
    sinx->interp = AST__SOMBCOS;
  } else if( astChrMatch( temp, "blockave" ) ) {
    sinx->interp = AST__BLOCKAVE;
  } else if( *status == SAI__OK ) {
    *status = SAI__ERROR;
    msgSetc( "I", temp );
    errRep( " ", "Bad value (^I) for simpar 'Interp'.", status );
  }

  if ( !astMapGet0D ( keymap, "PARAM1", sinx->params ) )
    sinx->params[0] = 2.0;

  if ( !astMapGet0D ( keymap, "PARAM2", sinx->params + 1 ) )
    sinx->params[1] = 2.0;

}

