/*
*+
*  Name:
*     sc2sim_get_drgroup.c

*  Purpose:
*     Return a unique data-reduction group string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_get_drgroup ( struct sc2sim_obs_struct *inx, char filter,
*                          char object, char *drgroup, int *status )

*  Arguments:
*     inx = const sc2sim_obs_struct* (Given)
*        Pointer to observation struct
*     filter = char* (Given)
*        Name of filter used in this observation
*     object = char* (Given)
*        Name of object observed
*     drgroup = char* (Returned)
*        Name of data-reduction group
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Determine the name of the data-reduction group used to combine
*     data within ORAC-DR. Currently the DRGROUP string is a
*     concatenation of the RA/Dec (or object name for moving sources),
*     observation mode (DREAM, STARE, PONG, etc), observation type
*     (SCIENCE, FOCUS or POINTING) and filter. The value is written as
*     a FITS header and is thus restricted to 40 characters or less so
*     as not to exceed the size of the FITS card.

*  Authors:
*     A.G. Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-19 (AGG):
*        Original

*  Copyright:
*     Copyright (C) 2008 University of British Columbia. All Rights Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "star/slalib.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_get_drgroup ( const struct sc2sim_obs_struct *inx, const char *filter, 
			  const char *object, char *drgroup, int *status ) {

  /* Local variables */
  size_t curlength;           /* Length of current queried string */
  char decstr[8];             /* Dec as sDDMMSS where s is + or - */
  size_t length = 0;          /* Length of string to copy into drgroup */
  size_t maxlen = 40;         /* Maximum length of DRGROUP string 
                                 This should really be a constant! */
  char sign[2];               /* Sign of declination */
  int iamsf[4];               /* Array containing H/D, M, S and .SS */
 
  /* Check status */
  if ( *status != SAI__OK ) return;

  /* DRGROUP is made up from ra . dec . obsmode . obstype . filter */

  /* Check whether we have a stationary or moving source */
  if ( inx->planetnum < 0 ) {
    /* We have a fixed target so use RA, Dec */
    /* Return RA as HHMMSS - first arg means zero dp */
    slaDr2tf( 0, inx->ra, sign, iamsf );
    sprintf( drgroup, "%02d%02d%02d", iamsf[0], iamsf[1], iamsf[2] );
    /* Return Dec as DDMMSS */
    slaDr2af( 0, inx->dec, sign, iamsf );
    length = 1;
    strncat( drgroup, sign, length);
    sprintf( decstr, "%02d%02d%02d", iamsf[0], iamsf[1], iamsf[2] );
    length = 7;
    strncat( drgroup, decstr, length);
  } else {
    /* We have a planet so use supplied object name */
    curlength = strlen( object );
    length += curlength;
    strncpy( drgroup, object, curlength);
  }

  /* obsmode */
  curlength = strlen( inx->obsmode );
  length += curlength;
  /* obstype */
  curlength = strlen( inx->obstype );
  length += curlength;
  /* filter */
  curlength = strlen( filter );
  length += curlength;

  /* The length has been checked so just use strcat */
  if ( length <= maxlen ) {
    strcat( drgroup, inx->obsmode );
    strcat( drgroup, inx->obstype );
    strcat( drgroup, filter );
  } else {
    /* Else set a blank group and allow the pipeline to make one up later */
    drgroup = '\0';
  }

  return;
}
