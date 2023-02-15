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
 *     sc2sim_get_drgroup ( struct sc2sim_obs_struct *inx, const char *filter,
 *                          const char *object, char *drgroup, int *status )

 *  Arguments:
 *     inx = const sc2sim_obs_struct* (Given)
 *        Pointer to observation struct
 *     filter = const char* (Given)
 *        Name of filter used in this observation
 *     object = const char* (Given)
 *        Name of object observed
 *     drgroup = char* (Returned)
 *        Name of data-reduction group. Must be at least 40 characters.
 *        If the drgroup does not fit in the buffer, an empty string
 *        is returned and status remains good.
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
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2008-03-19 (AGG):
 *        Original
 *     2008-07-11 (TIMJ):
 *        Use strl*
 *     2012-03-06 (TIMJ):
 *        Replace SLA with SOFA.

 *  Copyright:
 *     Copyright (C) 2008,2012 Science and Technology Facilities Council.
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

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
#include "sofa.h"
#include "star/one.h"
#include "one_err.h"
#include "mers.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_get_drgroup ( const struct sc2sim_obs_struct *inx, const char *filter,
                          const char *object, char *drgroup, int *status ) {

  /* Local variables */
  char decstr[8];             /* Dec as sDDMMSS where s is + or - */
  dim_t maxlen = 40;         /* Maximum length of DRGROUP string
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
    iauA2tf( 0, inx->ra, sign, iamsf );
    sprintf( drgroup, "%02d%02d%02d", iamsf[0], iamsf[1], iamsf[2] );
    /* Return Dec as DDMMSS */
    iauA2af( 0, inx->dec, sign, iamsf );
    one_strlcat( drgroup, sign, maxlen, status );
    sprintf( decstr, "%02d%02d%02d", iamsf[0], iamsf[1], iamsf[2] );
    one_strlcat( drgroup, decstr, maxlen, status);
  } else {
    /* We have a planet so use supplied object name */
    one_strlcpy( drgroup, object, maxlen, status);
  }

  /* obsmode */
  one_strlcat( drgroup, inx->obsmode, maxlen, status );
  /* obstype */
  one_strlcat( drgroup, inx->obstype, maxlen, status );
  /* filter */
  one_strlcat( drgroup, filter, maxlen, status );

  /* if we ran out of buffer (note that maxlen is currently not known
     to be right so we could still buffer overflow) return empty string
     rather than die */
  if (*status == ONE__TRUNC) {
    /* Else set a blank group and allow the pipeline to make one up later */
    errAnnul( status );
    drgroup[0] = '\0';
  }

  return;
}
