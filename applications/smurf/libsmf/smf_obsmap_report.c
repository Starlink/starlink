/*
*+
*  Name:
*     smf_obsmap_report

*  Purpose:
*     Report a summary of the observations being processed

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_obsmap_report( msglev_t msglev, AstKeyMap * obsmap, AstKeyMap * objmap,
*                        int * status );

*  Arguments:
*     msglev = msglev_t (Given)
*        Messaging level to be used for output information. This allows a
*        caller to control when the summary information will appear.
*     obsmap = AstKeyMap * (Given)
*        Key map containing observation information. Populated by
*        smf_obsmap_fill().
*     objmap = AstKeyMap * (Given).
*        KeyMap with keys of object name. Populated by smf_obsmap_fill().
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reports a summary of all the observations being processed.

*  See Also:
*     smf_obsmap_fill for populating the requird AstKeyMap structures.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-04-24 (TIMJ):
*        Initial version. Some code relocated from smf_find_darks.
*     2009-05-21 (TIMJ):
*        Support switching mode
*     2009-05-25 (TIMJ):
*        Add message level argument.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "star/one.h"
#include "ast.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

void smf_obsmap_report( msglev_t msglev, AstKeyMap * obsmap, AstKeyMap * objmap,
                      int * status ) {

  size_t i;
  size_t nobs;        /* number of distinct observations */
  size_t nobj;        /* number of distinct objects */
  size_t nsim = 0;    /* number of simulated observations */
  AstKeyMap * obsinfo = NULL; /* observation information */

  if (*status != SAI__OK) return;

  /* Now report the details of the observations */
  nobj = astMapSize( objmap );
  nobs = astMapSize( obsmap );
  if (nobs > 0) {
    if (nobs == 1) {
      msgSetc("S", "");
    } else {
      msgSetc("S", "s");
    }
    /* if we only have on object name report it now */
    if (nobj == 1) {
      msgSetc( "OBJ", "for object '");
      msgSetc( "OBJ", astMapKey( objmap, 0 ) );
      msgSetc( "OBJ", "'");
    } else {
      msgSetc( "OBJ", " ");
    }
    msgOutif(msglev, " ", "Processing data ^OBJ from the following observation^S :", status);
    for (i = 0; i < nobs; i++) {
      AstObject * ao = NULL;
      if (astMapGet0A( obsmap, astMapKey(obsmap, i ), &ao )) {
        const char * ctemp;
        int itemp;

        obsinfo = (AstKeyMap*)ao;  /* strict-aliasing warning avoidance in astMapGet0A */

        /* only display object if we have not already done so */
        if (nobj > 1) {
          astMapGet0C( obsinfo, "OBJECT", &ctemp );
          msgSetc( "OBJ", ctemp);
        } else {
          msgSetc( "OBJ", " ");
        }

        /* do not display "SCIENCE" as it is the default */
        astMapGet0I( obsinfo, "OBSTYPE", &itemp );
        if (itemp != SMF__TYP_SCIENCE) {
          msgSetc( "OT", "(");
          msgSetc( "OT", smf_obstype_str( itemp, status) );
          msgSetc( "OT", ")");
        } else {
          msgSetc( "OT", " ");
        }

        /* Do not display SELF or NONE switch mode as they contain
           no useful information over the obs mode */
        astMapGet0I( obsinfo, "SWMODE", &itemp );
        if (itemp != SMF__SWM_NULL && itemp != SMF__SWM_SELF) {
          msgSetc( "SW", "/");
          msgSetc( "SW", smf_swmode_str( itemp, status ) );
        } else {
          msgSetc( "SW", " " );
        }

        /* Simulation information */
        astMapGet0I( obsinfo, "SIMULATE", &itemp );
        if (itemp) {
          msgSetc( "SIM", "(simulated)" );
          nsim++;
        } else {
          msgSetc( "SIM", "" );
        }

        astMapGet0I( obsinfo, "OBSMODE", &itemp );
        msgSetc( "OM", smf_obsmode_str( itemp, status) );
        astMapGet0I( obsinfo, "OBSNUM", &itemp );
        msgSeti( "ON", itemp);
        astMapGet0I( obsinfo, "UTDATE", &itemp );
        msgSeti( "UT", itemp);
        msgOutif(msglev, "", "  ^UT #^ON ^OM^SW ^OBJ ^OT ^SIM", status);
      }
    }
    msgBlankif( msglev, status );

    /* Warn if we seem to have a mix of simulated and non-simulated data */
    if (nsim != 0 && nsim != nobs) {
      msgOutif( MSG__QUIET, "", "WARNING: Mixing simulated and observational data",
                status );
      msgBlankif( msglev, status );
    }

  }

  return;
}
