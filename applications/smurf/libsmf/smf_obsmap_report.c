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
*     2009-05-27 (TIMJ):
*        Sort observations into date order.
*     2009-07-08 (TIMJ):
*        Include INBEAM in report.
*     2009-07-10 (TIMJ):
*        Include instrument information in report.
*     2009-09-25 (TIMJ):
*        Move sort routine externally.
*     2010-09-28 (TIMJ):
*        Handle null pointers explicitly rather than letting MERS write <null>.
*     2011-01-25 (TIMJ):
*        Tweak to smfSortInfo struct member.
*     2012-08-17 (TIMJ):
*        INBEAM is now stored as an integer.

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

  int beamflag = 0;   /* True if we have an inbeam mismatch */
  int beamref = -1;    /* a reference inbeam value */
  int i;
  AstKeyMap * instmap = NULL; /* instrument information */
  dim_t ninst;       /* number of instruments */
  dim_t nobs;        /* number of distinct observations */
  dim_t nobj;        /* number of distinct objects */
  dim_t nsim = 0;    /* number of simulated observations */
  AstKeyMap * obsinfo = NULL; /* observation information */
  smfSortInfo * obslist = NULL; /* Sorted struct array */
  const char * str = NULL; /* temp string pointer */

  if (*status != SAI__OK) return;

  astBegin;

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

    /* See if we have more than one instrument */
    instmap = astKeyMap( " " );

    /* Sort everything into order - this takes additional time since we have to read the
       KeyMap twice, but makes the output more amenable to scrutiny */
    obslist = astCalloc( nobs, sizeof(*obslist) );
    for (i = 0; i < nobs; i++) {
      AstObject * ao = NULL;
      const char * instrume = NULL;
      if (astMapGet0A( obsmap, astMapKey(obsmap, i ), &ao )) {
        double dateobs;
        obsinfo = (AstKeyMap*)ao;  /* strict-aliasing warning avoidance in astMapGet0A */
        astMapGet0D( obsinfo, "MJD-OBS", &dateobs );
        obslist[i].index = i;
        obslist[i].sortval = dateobs;

        /* since we are looping already, extract INSTRUME information so that we can count it */
        astMapGet0C( obsinfo, "INSTRUME", &instrume );
        astMapPut0I( instmap, instrume, 0, NULL );

        obsinfo = astAnnul( obsinfo );
      }
    }
    qsort( obslist, nobs, sizeof(*obslist), smf_sort_bydouble );

    /* See how many instruments we have */
    ninst = astMapSize( instmap );
    if (ninst == 1) {
      msgFmt( "INST", "from instrument '%s'", astMapKey( instmap, 0 ) );
    } else {
      msgSetc( "INST", "" );
    }

    /* Now do the actual summarizing */
    msgOutif(msglev, " ", "Processing data ^INST ^OBJ from the following observation^S :", status);
    for (i = 0; i < nobs; i++) {
      AstObject *ao = NULL;
      int mapindex = (int) obslist[i].index;

      if (astMapGet0A( obsmap, astMapKey(obsmap, mapindex ), &ao )) {
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

        /* only display instrument if we have not already done so */
        if (ninst > 1 ) {
          astMapGet0C( obsinfo, "INSTRUME", &ctemp );
          msgSetc( "INS", "/" );
          msgSetc( "INS", ctemp );
        } else {
          msgSetc( "INS", "" );
        }

        /* do not display "SCIENCE" as it is the default */
        astMapGet0I( obsinfo, "OBSTYPE", &itemp );
        if (itemp != SMF__TYP_SCIENCE) {
          str = smf_obstype_str( itemp, status );
          if (str) {
            msgSetc( "OT", "(");
            msgSetc( "OT", smf_obstype_str( itemp, status) );
            msgSetc( "OT", ")");
          } else {
            msgSetc( "OT", " ");
          }
        } else {
          msgSetc( "OT", " ");
        }

        /* Do not display SELF or NONE switch mode as they contain
           no useful information over the obs mode */
        astMapGet0I( obsinfo, "SWMODE", &itemp );
        if (itemp != SMF__SWM_NULL && itemp != SMF__SWM_SELF) {
          str = smf_swmode_str( itemp, status );
          if (str) {
            msgSetc( "SW", "/");
            msgSetc( "SW", str );
          } else {
            msgSetc( "SW", " ");
          }
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

        /* What is in the beam */
        astMapGet0I( obsinfo, "INBEAM", &itemp );
        if (beamref == -1) {
          beamref = itemp;
        } else {
          if (beamref != itemp) beamflag = 1;
        }
        if (itemp > 0) {
          char beamstr[SZFITSTR];
          smf_inbeam_str( itemp, beamstr, sizeof(beamstr), status );
          msgSetc( "IB", "/" );
          msgSetc( "IB", beamstr );
        } else {
          msgSetc( "IB", "" );
        }


        astMapGet0I( obsinfo, "OBSMODE", &itemp );
        str = smf_obsmode_str( itemp, status);
        if (str) {
          msgSetc( "OM", str );
        } else {
          msgSetc( "OM", " " );
        }
        astMapGet0I( obsinfo, "OBSNUM", &itemp );
        msgSeti( "ON", itemp);
        astMapGet0I( obsinfo, "UTDATE", &itemp );
        msgSeti( "UT", itemp);
        msgOutif(msglev, "", "  ^UT #^ON ^OM^SW^INS^IB ^OBJ ^OT ^SIM", status);

        obsinfo = astAnnul( obsinfo );
      }
    }
    msgBlankif( msglev, status );

    obslist = astFree( obslist );

    /* Warn if we seem to have a mix of simulated and non-simulated data */
    if (nsim != 0 && nsim != nobs) {
      msgOutif( MSG__QUIET, "", "WARNING: Mixing simulated and observational data",
                status );
      msgBlankif( msglev, status );
    }

    /* Warn if we are mixing observations with different INBEAM settings */
    if ( beamflag ) {
      msgOutif( MSG__QUIET, "", "WARNING: Mixing observations with different hardware in beam",
                status );
      msgBlankif( msglev, status );
    }

  }

  astEnd;

  return;
}
