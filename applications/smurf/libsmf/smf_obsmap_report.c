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

/* Define a simple struct to contain the integer index into the KeyMap
   timestamp for sorting. We will store these in an array
   and pass them to qsort so that we can ensure that the reports
   are in time order.
*/

typedef struct {
  int index;
  double utc;
} smfSortInfo;

/* local qsort sort routine */
static int sortbytime( const void *in1, const void *in2);


void smf_obsmap_report( msglev_t msglev, AstKeyMap * obsmap, AstKeyMap * objmap,
                      int * status ) {

  AstKeyMap * beaminfo = NULL; /* inbeam information */
  size_t i;
  size_t nobs;        /* number of distinct observations */
  size_t nobj;        /* number of distinct objects */
  size_t nsim = 0;    /* number of simulated observations */
  AstKeyMap * obsinfo = NULL; /* observation information */
  smfSortInfo * obslist = NULL; /* Sorted struct array */

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

    /* Sort everything into order - this takes additional time since we have to read the
       KeyMap twice, but makes the output more amenable to scrutiny */
    obslist = smf_malloc( nobs, sizeof(*obslist), 1, status );
    for (i = 0; i < nobs; i++) {
      AstObject * ao = NULL;
      if (astMapGet0A( obsmap, astMapKey(obsmap, i ), &ao )) {
        double dateobs;
        obsinfo = (AstKeyMap*)ao;  /* strict-aliasing warning avoidance in astMapGet0A */
        astMapGet0D( obsinfo, "MJD-OBS", &dateobs );
        obslist[i].index = i;
        obslist[i].utc = dateobs;
        obsinfo = astAnnul( obsinfo );
      }
    }
    qsort( obslist, nobs, sizeof(*obslist), sortbytime );

    /* create something for INBEAM */
    beaminfo = astKeyMap( " " );

    /* Now do the actual summarizing */
    msgOutif(msglev, " ", "Processing data ^OBJ from the following observation^S :", status);
    for (i = 0; i < nobs; i++) {
      AstObject * ao = NULL;
      int mapindex;
      mapindex = obslist[i].index;

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

        /* What is in the beam */
        if ( astMapHasKey( obsinfo, "INBEAM" ) ) {
          astMapGet0C( obsinfo, "INBEAM", &ctemp );
          astMapPut0I( beaminfo, ctemp, 0, NULL );
          msgSetc( "IB", "/" );
          msgSetc( "IB", ctemp );
        } else {
          astMapPut0I( beaminfo, "none", 0, NULL );
          msgSetc( "IB", "" );
        }


        astMapGet0I( obsinfo, "OBSMODE", &itemp );
        msgSetc( "OM", smf_obsmode_str( itemp, status) );
        astMapGet0I( obsinfo, "OBSNUM", &itemp );
        msgSeti( "ON", itemp);
        astMapGet0I( obsinfo, "UTDATE", &itemp );
        msgSeti( "UT", itemp);
        msgOutif(msglev, "", "  ^UT #^ON ^OM^SW^IB ^OBJ ^OT ^SIM", status);

        obsinfo = astAnnul( obsinfo );
      }
    }
    msgBlankif( msglev, status );

    obslist = smf_free( obslist, status );

    /* Warn if we seem to have a mix of simulated and non-simulated data */
    if (nsim != 0 && nsim != nobs) {
      msgOutif( MSG__QUIET, "", "WARNING: Mixing simulated and observational data",
                status );
      msgBlankif( msglev, status );
    }

    /* Warn if we are mixing observations with different INBEAM settings */
    if ( astMapSize(beaminfo) > 1) {
      msgOutif( MSG__QUIET, "", "WARNING: Mixing observations with different hardware in beam",
                status );
      msgBlankif( msglev, status );
    }

    beaminfo = astAnnul(beaminfo);

  }

  return;
}


/* This routine can be used to sort the darks */
static int sortbytime ( const void *in1, const void *in2 ) {
  const smfSortInfo * sort1;
  const smfSortInfo * sort2;
  double utc1;
  double utc2;

  sort1 = in1;
  sort2 = in2;

  utc1 = sort1->utc;
  utc2 = sort2->utc;

  if (utc1 < utc2) {
    return -1;
  } else if (utc1 > utc2) {
    return 1;
  } else {
    /* least likely case last */
    return 0;
  }
}
