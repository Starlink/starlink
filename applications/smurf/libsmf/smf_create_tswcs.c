/*
*+
*  Name:
*     smf_create_tswcs

*  Purpose:
*     Create a time series WCS given a smfHead

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_create_tswcs( const smfHead *hdr, int isTordered, AstFrameSet **frameset, int *status )

*  Arguments:
*     hdr = smfHead* (Given)
*        The supplied smfHead must have allState, telpos, nframes, instap, and
*        a complete fitshdr populated.
*     isTordered = int (Given)
*        The required axis order: isTorered==0 implies the time axis is the first
*        axis, isTorered==1 implies the time axis is the last axis.
*     fset = AstFrameSet **fset (Returned)
*        Newly constructed frameset for time-series data cube.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Create a time-series WCS for a data cube using information
*     stored in the FITS header and JCMTState. This is a wrapper for
*     sc2store_timeWcs.

*  Notes:

*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-19 (EC)
*        Initial version -- based on timeWcs() in sc2store.c
*     2010-12-06 (TIMJ):
*        Missing DUT1 is not fatal.
*     2014-01-28 (DSB):
*        Allow the required axis order to be specified.
*     2017-01-10 (GSB):
*        Read DTAI from header.
*     2017-04-06 (GSB):
*        Set dtai in telpar parameters passed to sc2store_timeWcs.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
*     Copyright (C) 2014 Science & Technology Facilities Council.
*     Copyright (C) 2017 East Asian Observatory.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/atl.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_create_tswcs"

void smf_create_tswcs( smfHead *hdr, int isTordered, AstFrameSet **frameset, int *status ){

  /* Local Variables */
  JCMTState *allState=NULL;     /* Full array of JCMTState for time series */
  double dut1;                  /* UT1-UTC (seconds) */
  double dtai = VAL__BADD;      /* TAI-UTC (seconds) */
  int i;                        /* loop counter */
  double *instap=NULL;          /* pointer to 2-element instrument aperture */
  int ntime;                    /* number of time slices */
  sc2ast_subarray_t subnum;     /* Subarray number */
  SC2STORETelpar telpar;        /* Additional telescope information */
  double *telpos=NULL;          /* telescope position pointer */
  double *times=NULL;           /* pointer to a time array */

  if( *status != SAI__OK ) return;

  if( !frameset ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL reference to frameset pointer supplied",
            status );
    return;
  }

  *frameset = NULL;

  if( !hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfHead supplied", status );
    return;
  }

  /* Obtain everything we need from the header */
  allState = hdr->allState;
  instap = hdr->instap;
  ntime = (int) hdr->nframes;
  telpos = hdr->telpos;

  if( !allState || !telpos || !ntime || !instap || !hdr->fitshdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Incomplete smfHead supplied", status );
    return;
  }

  /* do not get concerned if DUT1 is missing */
  dut1 = 0.0;
  smf_getfitsd( hdr, "DUT1", &dut1, status );
  dut1 *= SPD;

  smf_getfitsd(hdr, "DTAI", &dtai, status);

  /* Return here if an error encountered */
  if( *status != SAI__OK ) return;

  smf_find_subarray( hdr, NULL, 0, &subnum, status );

  /* copy RTS_END values into times, populate telpar, and call sc2store_timeWcs*/
  times = astCalloc( ntime, sizeof(*times) );

  if( *status == SAI__OK ) {
    telpar.dut1 = dut1;
    telpar.dtai = dtai;
    telpar.longdeg = -telpos[0];
    telpar.latdeg = telpos[1];
    telpar.instap_x = 0;
    telpar.instap_y = 0;

    for( i=0; i<ntime; i++ ) {
      times[i] = allState[i].rts_end;
    }

    *frameset = sc2store_timeWcs( subnum, ntime, 0, &telpar, times, status );

    /* Clear the time zone information which is irrelevant and we don't know it*/
    if( *status == SAI__OK ) {
      astClear( *frameset, "LToffset" );
    }

    /* sc2store_timeWcs assumes that the time axis is the third axis
       (i.e. isTordered=1). If the requested ordering is different,
       permute the base frame (i.e. grid) axes to take account of it. */
    smf_tswcsOrder( frameset, isTordered, status );
  }
  /* Clean up */
  times = astFree( times );

}
