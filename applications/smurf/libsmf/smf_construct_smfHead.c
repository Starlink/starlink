/*
*+
*  Name:
*     smf_construct_smfHead

*  Purpose:
*     Populate a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfHead( smfHead * tofill, inst_t instrument,
*                                   AstFrameSet * wcs, AstFrameSet * tswcs,
*                                   AstFitsChan * fitshdr,
*                                   JCMTState * allState, dim_t curframe,
*                                   const double instap[], dim_t nframes,
*                                   double steptime, double scanvel,
*                                   smf_obsmode obsmode, smf_swmode swmode,
*                                   smf_obstype obstype, smf_obstype seqtype,
*                                   smf_inbeam_t inbeam, unsigned int ndet,
*                                   double fplanex[], double fplaney[],
*                                   double detpos[], char *detname,
*                                   int dpazel, double tsys[],
*                                   const char title[], const char dlabel[],
*                                   const char units[], const double telpos[],
*                                   char * ocsconfig, const char obsidss[],
*                                   int * status );

*  Arguments:
*     tofill = smfHead* (Given)
*        If non-NULL, this is the smfHead that is populated by the remaining
*        arguments. If NULL, the smfHead is malloced.
*     instrument = inst_t (Given)
*        Instrument code. Can be INST__NONE.
*     wcs = AstFrameSet * (Given)
*        Frameset for the world coordinates. The pointer is copied,
*        not the contents.
*     tswcs = AstrFrameSet * (Given)
*        Frameset for the time series world coordinates. The pointer is copied,
*        not the contents.
*     fitshdr = AstFitsChan * (Given)
*        FITS header. The pointer is copied, not the contents.
*     allState = JCMTState* (Given)
*        Pointer to array of time series information for all time slices.
*        Should be at least "curslice" in size. Contents are not copied but it is
*        assumed that the memory will be owned by this smfHead.
*     curframe = dim_t (Given)
*        Current time index corresponding to the associated WCS. sc2head
*        will be set to this location.
*     nframes = dim_t (Given)
*        Number of frames (timeslices) in data.
*     steptime = double (Given)
*        Length of a sample in seconds
*     scanvel = double (Given)
*        Scan velocity in arcsec/sec
*     obsmode = smf_obsmode (Given)
*        Observing mode.
*     swmode = smf_swmode (Given)
*        Switching mode.
*     obstype = smf_obstype (Given)
*        Observation type
*     seqtype = smf_obstype (Given)
*        Sequence type
*     inbeam = smf_inbeam_t (Given)
*        What is in the beam
*     ndet = unsigned int (Given)
*        Number of positions in fplanex, fplaney arrays. Number of detectors.
*     fplanex = const double[] (Given)
*        X Coordinates of bolometers/receptors in the focal plane (radians)
*     fplanex = const double[] (Given)
*        Y Coordinates of bolometers/receptors in the focal plane (radians)
*     detpos = const double[] (Given)
*        The position of the bolometers/receptors in tracking coordinates,
*        in radians. This array should have a length of 2*ndet*nframes.
*     detname = const char * (Given)
*        A concatenated list of null-terminated detector names.
*     dpazel = int (Given)
*        If non-zero, then the values in "detpos" are AZEL
*        positions. Otherwise they are TRACKING positions.
*     tsys = const double[] (Given)
*        System temperatures for each receptor
*     title = const char[] (Given)
*        Title associated with header.
*     dlabel = const char[] (Given)
*        Data label associated with header.
*     units = const char[] (Given)
*        Data units.
*     telpos = const double[] (Given)
*        The telscope position.
*     ocsconfig = char * (Given)
*        OCS Configuration XML
*     obsidss = const char [] (Given)
*        Observation subsystem identifier.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfHead = smfHead*
*        Pointer to newly created smfHead (NULL on error) or,
*        if "tofill" is non-NULL, the pointer to the supplied struct.

*  Description:
*     This function fills a smfHead structure. Optionally, the smfHead
*     is allocated by this routines.

*  Notes:
*     - AST objects are neither cloned nor copied by this routine.
*       Use astCopy or astClone when calling if reference counts
*       should be incremented.
*     - "state" is set to point into allState[curslice]
*     - It is assumed that any array pointers supplied to this routine
*       (allState, fplanex, ocsconfig etc) will become owned by this
*       struct. If this is not the case set isCloned to true when the
*       smfHead is constructed. Note that if any of them are to be owned
*       by the struct all of them must be owned by the struct since
*       smf_close_file will assume this.
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.
*     - obsidss, units, data label and title are copied into this header and will
*       not be freed (they are a defined part of the struct, not dynamic memory).
*     - ocsconfig is assigned to this struct and not copied. It is assumed
*       to be dynamic memory.

*  Authors:
*     Tim Jenness (TIMJ)
*     David Berry (DSB)
*     Andy Gibb (AGG)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        Replace sc2head with allsc2heads. sc2head now indexed
*        into allsc2heads.
*     2006-03-23 (AGG):
*        curslice changed to curframe, nframes added
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-07-26 (TIMJ):
*        Add tswcs.
*     2006-07-31 (TIMJ):
*        Add instrument code. Add fplanex and fplaney.
*     2006-10-02 (DSB):
*        Add detpos.
*     2006-11-06 (DSB):
*        Add detname.
*     2007-02-06 (AGG):
*        Add tsys
*     2007-02-23 (AGG):
*        Add instap to API
*     2008-04-30 (TIMJ):
*        Add title, units and dlabel.
*     2008-07-01 (DSB):
*        Add telpos.
*     2008-07-24 (TIMJ):
*        Add obsmode, obstype.
*     2008-07-28 (TIMJ):
*        Add steptime
*     2009-05-21 (TIMJ):
*        Add switching mode.
*     2009-06-23 (TIMJ):
*        Add ocsconfig
*     2010-03-15 (TIMJ):
*        Add seqtype and obsidss. Use one_strlcpy
*     2010-07-09 (TIMJ):
*        Change default for isCloned to false.
*     2010-08-09 (TIMJ):
*        Add INBEAM support
*     2010-09-23 (EC):
*        Add SCANVEL support
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2007,2010 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/one.h"

/* SMURF routines */
#include "jcmt/state.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfHead"

smfHead *
smf_construct_smfHead( smfHead * tofill, inst_t instrument,
                       AstFrameSet * wcs, AstFrameSet * tswcs,
                       AstFitsChan * fitshdr,
                       JCMTState * allState, dim_t curframe,
                       const double instap[], dim_t nframes, double steptime,
                       double scanvel, smf_obsmode obsmode, smf_swmode swmode,
                       smf_obstype obstype, smf_obstype seqtype,
                       smf_inbeam_t inbeam, unsigned int ndet, double fplanex[],
                       double fplaney[], double detpos[], char *detname,
                       int dpazel, double tsys[], const char title[],
                       const char dlabel[], const char units[],
                       const double telpos[], char * ocsconfig,
                       const char obsidss[], int * status ) {

  smfHead * hdr = NULL;   /* Header components */

  hdr = tofill;
  if (*status != SAI__OK) return hdr;

  if (tofill == NULL) {
    hdr = smf_create_smfHead( status );
  }

  if (*status == SAI__OK) {
    hdr->instrument = instrument;
    hdr->wcs = wcs;
    hdr->tswcs = tswcs;
    hdr->fitshdr = fitshdr;
    hdr->curframe = curframe;
    hdr->nframes = nframes;
    hdr->allState = allState;
    hdr->state = &(allState[curframe]);
    hdr->ndet = ndet;
    hdr->fplanex = fplanex;
    hdr->fplaney = fplaney;
    hdr->detpos = detpos;
    hdr->detname = detname;
    hdr->dpazel = dpazel;
    hdr->tsys = tsys;
    hdr->isCloned = 0;
    hdr->instap[0] = instap[0];
    hdr->instap[1] = instap[1];
    hdr->telpos[0] = telpos[0];
    hdr->telpos[1] = telpos[1];
    hdr->telpos[2] = telpos[2];
    hdr->obsmode = obsmode;
    hdr->swmode = swmode;
    hdr->obstype = obstype;
    hdr->seqtype = seqtype;
    hdr->scanvel = scanvel;
    hdr->steptime= steptime;
    hdr->inbeam  = inbeam;
    hdr->ocsconfig = ocsconfig;

    /* Have to copy the string items in since the struct has a slot for them */
    if (units) one_strlcpy( hdr->units, units, sizeof(hdr->units), status );
    if (dlabel) one_strlcpy(hdr->dlabel, dlabel, sizeof(hdr->dlabel), status );
    if (title) one_strlcpy(hdr->title, title, sizeof(hdr->title), status );
    if (obsidss) one_strlcpy( hdr->obsidss, obsidss, sizeof(hdr->obsidss), status );

  }

  return hdr;
}
