/*
*+
*  Name:
*     smf_find_science

*  Purpose:
*     Extract science data from an input group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_find_science( ThrWorkForce *wf, const Grp * ingrp, Grp **outgrp, int reverttodark,
*                     Grp **darkgrp, Grp **flatgrp, int reducedark, int calcflat,
*                     smf_dtype darktype, smfArray ** darks,
*                     smfArray **fflats, AstKeyMap **heateffmap,
*                     double * meanstep, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     ingrp = const Grp* (Given)
*        Input group consisting of science and non-science observations.
*     outgrp = Grp ** (Returned)
*        Output group consisting of all the science observations. Can be
*        NULL. If no science observations found, but darks are found, darks
*        will ne returned in outgrp, and darkgrp and flatgrp will be explicitly
*        set to NULL if reverttodark is set.
*     reverttodark = int (Given)
*        If set will revert outgrp to darkgrp if no science observations found
*     darkgrp = Grp ** (Returned)
*        If non-null, will contain the group of dark files. Will not be
*        created if no darks were found. The group will be sorted by
*        date.
*     flatgrp = Grp ** (Returned)
*        If non-null, will contain the group of fast flat files. Will not be
*        created if no fast flats were found. The group will be sorted by
*        date.
*     reducedark = int (Given)
*        Logical, if true the darks are reduced (if needed) and converted
*        to 2d images from the time series. If false, the darks are not
*        touched. Only accessed if "darks" is true.
*     calcflat = int (Given)
*        Calculate the flatfield. Only accessed if "fflats" is non-NULL.
*     darktype = smf_dtype (Given)
*        Data type to use for reduced dark. Only accessed if "reducedark" is
*        true. SMF__NULL will indicate that the data type should be the
*        same as the input type.
*     darks = smfArray ** (Returned)
*        Pointer to smfArray* to be created and filled with darks. Must
*        be freed using smf_close_related. Can be NULL pointer if the darks
*        do not need to be used. *darks will be NULL if no darks were found. Will
*        be sorted by date.
*     fflats = smfArray ** (Returned)
*        Pointer to smfArray* to be created and filled with fast flatfield ramps.
*        Must be freed using smf_close_related. Can be NULL pointer if the flatfields
*        do not need to be returned. *fflats will be NULL if no fast flatfields
*        are found. The flatfield solution will be calculated and stored in the smfDA
*        component of each smfData if "calcflat" is true. Ramps are collapsed using
*        smf_flat_fastflat irrespective of the calcflat parameter.
*     heateffmap = AstKeyMap ** (Returned)
*        AstKeyMap with keys of ARRAYID and values of a pointer to a smfData containing
*        the associated heater efficiency data. Can be NULL.
*     meanstep = double * (Returned)
*        If non-NULL will contain the mean step time of all the data in outgrp
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine opens each of the files in the input group to read
*     the header and determine whether it is science data or non-science
*     (a dark or a fast flat) data. If it is a dark or ramp the data are read
*     and stored in the output smfArray. If it is science data
*     the group entry is copied to the output group and the
*     file is closed.

*  Notes:
*     - All HARP/ACSIS files are considered to be science files.
*     - use smf_close_related to free the smfArray.
*     - if "outgrp" is NULL then only dark information will be returned.
*     - if both "darkgrp" and "darks" are NULL on entry, this routine
*       will simply filter out the dark frames.
*     - it is an error for "darkgrp", "outgrp" and "darks" to all be NULL.
*     - The darks will be returned in time order.
*     - Observations which have a SEQ_TYPE header and whose value differs
*       OBS_TYPE will be filtered out and not copied to outgrp. FASTFLATS
*       can optionally be copied to an output group.
*     - Observations with inconsistent SEQSTART and RTS_NUM entries will be
*       filtered out. These are commonly caused by the DA system using a
*       header from the previous file.
*     - Flatfield calculation requires access to the parameter system and
*       uses a standard set of parameters: REFRES, RESIST, FLATMETH, FLATORDER,
*       "RESPMASK" and "FLATSNR". See the CALCFLAT documentation for more details.
*       There is no facility for creating an output responsivity image.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-17 (TIMJ):
*        Initial version
*     2008-07-29 (TIMJ):
*        Did not realise that ndgCpsup should be used to copy entries.
*     2008-07-31 (TIMJ):
*        Report observation details.
*     2008-12-09 (TIMJ):
*        Do not react badly if a fits header is missing.
*     2009-04-24 (TIMJ):
*        Factor out observation summary reporting.
*     2009-05-25 (TIMJ):
*        Use new smf_obsmap_report API.
*     2009-09-02 (TIMJ):
*        Use smf_find_dateobs rather than directly accessing jcmtstate
*        to be a little more robust with junk files.
*     2009-09-25 (TIMJ):
*        Move sort routine externally.
*     2009-11-24 (TIMJ):
*        Quick hack to filter out SEQ_TYPE ne OBS_TYPE observations. Proper
*        solution is for an additional smfArray to be returned with these
*        items.
*     2010-01-14 (TIMJ):
*        Ignore files that have corrupt FITS header by seeing if the first RTS_NUM
*        matches the SEQSTART value.
*     2010-02-18 (TIMJ):
*        Rename to smf_find_science from smf_find_darks.
*     2010-03-15 (TIMJ):
*        Modify the SEQSTART test to simply check that the rtsnum lies
*        between SEQSTART and SEQEND. This allows for NDF sections to be supplied.
*     2010-04-07 (TIMJ):
*        Ignore dark fast flats.
*     2010-05-11 (EC):
*        Handle data with no JCMTState array
*     2010-06-01 (EC):
*        Return darks in outgrp if no science data were found
*     2010-06-02 (EC):
*        Only revert outgrp to darks if reverttodark set
*     2010-06-28 (TIMJ):
*        Add mean step time calculation
*     2010-07-12 (TIMJ):
*        Disable unstable bolometers as defined from before and after flatfield
*        ramps.
*     2010-07-15 (TIMJ):
*        API change for smf_flat_calcflat
*     2010-07-23 (TIMJ):
*        Trap bad flatfield ramps.
*     2010-08-05 (TIMJ):
*        Trap bad flatfields (at least flatfields with less than SMF__MINSTATSAMP
*        working bolometers).
*     2010-12-07 (TIMJ):
*        Do not do stability comparisons if the previous flat ramp has a different
*        reference heater setting.
*        In NEP mode do not drop bad flatfields.
*        Allow dark flat ramps through. They are now filtered in smf_choose_flat.
*     2011-01-25 (TIMJ):
*        Tweak to smfSortInfo struct member.
*     2011-04-12 (TIMJ):
*        If DARKS are to be treated as SCIENCE then we also need to free the
*        darks smfArray.
*     2011-08-12 (TIMJ):
*        Revamp flatfield handling to ensure that ratios are taken properly
*        for multiple subarrays and to determine if a failed flatfield was
*        required. We only ignore a bad flat if it was not going to be used.
*     2012-04-03 (TIMJ):
*        No longer set status to bad if we have a bad flat. Report the problem
*        and rely on downstream to complain.
*     2012-08-29 (TIMJ):
*        Allow the user to control whether the following flat should be used.
*     2013-11-07 (DSB):
*        All HARP/ACSIS files are now accepted as science files. Previously,
*        a "no SEQCOUNT header found" error was reported if any HARP/ACSIS
*        files were supplied.

*  Copyright:
*     Copyright (C) 2008-2013 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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
#include "ndf.h"
#include "star/thr.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"
#include "ast.h"
#include "star/one.h"
#include "prm_par.h"
#include "par_err.h"
#include "par.h"
#include "subpar_err.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_find_science"

static size_t
smf__addto_sortinfo ( const smfData * indata, smfSortInfo allinfo[],
                      size_t index,
size_t counter, const char * type, int *status );

static void
smf__addto_durations( const smfData *indata, double * duration,
                      size_t * nsteps, int *status );

static void smf__calc_flatobskey( smfHead *hdr, char * keystr, size_t keylen,
                                  int *status );

static int smf__is_file_junk(smfData* data, int* status);

void smf_find_science(ThrWorkForce *wf, const Grp * ingrp, Grp **outgrp, int reverttodark,
                      Grp **darkgrp, Grp **flatgrp, int reducedark,
                      int calcflat, smf_dtype darktype, smfArray ** darks,
                      smfArray **fflats, AstKeyMap ** heateffmap,
                      double * meanstep, int * status ) {

  smfSortInfo *alldarks; /* array of sort structs for darks */
  smfSortInfo *allfflats; /* array of fast flat info */
  Grp * dgrp = NULL;  /* Internal dark group */
  double duration_darks = 0.0; /* total duration of all darks */
  double duration_sci = 0.0;  /* Duration of all science observations */
  size_t dkcount = 0; /* Dark counter */
  size_t ffcount = 0; /* Fast flat counter */
  Grp * fgrp = NULL;  /* Fast flat group */
  size_t i;           /* loop counter */
  smfData *infile = NULL; /* input file */
  size_t insize;     /* number of input files */
  size_t nsteps_dark = 0;    /* Total number of steps for darks */
  size_t nsteps_sci = 0;     /* Total number of steps for science */
  AstKeyMap * heatermap = NULL; /* Heater efficiency map */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  AstKeyMap * scimap = NULL; /* All non-flat obs indexed by unique key */
  Grp *ogrp = NULL;   /* local copy of output group */
  size_t sccount = 0; /* Number of accepted science files */
  size_t junkcount = 0;
  struct timeval tv1;  /* Timer */
  struct timeval tv2;  /* Timer */

  if (meanstep) *meanstep = VAL__BADD;
  if (outgrp) *outgrp = NULL;
  if (darkgrp) *darkgrp = NULL;
  if (darks) *darks = NULL;
  if (fflats) *fflats = NULL;
  if (heateffmap) *heateffmap = NULL;

  if (*status != SAI__OK) return;

  /* Sanity check to make sure we return some information */
  if ( outgrp == NULL && darkgrp == NULL && darks == NULL && fflats == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Must have some non-NULL arguments"
            " (possible programming error)", status);
    return;
  }

  /* Start a timer to see how long this takes */
  smf_timerinit( &tv1, &tv2, status );

  /* Create new group for output files */
  ogrp = smf_grp_new( ingrp, "Science", status );

  /* and a new group for darks */
  dgrp =  smf_grp_new( ingrp, "DarkFiles", status );

  /* and for fast flats */
  fgrp =  smf_grp_new( ingrp, "FastFlats", status );

  /* and also create a keymap for the observation description */
  obsmap = astKeyMap( "KeyError=1" );

  /* and an object map */
  objmap = astKeyMap( "KeyError=1" );

  /* This keymap contains the sequence counters for each related
     subarray/obsidss/heater/shutter combination and is used to decide
     if a bad flat is relevant */
  scimap = astKeyMap( "KeyError=1,KeyCase=0" );

  /* This keymap is used to contain relevant heater efficiency data */
  heatermap = astKeyMap( "KeyError=1,KeyCase=0" );

  /* Work out how many input files we have and allocate sufficient sorting
     space */
  insize = grpGrpsz( ingrp, status );
  alldarks = astCalloc( insize, sizeof(*alldarks) );
  allfflats = astCalloc( insize, sizeof(*allfflats) );

  /* check each file in turn */
  for (i = 1; i <= insize; i++) {
    int seqcount = 0;
    char keystr[100];  /* Key for scimap entry */

    /* open the file but just to get the header */
    smf_open_file( NULL, ingrp, i, "READ", SMF__NOCREATE_DATA, &infile, status );
    if (*status != SAI__OK) break;

    /* Fill in the keymap with observation details */
    smf_obsmap_fill( infile, obsmap, objmap, status );

    if (smf__is_file_junk(infile, status)) {
      junkcount ++;

    /* If this is a HARP/ACSIS file it must be a science file, so store it
       in the output group. */
    } else if( infile->hdr->instrument == INST__ACSIS) {
      smf_smfFile_msg( infile->file, "F", 1, "<unknown file>");
      ndgCpsup( ingrp, i, ogrp, status );
      msgOutif( MSG__DEBUG, " ", "Found ACSIS file ^F",status);
      smf__addto_durations( infile, &duration_sci, &nsteps_sci, status );
      sccount++;

    /* Now handle non-HARP/ACSIS files. */
    } else {

      /* Find the heater efficiency map if required */
      if (*status == SAI__OK && heateffmap) {
        char arrayidstr[32];
        smf_fits_getS( infile->hdr, "ARRAYID", arrayidstr, sizeof(arrayidstr),
                       status );
        if (!astMapHasKey( heatermap, arrayidstr ) ) {
          smfData * heateff = NULL;
          dim_t nbolos = 0;
          smf_flat_params( infile, "RESIST", NULL, NULL, NULL, NULL, NULL,
                           NULL, NULL, NULL, NULL, NULL, &heateff, status );
          smf_get_dims( heateff, NULL, NULL, &nbolos, NULL, NULL, NULL, NULL,
                        status );
          if (heateff) astMapPut0P( heatermap, arrayidstr, heateff, NULL );
        }
      }

      /* Get the sequence counter for the file. We do not worry about
         duplicate sequence counters (at the moment) */
      smf_find_seqcount( infile->hdr, &seqcount, status );

      /* The key identifying this subarray/obsidss/heater/shutter combo */
      smf__calc_flatobskey( infile->hdr, keystr, sizeof(keystr), status );

      if (smf_isdark( infile, status )) {
        /* Store the sorting information */
        dkcount = smf__addto_sortinfo( infile, alldarks, i, dkcount, "Dark", status );
        smf__addto_durations( infile, &duration_darks, &nsteps_dark, status );
        astMapPutElemI( scimap, keystr, -1, seqcount );
      } else {
        /* compare sequence type with observation type and drop it (for now)
           if they differ */
        if ( infile->hdr->obstype == infile->hdr->seqtype ) {
          /* Sanity check the header for corruption. Compare RTS_NUM with SEQSTART
             and SEQEND. The first RTS_NUM must either be SEQSTART or else between
             SEQSTART and SEQEND (if someone has giving us a section) */
          int seqstart = 0;
          int seqend = 0;
          int firstnum = 0;
          JCMTState *tmpState = NULL;
          smf_getfitsi( infile->hdr, "SEQSTART", &seqstart, status );
          smf_getfitsi( infile->hdr, "SEQEND", &seqend, status );
          tmpState = infile->hdr->allState;

          if( tmpState ) {
            firstnum = (tmpState[0]).rts_num;
            smf_smfFile_msg( infile->file, "F", 1, "<unknown file>");
            if ( firstnum >= seqstart && firstnum <= seqend ) {
              /* store the file in the output group */
              ndgCpsup( ingrp, i, ogrp, status );
              msgOutif(MSG__DEBUG, " ", "Non-dark file: ^F",status);
              smf__addto_durations( infile, &duration_sci, &nsteps_sci, status );
              astMapPutElemI( scimap, keystr, -1, seqcount );
              sccount++;
            } else {
              msgOutif( MSG__QUIET, "",
                        "File ^F has a corrupt FITS header. Ignoring it.",
                        status );
            }
          } else {
            smf_smfFile_msg( infile->file, "F", 1, "<unknown file>");
            /* store the file in the output group */
            ndgCpsup( ingrp, i, ogrp, status );
            msgOutif( MSG__DEBUG, " ",
                      "File ^F lacks JCMTState: assuming it is non-dark",status);
            smf__addto_durations( infile, &duration_sci, &nsteps_sci, status );
            astMapPutElemI( scimap, keystr, -1, seqcount );
            sccount++;
          }

        } else if (infile->hdr->seqtype == SMF__TYP_FASTFLAT ) {
          ffcount = smf__addto_sortinfo( infile, allfflats, i, ffcount, "Fast flat", status );
        } else {
          smf_smfFile_msg( infile->file, "F", 1, "<unknown file>");
          msgOutif(MSG__DEBUG, " ", "Sequence type mismatch with observation type: ^F",status);
        }
      }
    }

    /* close the file */
    smf_close_file( wf, &infile, status );
  }

  /* Store output group in return variable or else free it */
  if (outgrp) {
    *outgrp = ogrp;
  } else {
    grpDelet( &ogrp, status );
  }

  /* process flatfields if necessary */
  if (ffcount > 0 && fflats ) {
    smfArray * array = NULL;

    /* sort flats into order */
    qsort( allfflats, ffcount, sizeof(*allfflats), smf_sort_bydouble);

    if (fflats) array = smf_create_smfArray( status );

    /* now open the flats and store them if requested */
    if (*status == SAI__OK && array && ffcount) {
      size_t start_ffcount = ffcount;
      AstKeyMap * flatmap = NULL;

      if (calcflat) {
        /* Use AgeUp so that we get the keys out in the sorted order
           that allfflats used */
        flatmap = astKeyMap( "KeyCase=0,KeyError=1,SortBy=AgeDown" );
      }

      /* Read each flatfield. Calculate a responsivity image and a flatfield
         solution. Store these in a keymap along with related information
         which is itself stored in a keymap indexed by a string made of
         OBSIDSS, reference heater value, shutter and subarray.
      */

      for (i = 0; i < start_ffcount; i++ ) {
        size_t ori_index =  (allfflats[i]).index;
        smfData * outfile = NULL;
        char keystr[100];
        AstKeyMap * infomap = astKeyMap( "KeyError=1" );
        int oplen = 0;
        char thisfile[MSG__SZMSG];
        int seqcount = 0;

        /* read filename from group */
        infile = NULL;
        smf_open_file( NULL, ingrp, ori_index, "READ", 0, &infile, status );
        if ( *status != SAI__OK ) {
          /* This should not happen because we have already opened
             the file. If it does happen we abort with error. */
          if (infile) smf_close_file( wf, &infile, status );
          break;
        }

        /* Calculate the key for this observation */
        smf__calc_flatobskey( infile->hdr, keystr, sizeof(keystr), status );

        /* Get the file name for error messages */
        smf_smfFile_msg( infile->file, "F", 1, "<unknown file>" );
        msgLoad( "", "^F", thisfile, sizeof(thisfile), &oplen, status );

        /* And the sequence counter to link against science observations */
        smf_find_seqcount( infile->hdr, &seqcount, status );

        /* Prefill infomap */
        astMapPut0C( infomap, "FILENAME", thisfile, "");
        astMapPut0I( infomap, "SEQCOUNT", seqcount, "");

        /* Collapse it */
        if (*status == SAI__OK) {
          smf_flat_fastflat( infile, &outfile, status );
          if (*status == SMF__BADFLAT) {
            errFlush( status );

            if (calcflat) {
              /* Need to generate an outfile like smf_flat_fastflat
                 and one heater setting will force smf_flat_calcflat to fail */
              smf_flat_malloc( 1, infile, NULL, &outfile, status );
            } else {
              if (outfile) smf_close_file( wf, &outfile, status );
              if (infile) smf_close_file( wf, &infile, status );
              infomap = astAnnul( infomap );
              ffcount--;
              continue;
            }
          }
        }

        if (outfile && *status == SAI__OK) {
          smf_close_file( wf, &infile, status );
          infile = outfile;

          if (calcflat) {
            size_t ngood = 0;
            smfData * curresp = NULL;
            int utdate;

            if (*status == SAI__OK) {
              ngood = smf_flat_calcflat( wf, MSG__VERB, NULL, "RESIST",
                                         "FLATMETH", "FLATORDER", NULL, "RESPMASK",
                                         "FLATSNR", NULL, infile, &curresp, status );
              if (*status != SAI__OK) {
                /* if we failed to calculate a flatfield we continue but force the
                   flatfield to be completely bad. This will force the science data associated
                   with the flatfield to be correctly blanked. We do not annul though
                   if we have a SUBPAR error telling us that we have failed to define
                   our parameters properly. */
                if (*status != SUBPAR__NOPAR) errAnnul(status);

                /* parameters of flatfield */
                ngood = 0;

                /* Generate a blank flatfield and blank responsivity image */
                smf_flat_badflat( wf, infile, &curresp, status );
              }

              /* Retrieve the UT date so we can decide whether to compare
                 flatfields */
              smf_getfitsi( infile->hdr, "UTDATE", &utdate, status );

              /* Store the responsivity data for later on and the processed
                 flatfield until we have vetted it */
              astMapPut0P( infomap, "CALCFLAT", infile, "" );
              astMapPut0P( infomap, "RESP", curresp, "" );
              astMapPut0I( infomap, "UTDATE", utdate, "" );
              astMapPut0I( infomap, "ISGOOD", 1, "" );
              astMapPut0I( infomap, "NGOOD", ngood, "" );
              astMapPut0I( infomap, "GRPINDEX", ori_index, "" );
              astMapPut0I( infomap, "SMFTYP", infile->hdr->obstype, "" );
              astMapPutElemA( flatmap, keystr, -1, infomap );

            }

          } else { /* if (calcflat) */
            /* Store the collapsed flatfield  - the processed flat is not stored here yet */
            smf_addto_smfArray( array, infile, status );

            /* Copy the group info */
            ndgCpsup( ingrp, ori_index, fgrp, status );

          }

        } /* if (outfile) */

        /* Annul the keymap (will be fine if it is has been stored in another keymap) */
        infomap = astAnnul( infomap );

      } /* End loop over flatfields */

      /* Now we have to loop over the related flatfields to disable
         bolometers that are not good and also decide whether we
         need to set status to bad. */
      if (*status == SAI__OK && calcflat ) {
        size_t nkeys = astMapSize( flatmap );
        for (i = 0; i < nkeys; i++ ) {
          const char *key = astMapKey( flatmap, i );
          int nf = 0;
          AstKeyMap ** kmaps = NULL;
          int nelem = astMapLength( flatmap, key );
          kmaps = astMalloc( sizeof(*kmaps) * nelem );
          astMapGet1A( flatmap, key, nelem, &nelem, kmaps );

          for ( nf = 0; nf < nelem && *status == SAI__OK; nf++ ) {
            AstKeyMap * infomap = kmaps[nf];
            int isgood = 0;

            astMapGet0I( infomap, "ISGOOD", &isgood );

            if (isgood) {
              /* The flatfield worked */
              size_t ngood = 0;
              int itemp;
              int utdate = 0;
              int ratioFlats = 0;

              /* Get the UT date - we do not compare flatfields after
                 the time we enabled heater tracking at each sequence. */
              astMapGet0I( infomap, "UTDATE", &utdate );

              /* Get the number of good bolometers at this point */
              astMapGet0I( infomap, "NGOOD", &itemp );
              ngood = itemp;

              /* Decide if we want to do the ratio test. We default to
                 not doing it between 20110901 and 20120827 which is
                 the period when we did mini-heater tracks before each
                 flat. ! indicates that we choose based on date. */
              if (*status == SAI__OK) {
                parGet0l( "FLATUSENEXT", &ratioFlats, status );
                if ( *status == PAR__NULL ) {
                  errAnnul( status );
                  if (utdate >= 20110901 || utdate <= 20120827 ) {
                    ratioFlats = 0;
                  } else {
                    ratioFlats = 1;
                  }
                }
              }

              /* Can we compare with the next flatfield? */
              if (ngood < SMF__MINSTATSAMP || !ratioFlats ) {
                /* no point doing all the ratio checking for this */
              } else if ( nelem - nf >= 2 ) {
                AstKeyMap * nextmap = kmaps[nf+1];
                const char *nextfname = NULL;
                const char *fname = NULL;
                smfData * curresp = NULL;
                smfData * nextresp = NULL;
                smfData * curflat = NULL;
                void *tmpvar = NULL;
                size_t bol = 0;
                smfData * ratio = NULL;
                double *in1 = NULL;
                double *in2 = NULL;
                double mean = VAL__BADD;
                size_t nbolo;
                double *out = NULL;
                double sigma = VAL__BADD;
                float clips[] = { 5.0, 5.0 }; /* 5.0 sigma iterative clip */
                size_t ngoodz = 0;

                astMapGet0C( nextmap, "FILENAME", &nextfname );
                astMapGet0C( infomap, "FILENAME", &fname );

                /* Retrieve the responsivity images from the keymap */
                astMapGet0P( infomap, "RESP", &tmpvar );
                curresp = tmpvar;
                astMapGet0P( nextmap, "RESP", &tmpvar );
                nextresp = tmpvar;
                astMapGet0P( infomap, "CALCFLAT", &tmpvar );
                curflat = tmpvar;

                nbolo = (curresp->dims)[0] * (curresp->dims)[1];

                /* get some memory for the ratio if we have not already.
                   We could get some memory once assuming each flat has the
                   same number of bolometers... */
                ratio = smf_deepcopy_smfData( wf, curresp, 0, 0, 0, 0, status );
                if( *status == SAI__OK ) {

                  /* divide: smf_divide_smfData ? */
                  in1 = (curresp->pntr)[0];
                  in2 = (nextresp->pntr)[0];
                  out = (ratio->pntr)[0];

                  for (bol=0; bol<nbolo;bol++) {
                    if ( in1[bol] != VAL__BADD && in1[bol] != 0.0 &&
                         in2[bol] != VAL__BADD && in2[bol] != 0.0 ) {
                      out[bol] = in1[bol] / in2[bol];
                    } else {
                      out[bol] = VAL__BADD;
                    }
                  }
                }

                /* find some statistics */
                smf_clipped_stats1D( out, 2, clips, 1, nbolo, NULL, 0, 0, &mean,
                                     &sigma, NULL, 0, &ngoodz, status );

                if (*status == SMF__INSMP) {
                  errAnnul(status);
                  msgOutiff( MSG__QUIET, "",
                            "Flatfield ramp ratio of %s with %s had too few bolometers (%zu < %d).",
                             status, fname, nextfname, ngoodz, SMF__MINSTATSAMP );
                  ngood = ngoodz; /* Must be lower or equal to original ngood */

                } else if (*status == SAI__OK && mean != VAL__BADD && sigma != VAL__BADD && curflat->da) {
                  /* Now flag the flatfield as bad for bolometers that have changed
                     more than n%. We expect the variation to be 1+/-a small bit */
                  const double pmrange = 0.10;
                  double thrlo = 1.0 - pmrange;
                  double thrhi = 1.0 + pmrange;
                  size_t nmasked = 0;
                  double *flatcal = curflat->da->flatcal;

                  msgOutiff( MSG__DEBUG, "", "Flatfield fast ramp ratio mean = %g +/- %g (%zu bolometers)",
                             status, mean, sigma, ngood);

                  /* we can just set the first slice of the flatcal to bad. That should
                     be enough to disable the entire bolometer. We have just read these
                     data so they should be in ICD order. */
                  for (bol=0; bol<nbolo;bol++) {
                    if ( out[bol] != VAL__BADD &&
                         (out[bol] < thrlo || out[bol] > thrhi ) ) {
                      flatcal[bol] = VAL__BADD;
                      nmasked++;
                    } else if ( in1[bol] != VAL__BADD && in2[bol] == VAL__BADD ) {
                      /* A bolometer is bad next time but good now so we must set it bad now */
                      flatcal[bol] = VAL__BADD;
                      nmasked++;
                    }
                  }

                  if ( nmasked > 0 ) {
                    msgOutiff( MSG__NORM, "", "Masked %zu bolometers in %s from unstable flatfield",
                               status, nmasked, fname );

                    /* update ngood to take into account the masking */
                    ngood -= nmasked;
                  }

                }

                smf_close_file( wf, &ratio, status );

              } /* End of flatfield responsivity comparison */

              /* if we only have a few bolometers left we now consider this
                 a bad flat unless it is actually an engineering measurement
                 where expect some configurations to give zero bolometers */
              if (ngood < SMF__MINSTATSAMP) {
                const char *fname = NULL;
                void * tmpvar = NULL;
                int smftyp = 0;
                smfData * curflat = NULL;
                astMapGet0I( infomap, "SMFTYP", &smftyp );
                astMapGet0C( infomap, "FILENAME", &fname );
                if (smftyp != SMF__TYP_NEP) {
                  msgOutiff( MSG__QUIET, "",
                            "Flatfield %s has %zu good bolometer%s.%s",
                             status, fname, ngood, (ngood == 1 ? "" : "s"),
                             ( ngood == 0 ? "" : " Keeping none.") );
                  isgood = 0;

                  /* Make sure that everything is blanked. */
                  if (ngood > 0) {
                    astMapGet0P( infomap, "CALCFLAT", &tmpvar );
                    curflat = tmpvar;
                    if (curflat && curflat->da) {
                      size_t bol;
                      size_t nbolo = (curflat->dims)[0] * (curflat->dims)[1];
                      double *flatcal = curflat->da->flatcal;
                      for (bol=0; bol<nbolo; bol++) {
                        /* Just need to set the first element to bad */
                        flatcal[bol] = VAL__BADD;
                      }
                    }
                  }

                } else {
                  msgOutiff( MSG__NORM, "",
                            "Flatfield ramp file %s has %zu good bolometer%s. Eng mode.",
                             status, fname, ngood, (ngood == 1 ? "" : "s") );
                }
              }

              /* We do not need the responsivity image again */
              {
                void *tmpvar = NULL;
                smfData * resp = NULL;
                astMapGet0P( infomap, "RESP", &tmpvar );
                resp = tmpvar;
                if (resp) smf_close_file( wf, &resp, status );
                astMapRemove( infomap, "RESP" );
              }

            } /* End of isgood comparison */

            /* We are storing flats even if they failed. Let the downstream
               software worry about it */
            {
              int ori_index;
              smfData * flatfile = NULL;
              void *tmpvar = NULL;

              /* Store in the output group */
              astMapGet0I( infomap, "GRPINDEX", &ori_index );
              ndgCpsup( ingrp, ori_index, fgrp, status );

              /* And store in the smfArray */
              astMapGet0P( infomap, "CALCFLAT", &tmpvar );
              astMapRemove( infomap, "CALCFLAT" );
              flatfile = tmpvar;
              smf_addto_smfArray( array, flatfile, status );
            }

            /* Free the object as we go */
            kmaps[nf] = astAnnul( kmaps[nf] );
          } /* End of loop over this obsidss/subarray/heater */

          kmaps = astFree( kmaps );

        }
      }

      if (array->ndat) {
        if (fflats) *fflats = array;
      } else {
        smf_close_related( wf, &array, status );
        if (fflats) *fflats = NULL;
      }

      if( flatmap ) flatmap = astAnnul( flatmap );

    }
  }

  /* no need to do any more if neither darks nor darkgrp are defined or we might
     be wanting to revert to darks. */
  if (dkcount > 0 && (darks || darkgrp || reverttodark ) ) {
    smfArray * array = NULL;

    /* sort darks into order */
    qsort( alldarks, dkcount, sizeof(*alldarks), smf_sort_bydouble);

    if (darks) array = smf_create_smfArray( status );

    /* now open the darks and store them if requested */
    if (*status == SAI__OK) {
      for (i = 0; i < dkcount; i++ ) {
        size_t ori_index =  (alldarks[i]).index;

         /* Store the entry in the output group */
        ndgCpsup( ingrp, ori_index, dgrp, status );

        if (darks) {

          /* read the value from the new group */
          smf_open_file( NULL, dgrp, i+1, "READ", 0, &infile, status );

          /* do we have to process these darks? */
          if (reducedark) {
            smfData *outfile = NULL;
            smf_reduce_dark( infile, darktype, &outfile, status );
            if (outfile) {
              smf_close_file( wf, &infile, status );
              infile = outfile;
            }
          }

          smf_addto_smfArray( array, infile, status );
        }
      }
      if (darks) *darks = array;
    }
  }

  /* free memory */
  alldarks = astFree( alldarks );
  allfflats = astFree( allfflats );

  if( reverttodark && outgrp && (grpGrpsz(*outgrp,status)==0) &&
      (grpGrpsz(dgrp,status)>0) ) {
    /* If outgrp requested but no science observations were found, and
       dark observations were found, return darks in outgrp and set
       flatgrp and darkgrp to NULL. This is to handle cases where we
       want to process data taken in the dark like normal science
       data. To activate this behaviour set reverttodark */

    msgOutiff( MSG__NORM, "", "Treating the dark%s as science data",
               status, ( dkcount > 1 ? "s" : "" ) );

    *outgrp = dgrp;

    if( darkgrp ){
      *darkgrp = NULL;
    }

    if( flatgrp ) {
      *flatgrp = NULL;
    }

    grpDelet( &ogrp, status);
    grpDelet( &fgrp, status);

    if (meanstep && nsteps_dark > 0) *meanstep = duration_darks / nsteps_dark;

    /* Have to clear the darks smfArray as well */
    if (darks) smf_close_related( wf, darks, status );

  } else {
    /* Store the output groups in the return variable or free it */
    if (darkgrp) {
      *darkgrp = dgrp;
    } else {
      grpDelet( &dgrp, status);
    }
    if (flatgrp) {
      *flatgrp = fgrp;
    } else {
      grpDelet( &fgrp, status);
    }

    if (meanstep && nsteps_sci > 0) *meanstep = duration_sci / nsteps_sci;
  }

  msgSeti( "ND", sccount );
  msgSeti( "DK", dkcount );
  msgSeti( "FF", ffcount );
  msgSeti( "JK", junkcount );
  msgSeti( "TOT", insize );
  if ( insize == 1 ) {
    if (dkcount == 1) {
      msgOutif( MSG__VERB, " ", "Single input file was a dark",
                status);
    } else if (ffcount == 1) {
      msgOutif( MSG__VERB, " ", "Single input file was a fast flatfield",
                status);
    } else if (sccount == 1) {
      msgOutif( MSG__VERB, " ", "Single input file was accepted (observation type same as sequence type)",
                status);
    } else {
      msgOutif( MSG__VERB, " ", "Single input file was not accepted.",
                status);
    }

  } else {
    if (dkcount == 1) {
      msgSetc( "DKTXT", "was a dark");
    } else {
      msgSetc( "DKTXT", "were darks");
    }
    if (ffcount == 1) {
      msgSetc( "FFTXT", "was a fast flat");
    } else {
      msgSetc( "FFTXT", "were fast flats");
    }
    if (sccount == 1) {
      msgSetc( "NDTXT", "was science");
    } else {
      msgSetc( "NDTXT", "were science");
    }
    if (junkcount == 1) {
      msgSetc("JKTXT", "was junk");
    } else {
      msgSetc("JKTXT", "were junk");
    }

    /* This might be a useful message */
    msgOutif( MSG__NORM, " ", "Out of ^TOT input files, ^DK ^DKTXT, ^FF ^FFTXT, "
              "^ND ^NDTXT and ^JK ^JKTXT", status );
  }

  if (meanstep && *meanstep != VAL__BADD) {
    msgOutiff( MSG__VERB, "", "Mean step time for input files = %g sec",
             status, *meanstep );
  }

  /* Store the heater efficiency map */
  if (*status != SAI__OK) heatermap = smf_free_effmap( heatermap, status );
  if (heateffmap) *heateffmap = heatermap;

  /* Now report the details of the observation */
  smf_obsmap_report( MSG__NORM, obsmap, objmap, status );

  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );
  scimap = astAnnul( scimap );

  msgOutiff( SMF__TIMER_MSG, "",
             "Took %.3f s to find science observations",
             status, smf_timerupdate( &tv1, &tv2, status ) );

  return;
}


/* Routine to put info from smfData into a sort struct. "this_index" is the index
   in the input group. "counter" is the current position to use to store
   the item. Returns the next index to be used (ie updated counter). */

static size_t
smf__addto_sortinfo ( const smfData * indata, smfSortInfo allinfo[], size_t this_index,
                      size_t counter, const char * type, int *status ) {
  smfSortInfo * sortinfo = NULL;

  if (*status != SAI__OK) return counter;

  /* store the name and time in the struct */
  sortinfo = &(allinfo[counter]);
  one_strlcpy( sortinfo->name, indata->file->name, sizeof(sortinfo->name),
               status );
  smf_find_dateobs( indata->hdr, &(sortinfo->sortval), NULL, status );
  msgOutiff(MSG__DEBUG, " ", "%s file: %s",status,
            type, indata->file->name);
  sortinfo->index = this_index;
  counter++;
  return counter;
}

/* Helper routine to just add the number of steps and the duration of the
   file to a running total */

static void smf__addto_durations ( const smfData *indata, double *duration,
                                   size_t *nsteps, int *status ) {
  if (*status != SAI__OK) return;
  if (!indata) return;
  if (!indata->hdr) return;
  if (indata->hdr->steptime == VAL__BADD) return;
  if (indata->hdr->nframes == 0) return;

  *nsteps += indata->hdr->nframes;
  *duration += ( indata->hdr->nframes * indata->hdr->steptime );

  return;
}

static void smf__calc_flatobskey( smfHead *hdr, char * keystr, size_t keylen,
                                  int *status ) {

  int curheat = 0;
  int detbias = 0;
  char subarray[10];
  double shutter = 0.0;
  size_t nwrite = 0;
  int utdate = 0;

  if (*status != SAI__OK) return;
  if (!hdr) return;

  /* get reference heater value, bias, shutter and subarray string */
  /* PIXHEAT and DETBIAS are not written by the simulator so we default
     those to 0 */
  smf_getfitsi( hdr, "PIXHEAT", &curheat, status );
  if (*status == SMF__NOKWRD) errAnnul(status);

  /* As of September 1st we heater track after doing the flat ramp.
     This means that the heater value will have changed slightly between
     ramp and science. */
  smf_getfitsi( hdr, "UTDATE", &utdate, status );
  if (utdate >= 20110901 &&
      (hdr->obstype == SMF__TYP_SCIENCE || hdr->obstype == SMF__TYP_POINTING)) {
    curheat = 0;
  }

  smf_getfitsi( hdr, "DETBIAS", &detbias, status );
  if (*status == SMF__NOKWRD) errAnnul(status);

  smf_getfitsd( hdr, "SHUTTER", &shutter, status );
  smf_find_subarray( hdr, subarray, sizeof(subarray), NULL,
                     status );
  if (*status != SAI__OK) return;

  nwrite = snprintf(keystr, keylen, "%s_%s_%.1f_%d_%d",
                    hdr->obsidss, subarray, shutter, detbias, curheat );

  if (nwrite >= keylen) {
    /* The string was truncated */
    *status = SMF__STRUN;
    errRep("", "String truncation forming flatfield key (Possible programming error)",
           status );
  }

}


/* Check whether the given file is known to be junk. */
static int smf__is_file_junk(smfData* data, int* status) {
    if (SAI__OK != *status) {
        return 0;
    }

    char obsidss[81];
    smf_getobsidss(data->hdr->fitshdr, 0, 0, obsidss, sizeof(obsidss), status);

    int subscan;
    smf_getfitsi(data->hdr, "NSUBSCAN", &subscan, status);

    if (SAI__OK != *status) {
        errAnnul(status);
        return 0;
    }

    static const struct {
        const char* obsidss;
        int ss_start;
        int ss_end;
    } junk[] = {
        /* TSS inadvertently closed shutter in the middle of
           20170401 observation 10: see fault 20170401.003. */
        {"scuba2_10_20170401T040818_850", 35, 38},
        {"scuba2_10_20170401T040818_450", 35, 38},

        /* TSS closed roof and doors after ten minutes, see log.*/
        {"scuba2_30_20180412T133748_450", 21, 24},
        {"scuba2_30_20180412T133748_850", 21, 24},

        /*Emergency stop was hit during the observation.*/
        {"scuba2_00039_20160908T152015_450", 38, 47},
        {"scuba2_00039_20160908T152015_850", 38, 47},

        /* End marker. */
        {0, 0, 0},
    };

    for (size_t i = 0; junk[i].obsidss; i ++) {
        if ((subscan >= junk[i].ss_start)
                && (subscan <= junk[i].ss_end)
                && ! strcmp(junk[i].obsidss, obsidss)) {
            smf_smfFile_msg(data->file, "F", 1, "<unknown file>");
            msgOut("", "File ^F identified as junk", status);

            return 1;
        }
    }

    return 0;
}
