/*
 *+
 *  Name:
 *     smf_grp_related

 *  Purpose:
 *     Create a group of related files

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SMURF subroutine

 *  Invocation:
 *     smf_grp_related( Grp *igrp, const size_t grpsize,
 *                      const int grouping, const int checksubinst,
 *                      double maxlen_s, double *srate_maxlen,
 *                      AstKeyMap *keymap, dim_t *maxconcatlen,
 *                      dim_t *maxfilelen, smfGroup **group, Grp **basegrp,
 *                      dim_t *pad, int *status );

 *  Arguments:
 *     igrp = Grp* (Given)
 *        Input Grp. The entries in this Grp for any rejected subscans will
 *        be set to a blank string on exit.
 *     grpsize = const size_t (Given)
 *        Size of input Grp
 *     grouping = const int (Given)
 *        Flag describing how to group the data: 0 = all data taken
 *        simultaneously; 1 = all data at same wavelength taken
 *        simultaneously; 2 = only data from same subarray.
 *     checksubinst = int (Given)
 *        If set, and a mixture of subinstruments (i.e. 450 and 850 um)
 *        are encountered, bad status will be set.
 *     maxlen_s = double (Given)
 *        If set, maximum length of a continuous chunk in seconds.
 *        If 0 don't enforce a maximum length.
 *     srate_maxlen = double * (Returned)
 *        If non-NULL, return the sample rate that was used (or would be used)
 *        internally to convert maxlen_s into a number of samples (taken from
 *        first file)
 *     keymap = AstKeyMap * (Given)
 *        A pointer to a KeyMap holding the configuration parameters. Only
 *        needed if "pad" is not NULL.
 *     maxconcatlen = dim_t* (Returned)
 *        The actual length in time samples of the longest continuous chunk.
 *        Can be NULL.
 *     maxfilelen = dim_t* (Returned)
 *        Max length in time samples of an individual data file.
 *     group = smfGroup ** (Returned)
 *        Returned smfGroup
 *     basegrp = Grp ** (Returned)
 *        For each group of related files, this group will contain the
 *        names of the first member in each of those groups. Can
 *        be used as a basis group for requesting output files that
 *        could be derived from the smfGroup. Can be NULL if this information
 *        is not required.
 *     pad = dim_t * (Returned)
 *        The number of samples of padding to add to the start and end of
 *        each time stream to avoid wrap-around efffects and ringing when
 *        filtering. May be NULL.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine groups related files together and populates a
 *     smfGroup. Related files are those that were written at the same
 *     time by the data acquisition system and so will have the same
 *     values for the DATE-OBS, OBSID and SEQCOUNT FITS
 *     headers. Additionally if grouping is 1, files will not be
 *     designated as related if they have a different value for the
 *     WAVELEN FITS header. For SCUBA-2 this means that there will be
 *     at most 8 related files unless we are grouping by wavelength
 *     where the limit will be 4 files. If the related files have
 *     different dimensions they are not treated as related files. If
 *     grouping is 2, data from different subarrays will not be
 *     considered related.
 *
 *     The constructed smfGroup stores the indices of the file as they occur
 *     in the supplied Grp in the smfGroup.subgroups array which is an array
 *     of size_t pointers indexed by group number and then number of related
 *     files. If an entry is zero there is no corresponding file for that
 *     slot (since there are at most 4 or 8 files but there does not need
 *     to be that many for each group).
 *
 *     The smfGroup is filled in time order. If we are grouping by wavelength
 *     the sorting will be in time order for each wavelength. Contiguous chunks
 *     of related files that all share the same sequence (SEQCOUNT and OBSID
 *     being equal) are assigned an integer "chunk number" which is stored in
 *     smfGroup.chunks. The chunk number increments if the consecutive groups
 *     are not part of the sequence, if the sequence is too long ("maxlen_s")
 *     or if the two groups contain different subarrays. Files are not broken
 *     up so it is not guaranteed that chunks are equal size.
 *
 *     The supplied Grp is cloned and the Grp stored in the smfGroup
 *     is independent of the supplied Grp which can be deleted.
 *
 *     If the input files are 4D FFT data, each file appears in a single time
 *     chunk (i.e. checking for continuity does not make sense in this case).

 *  Notes:
 *     - Resources allocated with this routine should be freed by calling
 *     smf_close_smfGroup
 *     - This routine is not clever when deciding to break continuous chunks
 *     into smaller entities if "maxlen_s" is exceeded. If there are 6 files
 *     contributing to a sequence and the 6th makes the sequence too long
 *     this function will return two chunks with one chunk containing 5
 *     files and the other containing 1. The clever thing to do would be to
 *     return two chunks of 3 files in each.

 *  Authors:
 *     Andy Gibb (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     Ed Chapin (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2006-06-24 (AGG):
 *        Initial version
 *     2006-07-26 (TIMJ):
 *        sc2head no longer used. Use JCMTState instead.
 *     2006-10-11 (AGG):
 *        Additional checks for relatedness: wavelength and dimensions
 *        of data array
 *     2007-07-16 (EC):
 *        -Changed smf_construct_smfGroup interface
 *     2007-10-29 (EC):
 *        Modified interface to smf_open_file; use SMF__NOCREATE_DATA flag.
 *     2007-12-14 (EC):
 *        Actually use SMF__NOCREATE_DATA in smf_open_file call.
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-04-16 (EC):
 *        Fill chunk component of smfGroup based on time stamps
 *     2008-04-17 (EC):
 *        - check for the same subarrays in each chunk
 *        - added maxlen to interface
 *     2008-04-18 (EC):
 *        - check for very short chunks and remove from smfGroup
 *        - fixed memory unallocation bug
 *     2008-04-24 (EC):
 *        - Determine actual nrelated for these data and store in smfGroup
 *     2008-04-28 (EC):
 *        - Added maxconcatlen to interface
 *     2008-04-29 (EC):
 *        -Fixed bug in calculation of actual nrelated (subarrays)
 *     2008-05-03 (EC):
 *        Check absolute value of time step to find discontinuities
 *     2008-07-11 (TIMJ):
 *        sizeof(int) does not work when your ints are changed to longs.
 *        Always use sizeof(*var).
 *     2008-07-18 (TIMJ):
 *        Use smf_find_subarray
 *     2008-07-29 (TIMJ):
 *        Steptime is now in smfHead.
 *     2008-08-20 (EC):
 *        Establish continuity based on OBSIDSS, SEQCOUNT and NSUBSCAN
 *     2008-11-14 (TIMJ):
 *        Use smf_find_seqcount
 *     2009-10-01 (TIMJ):
 *        Add basegrp argument.
 *     2010-03-02 (EC):
 *        Added maxfilelen to interface.
 *     2010-10-04 (DSB):
 *        Added pad and keymap to interface.
 *     2010-10-25 (EC):
 *        Move down-sampling length calc here from smf_concat_smfGroup
 *     2010-10-26 (EC):
 *        Account for down-sampling in maxlen
 *     2010-10-28 (EC):
 *        Account for down-sampling in pad
 *     2010-11-01 (EC):
 *        Handle 4d FFT data by placing each file in its own time chunk
 *     2011-01-19 (TIMJ):
 *        Simulated RTS does not have the same precision as the real RTS
 *        so we have to be careful when using rts_end for multiple
 *        subarrays.
 *     2011-01-24 (TIMJ):
 *        Rewrite to use an AST KeyMap to determine related files. Has the
 *        side effect of sorting the chunks into date order.
 *     2011-01-25 (TIMJ):
 *        Do not scale maxlen.
 *     2011-04-15 (EC):
 *        Change grpbywave to grouping to enable handling subarrays separately
 *     2011-04-20 (DSB):
 *        Change interface for smf_get_padding.
 *     2011-07-06 (DSB):
 *        Fix infinite loop when freeing memory used by subgroups.
 *     2011-08-25 (EC):
 *        Add checksubinst option
 *     2011-08-26 (EC):
 *        Change checkwave to checksubinst for robustness against future
 *        SCUBA-2 filter modifications
 *     2011-09-08 (EC):
 *        Provide maxlen_s instead of maxlen so that correct down-sampled length
 *        may be calculated.
 *     2011-09-09 (EC):
 *        Add srate_maxlen
 *     2018-10-02 (DSB):
 *        Handle cases where input does not have a NSUBSCAN value (e.g.
 *        if it is the concatenation of several subscans).
 *     2019-9-9 (DSB):
 *        If smf_open_file reports a SMF__REJECT error (caused by the
 *        VALIDATE_SCANS parameter being set to -1 and the subscan being
 *        crazy), annul the error, reject the subscan and continue to
 *        process other subscans. This will cause a the data to be split
 *        into separate groups at the rejected subscan.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2008-2011 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2011 University of British Columbia.
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_grp_related"

void smf_grp_related( Grp *igrp, const size_t grpsize,
                      const int grouping, const int checksubinst,
                      double maxlen_s, double *srate_maxlen,
                      AstKeyMap *keymap, dim_t *maxconcatlen,
                      dim_t *maxfilelen, smfGroup **group,
                      Grp **basegrp, dim_t *pad, int *status ) {

  /* Local variables */
  size_t *chunk=NULL;         /* Array of flags for continuous chunks */
  dim_t * chunklen = NULL;    /* Length of continuous chunk */
  dim_t chunkminlen;          /* Min length of continuous chunk */
  size_t currentindex = 0;    /* Counter */
  char cwave[10];             /* String containing wavelength */
  smfData *data = NULL;       /* Current smfData */
  double downsampscale=0;     /* Angular scale downsampling size */
  double downsampfreq=0;      /* Target downsampling frequency */
  AstKeyMap * grouped = NULL; /* Primary AstKeyMap for grouping */
  size_t i;                   /* Loop counter for index into Grp */
  int isFFT=0;                /* Set if data are 4d FFT */
  size_t j;                   /* Loop counter */
  int *keepchunk=NULL;        /* Flag for chunks that will be kept */
  dim_t maxconcat=0;          /* Longest continuous chunk length */
  dim_t maxflen=0;            /* Max file length in time steps */
  dim_t maxlen=0;             /* Maximum concat length in samples */
  int maxlen_scaled=0;        /* Set once maxlen has been scaled, if needed */
  dim_t maxpad=0;             /* Maximum padding neeed for any input file */
  size_t maxrelated = 0;      /* Keep track of max number of related items */
  size_t *new_chunk=NULL;     /* keeper chunks associated with subgroups */
  dim_t *new_tlen=NULL;       /* tlens for new_subgroup */
  size_t ngroups = 0;         /* Counter for subgroups to be stored */
  size_t nkeep = 0;           /* Number of chunks to keep */
  dim_t * piecelen = NULL;    /* Length of single file */
  int pol2;                   /* Got pol2 stokes parameter data? */
  smf_subinst_t refsubinst;   /* Subinst of first file */
  size_t **subgroups = NULL;  /* Array containing index arrays to parent Grp */
  smf_subinst_t subinst;      /* Subinst of current file */

  if ( *status != SAI__OK ) return;

  if( maxlen_s < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": maxlen_s cannot be < 0!", status );
    return;
  }

  /* Get downsampling parameters */

  if( keymap ) {
    smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                      NULL, NULL, NULL, NULL, &downsampscale, &downsampfreq,
                      NULL, NULL, NULL, NULL, NULL, status );

    if( downsampscale && downsampfreq ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": both downsampscale and downsampfreq are set",
              status );
      return;
    }
  }

  /* Initialize refcwave */
  refsubinst = SMF__SUBINST_NONE;

  /* Loop over files in input Grp: remember Grps are indexed from 1 */
  pol2 = 0;
  grouped = astKeyMap( "SortBy=KeyUp" );
  for (i=1; i<=grpsize; i++) {
    char newkey[128];
    char dateobs[81];
    char subarray[10];
    size_t nrelated = 0;
    AstKeyMap * filemap = NULL;
    AstKeyMap * indexmap = NULL;

    /* First step: open file and harvest metadata */
    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );

/* If this file has been rejected, annull the error, store a blank value
   in place of the file name in the Grp and proceed to process other inputs. */
    if( *status == SMF__REJECT ) {
       errAnnul( status );
       grpPut1( igrp, " ", i, status );
       continue;
    } else if (*status != SAI__OK) {
       break;
    }

    if( i==1 ) {
      isFFT = smf_isfft( data, NULL, NULL, NULL, NULL, NULL, status );
    } else if( smf_isfft(data, NULL, NULL, NULL, NULL, NULL, status) != isFFT ){
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": mixture of time-series and FFT data encountered!",
              status );
      break;
    }

    /* If maxlen has not been set, do it here */
    if( !maxlen && maxlen_s && data->hdr->steptime) {
      maxlen = (dim_t) (maxlen_s / data->hdr->steptime );
    }

    /* Check for pol2 data . */
    if( !strcmp( data->hdr->dlabel, "Q" ) || !strcmp( data->hdr->dlabel, "U" )
        || !strcmp( data->hdr->dlabel, "I" ) ) pol2 = 1;

    /* Return srate_maxlen if requested: may want to know this number
       even if maxlen_s is not set. Only calculate once, although it
       gets overwritten once later if down-sampling. */

    if( (i==1) && srate_maxlen && data->hdr->steptime ) {
      *srate_maxlen = 1. / (double) data->hdr->steptime;
    }


    /* If requested check to see if we are mixing wavelengths */
    if( checksubinst ) {
      if( refsubinst == SMF__SUBINST_NONE ) {
        refsubinst = smf_calc_subinst( data->hdr, status );
      }

      subinst = smf_calc_subinst( data->hdr, status );

      if( subinst != refsubinst ) {
        const char *refsubstr = smf_subinst_str( refsubinst, status );
        const char *substr = smf_subinst_str( subinst, status );

        *status = SAI__ERROR;
        smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
        msgSetc( "REFSUB", refsubstr );
        msgSetc( "SUB", substr );
        errRep( "", FUNC_NAME
                ": ^FILE uses sub-instrument ^SUB which doesn't match "
                "reference ^REFSUB", status );
      }
    }

    /* Want to form a key that will be unique for a particular subscan
       We know that DATE-OBS will be set for SCUBA-2 files and be the same
       for a single set. Prefix by wavelength if we are grouping by wavelength.
     */
    newkey[0] = '\0';

    smf_find_subarray( data->hdr, subarray, sizeof(subarray), NULL, status );

    if( grouping == 1 ) {
      /* Group different wavelengths separately */
      smf_fits_getS( data->hdr, "WAVELEN", cwave, sizeof(cwave), status);
      one_strlcat( newkey, cwave, sizeof(newkey), status );
      one_strlcat( newkey, "_", sizeof(newkey), status );
    }

    if( grouping == 2 ) {
      /* Group different subarrays separately */
      one_strlcat( newkey, subarray, sizeof(newkey), status );
    }

    smf_fits_getS( data->hdr, "DATE-OBS", dateobs, sizeof(dateobs), status );
    one_strlcat( newkey, dateobs, sizeof(newkey), status );

    /* Include the dimentionality of the time series in the primary key
       so that we do not end up doing something confusing like relating
       a truncated file with a full length file */
    if (*status == SAI__OK) {
      dim_t dims[3];
      char formatted[32];
      smf_get_dims( data, &dims[0], &dims[1], NULL, &dims[2], NULL, NULL, NULL,
                    status );
      sprintf(formatted, "_%" DIM_T_FMT "_%" DIM_T_FMT "_%" DIM_T_FMT, dims[0], dims[1], dims[2]);
      one_strlcat( newkey, formatted, sizeof(newkey), status );
    }

    /* May want to read the dimensionality of the file outside of loop
       so that we can compare values when storing in the keymap */

    /* Now we want to create a keymap based on this key */
    if (!astMapGet0A( grouped, newkey, &filemap ) ) {
      int itemp = 0;
      double steptime = data->hdr->steptime;
      dim_t ntslice = 0;
      dim_t thispad;              /* Padding neeed for current input file */

      filemap = astKeyMap( " " );
      astMapPut0A( grouped, newkey, filemap, NULL );

      /* Fill up filemap with general information on this file */
      smf_find_seqcount( data->hdr, &itemp, status );
      astMapPut0I( filemap, "SEQCOUNT", itemp, NULL );

      if( *status == SAI__OK ) {
         smf_fits_getI( data->hdr, "NSUBSCAN", &itemp, status );
         if( *status != SAI__OK ) { /* No NSCUBSCAN available if input is */
            errAnnul( status );     /* concatenation of multiple subscans. */
            itemp = VAL__BADI;
         }
      }
      astMapPut0I( filemap, "NSUBSCAN", itemp, NULL );

      /* Number of time slices */
      smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL,
                    status );

      /* Find length of down-sampled data, new steptime and maxlen */
      if( (downsampscale || downsampfreq) && data->hdr && (*status==SAI__OK) ) {
        double scalelen;

        if( downsampscale ) {

          /* If no SCAN_VEL value was read from the FITS header,
             calculate a scan velocity from the pointing information. */
          if( data->hdr->scanvel == VAL__BADD ) {
             size_t nflagged;
             smf_flag_slewspeed( data, 0.0, 0.0, &nflagged,
                                 &data->hdr->scanvel, status );
             if( data->hdr->scanvel != VAL__BADD ) {
                msgOutf( "", FUNC_NAME ": adopting mean SCANVEL=%g arcsec/sec",
                          status, data->hdr->scanvel );
             }
          }

          if( data->hdr->scanvel != VAL__BADD ) {
             double oldscale = steptime * data->hdr->scanvel;
             scalelen = oldscale / downsampscale;
          } else if( *status == SAI__OK ) {
             *status = SAI__ERROR;
            scalelen = VAL__BADD;
            smf_smfFile_msg( data->file, "FILE", 1, "" );
            errRep( "", FUNC_NAME ": can't resample ^FILE because it has "
                    "unknown scan velocity", status );
          }
        } else {
          if( steptime ) {
            double oldsampfreq = 1./steptime;
            scalelen = downsampfreq / oldsampfreq;
          } else {
            *status = SAI__ERROR;
            scalelen = VAL__BADD;
            smf_smfFile_msg( data->file, "FILE", 1, "" );
            errRep( "", FUNC_NAME ": can't resample ^FILE because it has "
                    "unknown sample rate", status );
          }
        }

        /* only down-sample if it will be a reasonable factor */
        if( (*status==SAI__OK) && (scalelen <= SMF__DOWNSAMPLIMIT) ) {
          smf_smfFile_msg(data->file, "FILE", 1, "" );
          msgOutiff( MSG__VERB, "", FUNC_NAME
                     ": will down-sample file ^FILE from %5.1lf Hz to "
                     "%5.1lf Hz", status, (1./steptime), (scalelen/steptime) );

          ntslice = round(ntslice * scalelen);

          /* If maxlen has been requested, and we have not already worked
             out a scaled version (just uses the sample rates for the first
             file... should be close enough -- the alternative is a 2-pass
             system). */

          if( !maxlen_scaled ) {
            maxlen = round(maxlen*scalelen);
            maxlen_scaled = 1;
            msgOutiff( MSG__VERB, "", FUNC_NAME
                       ": requested maxlen %g seconds = %" DIM_T_FMT " down-sampled "
                       "time-slices", status, maxlen_s, maxlen );

            /* Return updated srate_maxlen for down-sampling if requested */
            if( srate_maxlen ) {
              *srate_maxlen = scalelen/steptime;
            }
          }
        }
      }

      /* Check that an individual file is too long (we assume related
         files are all the same) */
      if( maxlen && (ntslice > maxlen) && *status == SAI__OK) {
        *status = SAI__ERROR;
        msgSeti("NTSLICE",ntslice);
        msgSeti("MAXLEN",maxlen);
        smf_smfFile_msg( data->file, "FILE", 1, "" );
        errRep(FUNC_NAME,
               "Number of time steps in file ^FILE time exceeds maximum "
               "(^NTSLICE>^MAXLEN)", status);
      }

      /* Scaled values of ntslice and maximum length */
      astMapPut0I( filemap, "NTSLICE", ntslice, NULL );

      /* Work out the padding needed for this file including downsampling. */
      if( keymap ) {
        thispad = smf_get_padding( keymap, 0, data->hdr, VAL__BADD, status );
        if( thispad > maxpad ) maxpad = thispad;
      } else {
        thispad = 0;
      }
      astMapPut0I( filemap, "PADDING", thispad, NULL );

      /* Update maxflen */
      if( ntslice > maxflen ) {
        maxflen = ntslice;
      }

      /* Store OBSID or OBSIDSS depending on whether we are grouping by wavelength */
      if (grouping) {
        astMapPut0C( filemap, "OBSID", data->hdr->obsidss, NULL );
      } else {
        char obsid[81];
        smf_getobsidss( data->hdr->fitshdr, obsid, sizeof(obsid), NULL, 0, status );
        astMapPut0C( filemap, "OBSID", obsid, NULL );
      }
    }

    /* Store the file index in another keymap indexed by subarray */
    if ( !astMapGet0A( filemap, "GRPINDICES", &indexmap ) ) {
      indexmap = astKeyMap( "SortBy=KeyUp" );
      astMapPut0A( filemap, "GRPINDICES", indexmap, NULL );
    }

    astMapPut0I( indexmap, subarray, i, NULL );

    /* Need to track the largest number of related subarrays in a single slot */
    nrelated = astMapSize( indexmap );
    if (nrelated > maxrelated) maxrelated = nrelated;

    /* Free resources */
    filemap = astAnnul( filemap );
    indexmap = astAnnul( indexmap );
    smf_close_file( NULL, &data, status );
  }

  /* We now know how many groups there are */
  ngroups = astMapSize( grouped );

  /* Sort out chunking. The items are sorted by date and then by wavelength.
     We define a continuous chunk if it has the same OBSID, the same SEQCOUNT
     and NSUBSCAN increments by one from the previous entry.

     Also count number of related items in each slot.
   */
  if (*status == SAI__OK) {
    typedef struct { /* somewhere to store the values easily */
      char obsid[81];
      char related[81];  /* for concatenated subarrays */
      int nsubscan;
      int seqcount;
    } smfCompareSeq;
    smfCompareSeq current;
    smfCompareSeq previous;
    dim_t totlen = 0;
    size_t thischunk;

    /* Get the chunk flags and also store the size of the chunk */
    chunk = astCalloc( ngroups, sizeof(*chunk) );
    chunklen = astCalloc( ngroups, sizeof(*chunklen) );
    piecelen = astCalloc( ngroups, sizeof(*piecelen) );

    thischunk = 0;  /* The current chunk */
    for (i=0; i<ngroups; i++) {
      AstKeyMap * thismap = NULL;
      AstKeyMap * grpindices = NULL;
      const char * tempstr = NULL;
      int thistlen = 0;
      size_t nsubarrays = 0;

      /* Get the keymap entry for this slot */
      astMapGet0A( grouped, astMapKey(grouped, i), &thismap );

      /* Get info for length limits */
      astMapGet0I( thismap, "NTSLICE", &thistlen );
      piecelen[i] = thistlen;

      if (isFFT) {
        /* Never concatenate FFT data */
        thismap = astAnnul(thismap);
        chunk[i] = i;
        chunklen[i] = thistlen;
        continue;
      }

      /* Get indices information and retrieve the sub-instrument names
         in sort order to concatenate for comparison. We only store in
         a continuous chunk if we have the same subarrays for the whole
         chunk. */
      astMapGet0A( thismap, "GRPINDICES", &grpindices );
      nsubarrays = astMapSize( grpindices );
      (current.related)[0] = '\0';
      for (j = 0; j < nsubarrays; j++ ) {
        one_strlcat( current.related, astMapKey(grpindices, j), sizeof(current.related), status );
      }
      grpindices = astAnnul( grpindices );

      /* Fill in the current struct */
      astMapGet0I( thismap, "SEQCOUNT", &(current.seqcount) );
      astMapGet0I( thismap, "NSUBSCAN", &(current.nsubscan) );
      astMapGet0C( thismap, "OBSID", &tempstr );
      one_strlcpy( current.obsid, tempstr, sizeof(current.obsid), status );

      /* First chunk is special, else compare */
      if (i == 0) {
        totlen = thistlen;
      } else {
        if (  ( current.seqcount == previous.seqcount  ) &&
              ( current.nsubscan - previous.nsubscan == 1 ) &&
              ( strcmp( current.obsid, previous.obsid ) == 0 ) &&
              ( strcmp( current.related, previous.related ) == 0 ) ) {
          /* continuous - check length */
          totlen += thistlen;
          if ( maxlen && totlen > maxlen ) {
            thischunk++;
            totlen = thistlen; /* reset length */
          } else {
            /* Continuous */
          }
        } else {
          /* discontinuity */
          thischunk++;
          totlen = thistlen;  /* Update length of current chunk */
        }
      }

      chunklen[thischunk] = totlen;
      chunk[i] = thischunk;
      memcpy( &previous, &current, sizeof(current) );

      thismap = astAnnul( thismap );
    }
  }

  /* Decide if we are keeping a chunk by looking at the length. "Stare and
     spin" POl2 data has very short chunks, so relax the check for this. */
  chunkminlen = pol2 ? 10 : SMF__MINCHUNKSAMP;

  maxconcat = 0;
  nkeep = 0;
  keepchunk = astMalloc( ngroups*sizeof(*keepchunk) );
  for (i=0; i<ngroups; i++) {
    size_t thischunk;

    thischunk = chunk[i];
    if ( chunklen[thischunk] < chunkminlen ) {
      /* Warning message */
      msgSeti("LEN",chunklen[thischunk]);
      msgSeti("MIN", chunkminlen );
      msgOut( " ", "SMF_GRP_RELATED: ignoring short chunk (^LEN<^MIN)",
              status);
      keepchunk[i] = 0;
    } else {
      keepchunk[i] = 1;
      if (maxconcat < chunklen[thischunk]) maxconcat = chunklen[thischunk];
      nkeep++;
    }

  }

  /* If no useful chunks generate an error */
  if( (*status==SAI__OK) && (!nkeep) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No useful chunks.", status );
    goto CLEANUP;
  }

  /* Allocate a subgroup array of the right size and fill it. They keymap
     is sorted by date (and wavelength) so we can always index into it by using
     indices from the subgroup. */
  subgroups = astCalloc( nkeep, sizeof(*subgroups) );
  new_chunk = astCalloc( nkeep, sizeof(*new_chunk) );
  new_tlen  = astCalloc( nkeep, sizeof(*new_tlen) );

  currentindex = 0;
  for (i=0;i<ngroups;i++) {
    AstKeyMap * thismap = NULL;
    AstKeyMap * grpindices = NULL;
    size_t nsubarrays = 0;
    size_t *indices = astCalloc( maxrelated, sizeof(*indices) );

    /* skip if we are dropping this chunk */
    if (!keepchunk[i]) continue;

    /* Get the keymap entry for this slot */
    astMapGet0A( grouped, astMapKey(grouped, i), &thismap );

    /* Get the indices keymap */
    astMapGet0A( thismap, "GRPINDICES", &grpindices );
    nsubarrays = astMapSize( grpindices );
    for (j=0; j<nsubarrays; j++) {
      int myindex;
      astMapGet0I( grpindices, astMapKey(grpindices, j), &myindex );
      indices[j] = myindex;
    }
    grpindices = astAnnul( grpindices );
    thismap = astAnnul( thismap );

    subgroups[currentindex] = indices;
    new_chunk[currentindex] = chunk[i];
    new_tlen[currentindex]  = piecelen[i];
    currentindex++;

  }

  /* Create the smfGroup */
  *group = smf_construct_smfGroup( igrp, subgroups, new_chunk, new_tlen,
                                   nkeep, maxrelated, 0, status );

  /* Return maxfilelen if requested */
  if( maxfilelen ) {
    *maxfilelen = maxflen;
  }

  /* Return maxconcatlen if requested */
  if( maxconcatlen ) {
    *maxconcatlen = maxconcat;
  }

  /* Create a base group for output files if required */
  /* Create a base group of filenames */
  if (*status == SAI__OK && basegrp ) {
    *basegrp = smf_grp_new( (*group)->grp, "Base Group", status );

    /* Loop over time chunks */
    for( i=0; (*status==SAI__OK)&&(i<(*group)->ngroups); i++ ) {
      size_t idx;
      /* Check for new continuous chunk */
      if( i==0 || ( (*group)->chunk[i] != (*group)->chunk[i-1]) ) {
        /* Loop over subarray */
        for( idx=0; idx<(*group)->nrelated; idx++ ) {
          size_t grpindex = (*group)->subgroups[i][idx];
          if ( grpindex > 0 ) {
            ndgCpsup( (*group)->grp, grpindex, *basegrp, status );
          }
        }
      }
    }
  }

 CLEANUP:
  keepchunk = astFree( keepchunk );
  chunk = astFree( chunk );
  chunklen = astFree( chunklen );
  piecelen = astFree( piecelen );
  grouped = astAnnul( grouped );

  if( *status != SAI__OK ) {
    /* free the group */
    if (basegrp && *basegrp) grpDelet( basegrp, status );
    if (group && *group) {
      smf_close_smfGroup( group, status );
    } else {
      /* have to clean up manually */
      new_chunk = astFree( new_chunk );
      new_tlen = astFree( new_tlen );
      if( subgroups ) {
        size_t isub;
        for( isub=0; isub<nkeep; isub++ ) {
          subgroups[isub] = astFree( subgroups[isub] );
        }
        subgroups = astFree( subgroups );
      }
    }
  }

  /* Return the maximum padding if required. */
  if( pad ) *pad = maxpad;
}
