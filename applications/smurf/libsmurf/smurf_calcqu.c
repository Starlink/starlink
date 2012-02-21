/*
*+
*  Name:
*     CALCQU

*  Purpose:
*     Calculate Q and U images from a set of time-series data files.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_calcqu( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This application creates a set of 2-dimensional Q and U images from
*     a set of SCUBA-2 fast-spinning polarimeter time series data files.
*     Each Q or U image is created from a block of time slices over which
*     the sky position of each bolometer does not change significantly (see
*     parameter ARCERROR). The resulting set of Q images can be combined
*     subsequently using KAPPA:WCSMOSAIC, to form a single Q image
*     (normally, the "I" image should be used as the reference image when
*     running WCSMOSIAC). Likewise, the set of U images can be combined in
*     the same way. All the created Q and U images use celestial north in
*     the telescope tracking system as the reference direction.
*
*     The supplied time series data files are first flat-fielded, cleaned
*     and concatenated, before being used to create the Q and U images. The
*     Q and U values are determined from the Fourier component of each time
*     series corresponding to the frequency of the spinning half-waveplate
*     (6 - 12 Hz), and should be largely unaffected by signal at other
*     frequencies. For this reason, the cleaning specified by parameter
*     CONFIG should usually not include any filtering.

*  ADAM Parameters:
*     ARCERROR = _REAL (Read)
*        The maximum spatial drift allowed within a single Q or U image,
*        in arc-seconds. The default value is wavelength dependant, and
*        is equal to half the default pixel size used by smurf:makemap. []
*     CONFIG = GROUP (Read)
*        Specifies values for various configuration parameters. If the
*        string "def" (case-insensitive) or a null (!) value is supplied,
*        a set of default configuration parameter values will be used.
*
*        The supplied value should be either a comma-separated list of
*        strings or the name of a text file preceded by an up-arrow
*        character "^", containing one or more comma-separated lists of
*        strings. Each string is either a "keyword=value" setting, or
*        the name of a text file preceded by an up-arrow character "^".
*        Such text files should contain further comma-separated lists
*        which will be read and interpreted in the same manner (any blank
*        lines or lines beginning with "#" are ignored). Within a text
*        file, newlines can be used as delimiters, as well as commas.
*        Settings are applied in the order in which they occur within the
*        list, with later settings over-riding any earlier settings given
*        for the same keyword.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*
*        The available parameters include the cleaning parameters used
*        by the SC2CLEAN and MAKEMAP commands, plus additional parameter
*        related to the calculation of Q and U. Further information
*        about all these parameters is available in the "Configuration
*        Parameters" section below, and can be found, with defaults, in
*        $SMURF_DIR/smurf_calcqu.def. Default values will be used for
*        any unspecified parameters. Assigning the value "<def>" (case
*        insensitive) to a keyword has the effect of resetting it to its
*        default value. Parameters not understood will trigger an error.
*        Use the "cleandk." namespace for configuring cleaning parameters
*        for the dark squids. [current value]
*     IN = NDF (Read)
*        Input file(s).
*     MSG_FILTER = _CHAR (Read)
*        Control the verbosity of the application. Values can be
*        NONE (no messages), QUIET (minimal messages), NORMAL,
*        VERBOSE, DEBUG or ALL. [NORMAL]
*     OUTQ = LITERAL (Write)
*        The name of an output HDS container file containing the Q images.
*        Each image is held in a separate 2D NDF within the container file.
*        At their simplest the NDF names will be "Q<i>", where "<i>" is the
*        integer one-based index of the time slice block from which the
*        image was made. However, if the supplied input NDFs contain data
*        from more than one subarray, these names will be modified to
*        "Q<i>_<s>", where "<s>" is the name of the subarray (e.g. "s4a",
*        etc). Further, if the supplied input NDFs do not form a contiguous
*        sequence of time slices but are instead broken up into a set of
*        non-contiguous chunks (for instance if the input NDFs contained
*        data from more than one observation), the names will be modified
*        to "Q<i>_<s>_<c>", where "<c>" is an integer one-based chunk index.
*     OUTU = LITERAL (Write)
*        The name of an output HDS container file containing the U images.
*        The NDFs within this container file are stored and named in the
*        same way as those in the "OUTQ" container file, but using "U"
*        insead of "Q" in the NDF names.

*  Configuration Parameters:
*     APOD = INTEGER
*        Apodize signals (smoothly roll-off) using sine/cosine functions at
*        start and end of the signal across this many samples. The supplied
*        APOD value is ignored and a value of zero is used if ZEROPAD is set to 0.
*     BADFRAC = REAL
*        Flag entire bolometer as dead if at least this fraction of the samples
*        in a detector time series were flagged as bad by the DA system.
*     DCFITBOX = REAL
*        Number of samples (box size) used on either side of a DC step to
*        estimate the height of the step. If negative, it is taken as a
*        duration in seconds, which is converted to a number of samples
*        using the data sample rate.
*     DCMAXSTEPS = INTEGER
*        The maximum number of steps that can be corrected in each minute of
*        good data (i.e. per 12000 samples) from a bolometer before the entire
*        bolometer is flagged as bad. A value of zero will cause a bolometer to
*        be rejected if any steps are found in the bolometer data stream.
*     DCSMOOTH = INTEGER
*        The number of samples in the median filter used to smooth a bolometer
*        data stream prior to finding DC steps. If negative, it is taken as a
*        duration in seconds, which is converted to a number of samples
*        using the data sample rate.
*     DCTHRESH = REAL
*        Threshold S/N to detect and flag DC (baseline) steps.
*     DKCLEAN = LOGICAL
*        Clean the bolometers using the dark squids. Defaults to false.
*     FILLGAPS = LOGICAL
*        Fill vicinity of spikes / DC steps with constrained realization of
*        noise. Also (unless ZEROPAD is 1), fill the padded region at the start
*        and end of each time stream with artificial data. You almost always
*        want to do this.
*     FILT_EDGEHIGH = REAL
*        Hard-edge high-pass frequency-domain filter (Hz).
*     FILT_EDGELOW = REAL
*        Hard-edge low-pass frequency-domain filter (Hz).
*     FILT_NOTCHHIGH( ) = REAL
*        Hard-edge band-cut frequency-domain notch filters. FILT_NOTCHHIGH is
*        an array of upper-edge frequencies (Hz).
*     FILT_NOTCHLOW( ) = REAL
*        Array of lower-edge frequencies corresponding to FILT_NOTCHHIGH.
*     FLAGSTAT = REAL
*        Flag data taken while the telescope was stationary so that it
*        they are ignored in the final map. The value given is a threshold
*        slew velocity (arcsec/sec) measured in tracking coordinates
*        below which the telescope is considered to be stationary.
*     ORDER = INTEGER
*        Subtract a fitted baseline polynomial of this order (0 to remove mean).
*     SPIKEBOX = INTEGER
*        The size of the filter box for the sigma-clipper. If negative, it is
*        taken as a duration in seconds, which is converted to a number of
*        samples using the data sample rate.
*     SPIKETHRESH = REAL
*        Threshold S/N to flag spikes using sigma-clipper.
*     ZEROPAD = LOGICAL
*        Determines the nature of the padding added to the start and end of
*        each bolometer time stream. Padding is needed to avoid
*        interaction between the data values at the start and end of each
*        bolometer time stream, due to the cyclic wrap-around nature of the
*        FFTs used to filter each time stream. If ZEROPAD is set to 1, the
*        padded sections will be filled with zeros. This requires that the
*        data also be apodised (see APOD) to avoid ringing due the sudden
*        steps down to zero at the start and end of each time stream). If
*        ZEROPAD is set to 0, then the padded regions will be filled with
*        artificial data that links the data values at the start and end of
*        the time stream smoothly. In this case, no apodisation is
*        performed, and the value of the APOD parameter is ignored. The
*        default for ZEROPAD is 0 (i.e. pad with artificial data rather
*        than zeros) unless FILLGAPS is zero, in which case the supplied
*        ZEROPAD value is ignored and a value of 1 is always used.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 20011 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "msg_par.h"
#include "ndf.h"
#include "par.h"
#include "par_err.h"
#include "ast.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/ndg.h"
#include "star/hdspar.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "smurf_par.h"
#include "smurflib.h"

void smurf_calcqu( int *status ) {

/* Local Variables: */
   AstFitsChan *fc;           /* Holds FITS headers for output NDFs */
   AstKeyMap *config;         /* Holds all cleaning parameters */
   AstKeyMap *dkpars;         /* Holds dark squid cleaning parameters */
   AstKeyMap *heateffmap = NULL; /* Heater efficiency data */
   AstKeyMap *sub_instruments;/* Indicates which instrument is being used */
   Grp *bgrp = NULL;          /* Group of base names for each chunk */
   Grp *igrp = NULL;          /* Group of input files */
   Grp *ogrp = NULL;          /* Group of output files  */
   Grp *sgrp = NULL;          /* Group of science files */
   HDSLoc *locq = NULL;       /* Locator for output Q container file */
   HDSLoc *locu = NULL;       /* Locator for output U container file */
   NdgProvenance *oprov;      /* Provenance to store in each output NDF */
   char headval[ 81 ];        /* FITS header value */
   char ndfname[ 30 ];        /* Name of output Q or U NDF */
   char polcrd[ 81 ];         /* FITS 'POL_CRD' header value */
   char subarray[ 10 ];       /* Subarray name (e.g. "s4a", etc) */
   float arcerror;            /* Max acceptable error (arcsec) in one block */
   int block_end;             /* Index of last time slice in block */
   int block_start;           /* Index of first time slice in block */
   int dkclean;               /* Clean dark squids? */
   int iblock;                /* Index of current block */
   int ipolcrd;               /* Reference direction for waveplate angles */
   int nc;                    /* Number of characters written to a string */
   int qplace;                /* NDF placeholder for current block's Q image */
   int uplace;                /* NDF placeholder for current block's U image */
   size_t ichunk;             /* Continuous chunk counter */
   size_t idx;                /* Subarray counter */
   size_t igroup;             /* Index for group of related input NDFs */
   size_t inidx;              /* Index into group of science input NDFs */
   size_t nchunk;             /* Number continuous chunks outside iter loop */
   size_t ssize;              /* Number of science files in input group */
   smfArray *concat = NULL;   /* Pointer to smfArray holding bolometer data */
   smfArray *darks = NULL;    /* dark frames */
   smfArray *dkarray = NULL;  /* Pointer to smfArray holding dark squid data */
   smfArray *flatramps = NULL;/* Flatfield ramps */
   smfData *data = NULL;      /* Concatenated data for one subarray */
   smfData *dkdata = NULL;    /* Concatenated dark squid data for one subarray */
   smfGroup *sgroup = NULL;   /* smfGroup corresponding to sgrp */
   ThrWorkForce *wf;          /* Pointer to a pool of worker threads */

/* Check inhereited status */
   if( *status != SAI__OK ) return;

/* Start new AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Find the number of cores/processors available and create a work force
   holding the same number of threads. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get a group of input files */
   kpg1Rgndf( "IN", 0, 1, "  Give more NDFs...", &igrp, &ssize, status );

/* Get a group containing just the files holding science data. */
   smf_find_science( igrp, &sgrp, 0, NULL, NULL, 1, 1, SMF__NULL, &darks,
                     &flatramps, &heateffmap, NULL, status );

/* Check we have at least once science file. */
   ssize = grpGrpsz( sgrp, status );
   if( ssize == 0 ) {
      msgOutif( MSG__NORM, " ", "All supplied input frames were DARK.",
                status );
   } else {

/* Create HDS container files to hold the output NDFs. */
      datCreat( "OUTQ", "CALCQU", 0, 0, status );
      datCreat( "OUTU", "CALCQU", 0, 0, status );

/* Associate the locators with the structures. */
      datAssoc( "OUTQ", "WRITE", &locq, status );
      datAssoc( "OUTU", "WRITE", &locu, status );

/* Group the input files so that all files within a single group have the
   same wavelength and belong to the same subscan of the same observation.
   Also identify chunks of data that are contiguous in time, and
   determine to which such chunk each group belongs. All this information
   is returned in a smfGroup structure ("*sgroup"). */
      smf_grp_related( sgrp, ssize, 1, 1, 0, NULL, NULL, NULL,
                       NULL, &sgroup, &bgrp, NULL, status );

/* Obtain the number of contiguous chunks. */
      if( *status == SAI__OK ) {
         nchunk = sgroup->chunk[ sgroup->ngroups - 1 ] + 1;
      } else {
         nchunk = 0;
      }

/* Indicate we have not yet found a value for the ARCERROR parameter. */
      arcerror = 0.0;

/* Loop over all contiguous chunks */
      for( ichunk = 0; ichunk < nchunk && *status == SAI__OK; ichunk++ ) {

/* Display the chunk number. */
            if( nchunk > 1 ) {
               msgOutiff( MSG__VERB, "", "   Doing chunk %d of %d.",
                          status, (int) ichunk + 1, (int) nchunk );
            }

/* Concatenate the data within this contiguous chunk. This produces a
   smfArray ("concat") containing a smfData for each subarray present in
   the chunk. Each smfData holds the concatenated data for a single
   subarray. */
         smf_concat_smfGroup( wf, NULL, sgroup, darks, NULL, flatramps,
                              heateffmap, ichunk, 1, 1, NULL, 0, NULL, NULL,
                              0, 0, 0, &concat, NULL, status );

/* Get a KeyMap holding values for the configuration parameters. Since we
   sorted by wavelength when calling smf_grp_related, we know that all
   smfDatas in the current smfArray (i.e. chunk) will relate to the same
   wavelength. Therefore we can use the same parameters for all smfDatas in
   the current smfArray. */
         sub_instruments = smf_subinst_keymap( SMF__SUBINST_NONE,
                                               concat->sdata[ 0 ], NULL,
                                               0, status );
         config = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_calcqu.def",
                               sub_instruments, status );
         sub_instruments = astAnnul( sub_instruments );

/* See if the dark squids should be cleaned. */
         if( !astMapGet0I( config, "DKCLEAN", &dkclean ) ) dkclean = 0;

/* If required, clean the dark squids now since we might need to use them to
   clean the bolometer data. */
         if( dkclean ) {

/* Create a smfArray containing the dark squid data. For each one, store
   a pointer to the main header so that smf_clean_smfArray can get at the
   JCMTState information. */
            dkarray = smf_create_smfArray( status );
            for( idx = 0; idx < concat->ndat && *status == SAI__OK; idx++ ) {
               data = concat->sdata[ idx ];
               if( data && data->da && data->da->dksquid ) {
                  dkdata = data->da->dksquid;
                  dkdata->hdr = data->hdr;
                  smf_addto_smfArray( dkarray, dkdata, status );
               }
            }

/* Clean the smfArray containing the dark squid data. Use the "CLEANDK.*"
   parameters. */
            (void) astMapGet0A( config, "CLEANDK", &dkpars );
            smf_clean_smfArray( wf, dkarray, NULL, NULL, NULL, dkpars, status );
            dkpars = astAnnul( dkpars );

/* Nullify the header pointers so that we don't accidentally close any. */
            if( dkarray ) {
               for( idx = 0; idx < dkarray->ndat; idx++ ) {
                  dkdata = dkarray->sdata[ idx ];
                  dkdata->hdr = NULL;
               }

/* Free the smfArray holding the dark squid data, but do not free the
   individual smfDatas within it. */
               dkarray->owndata = 0;
               smf_close_related( &dkarray, status );
            }
         }

/* Now clean the bolometer data */
         smf_clean_smfArray( wf, concat, NULL, NULL, NULL, config, status );

/* Loop round each sub-array in the current contiguous chunk of data. */
         for( idx = 0; idx < concat->ndat && *status == SAI__OK; idx++ ) {
            data = concat->sdata[ idx ];

/* Find the name of the subarray that generated the data. */
            smf_find_subarray( data->hdr, subarray, sizeof(subarray), NULL,
                               status );

/* Display the sub-array. */
            if( concat->ndat > 1 ) {
               msgOutiff( MSG__VERB, "", "   Doing sub-array %s.",
                          status, subarray );
            }

/* Create an empty provenance structure. Each input NDF that contributes
   to the current chunk and array will be added as an ancestor to this
   structure, which will later be stored in each output NDF created for
   this chunk and array. */
            oprov = ndgReadProv( NDF__NOID, "SMURF:CALCQU", status );

/* Indicate we do not yet have any FITS headers for the output NDFs */
            fc = NULL;

/* Indicate we do not yet know the coordinate reference frame for the
   half-waveplate angle. */
            polcrd[ 0 ] = 0;
            ipolcrd = 0;

/* Go through the smfGroup looking for groups of related input NDFs that
   contribute to the current chunk. */
            for( igroup = 0; igroup < sgroup->ngroups; igroup++ ) {
               if( sgroup->chunk[ igroup ] == ichunk ) {

/* Get the integer index into the GRP group (sgrp) that holds the input NDFs.
   This index identifies the input NDF that provides the data for the current
   chunk and subarray. This assumes that the order in which smf_concat_smfGroup
   stores arrays in the "concat" smfArray matches the order in which
   smf_grp_related stores arrays within the sgroup->subgroups. */
                  inidx = sgroup->subgroups[ igroup ][ idx ];

/* Add this input NDF as an ancestor into the output provenance structure. */
                  smf_accumulate_prov( NULL, sgrp, inidx, NDF__NOID,
                                       "SMURF:CALCQU", &oprov, status );

/* Merge the FITS headers from the current input NDF into the FitsChan
   that holds headers for the output NDFs. The merging retains only those
   headers which have the same value in all input NDFs. */
                  smf_fits_outhdr( data->hdr->fitshdr, &fc, status );

/* Get the polarimetry related FITS headers and check that all input NDFs
   have usabie values. */
                  headval[ 0 ] = 0;
                  smf_getfitss( data->hdr, "POL_MODE", headval,
                                sizeof(headval), status );
                  if( strcmp( headval, "CONSTANT" ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     grpMsg( "N", sgrp, inidx );
                     errRep( " ", "Input NDF ^N does not contain "
                             "polarimetry data obtained with a spinning "
                             "half-waveplate.", status );
                  }

                  headval[ 0 ] = 0;
                  smf_getfitss( data->hdr, "POLWAVIN", headval,
                                sizeof(headval), status );
                  if( strcmp( headval, "Y" ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     grpMsg( "N", sgrp, inidx );
                     errRep( " ", "Half-waveplate was not in the beam for "
                             "input NDF ^N.", status );
                  }

                  headval[ 0 ] = 0;
                  smf_getfitss( data->hdr, "POLANLIN", headval,
                                sizeof(headval), status );
                  if( strcmp( headval, "Y" ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     grpMsg( "N", sgrp, inidx );
                     errRep( " ", "Analyser was not in the beam for input "
                             "NDF ^N.", status );
                  }

                  if( polcrd[ 0 ] ) {
                     headval[ 0 ] = 0;
                     smf_getfitss( data->hdr, "POL_CRD", headval,
                                   sizeof(headval), status );
                     if( strcmp( headval, polcrd ) && *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        errRep( " ", "Input NDFs have differing values for "
                                "FITS header 'POL_CRD'.", status );
                     }

                  } else {
                     smf_getfitss( data->hdr, "POL_CRD", polcrd,
                                   sizeof(polcrd), status );
                     if( !strcmp( polcrd, "FPLANE" ) ) {
                        ipolcrd = 0;
                     } else if( !strcmp( polcrd, "AZEL" ) ) {
                        ipolcrd = 1;
                     } else if( !strcmp( polcrd, "TRACKING" ) ) {
                        ipolcrd = 2;
                     } else if( *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        msgSetc( "N", data->file->name );
                        msgSetc( "V", polcrd );
                        errRep( " ", "Input NDF ^N contains unknown value "
                                "'^V' for FITS header 'POL_CRD'.", status );
                     }
                  }
               }
            }

/* If not already done, get the maximum spatial drift (in arc-seconds) that
   can be tolerated whilst creating a single Q/U image. The default value is
   half the makemap default pixel size. */
            if( arcerror == 0.0 ) {
               parDef0d( "ARCERROR", 0.5*smf_calc_telres( data->hdr->fitshdr,
                                                          status ), status );
               parGet0r( "ARCERROR", &arcerror, status );
            }

/* The algorithm that calculates Q and U assumes that all samples for a
   single bolometer measure flux from the same point on the sky. Due to
   sky rotation, this will not be the case - each bolometer will drift
   slowly across the sky. However, since the drift is (or should be)
   slow we can apply the Q/U algorithm to blocks of contiguous data over
   which the bolometers do not move significantly. We produce a separate
   Q and U image for each such block. The first block starts at the first
   time slice in the smfData. */
            block_start = 0;

/* Find the time slice at which the corner bolometers have moved
   a critical distance (given by parameter ARCERROR) from their
   positions at the start of the block. Then back off some time slices
   to ensure that the block holds an integral number of half-waveplate
   rotations. */
            block_end = smf_block_end( data, block_start, ipolcrd, arcerror,
                                       status );

/* Loop round creating a pair of Q/U images for each block. Count them. */
            iblock = 0;
            while( block_end >= 0 && *status == SAI__OK ) {

/* Skip very short blocks. */
               if( block_end - block_start > 4 ) {

/* Display the start and end of the block. */
                  msgOutiff( MSG__VERB, "", "   Doing time slice block %d "
                             "-> %d", status, (int) block_start,
                             (int) block_end );

/* Get the name for the Q NDF for this block. Start of with "Q" followed by
   the block index. */
                  iblock++;
                  nc = sprintf( ndfname, "Q%d", iblock );

/* If we are producing Q images for more than one subarray, append the
   subarray name to the NDF name. */
                  if( concat->ndat > 1 ) nc += sprintf( ndfname + nc, "_%s",
                                                        subarray );

/* If we are producing Q images for more than one contiguous chunk of
   data, append the chunk index to the NDF name. */
                  if( nchunk > 1 ) nc += sprintf( ndfname + nc, "_%d",
                                                  (int) ichunk );

/* Get NDF placeholder for the Q NDF. The NDFs are created inside the
   output container file. */
                  ndfPlace( locq, ndfname, &qplace, status );

/* The name of the U NDF is the same except the initial "Q" is changed to
   "U". */
                  ndfname[ 0 ] = 'U';
                  ndfPlace( locu, ndfname, &uplace, status );

/* Create the Q and U images for the current block of time slices from
   the subarray given by "idx", storing them in the output container
   file. */
                  smf_calc_qu( wf, data, block_start, block_end, ipolcrd,
                               qplace, uplace, oprov, fc, status );

/* The next block starts at the first time slice following the previous
   block. */
                  block_start = block_end + 1;
               }

/* Find the time slice at which the corner bolometers have moved
   a critical distance (given by parameter ARCERROR) from their
   positions at the start of the block. Then back off some time slices
   to ensure that the block holds an integral number of half-waveplate
   rotations. This returns -1 if all time slices have been used. */
               block_end = smf_block_end( data, block_start, ipolcrd,
                                          arcerror, status );
            }

/* Free resources */
            oprov = ndgFreeProv( oprov, status );
            fc = astAnnul( fc );
         }
         config = astAnnul( config );

/* Close the smfArray. */
         smf_close_related( &concat, status );
      }

/* Annul the locators for the output container files. */
      datAnnul( &locq, status );
      datAnnul( &locu, status );

/* The parameter system hangs onto a primary locator for each container
   file, so cancel the parameters to annul these locators. */
      datCancl( "OUTQ", status );
      datCancl( "OUTU", status );
   }

/* Free resources. */
   smf_close_related( &darks, status );
   smf_close_related( &flatramps, status );

   if( igrp ) grpDelet( &igrp, status);
   if( sgrp ) grpDelet( &sgrp, status);
   if( bgrp ) grpDelet( &bgrp, status );
   if( ogrp ) grpDelet( &ogrp, status );
   if( sgroup ) smf_close_smfGroup( &sgroup, status );
   if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );

/* End the NDF and AST contexts. */
   ndfEnd( status );
   astEnd;

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
     msgOutif( MSG__VERB, " ", "CALCQU succeeded.", status);
   } else {
     msgOutif( MSG__VERB, " ", "CALCQU failed.", status);
   }
}
