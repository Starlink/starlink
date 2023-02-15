/*
*+
*  Name:
*     smf_get_cleanpar

*  Purpose:
*     Obtain values of cleaning parameters from a keymap

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_get_cleanpar( AstKeyMap *keymap, const char *qualifier,
*                       const smfData *data, double *badfrac,
*                       int *dcfitbox, int *dcmaxsteps, double *dcthresh,
*                       int *dcsmooth, int *dclimcorr, int *dkclean,
*                       int *fillgaps, int *zeropad, double *filt_edgelow,
*                       double *filt_edgehigh, double *filt_edge_smallscale,
*                       double *filt_edge_largescale,
*                       double *filt_notchlow, double *filt_notchhigh,
*                       int *filt_nnotch, int *dofilt, double *flagslow,
*                       double *flagfast, int *order, double *spikethresh,
*                       dim_t *spikebox, double *noisecliphigh,
*                       double *noisecliplow, int *whiten, int *compreprocess,
*                       dim_t *pcalen, double *pcathresh, int groupsubarray,
*                       double *downsampscale, double *downsampfreq,
*                       int *noiseclipprecom, int *deconvmce, double delay,
*                       int *filt_order, dim_t *startup, int *status )

*  Arguments:
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters
*     qualifier = const char * (Given)
*        A string which is added to the end of the usual "base" configuration
*        parameter names. For instance, "_LAST" could be used in order to
*        use an alternate set of parameters names that are like the usual
*        parameter names but with "_LAST" appended to the end. If the
*        qualified parameter name has an <undef> value, then the unqualified
*        parameter name is used instead. Should be NULL to use the normal
*        base parameter names.
*     data = const smfData  * (Given)
*        Pointer to a smfData that is used to define the sample rate
*        needed when converting parameter values form seconds to time
*        slices. If NULL is supplied, a default sample rate of 200 Hz is
*        used.
*     badfrac = double* (Returned)
*        Fraction of bad samples in order for entire bolometer to be
*        flagged as bad (NULL:0)
*     dcfitbox = int * (Returned)
*        Length of box (in samples) over which each linear fit is
*        performed when detecting DC steps. If zero, no steps will be
*        corrected.
*     dcmaxsteps = int* (Returned)
*        The maximum number of DC jumps that can be corrected in each minute
*        of good data (i.e. per 12000 samples) from a bolometer before the
*        entire bolometer is flagged as bad. A value of zero will cause a
*        bolometer to be rejected if any jumps are found in the bolometer
*        data stream.
*     dcthresh = double* (Returned)
*        N-sigma threshold at which to detect DC steps
*     dcsmooth = int * (Returned)
*        Width of median filter for DC step detection.
*     dclimcorr = int * (Returned)
*        The detection threshold for steps that occur at the same time in
*        many bolometers. Set it to zero to suppress checks for correlated
*        steps. If dclimcorr is greater than zero, and a step is found at
*        the same time in more than "dclimcorr" bolometers, then all
*        bolometers are assumed to have a step at that time, and the step
*        is fixed no matter how small it is.
*     dkclean = int* (Returned)
*        If true, clean dark squids from bolos (NULL:-1)
*     fillgaps = int* (Returned)
*        If true, fill regions of data flagged with spikes, DC steps, bad
*        samples with constrained realization of noise.
*     zeropad = int* (Returned)
*        If true, each time stream is padded with zeros at start and end.
*        Otherwise, artificial data that retains continuity between start
*        and end of the data stream is used.
*     filt_edgelow = double* (Returned)
*        Apply a hard-edged low-pass filter at this frequency (Hz) (NULL:0)
*     filt_edgehigh = double* (Returned)
*        Apply a hard-edged high-pass filter at this frequency (Hz) (NULL:0)
*     filt_edge_smallscale* (Returned)
*        Apply low-pass filter that preserves this scale (arcsec) (NULL:0)
*     filt_edge_largescale* (Returned)
*        Apply high-pass filter that preserves this scale (arcsec) (NULL:0)
*     filt_notchlow = double* (Returned)
*        Array of lower-frequency edges for hard notch filters (Hz). The
*        maximum length of this array is SMF__MXNOTCH.
*     filt_notchhigh = double* (Returned)
*        Array of upper-frequency edges for hard notch filters (Hz). The
*        maximum length of this array is SMF__MXNOTCH.
*     filt_nnotch = int* (Returned)
*        Number of notches in notch filter (NULL:0)
*     dofilt = int* (Returned)
*        If true, frequency-domain filtering is required (NULL:0)
*     flagslow = double* (Returned)
*        Flag data during slew speeds less than FLAGSLOW (arcsec/sec) (NULL:0)
*     flagfast = double* (Returned)
*        Flag data during slew speeds greater than FLAGFAST (arcsec/sec)(NULL:0)
*     order = int* (Returned)
*        Fit and remove polynomial baselines of this order (NULL:-1)
*     spikethresh = double* (Returned)
*        Flag spikes SPIKETHRESH-sigma away from mean (NULL:0)
*     spikebox = dim_t* (Returned)
*        The size of the filter box used for flagging spikes.
*     noisecliphigh = double * (Returned)
*        Number of standard deviations to clip the upper bound
*        of a noise distribution in order to generate a bad bolometer
*        mask.
*     noisecliplow = double * (Returned)
*        Number of standard deviations to clip the lower bound
*        of a noise distribution in order to generate a bad bolometer
*        mask.
*     whiten = int * (Returned)
*        Apply a whitening filter to the data?
*     compreprocess = int * (Returned)
*        If set do common-mode rejection and bad-data rejection.
*     pcalen = dim_t * (Returned)
*        Chunk length for PCA cleaning in time slices (if 0 default to full
*        length of the data)
*     pcathresh = double * (Returned)
*        Outlier threshold for PCA amplitudes to remove from data for cleaning
*     groupsubarray = int * (Returned)
*        If set, handle subarrays separately instead of grouping data at same
*        wavelength that were taken simultaneously together.
*     downsampscale = double * (Returned)
*        Determine downsampfreq internally based on this angular scale (arcsec)
*        and the slew speed.
*     downsampfreq = double * (Returned)
*        Target down-sampling rate in Hz.
*     noiseclipprecom = int * (Returned)
*        Should clipping of noisy bolometers happen before common-mode cleaning
*        instead of after as is the default?
*     deconvmce = int * (Returned)
*        Should the effects of the anti-aliasing MCE filter be removed?
*     delay = double * (Returned)
*        A time, in seconds, by which to delay each time stream, causing
*        each sample to be associated with a different position on the sky.
*     filt_order = int * (Returned)
*        Order of Butterworth filter to use (0=hard-edged).
*     filt_order = int * (Returned)
*        Order of Butterworth filter to use (0=hard-edged).
*     startup = dim_t * (Returned)
*        The number of time slices to blank at the start of the
*        observation.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function obtains data cleaning values from a keymap and stores
*     them in the supplied variables. The assumed name for each variable in
*     the keymap is the same as the name of the variable. If a NULL pointer
*     is supplied for a given variable, there is no search for its value in
*     the keymap. If a given value is requested (nonzero pointer) and no
*     value exists in the keymap, the value is set to the "NULL" that is given
*     in brackets after the description of each parameter. The notch filter
*     frequency edges are arrays with a maximum length of SMF__MXNOTCH and
*     should be statically allocated before calling this routine. For these
*     special cases (FILT_NOTCHHIGH/NOTCHLOW), if no values are present in the
*     keymap, the way the caller can detect this is by checking the value
*     of filt_nnotch (which will be set to 0).
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     DSB: David S. Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-04-15 (EC):
*        Initial version.
*     2010-01-08 (EC):
*        Add parameter dcflagall to dcflag (renamed from dcbad)
*     2010-01-11 (EC):
*        Add fillgaps
*     2010-03-23 (DSB):
*        Replace old DC jump config parameters with new ones.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-07-06 (TIMJ):
*        Add noiseclip parameter.
*     2010-09-10 (DSB):
*        Remove dclimcorr, and rename dcmedianwidth as dcsmooth.
*     2010-09-15 (DSB):
*        Change spikeiter argument to spikebox.
*     2010-09-23 (EC):
*        -Rename FLAGSTAT to FLAGSLOW
*        -Add FLAGFAST
*     2010-09-28 (DSB):
*        Added zeropad.
*     2010-10-12 (EC):
*        Add whiten
*     2010-10-13 (EC):
*        Add compreprocess
*     2011-03-22 (EC):
*        Add pcathresh
*     2011-04-15 (EC):
*        Add groupsubarray
*     2011-06-16 (EC):
*        Add downsampfreq
*     2011-06-21 (EC):
*        Add downsampscale
*     2011-06-22 (EC):
*        Add downsampfreq
*     2011-06-23 (EC):
*        noisecliphigh and noisecliplow instead of noiseclip
*     2011-08-09 (EC):
*        Add noiseclipprecom
*     2011-10-17 (EC):
*        Add pcalen
*     2012-02-21 (EC):
*        Add argument "data" and allow parameters that represent a number
*        of time slices to be given in units of seconds instead.
*     2012-11-14 (DSB):
*        Add argument "deconvmce".
*     2013-02-18 (DSB):
*        Add argument "filt_edgewidth".
*     2013-03-18 (DSB):
*        Added argument "qualifier".
*     2013-10-21 (DSB):
*        Changed argument "filt_edgewidth" to "filt_order".
*     2021-4-5 (DSB):
*        Added "startup".
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2009-2011 University of British Columbia
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_get_cleanpar"

void smf_get_cleanpar( AstKeyMap *keymap, const char *qualifier,
                       const smfData *data, double *badfrac,
                       int *dcfitbox, int *dcmaxsteps, double *dcthresh,
                       int *dcsmooth, int *dclimcorr, int *dkclean,
                       int *fillgaps, int *zeropad, double *filt_edgelow,
                       double *filt_edgehigh, double *filt_edge_smallscale,
                       double *filt_edge_largescale, double *filt_notchlow,
                       double *filt_notchhigh, int *filt_nnotch, int *dofilt,
                       double *flagslow, double *flagfast, int *order,
                       double *spikethresh, dim_t *spikebox,
                       double *noisecliphigh, double *noisecliplow, int *whiten,
                       int *compreprocess, dim_t *pcalen, double *pcathresh,
                       int *groupsubarray, double *downsampscale,
                       double *downsampfreq, int *noiseclipprecom,
                       int *deconvmce, double *delay, int *filt_order,
                       dim_t *startup, int *status ) {

  char buf[255];                /* Buffer for qualified parameter names */
  const char *key;              /* Pointer to used parameter name */
  dim_t temp2;
  int dofft=0;                  /* Flag indicating that filtering is required */
  int f_nnotch=0;               /* Number of notch filters in array */
  int f_nnotch2=0;              /* Number of notch filters in array */
  int i;                        /* Loop counter */
  int temp;                     /* temporary signed integer */

  /* Main routine */
  if (*status != SAI__OK) return;


  /* Obtain parameters from keymap when non-NULL pointers given */

  if( badfrac ) {
    *badfrac = 0.0;
    key = smf_keyname( keymap, "BADFRAC", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, badfrac );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *badfrac );
  }

  if( dcfitbox ) {
    temp2 = 0;
    key = smf_keyname( keymap, "DCFITBOX", qualifier, buf, sizeof( buf ), status );
    smf_get_nsamp( keymap, key, data, &temp2, status );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%" DIM_T_FMT, status,
               key, temp2 );
    *dcfitbox = (int) temp2;
  }

  if( dcmaxsteps ) {
    *dcmaxsteps = 0;
    key = smf_keyname( keymap, "DCMAXSTEPS", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( (temp < 0) ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be 0 or more.", status, key );
    } else {
      *dcmaxsteps = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%i", status, key,
               *dcmaxsteps );
  }

  if( dclimcorr ) {
    *dclimcorr = 0;
    key = smf_keyname( keymap, "DCLIMCORR", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, dclimcorr );
    if( *dclimcorr < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be 0 or more.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%i", status, key,
               *dclimcorr );
  }

  if( dcthresh ) {
    *dcthresh = 0;
    key = smf_keyname( keymap, "DCTHRESH", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, dcthresh );
    if( *dcthresh < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be >= 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *dcthresh );
  }

  if( dcsmooth ) {
    temp2 = 0;
    key = smf_keyname( keymap, "DCSMOOTH", qualifier, buf, sizeof( buf ), status );
    smf_get_nsamp( keymap, key, data, &temp2, status );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%" DIM_T_FMT, status, key,
               temp2 );
    *dcsmooth = (int) temp2;
  }

  if( dkclean ) {
    *dkclean = 0;
    key = smf_keyname( keymap, "DKCLEAN", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( temp != 0 && temp != 1 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *dkclean = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*dkclean ? "enabled" : "disabled") );
  }

  if( fillgaps ) {
    *fillgaps = 0;
    key = smf_keyname( keymap, "FILLGAPS", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( (temp != 0) && (temp != 1) ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *fillgaps = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*fillgaps ? "enabled" : "disabled" ) );
  }

  if( zeropad ) {
    *zeropad = 1;
    key = smf_keyname( keymap, "ZEROPAD", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( (temp != 0) && (temp != 1) ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *zeropad = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*zeropad ? "enabled" : "disabled" ) );
  }

  if( filt_edgelow ) {
    *filt_edgelow = 0;
    key = smf_keyname( keymap, "FILT_EDGELOW", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, filt_edgelow );

    if( *filt_edgelow ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *filt_edgelow );
  }

  if( filt_edgehigh ) {
    *filt_edgehigh = 0;
    key = smf_keyname( keymap, "FILT_EDGEHIGH", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, filt_edgehigh );

    if( *filt_edgehigh ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *filt_edgehigh );
  }

  if( filt_edge_largescale ) {
    *filt_edge_largescale = 0;
    key = smf_keyname( keymap, "FILT_EDGE_LARGESCALE", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, filt_edge_largescale );

    if( *filt_edge_largescale ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *filt_edge_largescale );
  }

  if( filt_edge_smallscale ) {
    *filt_edge_smallscale = 0;
    key = smf_keyname( keymap, "FILT_EDGE_SMALLSCALE", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, filt_edge_smallscale );

    if( *filt_edge_smallscale ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *filt_edge_smallscale );
  }

  if( filt_order ) {
    *filt_order = 0;
    key = smf_keyname( keymap, "FILT_ORDER", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, filt_order );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%d", status, key,
               *filt_order );
  }

  if( filt_notchlow ) {
    key = smf_keyname( keymap, "FILT_NOTCHLOW", qualifier, buf, sizeof( buf ), status );
    if( !astMapGet1D( keymap, key, SMF__MXNOTCH, &f_nnotch, filt_notchlow ) ) {
      f_nnotch=0;
    }
  }

  if( filt_notchhigh ) {
    key = smf_keyname( keymap, "FILT_NOTCHHIGH", qualifier, buf, sizeof( buf ), status );
    if( !astMapGet1D( keymap, key, SMF__MXNOTCH, &f_nnotch2,
                      filt_notchhigh ) ) {
      f_nnotch2=0;
    }
  }

  if( f_nnotch && f_nnotch2 ) {
    /* Number of upper and lower edges must match */
    if( f_nnotch != f_nnotch2 ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,
             ": Elements in FILT_NOTCHHIGH != number in FILT_NOTCHLOW",
             status);
    } else if( (f_nnotch > 1) || (filt_notchlow[0] && filt_notchhigh[0]) ) {
      /* Make sure we have multiple notches, or if there is only 1, that
         the edges are not both set to 0 (defaults from smurf_sc2clean if
         not being used) */
      dofft = 1;
      if( filt_nnotch ) {
        *filt_nnotch = f_nnotch;
      }

      msgOutiff( MSG__DEBUG, "", FUNC_NAME
                 ": number components in notch filter=%i", status,
                 f_nnotch );

      for( i=0; i<f_nnotch; i++ ) {
        msgOutiff( MSG__DEBUG, "", FUNC_NAME ":     %f -- %f Hz", status,
                   filt_notchlow[i], filt_notchhigh[i] );
      }
    } else {
      f_nnotch = 0;
      *filt_nnotch = f_nnotch;
    }
  }

  if( whiten ) {
    key = smf_keyname( keymap, "WHITEN", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, whiten );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*whiten ? "enabled" : "disabled") );

    if( *whiten ) {
      dofft = 1;
    }
  }

  if( dofilt ) {
    *dofilt = dofft;
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DOFILT=%i", status,
               *dofilt );
  }

  if( flagslow ) {
    *flagslow = 0;
    key = smf_keyname( keymap, "FLAGSLOW", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, flagslow );
    if( *flagslow < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s cannot be < 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *flagslow );
  }

  if( flagfast ) {
    *flagfast = 0;
    key = smf_keyname( keymap, "FLAGFAST", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, flagfast );
    if( *flagfast < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s cannot be < 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *flagfast );
  }

  if( order ) {
    *order = -1;
    key = smf_keyname( keymap, "ORDER", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, order );
    if( *order < -1 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be >= -1", status, key );
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%i", status, key,
               *order );
  }

  if( spikethresh ) {
    *spikethresh = 0;
    key = smf_keyname( keymap, "SPIKETHRESH", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, spikethresh );
    if( *spikethresh < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be >= 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *spikethresh );
  }

  if( spikebox ) {
    *spikebox = 0;
    key = smf_keyname( keymap, "SPIKEBOX", qualifier, buf, sizeof( buf ), status );
    smf_get_nsamp( keymap, key, data, spikebox, status );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%" DIM_T_FMT, status, key,
               *spikebox );
  }

  if( noisecliphigh ) {
    key = smf_keyname( keymap, "NOISECLIPHIGH", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, noisecliphigh );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%g", status, key, *noisecliphigh );
  }

  if( noisecliplow ) {
    key = smf_keyname( keymap, "NOISECLIPLOW", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, noisecliplow );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%g", status, key,
               *noisecliplow );
  }

  if( compreprocess ) {
    key = smf_keyname( keymap, "COMPREPROCESS", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, compreprocess );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*compreprocess ? "enabled" : "disabled") );
  }

  if( pcalen ) {
    *pcalen = 0;
    key = smf_keyname( keymap, "PCALEN", qualifier, buf, sizeof( buf ), status );
    smf_get_nsamp( keymap, key, data, pcalen, status );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%" DIM_T_FMT, status, key,
               *pcalen );
  }

  if( pcathresh ) {
    *pcathresh = 0;
    key = smf_keyname( keymap, "PCATHRESH", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, pcathresh );
    if( *pcathresh < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be >= 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *pcathresh );
  }

  if( groupsubarray ) {
    *groupsubarray = 0;
    key = smf_keyname( keymap, "GROUPSUBARRAY", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );

    if( (temp != 0) && (temp != 1) ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *groupsubarray = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*groupsubarray ? "enabled" : "disabled" ) );
  }

  if( downsampscale ) {
    *downsampscale = 0;

    key = smf_keyname( keymap, "DOWNSAMPSCALE", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, downsampscale );

    if( *downsampscale < 0 ) {
      msgOutiff( MSG__DEBUG, "", FUNC_NAME
                ": WARNING can't convert %s < 0 to a frequency, "
                "skipping.", status, key );
      *downsampscale = 0;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *downsampscale );
  }

  if( downsampfreq ) {
    *downsampfreq = 0;
    key = smf_keyname( keymap, "DOWNSAMPFREQ", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, downsampfreq );
    if( *downsampfreq < 0 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be >= 0.", status, key );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *downsampfreq );
  }

  if( noiseclipprecom ) {
    *noiseclipprecom = 0;
    key = smf_keyname( keymap, "NOISECLIPPRECOM", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( temp != 0 && temp != 1 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *noiseclipprecom = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*noiseclipprecom ? "enabled" : "disabled") );
  }

  if( deconvmce ) {
    *deconvmce = 0;
    key = smf_keyname( keymap, "DECONVMCE", qualifier, buf, sizeof( buf ), status );
    astMapGet0I( keymap, key, &temp );
    if( temp != 0 && temp != 1 ) {
      *status = SAI__ERROR;
      errRepf(FUNC_NAME, "%s must be either 0 or 1.", status, key );
    } else {
      *deconvmce = temp;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s is %s", status, key,
               (*deconvmce ? "enabled" : "disabled") );
  }

  if( delay ) {
    *delay = 0.0;
    key = smf_keyname( keymap, "DELAY", qualifier, buf, sizeof( buf ), status );
    astMapGet0D( keymap, key, delay );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%f", status, key,
               *delay );
  }

  if( startup ) {
    *startup = 0;
    smf_get_nsamp( keymap, "STARTUP", data, startup, status );
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": %s=%" DIM_T_FMT, status,
               "STARTUP", *startup );
  }

}
