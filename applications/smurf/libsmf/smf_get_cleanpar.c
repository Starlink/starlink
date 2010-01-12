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
*     smf_get_cleanpar( AstKeyMap *keymap, size_t *apod, double *badfrac,
*                       dim_t *dcbox, int *dcflag, double *dcthresh,
*                       int *dkclean, int *fillgaps, double *filt_edgelow,
*                       double *filt_edgehigh, double *filt_notchlow,
*                       double *filt_notchhigh, int *filt_nnotch, int *dofilt,
*                       double *flagstat, int *order, double *spikethresh,
*                       size_t *spikeiter, int *status )

*  Arguments:
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters
*     apod = size_t* (Returned)
*        Apodize time series within this number of samples (NULL:0)
*     badfrac = double* (Returned)
*        Fraction of bad samples in order for entire bolometer to be
*        flagged as bad (NULL:0)
*     dcbox = dim_t* (Returned)
*        Width of the box (samples) over which to estimate the mean
*        signal level for DC step detection (NULL:0)
*     dcflag = int* (Returned)
*        if 0 handle all bolos independently and attempt to fix steps (NULL:0)
*        if 1 just flag entire bolo as bad if step encountered
*        if 2 identify steps, and then repair/flag ALL bolometers at those spots
*     dcthresh = double* (Returned)
*        N-sigma threshold at which to detect DC steps (NULL:0)
*     dkclean = int* (Returned)
*        If true, clean dark squids from bolos (NULL:-1)
*     fillgaps = int* (Returned)
*        If true, fill regions of data flagged with spikes, DC steps, bad
*        samples with constrained realization of noise.
*     filt_edgelow = double* (Returned)
*        Apply a hard-edged low-pass filter at this frequency (Hz) (NULL:0)
*     filt_edgehigh = double* (Returned)
*        Apply a hard-edged high-pass filter at this frequency (Hz) (NULL:0)
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
*     flagstat = double* (Returned)
*        Flag data during slew speeds less than FLAGSTAT (arcsec/sec) (NULL:0)
*     order = int* (Returned)
*        Fit and remove polynomial baselines of this order (NULL:-1)
*     spikethresh = double* (Returned)
*        Flag spikes SPIKETHRESH-sigma away from mean (NULL:0)
*     spikeiter = size_t* (Returned)
*        If 0 iteratively find spikes until convergence. Otherwise 
*        execute precisely this many iterations (NULL:0)
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
*     {enter_new_authors_here}

*  History:
*     2009-04-15 (EC):
*        Initial version.
*     2010-01-08 (EC):
*        Add parameter dcflagall to dcflag (renamed from dcbad)
*     2010-01-11 (EC):
*        Add fillgaps
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2009-2010 University of British Columbia
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

void smf_get_cleanpar( AstKeyMap *keymap, size_t *apod, double *badfrac,
                       dim_t *dcbox, int *dcflag, double *dcthresh,
                       int *dkclean, int *fillgaps, double *filt_edgelow, 
                       double *filt_edgehigh, double *filt_notchlow,
                       double *filt_notchhigh, int *filt_nnotch,
                       int *dofilt, double *flagstat, int *order,
                       double *spikethresh, size_t *spikeiter, int *status ) {
  
  int dofft=0;                  /* Flag indicating that filtering is required */
  int f_nnotch=0;               /* Number of notch filters in array */
  int f_nnotch2=0;              /* Number of notch filters in array */
  int i;                        /* Loop counter */
  int temp;                     /* temporary signed integer */

  /* Main routine */
  if (*status != SAI__OK) return;


  /* Obtain parameters from keymap when non-NULL pointers given */

  if( apod ) {
    if( astMapGet0I( keymap, "APOD", &temp ) ) {
      if( temp < 0 ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "APOD cannot be < 0.", status );
      } else {
        *apod = (size_t) temp;
      }
    } else {
      *apod = 0;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": APOD=%zu", status,
               *apod );
  }

  if( badfrac ) {
    if( !astMapGet0D( keymap, "BADFRAC", badfrac ) ) {
      *badfrac = 0;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": BADFRAC=%f", status,
               *badfrac );
  }

  if( dcbox ) {
    if( astMapGet0I( keymap, "DCBOX", &temp ) ) {
      if( temp < 0 ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "DCBOX cannot be < 0.", status );
      } else {
        *dcbox = (dim_t) temp;
      }
    } else {
      *dcbox = 0;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DCBOX=%" DIM_T_FMT, status,
               *dcbox );
  }

  if( dcflag ) {
    *dcflag = 0;

    if( astMapGet0I( keymap, "DCBAD", &temp ) ) {
      if( (temp < 0) || (temp > 1) ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "DCBAD must be either 0 or 1.", status );
      } else {
        *dcflag = temp;
      }
    }

    if( astMapGet0I( keymap, "DCFLAGALL", &temp ) ) {

      if( (temp < 0) && (temp > 1) ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "DCFLAGALL must be either 0 or 1.", status );
      } else {

        if( (*dcflag == 1) && (temp) ) {
          msgOutif( MSG__VERB, "", FUNC_NAME ": DCFLAGALL overriding DCBAD",
                    status );
        }

        *dcflag = temp*2;
      }
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DCFLAG=%i", status,
               *dcflag );
  }

  if( dcthresh ) {
    if( !astMapGet0D( keymap, "DCTHRESH", dcthresh ) ) {
      *dcthresh = 0;
    } else if( *dcthresh < 0 ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "DCTHRESH must be >= 0.", status );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DCTHRESH=%f", status,
               *dcthresh );
  }

  if( dkclean ) {
    if( astMapGet0I( keymap, "DKCLEAN", &temp ) ) {
      if( (temp < 0) || (temp > 1) ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "DKCLEAN must be either 0 or 1.", status );
      } else {
        *dkclean = temp;
      }
    } else {
      *dkclean = -1;
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DKCLEAN=%i", status,
               *dkclean );
  }

  if( fillgaps ) {
    if( astMapGet0I( keymap, "FILLGAPS", &temp ) ) {
      if( (temp < 0) || (temp > 1) ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "FILLGAPS must be either 0 or 1.", status );
      } else {
        *fillgaps = temp;
      }
    } else {
      *fillgaps = -1;
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": FILLGAPS=%i", status,
               *fillgaps );
  }

  if( filt_edgelow ) {
    if( !astMapGet0D( keymap, "FILT_EDGELOW", filt_edgelow ) ) {
      *filt_edgelow = 0;
    }

    if( *filt_edgelow ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": FILT_EDGELOW=%f", status,
               *filt_edgelow );
  }

  if( filt_edgehigh ) {
    if( !astMapGet0D( keymap, "FILT_EDGEHIGH", filt_edgehigh ) ) {
      *filt_edgehigh = 0;
    } 

    if( *filt_edgehigh ) {
      dofft = 1;
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": FILT_EDGEHIGH=%f", status,
               *filt_edgehigh );
  }


  if( filt_notchlow ) {
    if( !astMapGet1D( keymap, "FILT_NOTCHLOW", SMF__MXNOTCH, &f_nnotch, 
                      filt_notchlow ) ) {
      f_nnotch=0;
    }
  }

  if( filt_notchhigh ) {
    if( !astMapGet1D( keymap, "FILT_NOTCHHIGH", SMF__MXNOTCH, &f_nnotch2, 
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

  if( dofilt ) {
    *dofilt = dofft;
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": DOFILT=%i", status,
               *dofilt );
  }

  if( flagstat ) {
    if( astMapGet0D( keymap, "FLAGSTAT", flagstat ) ) {
      if( *flagstat < 0 ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "FLAGSTAT cannot be < 0.", status );
      }
    } else {
      *flagstat = 0;
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": FLAGSTAT=%f", status,
               *flagstat );
  }

  if( order ) {
    if( !astMapGet0I( keymap, "ORDER", order ) ) {
      *order = -1;
    } else if( *order < -1 ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "ORDER must be >= -1", status );
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": ORDER=%i", status,
               *order );
  }

  if( spikethresh ) {
    if( !astMapGet0D( keymap, "SPIKETHRESH", spikethresh ) ) {
      *spikethresh = 0;
    } else if( *spikethresh < 0 ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "SPIKETHRESH must be >= 0.", status );
    }

    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": SPIKETHRESH=%f", status,
               *spikethresh );
  }

  if( spikeiter ) {
    if( astMapGet0I( keymap, "SPIKEITER", &temp ) ) {
      if( temp < 0 ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "spikeiter cannot be < 0.", status );
      } else {
        *spikeiter = (size_t) temp;
      }
    } else {
      *spikeiter = 0;
    }
    msgOutiff( MSG__DEBUG, "", FUNC_NAME ": SPIKEITER=%zu", status,
               *spikeiter );
  }
}
