/*
*+
*  Name:
*     smf_correct_steps

*  Purpose:
*     Locate and repair DC steps

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_correct_steps( smfData *data, unsigned char *quality, 
*                        double dcthresh, dim_t dcbox, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be repaired (in-place)
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of steps
*        will have bit SMF__Q_JUMP set. 
*     dcthresh = double (Given)
*        N-sigma threshold for DC jump to be detected
*     dcbox = dim_t (Given)
*        Length of box (in samples) over which to calculate statistics
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     First estimate white-noise level as r.m.s. in box of length dcbox.
*     Then calculate running averages in two adjacent intervals of
*     length dcbox. Jumps are flagged and corrected at peak values of
*     difference between the averages in two intervals. Data stream is
*     assumed to be periodic (routine wraps-around).

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-05 (EC):
*        Initial Version
*     2009-04-01 (EC):
*        - Use smf_quick_noise to estimate signal r.m.s.
*        - wrap-around at ends of the bolometer data
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <math.h>


#define FUNC_NAME "smf_correct_steps"

void smf_correct_steps( smfData *data, unsigned char *quality, 
		       double dcthresh, dim_t dcbox, int *status ) {

  /* Local Variables */
  double *alljump=NULL;         /* Buffer containing DC jumps */
  dim_t base;                   /* Index to starting point of bolo */
  double baseline;              /* Current baseline estimate  */
  double *dat=NULL;             /* Pointer to bolo data */
  double dcstep;                /* Size of DC steps to detect */
  dim_t i;                      /* Loop Counter */
  int injump;                   /* Flag for DC jump detection */
  dim_t j;                      /* Loop Counter */
  double maxdiff;               /* Max difference between mean1 and mean2 */
  dim_t maxind;                 /* index to location of maxdiff */
  double mean1;                 /* Box means to search for DC steps */
  double mean2;                 /* "    "                           */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t nmean1;                 /* Number of samples in mean1 */
  dim_t nmean2;                 /* Number of samples in mean1 */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  unsigned char mask;           /* bitmask for quality */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Assert bolo-ordered data to make life easier */
  smf_dataOrder( data, 0, status );

  /* Pointers to data and quality */
  dat = data->pntr[0];
  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a DATA component", status );
    return;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
	   "Data is not double-precision", status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    ntslice = (data->dims)[0];
    nbolo = (data->dims)[1]*(data->dims)[2];


    /* If data stream too short for box size generate error */
    if( (dcbox*2) > ntslice ) {
      *status = SAI__ERROR;
      msgSeti("NTSLICE",ntslice);
      msgSeti("DCBOX",ntslice);
      errRep(FUNC_NAME,
	     "Can't find jumps: ntslice=^NTSLICE, must be > dcbox(=^DCBOX)*2",
	     status);
    }

    /* Check for valid threshold */
    if( dcthresh <= 0 ) {
      *status = SAI__ERROR;
      msgSeti("DCTHRESH",dcthresh);
      errRep(FUNC_NAME,
	     "Can't find jumps: dcthresh (^dcthresh) must be > 0",
	     status);
    }
  }  

  /* Set the quality mask to complement of SMF__Q_JUMP so that past DC step
     flags get ignored */

  mask = ~SMF__Q_JUMP;

  /* Repair DC steps */
  if( dcbox && dcthresh && (*status == SAI__OK) ) {

    /* allocate alljump buffer */
    alljump = smf_malloc( ntslice, sizeof(*alljump), 0, status );
    
    if( *status == SAI__OK ) for( i=0; i<nbolo; i++ ) {
      base = i*ntslice;

      /* Continue if bolo stream is not flagged bad */
      if( !(qua[base] & SMF__Q_BADB) && (*status == SAI__OK) ) {
	
	/* initial conditions for jump detection */
	smf_simple_stats( dat, base, dcbox, qua, mask, &mean1, NULL, 
			  &nmean1, status );
	
	smf_simple_stats( dat, base+dcbox, dcbox, qua, mask, &mean2, NULL, 
			  &nmean2, status );
	
	/* Estimate rms in a box as the bolo rms divided by sqrt(dcbox) */
	dcstep = smf_quick_noise( data, i, dcbox, 10, qua, mask, status ) *
	  dcthresh / sqrt(dcbox);

	if( *status == SAI__OK ) {
	  memset( alljump, 0, ntslice*sizeof(*alljump) );
	  injump = 0;  /* jump occured somewhere in boxes */
	  maxdiff = 0; /* max difference between mean1 and mean2 for jump */
	  maxind = 0;  /* index to location of maxdiff */
	  
	  /* counter is at mean1/mean2 boundary -- location potential jumps.
	     Counter runs from dcbox --> ntslice+dcbox-1, and the ends of
             the bolometer array are assumed to wrap-around */

	  for( j=dcbox; j<(ntslice+dcbox); j++ ) {

	    if( fabs(mean2 - mean1) >= dcstep ) {
	      /* is the difference between the two boxes significant? */
	      
	      if( !injump ) {
		/* Starting new jump, initialize search */
		maxdiff = mean2 - mean1;
		maxind = j%ntslice;
		injump = 1;
	      } else if( fabs(mean2 - mean1) > fabs(maxdiff) ) {
		/* Update the search for the maximum step size */
		maxdiff = mean2 - mean1;
		maxind = j%ntslice;
	      }
	    } else {
	      /* If difference is small, but injump is set, that means we've
		 finished the search for the last step */
	      if( injump ) {
		alljump[maxind] = maxdiff;
		injump = 0;
	      }
	    }
	    
	    /* Move along the boxes and update the mean estimates */
	    if( !(qua[base+(j-dcbox)] & mask) ) {
	      /* Drop sample at j-dcbox from mean1 */
	      mean1 = (nmean1*mean1 - dat[base+(j-dcbox)]) / (nmean1-1);
	      nmean1 --;
	    }
	    if( !(qua[base+(j%ntslice)] & mask) ) {
	      /* Move sample at j from mean2 to mean1 */
	      mean1 = (nmean1*mean1 + dat[base+(j%ntslice)]) / (nmean1+1);
	      nmean1 ++;
	      
	      mean2 = (nmean2*mean2 - dat[base+(j%ntslice)]) / (nmean2-1);
	      nmean2 --;
	      
	    }
	    if( !(qua[base+((j+dcbox+1)%ntslice)] & mask) ) {
	      /* Add sample at j+dcbox+1 to mean1 */
	      mean2 = (nmean2*mean2 + dat[base+((j+dcbox+1)%ntslice)]) / 
		(nmean2+1);
	      nmean2 ++;
	    }
	  }
	  
	  /* calculate the new corrected baseline */
	  baseline = 0;
	  for( j=1; j<ntslice; j++ ) {
	    if( alljump[j] ) {

	      /* Update the baseline at each sample in which jumps occured */
	      baseline += alljump[j];
	      
	      /* Flag the jump in QUALITY */
	      qua[base+j] |= SMF__Q_JUMP;
	    } 

	    /* Correct the data by the current baseline estimate */
	    dat[base+j] -= baseline;
	  }
	}
      }
    }

    /* Clean up */
    alljump = smf_free( alljump, status );
  }
    
}
