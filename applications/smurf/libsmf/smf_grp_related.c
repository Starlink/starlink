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
*     smf_grp_related(  Grp *igrp, const int grpsize, const int grpbywave, 
*                       smfGroup **group,int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Input Grp
*     grpsize = const int (Given)
*        Size of input Grp
*     grpbywave = const int (Given)
*        Flag to denote whether to group files by common wavelength
*     group = smfGroup ** (Returned)
*        Returned smfGroup
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine groups related files together and populates a
*     smfGroup. The smfGroup contains a copy of the input Grp, the
*     number of subgroups within the smfGroup, and an array of
*     pointers to integer arrays which contain the Grp index values
*     corresponding to related files. This method reduces the number
*     of Grps required to 1, and allows new Grps to be created on
*     demand so that the maximum Grp number is not exceeded.

*  Notes:
*     Resources allocated with this routine should be freed by calling
*     smf_close_smfGroup

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.
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
#include "ndf.h"
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_grp_related"

void smf_grp_related (  Grp *igrp, const int grpsize, const int grpbywave, smfGroup **group, int *status ) {

  /* Local variables */
  int i;                      /* Loop counter for index into Grp */
  int j;                      /* Loop counter */
  int k = 0;                  /* Incremental counter for subgroup indices */
  int ngroups = 0;            /* Incremental counter for subgroups to be stored */
  smfHead *hdr;               /* Header for current file */
  double opentime;            /* RTS_END value at beginning of written data */
  double writetime;           /* RTS_END value at end of written data */
  int frame;                  /* Variable to store either first or last frame */

  int **subgroups = NULL;     /* Array of subgroups containing index arrays into parent Grp */
  int *indices = NULL;        /* Array of indices to be stored in subgroup */
  double *starts = NULL;      /* Array of starting RTS_END values */
  double *ends = NULL;        /* Array of ending RTS_END values */
  int nelem;                  /* Number of elements in index array */

  smfData *data = NULL;       /* Current smfData */
  double *lambda = NULL;      /* Wavelength - only used if grpbywave is true */
  dim_t *nbolx = NULL;        /* Number of bolometers in X direction */
  dim_t *nboly = NULL;        /* Number of bolometers in Y direction */
  dim_t nx;                   /* (data->dims)[0] */
  dim_t ny;                   /* (data->dims)[1] */
  size_t allOK = 1;           /* Flag to determine whether to continue */
  double obslam;              /* Observed wavelength from FITS header (m) */

  if ( *status != SAI__OK ) return;

  /* Check that the Grp size is reasonable */
  if ( grpsize < 1 || grpsize > GRP__MAXG ) {
    if ( *status == SAI__OK ) {
      msgSeti("SZ",grpsize);
      msgSeti("MAX",GRP__MAXG);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid input Grp size, ^SZ. Must lie between 1 and ^MAX", status);
    }
  }

  /* Allocate space for groups array - can't be any bigger than
     grpsize so use that. Initialize everything to NULL */
  subgroups = smf_malloc( grpsize, sizeof(int*), 1, status );
  starts = smf_malloc( grpsize, sizeof(double), 1, status );
  ends = smf_malloc( grpsize, sizeof(double), 1, status );
  nbolx = smf_malloc( grpsize, sizeof(dim_t), 1, status );
  nboly = smf_malloc( grpsize, sizeof(dim_t), 1, status );
  
  if ( *status != SAI__OK ) goto CLEANUP;

  /* Do we want to group by wavelength? */
  if ( grpbywave == 1 ) {
    /* If yes, then we only need to allocate space for 4 elements */
    nelem = SMF__MXSMF;
    /* And define the wavelength array */
    lambda = smf_malloc( grpsize, sizeof(double), 1, status );
  } else {
    /* OK we might have data from up to 8 subarrays so allocate
       twice as much space */
    nelem = 2*SMF__MXSMF;
  }

  /* Loop over files in input Grp: remember Grps are indexed from 1 */
  for (i=1; i<=grpsize; i++) {
    /* First step: open file and read start/end RTS_END values */
    smf_open_file( igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );
    hdr = data->hdr;
    /* Set header for first time slice */
    frame = 0;
    smf_tslice_ast( data, frame, 0, status );
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to retrieve first timeslice", status);
      return;
    }
    opentime = hdr->state->rts_end;
    /* Set header for last time slice */
    frame = (data->dims)[2] - 1;
    smf_tslice_ast( data, frame, 0, status );
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to retrieve final timeslice", status);
      return;
    }
    writetime = hdr->state->rts_end;
    /* Retrieve wavelength if necessary */
    if ( grpbywave ) {
      smf_fits_getD(hdr, "WAVELEN", &obslam, status );
    }
    if ( *status != SAI__OK ) return;

    /* Now get data dimensions */
    nx = (data->dims)[0];
    ny = (data->dims)[1];
    /* Now to check if it's a related file... */
    for ( j=0; j<grpsize; j++ ) {
      /* Does the subgroup exist? */
      if ( subgroups[j] != 0 ) {
	/* If yes, are we grouping by wavelength? */
	if ( grpbywave ) {
	  if ( obslam != lambda[j] ) {
	    allOK = 0;
	  }
	}
	if ( allOK ) {
	  /* Then check if the RTS_END values match */
	  indices = subgroups[j];
	  if ( opentime == starts[j] && writetime == ends[j] ) {
	    if ( nx == nbolx[j] && ny == nboly[j]) {
	      /* Store in first available slot in current subgroup.
		 No point starting at 0 because it WILL be occupied if
		 we've reached this point */
	      for ( k=1; k<nelem; k++) {
		if (indices[k] == 0) {
		  indices[k] = i;
		  goto CLOSE;
		}
	      }
	    } else {
	      msgOutif(MSG__VERB," ", 
		       "Start and end times match but data arrays are of different size", 
		       status);
	    }
	  }
	} /* Close if allOK */
      } else {
	/* If not, there's nothing to match so create it and add the
	 current index */
	indices = smf_malloc( nelem, sizeof(int), 1, status);
	/* Initialize the pointers to NULL */
	if ( *status != SAI__OK ) {
	  errRep(FUNC_NAME, "Unable to allocate memory to store index array", status);
	  return;
	}
	indices[0] = i;
	subgroups[j] = indices;
	/* Add new open/end times to arrays */
	starts[j] = opentime;
	ends[j] = writetime;
	nbolx[j] = nx;
	nboly[j] = ny;
	if ( grpbywave ) {
	  lambda[j] = obslam;
	}
	ngroups++;
	goto CLOSE;
      }
    }
  CLOSE:
    smf_close_file( &data, status );
  }
  /* Sanity check - make sure that the number of groups is less than
     the grpsize */
  if ( ngroups > grpsize ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("G",grpsize);
      msgSeti("S",ngroups);
      errRep(FUNC_NAME, 
	     "Number of subgroups, ^S, exceeds grpsize, ^G. Possible programming error", 
	     status);
    }
  }

  /* Create the smfGroup */
  *group = smf_construct_smfGroup( igrp, subgroups, ngroups, nelem, 0, 
				   status );

 CLEANUP:
  smf_free( starts, status );
  smf_free( ends, status );
  smf_free( nbolx, status );
  smf_free( nboly, status );
  if ( *status != SAI__OK ) {
    smf_free( indices, status );
    smf_free( subgroups, status );
  }

}
