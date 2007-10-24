/*
*+
*  Name:
*     smf_concat_smfGroup

*  Purpose:
*     Concatenate many small chunks of data into single large chunks.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_concat_smfGroup( smfGroup *igrp, smfArray **concat, int *status )

*  Arguments:
*     igrp = SmfGroup* (Given)
*        Group of input data files
*     concat = smfArray ** (Returned)
*        smfArray containing concatenated data for each subarray
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function takes an input group containing data taken continuously,
*     but chopped up into smaller files (possibly from multiple subarrays).
*     This routine attempts to load all of the data into memory at once, 
*     concatenates it into a single contiguous piece of memory for each
*     subarray, and optionally re-orders the data to bolo-ordered rather
*     than time-ordered if desired.
*     
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-10-19 (EC):
*        Initial version.

*  Notes:
*     Currently buggy / not fully implemented.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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


#define FUNC_NAME "smf_concat_smfGroup"

void smf_concat_smfGroup( smfGroup *igrp, smfArray **concat, int *status ) {

  /* Local Variables */
  smfData *data=NULL;           /* Concatenated smfData */
  int dims[3];                  /* Size of each axis in NDF */
  char dtype[NDF__SZTYP+1];     /* String for DATA/VARIANCE type */
  int exists;                   /* Flag for NDF component existence */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  int havearray[3];             /* flags for DATA/QUALITY/VARIANCE present */
  smfHead *hdr;                 /* pointer to smfHead in concat data */
  dim_t i;                      /* Loop counter */
  int indf;                     /* NDF identifier for current file */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  dim_t nbolo;                  /* Number of detectors */
  dim_t ndata;                  /* Total data points: nbolo*tlen */
  int ndims;                    /* Number of axes in NDF */
  int nrelated;                 /* Number of subarrays */
  int pass;                     /* Two passes over list of input files */
  char *pname=NULL;             /* Pointer to filename */
  smfData *refdata=NULL;        /* Reference smfData */
  int refdims[2];               /* reference dimensions for array (not time) */
  char refdtype[NDF__SZTYP+1];  /* String for reference DATA/VARIANCE type */
  smfHead *refhdr=NULL;         /* pointer to smfHead in ref data */
  dim_t refndata;               /* Number data points in reference file */
  dim_t reftlen;                /* Number of time slices in reference file */
  dim_t tchunk;                 /* Time offset in concat. array this chunk */
  dim_t tlen;                   /* Time length entire concatenated array */
  dim_t sz;                     /* Data type size */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Allocate space for the smfArray */
  *concat = smf_create_smfArray( status );

  /* Determine how many subarrays there actually are */
  nrelated = 0;
  for( i=0; i<igrp->nrelated; i++ ) {
    for( j=0; j<igrp->ngroups; j++ ) {
      if( (igrp->subgroups[j][i] > 0) && ((i+1) > nrelated) ) {
	nrelated = i+1;
      }
    }
  }
	
  /* Loop over related elements (number of subarrays) */
  for( i=0; i<nrelated; i++ ) {

    /* Initialize time length of concatenated array */
    tlen = 0; 

    /* Two passes over data for the subarray: first time to identify
       dimensions of each file, second time to actually open each file
       and copy into single array. */

    for( pass=0; pass<2; pass++ ) {
      
      /* Loop over subgroups (number of time chunks) */
      for( j=0; j<igrp->ngroups; j++ ) {

	printf("here1: %i %i %i\n", i, pass, j );
	
	/* First pass - get dimensions */
	if( pass == 0 ) {

	  /* Get filename from the group */
	  pname = filename;
	  grpGet( igrp->grp, igrp->subgroups[j][i], 1, &pname, SMF_PATH_MAX, 
		  status);

	  /* Obtain NDF identifier for this element of the grp */
	  ndgNdfas( igrp->grp, igrp->subgroups[j][i], "READ", &indf, status );

	  /* Obtain the dimensions of the NDF */
	  ndfDim( indf, 3, dims, &ndims, status );

	  /* Verify that the array is 3-dimensional and compatible with the
	     reference array dimensions. */

	  if( *status == SAI__OK ) {
	    if( ndims != 3 ) {
	      *status = SAI__ERROR;
	      msgSetc( "FILE", filename );
	      errRep( FUNC_NAME, "^FILE is not 3-dimensional data!", status );
	    }
	  }

	  if( *status == SAI__OK ) {
	    if( j == 0 ) {
	      /* If this is the first chunk we will use it for refdims - the
		 first two dimensions give numbers of detectors */
	      refdims[0] = dims[0];
	      refdims[1] = dims[1];
	      nbolo = dims[0]*dims[1];

	      /* Check for DATA/VARIANCE/QUALITY and data type */
	      ndfState( indf, "DATA", &(havearray[0]), status );
	      ndfState( indf, "VARIANCE", &(havearray[1]), status );
	      ndfState( indf, "QUALITY", &(havearray[2]), status );
	      ndfType( indf, "DATA,VARIANCE", refdtype, NDF__SZTYP+1, status );
	      
	    } else {
	      /* Check these dims against refdims */
	      if( (dims[0] != refdims[0]) || (dims[1] != refdims[1]) ) {
		*status = SAI__ERROR;
		msgSeti( "XREF", refdims[0] );
		msgSeti( "YREF", refdims[1] );
		msgSeti( "X", dims[0] );
		msgSeti( "Y", dims[1] );
		msgSetc( "FILE", filename );

		errRep( FUNC_NAME, "Detector dimensions (^X,^Y) in ^FILE do not match reference (^XREF,^YREF)", status );
	      }
	      
	      /* Check existence of DATA/QUALITY/VARIANCE */
	      ndfState( indf, "DATA", &exists, status );
	      if( exists != havearray[0] ) {
		*status = SAI__ERROR;
		msgSetc( "FILE", filename );
		if( havearray[0] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component DATA", status );
	      }

	      ndfState( indf, "VARIANCE", &exists, status );
	      if( exists != havearray[1] ) {
		*status = SAI__ERROR;
		msgSetc( "FILE", filename );
		if( havearray[1] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component VARIANCE", status );
	      }

	      ndfState( indf, "QUALITY", &exists, status );
	      if( exists != havearray[2] ) {
		*status = SAI__ERROR;
		msgSetc( "FILE", filename );
		if( havearray[2] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component QUALITY", status );
	      }

	      /* Check data type */
	      ndfType( indf, "DATA,VARIANCE", dtype, NDF__SZTYP+1, status );
	      if( strncmp( dtype, refdtype, NDF__SZTYP+1 ) != 0 ) {
		*status = SAI__ERROR;
		msgSetc( "FILE", filename );
		msgSetc( "DTYPE", dtype );
		msgSetc( "REFDTYPE", refdtype );
		errRep( FUNC_NAME, 
			"^FILE data type is ^DTYPE, should be ^REFDTYPE", 
			status);

	      }
	    }
	  }

	  if( *status == SAI__OK ) {
	    /* At this stage increment tlen for this chunk */	  
	    tlen += dims[2];
	  }

	  printf("here2: %i\n", tlen );
	} 

	/* Second pass copy data over to new array */
	if( (pass == 1) && (*status == SAI__OK) ) {

	  /* Open the file corresponding to this chunk */
	  smf_open_file( igrp->grp, igrp->subgroups[j][i], "READ", 1, 
			 &refdata, status );

	  if( *status == SAI__OK ) {

	    /* If first chunk initialize the concatenated array */
	    if( j == 0 ) {
	      tchunk = 0;
 
	      printf("here3: %i\n", tlen );

	      /* Allocate memory for empty smfData with a smfHead */
	      data = smf_create_smfData( SMF__NOCREATE_DA, status );

	      if( *status == SAI__OK ) {
		/* Copy over basic header information from the reference */
		hdr = data->hdr;
		refhdr = refdata->hdr;	    

		hdr->instrument = refhdr->instrument;

		switch ( hdr->instrument ) {
		case INST__ACSIS:
		  acs_fill_smfHead( hdr, indf, status );
		  break;
		case INST__AZTEC:
		  aztec_fill_smfHead( hdr, NDF__NOID, status );
		  break;
		default:
		  break;
		  /* SCUBA-2 has nothing special here because the focal plane
		     coordinates are derived using an AST polyMap */
		}

		hdr->fitshdr = astCopy( refhdr->fitshdr );

		printf("here4: %i\n", tlen );

		/* Allocate space for the concatenated allState */
		hdr->nframes = tlen;
		hdr->allState = smf_malloc( tlen, sizeof(*(hdr->allState)), 0, 
					    status );

		/* Allocate space in the smfData for DATA/VARAIANCE/QUALITY */
		data->dtype = smf_dtype_fromstring( refdtype, status );
		data->dims[0] = refdims[0];
		data->dims[1] = refdims[1];
		data->dims[2] = tlen;
		data->ndims = 3;

		ndata = nbolo*tlen;

		for( k=0; k<2; k++ ) if( havearray[k] ) {
		  if( k == 2 ) sz = smf_dtype_sz( SMF__USHORT, status );
		  else sz = smf_dtype_sz(data->dtype, status );
		  data->pntr[k] = smf_malloc(ndata, sz, 0, status);
		}
	      }
	    }

	    /* Copy DATA/QUALITY/VARIANCE and JCMTstate information into
               concatenated smfData */

	    reftlen = data->dims[2];
	    refndata = reftlen*nbolo;

	    for( k=0; k<2; k++ ) if( havearray[k] ) {
	      if( k == 2 ) sz = smf_dtype_sz( SMF__USHORT, status );
	      else sz = smf_dtype_sz(data->dtype, status );
	      
	      hdr = data->hdr;
	      refhdr = refdata->hdr;	    

	      printf("here5 \n");

	      if( *status == SAI__OK ) {
		printf("ooga1\n");
		memcpy( (char *)data->pntr[k] + tchunk*nbolo*sz,
			refdata->pntr[k], refndata*sz );

		printf("ooga2\n");
		memcpy( &(hdr->allState[tchunk]), refhdr->allState, 
			reftlen*sizeof(*hdr->allState) );
	      }

	    }
	    
	    /* increment tchunk */
	    tchunk += reftlen;
	  }

	  /* Close the file we had open */
	  smf_close_file( &refdata, status );
	}
	
      }
    }

    /* Put this concatenated subarray into the smfArray */
    smf_addto_smfArray( *concat, data, status );

  }
}

