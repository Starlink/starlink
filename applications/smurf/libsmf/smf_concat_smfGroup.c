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
*     smf_concat_smfGroup( smfGroup *igrp, size_t whichchunk, int isTordered, 
*			  AstFrameSet *outfset, int moving, 
*	        	  int *lbnd_out, int *ubnd_out, int flags,
*			  smfArray **concat, int *status )


*  Arguments:
*     igrp = smfGroup* (Given)
*        Group of input data files
*     whichchunk = size_t (Given)
*        Which continuous subset of igrp will get concatenated?
*     isTordered = int (Given)
*        If 0, ensure concatenated data is ordered by bolometer. If 1 ensure 
*        concatenated data is ordered by time slice (default ICD ordering)
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping if calculating
*        pointing LUT on-the-fly
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*        (if outfset specified) 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*        (if outfset specified) 
*     flags = int (Given)
*        Additional flags to control processing of individual data files
*        as they are being concatenated.
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
*     than time-ordered if desired. If a pointing LUT is to be calculated
*     as data is being loaded, specify outfset, moving, lbnd_out and
*     ubnd_out. Otherwise set outfset to NULL.
*     
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-10-19 (EC):
*        Initial version.
*     2007-10-29 (EC):
*        -Fixed loop bounds for addressing DATA/VARIANCE/QUALITY memory
*        -Inserted Ast status check after copying FITS headers
*        -Fixed bug in reference file dimensions 
*        -Modified interface to smf_open_file.
*     2007-11-15 (EC):
*        -Added projection information, flags and isTordered to interface.
*        -With projection info pointing LUT can now be calculated on-the-fly
*     2007-11-28 (EC):
*        -Use smf_open_file with SMF__NOCREATE_DATA for first pass
*        -Set bad status if input is not ICD-compliant time-ordered data
*        -Fixed bug in time-axis length which depends on output data order
*     2007-12-14 (EC):
*        -close reference data
*        -modified smf_calc_mapcoord interface
*        -properly set isTordered flag
*     2008-01-25 (EC):
*        -use smf_open_and_flatfield in case input data is raw
*        -store name of first file of subarray with "_con" suffix
*     2008-02-08 (EC):
*        -Fixed data type for QUALITY
*        -generate QUALITY array if not present
*        -Use SMF__NOCREATE* flags
*     2008-04-16 (EC):
*        -added chunking based on time stamps
*     2008-04-17 (EC):
*        -fixed calculation of number of subarrays
*     2008-04-23 (EC):
*        -propagate time series WCS

*  Notes:
*     If projection information supplied, pointing LUT will not be
*     concatenated if SMF__NOCREATE_LUT is specified. By default, a
*     QUALITY array is created even if one is not present in the
*     template file. This behaviour can be avoided by setting flag bit
*     SMF__NOCREATE_QUALITY. Additionally, if VARIANCE and/or QUALITY
*     is present in the template, prevent propagation to the
*     concatenated file by setting SMF__NOCREATE_VARIANCE /
*     SMF__NOCREATE_QUALITY.

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
#include "kpg_err.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libaztec/aztec.h"

#define FUNC_NAME "smf_concat_smfGroup"

void smf_concat_smfGroup( smfGroup *igrp, size_t whichchunk, int isTordered, 
			  AstFrameSet *outfset, int moving, 
			  int *lbnd_out, int *ubnd_out, int flags,
			  smfArray **concat, int *status ) {

  /* Local Variables */
  smfData *data=NULL;           /* Concatenated smfData */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  int firstpiece;               /* index to start of whichchunk */
  int havearray[3];             /* flags for DATA/QUALITY/VARIANCE present */
  int havelut;                  /* flag for pointing LUT present */
  smfHead *hdr;                 /* pointer to smfHead in concat data */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  dim_t l;                      /* Loop counter */
  int lastpiece;                /* index to end of whichchunk */
  dim_t nbolo;                  /* Number of detectors */
  dim_t ndata;                  /* Total data points: nbolo*tlen */
  int nrelated;                 /* Number of subarrays */
  int pass;                     /* Two passes over list of input files */
  char *pname;                  /* Pointer to input filename */
  smfData *refdata=NULL;        /* Reference smfData */
  int refdims[2];               /* reference dimensions for array (not time) */
  smf_dtype refdtype;           /* reference DATA/VARIANCE type */
  char *refdtypestr;            /* const string for reference data type */
  smfHead *refhdr=NULL;         /* pointer to smfHead in ref data */
  dim_t refndata;               /* Number data points in reference file */
  dim_t reftlen;                /* Number of time slices in reference file */
  int refTordered;              /* Data order of reference file */
  dim_t tchunk;                 /* Time offset in concat. array this chunk */
  dim_t tlen;                   /* Time length entire concatenated array */
  dim_t sz;                     /* Data type size */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Verify that we have a valid whichchunk, and determine the range of
     indices into igrp->chunk */
  if( (whichchunk < 0) || (whichchunk > igrp->chunk[igrp->ngroups-1] ) ) {
    msgSeti( "WHICHCHUNK", whichchunk );
    msgSeti( "MAXCHUNK", igrp->chunk[igrp->ngroups-1] );
    *status = SAI__ERROR;
    errRep( FUNC_NAME, 
	    "Invalid whichchunk: ^WHICHCHUNK. Must be 0 - ^MAXCHUNK", 
	    status );
  } else {
    /* Find the range of indices */
    firstpiece=-1;
    lastpiece=-1;
    
    for( i=0; i<(dim_t)igrp->ngroups; i++ ) {
      if( (igrp->chunk[i] == whichchunk) && (firstpiece == -1) ) {
	firstpiece = i;
      }

      if( igrp->chunk[i] == whichchunk ) {
	lastpiece = i;
      }
    }
  }

  /* Allocate space for the smfArray */
  *concat = smf_create_smfArray( status );

  /* Determine how many subarrays there actually are in this chunk*/
  nrelated = 0;
  for( i=0; i<igrp->nrelated; i++ ) {
    for( j=firstpiece; j<=lastpiece; j++ ) {
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
      
      /* Loop over subgroups (number of time chunks), continuing only
	 if the chunk is equal to whichchunk */
      for( j=firstpiece; j<=lastpiece; j++ ) {

	/* First pass - get dimensions */
	if( pass == 0 ) {

	  smf_open_file( igrp->grp, igrp->subgroups[j][i], "READ", 
			 flags, &refdata, status );

	  /* Verify that the array is 3-dimensional and compatible with the
	     reference array dimensions. */

	  if( *status == SAI__OK ) {
	    msgSetc( "FILE", refdata->file->name );

	    if( refdata->ndims != 3 ) {
	      *status = SAI__ERROR;
	      errRep( FUNC_NAME, "^FILE does not contain 3-dimensional data!", 
		      status );
	    }
	  }

	  /* If data order is 0 (bolo-ordered) then fail since that case
             is not currently handled. */

	  if( (*status == SAI__OK) && (refdata->isTordered == 0) ) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, "^FILE contains bolo-ordered data (unsupported)",
	  	   status);
	  }

	  if( *status == SAI__OK ) {
	    if( j == firstpiece ) {
	      /* If this is the first chunk we will use it for refdims
                 - check the number of bolometers! (Assumption is that
                 input data is standard ICD-compliant time-ordered
                 data) */

	      refdims[0] = refdata->dims[0];
	      refdims[1] = refdata->dims[1];
	      nbolo = refdims[0]*refdims[1];

	      /* Check for DATA/VARIANCE/QUALITY and data type */
	      for( k=0; k<3; k++ ) {
		havearray[k] = (refdata->pntr[k] != NULL);
	      }

	      /* Concatenated data is always double precision */
	      refdtype = SMF__DOUBLE;
	      refdtypestr = smf_dtype_string(refdata, status);

	    } else {
	      /* Check these dims against refdims */
	      if( (refdata->dims[0] != refdims[0]) || 
		  (refdata->dims[1] != refdims[1]) ) {

	  	*status = SAI__ERROR;
	  	msgSeti( "XREF", refdims[0] );
	  	msgSeti( "YREF", refdims[1] );
	  	msgSeti( "X", refdata->dims[0] );
	  	msgSeti( "Y", refdata->dims[1] );
		
	  	errRep( FUNC_NAME, "Detector dimensions (^X,^Y) in ^FILE do not match reference (^XREF,^YREF)", status );
	      }
	      
	      /* Check existence of DATA/QUALITY/VARIANCE */

	      if( (refdata->pntr[0] != NULL) != havearray[0] ) {
		*status = SAI__ERROR;
		if( havearray[0] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component DATA", status );
	     }

	      if( (refdata->pntr[1] != NULL) != havearray[1] ) {
		*status = SAI__ERROR;
		if( havearray[1] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component VARIANCE", status );
	      }

	      if( (refdata->pntr[2] != NULL) != havearray[2] ) {
		*status = SAI__ERROR;
		if( havearray[2] ) msgSetc( "FLAG", "is missing" );
		else msgSetc( "FLAG", "has extra" );
		errRep( FUNC_NAME, "^FILE ^FLAG component QUALITY", status );
	      }
	    }
	  }

	  if( *status == SAI__OK ) {
	    /* At this stage increment tlen for this chunk */	  
	    tlen += refdata->dims[2];
	  }

	  /* Close the reference file */
	  smf_close_file( &refdata, status );

	}
      
      	/* Second pass copy data over to new array */
	if( (pass == 1) && (*status == SAI__OK) ) {

	  /* Open the file corresponding to this chunk. Data may
             require flat-fielding. */

	  smf_open_and_flatfield( igrp->grp, NULL, igrp->subgroups[j][i], 
				  &refdata, status );

	  /* Calculate the pointing LUT if requested */
	  if( !(flags & SMF__NOCREATE_LUT) && outfset ) {
	    
	    /* Set havelut flag */
	    havelut = 1;

	    /* Calculate the LUT for this chunk */

	    smf_calc_mapcoord( refdata, outfset, moving, lbnd_out, ubnd_out, 
			       SMF__NOCREATE_FILE, status );
	  }

	  /* Change data order if required */
	  smf_dataOrder( refdata, isTordered, status );

	  if( *status == SAI__OK ) {

	    /* If first chunk initialize the concatenated array */
	    if( j == firstpiece ) {
	      tchunk = 0;
 
	      /* Allocate memory for empty smfData with a smfHead */
	      data = smf_create_smfData( SMF__NOCREATE_DA, status );

	      if( *status == SAI__OK ) {
		/* Copy over basic header information from the reference */
		hdr = data->hdr;
		refhdr = refdata->hdr;	    
		hdr->instrument = refhdr->instrument;

		switch ( hdr->instrument ) {
		case INST__AZTEC:
		  aztec_fill_smfHead( hdr, NDF__NOID, status );
		  break;
		default:
		  break;
		  /* SCUBA-2 has nothing special here because the focal plane
		     coordinates are derived using an AST polyMap */
		}

		/* Copy over the name of the first file in subarray. Add
                   the suffix "_con" to denote concatenated data */

		pname = filename;
		grpGet( igrp->grp, igrp->subgroups[j][i], 1, &pname, 
			SMF_PATH_MAX, status);		
		strncpy( data->file->name, filename, SMF_PATH_MAX );
		strncat( data->file->name, "_con", SMF_PATH_MAX );

		/* Allocate space for the concatenated allState */
		hdr->nframes = tlen;
		hdr->allState = smf_malloc( tlen, sizeof(*(hdr->allState)), 
					    0, status );
		
		/* Allocate space in the smfData for DATA/VARAIANCE/QUALITY */
		if( isTordered ) {
		  data->dims[0] = refdims[0];
		  data->dims[1] = refdims[1];
		  data->dims[2] = tlen;
		} else {
		  data->dims[0] = tlen;
		  data->dims[1] = refdims[0];
		  data->dims[2] = refdims[1];
		}
		data->ndims = 3;
		
		/* Set the data type and order */
		data->dtype = refdtype;
		data->isTordered = isTordered;

		ndata = nbolo*tlen;

		/* Un-set havearray values corresponding to flags */
		havearray[0] = havearray[0] && !(flags&SMF__NOCREATE_DATA);
		havearray[1] = havearray[1] && !(flags&SMF__NOCREATE_VARIANCE);
		havearray[2] = havearray[2] && !(flags&SMF__NOCREATE_QUALITY);

		/* Allocate space for arrays being propagated from template */
		for( k=0; k<3; k++ ) if( havearray[k] ) {
		  if( k == 2 ) sz = smf_dtype_sz( SMF__UBYTE, status );
		  else sz = smf_dtype_sz(data->dtype, status );
		  data->pntr[k] = smf_malloc(ndata, sz, 0, status);
		}

		/* Check to see if havearray for QUALITY is not set,
                   but SMF__NOCREATE_QUALITY is also not set. In this
                   case, allocate a fresh QUALITY component that will
                   not require propagation from the template */
		
		if( !havearray[2] && !(flags & SMF__NOCREATE_QUALITY) ) {
		  data->pntr[2] = smf_malloc(ndata, 
					     smf_dtype_sz(SMF__UBYTE,status),
					     1, status);
		}

		/* Allocate space for the pointing LUT if needed */
		if( havelut ) {
		  data->lut = smf_malloc(ndata, sizeof(*(data->lut)), 0, 
					 status);
		}

		/* Copy over the FITS header */
		if( (*status == SAI__OK) && (refhdr->fitshdr) ) {
		  hdr->fitshdr = astCopy( refhdr->fitshdr );
		  if (!astOK) {
		    if (*status == SAI__OK) {
		      *status = SAI__ERROR;
		      errRep( FUNC_NAME, "AST error copying FITS header", 
			      status);
		    }
		  }
		}

		/* Copy over the TSWCS */
		if( (*status == SAI__OK) && (refhdr->tswcs) ) {
		  hdr->tswcs = astCopy( refhdr->tswcs );
		  if (!astOK) {
		    if (*status == SAI__OK) {
		      *status = SAI__ERROR;
		      errRep( FUNC_NAME, 
			      "AST error copying time series WCS", 
			      status);
		    }
		  }
		}		

	      }
	    }

	    /* Copy DATA/QUALITY/VARIANCE and JCMTstate information into
               concatenated smfData */

	    if( *status == SAI__OK ) {

	      /* Which dimension contains reference time slices depends on
		 ordering */
	      if( isTordered ) {
		reftlen = refdata->dims[2]; 
	      } else {
		reftlen = refdata->dims[0];
	      }

	      refndata = reftlen*nbolo;

	      /* Copy over JCMTstate */
	      hdr = data->hdr;
	      refhdr = refdata->hdr;	    
	      
	      memcpy( &(hdr->allState[tchunk]), refhdr->allState, 
		      reftlen*sizeof(*hdr->allState) );

	      /* Copy LUT */

	      if( havelut ) {
		sz = sizeof( *(refdata->lut) );
		if( isTordered ) {
		  /* If concatenating time-ordered data just copy entire
		     chunk over at once */

		  memcpy( (char *)data->lut + tchunk*nbolo*sz,
			  refdata->lut, refndata*sz );
		} else {
		  /* If concatenating bolo-ordered data need to copy
		     one chunk of bolometer data over at a time */
		  
		  for( l=0; l<nbolo; l++ ) {
		    memcpy( (char *)data->lut + l*tlen*sz + tchunk*sz,
			    (char *)refdata->lut + l*reftlen*sz,
			    reftlen*sz );
		  }
		}
	      }

	      /* Now do DATA/QUALITY/VARIANCE */
	      for( k=0; k<3; k++ ) if( havearray[k] ) {
		if( k == 2 ) sz = smf_dtype_sz( SMF__UBYTE, status );
		else sz = smf_dtype_sz(data->dtype, status );
	      
		if( *status == SAI__OK ) {

		  if( isTordered ) {
		    /* If concatenating time-ordered data just copy entire
		       chunk over at once */

		    memcpy( (char *)data->pntr[k] + tchunk*nbolo*sz,
			    refdata->pntr[k], refndata*sz );

		  } else {
		    /* If concatenating bolo-ordered data need to copy
                       one chunk of bolometer data over at a time */

		    for( l=0; l<nbolo; l++ ) {
		      memcpy( (char *)data->pntr[k] + l*tlen*sz + tchunk*sz,
			      (char *)refdata->pntr[k] + l*reftlen*sz,
			      reftlen*sz );
		    }

		  }
		}

	      }
	    
	      /* increment tchunk */
	      tchunk += reftlen;
	    }
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

