/*
*+
*  Name:
*     smf_NDFexport

*  Purpose:
*     Export smfData to NDF file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_NDFexport( const smfData *data, void *variance, 
*                    unsigned char *quality, smfHead *hdr,
*                    const char *name, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Input smfData (any variety) that will have its DATA array copied.
*     variance = void * (Given)
*        If set, use this buffer instead of VARIANCE associated with data.
*     quality = unsigned char * (Given)
*        If set, use this buffer instead of QUALITY associated with data.
*     hdr = smfHead * (Given)
*        If set use this header instead of data->hdr.
*     name = const char* (Given)
*        Name of the NDF container
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a new NDF container with the same
*     dimensions as the input. The following components are
*     propagated: DATA, QUALITY, VARIANCE, HISTORY, FITS headers,
*     JCMTSTATE headers, WCS
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-06-15 (EC):
*        Initial version.
*     2008-04-03 (EC):
*        Asset ICD data-order before writing file.
*     2008-04-14 (EC):
*        Write QUALITY and VARIANCE if present.
*     2008-04-15 (EC):
*        Add ability to supply external VARIANCE & QUALITY arrays
*     2008-04-17 (EC):
*        Changed time-ordered data assertion to check to force caller to
*        order the data themselves before calling the routine.
*     2008-04-23 (EC):
*        Propagate header information to file
*     2008-04-24 (EC):
*        Fixed axis index bug in 1-d frameset case using patch from DB
*     2008-06-11 (EC):
*        Renamed to smf_NDFexport from smf_model_NDFexport
*     2008-07-23 (EC):
*        Support exportation of FFT'd data
*     2008-07-29 (EC):
*        Write useful WCS for FFT'd data

*  Notes:
*
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
#include "jcmt/state.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_NDFexport"

void smf_NDFexport( const smfData *data, void *variance, 
			  unsigned char *quality, smfHead *hdr, 
			  const char *name, int *status ){

  /* Local Variables */
  int added=0;                  /* Number of names added to group */
  AstMapping *cbmap=NULL;       /* Pointer to current->base mapping */
  size_t datalen=0;             /* Length in bytes of data array */
  int flag=0;                   /* Flag */
  AstFrameSet *fset1d=NULL;    	/* Frameset for 1D data */
  size_t i;                     /* Counter */
  int fft;                      /* Flag for FFT data */
  HDSLoc *jcmtstateloc=NULL;    /* HDS Locator for JCMT headers */
  int lbnd[NDF__MXDIM];         /* Dimensions of container */
  Grp *inname=NULL;             /* 1-element group to hold input filename */
  size_t msize=0;               /* Number of files in name group */
  dim_t nbolo;                  /* Number of bolometers */
  size_t ndata=0;               /* Number of elements in data array */
  dim_t nf=0;                   /* Number of frequencies if FFT */
  dim_t ntslice=0;              /* Number of time slices */
  int out[NDF__MXDIM];          /* Indices outputs of mapping */
  Grp *outname = NULL;          /* 1-element group to hold output filename */
  unsigned char *qual=NULL;     /* Pointer to QUALITY buffer */
  smfHead *header=NULL;         /* Pointer to header data */  
  int taxis;                    /* Index of time axis */
  smfData *tempdata=NULL;       /* Temporary smfData pointer */
  AstFrame *tfrm=NULL;          /* 1D frame (TimeFrame) */
  AstMapping *tmap=NULL;        /* Mapping for time axis */
  int ubnd[NDF__MXDIM];         /* Dimensions of container */
  void *var=NULL;               /* Pointer to VARIANCE buffer */

  if (*status != SAI__OK ) return;

  fft = smf_isfft( data, NULL, status );

  /* Check for ICD-compliant data order */
  if( !fft && !data->isTordered ) {
    *status = SMF__BORDB;
    errRep( FUNC_NAME, "Data is bolo-ordered, must be time-ordered", status );
    return;
  }

  /* Get number of bolos / time slices */
  if( data->ndims == 1 ) {
    /* Assume one time-domain axis */
    nbolo = 1;
    ntslice = data->dims[0];
  } else if( data->ndims == 3 ) {
    /* Assume 3-d cube of data */
    nbolo = data->dims[0]*data->dims[1];
    ntslice = data->dims[2];
  } else if ( (data->ndims == 2) && fft ) {
    nbolo = 1;
    nf = data->dims[0];
    ntslice = (nf-1)*2;
  } else if ( (data->ndims == 4) && fft ) {
    nbolo = data->dims[1]*data->dims[2];
    nf = data->dims[0];
    ntslice = (nf-1)*2;
  } else {
    *status = SAI__ERROR;
    msgSeti("NDIMS",data->ndims);
    errRep( FUNC_NAME, "Don't know how to export ^NDIMS dimensional data",
	    status );
  }

  /* Make a 1-element group containing the name of the new file */
  inname = grpNew( "GRP", status );
  outname = grpNew( "GRP", status );
  grpPut1( inname, name, 1, status );
  grpGrpex( "*|dimm|sdf|", inname, outname, &msize, &added, &flag, status );

  /* Create lbnd and ubnd arrays, and calculate buffer size */
  if( *status == SAI__OK ) {
    ndata = 1;
    for( i=0; i<data->ndims; i++ ) {
      lbnd[i] = 1;
      ubnd[i] = data->dims[i];
      ndata *= data->dims[i];
    }

    datalen = ndata * smf_dtype_sz( data->dtype, status );
  }

  /* Check for VARIANCE and QUALITY components, and header */
  if( variance ) {
    var = variance;
  } else {
    var = (data->pntr)[1];    
  }
    
  if( quality ) {
    qual = quality;
  } else {
    qual = (data->pntr)[2];
  }

  if( hdr ) {
    header = hdr;
  } else {
    header = data->hdr;
  }

  /* Make a new empty container with associated smfData struct */
  flag = 0;
  if( var ) flag |= SMF__MAP_VAR;
  if( qual ) flag |= SMF__MAP_QUAL;

  smf_open_newfile( outname, 1, data->dtype, data->ndims, lbnd, ubnd, 
		    flag, &tempdata, status );

  /* Copy the data/variance/quality array to new smfData */
  if( *status == SAI__OK ) {
    memcpy( (tempdata->pntr)[0], (data->pntr)[0], datalen );

    if( var ) {
      memcpy( (tempdata->pntr)[1], var, datalen );
    }

    if( qual ) {
      memcpy( (tempdata->pntr)[2], qual, ndata*sizeof(*qual) );
    }
  }

  /* If header was provided, create and map relevant information */
  if( header && (tempdata->file) ) {
    
    if( header->instrument == INST__SCUBA2 ) {
      
      /* MORE.JCMTSTATE */
      if( header->allState ) {
	
	/* Get an HDS locator */
	ndfXnew( tempdata->file->ndfid, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0,
		 &jcmtstateloc, status );

	/* Map the header */
	sc2store_headcremap( jcmtstateloc, ntslice, INST__SCUBA2, status  );

	/* Write out the per-frame headers */
	for( i=0; i<ntslice; i++ ) {
	  sc2store_headput( i, header->allState[i], status );
	}
      }

      /* MORE.FITS */
      if( header->fitshdr ) {
	kpgPtfts( tempdata->file->ndfid, header->fitshdr, status );
      }

      /* WCS */
      if( header->tswcs ) {
	if( data->ndims == 3 ) {
	  /* For 3-dimensional data assume it is ICD bolo-format */
	  ndfPtwcs( header->tswcs, tempdata->file->ndfid, status );

	} else if( smf_isfft(data, NULL, status) ) {
          /* Data is FFT */
          if( data->ndims == 4 ) {
            /* Only handle 4d FFT data at the moment */
            ndfPtwcs( header->tswcs, tempdata->file->ndfid, status );
          }

        } else if( data->ndims == 1 ) {
	  /* For 1=dimensional data, assume it is the time axis which we
	     extract from the 3d WCS */

	  astBegin;

	  /* Get a pointer to the current->base Mapping (i.e. the Mapping from
	     WCS coords to GRID coords). */
	  cbmap = astGetMapping( header->tswcs, AST__CURRENT, AST__BASE );

	  /* Use astMapSplit to split off the Mapping for the time
	     axis. This assumes that the time axis is the 3rd axis
	     (i.e. index 2) */

	  taxis = 3;
	  astMapSplit( cbmap, 1, &taxis, out, &tmap );

	  /* We now check that the Mapping was split succesfully. This should
	     always be the case for the time axis since the time axis is 
	     independent of the others, but it is as well to check in case of 
	     bugs, etc. */
	  if( !tmap ) {
	    /* The "tmap" mapping will have 1 input (the WCS time value) -
	       astMapSplit guarantees this. But we
	       should also check that it also has only one output (the
	       corresponding GRID axis). */
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME, "Couldn't extract time-axis mapping",
		    status );
	  } else if( astGetI( tmap, "Nout" ) != 1 ) {
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME, 
		    "Time-axis mapping has incorrect number of outputs",
		    status );
	  } else {

	    /* Create a new FrameSet containing a 1D GRID Frame. */
	    fset1d = astFrameSet( astFrame( 1, "Domain=GRID" ), "" );

	    /* Extract the 1D Frame (presumably a TimeFrame)
	       describing time from the current (WCS) 3D Frame. */
	    tfrm = astPickAxes( header->tswcs, 1, &taxis, NULL);

	    /* Add the time frame into the 1D FrameSet, using the
	       Mapping returned by astMapSplit. Note, this Mapping
	       goes from time to grid, so we invert it first so that
	       it goes from grid to time, as required by
	       astAddFrame. */

	    astInvert( tmap );
	    astAddFrame( fset1d, AST__BASE, tmap, tfrm );
	  }

	  if( astOK && *status==SAI__OK ) {
	    /* Write out the frameset */
	    ndfPtwcs( fset1d, tempdata->file->ndfid, status );
	  } else {
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME, "Error extracting 1-d frameset",
		    status );
	  }

	  astEnd;

	} else {
	  msgSeti("NDIMS",data->ndims);
	  msgOut( " ", "SMF_NDFEXPORT: Don't know how to handle WCS for ^NDIMS dimensions", status );
	}
      }

    } else {
      msgOut(" ",
	     "SMF_NDFEXPORT: Warning: only know how to write SCUBA2 header", status );
    }

  }


  /* Close files and clean up */  
  smf_close_file( &tempdata, status );
  grpDelet( &inname, status );
  grpDelet( &outname, status );
}
