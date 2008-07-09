/*
*+
*  Name:
*     smf_model_create

*  Purpose:
*     Create group of containers for iterative map-maker model components

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_model_create( const smfGroup *igroup, const smfArray **iarray,
*                       dim_t nchunks, smf_modeltype mtype, int isTordered, 
*		        AstFrameSet *outfset, int moving, 
*		        int *lbnd_out, int *ubnd_out,
*                       smfGroup **mgroup, int nofile, int leaveopen,
*                       smfArray **mdata, int *status);

*  Arguments:
*     igroup = const smfGroup * (Given)
*        NDG group identifier for input template files
*     iarray = const smfArray ** (Given)
*        If igroup unspecified, use an array of smfArrays as the template
*        instead. In this case nchunks must also be specified.
*     nchunks = dim_t (Given)
*        If iarray specified instead of igroup, nchunks gives number of
*        smfArrays in iarray (otherwise it is derived from igroup).
*     mtype = smf_modeltype (Given)
*        Type of model component to create
*     isTordered = int (Given)
*        If 0, ensure template data is ordered by bolometer. If 1 ensure 
*        template data is ordered by time slice (default ICD ordering). 
*        Ignored if not creating SMF__LUT.
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping if calculating
*        pointing LUT on-the-fly. Ignored if not creating SMF__LUT.
*     moving = int (Given)
*        Is coordinate system tracking moving object? (if outfset specified)
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*        (if outfset specified) 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*        (if outfset specified) 
*     mgroup = smfGroup ** (Returned)
*        Pointer to smfGroup pointer that will contain model file names
*     nofile = int (Given)
*        If set don't create a file on disk - just create the smfArray. In
*        this case leaveopen is implied (and overrides user supplied value)
*     leaveopen = int (Given)
*        If true, don't close files once created and store in mdata
*     mdata = smfArray ** (Given and Returned)
*        Container to store data if leaveopen is set: array of
*        smfArray pointers. The top-level array must already be
*        allocated (same number of elements as ngroups in igroup), but
*        the individual smfArrays get allocated here.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a group of input (template) data files, this routine
*     creates new NDF files with dimensions appropriate for the model
*     parameters.  For example, a common-model signal is represented
*     by a 1-dimensional array as a function of time. The names of the
*     containers are the same as the input template, with a suffix
*     added. The containers can be stored in smfArrays if leavelopen
*     is set. In this case it is up to the caller to first generate an
*     array of smfArray pointers (mdata) which then get new smfArrays
*     assigned to them. In this case it is up to the caller to close
*     the smfArrays. If a SMF__LUT component is being calculated, the
*     projection information must be supplied: outfset, moving and ?bnd_out.

*  Notes:
*     QUAlity components are initialized to 0. Before using the caller
*     should synchronize the SMF__Q_BADS bits with smf_update_quality.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-07-06 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Propagate inputs to residual, create others with smf_open_newfile 
*     2007-02-07 (EC):
*        - Simplified container files.
*        - In copyinput case map data array so that it gets copied.
*     2007-02-12 (EC):
*        Now form the grpex here for the model container names
*     2007-03-02 (EC):
*        - Map variance to ensure creation for RESidual container
*        - Set initial variance to 1 
*     2007-06-13 (EC):
*        - Use new DIMM binary file format 
*     2007-06-25 (EC)
*        Header length is now static / padded to multiple of pagesize 
*     2007-07-10 (EC):
*        Use smfGroups & smfArrays instead of groups and smfDatas
*     2007-07-13 (EC):
*        Only create one smfData per subgroup for SMF__COM models
*     2007-07-16 (EC):
*        -Changed smf_construct_smfGroup interface
*     2007-08-09 (EC):
*        -use mmap rather than fwrite for creation
*        -option to leave files open, store in smfArrays
*     2007-08-17 (EC):
*        Added nofile flag
*     2007-08-21 (EC):
*        Fixed up warnings caused by ambiguous pointer math
*     2007-11-15 (EC):
*        -Added ability to create models from a smfArray template, 
*         requiring a change to the interface.
*        -Fixed memory allocation bug
*     2007-11-28 (EC):
*        -Added ability to assert the dataOrder (isTordered parameter)
*     2007-12-14 (EC):
*        -template is now loaded into refdata and copies to idata
*        -properly set isTordered in created smfData
*        -don't unmap the header portion of the model in DIMM files
*     2008-01-24 (EC):
*        -Template can now be non-flatfielded.
*        -Better file names for models based on iarray
*     2008-03-03 (EC):
*        -handle QUAlity
*     2008-04-16 (EC):
*        -added chunk to smfGroup
*     2008-04-17 (EC):
*        -improved initialization for SMF__NOI and SMF__QUA
*     2008-04-30 (EC):
*        -Calculate and store extinction coefficients for SMF__EXT
*     2008-06-24 (TIMJ):
*        - const mname since smf_model_getname now returns const
*     2008-07-03 (EC):
*        Changed nchunks to dim_t
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

/* Need ftruncate to be prototyped */
#define _POSIX_C_SOURCE 200112L

/* General includes */
#include <sys/mman.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>

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

#define FUNC_NAME "smf_model_create"

void smf_model_create( const smfGroup *igroup, smfArray **iarray,
		       dim_t nchunks, smf_modeltype mtype, int isTordered,
		       AstFrameSet *outfset, int moving, 
		       int *lbnd_out, int *ubnd_out,
		       smfGroup **mgroup, int nofile, int leaveopen,
		       smfArray **mdata, int *status ) {

  /* Local Variables */
  int added=0;                  /* Number of names added to group */
  void *buf=NULL;               /* Pointer to total container buffer */
  int copyinput=0;              /* If set, container is copy of input */
  smfData *data = NULL;         /* Data struct for file */
  size_t datalen=0;             /* Size of data buffer in bytes */
  void *dataptr=NULL;           /* Pointer to data portion of buffer */
  int fd=0;                     /* File descriptor */
  int flag=0;                   /* Flag */
  char fname_grpex[GRP__SZNAM+1];/* String for holding filename grpex */
  smfData head;                 /* Header for the file */
  size_t headlen=0;             /* Size of header in bytes */ 
  void *headptr=NULL;           /* Pointer to header portion of buffer */
  dim_t i;                      /* Loop counter */
  smfData *idata=NULL;          /* Pointer to input smfdata data */
  int idx=0;                    /* Index within subgroup */
  int isize=0;                  /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  int k;                        /* Loop counter */
  dim_t l;                      /* Loop counter */
  Grp *mgrp=NULL;               /* Temporary group to hold model names */
  const char *mname=NULL;       /* String model component name */
  int msize=0;                  /* Number of files in model group */
  char name[GRP__SZNAM+1];      /* Name of container file without suffix */
  size_t ndata=0;               /* Number of elements in data array */
  dim_t nrel=0;                 /* Number of related elements (subarrays) */
  int oflag=0;                  /* Flags for opening template file */
  long pagesize=0;              /* Size of memory page used by mmap */
  char *pname=NULL;             /* Poiner to fname */
  long remainder=0;             /* Extra length beyond integer pagesize */
  char suffix[] = SMF__DIMM_SUFFIX; /* String containing model suffix */
  double tau;                   /* 225 GHz optical depth */
  dim_t thisnrel;               /* Number of related items for this model */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check to see if igroup or iarray is being used for template */
  if( igroup == NULL ) {
    if( iarray == NULL ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Neither igroup nor iarray specified", 
	     status);        
    } else if( nchunks <= 0 ) {
      *status = SAI__ERROR;
      msgSeti("NCHUNKS",nchunks);
      errRep(FUNC_NAME, 
	     "iarray specified but invalid number of chunks, ^NCHUNKS",
	     status);        
    } else {
      /* Since we're using smfArrays as template, no associated files */
      nofile = 1;

      /* We have at most SMF__MXSMF related objects (subarrays) at each time 
	 chunk */
      nrel = SMF__MXSMF;

      /* NULL mgroup since we won't be using it */
      mgroup = NULL;
    }
  } else {
    /* Get number of time chunks and related objects from igroup */
    nchunks = igroup->ngroups;
    nrel = igroup->nrelated;
  }

  /* If nofile is set, leaveopen=0 is meaningless */
  if( nofile ) leaveopen = 1;

  /* If using igroup as a template use group expressions to make filenames */
  if( igroup != NULL ) {
    /* Get size of the input group */
    grpGrpsz( igroup->grp, &isize, status );

    /* Create group of NDF names with model name suffix */
    mgrp = grpNew( "model component", status );
    mname = smf_model_getname( mtype, status );

    /* Form a group expression for the filename */
    if( *status == SAI__OK ) {
      strncpy( fname_grpex, "*_", GRP__SZNAM );
      strncat( fname_grpex, mname, GRP__SZNAM );
      strncat( fname_grpex, suffix, GRP__SZNAM );
    }

    grpGrpex( fname_grpex, igroup->grp, mgrp, &msize, &added, &flag, status );

    if( (*status == SAI__OK) && (msize != isize) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Couldn't create group of NDF model containers.", 
	     status);        
    }

    /* Now that we have the Grp of names, create a new smfGroup with the same
       grouping as igroup. mgroup has a copy of mgrp inside, so free up
       mgrp afterward. */

    *mgroup = smf_construct_smfGroup( mgrp, igroup->subgroups, igroup->chunk,
				      igroup->ngroups, igroup->nrelated, 1, 
				      status );

    if( mgrp ) grpDelet( &mgrp, status );
  }

  /* Check the mtype to decide how we should open the template files, and
     decide if we will propagate the template to the model file */

  oflag = 0;

  /* Only map head if creating LUT or EXT */
  if( (mtype != SMF__LUT) && (mtype != SMF__EXT) ) oflag |= SMF__NOCREATE_HEAD;

  if( mtype == SMF__RES ) {
    /* Propagate input if RES */
    copyinput = 1;
  } else {
    /* For all remaining types don't need data array */
    oflag |= SMF__NOCREATE_DATA;
  }
	
  /* Loop over time chunks */
  if( *status == SAI__OK ) for( i=0; i<nchunks; i++ ) {
    
    /* For models that only have one file per subgroup fix up 
       mgroup such that only the first filename in each subgroup
       is used. Do this by setting remaining elements of mgroup->subgroups
       to 0. nrel is 1. */

    if( mtype == SMF__COM ) {
      if (mgroup != NULL) {
	for( j=1; j<(*mgroup)->nrelated; j++ ) {
	  (*mgroup)->subgroups[i][j] = 0;
	}
      }

      nrel = 1;
    }

    /* Check to see how many related elements in this chunk */

    thisnrel = 0;

    for( j=0; j<nrel; j++ ) {
      /* Check mgroup if we're using igroup as a template */
      if( mgroup != NULL ) {

	/* Check for non-zero grp index at this position */
	if( (*mgroup)->subgroups[i][j] != 0 ) {
	  thisnrel = j+1;
	} else {
	  /* If grp index is 0 we've reached the end of the subarrays. Set
             the exit condition */
	  j=nrel;
	}
      }
    }

    if( mgroup == NULL ) {
      /* Otherwise just check the iarray */
      thisnrel = iarray[i]->ndat;

      /* Ensure that thisnrel isn't bigger than nrel */
      if( thisnrel > nrel ) {
	thisnrel = nrel;
      }
    }

    /* Loop over subarrays */
    for( j=0; j<thisnrel; j++ ) {
    
      /* Open the relevant template file if using igroup */
      if( igroup ) {
	
	/* obtain grp idx for j'th element of i'th subgroup */
	idx=(*mgroup)->subgroups[i][j];
	
	/* Only continue if there is a valid idx */
	if( idx > 0 ) {
	  
	  /* Open the template file - flags are set above depending
             on the type of model. If we're propagating input, then
             do an open_and_flatfield */

	  if( copyinput ) {
	    smf_open_and_flatfield( igroup->grp, NULL, idx, &idata, status );
	  } else {
	    smf_open_file( igroup->grp, idx, "READ", oflag, &idata, status );
	  }
	
	  /* Calculate the LUT if necessary */
	  
	  if( mtype == SMF__LUT ) {
	    smf_calc_mapcoord( idata, outfset, moving, lbnd_out, 
			       ubnd_out, SMF__NOCREATE_FILE, status );
	  }

	}

      } else {
	/* Otherwise obtain a pointer to the relevant smfData in the
	   template smfArray at this time chunk */
	idata = iarray[i]->sdata[j];
      }

      /* Assert the data order in the template */
      smf_dataOrder( idata, isTordered, status );

      /* Check that the template is time-varying data */

      if( *status == SAI__OK ) {
	if( idata->ndims != 3 ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Template data is not time-varying!", 
		 status);      
	}
	
      }
      
      if( *status == SAI__OK ) {
	  
	  /* initialize the header */
	  
	  memset( &head, 0, sizeof(head) );
	  head.dtype=SMF__NULL;
	  
	  /* Determine dimensions of model component */
	  
	  switch( mtype ) {
	    
	  case SMF__CUM: /* Cumulative model */
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;
	    
	  case SMF__RES: /* Model residual */
	    /* Nothing here since copyinput set */
	    break;
	    
	  case SMF__AST: /* Time-domain projection of map */
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;
	    
	  case SMF__COM: /* Common-mode at each time step */
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 1;

	    if( isTordered ) { /* T is 3rd axis if time-ordered */
	      head.dims[0] = (idata->dims)[2]; 
	    } else {           /* T is 1st axis if bolo-ordered */
	      head.dims[0] = (idata->dims)[0]; 
	    }
	    break;
	
	  case SMF__NOI: /* Noise model */
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  case SMF__EXT: /* Extinction correction - gain for each bolo/time */
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  case SMF__LUT: /* Pointing LookUp Table for each data point */
	    head.dtype = SMF__INTEGER;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  case SMF__QUA: /* Quality byte for each data point */
	    head.dtype = SMF__UBYTE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  default:
	    *status = SAI__ERROR;
	    msgSetc( "TYPE", smf_model_getname(mtype, status) );
	    errRep(FUNC_NAME, "Don't know how to handle model type ^TYPE",
		   status);
	  }

	  /* Propagate information from template if copying */
	  if( copyinput ) { /* If copying input, copy data dims directly */
	    head.dtype = idata->dtype; /* Inherit data type from template */
	    head.ndims = idata->ndims;
	    for( k=0; k<head.ndims; k++ ) {
	      head.dims[k] = (idata->dims)[k];
	    }
	  } 

	  /* Set the data-ordering flag in the header */
	  head.isTordered = idata->isTordered;

	  /* Calculate the size of the data buffer. Format:

	  Header:
	  smfData struct 

	  Data:
	  buf   = [smf_dtype] * dims[0] * dims[1] * ...
	  */

	  /* Header must fit into integer multiple of pagesize so that the 
	     data array starts on a page boundary (for later mmap) */
	  pagesize = sysconf(_SC_PAGESIZE);
	  headlen = sizeof(head);
	  remainder = headlen % pagesize;
	  if( remainder  ) headlen = headlen - remainder + pagesize;

	  /* Length of data array buffer */
	  ndata = 1;
	  for( k=0; k<head.ndims; k++ ) {
	    ndata *= head.dims[k];
	  }
	  datalen = ndata * smf_dtype_sz(head.dtype, status); 

	  if( mgroup != NULL ) {
	    /* Obtain a character string corresponding to the file name 
	       if we used a group as the template */
	    pname = name;
	    grpGet( (*mgroup)->grp, idx, 1, &pname, GRP__SZNAM, status );
	  } else {
	    /* Otherwise get the name from the smfArray */
	    mname = smf_model_getname( mtype, status );

	    strncpy( name, idata->file->name, GRP__SZNAM );
	    strncat( name, "_", GRP__SZNAM );
	    strncat( name, mname, GRP__SZNAM );
	  }

	  if( nofile ) {
	    /* If there is no file associated with the data, use smf_malloc 
	       to allocate memory but don't initialize since we do that
               later */

	    dataptr = smf_malloc( datalen, 1, 0, status );
	    
	  } else {
	    /* If we are writing a file create and map it here */
	    
	    if( (fd = open( name, O_RDWR | O_CREAT | O_TRUNC, 
			    S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH )) == -1 ) {
	      *status = SAI__ERROR;
	      errRep( FUNC_NAME, "Unable to open model container file", 
		      status );
	    }
	    
	    /* First truncate the file to make it the correct size, and then
	       map it (without the ftruncate bus errors are generated under
	       linux when the memory is subsequently accessed...) */

	    if( *status == SAI__OK ) {
	      if( ftruncate( fd, datalen+headlen ) == -1 ) {
		*status = SAI__ERROR;
		errRep( FUNC_NAME, "Unable to re-size container file", 
			status ); 
	      } else if( (buf = mmap( 0, datalen+headlen, 
				      PROT_READ | PROT_WRITE,
				      MAP_SHARED, fd, 0 ) ) == MAP_FAILED ) {
		*status = SAI__ERROR;
		errRep( FUNC_NAME, "Unable to map model container file", 
			status ); 
	      }
	    } 

	    if( *status == SAI__OK ) {
	      headptr = buf;
	      dataptr = (char *)buf + headlen;

	      /* Fill the header. memset to 0 first since much of this space is
		 padding to make it a multiple of the page size */
	      memset( headptr, 0, headlen );
	      memcpy( headptr, &head, sizeof(head) ); 
	    }
	  }

	  /* Initialize the data buffer */
	  if( *status == SAI__OK ) {
	    if( copyinput ) {
	      /* memcpy because target and source are same type */
	      memcpy( dataptr, (idata->pntr)[0], datalen );

	    } else if( mtype == SMF__LUT ) {
	      /* If this is a LUT copy it over from the template */
	      if( idata->lut ) {	  
		/* dataptr can be mmap'd or malloc'd memory */
		memcpy( dataptr, idata->lut, datalen );
	      } else {
		*status = SAI__ERROR;
		errRep(FUNC_NAME, "No LUT present in template for LUT model", 
		       status);      
	      }
	      
	    } else if( mtype == SMF__NOI ) {
	      /* If this is a NOI, set to 1 to avoid divide-by-zeros */
	      if( head.dtype == SMF__DOUBLE ) {
		for( l=0; l<ndata; l++ ) {
		  ((double *) dataptr)[l] = 1;
		}
	      } else {
		/* Generate error message if NOI is not double... */
		*status = SAI__ERROR;
		errRep(FUNC_NAME,
		       "Possible programming error. NOI should be DOUBLE.", 
		       status);
	      }
	      
	    } else if( mtype == SMF__QUA ) {
	      /* If this is a QUA, and quality available in template copy it */
	      if( (idata->pntr)[2] ) {
		memcpy( dataptr, (idata->pntr)[2], datalen );
	      }

	    } else if( mtype == SMF__EXT ) {
	      /* In this case run smf_correct_extinction on the input data
		 (with only the header mapped) and store the gain coefficients
		 in the model bufffer */
              
              smf_fits_getD( idata->hdr, "MEANWVM", &tau, status );
              smf_correct_extinction( idata, "CSOTAU", 1, tau, 
                                      (double *) dataptr, status );

	    } else {
	      /* otherwise zero the buffer */
	      memset( dataptr, 0, datalen );
	    }
	  }
	
	
	  if( *status == SAI__OK ) {
	  
	    /* If leaveopen set, pack the data into a smfArray */
	    if( leaveopen ) {
	      
	      /* If this is the first element of the subgroup create
		 the smfArray */
	      if( j == 0 ) {
		mdata[i] = smf_create_smfArray( status );
	      }
	      
	      /* Create a smfData for this element of the subgroup */
	      flag = SMF__NOCREATE_HEAD | SMF__NOCREATE_DA;

	      data = smf_create_smfData( flag, status );

	      if( *status == SAI__OK ) {
		data->isTordered = head.isTordered; 
		data->dtype = head.dtype;
		data->ndims = head.ndims;
		memcpy( data->dims, head.dims, sizeof( head.dims ) );
		
		/* Data pointer points to mmap'd memory AFTER HEADER */
		data->pntr[0] = dataptr;
		
		/* Store the file descriptor to enable us to unmap when we 
		   close */
		if( !nofile ) {
		  data->file->fd = fd;
		}
		
		/* Copy the DIMM filename into the smfFile. Even though
                   there may not be an associated file on disk we store
                   the name here in case we wish to export the data
                   to an NDF file at a later point. */
		strncpy( data->file->name, name, SMF_PATH_MAX );
		
		/* Add the smfData to the smfArray */
		smf_addto_smfArray( mdata[i], data, status );
	      }
	      
	    } else if (!nofile) {
	      
	      /* If leaveopen not set (and there is a file) write buffer to 
		 file and close container */
	      
	      if( msync( buf, headlen+datalen, MS_ASYNC ) == -1 ) {
		*status = SAI__ERROR;
		errRep( FUNC_NAME, "Unable to sync model container file", 
			status ); 
	      } else if( munmap( buf, headlen+datalen ) == -1 ) {
		*status = SAI__ERROR;
		errRep( FUNC_NAME, "Unable to unmap model container file", 
			status ); 
	      } else if( close( fd ) == -1 ) {
		*status = SAI__ERROR;
		errRep( FUNC_NAME, "Unable to close model container file", 
			status ); 
	      }
	    }
	  }
	  
	  /* Close the input template file if it was opened here */
	  if( igroup ) {
	    smf_close_file( &idata, status );
	  }
      }

      /* Set loop exit condition if bad status was set */
      if( *status != SAI__OK ) {
	j = thisnrel;
      }
    }
    
    /* Set loop exit condition if bad status was set */
    if( *status != SAI__OK ) {
      i = nchunks;
    }
  }
}


