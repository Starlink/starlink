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
*     smf_model_create( const smfGroup *igroup, smf_modeltype mtype, 
*                       smfGroup **mgroup, int *status);

*  Arguments:
*     igroup = const smfGroup * (Given)
*        NDG group identifier for input template files
*     mtype = smf_modeltype (Given)
*        Type of model component to create
*     mgroup = smfGroup ** (Returned)
*        Pointer to smfGroup pointer that will contain model file names
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a group of input (template) data files, this routine creates
*     new NDF files with dimensions appropriate for the model parameters.
*     For example, a common-model signal is represented by a 1-dimensional 
*     array as a function of time. The names of the containers are the same as
*     the input templated, with a suffix added.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-06 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Propagate inputs to residual, create others with sm_open_newfile 
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

/* General includes */
//#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_model_create"

void smf_model_create( const smfGroup *igroup, smf_modeltype mtype, 
		       smfGroup **mgroup, int *status) {

  /* Local Variables */
  int added=0;                  /* Number of names added to group */
  void *buf=NULL;               /* Pointer to total container buffer */
  int copyinput=0;              /* If set, container is copy of input */
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
  int indf=0;                   /* NDF ID for propagation */
  int isize=0;                  /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  Grp *mgrp=NULL;               /* Temporary group to hold model names */
  char *mname=NULL;             /* String model component name */
  int mndf=0;                   /* NDF ID for propagation */
  int msize=0;                  /* Number of files in model group */
  char name[GRP__SZNAM+1];      /* Name of container file without suffix */
  size_t ndata=0;               /* Number of elements in data array */
  int nmap=0;                   /* Number of elements mapped */
  long pagesize=0;              /* Size of memory page used by mmap */
  char *pname=NULL;             /* Poiner to fname */
  long remainder=0;             /* Extra length beyond integer pagesuze */
  char suffix[] = SMF__DIMM_SUFFIX; /* String containing model suffix */
  smfData *tempdata=NULL;       /* Temporary smfData pointer */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get size of the input group */
  grpGrpsz( igroup->grp, &isize, status );

  /* Create group of NDF names with model name suffix */
  mgrp = grpNew( "model component", status );
  mname = smf_model_getname( mtype, status );

  /* Form a group expression for the filename */

  if( *status == SAI__OK ) {
    sprintf( fname_grpex, "*_");
    strncat( fname_grpex, mname, sizeof(mname) );
    strncat( fname_grpex, suffix, sizeof(suffix) );
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

  *mgroup = smf_construct_smfGroup( mgrp, igroup->subgroups, igroup->ngroups,
				   igroup->nrelated, status );

  if( mgrp ) grpDelet( &mgrp, status );

  /* Loop over subgroups */
  for( i=0; i<(*mgroup)->ngroups; i++ ) {

    /* For models that only have one file per subgroup, fix up 
       mgroup such that only the first filename in each subgroup
       is used. Do this by setting remaining elements of mgroup->subgroups
       to 0. */

    if( mtype == SMF__COM ) {
      for( j=1; j<(*mgroup)->nrelated; j++ ) {
	(*mgroup)->subgroups[i][j] = 0;
      }
    }

    /* Loop over elements of subgroup */
    for( j=0; j<(*mgroup)->nrelated; j++ ) {
    
      /* obtain grp idx for j'th element of i'th subgroup */
      idx=(*mgroup)->subgroups[i][j];
      
      /* Only continue if there is a valid idx */
      if( idx > 0 ) {

	/* Open the template file */
	smf_open_file( igroup->grp, idx, "READ", 0, &idata, status );
    
	/* Check that the template is time-ordered data */
	if( *status == SAI__OK ) {
	  if( idata->ndims != 3 ) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, "Input file is not time-ordered data!", 
		   status);      
	  }
	  
	  if( idata->dtype != SMF__DOUBLE ) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, 
		   "Input file does not contain double precision data!",
		   status);      
	  }
	}
	
	if( *status == SAI__OK ) {
	  
	  /* initialzie the header */
	  
	  memset( &head, 0, sizeof(head) );
	  head.dtype=SMF__NULL;
	  
	  /* Determine dimensions of model component */
	  
	  switch( mtype ) {
	    
	  case SMF__CUM: /* Cumulative model */
	    copyinput = 0;
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;
	    
	  case SMF__RES: /* Model residual */
	    copyinput = 1;
	    break;
	    
	  case SMF__AST: /* Time-domain projection of map */
	    copyinput = 0;
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;
	    
	  case SMF__COM: /* Common-mode at each time step */
	    copyinput = 0;
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 1;
	    head.dims[0] = (idata->dims)[2];
	    break;
	
	  case SMF__NOI: /* Noise model */
	    copyinput = 0;
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  case SMF__EXT: /* Extinction correction - gain for each bolo/time */
	    copyinput = 0;
	    head.dtype = SMF__DOUBLE;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;

	  case SMF__LUT: /* Pointing LookUp Table for each data point */
	    copyinput = 0;
	    head.dtype = SMF__INTEGER;
	    head.ndims = 3;
	    head.dims[0] = (idata->dims)[0];
	    head.dims[1] = (idata->dims)[1];
	    head.dims[2] = (idata->dims)[2];
	    break;
	  }

	  /* Propagate information from template if copying */

	  if( copyinput ) { /* If copying input, copy data dims directly */
	    head.dtype = idata->dtype; /* Inherit data type from template */
	    head.ndims = idata->ndims;
	    for( k=0; k<head.ndims; k++ ) {
	      head.dims[k] = (idata->dims)[k];
	    }
	  } 

	  /* Calculate the size of the data buffer. Format:

	  Header:
	  smfData with only dtype, ndims and dims defined

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

	  /* Obtain a character string corresponding to the file name */
	  pname = name;
	  grpGet( (*mgroup)->grp, idx, 1, &pname, GRP__SZNAM, status );

	  /* Create the model container */
	  if( (fd = open( name, O_RDWR | O_CREAT | O_TRUNC, 
			  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH )) == -1 ) {
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME, "Unable to open model container file", status );
	  }
      
	  buf = smf_malloc( headlen+datalen, 1, 0, status );

	  if( *status == SAI__OK ) {

	    headptr = buf;
	    dataptr = buf + headlen;

	    /* Write the header. memset to 0 first since much of this space is
	       padding to make it a multiple of the page size */
	    memset( headptr, 0, headlen );
	    memcpy( headptr, &head, sizeof(head) );

	    /* Initialize the data buffer */
	    if( copyinput ) {
	      /* memcpy because target and source are same type */
	      memcpy( dataptr, (idata->pntr)[0], datalen );

	    } else {
	      /* otherwise zero the buffer */
	      memset( dataptr, 0, datalen );
	    }

	    /* If this is a LUT try to open the LUT extension of the
	       template and copy it over */
	    if( mtype == SMF__LUT ) {	  
	      smf_open_mapcoord( idata, status );
	  
	      if( *status == SAI__OK ) {
		/* memcpy because target and source are same type */
		memcpy( dataptr, idata->lut, datalen );
	      }
	    }
	  }

	  /* Write buffer and close the container */

	  if( *status == SAI__OK ) {

	    if( write( fd, buf, headlen+datalen ) == -1 ) {
	      *status = SAI__ERROR;
	      errRep( FUNC_NAME, "Unable to write model container file", 
		      status ); 
	    }
	
	    if( close( fd ) == -1 ) {
	      *status = SAI__ERROR;
	      errRep( FUNC_NAME, "Unable to close model container file", 
		      status ); 
	    }
	  }

	  /* Close the input template file */
	  smf_close_file( &idata, status );
	}
      }

      /* Set main loop exit condition if bad status was set */
      if( *status != SAI__OK ) {
	i = (*mgroup)->ngroups;
	j = (*mgroup)->nrelated;
      }
    }
  }
}
