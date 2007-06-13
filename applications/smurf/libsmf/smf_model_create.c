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
*     smf_model_create( Grp *igrp, smf_modeltype mtype, Grp **mgrp, 
*                       int *status);

*  Arguments:
*     igrp = const Grp * (Given)
*        NDG group identifier for input template files
*     mtype = smf_modeltype (Given)
*        Type of model component to create
*     mgrp = const Grp ** (Returned)
*        Pointer to NDG group identifier for model files
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

void smf_model_create( Grp *igrp, smf_modeltype mtype, Grp **mgrp, 
		       int *status) {

  /* Local Variables */
  int added=0;                  /* Number of names added to group */
  int copyinput=0;              /* If set, container is copy of input */
  dim_t dims[NDF__MXDIM];       /* Size of model dimensions */
  smf_dtype dtype=SMF__NULL;    /* Type of data stored in component */
  int flag=0;                   /* Flag */
  char fname_grpex[GRP__SZNAM+1];/* String for holding filename grpex */
  dim_t i;                      /* Loop counter */
  smfData *idata=NULL;          /* Pointer to input smfdata data */
  int indf=0;                   /* NDF ID for propagation */
  int isize=0;                  /* Number of files in input group */
  dim_t j;                      /* Loop counter */
  int lbnd[NDF__MXDIM];         /* Dimensions of container */
  void *mapptr[3]={NULL,NULL,NULL};/* Pointer to array of mapped components */
  char *mname=NULL;             /* String model component name */
  int mndf=0;                   /* NDF ID for propagation */
  int msize=0;                  /* Number of files in model group */
  int ndims=0;                  /* Number of dimensions in container */
  int nmap=0;                   /* Number of elements mapped */
  smfData *tempdata=NULL;       /* Temporary smfData pointer */
  int ubnd[NDF__MXDIM];         /* Dimensions of container */

  char name[GRP__SZNAM+1];      /* Name of container file without suffix */
  char *pname=NULL;             /* Poiner to fname */
  char suffix[] = SMF__DIMM_SUFFIX; /* String containing model suffix */

  void *buf=NULL;               /* Pointer to total container buffer */
  void *headptr=NULL;           /* Pointer to header portion of buffer */
  void *dataptr=NULL;           /* Pointer to data portion of buffer */
  size_t headlen=0;             /* Size of header in bytes */ 
  size_t datalen=0;             /* Size of data buffer in bytes */
  size_t ndata=0;               /* Number of elements in data array */
  int fd=0;                     /* File descriptor */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get size of the input group */
  grpGrpsz( igrp, &isize, status );

  /* Create group of NDF names with model name suffix */
  *mgrp = grpNew( "model component", status );
  mname = smf_model_getname( mtype, status );

  /* Form a group expression for the filename */

  if( *status == SAI__OK ) {
    sprintf( fname_grpex, "*_");
    strncat( fname_grpex, mname, sizeof(mname) );
    strncat( fname_grpex, suffix, sizeof(suffix) );
  }

  grpGrpex( fname_grpex, igrp, *mgrp, &msize, &added, &flag, status );

  if( (*status == SAI__OK) && (msize != isize) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Couldn't create group of NDF model containers.", 
	   status);        
  }

  /* Loop over files */
     
  for( i=1; i<=isize; i++ ) {

    /* Open the template file */
    smf_open_file( igrp, i, "READ", 0, &idata, status );
    
    /* Check that the template is time-ordered data */
    if( *status == SAI__OK ) {
      if( idata->ndims != 3 ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Input file is not time-ordered data!", 
	       status);      
	i = isize;
      }

      if( idata->dtype != SMF__DOUBLE ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Input file does not contain double precision data!",
	       status);      
      }
    }
    
    if( *status == SAI__OK ) {
      
      /* Determine dimensions of model component */
      
      switch( mtype ) {

      case SMF__CUM: /* Cumulative model */
	copyinput = 0;
	dtype = SMF__DOUBLE;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;

      case SMF__RES: /* Model residual */
	copyinput = 1;
	break;

      case SMF__AST: /* Time-domain projection of map */
	copyinput = 0;
	dtype = SMF__DOUBLE;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;
	
      case SMF__COM: /* Single-valued common-mode at each time step */
	copyinput = 0;
	dtype = SMF__DOUBLE;
	ndims = 1;
	lbnd[0] = 1;
	ubnd[0] = (idata->dims)[2];
	break;
	
      case SMF__NOI: /* Noise model */
	copyinput = 0;
	dtype = SMF__DOUBLE;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;

      case SMF__EXT: /* Extinction correction - gain for each bolo/time */
	copyinput = 0;
	dtype = SMF__DOUBLE;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;

      case SMF__LUT: /* Pointing LookUp Table for each data point */
	copyinput = 0;
	dtype = SMF__INTEGER;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;
      }

      /* Propagate more information from template if copying */

      if( copyinput ) { /* If copying input, copy data dimensions directly */
	dtype = idata->dtype; /* Inherit data type from template */
	ndims = idata->ndims;
	for( j=0; j<ndims; j++ ) {
	  lbnd[j] = 0;
	  ubnd[j] = (idata->dims)[j];
	}
      } 

      /* Calculate the size of the data buffer. Format:

	 Header:
         dtype = [integer]
         ndims = [integer] 
         dims  = [integer]*ndims

         Data:
	 buf   = [smf_dtype] * dims[0] * dims[1] * ...
      */
      
      ndata = 1;
      for( j=0; j<ndims; j++ ) {
	ndata *= ubnd[j];
      }
     
      /* Length of data array buffer and header in bytes */
      datalen = ndata * smf_dtype_sz(dtype,status); 
      headlen = sizeof(dtype) + sizeof(ndims) + ndims*sizeof(dims[0]);

      /* Obtain a character string corresponding to the file name */
      pname = name;
      grpGet( *mgrp, i, 1, &pname, GRP__SZNAM, status );

      /* Create the model container */

      if( (fd = open( name, O_RDWR | O_CREAT | O_TRUNC, 
		      S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH )) == -1 ) {
	*status = SAI__ERROR;
	errRep( FUNC_NAME, "Unable to open model container file", status ); 
      }
      
      buf = smf_malloc( headlen+datalen, 1, 0, status );
      headptr = buf;
      dataptr = buf + headlen;

      if( *status == SAI__OK ) {

	/* Write the header */
	((smf_dtype *) headptr)[0] = dtype;
	((int *)(headptr + sizeof(dtype)))[0] = ndims;

	for( j=0; j<ndims; j++ ) {
	  ((dim_t *)(headptr + sizeof(dtype) + sizeof(ndims)))[j] = ubnd[j];
	}

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
	  errRep( FUNC_NAME, "Unable to write model container file", status ); 
	}
	
	if( close( fd ) == -1 ) {
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME, "Unable to close model container file", status ); 
	}
      }

      /* Close the input template file */

      smf_close_file( &idata, status );

      /* Create the model container */
 
      
      //if( copyinput ) { /* Make a copy of the template file */
      //	ndgNdfas( igrp, i, "READ", &indf, status );
      //	ndgNdfpr( indf, "DATA,VARIANCE,QUALITY", *mgrp, i, &mndf, status );
      //	ndfAnnul( &indf, status );

	/* Map to ensure that the DATA array is defined on exit */
      //	ndfMap( mndf, "DATA,", "_DOUBLE", "UPDATE", &mapptr[0], &nmap, 
      //		status );

	/* Map to ensure that the VARAINCE array is defined on exit */
      //	ndfMap( mndf, "VARIANCE", "_DOUBLE", "WRITE", &mapptr[1], &nmap, 
      //		status );

	/* Initialize VARIANCE component of residuals to 1 */
      //	if( (*status == SAI__OK) && (mapptr[1]) ) {
      //	  for( j=0; j<nmap; j++ ) {
      //	    ((double *)(mapptr[1]))[j] = 1; 
      //	  }
      //	}

      //	ndfAnnul( &mndf, status );
	
      //      } else {          /* Make a new empty container */
	/*
	smf_open_newfile( *mgrp, i, SMF__DOUBLE, ndims, lbnd, ubnd, 
			  SMF__MAP_VAR, &tempdata, status );
	*/
	  
      //	smf_open_newfile( *mgrp, i, SMF__DOUBLE, ndims, lbnd, ubnd, 
      //			  0, &tempdata, status );
      //	smf_close_file( &tempdata, status );
      //      }
    }
  }
}
