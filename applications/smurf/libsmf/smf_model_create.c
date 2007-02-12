/*
*+
*  Name:
*     smf_model_create

*  Purpose:
*     Create group of NDF containers for iterative map-maker model components

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
  int flag=0;                   /* Flag */
  char fname_grpex[255];        /* String for holding filename grpex */
  int i;                        /* Loop counter */
  smfData *idata=NULL;          /* Pointer to input smfdata data */
  int indf=0;                   /* NDF ID for propagation */
  int isize=0;                  /* Number of files in input group */
  int lbnd[NDF__MXDIM];         /* Dimensions of container */
  void *mapptr[3];              /* Pointer to array of mapped components */
  char *mname=NULL;             /* String model component name */
  int mndf=0;                   /* NDF ID for propagation */
  int msize=0;                  /* Number of files in model group */
  int ndims=0;                  /* Number of dimensions in container */
  int nmap=0;                   /* Number of elements mapped */
  smfData *tempdata=NULL;       /* Temporary smfData pointer */
  int ubnd[NDF__MXDIM];         /* Dimensions of container */


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
    strncat( fname_grpex, mname, 10 );
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
    }
    
    if( *status == SAI__OK ) {
      
      /* Determine dimensions of model component */
      
      switch( mtype ) {

      case SMF__CUM: /* Cumulative model */
	copyinput = 0;
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
	ndims = 1;
	lbnd[0] = 1;
	ubnd[0] = (idata->dims)[2];
	break;
	
      case SMF__NOI: /* Noise model */
	copyinput = 0;
	ndims = 3;
	lbnd[0] = 1;
	lbnd[1] = 1;
	lbnd[2] = 1;
	ubnd[0] = (idata->dims)[0];
	ubnd[1] = (idata->dims)[1];
	ubnd[2] = (idata->dims)[2];
	break;
      }

      /* Close the input template file */

      smf_close_file( &idata, status );

      /* Create the model container */

      if( copyinput ) { /* Make a copy of the template file */
	ndgNdfas( igrp, i, "READ", &indf, status );
	ndgNdfpr( indf, "DATA,VARIANCE,QUALITY", *mgrp, i, &mndf, status );
	ndfAnnul( &indf, status );

	/* Map to ensure that the DATA array is defined on exit */
	ndfMap( mndf, "DATA", "_DOUBLE", "UPDATE", &mapptr[0], &nmap, status );

	ndfAnnul( &mndf, status );
	
      } else {          /* Make a new empty container */
	/*
	smf_open_newfile( *mgrp, i, SMF__DOUBLE, ndims, lbnd, ubnd, 
			  SMF__MAP_VAR, &tempdata, status );
	*/
	smf_open_newfile( *mgrp, i, SMF__DOUBLE, ndims, lbnd, ubnd, 
			  0, &tempdata, status );
	smf_close_file( &tempdata, status );
      }
    }
  }
}
