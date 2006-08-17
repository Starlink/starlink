/*
*+
*  Name:
*     smf_model_create

*  Purpose:
*     Create group of NDF containers for map-maker model components

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
*     new NDF files with dimensions appropriate for the common-model
*     signal, namely a 1-dimensional array as a function of time. The names
*     are the same as the input files, with a suffix "_common"

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-06 (EC):
*        Initial Version
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
  int flag=0;                   /* Flag */
  int i;                        /* Loop counter */
  smfData *idata=NULL;          /* Pointer to input smfdata data */
  int isize=0;                  /* Number of files in input group */
  void *mapptr[3];              /* Pointer to array of mapped components */
  char *mname=NULL;             /* String model component name */
  int mndf=NDF__NOID;           /* Model NDF identifier */
  int msize=0;                  /* Number of files in model group */
  int ubnd[NDF__MXDIM];         /* Upper bounds of model data array */
  int nmap=0;                   /* Number of elements mapped */
  int ndims=0;                  /* Number of dimensions in model */
  int propall=0;                /* If set propagate all components */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get size of the input group */
  grpGrpsz( igrp, &isize, status );

  /* Create group of NDF names with model name suffix */
  *mgrp = grpNew( "model component", status );
  mname = smf_model_getname( mtype, status );

  printf("!!!!!!!!! mname=%s\n", mname);

  grpGrpex( mname, igrp, *mgrp, &msize, &added, &flag, status );

  if( (*status == SAI__OK) && (msize != isize) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Couldn't create group of NDF model containers.", 
	   status);        
  }

  /* Loop over files */
     
  for( i=1; i<=isize; i++ ) {

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

      case SMF__AST: /* 2d output image */
	/* Special: One map for all data files */
	propall = 1;
	break;

      case SMF__COM: /* One value at each time step */
	propall = 0;
	ndims = 1;
	ubnd[0] = (idata->dims)[2];
	break;

      case SMF__CUM: /* Same dimensions as the template */
	propall = 1;
	break;

      case SMF__RES: /* Same dimensions as the template */
	propall = 1;
	break;

      case SMF__NOI: /* Same dimensions as the template */
	propall = 1;
	break;
      }

      if( propall ) {
	/* Propagate everything */
	ndgNdfpr( (idata->file)->ndfid, " ", *mgrp, i, &mndf, status );

      } else {
	/* Define the new dimensions for the NDF */
	/* ndgNdfcp( *mgrp, i, "_DOUBLE", ndims, ubnd, &mndf, status ); */

	ndgNdfpr( (idata->file)->ndfid, " ", *mgrp, i, &mndf, status );
      }
      
      /* Map the DATA component before annulling so that it is defined */
      ndfMap( mndf, "DATA", "_DOUBLE", "WRITE", &(mapptr[0]), &nmap, 
	      status );
      ndfAnnul( &mndf, status );

      /* Close the input template file */
      smf_close_file( &idata, status );
    }
  }
}
