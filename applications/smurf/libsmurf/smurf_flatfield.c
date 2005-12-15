/*
*+
*  Name:
*     smurf_flatfield

*  Purpose:
*     Top-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_flatfield( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the FLATFIELD task.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded
*     FLAT = NDF (Read)
*          Optional file containing flatfield solution
*     OUT = NDF (Write)
*          Output file

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version. Copy from smurf_extinction
*     2005-11-05 (AGG):
*        Factor out I/O code to smf_open_file
*     2005-11-07 (TIMJ):
*        Replace fits example code with call to smf_fits_getI
*     2005-11-28 (TIMJ):
*        Use smf_close_file
*     2005-12-12 (AGG):
*        Use smf_flatfield, remove need to include sc2da info, fix
*        loop counter bug
*     2005-12-14 (AGG/TIMJ):
*        Use smf_open_file on the output data. Note that
*        smf_close_file has been temporarily commented out until it is
*        also updated to take account of reference counting.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"

void smurf_flatfield( int *status ) {

  smfDA *da;                /* Pointer to da struct containing flatfield info */
  smfData *data;            /* Pointer to input data struct */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  smfFile *file;            /* Pointer to input file struct */
  int flag;                 /* */
  smfHead *head;            /* Pointer to input file header info */
  int i;                    /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  int indf;                 /* NDF identifier for input file */
  int j;                    /* Counter, index */
  int nboll;                /* Number of bolometers */
  int ndfdims[NDF__MXDIM];  /* Dimensions of input NDF */
  char ndftype[NDF__SZTYP]; /* Type of data in output data file */
  int ndims;                /* Number of active dimensions in input */
  int nframes;              /* Number of time slices */
  int nout;                 /* Number of data points in output data file */
  Grp *ogrp = NULL;         /* Output group of files */
  double *outdata = NULL;   /* Pointer to output DATA array */
  int outndf;               /* Output NDF identifier */
  int outsize;              /* Total number of NDF names in the output group */
  char *pname;              /* Pointer to input filename */
  int size;                 /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  for (i=1; i<=size; i++ ) {

    /* Q&D open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, " ", ogrp, i, &outndf, status );
    ndfAnnul( &indf, status);

    smf_open_file( igrp, i, "READ", &data, status);
    /* Should check status here to make sure that the file was opened OK */

    file = data->file;
    pname = file->name;
    msgSetc("FILE", pname);
    msgOutif(MSG__VERB, " ", "Flatfielding file ^FILE", status);

    /* Check we have a smfDA struct with flatfield info */
    da = data->da;
    if ( da != NULL ) {
      msgSetc("FLATNAME", da->flatname);
      msgSeti("NFRAMES", (data->dims)[2]);
      msgOutif(MSG__VERB, " ", 
	       "Read ^NFRAMES from time stream, flatfield method ^FLATNAME", 
	       status);
    } else { /* What if 10 out of 20 are bad? .... */
      if ( *status == SAI__OK) {
	/*        *status = SAI__ERROR;
		  errRep( "smurf_flatfield", "Flatfield has already been applied",status);*/
      }
    }

    /* Set parameters of the DATA array in the output file */
    ndfStype( "_DOUBLE", outndf, "DATA", status);
    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &outdata, &nout, status );

    ndfAnnul( &outndf, status);

    smf_open_file( ogrp, i, "WRITE", &ffdata, status);

    /* Call flatfield routine */
    smf_flatfield( data, &ffdata, status );
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB, "smurf_flatfield",
	     "smurf_flatfield: Data are already flatfielded", status);
    } else {
      msgOutif(MSG__VERB," ","Flat field applied", status);
    }

    /*    smf_close_file( &data, status );*/
    /*smf_close_file( &ffdata, status );*/

  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}

