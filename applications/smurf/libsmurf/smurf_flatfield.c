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
*     2005-11-02 (TIMJ):
*        Initial test version
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

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"


void smurf_flatfield( int *status ) {

  int flag;                  /* */
  /*  int fndf; */                 /* Flatfield NDF identifier */
  int i;                     /* Counter, index */
  int j;                     /* Counter, index */
  Grp *igrp = NULL;
  int ndfdims[NDF__MXDIM];   /* Dimensions of input NDF */
  int ndims;                 /* Number of active dimensions in input */
  Grp *ogrp = NULL;
  int outndf;                  /* Output NDF identifier */
  int outsize;               /* Total number of NDF names in the output group */
  int size;                  /* Number of files in input group */
  int nboll;
  int nout;
  double *outdata = NULL;
  int indf;
  int rawdata;
  int subsysnr; /* dummy var to print */

  smfDA * da;
  smfData * data;
  smfFile * file;
  smfHead * head;

  void * pntr[3];

  char *pname;
  int *tstream;           /* pointer to array data */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Read flatfield file */
  /*  ndfAssoc( "FLAT", "READ", &fndf, status );*/

  /* Get output file(s) */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );


  for (i=1; i<=size; i++ ) {

    /* Q&D open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, " ", ogrp, i, &outndf, status );
    ndfAnnul( &indf, status);

    smf_open_file( igrp, i, "READ", &data, status);

    file = data->file;
    pname = file->name;
    msgSetc("FILE", pname);
    msgOutif(MSG__VERB, " ", "Flatfielding file ^FILE", status);

    da = data->da;
    if ( da != NULL ) {
      msgSetc("FLATNAME", da->flatname);
      msgSeti("NFRAMES", (data->dims)[2]);
      msgOutif(MSG__VERB, " ", "Read ^NFRAMES from time stream, flatfield method ^FLATNAME", status);
    } else { /* What if 10 out of 20 are bad? .... */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	errRep( "smurf_flatfield", "Flatfield has already been applied",status);
      }
    }


    ndfStype( "_DOUBLE", outndf, "DATA", status);

    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &outdata, &nout, status );
    ndfDim( outndf, NDF__MXDIM, ndfdims, &ndims, status );

    /* Check ndims = 3 */
    if ( *status == SAI__OK ) {
      if ( ndims != 3 ) {
	msgSeti( "NDIMS", ndims);
	*status = SAI__ERROR;
	errRep( "smurf_flatfield", "Number of dimensions in output, ^NDIMS, is not equal to 3",status);
      }
    }

    /* Check nout = nrows * ncols * nframes */
    if ( *status == SAI__OK ) {
      if (nout != (data->dims)[0]*(data->dims)[1]*(data->dims)[2]) {
	msgSeti( "NR", (data->dims)[0]);
	msgSeti( "NC", (data->dims)[1]);
	msgSeti( "NF", (data->dims)[2]);
	msgSeti( "NOUT", nout);
	*status = SAI__ERROR;
	errRep( "smurf_flatfield", "Number of input pixels not equal to the number of output pixels (^NR*^NC*^NF != ^NOUT)",status);
      } else {
	pntr[0] = (data->pntr)[0];
	tstream = pntr[0]; /* Input time series data */
	printf("%p %p %p \n",(data->pntr)[0],(data->pntr)[1],(data->pntr)[2]);
	for (j=0; j<nout; j++) {
	  outdata[j] = (double)tstream[j];
	}
      }
    }

    /* Print something interesting from the FITS header */
    head = data->hdr;
    if ( !astGetFitsI( head->fitshdr, "SUBSYSNR", &subsysnr) ) {
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	msgSetc("FITS", "SUBSYSNR");
	errRep("smurf_flatfield", "Unable to retrieve ^SUBSYSNR", status);
      }
    }
    msgSeti("FITS",subsysnr);
    msgOut(" ","SUBSYSNR = ^FITS", status);

    /* */
    if ( *status == SAI__OK) {
      nboll = (data->dims)[0]*(data->dims)[1];

      /* Apply the flat field to the data */
      msgOutif(MSG__VERB," ","Applying flat field ", status);
      sc2math_flatten( nboll, (data->dims)[2], da->flatname, da->nflat, da->flatcal, 
		       da->flatpar, outdata, status);
    } 
    msgOutif(MSG__VERB," ","Flat field applied", status);
    ndfAnnul( &outndf, status);
  }


  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
