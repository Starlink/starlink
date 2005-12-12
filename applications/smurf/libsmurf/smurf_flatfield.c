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

  smfData *fdata = NULL;
  void *pntr[3];
  void *ipntr[3];

  char *pname;
  int *tstream;           /* pointer to array data */

  smfHead *ohdr;
  AstFrameSet *outwcs;
  AstFitsChan *outfits;
  int npts;
  int *indata = NULL;
  int nframes;

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
    /* Should check status here to make sure that the file was opened OK */

    file = data->file;
    indf = file->ndfid;

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
	/*        *status = SAI__ERROR;
		  errRep( "smurf_flatfield", "Flatfield has already been applied",status);*/
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


    smf_flatfield( data, &fdata, status );
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOut("smurf_flatfield",
	     "smurf_flatfield: Data are already flatfielded", status);
    }

    pntr[0] = (fdata->pntr)[0];
    outdata = pntr[0];

    ipntr[0] = (data->pntr)[0];
    indata = ipntr[0];

    /*    npts = (fdata->dims)[0]*(fdata->dims)[1]*(fdata->dims)[2];
    printf("ndims = %d, nout = %d \n",ndims, nout);
    printf("output ndims = %d, npts = %d \n",fdata->ndims, npts);*/

    npts = (data->dims)[0]*(data->dims)[1]*(data->dims)[2];
    printf("input ndims = %d, npts = %d \n",data->ndims, npts);
    for ( i=0; i<npts; i++) {
      printf("i = %d, indata = %d, outdata = %g \n",i,indata[i],outdata[i]);
    }
    /*    for ( i=0; i<npts; i++) {
      printf("i = %d, outdata = %g \n",i,outdata[i]);
      }*/

    ohdr = fdata->hdr;
    outwcs = ohdr->wcs;
    outfits = ohdr->fitshdr;

    /*astShow(outfits);*/
    ndfAnnul( &outndf, status);

    /* Check status.... */

    msgOutif(MSG__VERB," ","Flat field applied", status);

    smf_close_file( &data, status );
  }


  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
