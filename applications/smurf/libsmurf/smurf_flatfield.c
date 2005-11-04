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
*          File containing the flatfield solution
*     OUT = NDF (Write)
*          Output file

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

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
#include "sc2da/sc2store_sys.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"

#define SMF_PATH_MAX 4096

void smurf_flatfield( int *status ) {

  int flag;                  /* */
  int fndf;                  /* Flatfield NDF identifier */
  int i;                     /* Counter, index */
  Grp *igrp = NULL;
  int indf;                  /* Input NDF identifier */
  dim_t indims[2];    /* Copy of the NDF dimensions */
  char name[SMF_PATH_MAX];   /* */
  int ndfdims[NDF__MXDIM];   /* Dimensions of input NDF */
  int ndims;                 /* Number of active dimensions in input */
  Grp *ogrp = NULL;
  int outndf;                  /* Output NDF identifier */
  int outsize;               /* Total number of NDF names in the output group */
  int size;                  /* */
  int nboll;
  int nout;
  double *outdata = NULL;

  /* Pasted from readsc2ndf */
  int colsize;            /* number of pixels in column (returned) */
  int *dksquid;           /* pointer to dark SQUID data */
  char *pname;
  char filename[SMF_PATH_MAX];     /* name of file */
  struct sc2head *frhead; /* structure for headers for a frame */
  double *flatcal;        /* pointer to flatfield calibration */
  double *flatpar;        /* pointer to flatfield parameters */
  char headrec[80][81];   /* FITS headers */
  char flatname[SC2STORE_FLATLEN]; /* name of flatfield algorithm */
  int j;                  /* loop counter */
  int maxfits;            /* maximum number of FITS headers */
  int maxlen;             /* maximum length of a FITS header */
  int nfits;              /* number of FITS headers */
  int nflat;              /* number of flat coeffs per bol */
  int nframes;            /* number of frames */
  int *tstream;           /* pointer to array data */
  int rowsize;            /* number of pixels in row (returned) */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Read flatfield file */
  /*  ndfAssoc( "FLAT", "READ", &fndf, status );*/

  /* */
  /*  sc2store_rdflat();*/

  /* Get output file(s) */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  pname = filename;

  for (i=1; i<=size; i++ ) {
    /*    msgOut("smurf_flatfield","About to call NDFAS",status)*/
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, " ", ogrp, i, &outndf, status );
    ndfAnnul( &indf, status);

    grpGet( igrp, i, 1, &pname, SMF_PATH_MAX, status);

    msgSetc("FILE", pname);
    msgOutif(MSG__VERB, " ", "Flatfielding file ^FILE", status);


    sc2store_rdtstream( pname, SC2STORE_FLATLEN, maxlen, maxfits, 
			&nfits, headrec, &colsize, &rowsize, 
			&nframes, &nflat, flatname, &frhead,
			&tstream, &dksquid, &flatcal, &flatpar, status);

    sc2store_free( status );

    msgSetc("FLATNAME", flatname);
    msgSeti("NFRAMES", nframes);
    msgOutif(MSG__VERB, " ", "Read ^NFRAMES from time stream, flatfield method ^FLATNAME", status);

    msgOutif(MSG__VERB," ","Read the time stream ", status);
    /*    msgOut("smurf_flatfield","About to call NDFPR",status)*/
   

    ndfStype( "_DOUBLE", outndf, "DATA", status);

    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &outdata, &nout, status );
    ndfDim( outndf, NDF__MXDIM, ndfdims, &ndims, status );

    if ( *status == SAI__OK ) {
      indims[0] = (dim_t)ndfdims[0];
      indims[1] = (dim_t)ndfdims[1];
    }

    /* Check ndims = 3 */
    /*    nboll = ndfdims[0] * ndfdims[1];*/
    /*    nframes = ndfdims[2];*/

    /* Use values returned from sc2store_rdtsream */
    nboll = rowsize * colsize;

    /* Check nout = row*col*nframes */
    if ( *status == SAI__OK ) {
      if (nout != rowsize*colsize*nframes) {
	printf ("Error: nout = %d  rowsize*colsize*nframes = %d \n",
		nout,rowsize*colsize*nframes);
      }
      for (j=0; j<nout; j++) {
	outdata[j] = (double)tstream[j];
      }
    }


    msgOutif(MSG__VERB," ","About to apply flat field ", status);
    sc2math_flatten( nboll, nframes, flatname, nflat, flatcal, flatpar, 
		     outdata, status);

    msgOutif(MSG__VERB," ","Flat field applied", status);
    ndfAnnul( &outndf, status);
  }


  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
