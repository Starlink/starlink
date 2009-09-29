/*
*+
*  Name:
*     STACKFRAMES

*  Purpose:
*     Stack 2d processed frames into time series cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_stackframes( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Takes a stack of 2d frames of bolometer data, usually noise
*     images or responsivity images, and combines them into a single
*     cube with an annotated time axis. This makes it easy to look
*     at the behaviour of a single detector as it varies with time.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s). Files must all be 2-d and have the same
*          dimensions and must have a DATE-OBS FITS header.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Single output file with all the 2d images stacked into a single
*          observation.

*  Notes:
*     - No special SCUBA-2 processing is applied. The assumption is simply
*     that you have some images that are all the same size and you want to
*     put them into a single cube with a time axis.
*     - Variations in pixel origin are ignored.

*  Related Applications:
*     SMURF: CALCNOISE, CALCFLAT

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-09-25 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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
#include <stdio.h>
#include <stdlib.h>

/* Starlink includes */
#include "star/ndg.h"
#include "star/grp.h"
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smurf_stackframes"
#define TASK_NAME "STACKFRAMES"

void smurf_stackframes( int *status ) {

  AstFrame * frame2d;            /* 2D frame from input image */
  AstObject * framewcs = NULL;   /* input WCS frame */
  AstFrame * gridfrm;            /* 3d grid frame */
  size_t i;                      /* counter */
  Grp *igrp = NULL;              /* Input files */
  int indf;                      /* Input NDF identifer */
  int itemp;                     /* Temporary int */
  int lbnd[NDF__MXDIM];          /* Lower bounds of output */
  AstShiftMap * map2d;
  char ndftype[NDF__SZTYP+1];    /* type of data array */
  Grp *ogrp = NULL;              /* Output group */
  char * odatad = NULL;          /* Output data array as bytes */
  char * odataq = NULL;          /* Output quality array */
  char * odatav = NULL;          /* Output variance array as bytes */
  double origin = 0.0;           /* Origin of time frame */
  smfData * outdata = NULL;      /* Output smfData */
  int outndf;                    /* Output NDF file */
  size_t outsize;                /* Size of output group */
  AstFrameSet *outwcs;           /* Output frameset */
  void * pntr[3];                /* for ndfMap */
  dim_t refdims[NDF__MXDIM];     /* Reference dimensions */
  int refndims = 0;              /* Number of dims in first image */
  char reftype[NDF__SZTYP+1];    /* reference data type */
  size_t size;                   /* Size of intput group */
  smfSortInfo * sortinfo = NULL; /* For sorting */
  size_t szplane = 0;            /* Number of elements in a plane */
  size_t szplaneb = 0;           /* Number of bytes in plane */
  size_t sztype = 0;             /* Number of bytes in the primitive data type */
  AstTimeFrame * timefrm = NULL; /* Timeframe */
  AstLutMap *timemap = NULL;     /* Output time mapping */
  double * times = NULL;         /* Array of MJDs for each input file */
  AstCmpMap * totmap;
  AstCmpFrame *totfrm;
  int ubnd[NDF__MXDIM];          /* Upper bounds of output */
  double zshift[2];              /* Pixel shift for output WCS */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Read the input files (at least 2) */
  kpg1Rgndf( "IN", 0, 2, "Must provide at least 2 frames for stacking",
	     &igrp, &size, status );

  /* Get the output file */
  kpg1Wgndf( "OUT", igrp, 1, 1, "",
	     &ogrp, &outsize, status );

  if (*status != SAI__OK) goto CLEANUP;

  /* To sort into time order we need to extract some information
     from each file */
  sortinfo = smf_malloc( size, sizeof(*sortinfo), 1, status );

  /* First check that all the input files are the right shape */
  for (i=1; i<=size; i++) {
    smfData * data = NULL;

    smf_open_file( igrp, i, "READ", SMF__NOCREATE_DATA, &data, status);
    if (*status == SAI__OK) {
      /* Remove trailing dims that are size 1 */
      size_t j;
      int ndims = 1;
      for (j=data->ndims; j > 0; j--) {
	/* Loop until we don't have a size 1 */
	if (data->dims[j-1] != 1) {
	  ndims = j;
	  break;
	}
      }
      for (j = 1; j<=ndims; j++) {
	msgSeti( "DIMS", data->dims[j-1] );
	if (j != ndims) msgSetc("DIMS", "x" );
      }
      msgOutiff( MSG__DEBUG, " ",
		 "File %d has %d dimensions [^DIMS]",status, i, ndims );

      if (refndims == 0) {
	/* need to store reference dims - do not use memcpy since
	   we want to make sure that type conversion works */
	refndims = ndims;
	for (j = 0; j<ndims; j++) {
	  refdims[j] = (data->dims)[j];
	}
	one_strlcpy( reftype, smf_dtype_string( data, status ), sizeof(reftype), status );
      } else {
	const char * dtype = smf_dtype_string( data, status );
	if (!dtype && *status == SAI__OK) {
	  *status = SAI__ERROR;
	  errRepf( " ", "Unable to determine data type of file %d. Something is very wrong",
		   status, i );
	}

	if (refndims != ndims) {
	  if (*status != SAI__OK) {
	    *status = SAI__ERROR;
	    errRepf( " ", "Dimensionality of file %d does not match that of first file",
		     status, i );
	  }
        } else if ( *status == SAI__OK && strcmp( reftype, dtype ) != 0 ) {
	  /* This check would not be necessary if we could modify smf_open_file
	     to force the data type to use */
	  *status = SAI__ERROR;
	  errRepf( " ", "Data type of file %d [%s] differs from that of the first file [%s]",
		   status, i, dtype, reftype );

	} else {
	  /* check dimensionality */
	  if (*status != SAI__OK) {
	    for (j = 1; j <= ndims; j++) {
	      if ( refdims[j-1] != (data->dims)[j-1] ) {
		*status = SAI__ERROR;
		errRepf( " ", "Dimension %d of file %d differs from that of the first file",
			 status, i, j );
		break;
	      }
	    }
	  }
	}
      }
    }
    if (*status == SAI__OK) {
      smfSortInfo * thisitem = &(sortinfo[i-1]);
      smf_find_dateobs( data->hdr, &(thisitem->mjd), NULL, status );
      thisitem->index = i;
    }
    smf_close_file( &data, status );
  }

  if (*status != SAI__OK) goto CLEANUP;

  if (refndims != 2) {
    *status = SAI__OK;
    errRepf(" ", TASK_NAME " requires that all input files have 2 dimensions not %d", status,
	    refndims);
  }

  msgOutf( " ", "Input files all have dimensions of %d x %d", status,
	   refdims[0], refdims[1] );

  /* Now need to sort the files into time order. We have the dates and indices */
  qsort( sortinfo, size, sizeof(*sortinfo), smf_sort_bytime );

  /* Now we can do the real work */

  /* Propagate from the oldest file and resize it to 3d. */
  ndgNdfas( igrp, sortinfo[0].index, "READ", &indf, status );
  ndgNdfpr( indf, "WCS,QUALITY,UNITS,TITLE,LABEL,NOEXTENSION(PROVENANCE)",
	    ogrp, 1, &outndf, status );
  ndfBound( outndf, NDF__MXDIM, lbnd, ubnd, &refndims, status );
  lbnd[2] = 1;
  ubnd[2] = lbnd[2] + size - 1;
  ndfSbnd( 3, lbnd, ubnd, outndf, status );
  /* need to convince NDF that we have defined the data array
   - and use the native type to avoid type conversion. */
  ndfType( outndf, "DATA", ndftype, sizeof(ndftype), status );
  ndfMap(outndf, "DATA", ndftype, "WRITE", pntr, &itemp, status  );
  ndfAnnul( &outndf, status );

  /* Now reopen it with smf_open_file and copy all the data over
     - do not need header information at this point and smf_open_file
     will complain if we do read the header because there will be no
     JCMTSTATE info and it is wanting this to be raw time series
     data since it is 3d.
  */
  smf_open_file( ogrp, 1, "WRITE", SMF__NOCREATE_HEAD, &outdata, status );
  if (*status != SAI__OK) goto CLEANUP;

  /* Create an array of doubles to store the LutMap information */
  times = smf_malloc( size, sizeof(*times), 0, status );

  /* get pointers to each component and the size of each plane to be
     copied (noting that QUALITY is _CHAR type but we also work in bytes. */
  sztype = smf_dtype_size( outdata, status );
  szplane = (outdata->dims)[0] * (outdata->dims)[1];
  szplaneb = szplane * sztype;
  odatad = (outdata->pntr)[0];
  odatav = (outdata->pntr)[1];
  odataq = (outdata->pntr)[2];

  for (i = 1; i <= size; i++ ) {
    smfData * data = NULL;
    smf_open_file( igrp, sortinfo[i-1].index, "READ", 0, &data, status );
    if (*status != SAI__OK) break;
    smf_find_dateobs( data->hdr, &(times[i-1]), NULL, status );

    /* Store the first WCS */
    if (i==1) {
      framewcs = astClone( data->hdr->wcs );
    }

    /* copy data to slice */
    if ( odatad && (data->pntr)[0] ) {
      memcpy( odatad, (data->pntr)[0], szplaneb );
      odatad += szplaneb;
    }
    if ( odatav && (data->pntr)[1] ) {
      memcpy( odatav, (data->pntr)[1], szplaneb );
      odatav += szplaneb;
    }
    if ( odataq && (data->pntr)[2] ) {
      memcpy( odataq, (data->pntr)[2], szplane );
      odataq += szplane;
    }
    smf_close_file( &data, status );
  }

  /* Now need to sort out the WCS */

  /* Need a LutMap which transforms grid coord into MJD (in days)
     Can also tweak the TimeFrame attributes a little.
     We do not have to special case the LUT for the case of a single
     observation since this routine enforces a minimum of 2 frames.
     Do put in a TimeOrigin based on the first observation.
     Also assume that observations are
     going to be some distance apart so use iso.0 formatting.
   */
  timefrm = astTimeFrame ( "format=iso.0" );
  origin = floor( times[0] );
  astSetD( timefrm, "TimeOrigin", origin );
  for (i = 0; i < size; i++ ) {
    times[i] = times[i] - origin;
  }
  timemap = astLutMap( size, times, 1.0, 1.0, " " );
  times = smf_free( times, status );

  /* For now ignore the input 2d frameset and just make a new one */
  frame2d = astFrame( 2, "Domain=BOLO" );

  totfrm = astCmpFrame( frame2d, timefrm, " " );

  zshift[0] = -1; /* Set BOLO origin to 0, 0 */
  zshift[1] = -1;
  map2d = astShiftMap( 2, zshift, " " );

  totmap = astCmpMap( map2d, timemap, 0, " " );

  /* Create a 3D GRID Frame. */
  gridfrm = astFrame( 3, "Domain=GRID,Title=FITS pixel coordinates" );
  astSet( gridfrm, "Unit(1)=pixel,Label(1)=FITS pixel axis 1" );
  astSet( gridfrm, "Unit(2)=pixel,Label(2)=FITS pixel axis 2" );
  astSet( gridfrm, "Unit(3)=pixel,Label(2)=FITS pixel axis 3" );

  /* Create the frameset */
  outwcs = astFrameSet( gridfrm, "  ");
  astAddFrame( outwcs, AST__BASE, totmap, totfrm );

  if (*status == SAI__OK) ndfPtwcs( outwcs, outdata->file->ndfid, status );

 CLEANUP:
  smf_close_file( &outdata, status );
  if (sortinfo) sortinfo = smf_free( sortinfo, status );
  grpDelet( &igrp, status );
  grpDelet( &ogrp, status );
  ndfEnd(status);

}
