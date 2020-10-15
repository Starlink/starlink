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
*     cube with, if sort is enabled, an annotated time axis or an axis
*     based on a numeric FITS header item.
*     This makes it easy to look at the behaviour of a single detector
*     as it varies with time. Not all observations include time information
*     or should be sorted at all and for those set SORT to false. The 3rd
*     axis will not be a sorted axis in that case. This can be useful for
*     examining bolometer maps created by MAKEMAP.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s). Files must all be 2-d and have the same
*          dimensions. For the SORT option to be available they must
*          have a DATE-OBS FITS header.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Single output file with all the 2d images stacked into a single
*          observation.
*     SORT = _LOGICAL (Read)
*          Should the data be sorted into time order (true) or left in the
*          order given in IN (false). If the first file in IN has no
*          date information sorting will be disabled automatically.
*          Default is true if date information is available.
*     SORTBY = _CHAR (Read)
*          If the data are sorted (SORT=TRUE) this parameter controls
*          which header item should be used to do the sorting. Options are
*          MJD (the date) or the name of a numeric FITS header. MJD is not
*          allowed if no date items are present. [MJD]

*  Notes:
*     - No special SCUBA-2 processing is applied. The assumption is simply
*     that you have some images that are all the same size and you want to
*     put them into a single cube with a time axis.
*     - Variations in pixel origin are ignored. Make sure images are aligned
*     and are the same size.
*     - Useful for looking at the variations in bolometer parameters such as
*     images created by CALCNOISE or CALCFLAT.

*  Related Applications:
*     SMURF: CALCNOISE, CALCFLAT, MAKEMAP

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-09-25 (TIMJ):
*        Initial version.
*     2009-10-28 (TIMJ):
*        Add SORT option.
*     2009-10-30 (TIMJ):
*        Only ask for SORT option if we know that sorting is possible.
*     2009-11-27 (TIMJ):
*        Propagate provenance (if available on input) and merge input
*        FITS headers.
*     2010-04-19 (TIMJ):
*        Propagate the bad bits mask from the inputs.
*        Use atlAddWcsAxis.
*     2010-04-23 (TIMJ):
*        Force "not a time series" mode since we do get some images that
*        have a dummy third dimension but won't have JCMTSTATE.
*        Fix dimensionality test.
*     2011-01-25 (TIMJ):
*        Add SORTBY option to sort by any FITS header.
*     2011-09-21 (TIMJ):
*        Force output type to be _DOUBLE because, currently, smf_open_file
*        forces the 2D input files to be _DOUBLE regardless of type.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2011 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "par.h"
#include "star/atl.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smurf_stackframes"
#define TASK_NAME "STACKFRAMES"

void smurf_stackframes( int *status ) {

  smf_qual_t badbit = 0;         /* Default bad bits mask */
  int dosort = 0;                /* Sort into time order? */
  AstFitsChan *fchan = NULL;     /* FitsChan holding output NDF FITS extension */
  AstObject * framewcs = NULL;   /* input WCS frame */
  size_t i;                      /* counter */
  Grp *igrp = NULL;              /* Input files */
  int indf;                      /* Input NDF identifer */
  int  itemp;                    /* Temporary int */
  dim_t lbnd[NDF__MXDIM];        /* Lower bounds of output */
  char ndftype[NDF__SZTYP+1];    /* type of data array */
  Grp *ogrp = NULL;              /* Output group */
  char * odatad = NULL;          /* Output data array as bytes */
  smf_qual_t * odataq = NULL;    /* Output quality array */
  char * odatav = NULL;          /* Output variance array as bytes */
  smfData * outdata = NULL;      /* Output smfData */
  int outndf;                    /* Output NDF file */
  size_t outsize;                /* Size of output group */
  AstFrameSet *outwcs;           /* Output frameset */
  void * pntr[3];                /* for ndfMap */
  dim_t refdims[NDF__MXDIM];     /* Reference dimensions */
  dim_t refndims = 0;            /* Number of dims in first image */
  char reftype[NDF__SZTYP+1];    /* reference data type */
  size_t size;                   /* Size of intput group */
  char sortby[10];               /* Header item to sort by */
  int sortbytime = 0;            /* Are we sorting by time? */
  smfSortInfo * sortinfo = NULL; /* For sorting */
  size_t stemp;                  /* Temporary size_t */
  dim_t szplane = 0;             /* Number of elements in a plane */
  dim_t szplaneb = 0;            /* Number of bytes in plane */
  dim_t sztype = 0;              /* Number of bytes in the primitive data type */
  AstTimeFrame * timefrm = NULL; /* Timeframe */
  AstLutMap *timemap = NULL;     /* Output time mapping */
  double * times = NULL;         /* Array of MJDs for each input file */
  dim_t ubnd[NDF__MXDIM];        /* Upper bounds of output */

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
     from each file. For ease of programming we do use the sort
     struct even if not sorting */
  sortinfo = astCalloc( size, sizeof(*sortinfo) );

  /* First check that all the input files are the right shape.
     We also sort out FITS header merging here for the output fits header
     so that we do not need to read the header later on.
   */
  for (i=1; i<=size; i++) {
    smfData * data = NULL;

    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA|SMF__NOTTSERIES, &data, status);
    if (*status == SAI__OK) {
      /* Remove trailing dims that are size 1 */
      dim_t j;
      dim_t ndims = 1;
      for (j=data->ndims; j > 0; j--) {
	/* Loop until we don't have a size 1 */
	if (data->dims[j-1] != 1) {
	  ndims = j;
	  break;
	}
      }
      for (j = 1; j<=ndims; j++) {
	msgSetk( "DIMS", data->dims[j-1] );
	if (j != ndims) msgSetc("DIMS", "x" );
      }
      msgOutiff( MSG__DEBUG, " ",
		 "File %zu has %zu dimensions [^DIMS]",status, i, ndims );

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
	  errRepf( " ", "Unable to determine data type of file %zu. Something is very wrong",
		   status, i );
	}

	if (refndims != ndims) {
	  if (*status != SAI__OK) {
	    *status = SAI__ERROR;
	    errRepf( " ", "Dimensionality of file %zu does not match that of first file",
		     status, i );
	  }
        } else if ( *status == SAI__OK && strcmp( reftype, dtype ) != 0 ) {
	  /* This check would not be necessary if we could modify smf_open_file
	     to force the data type to use */
	  *status = SAI__ERROR;
	  errRepf( " ", "Data type of file %zu [%s] differs from that of the first file [%s]",
		   status, i, dtype, reftype );

	} else {
	  /* check dimensionality */
	  if (*status == SAI__OK) {
	    for (j = 1; j <= ndims; j++) {
	      if ( refdims[j-1] != (data->dims)[j-1] ) {
		*status = SAI__ERROR;
		errRepf( " ", "Dimension %zu of file %zu differs from that of the first file (%zu != %zu)",
			 status, j, i, refdims[j-1], (data->dims)[j-1] );
		break;
	      }
	    }
	  }
	}
      }
    }
    if (*status == SAI__OK) {
      smfSortInfo * thisitem = &(sortinfo[i-1]);
      /* We can sort on any key so we must always ask for SORT value */
      if (i==1) {
        parGet0l( "SORT", &dosort, status );

        /* but if sorting is requested we set the default SORTBY to MJD
           if we have dates */
        if (dosort && *status == SAI__OK) {
          double thismjd;
          int mjdok = 0;
          smf_find_dateobs( data->hdr, &thismjd, NULL, status );
          if (*status != SAI__OK) {
            mjdok = 0;
            errAnnul( status );
            msgOutif( MSG__NORM, "", "No date information available. Disabling date sorting option",
                      status);
          } else {
            mjdok = 1;
            parDef0c( "SORTBY", "MJD", status );
          }

          parGet0c( "SORTBY", sortby, sizeof(sortby), status );

          if ( strcmp(sortby, "MJD") == 0 ) {
            if (!mjdok) {
              *status = SAI__ERROR;
              errRep("", "Can not sort by MJD as no dates available in input file", status );
            } else {
              sortbytime = 1;
            }
          } else {
          /* check the FITS header */
            if (*status == SAI__OK) {
              int testint;
              smf_fits_getI( data->hdr, sortby, &testint, status );

              if (*status == SMF__NOKWRD) {
                errRep("", "Can not sort by a header item that does not exist in input file", status );
              }
            }
          }
        }
      }

      if (dosort) {
        if (sortbytime) {
          smf_find_dateobs( data->hdr, &(thisitem->sortval), NULL, status );
        } else {
          smf_fits_getD( data->hdr, sortby, &(thisitem->sortval), status );
        }
      } else {
        thisitem->sortval = 0.0;
      }
      thisitem->index = i;
    }

    /* Store the first WCS */
    if (i==1 && *status == SAI__OK) {
      framewcs = astClone( data->hdr->wcs );
    }

    /* Merge fits headers as we go */
    if (*status == SAI__OK && data->hdr->fitshdr) smf_fits_outhdr( data->hdr->fitshdr,
                                                                   &fchan, status );

    /* Retrieve the bad bits mask and OR it with the running value */
    if (*status == SAI__OK) {
      unsigned char bb = 0;
      ndfBb( data->file->ndfid, &bb, status );
      badbit |= bb;
    }

    smf_close_file( NULL, &data, status );
  }

  if (*status != SAI__OK) goto CLEANUP;

  if (refndims != 2) {
    *status = SAI__OK;
    errRepf(" ", TASK_NAME " requires that all input files have 2 dimensions not %zu", status,
	    refndims);
  }

  msgOutf( " ", "All %zu input files have dimensions of %zu x %zu", status,
	   size, (dim_t)refdims[0], (dim_t)refdims[1] );

  /* Now need to sort the files into time order. We have the dates and indices */
  if (dosort) qsort( sortinfo, size, sizeof(*sortinfo), smf_sort_bydouble );

  /* Now we can do the real work */

  /* Propagate from the oldest file and resize it to 3d. */
  ndgNdfas( igrp, sortinfo[0].index, "READ", &indf, status );
  ndgNdfpr( indf, "WCS,UNITS,TITLE,LABEL,NOEXTENSION(PROVENANCE)",
	    ogrp, 1, &outndf, status );
  ndfAnnul( &indf, status );
  ndfBound( outndf, NDF__MXDIM, lbnd, ubnd, &itemp, status );
  refndims = itemp;
  lbnd[2] = 1;
  ubnd[2] = lbnd[2] + size - 1;
  ndfSbnd( 3, lbnd, ubnd, outndf, status );

  /* Force the type of the output to _DOUBLE because
     smf_open_file currently forces 2d files to _DOUBLE */
  ndfStype( "_DOUBLE", outndf, "DATA", status );
  ndfStype( "_DOUBLE", outndf, "VARIANCE", status );

  /* need to convince NDF that we have defined the data array
   and VARIANCE - and use the native type to avoid type conversion. */
  ndfType( outndf, "DATA", ndftype, sizeof(ndftype), status );
  ndfMap(outndf, "VARIANCE,DATA", ndftype, "WRITE/BAD", pntr, &stemp, status  );
  ndfAnnul( &outndf, status );

  /* Now reopen it with smf_open_file and copy all the data over
     - indicate that this is not a time series file and don't waste
     time reading the header. Use UPDATE mode so that we retain BAD
     values in the variance slice if the input file does not have
     variance.
  */
  smf_open_file( NULL, ogrp, 1, "UPDATE", SMF__NOCREATE_HEAD|SMF__NOTTSERIES, &outdata, status );
  if (*status != SAI__OK) goto CLEANUP;

  /* Create an array of doubles to store the LutMap information */
  times = astMalloc( size*sizeof(*times) );

  /* get pointers to each component and the size of each plane to be
     copied (noting that QUALITY is _CHAR type but we also work in bytes. */
  sztype = smf_dtype_size( outdata, status );
  szplane = (outdata->dims)[0] * (outdata->dims)[1];
  szplaneb = szplane * sztype;
  odatad = (outdata->pntr)[0];
  odatav = (outdata->pntr)[1];
  odataq = outdata->qual;

  /* Read each file again to get the data but do not bother getting a
     header this time around */
  for (i = 1; i <= size; i++ ) {
    smfData * data = NULL;
    smf_open_file( NULL, igrp, sortinfo[i-1].index, "READ", SMF__NOCREATE_HEAD|SMF__NOTTSERIES, &data, status );
    if (*status == SAI__OK && !smf_dtype_check( data, NULL, outdata->dtype, status ) ) {
      *status = SAI__ERROR;
      errRep("", "Input data type differs from output data type. Possible programming error.",
             status );
    }

    if (*status != SAI__OK) break;
    if (dosort) times[i-1] = sortinfo[i-1].sortval;

    /* copy data to slice */
    if ( odatad && (data->pntr)[0] ) {
      memcpy( odatad, (data->pntr)[0], szplaneb );
      odatad += szplaneb;
    }
    if ( odatav && (data->pntr)[1] ) {
      memcpy( odatav, (data->pntr)[1], szplaneb );
      odatav += szplaneb;
    }
    if ( odataq && data->qual ) {
      memcpy( odataq, data->qual, szplane * sizeof(*odataq) );
      odataq += szplane; /* increment by number of elements */
    }
    /* output metadata */
    smf_accumulate_prov( data, igrp, sortinfo[i-1].index, outdata->file->ndfid,
                         "SMURF:" TASK_NAME, NULL, status );

    smf_close_file( NULL, &data, status );
  }

  /* Now need to sort out the WCS */

  if (dosort) {
    /* Need a LutMap which transforms grid coord into MJD (in days)
       Can also tweak the TimeFrame attributes a little.
       We do not have to special case the LUT for the case of a single
       observation since this routine enforces a minimum of 2 frames.
       Do put in a TimeOrigin based on the first observation.
       Also assume that observations are
       going to be some distance apart so use iso.0 formatting.
    */
    double origin = 0.0;           /* Origin of time frame */

    if (sortbytime) {
      timefrm = astTimeFrame ( "format=iso.0" );
      origin = floor( times[0] );
      astSetD( timefrm, "TimeOrigin", origin );
    } else {
      timefrm = (AstTimeFrame*)astFrame( 1, "Domain=%s,Label=%s", sortby, sortby );
    }
    for (i = 0; i < size; i++ ) {
      times[i] = times[i] - origin;
    }
    timemap = astLutMap( (int) size, times, 1.0, 1.0, " " );
  } else {
    timefrm = (AstTimeFrame*)astFrame( 1, "Domain=INDEX,Unit(1)=pixel,Label(1)=File Number");
    timemap = (AstLutMap*)astUnitMap( 1, " ");
  }
  times = astFree( times );

  /* Create the new frameset */
  outwcs = astCopy( framewcs );
  atlAddWcsAxis( outwcs, (AstMapping *)timemap, (AstFrame *)timefrm, NULL, NULL,
                 status );

  if (*status == SAI__OK) ndfPtwcs( outwcs, outdata->file->ndfid, status );

  /* Store the bad bits mask */
  if (*status == SAI__OK && odataq) ndfSbb( badbit, outdata->file->ndfid, status );

  /* Write output fits header if we have one */
  if( *status == SAI__OK && fchan && astGetI( fchan, "NCard" ) > 0 ) {
    kpgPtfts( outdata->file->ndfid, fchan, status );
  }

 CLEANUP:
  smf_close_file( NULL, &outdata, status );
  if (sortinfo) sortinfo = astFree( sortinfo );
  grpDelet( &igrp, status );
  grpDelet( &ogrp, status );
  ndfEnd(status);

}
