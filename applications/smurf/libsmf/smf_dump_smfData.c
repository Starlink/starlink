/*
*+
*  Name:
*     smf_dump_smfData

*  Purpose:
*     Print contents of smfData to stdout

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_dump_smfData( const smfData *data, int showflags, int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to input data struct
*     showflags = int (Given)
*        Flags to denote whether to print AST objects
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine attempts to access each component of the given
*     smfData and prints it out to stdout in a human-readable
*     manner. Large arrays and pointers are not printed, only their
*     presence is reported. The showflags parameter may be used to
*     show AST objects.

*  Notes:
*     - Use of showflags is not implemented yet.
*     - Does not query contents of smfDream

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-04-02 (AGG):
*        Initial version
*     2008-04-30 (TIMJ):
*        Add units, title and label.
*     2008-07-24 (TIMJ):
*        Support obstype and obsmode. Minor tidying.
*     2008-07-28 (TIMJ):
*        Add step time
*     2009-05-21 (TIMJ):
*        Add switching mode.
*     2009-06-23 (TIMJ):
*        Add ocsconfig presence indicator
*     2009-09-29 (TIMJ):
*       Report pixel origin.
*     2009-10-02 (TIMJ):
*       Add explicit history listing.
*     2010-03-09 (TIMJ):
*       Change flatfield type in smfDA
*     2010-06-23 (TIMJ):
*        Add Quality family.
*     2010-07-01 (TIMJ):
*        Include dark squid smfData in dump if present
*     2010-08-09 (TIMJ):
*        Report INBEAM
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "dat_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* SC2DA includes */
#include "sc2da/sc2math.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dump_smfData"

void smf_dump_smfData( const smfData *data, int showflags __attribute__((unused)),
                       int *status) {

  /* Local variables */
  smfFile *file;                /* File struct within smfData */
  smfHead *hdr;                 /* Header struct within smfData */
  smfDA *da;                    /* DA struct within smfData */
  dim_t i;                     /* Loop counter */
  dim_t ndims;                 /* Number of dimensions in data */
  char string[SZFITSTR];        /* General purpose string used for printing */
  const char * tempstr = NULL;  /* temp string for obsmode and type */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Check it smfData is NULL */
  if ( data == NULL ) {
    msgOut("", "data = NULL", status);
    return;
  }

  /* SmfData is OK */
  msgOut("", "data = OK", status);

  /* Data type */
  msgSetc("TYP", smf_dtype_string( data, status ) );
  msgOut("", "  dtype = ^TYP", status);

  /* Number of dimensions of data array */
  ndims = data->ndims;
  msgSeti("N",(int)ndims);
  if(ndims > DAT__MXDIM) {
    msgSetc( "MSG", " TOO LARGE!");
  } else {
    msgSetc( "MSG", " ");
  }
  msgOut("", "  ndims = ^N ^MSG", status);


  /* Size of array in each dimension */
  if (ndims <= DAT__MXDIM) {
    for ( i=0; i< ndims; i++) {
      sprintf( string, "%" DIM_T_FMT, (data->dims)[i]);
      msgSetc("D", string);
      if ( i != ndims-1 ) {
        msgSetc("D", "x");
      }
    }
    msgOut("", "  dims  = ^D", status);
  }

  /* Size of array in each dimension */
  if (ndims <= DAT__MXDIM) {
    for ( i=0; i< ndims; i++) {
      sprintf( string, "%" DIM_T_FMT, (data->lbnd)[i]);
      msgSetc("D", string);
      if ( i != ndims-1 ) {
        msgSetc("D", ",");
      }
    }
    msgOut("", "  pixel origin  = ^D", status);
  }

  /* Are data time-ordered? */
  msgSeti("N",data->isTordered);
  msgOut("", "  isTordered = ^N", status);

  /* Reference count for this struct */
  msgSeti("N",data->refcount);
  msgOut("", "  refcount = ^N", status);

  /* Does the smfData overlap the region of interest (map)? */
  msgSeti("N",data->onmap);
  msgOut("", "  onmap = ^N", status);

  /* Is this a virtual smfData? */
  msgSeti("N",data->virtual);
  msgOut("", "  virtual = ^N", status);

  /* State of DATA/VARIANCE/QUALITY pointers */
  if ( (data->pntr)[0] != NULL ) {
    msgSetc("D","D: OK,");
  } else {
    msgSetc("D","D: NULL,");
  }
  if ( (data->pntr)[1] != NULL ) {
    msgSetc("D"," V: OK,");
  } else {
    msgSetc("D"," V: NULL,");
  }
  if ( data->qual != NULL ) {
    msgSetc("D"," Q: OK");
    msgFmt("D", " (family: %s)", smf_qfamily_str( data->qfamily, status ) );
  } else {
    msgSetc("D"," Q: NULL");
  }
  msgOut("", "  pntr = ^D", status);

  /* Do we have polynomial coefficients? */
  if ( data->poly == NULL ) {
    msgSetc("P","NULL");
  } else {
    msgSetc("P","OK");
  }
  msgOut("", "  poly = ^P", status);
  msgSetk("N",data->ncoeff);
  msgOut("", "  ncoeff = ^N", status);

  /* Do we have a pointing lookup table? */
  if ( data->lut == NULL ) {
    msgSetc("P","NULL");
  } else {
    msgSetc("P","OK");
  }
  msgOut("", "  lut = ^P", status);

  /* History */
  if ( data->history == NULL ) {
    msgOut("", "  history = NULL", status );
  } else {
    dim_t nrec = astMapSize( data->history );
    if (nrec == 0) {
      msgOut( "", "  history = empty", status );
    } else {
      for ( i = 0; i < nrec; i++) {
        msgSetc( "H", astMapKey( data->history, (int) i ) );
        if (i != (nrec-1)) msgSetc( "H", ",");
      }
      msgOut( "", "  history = ^H", status );
    }
  }

  /* Now for the structs within a smfData - start with the smfFile */
  if ( data->file == NULL ) {
    msgOut("", "  file = NULL", status);
  } else {
    msgOut("", "  file = OK", status);
    file = data->file;
    /* File descriptor for mem-mapped files */
    msgSeti("F",file->fd);
    msgOut("", "    fd = ^F", status);
    /* NDF ID */
    msgSeti("F",file->ndfid);
    msgOut("", "    ndfid = ^F", status);
    /* Is the file opened by sc2store library? */
    msgSeti("F",file->isSc2store);
    msgOut("", "    isSc2store = ^F", status);
    /* Do we have time series data? */
    msgSeti("F",file->isTstream);
    msgOut("", "    isTstream = ^F", status);
    /* File name on disk */
    msgSetc("F",file->name);
    msgOut("", "    name = ^F", status);
    /* NDF identified for SMURF.MAPCOORD */
    msgSeti("F",file->mapcoordid);
    msgOut("", "    mapcoordid = ^F", status);
  }

  /* smfHead */
  if ( data->hdr == NULL ) {
    msgOut("", "  hdr = NULL", status);
  } else {
    msgOut("", "  hdr = OK", status);
    hdr = data->hdr;
    /* Current STATE structure */
    if ( hdr->state == NULL ) {
      msgOut("", "    state = NULL", status);
    } else {
      msgOut("", "    state = OK", status);
    }
    /* Instrument code */
    /* WCS */
    if ( hdr->wcs == NULL ) {
      msgOut("", "    wcs = NULL", status);
    } else {
      msgOut("", "    wcs = OK", status);
    }
    /* Full WCS for time series data */
    if ( hdr->tswcs == NULL ) {
      msgOut("", "    tswcs = NULL", status);
    } else {
      msgOut("", "    tswcs = OK", status);
    }
    /* FITS header */
    if ( hdr->fitshdr == NULL ) {
      msgOut("", "    fitshdr = NULL", status);
    } else {
      msgOut("", "    fitshdr = OK", status);
    }
    /* Index of current frame */
    msgSeti("I",(int)(hdr->curframe));
    msgOut("", "    curframe = ^I", status);
    /* Number of frames in time series */
    msgSeti("I",(int)(hdr->nframes));
    msgOut("", "    nframes = ^I", status);

    /* obs mode and type */
    tempstr = smf_obsmode_str( hdr->obsmode, status );
    if (tempstr) {
      msgSetc( "MOD", tempstr );
    } else {
      msgSetc( "MOD", "<NULL>" );
    }
    msgSetc( "SW", smf_swmode_str( hdr->swmode, status ));
    tempstr = smf_obstype_str( hdr->obstype, status );
    if (tempstr) {
      msgSetc( "TYP", tempstr );
    } else {
      msgSetc( "TYP", "<NULL>" );
    }
    tempstr = smf_obstype_str( hdr->seqtype, status );
    if (tempstr) {
      msgSetc( "SEQ", tempstr );
    } else {
      msgSetc( "SEQ", "<NULL>" );
    }
    msgOut(" ", "    obsmode=^MOD / switch mode=^SW / obstype=^TYP (^SEQ)", status );

    /* INBEAM */
    smf_inbeam_str( hdr->inbeam, string, sizeof(string), status );
    msgSetc( "INB", string );
    msgOut( " ", "    inbeam = ^INB", status );

    /* Step time */
    msgSetd( "ST", hdr->steptime );
    msgOut(" ", "    step time = ^ST", status);

    /* Ocs configuration */
    msgOutf( " ","    ocsconfig = %s",status, (hdr->ocsconfig ? "present" : "missing" ));

    /* Flag to indicate whether we have control of overall state structure */
    msgSeti("I",hdr->isCloned);
    msgOut("", "    isCloned = ^I", status);
    /* Overall state structure */
    if ( hdr->allState == NULL ) {
      msgOut("", "    allState = NULL", status);
    } else {
      msgOut("", "    allState = OK", status);
    }
    /* Number of focal plane detectors */
    msgSeti("I",(int)(hdr->ndet));
    msgOut("", "    ndet = ^I", status);
    /* X coords of detectors in FP */
    if ( hdr->fplanex == NULL ) {
      msgOut("", "    fplanex = NULL", status);
    } else {
      msgOut("", "    fplanex = OK", status);
    }
    /* Y coords of detectors in FP */
    if ( hdr->fplaney == NULL ) {
      msgOut("", "    fplaney = NULL", status);
    } else {
      msgOut("", "    fplaney = OK", status);
    }
    /* Tracking coords of detectors */
    if ( hdr->detpos == NULL ) {
      msgOut("", "    detpos = NULL", status);
    } else {
      msgOut("", "    detpos = OK", status);
    }
    /* System noise temperatures */
    if ( hdr->tsys == NULL ) {
      msgOut("", "    tsys = NULL", status);
    } else {
      msgOut("", "    tsys = OK", status);
    }
    /* Detector names */
    if ( hdr->detname == NULL ) {
      msgOut("", "    detname = NULL", status);
    } else {
      msgOut("", "    detname = OK", status);
    }
    /* Are tracking coords AzEl? */
    msgSeti("I",hdr->dpazel);
    msgOut("", "    dpazel = ^I", status);
    /* Instrument aperture */
    msgSetd("I",(hdr->instap)[0]);
    msgSetd("J",(hdr->instap)[1]);
    msgOut("", "    instap = ^I, ^J", status);
    /* Telescope position */
    for (i=0; i<3; i++) {
      sprintf(string, " %g", (hdr->telpos)[i]);
      msgSetc("T",string);
      if ( i != 2 ) {
	msgSetc("T",",");
      }
    }
    msgOut("", "    telpos =^T", status);

    msgSetc( "U", hdr->units );
    msgOut(" ","    units = ^U", status);
    msgSetc( "U", hdr->dlabel );
    msgOut(" ","    label = ^U", status);
    msgSetc( "U", hdr->title );
    msgOut(" ","    title = ^U", status);
    msgOutf("","    obsidss = %s", status, hdr->obsidss);
  }

  /* smfDA */
  if ( data->da == NULL ) {
    msgOut("", "  da = NULL", status);
  } else {
    msgOut("", "  da = OK", status);
    da = data->da;
    /* Flatfield calibration */
    if ( da->flatcal == NULL ) {
      msgSetc("F","NULL");
    } else {
      msgSetc("F","OK");
    }
    msgOut("", "    flatcal = ^F", status);
    /* Flatfield parameters */
    if ( da->flatpar == NULL ) {
      msgSetc("F","NULL");
    } else {
      msgSetc("F","OK");
    }
    msgOut("", "    flatpar = ^F", status);
    /* Flatfield algorithm */
    msgSetc("F",smf_flat_methstring(da->flatmeth,status));
    msgOut("", "    flatmeth = ^F", status);
    /* Number of flatfield coefficients per bolometer */
    msgSeti("F",(int)da->nflat);
    msgOut("", "    nflat = ^F", status);
    if (da->refres != VAL__BADD) {
      msgSetd( "RR", da->refres );
    } else {
      msgSetc( "RR", "NOT SET" );
    }
    msgOut("", "    refres = ^RR", status);
    msgOutf("","    heatval = %s", status, ( da->heatval ? "OK" : "NULL" ));
    msgOutf("","    nheat = %zd", status, da->nheat);
    if ( da->dksquid ) {
      msgOut("", " begin dksquid", status );
      smf_dump_smfData( da->dksquid, showflags, status );
      msgOut("", " end dksquid", status);
    } else {
      msgOut( "", "   dksquid = NULL", status );
    }
  }

  /* smfDream */
  if ( data->dream == NULL ) {
    msgOut("", "  dream = NULL", status);
  } else {
    msgOut("", "  dream = OK", status);
  }


  return;
}
