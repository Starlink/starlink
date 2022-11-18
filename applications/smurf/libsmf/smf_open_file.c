/*
 *+
 *  Name:
 *     smf_open_file

 *  Purpose:
 *     Low-level file access function

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_open_file( ThrWorkForce *wf, const Grp * ingrp, size_t index,
 *                    const char * mode, int flags, smfData ** data,
 *                    int *status);

 *  Arguments:
 *     wf = ThrWorkForce * (Given)
 *        Pointer to a pool of worker threads
 *     ingrp = const Grp * (Given)
 *        NDG group identifier
 *     index = size_t (Given)
 *        Index corresponding to required file in group
 *     mode = const char * (Given)
 *        File access mode
 *     flags = int (Given)
 *        Bitmask controls which components are opened. If 0 open everything.
 *     data = smfData ** (Returned)
 *        Pointer to pointer smfData struct to be filled with file info and data
 *        Should be freed using smf_close_file. Will be NULL if this routine completes
 *        with error.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This is the main routine to open data files. The routine finds
 *     the filename from the input Grp and index, and opens the
 *     file. The smfData struct is populated, along with the associated
 *     smfFile, smfHead and smfDA & smfDream (if necessary). The
 *     history is read and stored for future reference.

 *  Notes:
 *     - If a file has no FITS header then a warning is issued
 *     - JCMTState is NULL for non-time series data
 *     - The following bit flags defined in smf_typ.h are used for "flags" par:
 *       SMF__NOCREATE_HEAD: Do not allocate smfHead
 *       SMF__NOCREATE_DATA: Do not map DATA/VARIANCE/QUALITY
 *       SMF__NOCREATE_FTS: Do not allocate smfFts
 *       SMF__NOFIX_METADATA: Do not fix metadata using smf_fix_metadata
 *       SMF__NOFIX_DATA: Do not fix data arrays using smf_fix_data
 *       SMF__NOTTSERIES: File is not a time series file even if 3d
 *       SMF__ISFLAT: File should not be flat-fielded, even if it is _INTEGER.

 *  Authors:
 *     Andy Gibb (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     Edward Chapin (UBC)
 *     David Berry (JAC, UCLan)
 *     Malcolm J. Currie (Starlink)
 *     COBA: Coskun Oba (UoL)
 *     Matt Sherwood (MS, UofL)
 *     {enter_new_authors_here}

 *  History:
 *     2005-11-03 (AGG):
 *        Initial test version
 *     2005-11-07 (TIMJ):
 *        Need to cache locator to FRAMEDATA
 *     2005-11-23 (TIMJ):
 *        Use HDSLoc for locator
 *     2005-11-28 (TIMJ):
 *        Malloc sc2head
 *     2005-12-01 (EC):
 *        Fixed up error determining data types
 *     2005-12-05 (TIMJ):
 *        Store isTstream flag for smf_close_file
 *     2005-12-05 (AGG):
 *        Add status check on retrieving FITS hdr
 *     2005-12-14 (TIMJ):
 *        Now sets a reference counter
 *     2006-01-26 (TIMJ):
 *        Use smf_create_smfData
 *        Use smf_dtype_fromstring
 *     2006-01-27 (TIMJ):
 *        - Open raw data read only
 *        - Read in full time series headers into smfHead during sc2store
 *        - Copy flatfield information into struct and close raw file
 *        - read all time series headers into struct even when not sc2store
 *        - No longer need to store xloc locator
 *     2006-02-17 (AGG):
 *        Add reading of SCANFIT coefficients
 *     2006-03-03 (AGG):
 *        Return a NULL pointer if the group is undefined
 *     2006-03-23 (AGG);
 *        Store the number of frames (timeslices) in the smfData struct
 *     2006-03-24 (TIMJ):
 *        Fix bug where allsc2heads wasn't being set
 *     2006-04-21 (AGG):
 *        Add history read
 *     2006-05-16 (AGG):
 *        Change msgOut to msgOutif
 *     2006-05-19 (EC):
 *        Map Q&V if not present before when mode is WRITE
 *     2006-05-24 (AGG):
 *        Add status check in case SCANFIT extension doesn't exist
 *     2006-06-08 (AGG):
 *        Set correct data type for QUALITY to fix HDS error
 *     2006-06-12 (EC):
 *        NULL pointers associated with .SMURF.MAPCOORD extension
 *     2006-06-30 (EC):
 *        Now NULL pointers in smf_create_smf*, changed to .SCU2RED.MAPCOORD
 *     2006-07-26 (TIMJ):
 *        sc2head no longer used. Use JCMTState instead.
 *     2006-07-28 (TIMJ):
 *        Use new API for sc2store_headrmap. Read cube WCS into tswcs.
 *     2006-07-31 (TIMJ):
 *        Use SC2STORE__MAXFITS.
 *        Calculate "instrument".
 *    2006-08-24 (AGG):
 *        Read and store DREAM parameters (from RAW data only at present)
 *     2006-09-05 (JB):
 *        Check to make sure file exists
 *     2006-09-05 (EC):
 *        Call aztec_fill_smfHead, smf_telpos_get
 *     2006-09-07 (EC):
 *        Added code to isNDF=0 case to handle compressed AzTEC data
 *     2006-09-15 (AGG):
 *        Insert code for opening and storing DREAM parameters
 *     2006-09-21 (AGG):
 *        Check that we have a DREAM extension before attempting to access it
 *     2006-09-21 (AGG):
 *        Move the instrument-specific stuff until after hdr->nframes has
 *        been assigned (nframes is needed by acs_fill_smfHead).
 *     2006-12-20 (TIMJ):
 *        Clean up some error handling.
 *     2007-02-07 (EC):
 *        - renamed isNDF to isFlat to be less confusing
 *        - only issue a warning if data not 2d or 3d to handle iterative
 *          map-maker model containers.
 *     2007-03-28 (TIMJ):
 *        - Annul SCU2RED locator even if SCANFIT is not present
 *        - Noting Ed's comment that isFlat is less confusing, I disagree
 *          because isNDF applied to non-SCUBA2 data
 *     2007-05-29 (AGG):
 *        Check if data type is _REAL and map as _DOUBLE
 *     2007-10-29 (EC):
 *        Add flag controlling header read.
 *     2007-10-31 (TIMJ):
 *        Use size_t following changes to sc2store.
 *     2007-11-28 (EC):
 *        Add check for TORDERED keyword in FITS header
 *     2007-11-28 (TIMJ):
 *        Raw data is now _WORD and can be _INTEGER
 *     2007-12-02 (AGG):
 *        Do not map DATA/VARIANCE/QUALITY if SMF__NOCREATE_DATA flag is set
 *     2008-01-25 (EC):
 *        -removed check for TORDERED FITS keyword
 *        -Added check for SMF__NOCREATE_HEAD when extracting DREAM parameters
 *     2008-02-08 (EC):
 *        -In general map QUALITY unless SMF__NOCREATE_QUALITY set, or
 *         QUALITY doesn't exist, and access mode READ
 *     2008-03-07 (AGG):
 *        Read/create quality names extension
 *     2008-03-10 (AGG):
 *        Factor out quality names code into new routine
 *     2008-04-23 (EC):
 *        Read time series WCS even if the data is raw
 *     2008-04-30 (TIMJ):
 *        Support reading of units, title and data label.
 *     2008-05-01 (DSB):
 *        Ensure AST issues warnings if it tries to return undefined
 *        keyword values via the astGetFits<X> functions.
 *     2008-05-28 (TIMJ):
 *        Make sure return pointer is initialised to NULL even if status is
 *        bad on entry.
 *     2008-06-20 (DSB):
 *        Ensure that sc2store_rdtstream never maps the data array or dark
 *        squid values if the SMF__NOCREATE_DATA flag is supplied.
 *     2008-07-10 (TIMJ):
 *        Read dark squid information.
 *     2008-07-18 (TIMJ):
 *        Use size_t
 *     2008-07-24 (TIMJ):
 *        Calculate obs mode.
 *     2008-07-28 (TIMJ):
 *        Calculate and store steptime
 *     2008-07-31 (TIMJ):
 *        Free any resources if there is an error opening the file.
 *     2008-12-02 (DSB):
 *        Avoid use of astIsUndef functions.
 *     2009-04-06 (TIMJ):
 *        Improve the step time guess to take into account RTS_NUM.
 *        The only guaranteed way to "guess" is to read the embedded XML configuration.
 *     2009-05-26 (TIMJ):
 *        Move TCS_TAI fix up to smf_fix_metadata
 *     2009-08-25 (MJC):
 *        Add star/irq.h include as it is no longer in star/kaplibs.h.
 *     2010-01-28 (TIMJ):
 *        Flatfield output files have mixed type for data and variance
 *        so assume they are raw data for the purposes of reading the
 *        DA extension. Also tidy up some string sizing.
 *     2010-03-09 (TIMJ):
 *        Change type of flatfield method in smfDA
 *     2010-03-16 (TIMJ):
 *        Use one_strlcpy instead of strncpy
 *     2010-05-19 (EC):
 *        Read the SMFMODEL FITS header into hdr->mtype.
 *     2010-05-20 (TIMJ):
 *        Read JCMTSTATE if it is present even for non-3D data.
 *     2010-05-20 (EC):
 *        Store dark squids in a smfData instead of a bare buffer
 *     2010-06-03 (EC):
 *        Map the dark squids even if data were already flatfielded
 *     2010-06-10 (EC):
 *        Add quality to dark squids
 *      2010-06-14 (DSB):
 *        Correct check for read access before creating a new QUALITY array
 *        in the DKSQUID NDF.
 *     2010-06-18 (TIMJ):
 *        Use smf_qual_map to map QUALITY information and store quality family
 *        in smfData.
 *     2010-09-17 (COBA):
 *        Read smfFts
 *     2010-09-21 (COBA):
 *        Add SMF__NOCREATE_FTS
 *     2010-09-22 (COBA):
 *        Add check for status in FTS2 segment
 *     2010-09-23 (TIMJ):
 *        Allow raw data to be read even if NOCREATE_DA is being used
 *     2010-10-19 (COBA):
 *        Validate FTS2 read in operations
 *     2011-05-17 (DSB):
 *        Add pointing corrections onto SMU jiggle positions before
 *        returning.
 *     2011-07-11 (EC):
 *        Call smf_fix_data to repair may/june 2011 s4a row order problem
 *     2011-09-07 (TIMJ):
 *        Be forgiving of 2d images that do not have a full set of FITS
 *        headers.
 *     2011-09-08 (TIMJ):
 *        refres is added to rdtstream
 *     2012-01-19 (DSB):
 *        If the isFFT header cannot be read for any reason, annul the
 *        error and continue.
 *     2014-01-10 (DSB):
 *        Added argument wf.
 *     2014-01-21 (DSB):
 *        Use the STEPTIME and SCANVEL values from the SMURF extension, if
 *        any, in preference to the FITS headers. These extension items are
 *        created by makemap when exporting cleaned time series data.
 *     2015-02-20 (MS)
 *        Added new smfFts fields for quality statistics
 *     2016-10-17 (DSB)
 *        Added SMF__ISFLAT flag to allow LUT models to be opened. These
 *        are _INTEGER arrays holding pixel indices, and so should not be
 *        flat-fielded.
 *     2018-2-28 (DSB):
 *        Use RTS_END to define the length of the JCMTSTATE vectors,
 *        rather than the first component, since the first component may
 *        be a scalar.
 *     2018-9-24 (DSB):
 *        Do not over-write potentially good scanvel and steptime values
 *        in the smfHead with bad values read from the SMURF extension.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2007-2014 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2005-2008,2010-2011 University of British Columbia.
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "star/thr.h"
#include "ndf.h"
#include "ast.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "star/one.h"
#include "kpg_err.h"
#include "prm_par.h"

/* SC2DA includes */
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "libacsis/acsis.h"
#include "libaztec/aztec.h"

static char * smf__read_ocsconfig ( int ndfid, int *status);
static void smf1_open_file_caller( void *data, int *status );


typedef struct smfOpenFileData {
   const Grp *grp;
   size_t index;
   const char *mode;
   int flags;
   smfData **data;
} smfOpenFileData;

#define FUNC_NAME "smf_open_file"

void smf_open_file( ThrWorkForce *wf, const Grp * igrp, size_t index,
                    const char * mode, int flags, smfData ** data,
                    int *status) {

  int canwrite = 0;          /* We can write to the file if true */
  char datatype[NDF__SZTYP+1];  /* String for DATA type */
  char dtype[NDF__SZTYP+1];  /* String for DATA/VARIANCE type */
  int indf = NDF__NOID;      /* NDF identified for input file */
  int isFFT;                 /* FFT data? */
  int lbnd[NDF__MXDIM];      /* Lower pixel bounds of NDF */
  int ubnd[NDF__MXDIM];      /* Upper pixel bounds of NDF */
  int ndfdims[NDF__MXDIM];   /* Array containing size of each axis of array */
  int ndims;                 /* Number of dimensions in data */
  int qexists;               /* Boolean for presence of QUALITY component */
  int vexists;               /* Boolean for presence of VARIANCE component */
  int itexists;              /* Boolean for presence of other components */
  char filename[GRP__SZNAM+1]; /* Input filename, derived from GRP */
  char *pname;               /* Pointer to input filename */
  void *outdata[] = { NULL, NULL }; /* Array of pointers to
                                       output data components:
                                       one each for DATA and VARIANCE */
  smf_qual_t * outqual = NULL;/* Pointer to output quality */
  smf_qfam_t qfamily = SMF__QFAM_NULL; /* Quality family of file */
  int isFlat = 1;            /* Flag to indicate if file flatfielded */
  int isTseries = 0;         /* Flag to specify whether the data are
                                in time series format */
  smf_dtype itype = SMF__NULL; /* Data type for DATA (and VARIANCE) array(s) */
  size_t i;                  /* Loop counter */
  int nout;                  /* Number of output pixels */
  int **ptdata;              /* Pointer to raw time series data (DATA cpt) */
  int *rawts;                /* Raw time series via sc2store */
  smfFile *file = NULL;      /* pointer to smfFile struct */
  smfHead *hdr = NULL;       /* pointer to smfHead struct */
  smfDA *da = NULL;          /* pointer to smfDA struct, initialize to NULL */
  smfFts* fts = NULL;        /* pointer to smfFts struct, initialize to NULL */

  HDSLoc *tloc = NULL;       /* Locator to the NDF JCMTSTATE extension */
  HDSLoc *xloc = NULL;       /* Locator to time series headers,
                                SCANFIT coeffs and DREAM parameters*/

  /* Flatfield parameters */
  double * flatcal = NULL;
  double * flatpar = NULL;
  int * dksquid = NULL;
  int **pdksquid = NULL;
  JCMTState *tmpState = NULL;

  /* DREAM parameters */
  int *jigvert = NULL;       /* Pointer to jiggle vertices in DREAM pattern */
  double *jigpath = NULL;    /* Pointer to jiggle path */
  smfDream *dream = NULL;    /* Pointer to DREAM parameters */
  size_t nsampcycle;         /* Number of positions in jiggle path */
  size_t nvert;              /* Number of vertices in DREAM pattern */
  int jigvndf;               /* NDF identifier for jiggle vertices */
  int jigpndf;               /* NDF identifier for SMU path */
  smfData *jigvdata = NULL;  /* Jiggle vertex data */
  smfData *jigpdata = NULL;  /* SMU path data */

  /* Pasted from readsc2ndf */
  size_t colsize;               /* number of pixels in column */
  char fitsrec[SC2STORE__MAXFITS*SZFITSCARD+1];   /* FITS headers read from sc2store */
  size_t nfits;              /* number of FITS headers */
  size_t nframes;            /* number of frames */
  size_t rowsize;            /* number of pixels in row (returned) */
  int createflags = 0;       /* Flags for smf_create_smfData */

  int gndf;                  /* General purpose NDF identifier (SCU2RED & DREAM) */
  int place;                 /* NDF placeholder for SCANFIT extension */
  int npoly;                 /* Number points in polynomial coeff array */
  void *tpoly[1] = { NULL }; /* Temp array of void for ndfMap */
  double *poly = NULL;       /* Pointer to array of polynomial coefficients */
  double *opoly;             /* Pointer to store in output struct */
  int npdims;                /* Number of dimensions in the polynomial array */
  int pdims[NDF__MXDIM];     /* Size of each dimension */

  /* make sure return pointer is initialised */
  *data = NULL;

  if ( *status != SAI__OK ) return;

  /* Return a null pointer to the smfData if the input grp is null */
  if ( igrp == NULL ) {
    *data = NULL;
    return;
  }

  /* Translate mode into a boolean */
  if (strncmp( mode, "READ", 4) == 0) {
    /* READ mode */
    canwrite = 0;
  } else {
    canwrite = 1;
  }

  /* Get filename from the group */
  pname = filename;
  grpGet( igrp, index, 1, &pname, sizeof(filename), status);

  /* Return the NDF identifier */
  if (*status == SAI__OK) {
    ndgNdfas( igrp, index, mode, &indf, status );
    grpMsg( "F", igrp, index );
    if ( indf == NDF__NOID ) {
      if (*status == SAI__OK) *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Could not locate file ^F", status);
      return;
    }
    msgOutif(MSG__DEBUG, "", "Opening file ^F", status);
  }

  /* Determine the dimensions of the DATA component and pixel origin */
  ndfBound( indf, NDF__MXDIM, lbnd, ubnd, &ndims, status );
  ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );

  /* Check type of DATA and VARIANCE arrays (they should both be the same!) */
  ndfType( indf, "DATA,VARIANCE", dtype, sizeof(dtype), status);
  ndfType( indf, "DATA", datatype, sizeof(datatype), status );

  /* for flatfield output files the variance can be double but the data
     _INTEGER. Since in this case we want the flatfield information
     we force lesser type. */
  if (strncmp( datatype, "_INTEGER", 8) == 0 &&
      strncmp( dtype,    "_DOUBLE", 7) == 0 ) {
    msgOutif( MSG__DEBUG, "",
              "Input file is mixed _INTEGER/_DOUBLE", status );
    one_strlcpy( dtype, "_INTEGER", sizeof(dtype), status );
  }


  /* Check dimensionality: 2D is a .In image, 3D is a time series */
  if (ndims == 2) {
    if (strncmp(dtype, "_REAL", 5) == 0) {
      /* Change _REAL to _DOUBLE */
      msgOutif( MSG__DEBUG, "",
                "Input file is _REAL, will map as _DOUBLE for internal handling",
                status );
      one_strlcpy( dtype, "_DOUBLE", sizeof(dtype), status );
    }
    isFlat = 1;    /* Data have been flat-fielded */
    isTseries = 0; /* Data are not in time series format */
  } else if (ndims == 3) { /* Time series data */
    /* Check if raw timeseries - _WORD, _UWORD or _INTEGER */
    /* Note that _INTEGER may or may not have been flatfielded */
    /* NOTTSERIES is treated as flatfielded to prevent use of sc2store */
    if ( (strncmp(dtype, "_WORD", 5) == 0 ) ||     /* Compressed */
         (strncmp(dtype, "_INTEGER", 8) == 0 ) ||  /* Uncompressed */
         (strncmp(dtype, "_UWORD", 6) == 0 ) ) {   /* Old format */
      isFlat = ( flags & SMF__NOTTSERIES ? 1 : 0);  /* Data may not have been flatfielded */
    } else {
      /* Note that the data should be of type _DOUBLE here */
      isFlat = 1;  /* Data have been flatfielded */
    }
    isTseries = ( flags & SMF__NOTTSERIES ? 0 : 1); /* Data are in time series format */
    isFlat = ( flags & SMF__ISFLAT ? 1 : isFlat);   /* Data does not need flat-fielding */


  } else {
    /* Report a warning due to non-standard dimensions for file */
    if ( *status == SAI__OK) {
      msgSeti( "NDIMS", ndims);
      msgOutif(MSG__DEBUG," ",
               "Number of dimensions in output, ^NDIMS is not equal to 2 or 3",
               status);
      /* Data is neither flat-fielded nor standard time-series data. However
         in this context "flat" data is really just data that doesn't
         need to be de-compressed using the sc2store library, so we set
         isFlat to true */
      isTseries = 0;
      isFlat = 1;
    }
  }

  /* Now we need to create some structures */
  if (flags & SMF__NOCREATE_HEAD) createflags |= SMF__NOCREATE_HEAD;
  if (flags & SMF__NOCREATE_FILE) createflags |= SMF__NOCREATE_FILE;
  if (flags & SMF__NOCREATE_DA)   createflags |= SMF__NOCREATE_DA;
  if (flags & SMF__NOCREATE_FTS)  createflags |= SMF__NOCREATE_FTS;

  /* Allocate memory for the smfData */
  *data = smf_create_smfData( createflags, status );

  /* If all's well, proceed */
  if ( *status == SAI__OK) {
    file = (*data)->file;
    hdr = (*data)->hdr;

    /* If we have timeseries data then look for and read polynomial
       scan fit coefficients */
    if ( isTseries ) {
      ndfXstat( indf, "SCU2RED", &itexists, status );
      if ( itexists) {
        ndfXloc( indf, "SCU2RED", "READ", &xloc, status );
        if ( xloc == NULL ) {
          if ( *status == SAI__OK) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain an HDS locator to the "
                   "SCU2RED extension, despite its existence", status);
          }
        }
        ndfOpen( xloc, "SCANFIT", "READ", "OLD", &gndf, &place, status );
        /* Check status here in case not able to open NDF */
        if ( *status == SAI__OK ) {
          if ( gndf == NDF__NOID ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain an NDF identifier for "
                   "the SCANFIT coefficients", status);
          } else {
            /* Read and store the polynomial coefficients */
            ndfMap( gndf, "DATA", "_DOUBLE", "READ", &tpoly[0], &npoly,
                    status );
            poly = tpoly[0];
            ndfDim( gndf, NDF__MXDIM, pdims, &npdims, status );
            (*data)->ncoeff = pdims[2];
            /* Allocate memory for poly coeffs & copy over */
            opoly = astMalloc( npoly*sizeof( *opoly ) );
            memcpy( opoly, poly, npoly*sizeof( *opoly ) );
            (*data)->poly = opoly;
          }
          /* Release these resources immediately as they're not needed */
          ndfAnnul( &gndf, status );
        } else {
          /* If status is bad, then the SCANFIT extension does not
             exist. This is not fatal so annul the error */
          errAnnul(status);
          msgOutif(MSG__DEBUG," ",
                   "SCU2RED exists, but not SCANFIT - continuing",
                   status);
        }
        /* Annul the locator */
        datAnnul( &xloc, status );

      } else {
        msgOutif(MSG__DEBUG," ",
                 "File has no SCU2RED extension: no DA-processed data present",
                 status);
      }
    }

    // READ IN FTS2 DATA IF EXISTS
    if(!(flags & SMF__NOCREATE_FTS)) {
      int CREATEFLAG = 0;
      int dimsFTS[NDF__MXDIM];
      int hasFTS     = 0;
      int ndfFTS     = 0;
      int ndimsFTS   = 0;
      int nmapFTS    = 0;
      int placeFTS   = 0;
      HDSLoc* hdsFTS = NULL;
      void* pntr     = NULL;

      ndfXstat(indf, "FTS2", &hasFTS, status);
      if(hasFTS) {
        ndfXloc(indf, "FTS2", mode, &hdsFTS, status);
        if(*status == SAI__OK && hdsFTS != NULL) {
          CREATEFLAG |= SMF__NOCREATE_HEAD;
          CREATEFLAG |= SMF__NOCREATE_FILE;
          CREATEFLAG |= SMF__NOCREATE_DA;
          CREATEFLAG |= SMF__NOCREATE_FTS;

          fts = (*data)->fts;

          // READ IN SMFFTS->ZPD
          ndfOpen(hdsFTS, "ZPD", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
          if(*status == SAI__OK && ndfFTS != NDF__NOID) {
            ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
            if(*status == SAI__OK) {
              if(ndimsFTS != 2) {
                *status = SAI__ERROR;
                errRep("", FUNC_NAME ": FTS2->ZPD must be 2-D!", status);
              }
              ndfMap(ndfFTS, "DATA", "_INTEGER", mode, &pntr, &nmapFTS, status);
              if(*status == SAI__OK) {
                fts->zpd = smf_create_smfData(CREATEFLAG, status);
                if(*status == SAI__OK) {
                  size_t count;
                  fts->zpd->dtype   = SMF__INTEGER;
                  fts->zpd->ndims   = ndimsFTS;
                  fts->zpd->dims[0] = dimsFTS[0];
                  fts->zpd->dims[1] = dimsFTS[1];
                  fts->zpd->lbnd[0] = 0;
                  fts->zpd->lbnd[1] = 0;

                  // MAKE A DEEP COPY
                  count = dimsFTS[0] * dimsFTS[1];
                  fts->zpd->pntr[0] = astMalloc( count*sizeof(int) );
                  for(index = 0; index < count; index++) {
                    *((int*) (fts->zpd->pntr[0]) + index) = *((int*) pntr + index);
                  }
                }
              }
            }
          }

          // READ IN SMFFTS->FPM
          ndfOpen(hdsFTS, "FPM", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
          if(*status == SAI__OK && ndfFTS != NDF__NOID) {
            ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
            if(*status == SAI__OK) {
              if(ndimsFTS != 3) {
                *status = SAI__ERROR;
                errRep("", FUNC_NAME ": FTS2->FPM must be 3-D!", status);
              }
              ndfMap(ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
              if(*status == SAI__OK) {
                fts->fpm = smf_create_smfData(CREATEFLAG, status);
                if(*status == SAI__OK) {
                  size_t count;
                  fts->fpm->dtype   = SMF__DOUBLE;
                  fts->fpm->ndims   = ndimsFTS;
                  fts->fpm->dims[0] = dimsFTS[0];
                  fts->fpm->dims[1] = dimsFTS[1];
                  fts->fpm->dims[2] = dimsFTS[2];
                  fts->fpm->lbnd[0] = 0;
                  fts->fpm->lbnd[1] = 0;
                  fts->fpm->lbnd[2] = 1;

                  // MAKE A DEEP COPY
                  count = dimsFTS[0] * dimsFTS[1] * dimsFTS[2];
                  fts->fpm->pntr[0] = astMalloc( count*sizeof(double) );
                  for(index = 0; index < count; index++) {
                    *((double*) (fts->fpm->pntr[0]) + index) = *((double*) pntr + index);
                  }
                }
              }
            }
          }

          // READ IN SMFFTS->SIGMA
          ndfOpen(hdsFTS, "SIGMA", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
          if(*status == SAI__OK && ndfFTS != NDF__NOID) {
            ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
            if(*status == SAI__OK) {
              if(ndimsFTS != 2) {
                *status = SAI__ERROR;
                errRep("", FUNC_NAME ": FTS2->SIGMA must be 2-D!", status);
              }
              ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
              if(*status == SAI__OK) {
                fts->sigma = smf_create_smfData(CREATEFLAG, status);
                if(*status == SAI__OK) {
                  size_t count;
                  fts->sigma->dtype   = SMF__DOUBLE;
                  fts->sigma->ndims   = ndimsFTS;
                  fts->sigma->dims[0] = dimsFTS[0];
                  fts->sigma->dims[1] = dimsFTS[1];
                  fts->sigma->lbnd[0] = 0;
                  fts->sigma->lbnd[1] = 0;

                  // MAKE A DEEP COPY
                  count = dimsFTS[0] * dimsFTS[1];
                  fts->sigma->pntr[0] = astMalloc( count*sizeof(double) );
                  for(index = 0; index < count; index++) {
                    *((double*) (fts->sigma->pntr[0]) + index) = *((double*) pntr + index);
                  }
                }
              }
            }
          }

          /* Read in SMFFTS->DEAD */
          ndfOpen(hdsFTS, "DEAD", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
          if(*status == SAI__OK && ndfFTS != NDF__NOID) {
              ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
              if(*status == SAI__OK) {
                  if(ndimsFTS != 2) {
                      *status = SAI__ERROR;
                      errRep("", FUNC_NAME ": FTS2->DEAD must be 2-D!", status);
                  }
                  ndfMap( ndfFTS, "DATA", "_INTEGER", mode, &pntr, &nmapFTS, status);
                  if(*status == SAI__OK) {
                      fts->dead = smf_create_smfData(CREATEFLAG, status);
                      if(*status == SAI__OK) {
                          size_t count;
                          fts->dead->dtype   = SMF__INTEGER;
                          fts->dead->ndims   = ndimsFTS;
                          fts->dead->dims[0] = dimsFTS[0];
                          fts->dead->dims[1] = dimsFTS[1];
                          fts->dead->lbnd[0] = 0;
                          fts->dead->lbnd[1] = 0;

                          /* Make a deep copy */
                          count = dimsFTS[0] * dimsFTS[1];
                          fts->dead->pntr[0] = astMalloc( count*sizeof(int) );
                          for(index = 0; index < count; index++) {
                              *((int*) (fts->dead->pntr[0]) + index) = *((int*) pntr + index);
                          }
                       }
                    }
                }
            }

            /* Read in SMFFTS->A */
            ndfOpen(hdsFTS, "A", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->A must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->a = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->a->dtype   = SMF__DOUBLE;
                            fts->a->ndims   = ndimsFTS;
                            fts->a->dims[0] = dimsFTS[0];
                            fts->a->dims[1] = dimsFTS[1];
                            fts->a->lbnd[0] = 0;
                            fts->a->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->a->pntr[0] = astMalloc( count*sizeof(double) );
                            for(index = 0; index < count; index++) {
                                *((double*) (fts->a->pntr[0]) + index) = *((double*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->B */
            ndfOpen(hdsFTS, "B", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->B must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                         fts->b = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->b->dtype   = SMF__DOUBLE;
                            fts->b->ndims   = ndimsFTS;
                            fts->b->dims[0] = dimsFTS[0];
                            fts->b->dims[1] = dimsFTS[1];
                            fts->b->lbnd[0] = 0;
                            fts->b->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->b->pntr[0] = astMalloc( count*sizeof(double) );
                            for(index = 0; index < count; index++) {
                                *((double*) (fts->b->pntr[0]) + index) = *((double*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->C */
            ndfOpen(hdsFTS, "C", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->C must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->c = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->c->dtype   = SMF__DOUBLE;
                            fts->c->ndims   = ndimsFTS;
                            fts->c->dims[0] = dimsFTS[0];
                            fts->c->dims[1] = dimsFTS[1];
                            fts->c->lbnd[0] = 0;
                            fts->c->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->c->pntr[0] = astMalloc( count*sizeof(double) );
                            for(index = 0; index < count; index++) {
                                *((double*) (fts->c->pntr[0]) + index) = *((double*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->D */
            ndfOpen(hdsFTS, "D", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->D must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->d = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->d->dtype   = SMF__DOUBLE;
                            fts->d->ndims   = ndimsFTS;
                            fts->d->dims[0] = dimsFTS[0];
                            fts->d->dims[1] = dimsFTS[1];
                            fts->d->lbnd[0] = 0;
                            fts->d->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->d->pntr[0] = astMalloc( count*sizeof(double) );
                            for(index = 0; index < count; index++) {
                                *((double*) (fts->d->pntr[0]) + index) = *((double*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->PHASEFIT */
            ndfOpen(hdsFTS, "PHASEFIT", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->PHASEFIT must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_DOUBLE", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->phaseFit = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->phaseFit->dtype   = SMF__DOUBLE;
                            fts->phaseFit->ndims   = ndimsFTS;
                            fts->phaseFit->dims[0] = dimsFTS[0];
                            fts->phaseFit->dims[1] = dimsFTS[1];
                            fts->phaseFit->lbnd[0] = 0;
                            fts->phaseFit->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->phaseFit->pntr[0] = astMalloc( count*sizeof(double) );
                            for(index = 0; index < count; index++) {
                                *((double*) (fts->phaseFit->pntr[0]) + index) = *((double*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->COSMICRAYS */
            ndfOpen(hdsFTS, "COSMICRAYS", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->COSMICRAYS must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_INTEGER", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->cosmicRays = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->cosmicRays->dtype   = SMF__INTEGER;
                            fts->cosmicRays->ndims   = ndimsFTS;
                            fts->cosmicRays->dims[0] = dimsFTS[0];
                            fts->cosmicRays->dims[1] = dimsFTS[1];
                            fts->cosmicRays->lbnd[0] = 0;
                            fts->cosmicRays->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->cosmicRays->pntr[0] = astMalloc( count*sizeof(int) );
                            for(index = 0; index < count; index++) {
                                *((int*) (fts->cosmicRays->pntr[0]) + index) = *((int*) pntr + index);
                            }
                        }
                    }
                }
            }

            /* Read in SMFFTS->FLUXJUMPS */
            ndfOpen(hdsFTS, "FLUXJUMPS", mode, "UNKNOWN", &ndfFTS, &placeFTS, status);
            if(*status == SAI__OK && ndfFTS != NDF__NOID) {
                ndfDim(ndfFTS, NDF__MXDIM, dimsFTS, &ndimsFTS, status);
                if(*status == SAI__OK) {
                    if(ndimsFTS != 2) {
                        *status = SAI__ERROR;
                        errRep("", FUNC_NAME ": FTS2->FLUXJUMPS must be 2-D!", status);
                    }
                    ndfMap( ndfFTS, "DATA", "_INTEGER", mode, &pntr, &nmapFTS, status);
                    if(*status == SAI__OK) {
                        fts->fluxJumps = smf_create_smfData(CREATEFLAG, status);
                        if(*status == SAI__OK) {
                            size_t count;
                            fts->fluxJumps->dtype   = SMF__INTEGER;
                            fts->fluxJumps->ndims   = ndimsFTS;
                            fts->fluxJumps->dims[0] = dimsFTS[0];
                            fts->fluxJumps->dims[1] = dimsFTS[1];
                            fts->fluxJumps->lbnd[0] = 0;
                            fts->fluxJumps->lbnd[1] = 0;

                            /* Make a deep copy */
                            count = dimsFTS[0] * dimsFTS[1];
                            fts->fluxJumps->pntr[0] = astMalloc( count*sizeof(int) );
                            for(index = 0; index < count; index++) {
                                *((int*) (fts->fluxJumps->pntr[0]) + index) = *((int*) pntr + index);
                            }
                        }
                    }
                }
            }

          // CLEANUP FTS2
          if(hdsFTS) { datAnnul(&hdsFTS, status); }
        }
      }
    }

    if (isFlat) {

      /* Map the DATA, VARIANCE and QUALITY if requested */
      if ( !(flags & SMF__NOCREATE_DATA) ) {
        ndfState( indf, "QUALITY", &qexists, status);
        ndfState( indf, "VARIANCE", &vexists, status);

        /* If access mode is READ, map the QUALITY array only if it
           already existed, and SMF__NOCREATE_QUALITY is not set. However,
           if the access mode is not READ, create QUALITY by default. */

        if ( !(flags & SMF__NOCREATE_QUALITY) &&
             ( qexists || canwrite ) ) {
          size_t nqout;
          char qmode[NDF__SZMMD+1];

          /* Now map the data with the appropriate mode */
          qmode[0] = '\0';
          if ( qexists ) {
            one_strlcpy( qmode, mode, sizeof(qmode), status );

          } else if ( canwrite ) {
            one_strlcpy( qmode, "WRITE/ZERO", sizeof(qmode), status );
          }
          outqual = smf_qual_map( wf, indf, qmode, &qfamily, &nqout, status );

          /* Since we may not technically be mapping the QUALITY at this
             point (if it was mapped, copied, unmapped) we have to tell
             NDF not to use automatic quality masking */
          ndfSqmf( 0, indf, status );

        }

        /* Always map DATA if we get this far */
        ndfMap( indf, "DATA", dtype, mode, &outdata[0], &nout, status );

        /* Default behaviour is to map VARIANCE only if it exists already. */
        if (vexists) {
          ndfMap( indf, "VARIANCE", dtype, mode, &outdata[1], &nout, status );
        }
      }

      /* Map the DA information (just dksquid at present) */
      if ( !(flags & SMF__NOCREATE_DA) && (*data)->da ) {
        HDSLoc *dkloc=NULL;
        int dkdims[NDF__MXDIM];
        smfFile *dkfile=NULL;
        int dkndims;
        int dkndf;
        int dkplace;
        int dqexists;
        char qmode[NDF__SZMMD+1];
        int nmap;
        void *dpntr[] = {NULL,NULL};
        smf_qual_t * qpntr = NULL;

        ndfXstat( indf, "SCUBA2", &itexists, status );
        if( itexists ) {
          ndfXloc( indf, "SCUBA2", mode, &dkloc, status );
          if( (!dkloc) && (*status==SAI__OK) ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain an HDS locator to the "
                   "SCUBA2 extension, despite its existence", status);
          }
          ndfOpen( dkloc, "DKSQUID", mode, "UNKNOWN", &dkndf, &dkplace, status);
          if( (dkndf==NDF__NOID) && (*status==SAI__OK) ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain an NDF identifier for "
                   "the DKSQUID", status);
          }
          ndfDim( dkndf, NDF__MXDIM, dkdims, &dkndims, status );
          if( (dkndims!=2) && (*status==SAI__OK) ) {
            *status = SAI__ERROR;
            errRepf("", FUNC_NAME ": DKSQUID has %i dimensions, should be 2",
                    status, dkndims);
          }

          /* Also generically map/create a quality array unless we're
             in READ mode and one didn't previously exist */
          ndfState( dkndf, "QUALITY", &dqexists, status );
          qmode[0] = '\0';
          if ( dqexists ) {
            one_strlcpy( qmode, mode, sizeof(qmode), status );
            msgOutif( MSG__DEBUG, "", "Mapping existing DKSQUID QUALITY",
                      status);
          } else if ( canwrite ) {
            one_strlcpy( qmode, "WRITE/ZERO", sizeof(qmode), status );
            msgOutif( MSG__DEBUG, "", "Creating new DKSQUID QUALITY",
                      status);
          }
          if (strlen(qmode)) {
            size_t nqmap;
            qpntr = smf_qual_map( wf, dkndf, qmode, NULL, &nqmap, status );
          }

          /* Map DATA after quality to prevent automatic quality masking */
          ndfMap( dkndf, "DATA", "_DOUBLE", mode, &dpntr[0], &nmap, status );

          if(*status == SAI__OK) {
            da = (*data)->da;
            da->dksquid = smf_create_smfData( SMF__NOCREATE_HEAD |
                                              SMF__NOCREATE_DA |
                                              SMF__NOCREATE_FTS, status );
            da->dksquid->pntr[0] = dpntr[0];
            da->dksquid->qual = qpntr;
            da->dksquid->qfamily = SMF__QFAM_TSERIES;

            da->dksquid->dtype = SMF__DOUBLE;
            da->dksquid->isTordered = 1;
            da->dksquid->isFFT = -1;
            da->dksquid->ndims = 3;
            da->dksquid->dims[0] = dkdims[0];
            da->dksquid->dims[1] = 1;
            da->dksquid->dims[2] = dkdims[1];
            da->dksquid->lbnd[0] = 0;
            da->dksquid->lbnd[1] = 0;
            da->dksquid->lbnd[2] = 1;

            dkfile = da->dksquid->file;
            dkfile->ndfid = dkndf;
            dkfile->isSc2store = 0;
            dkfile->isTstream = 1;
          }
        }
      }

      /* Map the header */
      if ( !(flags & SMF__NOCREATE_HEAD) ) {
        /* Read the units and data label */
        ndfCget( indf, "UNITS", hdr->units, SMF__CHARLABEL, status );
        ndfCget( indf, "LABEL", hdr->dlabel, SMF__CHARLABEL, status );
        ndfCget( indf, "TITLE", hdr->title, SMF__CHARLABEL, status );

        /* Read the FITS headers */
        kpgGtfts( indf, &(hdr->fitshdr), status );
        /* Just continue if there are no FITS headers */
        if ( *status == KPG__NOFTS ) {
          errRep("", FUNC_NAME ": File has no FITS header - continuing but "
                 "this may cause problems later", status );
          errAnnul( status );
        }

        /* Check status is good so we can be sure any bad status found
           below is generated by smf_fits_getI. */
        if( *status == SAI__OK ) {

        /* Are the data the FFT of something? If there is no FITS keyword
           data->isFFT will be left in the unknown state of -1 */
          smf_fits_getI( hdr, "ISFFT", &isFFT, status );
          if( *status == SAI__OK ) {
            (*data)->isFFT = isFFT;
          }

        /* If we have a FITS header but no keyword, that means we haven't
           ever run these data through smf_fft_data so it is safe to assume
           that this is not the FFT of something */
          if( *status == SMF__NOKWRD ) {
            (*data)->isFFT = -1;
            errAnnul( status );
          }

        /* We simply leave isFFT in the default unknown state (-1) if there
           was no FITS header, but we annul the bad status so we can continue */
          if ( *status != SAI__OK ) {
            errAnnul( status );
          }
        }

        /* Determine the instrument - assume that header fixups are not required for this */
        smf_inst_get( hdr, &(hdr->instrument), &(hdr->realinst), status );

        /* We always try to read the JCMTSTATE even if we don't expect it to be
           here. This lets us write out map models that have strangely compressed
           data arrays and be able to expand them again */
        hdr->nframes = 0;
        if (*status == SAI__OK) {
          /* Need to get the location of the extension for STATE parsing */
          ndfXloc( indf, JCMT__EXTNAME, "READ", &tloc, status );

          /* Only read the state header if available */
          if( *status == SAI__OK ) {

            if ( isTseries ) {
              /* Re-size the arrays in the JCMTSTATE extension to match the
                 pixel index bounds of the NDF. The resized arrays are stored in
                 a new temporary HDS object, and the old locator is annull. */
              sc2store_resize_head( indf, &tloc, &xloc, status );
              nframes = ndfdims[2];
            } else {
              /* Make sure the locator is in the right variable */
              xloc = tloc;
              tloc = NULL;

              /* Find out how many state structures we really have without
                 assuming we know the answer from the primary data array. This
                 is more annoying than it should be because we have to open up
                 one of the state entries, close it and then reopen it again. */
              datFind( xloc, "RTS_END", &tloc, status );
              datSize( tloc, &nframes, status );
              datAnnul( &tloc, status );

            }
            /* And need to map the header making sure we have the right
               components for this instrument. */

            sc2store_headrmap( xloc, nframes, hdr->instrument, status );

            /* Malloc some memory to hold all the time series data */
            hdr->allState = astCalloc( nframes, sizeof(*(hdr->allState)) );

            /* Loop over each element, reading in the information */
            tmpState = hdr->allState;
            for (i=0; i<nframes; i++) {
              sc2store_headget(i, &(tmpState[i]), status);
            }
            hdr->nframes = nframes;
            /* Unmap the headers */
            sc2store_headunmap( status );
          } else if (*status==NDF__NOEXT) {
            /* If the header just wasn't there, annul status and continue */
            errAnnul( status );
            msgOutif( MSG__DEBUG, "",
                      FUNC_NAME
                      ": File has no JCMTState information continuing anyways",
                      status );
          }

          /* Annul the locator in any case */
          if( xloc ) datAnnul( &xloc, status );
        }

        /* If not time series, then we can retrieve the stored WCS info. */
        if ( !isTseries ) {
          ndfGtwcs( indf, &(hdr->wcs), status);
          if (hdr->nframes == 0) hdr->nframes = 1;
        } else {
          /* Get the time series WCS */
          ndfGtwcs( indf, &(hdr->tswcs), status );

          /* Get the obsidss */
          if( hdr->fitshdr ) {
            (void )smf_getobsidss( hdr->fitshdr, NULL, 0, hdr->obsidss,
                                   sizeof(hdr->obsidss), status );
          }

          /* Read the OCS configuration xml */
          hdr->ocsconfig = smf__read_ocsconfig( indf, status );

          /* Metadata corrections - hide the messages by default.
             Only correct time series data at the moment.
          */
          if ( !(flags & SMF__NOFIX_METADATA) && isTseries ) smf_fix_metadata( MSG__DEBUG, *data, status );

        }

        /* and work out the observing mode (assumes fixed headers and we
           do not get upset if this fails for NOTTSERIES data) */
        if (*status == SAI__OK && hdr->fitshdr) {
          smf_calc_mode( hdr, status );
          if (!isTseries && *status != SAI__OK) errAnnul(status);
        }

        /* Determine and store the telescope location in hdr->telpos */
        smf_telpos_get( hdr, status );

        /* Store the INSTAP values */
        smf_instap_get( hdr, status );

        /* On the basis of the instrument, we know need to fill in some
           additional header parameters. Some of these may be constants,
           whereas others may involve more file access. Currently we use
           a simple switch statement. We could modify this step to use
           vtables of function pointers.
        */
        switch ( hdr->instrument ) {
        case INST__ACSIS:
          if (isTseries) acs_fill_smfHead( hdr, indf, status );
          break;
        case INST__AZTEC:
          aztec_fill_smfHead( hdr, NDF__NOID, status );
          break;
        default:
          break;
          /* SCUBA-2 has nothing special here because the focal plane
             coordinates are derived using an AST polyMap */
        }

      }
      /* Establish the data type */
      itype = smf_dtype_fromstring( dtype, status );

      /* Store NDF identifier and set isSc2store to false */
      if (*status == SAI__OK) {
        file->ndfid = indf;
        file->isSc2store = 0;
        file->isTstream = isTseries;
      }
    } else {
      char units[SC2STORE_UNITLEN];
      char dlabel[SC2STORE_LABLEN];
      char flatname[SC2STORE_FLATLEN];
      size_t nflat = 0;
      double refres = VAL__BADD;

      /* Get the time series WCS if header exists */
      if( hdr ) {
        ndfGtwcs( indf, &(hdr->tswcs), status );
      }

      /* Read the OCS configuration xml whilst we have the file open */
      if ( !(flags & SMF__NOCREATE_HEAD) ) hdr->ocsconfig = smf__read_ocsconfig( indf, status );

      /* OK, we have raw data. Close the NDF because
         sc2store_rdtstream will open it again */
      ndfAnnul( &indf, status );

      /* Read time series data from file */
      da = (*data)->da;
      if (*status == SAI__OK && !(flags & SMF__NOCREATE_DA) && da == NULL) {
        *status = SAI__ERROR;
        errRep("", FUNC_NAME ": Internal programming error. Status good but "
               "no DA struct allocated", status);
      }

      /* decide if we are storing header information */
      tmpState = NULL;

      /* If access to the data is not required, pass NULL pointers to
         sc2store_rdtstream. Otherwise, use pointers to the relavent buffers. */
      if ( flags & SMF__NOCREATE_DATA ) {
        ptdata = NULL;
        pdksquid = NULL;
      } else {
        ptdata = &rawts;
        pdksquid = &dksquid;
      }

      /* Read time series data from file */
      sc2store_force_initialised( status );
      sc2store_rdtstream( pname, "READ", SC2STORE_FLATLEN,
                          SC2STORE__MAXFITS,
                          &nfits, fitsrec, units, dlabel, &colsize, &rowsize,
                          &nframes, &nflat, &refres, flatname,
                          &tmpState, ptdata, pdksquid,
                          &flatcal, &flatpar, &jigvert, &nvert, &jigpath,
                          &nsampcycle, status);
      if (ptdata) outdata[0] = rawts;

      if (da) da->flatmeth = smf_flat_methcode( flatname, status );
      if (da) da->nflat = nflat;
      if (da) da->refres = refres;

      if (*status == SAI__OK) {
        /* Free header info if no longer needed */
        if ( (flags & SMF__NOCREATE_HEAD) && tmpState != NULL) {
          /* can not use smf_free */
          astFree( tmpState );
          tmpState = NULL;
        } else {
          hdr->allState = tmpState;
        }

        /* Populate the DA struct if we have one */
        if (da) {
          /* Malloc local copies of the flatfield information.
             This allows us to close the file immediately so that
             we do not need to worry about sc2store only allowing
             a single file at a time */
          da->flatcal = astMalloc( colsize * rowsize * da->nflat *
                                   sizeof(*(da->flatcal)) );
          da->flatpar = astMalloc( (da->nflat)*sizeof(*(da->flatpar)) );

          /* Now copy across from the mapped version */
          if (da->flatcal != NULL) memcpy(da->flatcal, flatcal,
                                          sizeof(*(da->flatcal))*colsize*
                                          rowsize* da->nflat);
          if (da->flatpar != NULL) memcpy(da->flatpar, flatpar,
                                          sizeof(*(da->flatpar))* da->nflat);

          /* and dark squids -- we typecast here as a double and store
             in a 3d smfData with the row axis having length 1. */
          if (dksquid) {
            da->dksquid = smf_create_smfData(SMF__NOCREATE_FILE |
                                             SMF__NOCREATE_HEAD |
                                             SMF__NOCREATE_DA |
                                             SMF__NOCREATE_FTS, status );
            da->dksquid->dtype = SMF__DOUBLE;
            da->dksquid->isTordered = 1;
            da->dksquid->ndims = 3;
            da->dksquid->dims[0] = rowsize;
            da->dksquid->dims[1] = 1;
            da->dksquid->dims[2] = nframes;
            da->dksquid->lbnd[0] = 0;
            da->dksquid->lbnd[1] = 0;
            da->dksquid->lbnd[2] = 1;

            da->dksquid->pntr[0] = astMalloc( rowsize*nframes*
                                              smf_dtype_size(da->dksquid,
                                                             status) );

            /* Convert to double precision when we copy into da->dksquid */
            if( *status == SAI__OK ) {
              double *ptr = da->dksquid->pntr[0];
              for( i=0; i<(rowsize*nframes); i++ ) {
                ptr[i] = (double) dksquid[i];
              }
            }

            /* Create an empty QUALITY array */
            da->dksquid->qual = astCalloc( rowsize*nframes,
                                           sizeof(*(da->dksquid->qual)));
            da->dksquid->qfamily = SMF__QFAM_TSERIES;
          }
        }

        /* Create a FitsChan from the FITS headers */
        if ( !(flags & SMF__NOCREATE_HEAD) ) {
          smf_fits_crchan( nfits, fitsrec, &(hdr->fitshdr), status);

          /* Instrument must be SCUBA-2 */
          /* hdr->instrument = INST__SCUBA2; */

          /* ---------------------------------------------------------------*/
          /* WARNING: This has been duplicated from the "isFlat" case to
             accomodate AzTEC data that was written using sc2sim_ndfwrdata.
             In principle AzTEC data should not be compressed, in which case
             the above assertion "Instrument must be SCUBA-2" would be
             correct, and the following code is unnecessary. */

          /* Are the data the FFT of something? If there is no FITS keyword
             data->isFFT will be left in the unknown state of -1 */
          smf_fits_getI( hdr, "ISFFT", &isFFT, status );
          if( *status == SAI__OK ) {
            (*data)->isFFT = isFFT;
          }

          /* If we have a FITS header but no keyword, that means we haven't
             ever run these data through smf_fft_data so it is safe to assume
             that this is not the FFT of something */
          if( *status == SMF__NOKWRD ) {
            (*data)->isFFT = -1;
            errAnnul( status );
          }

          /* We simply leave isFFT in the default unknown state (-1) if there
             was no FITS header, but we annul the bad status so we can
             continue */
          if ( *status == AST__FUNDEF  ) {
            errAnnul( status );
          }

          /* Determine the instrument */
          smf_inst_get( hdr, &(hdr->instrument), &(hdr->realinst), status );

          /* We need to assign a number of frames to the header just in case
             it is needed in the metadata fixup */
          hdr->nframes = nframes;

          /* Store units and label */
          one_strlcpy( hdr->units, units, sizeof(hdr->units), status );
          one_strlcpy( hdr->dlabel, dlabel, sizeof(hdr->dlabel), status );

          /* Metadata corrections - hide the messages by default */
          if ( !(flags & SMF__NOFIX_METADATA) ) smf_fix_metadata( MSG__DEBUG, *data, status );

          if (hdr->fitshdr) {
            /* and work out the observing mode */
            smf_calc_mode( hdr, status );

            /* Get the obsidss */
            (void )smf_getobsidss( hdr->fitshdr, NULL, 0, hdr->obsidss,
                                   sizeof(hdr->obsidss), status );
          }

          /* Determine and store the telescope location in hdr->telpos */
          smf_telpos_get( hdr, status );

          /* Store the INSTAP values */
          smf_instap_get( hdr, status );

          /* On the basis of the instrument, we know need to fill in some
             additional header parameters. Some of these may be constants,
             whereas others may involve more file access. Currently we use
             a simple switch statement. We could modify this step to use
             vtables of function pointers.
          */
          switch ( hdr->instrument ) {
          case INST__ACSIS:
            acs_fill_smfHead( hdr, indf, status );
            break;
          case INST__AZTEC:
            aztec_fill_smfHead( hdr, NDF__NOID, status );
            break;
          default:
            break;
            /* SCUBA-2 has nothing special here because the focal plane
               coordinates are derived using an AST polyMap */
          }

          /* ---------------------------------------------------------------*/
        }

        /* Raw data type is integer */
        itype = SMF__INTEGER;

        /* Verify that ndfdims matches row, col, nframes */
        /* Should probably inform user of the filename too */
        if (ndfdims[SC2STORE__ROW_INDEX] != (int)colsize) {
          msgSeti( "NR", colsize);
          msgSeti( "DIMS", ndfdims[0]);
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": Number of input rows not equal to the "
                  "number of output rows (^NR != ^DIMS)",status);
        }
        if (ndfdims[SC2STORE__COL_INDEX] != (int)rowsize) {
          msgSeti( "NC", rowsize);
          msgSeti( "DIMS", ndfdims[1]);
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ":Number of input columns not equal to the "
                  "number of output columns (^NC != ^DIMS)",status);
        }
        if (ndfdims[2] != (int)nframes) {
          msgSeti( "NF", nframes);
          msgSeti( "DIMS", ndfdims[2]);
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": Number of input timeslices not equal to "
                  "the number of output timeslices (^NF != ^DIMS)",status);
        } else {
          if ( !(flags & SMF__NOCREATE_HEAD) ) {
            hdr->nframes = nframes;
          }
        }

        /* Set flag to indicate data read by sc2store_() */
        file->isSc2store = 1;

        /* and it is a time series */
        file->isTstream = 1;

        /* Store DREAM parameters */
        if ( !(flags & SMF__NOCREATE_HEAD) ) {
          dream = smf_construct_smfDream( *data, nvert, nsampcycle, jigvert,
                                          jigpath, status );
          (*data)->dream = dream;
        }
      }

      /* Close the file */
      sc2store_free( status );

    }
    /* Store info in smfData struct */
    if (*status == SAI__OK) {
      (*data)->dtype = itype;
      one_strlcpy( file->name, pname, sizeof(file->name), status );

      /* Store the data in the smfData struct if needed */
      if ( !(flags & SMF__NOCREATE_DATA) ) {
        for (i=0; i<2; i++) {
          ((*data)->pntr)[i] = outdata[i];
        }
        (*data)->qual = outqual;

        /* if this is time series data then we have time series quality
           if we have no additional information. */
        if (qfamily == SMF__QFAM_NULL && isTseries) qfamily = SMF__QFAM_TSERIES;
        (*data)->qfamily = qfamily;
      }
      /* Store the dimensions, bounds and the size of each axis */
      (*data)->ndims = ndims;
      for (i=0; i< (size_t)ndims; i++) {
        ((*data)->dims)[i] = (dim_t)ndfdims[i];
        ((*data)->lbnd)[i] = lbnd[i];
      }
    }
    /* Store DREAM parameters for flatfielded data if they exist. This
       has to be done here as these methods rely on information in the
       main smfData struct. First retrieve jigvert and jigpath from
       file */
    if ( isTseries && isFlat && !(flags & SMF__NOCREATE_HEAD) ) {
      /* Obtain locator to DREAM extension if we have DREAM data */
      xloc = smf_get_xloc( *data, "DREAM", "DREAM_PAR", "READ", 0, 0, status );
      /* If it's NULL then we don't have dream data */
      if ( xloc != NULL ) {
        jigvndf = smf_get_ndfid( xloc, "JIGVERT", "READ", "OLD", "", 0, NULL,
                                 NULL, status);
        if ( jigvndf == NDF__NOID) {
          if (*status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain NDF ID for JIGVERT",
                   status);
          }
        } else {
          smf_open_ndf( jigvndf, "READ", SMF__INTEGER, &jigvdata, status);
        }
        jigpndf = smf_get_ndfid( xloc, "JIGPATH", "READ", "OLD", "", 0, NULL,
                                 NULL, status);
        if ( jigpndf == NDF__NOID) {
          if (*status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": Unable to obtain NDF ID for JIGPATH",
                   status);
          }
        } else {
          smf_open_ndf( jigpndf, "READ", SMF__DOUBLE, &jigpdata, status);
        }
        if ( jigvdata != NULL ) {
          jigvert = (jigvdata->pntr)[0];
          nvert = (int)(jigvdata->dims)[0];
        } else {
          if (*status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": smfData for jiggle vertices is NULL",
                   status);
          }
        }
        if ( jigpdata != NULL ) {
          jigpath = (jigpdata->pntr)[0];
          nsampcycle = (int)(jigpdata->dims)[0];
        } else {
          if (*status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep("", FUNC_NAME ": smfData for SMU path is NULL", status);
          }
        }

        dream = smf_construct_smfDream( *data, nvert, nsampcycle, jigvert,
                                        jigpath, status );
        (*data)->dream = dream;

        /* Free up the smfDatas for jigvert and jigpath */
        if ( jigvert != NULL ) {
          smf_close_file( wf, &jigvdata, status );
        }
        if ( jigpath != NULL ) {
          smf_close_file( wf, &jigpdata, status );
        }
        datAnnul( &xloc, status );
      }
    }
  }
  /* Read and store history */
  smf_history_read( *data, status );

  if ( hdr &&  (hdr->instrument!=INST__NONE) && *status == SAI__OK ) {
    double steptime = VAL__BADD;
    double scanvel = VAL__BADD;

    /* Store the STEPTIME in the hdr - assumes that smf_fix_metadata has
       fixed things up or complained. If there is a STEPTIME value in the
       SMURF NDF extension we use it in preference to the FITS header.
       Such an extension item will be present if the data has been
       pre-cleaned by a previous run of makemap, and will be the value
       actually used by the previous run of makemap. Similarly get the
       SCAN_VEL (in arcsec/sec). Ignore the extension values if they are
       bad. */
    smf_getfitsd( hdr, "STEPTIME", &steptime, status );
    smf_getfitsd( hdr, "SCAN_VEL", &scanvel, status );
    if( file && file->ndfid != NDF__NOID ) {
      int there = 0;
      ndfXstat( file->ndfid, SMURF__EXTNAME, &there, status );
      if( there ) {
         ndfXgt0d( file->ndfid, SMURF__EXTNAME, "STEPTIME", &steptime, status );
         ndfXgt0d( file->ndfid, SMURF__EXTNAME, "SCAN_VEL", &scanvel, status );
      }
    }
    if( scanvel != VAL__BADD ) hdr->scanvel = scanvel;
    if( steptime != VAL__BADD ) hdr->steptime = steptime;

    /* If this looks like a SCUBA-2 image but we are missing
       STEPTIME or SCAN_VEL we do not really mind if this is
       not time series data */
    if (!isTseries && *status != SAI__OK) errAnnul(status);

  }

  /* report data units */
  if (hdr) {
    msgOutiff( MSG__DEBUG, "", "Data read with label '%s (%s)'",
               status, (strlen(hdr->dlabel) ? hdr->dlabel : "<none>"),
               (strlen(hdr->units) ? hdr->units : ""));
  }

  /* Check to see if this is an iterative map-maker model */
  if( hdr && hdr->fitshdr && *status == SAI__OK ) {
    char mname[73];

    smf_getfitss( hdr, "SMFMODEL", mname, sizeof(mname), status );
    if( *status == SAI__OK ) {
      hdr->mtype = smf_model_gettype( mname, status );
    } else if( *status == SMF__NOKWRD ) {
      /* If no SMFMODEL keyword present just annul the status and continue */
      errAnnul(status);
    }
  }

  /* Add any required pointing corrections on to the SMU jiggle positions in the
     header. */
  if( hdr ) smf_pcorr( hdr, igrp, status );

  /* Now that everything else is done, check to see if any corrections
     to the DATA/VARIANCE/QUALITY components are needed (only
     meaningful for time series data at the moment). Hide messages by
     default. */
  if( !(flags&SMF__NOFIX_DATA) && isTseries ) {
    smf_fix_data( MSG__DEBUG, *data, status );
  }

  /* free resources on error */
  if (*status != SAI__OK) {
    smf_close_file( wf, data, status );
  }

}

/* Private routine for opening up a file and reading the OCS config.
   Returned value must be freed if non-NULL.
 */

static char * smf__read_ocsconfig ( int ndfid, int *status) {
  char * ocscfg = NULL;
  if (*status != SAI__OK) return ocscfg;

  /* Read the OCS configuration XML if available */
  if ( ndfid != NDF__NOID ) {
    int isthere = 0;
    ndfXstat( ndfid, "JCMTOCS", &isthere, status );
    if (isthere) {
      HDSLoc * jcmtocs = NULL;
      HDSLoc * configloc = NULL;
      size_t size;
      size_t clen;
      hdsdim dims[1];
      ndfXloc( ndfid, "JCMTOCS", "READ", &jcmtocs, status );
      datFind( jcmtocs, "CONFIG", &configloc, status );
      datAnnul( &jcmtocs, status );
      datSize( configloc, &size, status );
      datClen( configloc, &clen, status );
      /* allocate it slightly bigger in case the config *just* fits
         in SIZE * CLEN characters and we can't terminate it. Also
         initialise it so that we can walk back from the end */
      ocscfg = astCalloc( size + 1, clen );
      ocscfg[0] = '\0'; /* just to make sure */
      dims[0] = size;
      datGetC( configloc, 1, dims, ocscfg, clen, status );
      /* _CHAR buffer will not be terminated */
      cnfImprt( ocscfg, (size * clen) + 1, ocscfg );
      datAnnul( &configloc, status );
    }
  }
  return ocscfg;
}


void smf_open_file_job( ThrWorkForce *wf, int wait, const Grp *grp,
                        size_t index, const char *mode, int flags,
                        smfData **data, int *status ) {
/*
 *+
 *  Name:
 *     smf_open_file_job

 *  Purpose:
 *     Run smf_open_file in a thread.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_open_file_job( ThrWorkForce *wf, int wait, const Grp *grp,
 *                        size_t index, const char *mode, int flags,
 *                        smfData **data, int *status )

 *  Arguments:
 *     wf = ThrWorkForce *wf
 *        Workforce to use.
 *     wait = ThrWorkForce *wf
 *        Should this function wait until the job has completed before
 *        returning? If not, the job is added to the current job context
 *        and the function returns immediately. Note, if "wait" is zero,
 *        then the returned smfData should beb locked (using smf_lock_data)
 *        when the job eventually completes.
 *     grp = const Grp * (Given)
 *        NDG group identifier
 *     index = size_t (Given)
 *        Index corresponding to required file in group
 *     mode = const char * (Given)
 *        File access mode
 *     flags = int (Given)
 *        Bitmask controls which components are opened. If 0 open everything.
 *     data = smfData ** (Returned)
 *        Pointer to pointer smfData struct to be filled with file info and data
 *        Should be freed using smf_close_file. Will be NULL if this routine completes
 *        with error.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This runs smf_open_file within a worker thread of a given WorkForce.
 *     Note, since smf_open_file calls Fortran code, care should be taken
 *     to avoid running this job simultaneously in multiple threads.

 *-
 */

/* Local Variables; */
   smfOpenFileData *pdata;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Get pointer to a job data structure. */
   pdata = astMalloc( sizeof( *pdata ) );
   if( *status == SAI__OK ) {

/* begin a new job context. */
      thrBeginJobContext( wf, status );

/* Store the values needed by the smf_open_file function. */
      pdata->grp = grp;
      pdata->index = index;
      pdata->mode = mode;
      pdata->flags = flags;
      pdata->data = data;

/* Submit the job, telling it to free the job data (i.e. "pdata") when the
   job is completed. */
      thrAddJob( wf, THR__FREE_JOBDATA, pdata, smf1_open_file_caller, 0,
                   NULL, status );

/* If required, wait for the job to complete and then lock the AST
   objects for access by the current thread. */
      if( wait ) {
         thrWait( wf, status );
         smf_lock_data( *data, 1, status );
      }

/* End the current job context. */
      thrEndJobContext( wf, status );
   }
}

static void smf1_open_file_caller( void *data, int *status ){
/*
 *  Name:
 *     smf_open_file_caller

 *  Purpose:
 *     Calls smf_open_file.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf1_open_file_caller( void *data, int *status )

 *  Arguments:
 *     data = void *
 *        Data structure containing arguments for smf_open_file.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This calls smf_open_file within a worker thread.
 */

/* Local Variables; */
   smfOpenFileData *pdata;
   smfData * thisdata = NULL;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Get pointer to job data structure. */
   pdata = (smfOpenFileData *) data;

/* Open the file. */
   smf_open_file( NULL, pdata->grp, pdata->index, pdata->mode, pdata->flags,
                  pdata->data, status );

/* If this file is associated with an NDF then we do a deep copy
   and close the original file. We do this to allow smf_close_file
   to run in a thread */

   thisdata = *(pdata->data);
   if (thisdata && thisdata->file && thisdata->file->ndfid != NDF__NOID) {
     smfData *tmpdata = NULL;
     tmpdata = smf_deepcopy_smfData( NULL, thisdata, 0, 0, 0, 0, status );
     smf_close_file( NULL, pdata->data, status );
     *(pdata->data) = tmpdata;
   }

/* Unlock all AST objects within the smfData so that the calling thread
   can lock them. */
   smf_lock_data( *(pdata->data), 0, status );

}



