/*
*+
*  Name:
*     smf_typ.h

*  Purpose:
*     type definitions for the smf library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "smf_typ.h"

*  Description:
*     Data types used by the smf library.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:n
*     2005-11-02 (AGG):
*        Initial test version
*     2005-11-07 (TIMJ):
*        Document some of the struct items
*     2005-11-23 (TIMJ):
*        HDS Locator has changed type
*     2005-12-14 (TIMJ):
*        Add reference counter to smfData
*     2005-12-21 (AGG):
*        Add index to current timeslice
*     2006-01-25 (TIMJ):
*        - Create SMF__NOCREATE flags
*        - sc2head is now embedded in the smfHead struct
*     2006-01-27 (TIMJ):
*        - Add allsc2heads to header
*        - sc2head now a pointer into allsc2heads
*        - remove dksquid from DA until it is needed
*        - smfFile.xloc no longer needed
*        - Add smfHead.isCloned
*     2006-02-17 (AGG):
*        Add ncoeff and poly for scanfit polynomial coefficients
*     2006-03-23 (AGG):
*        Add nframes and curframe to smfHead
*     2006-04-21 (AGG):
*        Add history to smfData
*     2006-06-12 (EC):
*        Added smurfloc/mapcoordid to smfFile & lut to smfData
*     2006-06-25 (AGG):
*        Add smfGroup, smfArray
*     2006-07-12 (EC):
*        Added enumerated typedef smf_modeltype
*     2006-07-26 (TIMJ):
*        sc2head replaced by JCMTState
*     2006-07-28 (TIMJ):
*        Add time series wcs (tswcs) to distinguish from 2d wcs in smfHead
*     2006-08-01 (AGG):
*        Add SMF__MAP_VAR and SMF__MAP_QUAL flags
*     2006-09-01 (EC):
*        Added telpos to smfHead 
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/
#ifndef SMF_TYP_DEFINED
#define SMF_TYP_DEFINED

#include "star/hds_types.h"
#include "jcmt/state.h"
#include "sc2da/sc2store_par.h"
#include "ast.h"
#include "ndf.h"
#include "star/grp.h"
#include "smurf_typ.h"
#define SMF_PATH_MAX GRP__SZNAM
#define SMF_NAME_MAX GRP__SZFNM
#define SMF__MXSMF 4 /* Maximum number of smfDatas in a smfArray */

/* Different data types supported by SMURF */

typedef enum smf_dtype {
  SMF__NULL,
  SMF__INTEGER,
  SMF__FLOAT,
  SMF__DOUBLE,
  SMF__USHORT,
} smf_dtype;

/* Different types of model components used by iterative map-maker */

typedef enum smf_modeltype {
  SMF__AST=0,             /* Astronomical signal */
  SMF__COM=1,             /* Common-mode signal */
  SMF__CUM=2,             /* Cummulative signal */
  SMF__NOI=3,             /* Noise */
  SMF__RES=4,             /* Residual signal */
} smf_modeltype;


/* Flags for smf_create_smf* 
   Must be individual bits in a single integer
*/
#define SMF__NOCREATE_DA 1
#define SMF__NOCREATE_HEAD 2
#define SMF__NOCREATE_FILE 4

/* Flags for smf_open_newfile
   Must be individual bits in a single integer
*/
#define SMF__MAP_VAR 8
#define SMF__MAP_QUAL 16

/* Global information about the data file itself */

typedef struct smfFile {
  int ndfid;                 /* NDF ID of file if opened by SMURF */
  int isSc2store;            /* True if file opened by sc2store library */
  int isTstream;             /* True if file contains time series data */
  char name[SMF_PATH_MAX+1]; /* Name of file */
  int mapcoordid;            /* NDF identifier for SMURF.MAPCOORD */
} smfFile;

/* Contains header general header information obtained from the file */

typedef struct smfHead {
  const JCMTState *state;   /* Pointer to current STATE */
  inst_t     instrument;    /* Instrument code */
  AstFrameSet * wcs;        /* Frameset for a particular time slice (frame) */
  AstFrameSet * tswcs;      /* Frameset for full time series (if tseries) */
  AstFitsChan * fitshdr;    /* FITS header from the file */
  dim_t curframe;           /* Index corresponding to current frame */
  dim_t nframes;            /* Number of frames in smfData */
  int isCloned;             /* If false, allState is owned by this
			       struct, if true it should not be freed */
  const JCMTState *allState;/* Array of STATE for every time slice */ 
  unsigned int ndet;        /* Number of focal plane detectors */
  const double * fplanex;   /* X coords (radians) of focal plane detectors */
  const double * fplaney;   /* Y coords (radians) of focal plane detectors */
  const double telpos[3];   /* Lon/LAT/Alt of telescope (deg/deg/m) */
} smfHead;

/* This structure contains ancilliary information obtained from a raw
   data file that may be useful to SMURF.
*/

typedef struct smfDA {
  double *flatcal;           /* pointer to flatfield calibration */
  double *flatpar;           /* pointer to flatfield parameters */
  char flatname[SC2STORE_FLATLEN]; /* name of flatfield algorithm */
  int nflat;                 /* number of flat coeffs per bol */
} smfDA;

/* This struct is used to contain all information related to a particular
   data file (where possible since sc2store does not return a handle).
*/

typedef struct smfData {
  smfFile * file;            /* File information */
  smfHead * hdr;             /* Header information */
  smfDA * da;                /* If sc2store, associated data arrays */
  smf_dtype dtype;           /* Data type of DATA and VARIANCE arrays */
  void * pntr[3];            /* Array of pointers to DATA/VARIANCE/QUALITY */
  dim_t dims[NDF__MXDIM];    /* Dimensions of data array */
  int ndims;                 /* Number of active dimensions in "dims" */
  int refcount;              /* Reference count for data object */
  int virtual;               /* Flag for extracted timeslices */
  double *poly;              /* Polynomial scan fits */
  int ncoeff;                /* Number of coefficients in polynomial */
  int * lut;                 /* Pointing lookup table */
  AstKeyMap *history;        /* History entries */
} smfData;

/* This structure is a container for multiple, usually related,
   smfDatas */

typedef struct smfArray {
  smfData **sdata;           /* Pointers to smfDatas */
  int ndat;                  /* Number of smfDatas in current smfArray */
} smfArray;

/* This struct is used to group related files together */

typedef struct smfGroup {
  Grp *grp;                  /* Copy of input Grp */
  int **subgroups;           /* Array of indices into Grp */
  int ngroups;               /* Number of subgroups */
  int nrelated;              /* Maximum number of related files */
} smfGroup;

#endif /* SMF_TYP_DEFINED */

