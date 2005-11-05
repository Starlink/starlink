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
*     {enter_new_authors_here}

*  History:
*     2005-11-02 (AGG):
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
#ifndef SMF_TYP_DEFINED
#define SMF_TYP_DEFINED

#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2store_par.h"
#include "ast.h"
#include "ndf.h"
#include "grp.h"
#include "smurf_typ.h"
#define SMF_PATH_MAX GRP__SZNAM

enum {
  SMF__NULL,
  SMF__INTEGER,
  SMF__FLOAT,
  SMF__DOUBLE
};

typedef struct smfFile {
  int ndfid;
  int isSc2store;
  char name[SMF_PATH_MAX+1];
} smfFile;

typedef struct smfHead {
  struct sc2head * sc2head;
  AstFrameSet * wcs;
  AstFitsChan * fitshdr;
} smfHead;

typedef struct smfDA {
  int *dksquid;           /* pointer to dark SQUID data */
  double *flatcal;        /* pointer to flatfield calibration */
  double *flatpar;        /* pointer to flatfield parameters */
  char flatname[SC2STORE_FLATLEN]; /* name of flatfield algorithm */
  int nflat;              /* number of flat coeffs per bol */
} smfDA;

typedef struct smfData {
  smfFile * file;
  smfHead * hdr;
  smfDA * da;
  int dtype;
  void * pntr[3];
  dim_t dims[NDF__MXDIM];
  int ndims;
} smfData;

#endif /* SMF_TYP_DEFINED */
