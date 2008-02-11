/*
*+
*  Name:
*     gsdac.h

*  Purpose:
*     Prototypes for the libgsd library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "gsdac.h"

*  Description:
*     Prototypes used by the libgsd functions.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-01 (JB):
*        Original
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "ast.h"
#include "jcmt/state.h"
#include "gsdac_struct.h"
#include "libacsis/specwrite.h"

#ifndef GSDAC_DEFINED
#define GSDAC_DEFINED

void gsdac_get0b
( 
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0c
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value.  Should be declared with length 17 at 
                        least.  Returned string is null-terminated in value[16].                        (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0d
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */ 
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
double *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0i
( 
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
int *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0l
( 
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters  (given)*/
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0r
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */ 
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
float *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0w
( 
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
short *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get1b
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1c
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1d
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
double *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1i
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
int *values,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1l
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1r
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
float *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1w
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
short *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getArraySize
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */ 
int *size,           /* number of elements in the array (given and returned) */
int *status          /* pointer to global status (given and returned) */ 
);

void gsdac_getDateVars
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
const int subsysNum, /* subsystem number (given) */
const int obsNum,    /* observation number (given) */
const char *backend, /* name of the backend (given) */
char *dateObs,       /* datetime of obs start (given and returned) */
char *dateEnd,       /* datetime of obs end (given and returned) */
char *obsID,         /* unique observation ID (given and returned) */
char *obsIDs,        /* obsID + subsystem number (given and returned) */
char *HSTstart,      /* HST at obs start (given and returned) */
char *HSTend,        /* HST at obs end (given and returned) */
char *LSTstart,      /* LST at obs start (given and returned) */
char *LSTend,        /* LST at obs end (given and returned) */
int *status          /* pointer to global status (given and returned) */ 
);

void gsdac_getElemb
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
char *value,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getElemc
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
char *value,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getElemd
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
double *value,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getElemi
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
int *value,          /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getEleml
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
char *value,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getElemr
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
float *value,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getElemw
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
const int index,     /* index of element to be returned (given) */
short *value,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getMapVars
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
char *samMode,       /* sampling mode (given and returned) */
char *swMode,        /* switch mode (given and returned) */
char *skyRefX,       /* x-coord of reference position (given and returned) */
char *skyRefY,       /* y-coord of reference position (given and returned) */
char *obsType,       /* observation type (given and returned) */
char *chopCrd,       /* chop coordinate frame (given and returned) */
float *chopFrq,      /* chop frequency (given and returned) */
float *chopPA,       /* chop position angle (given and returned) */
float *chopThr,      /* chop throw (given and returned) */
float *mapHght,      /* requested height of map (given and returned) */
float *mapPA,        /* requested position angle of map 
                        (given and returned) */
float *mapWdth,      /* requested width of map (given and returned) */
int *numPtsX,        /* number of points in X direction 
                        (given and returned) */
int *numPtsY,        /* number of points in Y direction 
                        (given and returned) */
char *obsDirection,  /* direction of map rows (given and returned) */
char *loclCrd,       /* local offset coordinates system for map 
                        (given and returned) */
double *mapX,        /* requested map x offset from centre 
                        (given and returned) */
double *mapY,        /* requested map x offset from centre 
                        (given and returned) */
char *scanCrd,       /* coordinate system of scan (given and returned) */
float *scanVel,      /* scan velocity */
float *scanDy,       /* scan spacing perpendicular to scan 
                        (given and returned) */
float *scanPA,       /* scan PA rel. to lat. line (given and returned) */
char *scanPat,       /* name of scanning scheme (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getStartIdx
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */ 
const char *samMode, /* sampling mode (given) */
const int *numPtsX,  /* number of points in x direction (given) */
const int *numPtsY,  /* number of points in y direction (given) */
const char *obsDirection, /* direction of rows (given) */
int *startIdx,       /* start index into pattern (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_putFits
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
const int nSubsys,   /* number of subsystems (given) */
const int subsysNum, /* subsystem number (given) */
const int obsNum,    /* observation number (given) */
const int utDate,    /* UT date (given) */
const int nChans,    /* total number of channels used in observation (given) */
const int nSteps,    /* number of time steps in observation (given) */
const char *backend, /* name of the backend (given) */
const int nRecep,    /* number of receptors (given) */
char *recepNames[],  /* names of receptors (given) */
const struct JCMTState *record, /* JCMTState headers (given) */
const AstFitsChan *fitschan,  /* FITS headers (given and returned) */
int *status          /* pointer to global status (given and returned) */
); 

void gsdac_putJCMTStateC
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters */
const unsigned int stepNum,    /* time step of this spectrum (given) */
struct JCMTState *record, /* JCMTState headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_putJCMTStateS
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters */
const unsigned int stepNum,    /* time step of this spectrum (given) */
const unsigned int subsysNum, /* subsystem number (given) */
struct JCMTState *record, /* JCMTState headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_putSpecHdr
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters */ 
const unsigned int nSteps,    /* number of time steps (given) */
const unsigned int stepNum,   /* time step of this spectrum (given) */
const unsigned int subsysNum, /* subsystem number (given) */
const JCMTState *record,      /* JCMTState headers (given) */
struct ACSISSpecHdr *specHdr, /* ACSIS Spec Headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */ 
);

void gsdac_wrtData
(
const struct gsdac_gsd_struct *gsd, /* GSD file access parameters (given) */
const unsigned int nSteps, /* number of time steps (given) */
const char *directory,     /* output write directory (given) */
int *status          /* pointer to global status (given and returned) */
);

#endif /* GSDAC_DEFINED */
