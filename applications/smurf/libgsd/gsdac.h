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
*     2008-02-13 (JB):
*        Add freeArrays, getGSDVars
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays 
*     2008-02-19 (JB):
*        Check dasFlag.  Added getDASFlag, removed getArraySize
*        and getElemx
*     2008-02-26 (JB):
*        Make gsdac_getWCS per-subsystem, add tranDate, tranTime
*        and velEncode
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum
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
*     License along/home/jbalfour with this program; if not, write to the Free
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

void gsdac_freeArrays
(
const dasFlag dasFlag,  /* DAS file structure flag (given) */
gsdVars *gsdVars,    /* GSD headers and arrays (given) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0b
( 
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0c
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value.  Should be declared with length 17 at 
                        least.  Returned string is null-terminated in value[16].                        (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0d
(
const gsd *gsd,      /* GSD file access parameters (given) */ 
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
double *value,       /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0i
( 
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
int *value,          /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0l
( 
const gsd *gsd,      /* GSD file access parameters  (given)*/
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0r
(
const gsd *gsd,      /* GSD file access parameters (given) */ 
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
float *value,        /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0w
( 
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
short *value,        /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get1b
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1c
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1d
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
double *values,      /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1i
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
int *values,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1l
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1r
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
float *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_get1w
(
const gsd *gsd,      /* GSD file access parameters (given) */
char *name,          /* name of the item (should be an array of 
                        16 characters) (given) */
short *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */ 
);

void gsdac_getDASFlag
(
const gsd *gsd,      /* GSD file access parameters (given) */
dasFlag *dasFlag,    /* DAS file type (given and returned) */
int *status          /* pointer to global status (given and returned) */ 
);

void gsdac_getDateVars
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const int subBandNum, /* subband number (given) */
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

void gsdac_getGSDVars
( 
const gsd *gsd,      /* GSD file access parameters (given) */
const dasFlag dasFlag, /* DAS file type (given) */
gsdVars *gsdVars,    /* GSD headers and arrays (given and returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_getMapVars
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const char *samMode, /* sampling mode (given) */
const char *obsType, /* observation type (given) */
char *skyRefX,       /* x-coord of reference position (given and returned) */
char *skyRefY,       /* y-coord of reference position (given and returned) */
char *swMode,        /* switch mode (given and returned) */
char *chopCrd,       /* chop coordinate frame (given and returned) */
float *mapHght,      /* requested height of map (given and returned) */
float *mapPA,        /* requested position angle of map 
                        (given and returned) */
float *mapWdth,      /* requested width of map (given and returned) */
char *loclCrd,       /* local offset coordinates system for map 
                        (given and returned) */
char *scanCrd,       /* coordinate system of scan (given and returned) */
float *scanVel,      /* scan velocity */
float *scanDy,       /* scan spacing perpendicular to scan 
                        (given and returned) */
float *scanPA,       /* scan PA rel. to lat. line (given and returned) */
char *scanPat,       /* name of scanning scheme (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getSampleMode
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
char *samMode,       /* sampling mode (given and returned) */
char *obsType,       /* observation type (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getStartIdx
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const char *samMode, /* sampling mode (given) */
int *startIdx,       /* start index into pattern (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getWCS
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const unsigned int stepNum,    /* time step of this spectrum (given) */
const int subBandNum, /* subband number (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
gsdWCS *wcs,         /* pointing and time values (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_putFits
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const int subBandNum, /* subband number (given) */
const int obsNum,    /* observation number (given) */
const int utDate,    /* UT date (given) */
const int nSteps,    /* number of time steps in observation (given) */
const char *backend, /* name of the backend (given) */
char *recepNames[],  /* names of receptors (given) */
const char *samMode, /* sample mode (given) */
const char *obsType, /* observation type (given) */
const gsdWCS *wcs,      /* pointing and time values (given) */
const AstFitsChan *fitschan,  /* FITS headers (given and returned) */
int *status          /* pointer to global status (given and returned) */
); 

void gsdac_putJCMTStateC
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const unsigned int stepNum,    /* time step of this spectrum (given) */
const char *backend,      /* name of the backend (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
struct JCMTState *record, /* JCMTState headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_putJCMTStateS
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const unsigned int stepNum,    /* time step of this spectrum (given) */
const int subBandNum, /* subband number (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
const gsdWCS *wcs,      /* pointing and time values (given) */
struct JCMTState *record, /* JCMTState headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_putSpecHdr
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const unsigned int nSteps,    /* number of time steps (given) */
const unsigned int stepNum,   /* time step of this spectrum (given) */
const int subBandNum, /* subband number (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
const JCMTState *record,      /* JCMTState headers (given) */
struct ACSISSpecHdr *specHdr, /* ACSIS Spec Headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */ 
);

void gsdac_tranDate
(
const double dDate,  /* date as a double (given) */
char *iDate,         /* date as string (given and returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_tranTime
(
const double dTime,  /* time as a double (given) */
char *iTime,         /* time as string (given and returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_velEncode 
(
const char *vframe,  /* velocity frame (given) */
const char *vdef,    /* velocity definition (given) */
int *LSRFlg,         /* LSR flag */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_wrtData
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const char *directory,     /* output write directory (given) */
const unsigned int nSteps, /* number of steps in the observation (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
int *status          /* pointer to global status (given and returned) */
);

#endif /* GSDAC_DEFINED */
