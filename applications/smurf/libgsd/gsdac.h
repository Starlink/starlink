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
*        Original.
*     2008-02-13 (JB):
*        Add freeArrays, getGSDVars.
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays .
*     2008-02-19 (JB):
*        Check dasFlag.  Added getDASFlag, removed getArraySize
*        and getElemx.
*     2008-02-26 (JB):
*        Make gsdac_getWCS per-subsystem, add tranDate, tranTime
*        and velEncode.
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum.
*     2008-02-28 (JB):
*        Use dateVars and mapVars structs.
*     2008-03-06 (JB):
*        Removed tranDate, updated tranTime.
*     2008-03-19 (JB):
*        Change obsNum to signed.
*     2008-03-24 (JB):
*        Pass nSubsys to putFits.
*     2008-03-25 (JB):
*        getWCS returns AstFrameSet.
*     2008-03-28 (JB):
*        Add getRecepNames and getTransition.
*     2008-04-04 (JB):
*        Add CALLGSD macro.
*     2008-04-11 (JB):
*        Remove wcs argument from putFits (not needed).
*     2008-04-14 (JB):
*        Remove obsType argument from getMapVars (not needed).
*     2008-04-18 (JB):
*        Add special configuration flag.
*     2008-04-21 (JB):
*        Add gsdac_printHdr.
*     2008-04-22 (JB):
*        Add gsdac_flagBad.
*     2013-07-28 (TIMJ):
*        Add gsdac_getRealInstrumentName
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "ast.h"
#include "jcmt/state.h"
#include "gsdac_struct.h"
#include "libacsis/specwrite/specwrite.h"

#ifndef GSDAC_DEFINED
#define GSDAC_DEFINED

#define CALLGSD( routine, status, errmsg )\
  if ( *status == SAI__OK ) {\
    int gsdstat;\
    gsdstat = routine;\
    if ( gsdstat != 0 ){\
       *status = SAI__ERROR;\
       msgSeti( "GSDERR", gsdstat ); \
       errmsg;\
    }\
  }

void gsdac_flagBad
(
const dasFlag dasFlag,  /* DAS file structure flag (given) */
gsdVars *gsdVars,    /* GSD headers and arrays (given and returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_freeArrays
(
const dasFlag dasFlag,  /* DAS file structure flag (given) */
gsdVars *gsdVars,    /* GSD headers and arrays (given) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0b
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0c
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *value,         /* data value.  Should be declared with length 17 at
                        least.  Returned string is null-terminated in value[16].                        (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0d
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
double *value,       /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0i
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
int *value,          /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0l
(
const gsd *gsd,      /* GSD file access parameters  (given)*/
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *value,         /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0r
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
float *value,        /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get0w
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
short *value,        /* data value (returned) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_get1b
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1c
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1d
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
double *values,      /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1i
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
int *values,         /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1l
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
char *values,        /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1r
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
                        16 characters) (given) */
float *values,       /* data values (returned) */
int *statuss         /* pointer to global status (given and returned) */
);

void gsdac_get1w
(
const gsd *gsd,      /* GSD file access parameters (given) */
const char *name,    /* name of the item (should be an array of
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
const char *backend, /* name of the backend (given) */
const int obsNum,    /* observation number (given) */
dateVars *dateVars,  /* date and time variables (given and returned) */
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
mapVars *mapVars,    /* map/chop/scan variables (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getRealInstrumentName
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
char * instrume,        /* Buffer to receive instrument name (given and returned) */
size_t instrumelen,     /* Size of instrume (given) */
int *status             /* global status (given and returned) */
);

void gsdac_getRecepNames
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
char *recepNames[],  /* receptor names (given and returned) */
int recepFlags[],    /* flags for which receptors were used
                        (given and returned) */
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

void gsdac_getTransition
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
char *molecule,      /* name of molecule (given and returned) */
char *transiti,      /* transition (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_getWCS
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const unsigned int stepNum,    /* time step of this spectrum (given) */
const int subBandNum, /* subband number (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
const double *lineFreqs, /* line frequencies of each subband (given) */
const double *IFFreqs, /* IF of each subband (given) */
gsdWCS *wcs,         /* pointing and time values (given and returned) */
AstFrameSet **WCSFrame, /* WCS frameset (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_matchFreqs
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
double *lineFreqs,   /* line frequency of molecular transitions for
                        each subband (given and returned) */
double *IFFreqs,     /* IF for each subband (given and returned) */
int *status          /* global status (given and returned) */
);

void gsdac_printHdr
(
const char *nrao,    /* NRAO name (given) */
const char *jcmt,    /* JCMT name (given) */
const gsdDType dType, /* data type (given) */
const char *desc,     /* description (given) */
void *value,         /* value(s) (given) */
int arrayFlag,       /* flag for array data (given) */
long arraySize,      /* size of array (if array data) (given) */
int descFlag,        /* print out description? (given) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_putFits
(
const gsdVars *gsdVars, /* GSD headers and arrays (given) */
const int subBandNum, /* subband number (given) */
const int nSubsys,   /* subsystem number (given) */
const int obsNum,    /* observation number (given) */
const int utDate,    /* UT date (given) */
const int nSteps,    /* number of time steps in observation (given) */
const char *backend, /* name of the backend (given) */
const int recepsUsed,/* number of receptors actually used (given) */
char *recepNames[],  /* names of receptors (given) */
const char *samMode, /* sample mode (given) */
const char *obsType, /* observation type (given) */
const dateVars *dateVars, /* date/time variables (given) */
const mapVars *mapVars, /* map/chop/scan variables (given) */
const double *lineFreqs, /* line frequencies of each subband (given) */
const double *IFFreqs, /* IF of each subband (given) */
const gsdWCS *wcs,     /* pointing and time values (given) */
AstFitsChan *fitschan,  /* FITS headers (given and returned) */
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
const int recepFlags[], /* flags for which receptors were used (given) */
const dasFlag dasFlag,  /* DAS file structure flag (given) */
const JCMTState *record,      /* JCMTState headers (given) */
struct ACSISSpecHdr *specHdr, /* ACSIS Spec Headers (given and returned ) */
int *status          /* pointer to global status (given and returned) */
);

void gsdac_tranTime
(
const char *dTime,   /* time as a string YYYY-MM-DD HH:MM:SS (given) */
char *iDate,         /* date as string (given and returned) */
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

const char * gsdac_code2tcssys
(
 gsdCoordType code, /* coordinate code to translate */
 int *status        /* inherited status */
);

#endif /* GSDAC_DEFINED */
