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
 *     David Berry (JAC, UCLan)
 *     Coskun Oba (COBA, UoL)
 *     Matt Sherwood (MS, UofL)
 *     {enter_new_authors_here}

 *  History:
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
 *     2006-09-07 (EC)
 *        Added instap to smfHead
 *     2006-09-15 (AGG):
 *        Add new smfDream struct
 *     2006-10-2 (DSB):
 *        Add detpos to smfHead
 *     2006-10-2 (DSB):
 *        Add detname to smfHead
 *     2006-12-13 (DSB):
 *        Add tsys to smfHead
 *     2007-02-07 (EC):
 *        Updated smf_modeltype
 *     2007-02-12 (EC):
 *        Add smf_calcmodelptr;
 *     2007-03-05 (EC):
 *        Add SMF__DIMM_FIRSTCOMP/FIRSTITER bit flags;
 *        Add SMF__EXT to smf_modeltype
 *     2007-06-13 (EC):
 *        Add SMF__LUT to smf_modeltype
 *        Add SMF__DIMM_SUFFIX
 *        Add DIMMbuf and DIMMlen to smfData
 *     2007-06-13 (EC):
 *        Added DIMMfd and moved DIMMbuf and DIMMlen to smfFile
 *     2007-06-25 (EC):
 *        Removed DIMMbuf/DIMMlen, renamed DIMMfd to fd in smfFile
 *     2007-07-10 (EC):
 *        -increased SMF__MXSMF to 8 from 4
 *        -made smfData.sdata static array of size SMF__MXSMF
 *        -modified smf_calcmodelptr prototype
 *     2007-08-21 (EC):
 *        Added SMF__NUL to typedef smf_modeltype
 *     2007-08-21 (DSB):
 *        Added smfBox and smfTile.
 *     2007-09-13 (EC):
 *        Added isTordered to smfData.
 *     2007-10-29 (EC):
 *        Added definition of SMF__NOCREATE_DATA for use by smf_open_file
 *     2007-11-15 (EC):
 *        Added SMF__NOCREATE_LUT flag for file I/O
 *     2008-1-15 (DSB):
 *        Added qlbnd/qubnd to the smfTile structure.
 *     2008-2-8 (EC):
 *        -Added SMF__NOCREATE_QUALITY flag, and data quality SMF__Q* flags
 *        -Added SMF__NOCREATE_VARIANCE, SMF__QUA model component
 *        -Added SMF__UBYTE data type
 *     2008-03-04 (EC):
 *        -Added smfDIMMData; updated smf_calcmodelptr prototype
 *     2008-04-16 (EC):
 *        -Added chunk to smfGroup
 *     2008-04-18 (EC):
 *        -Added chisquared to smfDIMMData
 *        -Added SMF__MINCHUNKSAMP and SMF__MINSTATSAMP
 *     2008-04-24 (EC):
 *        -Added SMF__MIB definition
 *     2008-04-30 (EC):
 *        -Added EXT to smfDIMMData
 *     2008-06-06 (EC):
 *        -Add smfFilter definition
 *     2008-06-12 (EC):
 *        -Switch to split real/imaginary arrays for smfFilter
 *     2008-06-23 (EC)
 *        -Added WCS to smfFilter
 *     2008-06-24 (EC)
 *        -Added SMF__Q_PAD quality flag
 *     2008-07-03 (EC)
 *        -Changed type to dim_t from int for smfArray.ndat and
 *         smfGroup.ngroups/nrelated
 *     2008-07-10 (TIMJ):
 *        DA struct now includes dark squid
 *     2008-07-14 (TIMJ):
 *        smfArray can be dynamic
 *     2008-07-25 (TIMJ):
 *        Add SMF__BADIDX
 *     2008-07-28 (TIMJ):
 *        Add steptime to smfHead so that we don't have to extract
 *        it from the FITS header each time.
 *     2008-08-20 (EC):
 *        Add obsidss to smfHead
 *     2008-08-25 (EC)
 *        Add smfDIMMHead
 *     2009-03-10 (EC)
 *        Add SMF__FLT to smf_modeltype
 *     2009-04-24 (TIMJ):
 *        Add ACSIS observing modes.
 *     2009-05-20 (TIMJ):
 *        Add switching modes.
 *     2009-12-09 (TIMJ):
 *        Add _BIT values from sc2headman_struct.h
 *     2010-01-08 (TIMJ):
 *        Add sub instrument enums.
 *     2010-01-11 (EC):
 *        Add SMF__Q_GAP (bits that indicate where gaps should be filled)
 *     2010-03-19 (EC):
 *        Renamed SMF__Q_BADS to SMF__Q_BADDA, and added SMF__Q_COM
 *     2010-04-20 (EC):
 *        Add map quality (SMF__MAPQ_ZERO, and mapqual in smfDIMMData)
 *     2010-05-19 (EC):
 *        Add mtype to smfHead, and smf_expmodelptr function prototype
 *     2010-05-20 (EC):
 *        Dark squids are now stored in a smfData within smfDA
 *     2010-05-27 (TIMJ):
 *        Add SMF__SMO model.
*     2010-06-08 (EC):
*        Add SMF__TWO model.
*     2010-09-16 (COBA):
*        - Add smfFts
*        - Create SMF__NOCREATE_FTS
*        - Update smfData
*     2010-09-20 (TIMJ):
*        We are using MiB not MB
*     2010-09-21 (EC):
*        Add scanvel to smfHead
*     2010-10-25 (EC):
*        Add tlen to smfGroup
*     2010-11-12 (EC):
*        Add theta (scan direction each tslice) to smfData
*     2010-12-06 (TIMJ):
*        Add enginerring obs types.
*     2011-09-19 (DSB):
*        Add SMF__Q_BADEF.
*     2011-09-20 (EC):
*        Add isFFT to smfData
*     2011-10-03 (EC):
*        Extend smfFilter to handle 2-d map filters
*     2015-02-20 (MS):
*        Added new smfFts fields for quality statistics
*     2015-11-19 (GSB):
*        Add WVMFIT option to smf_tausrc.
*     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2015 East Asian Observatory.
 *     Copyright (C) 2008-2010 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2005-2011 University of British Columbia.
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
 You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */
#ifndef SMF_TYP_DEFINED
#define SMF_TYP_DEFINED

#include <stdint.h>

#include "star/hds_types.h"
#include "jcmt/state.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/dream_par.h"
#include "sc2da/sc2ast.h"
#include "ast.h"
#include "prm_par.h"
#include "ndf.h"
#include "star/grp.h"
#include "star/thr.h"
#include "smurf_typ.h"
#include "smurf_par.h"
#include "fftw3.h"

/* Macro to convert a bit position to an integer value */
#define BIT_TO_VAL(bit) (1<<bit)

#define SMF_PATH_MAX GRP__SZNAM
#define SMF_NAME_MAX GRP__SZFNM
#define SMF_MODEL_MAX 64   /* Maximum model components for iterative map maker*/
#define SMF_QSTR_MAX 64    /* Maximum chars in quality bit string names */
#define SMF__MXSMF 8       /* Maximum number of smfDatas in a smfArray */
#define SMF__MIB 1048576   /* SMURF uses a Mebibyte = 2^20 */
#define SMF__MXNOTCH 64    /* Arbitrary maximum number of notch filters */
#define SMF__F_WHITELO 2   /* Lower freq. over which to calculate NEP/NEFD */
#define SMF__F_WHITEHI 10  /* Upper freq. over which to calculate NEP/NEFD */
#define SMF__MAXAPLEN ((size_t) -2) /* tell smf_apodize to use maximum interval */
#define SMF__DOWNSAMPLIMIT 0.8 /* Upper limit on usable downsampling factor */

/* Minimum number of time samples for a data chunk to be useful*/
#define SMF__MINCHUNKSAMP 100

/* Minimum number of samples for meaningful stats (like sample variance) */
#define SMF__MINSTATSAMP 5

/* Define the size of strings defining NDF char components (Units, label) */
#define SMF__CHARLABEL  80

/* Define the message reporting level to see timer messages */
#define SMF__TIMER_MSG MSG__DEBUG1

/* The name of the environment variable used to get the number of worker
threads to use. */
#define SMF__THREADS "SMURF_THREADS"

/* Different data types supported by SMURF */
typedef enum smf_dtype {
  SMF__NULL,
  SMF__INTEGER,
  SMF__FLOAT,
  SMF__DOUBLE,
  SMF__USHORT,
  SMF__UBYTE
} smf_dtype;

/* Define a special quality type locally for SMURF. We have to define
   the type in terms of the primary data types in smf_dtype, typedef
   a local variable type and define a bad value */

#define SMF__QUALTYPE SMF__USHORT
typedef unsigned short smf_qual_t;
#define VAL__BADQ VAL__BADUW

/* Define a bad size_t value - the wrap-around equivalent of -1 */
#define SMF__BADSZT ((size_t) -1)

/* Different types of model components used by iterative map-maker. These are
   powers of 2 so they can be used in bit masks. */
typedef enum smf_modeltype {
  SMF__NUL=0,               /* Null model */
  SMF__CUM=1,               /* Cumulative time-stream model */
  SMF__RES=2,               /* Residual time-stream signal */
  SMF__AST=4,               /* Astronomical time-stream signal */
  SMF__COM=8,               /* Common-mode signal */
  SMF__NOI=16,              /* Noise model */
  SMF__EXT=32,              /* Extinction correction */
  SMF__LUT=64,              /* Pointing LUT */
  SMF__QUA=128,             /* Quality flags */
  SMF__DKS=256,             /* Dark squid signals */
  SMF__GAI=512,             /* Relative Gain/Offset for each bolometer */
  SMF__FLT=1024,            /* Frequency domain filter */
  SMF__PLN=2048,            /* Spatial plane removal */
  SMF__SMO=4096,            /* Time series smoothing */
  SMF__TWO=8192,            /* Two-component common-mode */
  SMF__TMP=16384,           /* Generic fitting of external template to bolos */
  SMF__SSN=32768,           /* Scan-syhcnronous noise */
  SMF__PCA=65536            /* PCA noise */
} smf_modeltype;

/* Observing mode and type */

typedef enum smf_obsmode {
  SMF__OBS_NULL,
  SMF__OBS_DREAM,
  SMF__OBS_STARE,
  SMF__OBS_SCAN,
  SMF__OBS_GRID,
  SMF__OBS_JIGGLE
} smf_obsmode;

typedef enum smf_swmode {
  SMF__SWM_NULL,
  SMF__SWM_CHOP,
  SMF__SWM_PSSW,
  SMF__SWM_FREQSW,
  SMF__SWM_SELF
} smf_swmode;

typedef enum smf_obstype {
  SMF__TYP_NULL,
  SMF__TYP_SCIENCE,
  SMF__TYP_POINTING,
  SMF__TYP_FOCUS,
  SMF__TYP_SKYDIP,
  SMF__TYP_FLATFIELD,
  SMF__TYP_NOISE,
  SMF__TYP_FASTFLAT,
  SMF__TYP_HEATRAMP,
  SMF__TYP_BIASSAW,
  SMF__TYP_BIASRAMP,
  SMF__TYP_NEP,
  SMF__TYP_RAMP,
  SMF__TYP_IV_CURVES_M,
  SMF__TYP_IV_CURVES_H,
  SMF__TYP_OPEN_LOOP_G,
  SMF__TYP_SETUP
} smf_obstype;

/* The real instrument, rather than the interface */
typedef enum {
  SMF__RINST_NONE,
  SMF__RINST_SCUBA2,
  SMF__RINST_ACSIS,
  SMF__RINST_DAS,
  SMF__RINST_AZTEC,
  SMF__RINST_SUPERCAM,
  SMF__RINST_SMART
} smf_realinst_t;

typedef enum {
  SMF__INBEAM_NOTHING = 0,
  SMF__INBEAM_POL = BIT_TO_VAL(0),
  SMF__INBEAM_FTS = BIT_TO_VAL(1),
  SMF__INBEAM_BLACKBODY = BIT_TO_VAL(2)
} smf_inbeam_t;


/* Source of tau for extinction correction */
typedef enum smf_tausrc {
  SMF__TAUSRC_NULL,     /* No correction */
  SMF__TAUSRC_WVMRAW,   /* Use WVM raw time series */
  SMF__TAUSRC_CSOTAU,   /* Use CSO tau scaled to filter */
  SMF__TAUSRC_CSOFIT,   /* Use an external fit to CSO data, scaled to filter */
  SMF__TAUSRC_TAU,      /* Use this tau number: implements FILTERTAU option */
  SMF__TAUSRC_AUTO,     /* Use WVM, WVM fit or CSO fit */
  SMF__TAUSRC_CACHED,   /* Use a cached externally supplied time-series. Provenance unknown */
  SMF__TAUSRC_WVMFIT    /* Use an external fit to WVM data */
} smf_tausrc;

/* Method to use for extinction correction */
typedef enum smf_extmeth {
  SMF__EXTMETH_NONE,   /* No correction */
  SMF__EXTMETH_SINGLE, /* Assume all bolometers at same airmass */
  SMF__EXTMETH_FULL,   /* Calculate airmass of each bolometer */
  SMF__EXTMETH_ADAPT   /* Switch between FAST and FULL dynamically */
} smf_extmeth;


/* Ways of removing dark */
typedef enum smf_dark_sub_meth {
  SMF__DKSUB_NONE,        /* Ignore the dark */
  SMF__DKSUB_MEAN,        /* Use mean of previous and next dark */
  SMF__DKSUB_INTERP,      /* Interpolate dark over time */
  SMF__DKSUB_PREV,        /* Use previous dark */
  SMF__DKSUB_NEXT,        /* Use following dark */
  SMF__DKSUB_CHOOSE       /* Use one of mean, prev or next */
} smf_dark_sub_meth;

/* Ways of applying a bad bolometer mask - equivalent to bits */
typedef enum smf_bbm_meth {
  SMF__BBM_DATA=1,        /* Apply mask to dat array */
  SMF__BBM_QUAL=2,        /* Apply mask to quality array */
  SMF__BBM_QQUAL=4        /* Apply mask to first slice of quality array */
} smf_bbm_meth;

/* Parts of a header that can have their metadata fixed */
typedef enum smf_metadata_fixups {
  SMF__FIXED_FITSHDR=1,   /* Updated FITS header */
  SMF__FIXED_JCMTSTATE=2, /* Updated JCMTSTATE information */
  SMF__FIXED_ACSIS=4      /* Updated ACSIS extension information */
} smf_metadata_fixups;

/* Parts of the data/variance/quality components of smfData can have fixes */
typedef enum smf_data_fixups {
  SMF__FIXED_ROWORDER=1   /* Updated incorrect row ordering */
} smf_data_fixups;

/* Flatfield method */
typedef enum smf_flatmeth {
  SMF__FLATMETH_NULL,
  SMF__FLATMETH_TABLE,
  SMF__FLATMETH_POLY
} smf_flatmeth;

/* Indicate a bad array index */
static const size_t SMF__BADIDX = (size_t)-1;

/* suffix for simple binary files that store DIMM model components */
#define SMF__DIMM_SUFFIX ".dimm"

/* suffix for filename of concatenated data (residual files) */
#define SMF__CON_SUFFIX "con"

/* SMURF NDF extension name and type */
#define SMURF__EXTNAME "SMURF"
#define SMURF__EXTTYPE "SMURF_EXT"

/* Name of SMURF history component */
#define SMURF__HISTEXT "SMURFHIST"

/* Bit flags for smf_calcmodel* model component calculations */
typedef enum {
  SMF__DIMM_FIRSTCOMP = BIT_TO_VAL(0),  /* First component in the solution */
  SMF__DIMM_FIRSTITER = BIT_TO_VAL(1),  /* First iteration */
  SMF__DIMM_INVERT    = BIT_TO_VAL(2),  /* Inverse of the model calculation */
  SMF__DIMM_LASTITER  = BIT_TO_VAL(3),  /* Is this the last iteration? */
  SMF__DIMM_PREITER   = BIT_TO_VAL(4),  /* Before the first iteration */
  SMF__DIMM_PCACOM    = BIT_TO_VAL(5)   /* COM model used withuin PCA model */
} smf_calcmodel_flags;

/* Flags for smf_create_smf*, smf_open_file and smf_concat_smfGroup
   Must be individual bits in a single integer
*/
typedef enum {
  SMF__NOCREATE_DA       = BIT_TO_VAL(0),  /* Don't open DA data */
  SMF__NOCREATE_HEAD     = BIT_TO_VAL(1),  /* Don't open header */
  SMF__NOCREATE_FILE     = BIT_TO_VAL(2),  /* Don't open file */
  SMF__NOCREATE_DATA     = BIT_TO_VAL(3),  /* Don't open DATA/QUALITY/VARIANCE */
  SMF__NOCREATE_VARIANCE = BIT_TO_VAL(4),  /* If !SMF__NOCREATE_DATA don't map VARIANCE*/
  SMF__NOCREATE_QUALITY  = BIT_TO_VAL(5),  /* If !SMF__NOCREATE_DATA don't map QUALITY */
  SMF__NOCREATE_LUT      = BIT_TO_VAL(6),  /* Don't open pointing LUT */
  SMF__NOFIX_METADATA    = BIT_TO_VAL(7),  /* Do not fix up metadata */
  SMF__NOTTSERIES        = BIT_TO_VAL(8),  /* File is not time series data */
  SMF__NOCREATE_FTS      = BIT_TO_VAL(9),  /* Don't open FTS data */
  SMF__NOFIX_DATA        = BIT_TO_VAL(10), /* Do not fix up data */
  SMF__ISFLAT            = BIT_TO_VAL(11)  /* Do not do any flat fielding */
} smf_open_file_flags;

/* Flags for smf_open_newfile
   Must be individual bits in a single integer and must not clash with
   flags used in smf_create_smf* (the first three smf_open_file_flags above).
   Hence this enum starts at bit 3.
*/
typedef enum {
  SMF__MAP_VAR  = BIT_TO_VAL(3),
  SMF__MAP_QUAL = BIT_TO_VAL(4)
} smf_open_newfile_flags;

/* Data quality bit mask (bits in single byte for QUALITY arrays).

   To add a new quality bit:

   1. Add an entry to the relevant family in this file
   2. Increase the corresponding NQBITS constants (increasing SMF__NQBITS
      if required)
   3. Add quality to smf_qual_str_to_val
   4. Add quality to smf_qual_str
   5. Ensure that string quality names are smaller than SMF_QSTR_MAX

   If you have more than 8 bits in the quality family you now need to modify
   smf_qual_map and smf_qual_unmap to indicate how the new bit maps to the
   compressed version (e.g. TSERIES mapping to TCOMP). If this is the first
   time that this family has exceeded 8 bits and there is no compressed
   equivalent then a new family will be needed to handle the compression
   and associated new code in map/unmap.

 */


/* Time Series Quality */
typedef enum {
  SMF__Q_BADDA   = BIT_TO_VAL(0),   /* Bad sample flagged by DA system  */
  SMF__Q_BADB    = BIT_TO_VAL(1),   /* All samples from this bolo should be ignored */
  SMF__Q_SPIKE   = BIT_TO_VAL(2),   /* Location of a spike */
  SMF__Q_JUMP    = BIT_TO_VAL(3),   /* Location of a DC jump */
  SMF__Q_PAD     = BIT_TO_VAL(4),   /* Padded data */
  SMF__Q_APOD    = BIT_TO_VAL(5),   /* Apodized/boundary data */
  SMF__Q_STAT    = BIT_TO_VAL(6),   /* Telescope stationary */
  SMF__Q_COM     = BIT_TO_VAL(7),   /* Flagged as bad chunk in common-mode rejection */
  SMF__Q_NOISE   = BIT_TO_VAL(8),   /* Bolometer flagged because of noise constraint */
  SMF__Q_EXT     = BIT_TO_VAL(9),   /* Unable to apply extinction correction */
  SMF__Q_LOWAP   = BIT_TO_VAL(10),  /* Apodisation factor is too low to invert */
  SMF__Q_RING    = BIT_TO_VAL(11),  /* Sample suffers from FLT ringing */
  SMF__Q_SSN     = BIT_TO_VAL(12),  /* Flagged as bad by the SSN model */
  SMF__Q_PCA     = BIT_TO_VAL(13),  /* Flagged as bad by the PCA model */
  SMF__Q_IP      = BIT_TO_VAL(14),  /* Flagged as bad by the IP correction */

  /* Quality values are stored in a smf_qual_t variable. This is currently
     2 byte long. This means that if we ever go above 16 quality flags,
     we would need to double the size of smf_qual_t to 4 bytes, thus seriously
     increasing the amount of memory needed by makemap. This is bad, so
     instead we amalgamate low priority flags into one in order to limit
     ourselves to 16 flags. The following flags are never used in normal
     use of makemap. */
  SMF__Q_GENERIC = BIT_TO_VAL(15),  /* Generic value for other flags */
  SMF__Q_FILT    = BIT_TO_VAL(15),  /* Weight less than wlim when filtering */
  SMF__Q_BADEF   = BIT_TO_VAL(15)   /* Optical efficiency correction is bad */
} smf_qual_bits;

/* These macros are for several commonly-used combinations of quality flags */
#define SMF__Q_GOOD (~(smf_qual_t)0)   /* Samples that don't go into the map. Also
                                      don't include in chi^2 */
#define SMF__Q_MOD (SMF__Q_BADDA|SMF__Q_BADB|SMF__Q_PAD) /* Samples that can't
                                                           be modified
                                                           by fitted models */
#define SMF__Q_FIT ~(SMF__Q_APOD|SMF__Q_STAT)            /* Samples that can't
                                                           be used to fit
                                                           time-domain models */
#define SMF__Q_GAP (SMF__Q_IP|SMF__Q_PCA|SMF__Q_SSN|SMF__Q_RING|SMF__Q_BADDA|SMF__Q_SPIKE|SMF__Q_JUMP|SMF__Q_COM|SMF__Q_EXT|SMF__Q_LOWAP)/* Samples
                                                           that should
                                                           be gap-filled */
#define SMF__Q_BOUND (SMF__Q_PAD|SMF__Q_APOD)            /* apodized/padded
                                                            boundary */

/* Quality bits for maps */
typedef enum {
  SMF__MAPQ_AST = BIT_TO_VAL(0),   /* AST mask */
  SMF__MAPQ_FLT = BIT_TO_VAL(1),   /* FLT mask */
  SMF__MAPQ_COM = BIT_TO_VAL(2),   /* COM mask */
  SMF__MAPQ_SSN = BIT_TO_VAL(3),   /* SSN mask */
  SMF__MAPQ_PCA = BIT_TO_VAL(4),   /* PCA mask */
} smf_qual_map_bits;

/* These are used to group SMURF TSERIES quality into related groups that
   can be written to an NDF quality array. Should have no more than
   8 of these. These are compressed versions of the TSERIES family as the
   MAP quality does not have more than 8 bits. They are designated
   as members of the TCOMP family. They are not expected to be visible in
   SMURF itself since they are only used when reading or writing QUALITY to
   a file using 8bit quality. */

typedef enum {
  SMF__TCOMPQ_BAD   = BIT_TO_VAL(0),
  SMF__TCOMPQ_ENDS  = BIT_TO_VAL(1),
  SMF__TCOMPQ_BLIP  = BIT_TO_VAL(2),
  SMF__TCOMPQ_MATCH = BIT_TO_VAL(3),
  SMF__TCOMPQ_TEL   = BIT_TO_VAL(4),
  SMF__TCOMPQ_RING  = BIT_TO_VAL(5),
} smf_tcomp_qual_bits;

/* We have separate quality families */
typedef enum {
  SMF__QFAM_NULL,      /* No family */
  SMF__QFAM_TSERIES,   /* Time series quality */
  SMF__QFAM_MAP,       /* Output map quality */
  SMF__QFAM_TCOMP,     /* Compressed time series quality */
} smf_qfam_t;

/* Number of quality bits in each family. SMF__NQBITS can be used
   for declaring array sizes. */
typedef enum {
  SMF__NQBITS_TSERIES = 16,
  SMF__NQBITS_MAP     = 5,
  SMF__NQBITS_TCOMP   = 6,
  SMF__NQBITS         = 16    /* Largest number of bits in a family */
} smf_qfam_count_t;


/* Define a structure used to hold information cached by smf_create_lutwcs. */
typedef struct smfCreateLutwcsCache {
  AstMapping *map;
  AstFrameSet *frameset;

  /* The SkyFrame used to represent final spherical (Az,El) coords */
  AstSkyFrame *skyframe;

  /* Mappings needed in the tangent plane to celestial longitude,latitude
     Mapping. */
  AstMapping *azel[ 2 ];

  /* The instap values which were hard-wired into the cached Mapping. */
  double instap[ 2 ];

} smfCreateLutwcsCache;

/* Define a structure used to hold information cached by smf_detpos_wcs. */
typedef struct smfDetposWcsCache {
  double *latlut;
  double *lonlut;
  AstPermMap *pmap;
  AstFrame *grid;
  AstSkyFrame *sky;
} smfDetposWcsCache;

/* Global information about the data file itself */

typedef struct smfFile {
  int fd;                    /* file descriptor if data array was mmap'd */
  int ndfid;                 /* NDF ID of file if opened by SMURF */
  int isSc2store;            /* True if file opened by sc2store library */
  int isTstream;             /* True if file contains time series data */
  char name[SMF_PATH_MAX+1]; /* Name of file */
  int mapcoordid;            /* NDF identifier for SMURF.MAPCOORD */
} smfFile;

/* Contains header general header information obtained from the file */

typedef struct smfHead {
  const JCMTState *state;   /* Pointer to current STATE */
  inst_t     instrument;    /* Instrument code - specifies the interface */
  AstFrameSet * wcs;        /* Frameset for a particular time slice (frame) */
  AstFrameSet * tswcs;      /* Frameset for full time series (if tseries) */
  AstFitsChan * fitshdr;    /* FITS header from the file */
  sc2astCache * cache1;     /* Cached info used by sc2ast_createwcs. */
  smfCreateLutwcsCache * cache2; /* Cached info used by smf_create_lutwcs. */
  smfDetposWcsCache * cache3; /* Cached info used by smf_detpow_wcs. */
  dim_t curframe;           /* Index corresponding to current frame */
  dim_t nframes;            /* Number of frames in smfData */
  smf_obstype obstype;      /* Observation type */
  smf_obsmode obsmode;      /* observing mode */
  smf_swmode swmode;        /* switching mode */
  smf_obstype seqtype;      /* Sequence type */
  smf_inbeam_t inbeam;      /* What is in the beam (bitmask) */
  smf_realinst_t realinst;  /* Real instrument */
  int isCloned;             /* If false, allState is owned by this
                               struct, if true it should not be freed */
  JCMTState *allState;     /* Array of STATE for every time slice */
  unsigned int ndet;       /* Number of focal plane detectors */
  double * fplanex;   /* X coords (radians) of focal plane detectors */
  double * fplaney;   /* Y coords (radians) of focal plane detectors */
  double * detpos;    /* Tracking coords (radians) of detectors */
  double * tsys;      /* System Noise temperatures */
  char * detname;     /* Concatenated list of null-terminated detector names */
  int dpazel;               /* Flag: does "detpos" hold AZEL values? */
  double instap[2];         /* instrument aperture (focal plane offsets)  */
  double telpos[3];         /* West LON/LAT/Alt of telescope (deg/deg/m) */
  double scanvel;           /* Scan velocity in arcsec/sec */
  double steptime;          /* Steptime in seconds */
  char * ocsconfig;           /* Configuration XML */
  char units[SMF__CHARLABEL]; /* Data units */
  char dlabel[SMF__CHARLABEL];/* Label associated with data */
  char title[SMF__CHARLABEL]; /* Title associated with data */
  char obsidss[SZFITSTR];     /* Unique observation subsys id */
  smf_modeltype mtype;        /* type if iterative map-maker model container */
} smfHead;

/* This structure contains ancilliary information obtained from a raw
   data file that may be useful to SMURF. "heatval" is not read
   directly using smf_open_file but is used for flatfield calculations.
*/

struct smfData;

typedef struct smfDA {
  double *flatcal;           /* pointer to flatfield calibration */
  double *flatpar;           /* pointer to flatfield parameters */
  double *heatval;           /* Heater values in DAC units for flatfield */
  double refres;             /* Reference resistor used to calculate flatfield */
  smf_flatmeth flatmeth;     /* Flatfield algorithm name */
  size_t nflat;              /* number of flat coeffs per bol */
  size_t nheat;              /* number of elements in heatval */
  struct smfData *dksquid;   /* dark squid for each column */
} smfDA;

/* Structure containing data computed by FTS2 pipeline operations */
typedef struct smfFts
{
  struct smfData* zpd;       /* ZPD indeces, [m x n] dimensional data */
  struct smfData* fpm;       /* Coefficitients of the fitting polynomial for
                              * each bolometer in the subarray,
                              * [m x n x p] dimensional data */
  struct smfData* sigma;     /* Standard deviations obtained for each bolometer
                              * in the subarray, [m x n] dimensional data */
  struct smfData* dead;      /* Dead pixel indicator [m x n] dimensional data */
  struct smfData* a;         /* a band (low frequency (1/f)) integrated power [m x n] dimensional data */
  struct smfData* b;         /* b band (in band signal) integrated power [m x n] dimensional data */
  struct smfData* c;         /* c band (noise region) integrated power [m x n] dimensional data */
  struct smfData* d;         /* d band (1st harmonic) integrated power [m x n] dimensional data */
  struct smfData* phaseFit;  /* Phase X^2 goodness of fit measure [m x n] dimensional data */
  struct smfData* cosmicRays;/* Number of cosmic rays occuring [m x n] dimensional data */
  struct smfData* fluxJumps; /* Number of flux jumps occuring [m x n] dimensional data */
} smfFts;

/* This struct stores DREAM parameters */
typedef struct smfDream {
  size_t nvert;              /* Number of jiggle vertices */
  double jigscal;            /* SMU jiggle pattern scale factor (arcsec) */
  int jigvert[DREAM__MXVERT][2];   /* Jiggle vertex positions in DREAM
                                      pattern */
  double jigpath[DREAM__MXSAM][2]; /* X, Y SMU positions during a cycle, in
                                      arcsec */
  size_t ncycles;            /* Number of DREAM cycles in the input file */
  size_t nsampcycle;         /* Number of data samples per cycle */
  size_t ngrid;              /* Number of grid points in reconstruction */
  int gridpts[DREAM__MXGRID][2];   /* X, Y positions for reconstruction grid */
  double gridstep;           /* Spacing of grid in arcsec */
  double *gridwts;           /* Pointer to grid weights array */
  double *invmatx;           /* Pointer to inverse matrix */
} smfDream;

/* This struct is used to contain all information related to a particular
   data file (where possible since sc2store does not return a handle).
*/

typedef struct smfData {
  smfFile * file;            /* File information */
  smfHead * hdr;             /* Header information */
  smfDA * da;                /* If sc2store, associated data arrays */
  smfDream *dream;           /* DREAM parameters */
  smfFts* fts;               /* FTS2 specific information */
  smf_dtype dtype;           /* Data type of DATA and VARIANCE arrays */
  void * pntr[2];            /* Array of pointers to DATA and VARIANCE */
  int isdyn;                 /* If non-zero then data was allocated by smurf */
  smf_qual_t * qual;         /* Pointer for quality information */
  struct smfData * sidequal; /* Override external quality not owned by this smfData */
  smf_qfam_t qfamily;        /* Quality family used in "qual" */
  smf_qual_t qbits;          /* Quality bits to export to NDF */
  dim_t dims[NDF__MXDIM];    /* Dimensions of data array */
  int lbnd[NDF__MXDIM];      /* Lower PIXEL bounds of data array */
  int isFFT;                 /* -1=not fft,0=don't know,>0 if fft data*/
  int isTordered;            /* 0=order by bolo, 1=order by tslice (default) */
  size_t ndims;              /* Number of active dimensions in "dims" */
  int refcount;              /* Reference count for data object */
  int virtual;               /* Flag for extracted timeslices */
  double *poly;              /* Polynomial scan fits */
  size_t ncoeff;             /* Number of coefficients in polynomial */
  int * lut;                 /* Pointing lookup table */
  double * theta;            /* Scan direction each time slice */
  AstKeyMap *history;        /* History entries */
} smfData;

/* This structure is a container for multiple, usually related,
   smfDatas */

typedef struct smfArray {
  smfData **sdata;            /* pointers to smfData, static or dynamic */
  smfData *stdata[SMF__MXSMF];/* Pointers to pre-allocated smfDatas */
  smfData **dyndata;          /* pointer to dynamically allocated array */
  dim_t ndat;                 /* Number of smfDatas in current smfArray */
  dim_t dynsize;              /* Size of dynamically allocated array */
  int   owndata;              /* If true, the smfDatas are owned by the
                                 smfArray and can be freed by the smfArray */
} smfArray;

/* This struct is used to group related files together */

typedef struct smfGroup {
  Grp *grp;                  /* Copy of input Grp */
  size_t **subgroups;        /* Indices into Grp [ngroups][nrelated] */
  size_t *chunk;             /* Flag for continuous chunks in time [ngroups]*/
  dim_t *tlen;               /* Length in time slices each chunk [ngroups]*/
  size_t ngroups;            /* Number of subgroups */
  size_t nrelated;           /* Maximum number of related files */
} smfGroup;

/* Structure containing pointers to data required for DIMM component
   calculation */
typedef struct smfDIMMData {
  smfArray **res;            /* array of smfArray's of model residuals */
  smfArray **noi;            /* array of smfArray's of variance estimates */
  smfArray **qua;            /* array of smfArray's of quality flags */
  smfArray **lut;            /* array of smfArray's of pointing LUTs */
  smfArray **ext;            /* array of smfArray's of extinction corrections */
  smfArray **gai;            /* array of smfArray's of bolo gain corrections */
  smfArray **com;            /* array of smfArray's of common mode signal */
  smfArray **pcacom;         /* A COM model used within the PCA model */
  smfArray **pcagai;         /* A GAI model used within the PCA model */
  double *map;               /* pointer to the current map estimate */
  double *lastmap;           /* pointer to the previous map estimate */
  int *hitsmap;              /* pointer to the current hits map */
  smf_qual_t *mapqual;       /* pointer to the current map quality */
  double *mapvar;            /* pointer to the current map variance estimate */
  double *mapweight;         /* pointer to the current map weight */
  double *mapweightsq;       /* pointer to the current map weight^2 */
  int mapok;                 /* Do the map arrays contain usable values yet? */
  dim_t mdims[2];            /* dimensions of map */
  dim_t msize;               /* number of elements in map */
  double *chisquared;        /* total chisquared at each chunk */
  AstFrameSet *outfset;      /* contains map->sky transformation */
  int *lbnd_out;             /* map lower bounds */
  int *ubnd_out;             /* map upper bounds */
  unsigned char *ast_mask;   /* Map indicating region to be masked in ast */
  unsigned char *com_mask;   /* Map indicating region to be masked in com */
  unsigned char *flt_mask;   /* Map indicating region to be masked in flt */
  unsigned char *ssn_mask;   /* Map indicating region to be masked in ssn */
  unsigned char *pca_mask;   /* Map indicating region to be masked in pca */
  int iter;                  /* Iteration number */
  int ast_skipped;           /* True if the subtraction of AST was skipped
                                on the previous iteration */
  dim_t noi_boxsize;         /* The number of samples in a NOI box. */
  double pixsize;            /* Nominal map pixel size in arc-seconds */
  int poldata;               /* Are we mapping polarimetric Q or U values? */
  double mapchange;          /* Normalised change in map caused by previous iteration */
} smfDIMMData;


/* Prototype for function pointer to different models used by DIMM */
typedef void(*smf_calcmodelptr)( ThrWorkForce*, smfDIMMData*, int, AstKeyMap*,
                                 smfArray**, int, int* );

/* Prototype for function pointer to expand different DIMM model components */
typedef void(*smf_expmodelptr)( const smfData*, smfData**, int* );

/* Represents a box in some 2D cartesian coordinate system. */
typedef struct smfBox {
  double lbnd[2];
  double ubnd[2];
} smfBox;

/* Represents a single tile from a full size grid. */
typedef struct smfTile {
  int lbnd[ 3 ];
  int ubnd[ 3 ];
  int elbnd[ 3 ];
  int eubnd[ 3 ];
  int glbnd[ 3 ];
  int gubnd[ 3 ];
  int qlbnd[ 3 ];
  int qubnd[ 3 ];
  Grp *grp;
  int *jndf;
  int size;
  AstMapping *map2d;
  AstMapping *map3d;
  int qxl;
  int qxu;
  int qyl;
  int qyu;
} smfTile;

/* Structure to encapsulate frequency-domain filters implemented with FFTW. */
typedef struct smfFilter {
  size_t apod_length;   /* apodization length */
  double dateobs;       /* UTC MJD start of obs that filter corresponds to */
  double df[2];         /* frequency steps along each axis [Hz or 1/arcsec] */
  dim_t fdims[2];       /* filter frequency dimensions */
  double *imag;         /* Imaginary part of the filter */
  int isComplex;        /* Set if filter is fftw_complex, otherwise double */
  size_t ndims;         /* Should be 1 for time-series, or 2 for maps */
  dim_t rdims[2];       /* corresponding real space dimensions */
  double *real;         /* Real part of the filter */
  AstFrameSet *wcs;     /* Frameset describing filter */
  double wlim;          /* Minimum weight for valid filtered values */
} smfFilter;

/* Structure for static headers of DIMM files. Only some of the entries
   are used, such as the data dimension fields in data, and steptime
   in hdr. */
typedef struct smfDIMMHead {
  smfData data;
  smfHead hdr;
} smfDIMMHead;

/* Structure used to pass argument values to astRebinSeqF/D running in a
   different thread. */
typedef struct smfRebinSeqArgs {
  int is_double;
  AstMapping *this;
  double wlim;
  int ndim_in;
  const int *lbnd_in;
  const int *ubnd_in;
  void *in;
  void *in_var;
  int spread;
  const double *params;
  int flags;
  double tol;
  int maxpix;
  float badval_f;
  double badval_d;
  int ndim_out;
  const int *lbnd_out;
  const int *ubnd_out;
  int *lbnd;
  int *ubnd;
  void *out;
  void *out_var;
  double *weights;
  int64_t nused;
  int ijob;
} smfRebinSeqArgs;

/* Structure used to pass detector-independent data to smf_rebincube_paste2d/3d
   running in a different thread. */
typedef struct smfRebincubeNNArgs1 {
  int badmask;
  dim_t nchan;
  dim_t nchanout;
  int *spectab;
  int *specpop;
  dim_t nxy;
  int genvar;
  float *data_array;
  float *var_array;
  double *wgt_array;
  int *pop_array;
  dim_t nout;
  int is2d;
} smfRebincubeNNArgs1;

/* Structure used to pass detector-dependent data to smf_rebincube_paste2d/3d
   running in a different thread. */
typedef struct smfRebincubeNNArgs2 {
  smfRebincubeNNArgs1 *common;
  float *work;
  int iv0;
  double wgt;
  double invar;
  float *ddata;
  int64_t nused;
  int nreject;
  int naccept;
} smfRebincubeNNArgs2;

typedef struct smfRebinMapData {
  smfData *data;
  int rebinflags;
  AstSkyFrame *abskyfrm;
  AstMapping *sky2map;
  int moving;
  int spread;
  const double *params;
  int udim[ 2 ];
  double *map;
  double *variance;
  double *weights;
  int ijob;
  double *bolovar;
  int64_t nused;
  fts2Port fts_port;
} smfRebinMapData;

/* Struct to facilitate sorting of data by time.
   Array of these should be built up and passed
   to qsort.
*/

typedef struct {
  double sortval; /* Primary sort value. Generic double. Can be an MJD */
  void *misc;     /* Pointer to additional information */
  int index;      /* an index that will be sorted with the sortval */
  char name [GRP__SZNAM+1]; /* string to carry around naming the item */
} smfSortInfo;


/* Struct to store a description of a fixed step. */

typedef struct smfStepFix {
  int start;      /* Index of time slice at start of step */
  int end;        /* Index of time slice at end of step */
  int ibolo;      /* Index of bolometer containing step */
  double size;    /* The size of the step rise or fall */
  int id;         /* Integer identifier for the step fix */
  int corr;       /* Was step fixed as a "correlated step"? */
} smfStepFix;

/* Struct to store a description of a fixed step. */

typedef struct smfFITSMaths {
  int nname;      /* The number of names in "names" */
  char **names;   /* Array of FITS keyword names used in the expression */
  AstMathMap *map;/* The MathMap that defines the expression. */
} smfFITSMaths;

/* Struct to store a table of values falling in a specified map pixel (see smf_diag.c). */

typedef struct smfSampleTable {
  char *table;    /* The name of the ascii file to receive the table */
  int xpix;       /* X pixel index of the map pixel */
  int ypix;       /* X pixel index of the map pixel */
  size_t nrow;    /* Number of rows in the table */
  size_t *times;  /* A column of time slices indices */
  size_t *bolos;  /* A column of bolometer indices */
  dim_t ncol;     /* Number of other columns */
  double **colvals;/* Columns of values */
  char **colnames; /* The name for each column */
} smfSampleTable;


/* These are the bits defined in the DA for use in the SCUBA-2 DRCONTROL
   structure to indicate whether we are missing information from a particular
   DRAMA task. A completely valid state item will have a DRCONTROL flag
   of zero. These definitions should match those defined in sc2headman_struct.h
   in the online software. */

typedef enum {
  DRCNTRL__SMU_BIT = 1,
  DRCNTRL__PTCS_BIT = 2,
  DRCNTRL__SCUBA2_BIT = 4,
  DRCNTRL__RTS_BIT = 8,
  DRCNTRL__FTS2_BIT = 16,
  DRCNTRL__POL2_BIT = 32
} drcntrl_bits;

/* and define a combo value to indicate loss of telescope-ness */
#define DRCNTRL__POSITION ( DRCNTRL__SMU_BIT | DRCNTRL__PTCS_BIT )

/* Sub-instruments */

typedef enum {
  SMF__SUBINST_NONE,
  SMF__SUBINST_850,
  SMF__SUBINST_450,
  SMF__SUBINST_NSUBINST  /* This is always the last entry */
} smf_subinst_t;

/* Filtering options */

typedef enum {
  SMF__FILT_NONE,
  SMF__FILT_MEAN,     /* running mean */
  SMF__FILT_MEDIAN,   /* median filter */
  SMF__FILT_MAX,      /* max of the local values */
  SMF__FILT_MIN       /* min of the local values */
} smf_filt_t;

/* Math functions for fitting */

typedef enum {
  SMF__MATH_NULL          = 0,
  SMF__MATH_GAUSS         = 1,
  SMF__MATH_GAUSSHERMITE1 = 2,
  SMF__MATH_GAUSSHERMITE2 = 3,
  SMF__MATH_VOIGT         = 4,
  SMF__MATH_POLYNOMIAL    = 5,
  SMF__MATH_HISTOGRAM     = 6
} smf_math_function;


/* Some old POL2 data has POL_ANG values in arbitrary integer encoding
   units rather than radians. This is is the encoder value that
   corresponds to 2.PI. */
#define SMF__MAXPOLANG 944000


#endif /* SMF_TYP_DEFINED */
