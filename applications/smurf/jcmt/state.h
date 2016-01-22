
#ifndef INCLUDED_JCMT_STATE_H
#define INCLUDED_JCMT_STATE_H

/*
*+
*  Name:
*     jcmt/state.h

*  Purpose:
*     Define the public C data structures representing JCMT STATE information

*  Invocation:
*     #include "jcmt/state.h"

*  Language:
*     C Include file

*  Description:
*     This include file defines the basic data structure used to pass STATE
*     information between functions. Additionally, it defines the sizes of string
*     members and the data types and names used by HDS (with ordering information).

*  Notes:
*     The following definitions can be used:
*     - JCMT__EXTNAME and JCMT__EXTTYPE are the name and type of the standard extension
*       in which to store STATE information.
*     - INST__ACSIS / INST__SCUBA2 are bitmasks indicating whether an HDSdataRecord
*       should be used for a particular instrument.
*     - JCMT__SZ* are sizes of strings used in JCMTState structures.
*     - Each JCMTState struct member has a corresponding upper-cased string defining a unique
*       index allowing arrays of locators and pointers to be addressed by name.
*     The following structures are defined:
*     - JCMTState     : All STATE structure available
*     - HDSdataRecord : Information on the individual HDS component in files.
*     The following static data structures are available:
*     - hdsRecords    : Array of HDSdataRecord structs to be used in files.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     BDK: Dennis Kelly (UKATC, Edinburgh)

*  History:
*     17-AUG-2004 (BDK):
*        Original version for SCUBA-2 DA
*     27-FEB-2006 (TIMJ):
*        Original version for ACSIS specwriter
*     25-JUL-2006 (TIMJ):
*        Merge ACSIS and SCUBA-2 into a single include file.
*     27-JUL-2006 (TIMJ):
*        Remove RTS_TASKS. Remove spurious ACSIS members. Add more docs.
*     28-JUL-2006 (TIMJ):
*        Add inst_t enum.
*     31-JUL-2006 (TIMJ):
*        Add AZTEC instrument
*     09-OCT-2006 (TIMJ):
*        Add TCS_TAI field to state structure.
*     04-JAN-2007 (TIMJ):
*        Add ACS_OFF_EXPOSURE field to state structure. Add FE_DOPPLER and FE_LOFREQ.
*     08-JAN-2007 (TIMJ):
*        C++ compilers are much pickier in inst_t usage
*     24-JUN-2008 (TIMJ):
*        Add TCS_PERCENT_CMP. Fix a compiler const warning.
*     07-JUL-2008 (TIMJ):
*        - Remove WVM_TW/TH/QUAL
*        - Do not write JOS_DRCONTROL or SMU_XYZ to SCUBA-2 files
*        - Change type of SC2_HEAT to _INTEGER and WVM_TIME to _DOUBLE.
*     26-MAY-2009 (TIMJ):
*        Allow ACS_OFFEXPOSURE to be missing since it is now populated later.
*        TCS_TAI now optional for ACSIS but mandatory for SCUBA-2.
*     22-SEP-2009 (TIMJ):
*        JOS_DRCONTROL can now be read from SCUBA-2 files.
*        Add DOME and ENCODER positions.
*     2009-11-18 (TIMJ):
*        Add SC2_MIXTEMP in kelvin
*     2010-09-22 (TIMJ):
*        Add SC2_BIAS and SC2_FPUTEMP
*     2011-01-11 (TIMJ):
*        Define largest string length for _CHAR types
*     2012-02-23 (TIMJ):
*        Add SC2_1KNTDTEMP

*  Copyright:
*     Copyright (C) 2008-2012 Science and Technology Facilities Council.
*     Copyright (C) 2004, 2006, 2007 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

*-
*/

/* Name of standard NDF extension for state information */

#define JCMT__EXTNAME  "JCMTSTATE"
#define JCMT__EXTTYPE  "RTS_ARR"

/* Some sizes used for dimensioning the string arrays */
#define JCMT__SZRTS_TASKS      80
#define JCMT__SZTCS_SOURCE     32
#define JCMT__SZTCS_TR_SYS     16
#define JCMT__SZACS_SOURCE_RO  16
#define JCMT__SZSMU_CHOP_PHASE  1
#define JCMT__SZTCS_BEAM        1

/* The longest string length */
#define JCMT__SZLARGEST        JCMT__SZRTS_TASKS

/* Flags indicating whether a component should be used in ACSIS or SCUBA-2. Instrument
   usability can be checked by AND'ing the value in hdsRecord array with one of these.
 */

typedef enum {
  /* Should be indvidual bits */
  INST__NONE   = 0x00000000,  /* No instruments */
  INST__ACSIS  = 0x00000001,
  INST__SCUBA2 = 0x00000002,
  INST__AZTEC  = 0x00000004,
  INST__ALL    = 0x0FFFFFFF  /* All bits set */
} inst_t;

/* This struct represents all the information available from a JCMT SEQUENCE as published
   in STATE structures by each task involved. Not all will be defined. The struct ordering
   can be changed at any time and should never be relied upon.
*/
typedef struct JCMTState {
  unsigned int rts_num;
  double rts_end;    /* MJD TAI of end of sequence step */
  char   rts_tasks[JCMT__SZRTS_TASKS+1];
  double smu_x;
  double smu_y;
  double smu_z;
  char   smu_chop_phase[JCMT__SZSMU_CHOP_PHASE+1];
  short  smu_jig_index;
  double smu_az_jig_x;
  double smu_az_jig_y;
  double smu_az_chop_x;
  double smu_az_chop_y;
  double smu_tr_jig_x;
  double smu_tr_jig_y;
  double smu_tr_chop_x;
  double smu_tr_chop_y;
  double tcs_tai;
  double tcs_airmass;
  double tcs_az_ang;
  double tcs_az_ac1;
  double tcs_az_ac2;
  double tcs_az_dc1;
  double tcs_az_dc2;
  double tcs_az_bc1;
  double tcs_az_bc2;
  char   tcs_beam[JCMT__SZTCS_BEAM+1];
  short  tcs_index;
  short  tcs_percent_cmp;
  char   tcs_source[JCMT__SZTCS_SOURCE+1];
  char   tcs_tr_sys[JCMT__SZTCS_TR_SYS+1];
  double tcs_tr_ang;
  double tcs_tr_ac1;
  double tcs_tr_ac2;
  double tcs_tr_dc1;
  double tcs_tr_dc2;
  double tcs_tr_bc1;
  double tcs_tr_bc2;
  double tcs_en_dc1;  /* Telescope encoder coordinates */
  double tcs_en_dc2;
  double tcs_dm_abs;  /* Dome actual azimuth absolute */
  double tcs_dm_rel;  /* Dome actual azimuth relative */
  short  jos_drcontrol; /* JOS DR control tag */
  float  enviro_rel_hum;
  float  enviro_pressure;
  float  enviro_air_temp;
  float  wvm_t12;
  float  wvm_t42;
  float  wvm_t78;
  double wvm_time;
  unsigned short sc2_heat;
  unsigned short sc2_bias;
  float  sc2_mixtemp;
  float  sc2_fputemp;
  float  sc2_1kntdtemp;
  float  acs_exposure;
  float  acs_offexposure;
  short  acs_no_prev_ref;
  short  acs_no_next_ref;
  short  acs_no_ons;
  char   acs_source_ro[JCMT__SZACS_SOURCE_RO+1];
  double pol_ang;
  float  fts_pos;
  double fe_lofreq;
  double fe_doppler;
} JCMTState;

/* Generate indices to allow the programmer to access individual elements
   of the component definitions, guaranteeing uniqueness.
   Not certain that these need to be public since the hdsRecords array is designed to
   include the number.
*/

typedef enum
{
   RTS_NUM,
   RTS_END,
   RTS_TASKS,
   SMU_X,
   SMU_Y,
   SMU_Z,
   SMU_CHOP_PHASE,
   SMU_JIG_INDEX,
   SMU_AZ_JIG_X,
   SMU_AZ_JIG_Y,
   SMU_AZ_CHOP_X,
   SMU_AZ_CHOP_Y,
   SMU_TR_JIG_X,
   SMU_TR_JIG_Y,
   SMU_TR_CHOP_X,
   SMU_TR_CHOP_Y,
   TCS_TAI,
   TCS_AIRMASS,
   TCS_AZ_ANG,
   TCS_AZ_AC1,
   TCS_AZ_AC2,
   TCS_AZ_DC1,
   TCS_AZ_DC2,
   TCS_AZ_BC1,
   TCS_AZ_BC2,
   TCS_BEAM,
   TCS_INDEX,
   TCS_PERCENT_CMP,
   TCS_SOURCE,
   TCS_TR_SYS,
   TCS_TR_ANG,
   TCS_TR_AC1,
   TCS_TR_AC2,
   TCS_TR_DC1,
   TCS_TR_DC2,
   TCS_TR_BC1,
   TCS_TR_BC2,
   TCS_EN_DC1,
   TCS_EN_DC2,
   TCS_DM_ABS,
   TCS_DM_REL,
   JOS_DRCONTROL,
   ENVIRO_REL_HUM,
   ENVIRO_PRESSURE,
   ENVIRO_AIR_TEMP,
   WVM_T12,
   WVM_T42,
   WVM_T78,
   WVM_TIME,
   SC2_HEAT,
   SC2_BIAS,
   SC2_MIXTEMP,
   SC2_FPUTEMP,
   SC2_1KNTDTEMP,
   ACS_EXPOSURE,
   ACS_OFFEXPOSURE,
   ACS_NO_PREV_REF,
   ACS_NO_NEXT_REF,
   ACS_NO_ONS,
   ACS_SOURCE_RO,
   POL_ANG,
   FTS_POS,
   FE_LOFREQ,
   FE_DOPPLER,
   JCMT_COMP_NUM     /* Tells us the total number of available components */
} JCMT_HEADLIST;

/* Private Function to put quotes around a symbol so that we can do
   CPP string concatenation */
#define myxstr(s) mystr(s)
#define mystr(s) #s
#define CHARTYP(s) "_CHAR*" myxstr(s)

/* Define the names and types to be used in the HDS component and whether
   they should be used for ACSIS, SCUBA-2 or are common to all instruments.. */

/* To allow easy grouping, we define a struct */
typedef struct HDSdataRecord {
  int  position;    /* actual position in list. Used so that JCMT_HEADLIST is not forced to be in same order. */
  const char * type;
  const char * name;
  inst_t instrument;  /* bit mask indicating whether a particular instrument uses this field */
  inst_t optional;    /* bit mask indicating whether it is an optional component for that instrument */
} HDSdataRecord;

static const HDSdataRecord hdsRecords[JCMT_COMP_NUM] =
  {
    { RTS_NUM, "_INTEGER", "RTS_NUM", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { RTS_END, "_DOUBLE", "RTS_END", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { RTS_TASKS, CHARTYP(JCMT__SZRTS_TASKS), "RTS_TASKS", INST__ACSIS, INST__NONE },
    { SMU_X, "_DOUBLE", "SMU_X", INST__ACSIS, INST__NONE },
    { SMU_Y, "_DOUBLE", "SMU_Y", INST__ACSIS, INST__NONE },
    { SMU_Z, "_DOUBLE", "SMU_Z", INST__ACSIS, INST__NONE },
    { SMU_CHOP_PHASE, CHARTYP(JCMT__SZSMU_CHOP_PHASE), "SMU_CHOP_PHASE", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_JIG_INDEX, "_WORD", "SMU_JIG_INDEX", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_AZ_JIG_X, "_DOUBLE", "SMU_AZ_JIG_X", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_AZ_JIG_Y, "_DOUBLE", "SMU_AZ_JIG_Y", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_AZ_CHOP_X, "_DOUBLE", "SMU_AZ_CHOP_X", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_AZ_CHOP_Y, "_DOUBLE", "SMU_AZ_CHOP_Y", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_TR_JIG_X, "_DOUBLE", "SMU_TR_JIG_X", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_TR_JIG_Y, "_DOUBLE", "SMU_TR_JIG_Y", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_TR_CHOP_X, "_DOUBLE", "SMU_TR_CHOP_X", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { SMU_TR_CHOP_Y, "_DOUBLE", "SMU_TR_CHOP_Y", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    /* TCS_TAI is only optional for ACSIS because it was missing in data earlier than 20061013.
       SCUBA-2 should always have this component */
    { TCS_TAI, "_DOUBLE", "TCS_TAI", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__ACSIS },
    { TCS_AIRMASS, "_DOUBLE", "TCS_AIRMASS", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_ANG, "_DOUBLE", "TCS_AZ_ANG", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_AC1, "_DOUBLE", "TCS_AZ_AC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_AC2, "_DOUBLE", "TCS_AZ_AC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_DC1, "_DOUBLE", "TCS_AZ_DC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_DC2, "_DOUBLE", "TCS_AZ_DC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_BC1, "_DOUBLE", "TCS_AZ_BC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_AZ_BC2, "_DOUBLE", "TCS_AZ_BC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_BEAM, CHARTYP(JCMT__SZTCS_BEAM), "TCS_BEAM", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_INDEX, "_WORD", "TCS_INDEX", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_PERCENT_CMP, "_WORD", "TCS_PERCENT_CMP", (inst_t)(INST__ACSIS | INST__SCUBA2),
      (inst_t)(INST__ACSIS|INST__SCUBA2) },
    { TCS_SOURCE, CHARTYP(JCMT__SZTCS_SOURCE), "TCS_SOURCE", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_SYS, CHARTYP(JCMT__SZTCS_TR_SYS), "TCS_TR_SYS", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_ANG, "_DOUBLE", "TCS_TR_ANG", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_AC1, "_DOUBLE", "TCS_TR_AC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_AC2, "_DOUBLE", "TCS_TR_AC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_DC1, "_DOUBLE", "TCS_TR_DC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_DC2, "_DOUBLE", "TCS_TR_DC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_BC1, "_DOUBLE", "TCS_TR_BC1", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_TR_BC2, "_DOUBLE", "TCS_TR_BC2", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { TCS_EN_DC1, "_DOUBLE", "TCS_EN_DC1", (inst_t)(INST__ACSIS | INST__SCUBA2),
      (inst_t)(INST__ACSIS|INST__SCUBA2) },
    { TCS_EN_DC2, "_DOUBLE", "TCS_EN_DC2", (inst_t)(INST__ACSIS | INST__SCUBA2),
      (inst_t)(INST__ACSIS|INST__SCUBA2) },
    { TCS_DM_ABS, "_DOUBLE", "TCS_DM_ABS", (inst_t)(INST__ACSIS | INST__SCUBA2),
      (inst_t)(INST__ACSIS|INST__SCUBA2) },
    { TCS_DM_REL, "_DOUBLE", "TCS_DM_REL", (inst_t)(INST__ACSIS | INST__SCUBA2),
      (inst_t)(INST__ACSIS|INST__SCUBA2) },
    { JOS_DRCONTROL, "_WORD", "JOS_DRCONTROL", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__SCUBA2 },
    { ENVIRO_REL_HUM, "_REAL", "ENVIRO_REL_HUM", INST__ACSIS, INST__NONE },
    { ENVIRO_PRESSURE, "_REAL", "ENVIRO_PRESSURE", INST__ACSIS, INST__NONE },
    { ENVIRO_AIR_TEMP, "_REAL", "ENVIRO_AIR_TEMP", INST__ACSIS, INST__NONE },
    { WVM_T12, "_REAL", "WVM_T12", INST__SCUBA2, INST__NONE },
    { WVM_T42, "_REAL", "WVM_T42", INST__SCUBA2, INST__NONE },
    { WVM_T78, "_REAL", "WVM_T78", INST__SCUBA2, INST__NONE },
    { WVM_TIME, "_DOUBLE", "WVM_TIME", INST__SCUBA2, INST__NONE },
    { SC2_HEAT, "_UWORD", "SC2_HEAT", INST__SCUBA2, INST__NONE },
    { SC2_BIAS, "_UWORD", "SC2_BIAS", INST__SCUBA2, INST__SCUBA2 }, /* Old data did not have it */
    { SC2_MIXTEMP, "_REAL", "SC2_MIXTEMP", INST__SCUBA2, INST__SCUBA2 }, /* Old data did not have it */
    { SC2_FPUTEMP, "_REAL", "SC2_FPUTEMP", INST__SCUBA2, INST__SCUBA2 }, /* Old data did not have it */
    { SC2_1KNTDTEMP, "_REAL", "SC2_1KNTDTEMP", INST__SCUBA2, INST__SCUBA2 }, /* Old data did not have it */
    { ACS_SOURCE_RO, CHARTYP(JCMT__SZACS_SOURCE_RO), "ACS_SOURCE_RO", INST__ACSIS, INST__NONE },
    { ACS_NO_PREV_REF, "_WORD", "ACS_NO_PREV_REF", INST__ACSIS, INST__NONE },
    { ACS_NO_NEXT_REF, "_WORD", "ACS_NO_NEXT_REF", INST__ACSIS, INST__NONE },
    { ACS_NO_ONS, "_WORD", "ACS_NO_ONS", INST__ACSIS, INST__NONE },
    { ACS_EXPOSURE, "_REAL", "ACS_EXPOSURE", INST__ACSIS, INST__NONE },
    { ACS_OFFEXPOSURE, "_REAL", "ACS_OFFEXPOSURE", INST__ACSIS, INST__ACSIS },
    { POL_ANG, "_DOUBLE", "POL_ANG", (inst_t)(INST__ACSIS | INST__SCUBA2), INST__NONE },
    { FTS_POS, "_REAL", "FTS_POS", INST__SCUBA2, INST__NONE },
    { FE_LOFREQ, "_DOUBLE", "FE_LOFREQ", INST__ACSIS, INST__ACSIS },
    { FE_DOPPLER, "_DOUBLE", "FE_DOPPLER", INST__ACSIS, INST__ACSIS },
  };


/* tidy up namespace */
#undef CHARTYP
#undef mystr
#undef myxstr

/* INCLUDE_JCMT_STATE_H */
#endif
