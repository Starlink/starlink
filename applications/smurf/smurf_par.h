/*
 *+
 *  Name:
 *     smurf_par.h

 *  Purpose:
 *     Constants for the smurf application

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Header File

 *  Invocation:
 *     #include "smurf_par.h"

 *  Description:
 *     Constants used by the SMURF infrastructure.

 *  Notes:
 *     All the POSIX Math constants (M_PI, M_PI_2 etc) are made available
 *     when including the file. The SLA conversion constants (DD2R, DR2D etc)
 *     are also available. Note, that to avoid confusion, for constants that are
 *     defined both in SLA and POSIX (DPI and DPIBY2), the POSIX version is to be
 *     preferred. This means that M_PI_2 should be used for PI/2 rather than DPIBY2.

 *  Authors:
 *     Andy Gibb (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     Ed Chapin (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2005-09-27 (AGG):
 *        Initial test version
 *     2006-07-27 (TIMJ):
 *        Add slamac.h constants
 *     2006-09-14 (AGG):
 *        Add LEN__METHOD & SZFITSCARD
 *     2006-12-12 (AGG):
 *        Add SPD
 *     2008-07-07 (TIMJ):
 *        GSL is available always.
 *     2008-08-27 (TIMJ):
 *        Add SC2FLAT__DTOI
 *     2009-10-07 (TIMJ):
 *        Add SIPREFIX and SIMULT.
 *     2010-07-14 (TIMJ):
 *        Update SC2FLAT__DTOI value.
 *     2010-07-20 (TIMJ):
 *        Retain incorrect DTOI value until we can pin down accurate values
 *        for the resistors.
 *     2011-04-08 (TIMJ):
 *        Use correct DTOI value.
 *     2011-06-08 (EC):
 *        RAW2CURRENT is no longer a constant, moved to smf_raw2current.c
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2008-2011 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2005,2006,2011 University of British Columbia.
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

#ifndef SMURF_PAR_DEFINED
#define SMURF_PAR_DEFINED

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Check for math.h and then (preferably) gsl_math.h as we want to use
   a pre-defined value for PI and multiples/fractions thereof */
#if HAVE_MATH_H
#  include <math.h>
#endif

/* We know we have GSL installed */
#include <gsl/gsl_math.h>

#ifndef M_PI_2
#  ifdef M_PI
#    define M_PI_2  (M_PI/2)
#  else
error can not determine PI
#  endif
#endif

/* Assume we now have access to the standard POSIX definitions */

/* These convenient definitions come from slamac.h - we prefer to use the
   slamac.h header file directly. */

#if HAVE_SLAMAC_H
# include "slamac.h"
#else

/* pi */
#define DPI 3.1415926535897932384626433832795028841971693993751

/* 2pi */
#define D2PI 6.2831853071795864769252867665590057683943387987502

/* 1/(2pi) */
#define D1B2PI 0.15915494309189533576888376337251436203445964574046

/* 4pi */
#define D4PI 12.566370614359172953850573533118011536788677597500

/* 1/(4pi) */
#define D1B4PI 0.079577471545947667884441881686257181017229822870228

/* pi^2 */
#define DPISQ 9.8696044010893586188344909998761511353136994072408

/* sqrt(pi) */
#define DSQRPI 1.7724538509055160272981674833411451827975494561224

/* pi/2:  90 degrees in radians */
#define DPIBY2 1.5707963267948966192313216916397514420985846996876

/* pi/180:  degrees to radians */
#define DD2R 0.017453292519943295769236907684886127134428718885417

/* 180/pi:  radians to degrees */
#define DR2D 57.295779513082320876798154814105170332405472466564

/* pi/(180*3600):  arcseconds to radians */
#define DAS2R 4.8481368110953599358991410235794797595635330237270e-6

/* 180*3600/pi :  radians to arcseconds */
#define DR2AS 2.0626480624709635515647335733077861319665970087963e5

/* pi/12:  hours to radians */
#define DH2R 0.26179938779914943653855361527329190701643078328126

/* 12/pi:  radians to hours */
#define DR2H 3.8197186342054880584532103209403446888270314977709

/* pi/(12*3600):  seconds of time to radians */
#define DS2R 7.2722052166430399038487115353692196393452995355905e-5

/* 12*3600/pi:  radians to seconds of time */
#define DR2S 1.3750987083139757010431557155385240879777313391975e4

/* 15/(2pi):  hours to degrees x radians to turns */
#define D15B2P 2.3873241463784300365332564505877154305168946861068

#endif /* HAVE_SLAMAC_H */

/* Tidy up to prevent leakage of SLA symbols */
#undef DPI
#undef DPIBY2


/* Other conversions */

/* Arcsec to Degrees  (1/3600) */
#define DAS2D 0.00027777777777777777777777777777777777777777777778

/* Days to seconds */
#define SPD 86400.0

/* Other miscellaneous SMURF definitions */

/* Length of string for various `methods' */
#define LEN__METHOD 20
/* Length of a FITS record, does not include nul */
#define SZFITSCARD 80

/* Length of a string that can be read from a FITS record.
   Includes NUL */
#define SZFITSTR 70

/* SI prefix and multiplied for output data. Used for Watts and Amps calculations
  Set to 1e12 if you want all output in pico amps and pico watts.
  Set to 1.0 if you want Watts or Amps.

  Note that we convert raw DAC numbers both to current and power units:
  - include factor for MCE low-pass filter
  - convert everything to pA and pW (Since that is what
   Wayne uses).

   MCE * (DAC->Amps) * (Amps->pico Amps )

   For the conversion call smf_raw2current
 */

#define SIPREFIX "p"
#define SIMULT   1.0e12

/* Heater circuit constant for converting D/A setting to Amps */

#define SC2FLAT__DTOI (24.71e-6/65536)

#endif /* SMURF_PAR_DEFINED */
