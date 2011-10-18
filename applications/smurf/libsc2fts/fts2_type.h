/*
*+
*  Name:
*     fts2_type.h

*  Purpose:
*     Definitions of general purpose types needed by FTS2.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Definitions of general purpose types needed by FTS2.

*  Authors:
*     Coskun OBA (UoL)
*     Tim Jenness (JAC, Hawaii)

*  History :
*     2010-07-09 (COBA):
*        Created
*     2010-08-04 (TIMJ):
*        Remove boolean enum for the time being because some compilers
*        define FALSE and TRUE themselves and this breaks the build.
*     2010-08-09 (TIMJ):
*        Move subarray definitions to sc2ast
*     2010-08-26 (COBA):
*        Add smf_deglitchmode
*     2010-08-27 (COBA):
*        Rename scan mode enum
*     2011-09-26 (COBA):
*        Add smf_fts2apodizationmethod enum

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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

// FTS2 SCAN MODES
typedef enum smf_fts2scanmode
{
  SMF__FTS2_SCANMODE_UNKNOWN = -1,    // UNKNOWN
  SMF__FTS2_SCANMODE_FSCAN = 0,       // FAST SCAN
  SMF__FTS2_SCANMODE_STEPINT = 1      // STEP AND INTEGRATE
} smf_fts2scanmode;

// FTS3 DEGLITCHING MODES
typedef enum smf_deglitchmode
{
  SMF__DEGLITCH_ALL = 0,              // DEGLITCH ALL
  SMF__DEGLITCH_CORE = 1,             // DEGLITCH CORE
  SMF__DEGLITCH_TAIL = 2              // DEGLITCH TAIL
} smf_deglitchmode;

// FTS2 APODIZATION METHODS
typedef enum smf_fts2apodizationmethod
{
  SMF__FTS2_APODIZATION_NONE  = 0,    // NO APODIZATION
  SMF__FTS2_APODIZATION_GAUSS = 1,    // GAUSSIAN APODIZATION
  SMF__FTS2_APODIZATION_NB11  = 11,   // NORTON-BEER 1.1
  SMF__FTS2_APODIZATION_NB12  = 12,   // NORTON-BEER 1.2
  SMF__FTS2_APODIZATION_NB13  = 13,   // NORTON-BEER 1.3
  SMF__FTS2_APODIZATION_NB14  = 14,   // NORTON-BEER 1.4
  SMF__FTS2_APODIZATION_NB15  = 15,   // NORTON-BEER 1.5
  SMF__FTS2_APODIZATION_NB16  = 16,   // NORTON-BEER 1.6
  SMF__FTS2_APODIZATION_NB17  = 17,   // NORTON-BEER 1.7
  SMF__FTS2_APODIZATION_NB18  = 18,   // NORTON-BEER 1.8
  SMF__FTS2_APODIZATION_NB19  = 19,   // NORTON-BEER 1.9
  SMF__FTS2_APODIZATION_NB20  = 20,   // NORTON-BEER 2.0
} smf_fts2apodizationmethod;
