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
*     Coskun (Josh) OBA (UoL)
*     Tim Jenness (JAC, Hawaii)

*  History :
*     2010-07-09 (OBA):
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

typedef enum smf_fts2scanmode
{
  SMF__FTS2_SCANMODE_UNKNOWN = -1, // Undefined or Unknown
  SMF__FTS2_SCANMODE_FSCAN = 0,    // Fast Scan
  SMF__FTS2_SCANMODE_STEPINT = 1   // Step-and-Integrate
} smf_fts2scanmode;

typedef enum smf_deglitchmode
{
  SMF__DEGLITCH_ALL = 0,  /* DEGLITCH BOTH CORE & TAIL */
  SMF__DEGLITCH_CORE = 1, /* DEGLITCH CORE ONLY */
  SMF__DEGLITCH_TAIL = 2  /* DEGLITCH TAIL ONLY */
} smf_deglitchmode;
