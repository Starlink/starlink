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

/*
 * Boolean
 */

typedef int bool;

//
// FTS-2 SCAN MODES
//
typedef enum SMF__FTS_Mode
{
  UNKNOWN = -1, // Undefined or Unknown
  FSCAN = 0,    // Fast Scan
  STEPINT = 1   // Step-and-Integrate
} FTSMode;

//
//  SCUBA-2 SUBARRAYS
//
typedef enum SMF__SCUBA2_SUBARRAY
{
  s8a = 0, // 850 A
  s8b = 1, // 850 B
  s8c = 2, // 850 C
  s8d = 3, // 850 D
  s4a = 4, // 450 A
  s4b = 5, // 450 B
  s4c = 6, // 450 C
  s4d = 7  // 450 D
} SC2_Subarray;
