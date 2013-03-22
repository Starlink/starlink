************************************************************************
      BLOCK DATA AGI_BLOCK
*+
*  Name:
*     AGI_BLOCK

*  Purpose:
*     Block data for initialising counters.

*  Language:
*     VAX Fortran

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Block data for initialising counters.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Common Blocks:
      INCLUDE 'agi_cref'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'
      INCLUDE 'agi_eps'

*  Initialise Common:
      DATA CREF / 0 /
      DATA CNEST / 1 /
      DATA CURPID / 0 /
      DATA OLDEPS / ' ' /
      DATA NEWEPS / ' ' /
      DATA CLREPS / .FALSE. /
*.

      END


