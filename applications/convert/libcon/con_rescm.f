      SUBROUTINE CON_RESCM
*+
*  Name:
*     CON_RESCM

*  Purpose:
*     Resets the INTERIM common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_RESCM

*  Description:
*     This routine clears the user INTERIM common blocks so that INTERIM
*     will start constructing tables etc. from scratch.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 December 8 (AJC):
*        Original version.
*     1992 September 4 (MJC):
*        Given an SSE prologue.  Renamed from STL_RESCOM for CONVERT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(DFSCOM)'
      INCLUDE 'INTERIM(CLICOM)'

*.

      PCT_NENT = 0
      DFS_NENT = 0
      CLI_COMBUF = 'NOT ACCESSED'

      END
