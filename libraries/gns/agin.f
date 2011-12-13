      SUBROUTINE gns_1AGIN (M,N,LOG,L)
*+
*  Name:
*     gns_1AGIN

*  Purpose:
*     Construct a AGI_n_m name string

*  Language:
*     Starlink Fortran 77

*  Description:
*     A string of the form AGI_n_m is constructed and it and its length
*     returned

*  Arguments:
*     N = INTEGER (Given)
*         Workstation type
*     M = INTEGER (Given)
*         Sequence number
*     LOG = CHAR (Returned)
*         The resulting name
*     L = INTEGER (Returned)
*         The length of the name

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     2-DEC-1991 (NE):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External References:
*     none

*  Implicit Inputs:
*     none

*  Implicit Outputs:
*     none

*-
      IMPLICIT NONE

      INTEGER M, N, L
      CHARACTER *(*) LOG

      CHARACTER*10 CM, CN
      INTEGER I,J

      WRITE (UNIT=CM, FMT='(SP,I10)') M
      I = INDEX (CM,'+')
      WRITE (UNIT=CN, FMT='(SP,I10)') N
      J = INDEX (CN,'+')
      LOG = 'AGI_'//CM(I+1:)//'_'//CN(J+1:)

      L = 4 + (LEN(CM)-I) + 1 + (LEN(CN)-J)

      END

