      SUBROUTINE PARSECON_PACKL( LU, ARRAY, START, END, STATUS )











*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PACKL( LU, ARRAY, START, END, STATUS )

*  Description:
*     To encode elements of the compiled form of an interface file for a
*     1-D array to reduce the file size.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to write to
*     ARRAY(*) = LOGICAL (Given)
*        The array of values to be encoded
*     START = INTEGER (Given)
*        The first element to be encoded
*     END = INTEGER (Given)
*        The last element to be encoded
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Name:
*     PARSECON_PACKL

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (AJC):
*        Original version.
*     24-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     21-NOV-1996 (AJC):
*        Use NEQV to compare LOGICAL values
*     {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER LU
      LOGICAL ARRAY( * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR

*  Local Variables:
      LOGICAL VBUFF( SUBPAR__MAXPAR ) ! Values buffer
      INTEGER NBUFF( SUBPAR__MAXPAR ) ! Values count buffer
      LOGICAL LASTV               ! The last value handled
      INTEGER NV                 ! The consecutive value counter
      INTEGER BPT                ! Pointer to next entry in buffers
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise counters
      BPT = 1
      NV = 0
      LASTV = ARRAY( START )

*  Loop through ARRAY from START to END
      DO 10, I = START, END

         IF( ARRAY( I ) .NEQV. LASTV ) THEN
*        End of consecutive equal values
            VBUFF( BPT ) = LASTV
            NBUFF( BPT ) = NV
            BPT = BPT + 1
            LASTV = ARRAY( I )
            NV = 1

         ELSE
*        Count consecutive values
            NV = NV + 1

         ENDIF

10    CONTINUE

*  End of ARRAY
*  Save the last value and write the record
      VBUFF( BPT ) = LASTV
      NBUFF( BPT ) = NV

      WRITE ( LU ) BPT, (NBUFF(I),I=1,BPT), (VBUFF(I),I=1,BPT)

      END
