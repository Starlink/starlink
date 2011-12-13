      SUBROUTINE KPS1_ROBLI( NUMRA, LONG, SHORT, ROTSIZ, XLARGE, IDIM1,
     :                         IDIM2, ARRIN, ODIM1, ODIM2, ARROUT, WORK,
     :                         STATUS )
*+
*  Name:
*     KPS1_ROBLx

*  Purpose:
*     Rotates input array into output array for ROTATE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ROBLx( NUMRA, LONG, SHORT, ROTSIZ, XLARGE, IDIM1, IDIM2,
*    :                ARRIN, ODIM1, ODIM2, ARROUT, WORK, STATUS )

*  Description:
*     This routine rotates an array in 90-degree multiples by blocking
*     (in the NDF sense) the array into square sections.  The input
*     array, ARRIN, is rotated through NUMRA right angles in the
*     clockwise direction as a number of ROTSIZ by ROTSIZ sections.
*     The rotated array is put into ARROUT.

*  Arguments:
*     NUMRA = INTEGER (Given)
*        Number of right-angles through which the data will be rotated.
*     LONG = INTEGER (Given)
*        Longer dimension of the array to be rotated.
*     SHORT = INTEGER (Given)
*        Shorter dimension of the array to be rotated.
*     ROTSIZ = INTEGER (Given)
*        Size of the subsections to be rotated.
*     XLARGE = LOGICAL (Given)
*        Should be .TRUE. if first dimension of the array to be rotated
*        is greater than the second.
*     IDIM1 = INTEGER (Given)
*        The first dimension of the 2-dimensional array to be rotated.
*     IDIM2 = INTEGER (Given)
*        The second dimension of the 2-dimensional array to be rotated.
*     ARRIN( IDIM1, IDIM2 ) = ? (Given)
*        Data to be rotated.
*     ODIM1 = INTEGER (Given)
*        The first dimension of the output 2-dimensional array.
*     ODIM2 = INTEGER (Given)
*        The second dimension of the output 2-dimensional array.
*     ARROUT( ODIM1, ODIM2 ) = ? (Returned)
*        Will contain the rotated data.
*     WORK( ROTSIZ, ROTSIZ ) = ? (Returned)
*        Workspace to hold the subsection for rotation.
*     STATUS = INTEGER( UPDATE )
*        The global status.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     ARRIN, ARROUT, and WORK arguments of the routine must have the
*     data type specified.
*     - If an error occurs during the execution of this routine STATUS
*     will be returned containing the appropriate error value.

*  Algorithm:
*     If no error on entry then
*        ENDL is the position of the start of the last rotation box
*          along the longer side of the input array
*        ENDS is the position of the start of the last rotation box
*          along the shorter side of the input array
*        INDEXL is a pointer to the position of the start of the
*          rotation box along the longer side of the input array
*        INDEXS is a pointer to the position of the start of the
*          rotation box along the shorter side of the input array
*        ROTSIZ is the size of the rotation box
*        Move along longest side of input array
*        For INDEXL from 1, in steps of ROTSIZ, to a value less than or
*          equal to ENDL
*           Move along shortest side of input array
*           For INDEXS from 1, in steps or ROTSIZ, to a value less than
*             or equal to ENDS
*              Rotate box pointed at by INDEXL, INDEXS
*           End for
*           If shorter dimension of input array is not exactly divisible
*             by ROTSIZ then
*              There will be some data left unrotated so set INDEXS to
*                point at ENDS
*              Rotate box
*           Endif
*        Endfor
*        If the longer dimension of the input array is not exactly
*          divisible by ROTSIZ then
*           There will be some data left unrotated so set INDEXL to
*             point at ENDL
*           Move along shortest side of input array
*           For INDEXS from 1, in steps or ROTSIZ, to a value less than
*             or equal to ENDS
*              Rotate box pointed at by INDEXL, INDEXS
*           End for
*           If shorter dimension of input array is not exactly divisible
*             by ROTSIZ then
*              There will be some data left unrotated so set INDEXS to
*                point at ENDS
*              Rotate box
*           Endif
*        Endif
*     Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1989 Science & Engineering
*     Research Council. Copyright (C) 1995 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     27/07/1983 (DB):
*        Original version.
*     17/02/1984 (DB):
*        Documentation brought up to standard.
*     1986 September 9 (MJC):
*        Renamed parameters section to arguments and tidied.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1995 May 16 (MJC):
*        Made generic from ROTAS4.  Used an SST prologue and modern
*        variable declarations.  Corrected grammatical errors.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE global constants

*  Arguments Given:
      INTEGER NUMRA
      INTEGER LONG
      INTEGER SHORT
      INTEGER ROTSIZ
      LOGICAL XLARGE
      INTEGER IDIM1
      INTEGER IDIM2
      INTEGER ODIM1
      INTEGER ODIM2
      INTEGER ARRIN( IDIM1, IDIM2 )

*  Arguments Returned:
      INTEGER ARROUT( ODIM1, ODIM2 )
      INTEGER WORK( ROTSIZ, ROTSIZ )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ENDL               ! Position of last rotation box along
                                 ! longer side
      INTEGER ENDS               ! Position of last rotation box along
                                 ! shorter side
      INTEGER INDEXL             ! Pointer to subsection, long dimension
      INTEGER INDEXS             ! Pointer to subsection, short
                                 ! dimension

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the positions of the last rotation box along longer and
*  shorter dimensions.
      ENDL = LONG  + 1 - ROTSIZ
      ENDS = SHORT + 1 - ROTSIZ

*  Rotate the array as a number of ROTSIZ by ROTSIZ boxes move along
*  the longer side.
      DO INDEXL = 1, ENDL, ROTSIZ

*  Move along shorter side
         DO  INDEXS = 1, ENDS, ROTSIZ

            CALL KPS1_RORBI( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS,
     :                         INDEXL, INDEXS, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, ARROUT, WORK, STATUS )
         END DO

*  Check for unrotated data along the shorter side of the input array.
         IF ( MOD( SHORT, ROTSIZ ) .NE. 0 ) THEN

*  The pointer is set to position of the last rotation box along the
*  shorter side.
            INDEXS = ENDS

            CALL KPS1_RORBI( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS,
     :                         INDEXL, INDEXS, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, ARROUT, WORK, STATUS )
         END IF
      END DO

*  Check for unrotated data along the longer side of the input array.
      IF ( MOD( LONG, ROTSIZ ) .NE. 0 ) THEN

*  The pointer is set to the position of the last rotation box along
*  the longer side.
         INDEXL = ENDL

*  Move along the shorter side.
         DO INDEXS = 1, ENDS, ROTSIZ

            CALL KPS1_RORBI( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS,
     :                         INDEXL, INDEXS, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, ARROUT, WORK, STATUS )
         END DO

*  Check for unrotated data along the shorter side of the input array.
         IF ( MOD( SHORT, ROTSIZ ) .NE. 0 ) THEN

*  The pointer is set to the position of the last rotation box along
*  the shorter side.
            INDEXS = ENDS

            CALL KPS1_RORBI( NUMRA, ROTSIZ, XLARGE, ENDL, ENDS,
     :                         INDEXL, INDEXS, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, ARROUT, WORK, STATUS )
         END IF
      END IF

      END
