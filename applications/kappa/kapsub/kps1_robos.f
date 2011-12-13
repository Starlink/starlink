      SUBROUTINE KPS1_ROBOS( DIMS, SQRMAX, XLARGE, ROTSIZ, LONG, SHORT,
     :                       STATUS )

*+
*  Name:
*     KPS1_ROBOS

*  Purpose:
*     Calculates the size of a rotated box for ROTATE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ROBOS( DIMS, SQRMAX, XLARGE, ROTSIZ, LONG, SHORT,
*                      STATUS )

*  Description:
*     This routine computes the size of a rotated box.  XLARGE is set
*     to .true. if the first dimension of the input array is greater
*     than or equal to the second dimension of the input array, and
*     LONG and SHORT are set equal to the longer dimension and shorter
*     dimension of the input array respectively.  A value for the size
*     of the rotaton box, ROTSIZ, which is to be less than or equal to
*     the maximum allowed size, SQRMAX, is determined.
*
*     In a sense this is doing preapratory work for blocking (in the
*     NDF sense) the full array.

*  Arguments:
*     DIMS( 2 ) = INTEGER (Given)
*        Dimensions of data array to be rotated.
*     SQRMAX = INTEGER (Given)
*        Maximum allowed size for ROTSIZ, the size of the sub-array
*        to be rotated.
*     XLARGE = LOGICAL (Returned)
*        Set to .TRUE. if first dimension of input array is greater
*        than the second dimension.
*     ROTSIZ = INTEGER (Returned)
*        Size of the subsections of the input array which will be
*        rotated.
*     LONG = INTEGER (Returned)
*        Longer dimension of the array to be rotated.
*     SHORT = INTEGER (Returned)
*        Shorter dimension of the array to be rotated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If no error on entry then
*        If first dimension of input array is greater than or equal to
*          the second dimension of the input array then
*           XLARGE is set to .true.
*           LONG is set equal to the first dimension of the input array
*           SHORT is set equal to the second dimension of the input
*             array
*        Else
*           XLARGE is set to .false.
*           LONG is set equal to the second dimension of the input array
*           SHORT is set equal to the first dimension of the input array
*        Endif
*        Calculate the size of the rotation box
*        If SHORT is greater than the maximum allowed size for the
*          rotation box then
*           The smallest number of squares, NUMSQ, of side less than or
*           equal to the maximum allowed size of the rotation box, which
*           can be fitted along the shortest side of the input array is
*           calculated. The size of the rotation box, ROTSIZ, is then
*           given by dividing SHORT by NUMSQ. If NUMSQ does not divide
*           exactly into SHORT then ROTSIZ is increased by one to ensure
*           that all the data will be included.
*        Else
*           The size of the rotation box, ROTSIZ, is set equal to SHORT.
*        Endif
*     Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986 Science & Engineering Research
*     Council. Copyright (C) 1995 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*        Documentation brought up to date.
*     1986 September 9 (MJC):
*        Renamed parameters section to arguments and tidied.
*     1995 May 16 (MJC):
*        Renamed from ROTAS3.  Used an SST prologue and modern
*        variable declarations.  Corrected grammatical errors.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE global constants

*  Arguments Given:
      INTEGER DIMS( 2 )
      INTEGER SQRMAX

*  Arguments Returned:
      INTEGER ROTSIZ
      INTEGER LONG
      INTEGER SHORT
      LOGICAL XLARGE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NUMSQ              ! Number of rotation boxes along
                                 ! shorter side
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Sort out which dimension of input array is the longer.
      IF ( DIMS( 1 ) .GE. DIMS( 2 ) ) THEN

*  First dimension of input array is longer (or dimensions equal).
         XLARGE = .TRUE.
         LONG   = DIMS( 1 )
         SHORT  = DIMS( 2 )
      ELSE

*  Second dimension of input array is longer.
         XLARGE = .FALSE.
         LONG   = DIMS( 2 )
         SHORT  = DIMS( 1 )
      END IF

*  Calculate the size of the box to be rotated.
      IF ( SHORT .GT. SQRMAX ) THEN

*  Calculate the number of squares along the shorter side.
         NUMSQ = SHORT / SQRMAX
         IF ( MOD( SHORT, SQRMAX ) .NE. 0 ) THEN
            NUMSQ = NUMSQ + 1
         END IF

*  Calculate the size of the box for rotation.
         ROTSIZ = SHORT / NUMSQ

*  Make sure that all the data are included.
         IF ( MOD( SHORT, NUMSQ ) .NE. 0 ) THEN
            ROTSIZ = ROTSIZ + 1
         END IF
      ELSE

*  The size of rotation box equals the shortest dimension of input
*  array.
         ROTSIZ = SHORT
      END IF

      END
