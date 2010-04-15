      SUBROUTINE POL1_FBBOX( LBND1, UBND1, LBND2, UBND2, LBND3,
     :                       UBND3, LBND4, UBND4, DAT, STATUS )
*+
*  Name:
*     POL1_FBBOX

*  Purpose:
*     Find the bounding box enclosing the good data in the supplied cube.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_FBBOX( LBND1, UBND1, LBND2, UBND2, LBND3, UBND3,
*                      LBND4, UBND4, DAT, STATUS )

*  Description:
*     This routine modifies the supplied bounds so that they exclude any
*     margins containing only bad pixels in the supplied 3 or 4
*     dimensional cube.

*  Arguments:
*     LBND1 = INTEGER (Given and Returned)
*        The lower bound on the first axis.
*     UBND1 = INTEGER (Given and Returned)
*        The upper bound on the first axis.
*     LBND2 = INTEGER (Given and Returned)
*        The lower bound on the second axis.
*     UBND2 = INTEGER (Given and Returned)
*        The upper bound on the second axis.
*     LBND3 = INTEGER (Given and Returned)
*        The lower bound on the third axis.
*     UBND3 = INTEGER (Given and Returned)
*        The upper bound on the third axis.
*     LBND4 = INTEGER (Given and Returned)
*        The lower bound on the fourth axis. Should be supplied as 1 if
*        the data does not have a frequency axis.
*     UBND4 = INTEGER (Given and Returned)
*        The upper bound on the fourth axis. Should be supplied as 1 if
*        the data does not have a frequency axis.
*     DAT( LBND1:UBND1, LBND2:UBND2, LBND3:UBND3, LBND4:UBND4 ) = Real (Given)
*        The data cube.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the entire cube is bad, the supplied bounds are returned
*     unchanged.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-2000 (DSB):
*        Original version.
*     22-JAN-2001 (DSB):
*        Added fourth dimension.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given and Returned:
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER LBND3
      INTEGER UBND3
      INTEGER LBND4
      INTEGER UBND4

*  Arguments Given:
      REAL DAT( LBND1:UBND1, LBND2:UBND2, LBND3:UBND3, LBND4:UBND4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Axis 1 loop count
      INTEGER J                  ! Axis 2 loop count
      INTEGER K                  ! Axis 3 loop count
      INTEGER L                  ! Axis 4 loop count
      INTEGER NL1                ! New lower bound on axis 1
      INTEGER NL2                ! New lower bound on axis 2
      INTEGER NL3                ! New lower bound on axis 3
      INTEGER NL4                ! New lower bound on axis 4
      INTEGER NU1                ! New upper bound on axis 1
      INTEGER NU2                ! New upper bound on axis 2
      INTEGER NU3                ! New upper bound on axis 3
      INTEGER NU4                ! New upper bound on axis 4
      LOGICAL ALLBAD             ! Is the entire cube filled with bad values?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize the new bounds to extreme values.
      NL1 = VAL__MAXI
      NU1 = VAL__MINI
      NL2 = VAL__MAXI
      NU2 = VAL__MINI
      NL3 = VAL__MAXI
      NU3 = VAL__MINI
      NL4 = VAL__MAXI
      NU4 = VAL__MINI

*  Indicate that we have not yet found any good values.
      ALLBAD = .TRUE.

*  Loop round every pixel in the cube.
      DO L = LBND4, UBND4
         DO K = LBND3, UBND3
            DO J = LBND2, UBND2
               DO I = LBND1, UBND1

*  If this pixel does not have a bad value...
                  IF( DAT( I, J, K, L ) .NE. VAL__BADR ) THEN

*  Indicate that we have found at least one good pixel.
                     ALLBAD = .FALSE.

*  Extend the new bounds if necessary to include this pixel.
                     NL1 = MIN( NL1, I )
                     NU1 = MAX( NU1, I )
                     NL2 = MIN( NL2, J )
                     NU2 = MAX( NU2, J )
                     NL3 = MIN( NL3, K )
                     NU3 = MAX( NU3, K )
                     NL4 = MIN( NL4, L )
                     NU4 = MAX( NU4, L )

                  END IF
               END DO
            END DO
         END DO
      END DO

*  Return the new bounds, but only if at least one good pixel was found.
      IF( .NOT. ALLBAD ) THEN
         LBND1 = NL1
         UBND1 = NU1
         LBND2 = NL2
         UBND2 = NU2
         LBND3 = NL3
         UBND3 = NU3
         LBND4 = NL4
         UBND4 = NU4
      END IF

      END
