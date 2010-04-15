      SUBROUTINE TRACA4( PARAM1, PARAM2, NDISP, OFMTHD, OFFSET, STATUS )
*+
*  Name:
*     TRACA4

*  Purpose:
*     Get the way and the values to offset the traces.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA4( PARAM1, PARAM2, NDISP, OFMTHD, OFFSET, STATUS )

*  Description:
*     This routine get the way to vertically offset the traces from the
*     environment. And if the offset way is 'FREE', get the offset
*     values for each data trace from the environment.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the method of offset
*        from the environment.
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the offset values for
*        each data trace when the method of offset is 'FREE'.
*     NDISP = INTEGER (Given)
*        The number of traces to be displayed.
*     OFMTHD = INTEGER (Returned)
*        It specifies the method to offset the traces.
*
*          0 :'FREE' offset
*
*          1 :'CONSTANT' offset
*
*          2 :'AVERAGE' offset
*
*     OFFSET( NDISP ) = REAL (Returned)
*        When the offset method is 'FREE', it contains the offset values
*        specified by the user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1991 (WG):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PARAM1*( * )
      CHARACTER PARAM2*( * )
      INTEGER NDISP

*  Arguments Returned:
      INTEGER OFMTHD
      REAL OFFSET( NDISP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER METHOD*8         ! Specified offset method


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the offset method from the enviroment.
      CALL PAR_CHOIC( PARAM1, 'AVERAGE', 'FREE,CONSTANT,AVERAGE',
     :               .FALSE., METHOD, STATUS )

*  Remove the leading blank of METHOD.
      CALL CHR_LDBLK( METHOD )

*  If the specified method is 'FREE', get the offset values for each
*  trace from the environment.
      IF ( METHOD( : 4 ) .EQ. 'FREE' ) THEN
         OFMTHD = 0
         CALL PAR_EXACR( PARAM2, NDISP, OFFSET, STATUS )

*  Otherwise, just set the returned offset code.
      ELSE IF ( METHOD .EQ. 'CONSTANT' ) THEN
         OFMTHD = 1

      ELSE IF ( METHOD( : 7 ) .EQ. 'AVERAGE' ) THEN
         OFMTHD = 2

      END IF

      END
