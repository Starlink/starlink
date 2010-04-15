

      SUBROUTINE ELF1_CANCL(MODE,STATUS)
*+
*  Name:
*     ELF1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_CANCL(MODE,STATUS)

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which parameters must be cancelled.
*

*  Arguments:
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.0) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Backgound count.
         CALL PAR_CANCL('BACK',STATUS)
*      Refine the guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      The guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      The graphics device used for the graphs.
         CALL AGI_CANCL('DEVICE',STATUS)
      END IF

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Galaxy origin co-ordinates.
         CALL PAR_CANCL('ORIGIN',STATUS)
*      Background  count.
         CALL PAR_CANCL('BACK',STATUS)
*      Refine the guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      The guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
      END IF

 9999 CONTINUE

      END



      SUBROUTINE ELP1_CANCL(MODE,STATUS)
*+
*  Name:
*     ELP1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELP1_CANCL(MODE,STATUS)

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which parameters must be cancelled.
*

*  Arguments:
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.0) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Backgound count.
         CALL PAR_CANCL('BACK',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      Refine the position guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Position guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      The graphics display device used for the graphs.
         CALL AGI_CANCL('DEVICE',STATUS)
      END IF

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Galaxy origin co-ordinates.
         CALL PAR_CANCL('ORIGIN',STATUS)
*      Background  count.
         CALL PAR_CANCL('BACK',STATUS)
*      Refine the position guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Position guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE GRA1_CANCL(MODE,STATUS)
*+
*  Name:
*     GRA1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GRA1_CANCL(MODE,STATUS)

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which parameters must be cancelled.
*

*  Arguments:
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.0) THEN
*      Device on which the image is displayed.
         CALL AGI_CANCL('DEVICE',STATUS)
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Display what?
         CALL PAR_CANCL('WHATD',STATUS)
*      Display radius mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      Automatic selection of radii range.
         CALL PAR_CANCL('RRANGE',STATUS)
*      Range of radius over which the scale length calculations
*      are performed.
         CALL PAR_CANCL('FITLIM',STATUS)
      END IF

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      DISPLAY what?
         CALL PAR_CANCL('WHATD',STATUS)
*      Radius display mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      Automatic selection of radii range.
         CALL PAR_CANCL('RRANGE',STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CANCL(MODE,STATUS)
*+
*  Name:
*     SEC1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_CANCL(MODE,STATUS)

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which paramters must be cancelled.
*

*  Arguments:
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.0) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Symmetrical summation.
         CALL PAR_CANCL('MIRROR',STATUS)
*      Backgound count.
         CALL PAR_CANCL('BACK',STATUS)
*      Display brightness mode.
         CALL PAR_CANCL('SURF',STATUS)
*      Display radius mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Auto-locate better galaxy centre.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      The graphics display device used for the graphs.
         CALL AGI_CANCL('DEVICE',STATUS)
      END IF

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Galaxy origin co-ordinates.
         CALL PAR_CANCL('ORIGIN',STATUS)
*      Position angle of sector.
         CALL PAR_CANCL('POSANG',STATUS)
*      Angular width of sector.
         CALL PAR_CANCL('ANGWID',STATUS)
*      Length of sector.
         CALL PAR_CANCL('RLIM',STATUS)
*      Symmetrical summation.
         CALL PAR_CANCL('MIRROR',STATUS)
*      Background  count.
         CALL PAR_CANCL('BACK',STATUS)
*      Brightness display mode.
         CALL PAR_CANCL('SURF',STATUS)
*      Radius display mode.
         CALL PAR_CANCL('RADISP',STATUS)
*      Auto-locate better galaxy origin.
         CALL PAR_CANCL('AUTOL',STATUS)
*      Range of radius over which the scale length calculations
*      are performed.
         CALL PAR_CANCL('FITLIM',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
      END IF

 9999 CONTINUE

      END
