      SUBROUTINE IRC_SIMUL( IDC, SIMUL, STATUS )
*+
*  Name:
*     IRC_SIMUL

*  Purpose:
*     See if a CRDD file holds simultaneous detector samples.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_SIMUL( IDC, SIMUL, STATUS )

*  Description:
*     A column of data from the NDF DATA array consists of a set of
*     samples, one from each detector, which all have the same sample
*     number. This routine returns a true value for argument SIMUL if
*     the detector samples in each column were obtained simultaneously.
*     If this is true, it can be assumed that the boresight position at
*     a given sample number is the same for every detector (which can
*     reduce the amount of computation required by an application).

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     SIMUL = LOGICAL (Returned)
*        True if all samples in each column of the DATA array were
*        obtained simultaneously.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           HDS type of the DETAILS component.


*  Arguments Given:
      INTEGER IDC

*  Arguments Returned:
      LOGICAL SIMUL

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_SIMUL_ERR1',
     :                 'IRC_SIMUL: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Set the argument SIMUL appropriately for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         SIMUL = .TRUE.

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_SIMUL_ERR2',
     :                 'IRC_SIMUL does not yet support ^T data',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
