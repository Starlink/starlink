      SUBROUTINE IRM_GTBND( PARAM, DEFAUL, NULL, BAND, STATUS )
*+
*  Name:
*     IRM_GTBND

*  Purpose:
*     Get a survey waveband index from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GTBND( PARAM, DEFAUL, NULL, BAND, STATUS )

*  Description:
*     This routine gets an index for a survey waveband from the
*     environment using the supplied parameter. The user specifies the
*     waveband by giving its central wavelength in microns (eg 12, 25,
*     etc ), but the corresponding index ( 1, 2, etc ) is returned in
*     argument BAND. The user is re-prompted if a bad value is
*     obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use (integer valued)
*     DEFAUL = INTEGER (Given)
*        The default waveband index. If  a legal value is supplied
*        (1-4), then this value is established as the dynamic default
*        for the parameter before getting a value for the parameter. If
*        an illegal value is supplied, no dynamic default is set up, but
*        no error is reported.
*     NULL = LOGICAL (Given)
*        If true, then a null response for the parameter does not cause
*        an error status to be returned, but is treated as being
*        equivalent to the user specifying the band given by DEFAUL.
*     BAND = INTEGER (Returned)
*        The waveband index. Returned equal to DEFAUL if an error
*        occurs (unless DEFAUL is an invalid waveband index, in which
*        case 1 is returned).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'PAR_ERR'          ! PAR error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER DEFAUL
      LOGICAL NULL

*  Arguments Returned:
      INTEGER BAND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXBAD             ! Max. no. of bad values.
      PARAMETER ( MAXBAD = 4 )

*  Local Variables:
      LOGICAL  AGAIN             ! True if a new parameter value is
                                 ! required.
      INTEGER  ERRBND            ! Value to return if an error occurs.
      INTEGER  I                 ! Loop count.
      INTEGER  NBAD              ! No of bad values.
      INTEGER  WAVEL             ! Supplied band wave length.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If required set up the dynamic default, and store the value to return
*  in the event of an error.
      IF( DEFAUL .GT. 0 .AND. DEFAUL .LE. I90__BANDS ) THEN
         CALL PAR_DEF0I( PARAM, I90__WAVEL( DEFAUL ), STATUS )
         ERRBND = DEFAUL
      ELSE
         ERRBND = 1
      END IF

*  Set the number of bad values gioven to zero.
      NBAD = 0

*  Loop round until a good value is obtained.
      AGAIN = .TRUE.
      DO WHILE( AGAIN )

*  Get a value for the parameter.
         CALL PAR_GET0I( PARAM, WAVEL, STATUS )

*  If a null value was given, and if a null value is acceptable, annull
*  the error and return the default value.
         IF( STATUS .EQ. PAR__NULL .AND. NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            BAND = DEFAUL
            AGAIN = .FALSE.

*  If no error occurred, check the obtained value and construct a list
*  of legal values.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

            DO I = 1, I90__BANDS

               IF( WAVEL .EQ. I90__WAVEL( I ) ) THEN
                  AGAIN = .FALSE.
                  BAND = I
               END IF

               CALL MSG_SETI( 'LIST', I90__WAVEL( I ) )
               CALL MSG_SETC( 'LIST', ' ' )

            END DO

*  If an incorrect value was given, increment the number of bad values
*  given.
            IF( AGAIN ) THEN
               NBAD = NBAD + 1

*  If the maximum no. of bad values has not yet been reached, tell the
*  user about the error and then cancel the parameter value prior to
*  re-prompting.
               IF( NBAD .LT. MAXBAD ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'V', BAND )
                  CALL ERR_REP( 'IRM_GTBND_ERR1',
     :             '  Bad survey waveband (^V). Legal values are ^LIST',
     :                       STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARAM, STATUS )

*  If too many bad values have been given, abort.
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'V', BAND )
                  CALL ERR_REP( 'IRM_GTBND_ERR2',
     :          '  Bad survey waveband (^V). Too many bad values given',
     :                       STATUS )
                  AGAIN = .FALSE.

               END IF

            END IF

*  If an error occurred, abort.
         ELSE
            AGAIN = .FALSE.

         END IF

      END DO

*  If an error has occurred, give a context message and return a safe
*  band number.
      IF( STATUS .NE. SAI__OK ) THEN
         BAND = ERRBND
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'IRM_GTBND_ERR3',
     : 'IRM_GTBND: Unable to get a survey waveband using parameter %^P',
     :                  STATUS )
      END IF

      END
