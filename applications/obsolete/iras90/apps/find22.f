      SUBROUTINE FIND22( PBANDS, NEEDDF, SOWAB, WAFOUN, STATUS )
*+
*  Name:
*     FIND22

*  Purpose:
*     To obtain IRAS bands required. IRAS band requirements are input as
*     a string containing waveband identifiers. These are analysed to
*     provide one logical variable for each band. There is an option
*     to translate these logical variables into a string variable which
*     is then used as the dynamic default for the bands required
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND22( PBANDS, NEEDDF, SOWAB, WAFOUN, STATUS )

*  Description:
*     To obtain IRAS bands required. IRAS band requirements are input as
*     a string containing waveband identifiers ie 12, 25, 60, 100.
*     There is an option to create a dynamic default value for the
*     wavebands required parameter. If NEEDDF is set .TRUE. on entry
*     a dynamic default is created by translating the logical values
*     in SOWAB into a string containing the identifier for each waveband
*     logical set .TRUE. and this is used as dynamic default.
*     The user is then requested for the wavebands required.
*     The returned parameter is a string containing some or all of
*     identifiers 12,25,60,100. These are analysed to provide one
*     logical variable for each band, set .TRUE. if the waveband
*     identifier is in the string or .FALSE. otherwise. If the
*     requirements do not contain at least one valid waveband
*     identifier the user is reprompted. The user should enter ! if he
*     does not wish to continue this option.

*  Arguments:
*     PBANDS = CHARACTER * ( * ) (Given)
*        Parameter BANDSREQ for bands required entered as 12,25,60,100
*     NEEDDF = LOGICAL (Given)
*        .TRUE. if the default for the bands parameter should be set
*        from the source waveband requirements rather than using the
*        default of all wavebands.
*     SOWAB( 4 ) = LOGICAL (Given and Returned)
*        Waveband required logicals
*     WAFOUN = LOGICAL (Returned)
*        Flag indicating that some wavebands are required for this
*        source
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_DEF0C, PAR_GET0C

*  Authors:
*     DCP: Diana Parsons ( IPMAF/RAL )
*     {enter_new_authors_here}

*  History:
*     30-SEP-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Arguments Given:
      CHARACTER * ( * )  PBANDS
      LOGICAL NEEDDF

*  Arguments Given and Returned:
      LOGICAL SOWAB( 4 )

*  Arguments Returned:
      LOGICAL WAFOUN

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      CHARACTER * ( 20 ) BANDS   ! Character string to contain the
                                 ! wavelenghts of the bands required
                                 ! for this source
      INTEGER IK                 ! Position in string variable used to
                                 ! load the bands dynamic default
                                 ! strings
      CHARACTER * ( 12 ) WABDEF  ! String to contain dynamic default for
                                 ! bands required parameter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether dynamic default is required
      IF ( NEEDDF ) THEN

*  *********************************************************************
*  Create dynamic default string containg waveband identifiers for each
*  waveband whose logical variable is set .TRUE.
*  *********************************************************************

*  Create an empty dynamic default string
         WABDEF = '            '

*  Set the string position variable to the start of the dynamic default
         IK=1

*  Set the at least one waveband is required flag to FALSE.
         WAFOUN = .FALSE.

*  For first waveband check whether the band is required and if so load
*  the waveband indentifier ( 12 ) into the dynamic default string
         IF ( SOWAB( 1 ) ) THEN
            WABDEF(IK:IK+1) = '12'
            IK = IK + 2

*  Set the at least one waveband is required flag to TRUE, this
*  indicates that we will need a comma inserted before any following
*  waveband identifier
            WAFOUN = .TRUE.
         END IF

*  For second waveband check whether the band is required
         IF ( SOWAB( 2 ) ) THEN

*  Check whether there is a previous waveband in the string, and if so
*  insert a comma
            IF ( WAFOUN ) THEN
               WABDEF(IK:IK) = ','
               IK = IK + 1
            END IF

*  Load the waveband indentifier ( 25 ) into the dynamic default string
            WABDEF(IK:IK+1) = '25'
            IK = IK + 2

*  Set the at least one waveband is required flag to TRUE, this
*  indicates that we will need a comma inserted before any following
*  waveband identifier
            WAFOUN = .TRUE.
         END IF

*  For third waveband check whether the band is required
         IF ( SOWAB( 3 ) ) THEN

*  Check whether there is a previous waveband in the string, and if so
*  insert a comma
            IF ( WAFOUN ) THEN
               WABDEF(IK:IK) = ','
               IK = IK + 1
            END IF

*  Load the waveband indentifier ( 60 ) into the dynamic default string
            WABDEF(IK:IK+1) = '60'
            IK = IK + 2

*  Set the at least one waveband is required flag to TRUE, this
*  indicates that we will need a comma inserted before any following
*  waveband identifier
            WAFOUN = .TRUE.
         END IF

*  For fourth waveband check whether the band is required
         IF ( SOWAB( 4 ) ) THEN

*  Check whether there is a previous waveband in the string, and if so
*  insert a comma
            IF ( WAFOUN ) THEN
               WABDEF(IK:IK) = ','
               IK = IK + 1
            END IF

*  Load the waveband indentifier ( 100 ) into the dynamic default string
            WABDEF(IK:IK+2) = '100'
            WAFOUN = .TRUE.
         END IF

*  Check whether any wavebands have been found
         IF ( WAFOUN ) THEN

*  Set the character string as dynamic default for the bands required.
            CALL PAR_DEF0C( PBANDS, WABDEF, STATUS )
         ELSE

*  If wavebands have not been found set '12,25,60,100' as dynamic
*  default for the bands required.
            WABDEF = '12,25,60,100'
            CALL PAR_DEF0C( PBANDS, WABDEF, STATUS )

         END IF

      END IF

*  *********************************************************************
*  Ask user for bands required as a string
*  *********************************************************************

*  Program returns here if the bands required parameter does not contain
*  at least one valid band identifier ie WAFOUN = .FALSE.
 100  CONTINUE

*  If a default value is required for the wavebands parameter rather
*  than a dynamic default set according to the waveband requirements for
*  the source
      IF ( .NOT. NEEDDF ) THEN

*  Set '12,25,60,100' as dynamic default for the bands required.
         WABDEF = '12,25,60,100'
         CALL PAR_DEF0C( PBANDS, WABDEF, STATUS )
      END IF

*  Obtain the bands required from the user
      CALL PAR_GET0C( PBANDS, BANDS, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PBANDS, STATUS )

*  If parameter was not O.K. return
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Translate the bands required into logicals set .TRUE. for each band
*  required, or .FALSE. if it is not required.
*  *********************************************************************

*  Set each of the waveband required logicals to .FALSE. indicating
*  waveband not required
      SOWAB( 1 ) = .FALSE.
      SOWAB( 2 ) = .FALSE.
      SOWAB( 3 ) = .FALSE.
      SOWAB( 4 ) = .FALSE.

*  Set the WAFOUN flag to .FALSE. to indicate that no wavebands have
*  been marked as required for this source.
      WAFOUN = .FALSE.

*  For each waveband check the character string in which the user
*  returned the bands he required to see if it contains the waveband
*  identifier (12,25,60, or 100). If it does, set the waveband logical
*  to true indicating this band is required, and set the WAFOUN flag to
*  .TRUE. indicating that some waveband is required.
      IF ( INDEX( BANDS, '12') .NE. 0 ) THEN
         SOWAB( 1 ) = .TRUE.
         WAFOUN = .TRUE.
      END IF

      IF ( INDEX( BANDS, '25') .NE. 0 ) THEN
         SOWAB( 2 ) = .TRUE.
         WAFOUN = .TRUE.
      END IF

      IF ( INDEX( BANDS, '60') .NE. 0 ) THEN
         SOWAB( 3 ) = .TRUE.
         WAFOUN = .TRUE.
      END IF

      IF ( INDEX( BANDS, '100') .NE. 0 ) THEN
         SOWAB( 4 ) = .TRUE.
         WAFOUN = .TRUE.
      END IF

*  Check whether valid waveband requirements were entered to FIND22
      IF ( .NOT. WAFOUN ) THEN

*  If wavebands were invalid
         CALL MSG_OUT( ' ',
     :   ' Wavebands required must contain at least one valid waveband',
     :   STATUS )
         CALL MSG_OUT( ' ',
     :   ' identifier ie 12, 25, 60, 100,  or ! can be entered '//
     :   ' to the prompt to return to menu ', STATUS )
         GO TO 100
      END IF

      END
