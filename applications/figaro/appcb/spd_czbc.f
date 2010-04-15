      SUBROUTINE SPD_CZBC( REASON, STATUS )
*+
*  Name:
*     SPD_CZBC

*  Purpose:
*     ADAM-based MOVIE call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZBC( REASON, STATUS )

*  Description:
*     This is the call back for the MOVIE application for the
*     ADAM-based interface.
*
*     In the case of the Motif-based interface we are free to use
*     the Motif widgets to store, display, and make editable, the
*     current values of a parameter. But with the ADAM-based interface
*     this is not so easy, due to the complex behaviour of the ADAM
*     parameter system, which is more or less impossible to control from
*     the programme. Thus the principal parameter repository are SAVE
*     variables in this routine and the ADAM parameter system is used
*     only when prompting should occur.

*  Arguments:
*     REASON = INTEGER (Given)
*        The call back reason:
*         0 Clean up,
*         1 start up,
*         2 access input cube and get work spaces,
*         3 display forward sequence,
*         4 display backward sequence,
*         5 display specified frame,
*         6 display previous frame,
*         7 display next frame,
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     CLNUP = INTEGER
*        This flag is used to register the need for clean-up action
*        whence the application is de-selected. This flag is initially
*        zero, but is non-zero while clean up is necessary and pending.
*     FRSTT = INTEGER
*        This flag is used to register the need for additional action
*        necessary when the application performs its first real action.
*        Here for example, the flag is used to decide whether accessing
*        a new cube implies releasing any currently accessed cube: if
*        access occurs for the first time there is nothing to release.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     tdca: Tim Ash (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     19 May 1994 (hme):
*        Original version.
*     25 May 1994 (hme):
*        Cancel NDF parameter before associating. While this makes it
*        impossible to specify the NDF on the command line, it is
*        necessary to avoid losing the NDF access after PAR_CANCL.
*     15 Feb 1999 (tdca):
*        Specified value for MODE parameter can now be more than two
*        characters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER REASON

*  Status:
      INTEGER STATUS             ! Global status

*  Local Static Variables:
      LOGICAL ENABL( 7 )         ! True if call back reason enabled
      INTEGER INFO               ! INFO parameter
      INTEGER MODE               ! MODE parameter
      INTEGER CLNUP              ! See notes
      INTEGER FRSTT              ! See notes
      INTEGER OMAX               ! The highest pen number on the device
      INTEGER NDF                ! Cube NDF identifier
      INTEGER AXIS               ! AXIS parameter
      INTEGER FRAME              ! FRAME parameter
      REAL DELAY                 ! DELAY parameter
      REAL IMIN, IMAX            ! LOW, HIGH parameters

      SAVE ENABL, INFO, MODE, CLNUP, FRSTT, OMAX,
     :   NDF, AXIS, FRAME, DELAY, IMIN, IMAX

*  Local Volatile Variables:
      INTEGER I, J               ! Temporary integers
      LOGICAL T_INFO             ! LOGICAL version of INFO
      CHARACTER * ( 30 ) TEMP_MODE ! Temporary untrunicated version of MODE
      CHARACTER * ( 2 ) T_MODE   ! CHAR version of MODE

*  Local Data:
      DATA
     :   ENABL / .TRUE., .TRUE.,
     :      .FALSE., .FALSE., .FALSE., .FALSE., .FALSE. /,
     :   INFO  / 1 /, MODE  / 1 /,
     :   FRSTT / 1 /, CLNUP / 0 /,
     :   FRAME / 0 /, DELAY / 0. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Change cursor (Motif only).
*  Defer ADAM error reporting (Motif only).


*  Clean up action.
*  ================

      IF ( REASON .EQ. 0 ) THEN

*     Common call back.
         CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
     :      NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )

*     Save current viewport as AGI picture.
*     Close graphics device, without cancelling the parameter.
         CALL SPD_UGAD( 'DATA', 'SPECDRE_MOVIE', I, STATUS )
         CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )

*     End NDF context.
         CALL NDF_END( STATUS )

*     Clear cleanup flag, set firsttime flag.
         CLNUP = 0
         FRSTT = 1

*     Disable 3 to 7, i.e. all display options.
         ENABL(3)  = .FALSE.
         ENABL(4)  = .FALSE.
         ENABL(5)  = .FALSE.
         ENABL(6)  = .FALSE.
         ENABL(7)  = .FALSE.


*  Start up action.
*  ================

      ELSE IF ( REASON .EQ. 1 ) THEN

*     Common call back.
         CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
     :      NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )

*     Clear cleanup flag, set firsttime flag.
         CLNUP = 0
         FRSTT = 1


*  Access.
*  =======

      ELSE IF ( REASON .EQ. 2 ) THEN

*     If first time, get modal parameters, INFO and MODE.
         IF ( FRSTT .NE. 0 ) THEN
            CALL PAR_GET0L( 'INFO', T_INFO, STATUS )
            IF ( T_INFO ) THEN
               INFO = 1
            ELSE
               INFO = 0
            END IF
            CALL PAR_GET0C( 'MODE', TEMP_MODE, STATUS )
            T_MODE = TEMP_MODE( 1:2 )
            CALL CHR_UCASE(  T_MODE )
            IF ( T_MODE .EQ. 'FA' ) THEN
               MODE = 1
            ELSE IF ( T_MODE .EQ. 'SQ' ) THEN
               MODE = 2
            ELSE IF ( T_MODE .EQ. 'FI' ) THEN
               MODE = 3
            ELSE
               MODE = 1
               IF ( INFO .NE. 0 ) CALL MSG_OUT( 'SPD_CZBC_M01',
     :            'MOVIE: Warning: Mode not recognised, ' //
     :            'will use fast mode.', STATUS )
            END IF
         END IF

*     If first time, open NDF context, else close and re-open NDF
*     context.
         IF ( FRSTT .EQ. 0 ) CALL NDF_END( STATUS )
         CALL NDF_BEGIN

*     Access input NDF.
*     We cannot cancel after associating, since that destroys access to
*     the NDF.
*     We do not cancel if this is the first time, so that there is some
*     chance that a command line specification gets through.
         IF ( FRSTT .EQ. 0 ) CALL PAR_CANCL( 'IN', STATUS )
         CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Get parameters AXIS, LOW, HIGH. These are consulted only when a
*     new NDF is accessed. But then they are prompted.
         CALL PAR_GET0I( 'AXIS', AXIS, STATUS )
         CALL PAR_GET0R( 'LOW',  IMIN, STATUS )
         CALL PAR_GET0R( 'HIGH', IMAX, STATUS )
         CALL PAR_CANCL( 'AXIS', STATUS )
         CALL PAR_CANCL( 'LOW',  STATUS )
         CALL PAR_CANCL( 'HIGH', STATUS )

*     If first time.
*     Access graphics device. Use the current AGI picture, clear it.
*     Must also find out the highest pen number. 16 or less is
*     rejected.
         IF ( FRSTT .NE. 0 ) THEN
            CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', I, J, STATUS )
            CALL PGQCOL( I, OMAX )
            IF ( OMAX .LE. 16 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CZBC_EXX', 'MOVIE: Graphics ' //
     :            'device has insufficient colours reserved.', STATUS )
               CALL SPD_UGAB( 'DEVICE', .TRUE., STATUS )
            END IF
         END IF

*     Common call back.
         CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
     :      NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )

*     Set cleanup flag, clear firsttime flag.
         CLNUP = 1
         FRSTT = 0
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Enable 3 to 7, all the display options.
         ENABL(3)  = .TRUE.
         ENABL(4)  = .TRUE.
         ENABL(5)  = .TRUE.
         ENABL(6)  = .TRUE.
         ENABL(7)  = .TRUE.


*  Display specified frame.
*  ========================

      ELSE IF ( REASON .EQ. 5 .AND. ENABL(5) ) THEN

*     Get FRAME parameter.
         CALL PAR_GET0I( 'FRAME', FRAME, STATUS )
         CALL PAR_CANCL( 'FRAME', STATUS )

*     Common call back.
         CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
     :      NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )


*  Other options (3,4,6,7).
*  ========================

      ELSE IF ( REASON .GE. 3 .AND. REASON .LE. 7 ) THEN

*     Just call the common call back.
         IF ( ENABL(REASON) ) CALL SPD_CZBD( REASON, INFO, MODE, OMAX,
     :      NDF, AXIS, FRAME, DELAY, IMIN, IMAX, STATUS )

      END IF


*  Tidy up.
*  ========

 500  CONTINUE

*  Intercept or flush ADAM error reports (Motif only).

      END



