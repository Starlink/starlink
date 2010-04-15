      SUBROUTINE FINPUT( ICHAN, EOI, STATUS )
*+
*  Name:
*     FINPUT

*  Purpose:
*     Read the field centres

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FINPUT( ICHAN, EOI, STATUS )

*  Description:
*     Get the field centres from either the input unit ICHAN or from the
*     terminal.

*  Arguments:
*     ICHAN = INTEGER (Given)
*        The input unit number
*     EOI = LOGICAL (Returned)
*        End of input indicator
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     9-DEC-1991 (PMA):
*        First ADAM version.
*        Changed calls to CTOR to CHR_CTOR.
*     4-JAN-1993 (PMA):
*        Chang calls to INTERIM parameter routines to ADAM ones.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     17-MAR-1993 (PMA):
*        Set the default value of the parameter OK with a call to
*        PAR_DEF0L.
*        Test the value of STATUS before calling ERR_FLUSH.
*        If the user hits return when prompted for the RA, the status
*        is not bad (which it is in the INTERIM) environment), so test
*        for the value returned being a blank string.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*     27-APR-1993 (AJJB):
*        Added calls to ERR_FLUSH at points where an invalid value has
*        been entered by user, so that STATUS gets restored to OK value
*        before prompting user again for value, as it was getting into
*        infinite loops.
*     10-MAR-1993 (AJJB):
*        Put in a bit at the end which is jumped to at end-of-file in
*        batch mode, which prints out a message and exits the routine, and
*        an END= in the 1st READ statement to jump to it. Also put in a similar
*        bit which is jumped to on an error condition and ERR= in the READ
*        statements to jump to it.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'MAIN'             ! Main CHART common blocks
*        {descriptions_of_global_variables_referenced}...
      INCLUDE 'CONVF'            ! Conversion factors
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      INTEGER ICHAN

*  Arguments Returned:
      LOGICAL EOI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! CHART parameters
*     CHARACTER * ( 50 ) VAL     ! [local_variable_description]
      CHARACTER * ( 50 ) ERRVAL  ! [local_variable_description]
      CHARACTER * ( 80 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 1 ) ETYPE    ! [local_variable_description]
      LOGICAL RESPONSE           ! [local_variable_description]
      INTEGER NPAR               ! [local_variable_description]
      INTEGER NPOS               ! [local_variable_description]
      REAL DATE                  ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize the end of information flag.
      EOI = .FALSE.

*  Get the CHART parameters.
      CALL GETPARAMS( PARAMS, NPAR , STATUS )
      CALL GETDEFLT( PARAMS, NPAR, 'ERRBOX', ERRVAL, NPOS , STATUS )
      IF ( BATCH ) THEN
         READ( ICHAN, '(A)', ERR=790, END=800 ) VALUE
         CALL CONVRA( VALUE, 1, 80, A, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 810
         READ ( ICHAN, '(A)', ERR=790, END=790 ) VALUE
         CALL CONVDEC( VALUE, 1, 80, D, STATUS )
         READ ( ICHAN, '(A)',ERR=790, END=790 ) VALUE
         CALL CHR_CTOR( VALUE, EQUIN, STATUS )
         READ ( ICHAN, '(A10)',ERR=790, END=790) IDFLD
      ELSE
  210    CONTINUE
         CALL PAR_GET0C( 'RA', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

*  The first IF here tests for a blank response (which forces
*  exit from the loop).
         IF ( VALUE .EQ. ' ' ) GO TO 820

*  Convert the characters into a number, and return on
*  any of three error conditions (specified by STATUS).
*  (Suitable error messages are written by CONVRA)
*
         CALL CONVRA( VALUE, 1, 80, A, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PAR_CANCL( 'RA', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 210
         END IF
*
*   Repeat the process for the DEC
*
  220    CONTINUE
         CALL PAR_GET0C( 'DEC', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         CALL CONVDEC( VALUE, 1, 80, D, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PAR_CANCL( 'DEC', STATUS )
            CALL ERR_FLUSH( STATUS )
            GO TO 220
         END IF

*  Now pick up the equinox, with a default of 1950.0 the first time
*  around. The previous value becomes the the new default.
  230    CONTINUE
         VALUE = '1950.0'
         CALL PAR_DEF0C( 'EQUINOX', VALUE, STATUS )
         CALL PAR_GET0C( 'EQUINOX', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

*  Note that a response of TOD(AY) will pick up today's date and write
*  it in the correct format.
         IF ( VALUE( 1:3 ) .EQ. 'TOD' ) THEN
            CALL TODAY( DATE , STATUS )
         END IF
         CALL CHR_CTOR( VALUE, EQUIN, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'EQUIN_ERR', 'Error in equinox - try again',
     :         STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'EQUINOX', STATUS )
            GO TO 230
         END IF
         IF ( EQUIN .LT. 0.0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'EQUIN_ERR', 'Error in equinox - try again',
     :         STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'EQUINOX', STATUS )
            GO TO 230
         END IF

*  Finally pick up a title (default is a blank record).
  240    CONTINUE
         VALUE = ' '
         CALL PAR_DEF0C( 'TITLE', VALUE, STATUS )
         CALL PAR_GET0C( 'TITLE', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         READ (VALUE,'(A10)') IDFLD

*  Finally check if ERRBOX is set to PROMPT
         ERDIAM(1) = 0.0
         ERDIAM(2) = 0.0
         ERDIAM(3) = 0.0

         IF ( ERRVAL .EQ. 'PROMPT' ) THEN
  245       CONTINUE
            CALL PAR_GET0C( 'TYPE', VALUE, STATUS )
            CALL CHR_UCASE( VALUE )
            ETYPE = VALUE(1:1)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'TYPE_ERR', 'Error in TYPE - try again',
     :            STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'TYPE', STATUS )
               GO TO 245
            ELSE IF ( ETYPE .NE. 'C' .AND. ETYPE .NE. 'Q'
     :          .AND. ETYPE .NE. 'E' ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'TYPE_ERR', 'Error in TYPE - try again',
     :            STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'TYPE', STATUS )
               GO TO 245
            ENDIF

  250       CONTINUE
            CALL PAR_GET0C( 'XDIAM', VALUE, STATUS )
            CALL CHR_UCASE( VALUE )
            CALL CHR_CTOR( VALUE, ERDIAM( 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'XDIAM_ERR', 'Error in XDIAM - try again',
     :         STATUS )
               CALL PAR_CANCL( 'XDIAM', STATUS)
               CALL ERR_FLUSH( STATUS )
               GO TO 250
            ENDIF
            IF ( ERDIAM( 1) .LE. 0.0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'XDIAM_ERR', 'Error in XDIAM - try again',
     :         STATUS )
               CALL PAR_CANCL( 'XDIAM', STATUS)
               GO TO 250
            ENDIF

            IF ( ETYPE .EQ. 'C' ) GO TO 280
  260       CONTINUE
            CALL PAR_GET0C( 'YDIAM', VALUE, STATUS )
            CALL CHR_UCASE( VALUE )
            CALL CHR_CTOR( VALUE, ERDIAM( 2 ), STATUS)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'YDIAM_ERR', 'Error in YDIAM - try again',
     :         STATUS )
               CALL PAR_CANCL( 'YDIAM', STATUS)
               CALL ERR_FLUSH( STATUS )
               GO TO 260
            ENDIF
            IF ( ERDIAM( 2 ) .LE. 0.0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'YDIAM_ERR', 'Error in YDIAM - try again',
     :         STATUS )
               CALL PAR_CANCL( 'YDIAM', STATUS)
               GO TO 260
            ELSE IF ( ERDIAM( 2 ) .GT. ERDIAM( 1 ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'YDIAM_GT_XDIAM',
     :            'Error YDIAM greater than XDIAM - try again', STATUS )
               CALL PAR_CANCL( 'YDIAM', STATUS)
               GO TO 260
            ENDIF

  270       CONTINUE
            CALL PAR_GET0C( 'ORIENT', VALUE, STATUS )
            CALL CHR_UCASE( VALUE )
            CALL CHR_CTOR( VALUE, ERDIAM( 3 ), STATUS)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'ORIENT_ERR',
     :            'Error in ORIENT - try again', STATUS)
               CALL PAR_CANCL( 'ORIENT', STATUS)
               CALL ERR_FLUSH( STATUS )
               GO TO 270
            ENDIF
280         CONTINUE
            IF ( ETYPE .EQ. 'Q' ) THEN
               ERDIAM( 1 ) = -ERDIAM( 1 )
               ERDIAM( 2 ) = -ERDIAM( 2 )
            ENDIF
         ENDIF

*  The user then has the option to accept these values
         RESPONSE = .TRUE.
         CALL PAR_DEF0L( 'OK', RESPONSE, STATUS )
         CALL PAR_GET0L( 'OK', RESPONSE, STATUS )
         IF ( .NOT. RESPONSE ) THEN
            CALL PAR_CANCL( 'RA', STATUS )
            CALL PAR_CANCL( 'DEC', STATUS )
            CALL PAR_CANCL( 'EQUINOX', STATUS )
            CALL PAR_CANCL( 'TITLE', STATUS )
            CALL PAR_CANCL( 'TYPE', STATUS )
            CALL PAR_CANCL( 'XDIAM', STATUS )
            CALL PAR_CANCL( 'YDIAM', STATUS )
            CALL PAR_CANCL( 'ORIENT', STATUS )
            CALL PAR_CANCL( 'OK', STATUS )
            GO TO 210
         END IF
      END IF

*  Now precess the co-ordinates.
      CALL PRECES( A , D , AP , DP , EQUIN , 1950.0 , STATUS )
      CALL PRECES( A , D , AO , DO , EQUIN , EQUOUT , STATUS )

*  Clear the parameters for the next time around
      CALL PAR_CANCL( 'RA', STATUS )
      CALL PAR_CANCL( 'DEC', STATUS )
      CALL PAR_CANCL( 'EQUINOX', STATUS )
      CALL PAR_CANCL( 'TITLE', STATUS )
      CALL PAR_CANCL( 'TYPE', STATUS )
      CALL PAR_CANCL( 'XDIAM', STATUS )
      CALL PAR_CANCL( 'YDIAM', STATUS )
      CALL PAR_CANCL( 'ORIENT', STATUS )
      CALL PAR_CANCL( 'OK', STATUS )

*  and set the error pointer for OK
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      GO TO 899

*  Come here if there's an error on reading the file in batch mode
  790 CONTINUE
      CALL ERR_REP( ' ', 'FINPUT: Error reading file in batch mode',
     :   STATUS )
      EOI = .TRUE.
      GOTO 899

*  Come to here if an end-of-file was found in batch mode
  800 CONTINUE
      CALL MSG_OUT( ' ', 'Last field centre has been read',
     :   STATUS )
      EOI = .TRUE.
      GO TO 899

*  Come here if an error was found reading the file.
  810 CONTINUE
      STATUS = SAI__ERROR
      CALL ERR_REP( 'FIELD_READ_ERR',
     :   'Error reading the field centres from a file', STATUS )
      EOI = .TRUE.
      GO TO 899

*  Come here if a null response was given to RA prompt.
  820 CONTINUE
      CALL MSG_OUT( 'NO_MORE_FIELD',
     :   'No further field centres.', STATUS )
      EOI = .TRUE.
      GO TO 899

*  This is the only exit point from this subroutine.
  899 CONTINUE

      END
