      SUBROUTINE CHT_OSET( STATUS )
*+
*  Name:
*     CHT_OSET

*  Purpose:
*     Set output parameters

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_OSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program allows the user to set any or all of the "output"
*     parameters, i.e. those which define the size and scale of the
*     output chart and the device on which it is to be produced.

*  Usage:
*     OSET {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  Notes:
*     {routine_notes}...

*  External Routines Used:
*     CHR:
*        CHR_LEN, CHR_UCASE

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (University of Manchester and Starlink, RAL)
*     TNW: Tim Wilkins (Univeristy of Manchester)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1983 (KFH):
*        Original version.
*     10-NOV-1983 (KFH):
*        Modified.
*      8-DEC-1988 (PMA and TNW):
*        Convert to use GKS 7.2 instead of GKS 6.2.
*      9-DEC-1991: (PMA):
*        Changed calls to CTOR to CHR_CTOR.
*     10-DEC-1991: (PMA)
*        Changed from main program to subroutine to be an ADAM task.
*     29-JUN-1992 (PMA):
*        Finished conversion to A-task.
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_OSET.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     7-MAY-1993 (AJJB):
*        Removed bits where parameters COORDS and EXTRA are read in
*        which change the values entered to uppercase and strip file
*        type extension.
*     13-MAY-1993 (AJJB):
*        Put in an IF where the EXTRA parameter is read in to check if
*        it has been set to 'none', which is a standard value, and hance
*        wants converting to uppercase.
*     21-MAY-1993 (AJJB):
*        Check that the value of DEVICE is valid by calling GNS_TNG, and
*        I removed the bit which changes the value to uppercase, as
*        device names are case-sensitive.
*     7-JUN-1993 (AJJB):
*        Commented out unused local variables ZONEID, JSTAT, I, J, K, L
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! The length of a character string

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! [local_variable_description]
      CHARACTER * ( 50 ) EXTRA   ! [local_variable_description]
      CHARACTER * ( 50 ) TEMP    ! [local_variable_description]
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 50 ) DEVNAM  ! [local_variable_description]
      CHARACTER * ( 1 ) FIRST    ! [local_variable_description]
      LOGICAL FLAG               ! [local_variable_description]
      INTEGER NVAL               ! [local_variable_description]
      INTEGER NPOS               !
      INTEGER IEND               ! [local_variable_description]
*     INTEGER ZONEID             ! [local_variable_description]
*     INTEGER JSTAT              ! [local_variable_description]
      INTEGER WKID               ! Workstation identifier
      INTEGER CONID              ! Connection identifier
*     INTEGER I                  ! [local_variable_description]
*     INTEGER J                  ! [local_variable_description]
*     INTEGER K                  ! [local_variable_description]
*     INTEGER L                  ! [local_variable_description]
      REAL DATA                  ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current set of default parameters. This allows the user to
*  define the PLOT parameters.
      CALL GETPARAMS( PARAMS, NVAL, STATUS )

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',
     :   '             Set the OUTPUT parameters', STATUS )
      CALL MSG_BLANK( STATUS )

*  The same process is repeated (with minor variations) for each
*  parameter.

*  First pick up the current value for the plotting DEVICE.
      CALL GETDEFLT( PARAMS, NVAL, 'DEVICE', VALUE, NPOS, STATUS )

*  Only do anything else if a valid value was found.
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

  100    CONTINUE
         TEMP = VALUE

*  Ask for a new value.
         CALL PAR_DEF0C( 'DEVICE', VALUE, STATUS )
         CALL PAR_GET0C( 'DEVICE', VALUE, STATUS )
         CALL PAR_CANCL( 'DEVICE', STATUS )

*  Ignore it if a null response or an error are found (i.e. leave the
*  current value unchanged.)
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the Versatec or Printronix were selected there must be a sensible
*  connection identifier so that plots may submitted automatically.
            IF ( CHR_LEN( VALUE ) .EQ. 1 .AND.
     :         VALUE( 1:1 ) .EQ. '5' ) THEN
               VALUE( 1:3 ) = '5_9'
            ELSE IF ( CHR_LEN( VALUE ) .EQ. 1 .AND.
     :         VALUE( 1:1 ) .EQ. '7' ) THEN
               VALUE( 1:3 ) = '7_9'
            END IF

*  This is an attempt at verification.  First strip off "_A" if found,
            IEND = INDEX( VALUE, '_A' )
            DEVNAM = ' '
            IF ( IEND .GT. 1) THEN
               DEVNAM = VALUE( 1:IEND-1 )
            ELSE
               DEVNAM = VALUE
            END IF

            CALL GNS_TNG( DEVNAM, WKID, CONID, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               VALUE = TEMP
               GOTO 100
            ENDIF

            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

* If the DEVICE name had the qualifier "_A" then ask for an aspect
* ratio and scale factor.
      IF ( INDEX( VALUE, '_A' ) .NE. 0 ) THEN
         CALL GETDEFLT( PARAMS, NVAL, 'RATIO', VALUE, NPOS, STATUS )
         IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
  102       CONTINUE
            TEMP = VALUE
            CALL PAR_DEF0C( 'RATIO', VALUE, STATUS )
            CALL PAR_GET0C( 'RATIO', VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*  Validate entry.
               CALL CHR_UCASE( VALUE )
               CALL CHR_CTOR( VALUE, DATA, STATUS )
               IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
                  CALL ERR_REP( 'NEGVAL',
     :               'Entry must be positive and real', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( 'RATIO', STATUS )
                  VALUE = TEMP
                  GO TO 102
               END IF
               PARAMS( NPOS )( 21:70 ) = VALUE
            END IF
         END IF

*  Then the same thing for a scale factor.
         CALL GETDEFLT( PARAMS, NVAL, 'FACTOR', VALUE, NPOS, STATUS )
         IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
  104       CONTINUE
            TEMP = VALUE
            CALL PAR_DEF0C( 'FACTOR', VALUE, STATUS )
            CALL PAR_GET0C( 'FACTOR', VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*  Validate entry.
               CALL CHR_UCASE( VALUE )
               CALL CHR_CTOR( VALUE, DATA, STATUS )
               IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
                  CALL ERR_REP( 'NEGVAL',
     :               'Entry must be positive and real', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( 'FACTOR', STATUS )
                  VALUE = TEMP
                  GO TO 104
               END IF
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process for the size of the plotting area.
      CALL GETDEFLT( PARAMS, NVAL, 'PAREA', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
  110    CONTINUE
         TEMP = VALUE
         CALL PAR_DEF0C( 'PAREA', VALUE, STATUS )
         CALL PAR_GET0C( 'PAREA', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  Because the input should be a real number it is possible to convert
*  to a number and check for valid input, and also that the value is
*  within its prescribed range (in this case positive)
            CALL CHR_CTOR( VALUE, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
               CALL ERR_REP( 'NEGVAL',
     :            'Entry must be positive and real', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'PAREA', STATUS )
               VALUE = TEMP
               GO TO 110
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process for the plotting scale.
      CALL GETDEFLT( PARAMS, NVAL, 'SCALE', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
  120    CONTINUE
         TEMP = VALUE
         CALL PAR_DEF0C( 'SCALE', VALUE, STATUS )
         CALL PAR_GET0C( 'SCALE', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOR( VALUE, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
               CALL ERR_REP( 'NEGVAL',
     :            'Entry must be positive and real', STATUS )
               CALL ERR_FLUSH( STATUS )
               VALUE = TEMP
               CALL PAR_CANCL( 'SCALE', STATUS )
               GO TO 120
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process to sort out the SYMBOL type.
      CALL GETDEFLT( PARAMS, NVAL, 'SYMBOL', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         TEMP = VALUE
         CALL PAR_DEF0C( 'SYMBOL', VALUE, STATUS )
         CALL PAR_GET0C( 'SYMBOL', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )
            IF ( VALUE( 1:5 ) .EQ. 'CROSS' ) THEN
               VALUE = 'CROSS'
            ELSE IF ( VALUE( 1:4 ) .EQ. 'SPOT' ) THEN
               VALUE = 'SPOT'
            ELSE
               VALUE = 'CIRCLE'
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process to see if the Magnitude Scales are required, but
*  first find out if mode is NONS, because if it is then a magnitude
*  scale would be meaningless!
      CALL GETDEFLT( PARAMS, NVAL, 'MODE', VALUE, NPOS, STATUS )
      FLAG = VALUE( 1:1 ) .EQ. 'N'
      CALL GETDEFLT( PARAMS, NVAL, 'KEY', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. FLAG ) THEN
            TEMP = VALUE
            CALL PAR_DEF0C( 'KEY', VALUE, STATUS )
            CALL PAR_GET0C( 'KEY', VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL CHR_UCASE( VALUE )
               IF ( VALUE( 1:2 ) .EQ. 'NO' ) THEN
                  VALUE = 'NO'
               ELSE IF ( VALUE( 1:4 ) .EQ. 'NUMB' ) THEN
                  VALUE = 'NUMBERS'
               ELSE IF ( VALUE( 1:5 ) .EQ. 'SCALE' ) THEN
                  VALUE = 'SCALES'
               ELSE
                  VALUE = 'YES'
               END IF
            END IF
         ELSE
            VALUE = 'NUMBERS'
         END IF
         PARAMS( NPOS )( 21:70 ) = VALUE
      END IF

*  Repeat the process to see if there is going to be a GRID.
      CALL GETDEFLT( PARAMS, NVAL, 'GRID', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         TEMP = VALUE
         CALL PAR_DEF0C( 'GRID', VALUE, STATUS )
         CALL PAR_GET0C( 'GRID', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  Note that the valid responses are YES or NO or MINI with NO as the
*  fall-back position.
            IF ( VALUE( 1:1 ) .EQ. 'Y' ) THEN
               VALUE = 'YES'
            ELSE IF ( VALUE( 1:1 ) .EQ. 'M' ) THEN
               VALUE = 'MINI'
            ELSE
               VALUE = 'NO'
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process for an ERROR BOX.
      CALL GETDEFLT( PARAMS, NVAL, 'ERRBOX', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         TEMP = VALUE
         CALL PAR_DEF0C( 'ERRBOX', VALUE, STATUS )
         CALL PAR_GET0C( 'ERRBOX', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  In this case valid reponses start with C or P or Q, otherwise NONE
*  is stored.
            IF ( VALUE( 1:1 ) .EQ. 'C' ) THEN
               VALUE = 'CIRC'
            ELSE IF ( VALUE( 1:1 ) .EQ. 'P' ) THEN
               VALUE = 'PROMPT'
            ELSE IF ( VALUE( 1:1 ) .EQ. 'Q' ) THEN
               VALUE = 'QUAD'
            ELSE
               VALUE = 'NONE'
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF

*  The first character is stored and further parameters requested
*  depending on its value. If the result was NONE or PROMPT, then
*  nothing else is required.
         FIRST = VALUE( 1:1 )
         IF ( FIRST .EQ. 'C' ) THEN

*  If the response was CIRCULAR the ask for its radius.
            CALL GETDEFLT( PARAMS, NVAL, 'RADIUS', VALUE, NPOS, STATUS )
  150       CONTINUE
            TEMP = VALUE
            CALL PAR_DEF0C( 'RADIUS', VALUE, STATUS )
            CALL PAR_GET0C( 'RADIUS', VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL CHR_UCASE( VALUE )

*  The response should be a positive real number.
               CALL CHR_CTOR( VALUE, DATA, STATUS )
               IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
                  CALL ERR_REP( 'NEGVAL',
     :               'Entry must be positive and real', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( 'RADIUS', STATUS )
                  VALUE = TEMP
                  GO TO 150
               END IF
               PARAMS( NPOS )( 21:70 ) = VALUE
            END IF
         END IF
         IF ( FIRST .EQ. 'Q' ) THEN

*  On the other hand, a response of QUADRILATERAL means that a file
*  containing the co-ordinates of the vertices is required.
            CALL GETDEFLT( PARAMS, NVAL, 'COORDS', VALUE, NPOS, STATUS )
            TEMP = VALUE
            CALL PAR_DEF0C( 'COORDS', VALUE, STATUS )
            CALL PAR_GET0C( 'COORDS', VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               PARAMS( NPOS )( 21:70 ) = VALUE
            END IF
         END IF
      END IF

*  Read the parameter file and find the current default value.
      CALL GETDEFLT( PARAMS, NVAL, 'EXTRA', EXTRA, NPOS, STATUS )
      TEMP = EXTRA

*  Read a new value.
      CALL PAR_DEF0C( 'EXTRA', EXTRA, STATUS )
      CALL PAR_GET0C( 'EXTRA', EXTRA, STATUS )

*  If a valid response was given store it in the parameter array (after
*  checking if it was set to 'none', which is a standard value and so
*  wants converting to uppercase).
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( EXTRA(:4) .EQ. 'none' ) CALL CHR_UCASE( EXTRA )
         PARAMS( NPOS )( 21:70 ) = EXTRA
      END IF
      CALL PAR_CANCL( 'EXTRA', STATUS )

*  Now handle the direction of the plotting - responses are REVERSE or
*  NORMAL (the fall-back position)
      CALL GETDEFLT( PARAMS, NVAL, 'DIRECT', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         TEMP = VALUE
         CALL PAR_DEF0C( 'DIRECT', VALUE, STATUS )
         CALL PAR_GET0C( 'DIRECT', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )
            IF ( VALUE( 1:1 ) .EQ. 'R' ) THEN
               VALUE = 'REVERSE'
            ELSE
               VALUE = 'NORMAL'
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Now handle the question of a central cross.
      CALL GETDEFLT( PARAMS, NVAL, 'CROSS', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         TEMP = VALUE
         CALL PAR_DEF0C( 'CROSS', VALUE, STATUS )
         CALL PAR_GET0C( 'CROSS', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )
            IF ( VALUE( 1:1 ) .EQ. 'N' ) THEN
               VALUE = 'NO'
            ELSE
               VALUE = 'YES'
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  The new set of parameters may now be stored on disk.
      CALL PUTPARAMS( PARAMS, NVAL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'OSET_ERR',
     :   'OSET: Failed to set output parameters.',
     :   STATUS )
      END IF

      END
