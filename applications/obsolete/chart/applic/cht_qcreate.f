      SUBROUTINE CHT_QCREATE( STATUS )
*+
*  Name:
*     CHT_QCREATE

*  Purpose:
*     Create quadrilateral error box

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_QCREATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This allows definition of the 4 vertices of a quadrilateral error
*     box, which may be superimposed on a standard plot.
*     It is equivalent to the ERR option in the original CHART program.

*  Usage:
*     QCREATE {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1983 (KFH):
*        Original version.
*      9-DEC-1991: PMA
*        Changed calls to CTOR to CHR_CTOR
*     10-DEC-1991: (PMA)
*        Changed from main program to subroutine to be an ADAM task
*     17-JUN-1992 (PMA):
*        Convert INTERIM routines to ADAM calls.
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_QCREATE.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     4-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statement, with call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the CARRIAGECONTROL specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.DAT' to filenames.
*     27-APR-1993 (AJJB):
*        Added calls to ERR_FLUSH at points where an invalid value has
*        been entered by user, so that STATUS gets restored to OK value
*        before prompting user again for value, as it was getting into
*        infinite loops.
*     11-MAY-1993 (AJJB):
*        When user enters a filename, it now checks whether the file
*        already exists, and if so, it loops round and prompts again for
*        another filename.
*     7-JUN-1993 (AJJB):
*        Commented out unused local variable "L"
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

*  Local Variables:
      CHARACTER * ( 80 ) RA      ! Right Ascension
      CHARACTER * ( 80 ) DEC     ! Declination
      CHARACTER * ( 40 ) NAME    !
      CHARACTER * ( 40 ) TEMP    !
      CHARACTER * ( 44 ) FILE    !
      CHARACTER * ( 40 ) EQUINOX !
      DOUBLE PRECISION ANGLE     !
      LOGICAL RESPONSE           !
*     INTEGER L                  ! [local_variable_description]
      REAL DATE                  ! [local_variable_description]
      INTEGER ICO                ! [local_variable_description]
      LOGICAL EXIST              ! Flag for whether file exists or not
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   Set up the common block CONVF
C
      CALL CON_FACTOR( STATUS )
      RESPONSE=.TRUE.
      NAME='VERT'
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ',
     :      '         Create the vertices of an error box', STATUS )
         CALL MSG_BLANK( STATUS )
C
C   First get the name of the file in which they are to be stored
C   and open the file.
C
  100 CONTINUE
      TEMP=NAME
      CALL PAR_DEF0C( 'QFILE', NAME, STATUS )
      CALL PAR_GET0C( 'QFILE', NAME, STATUS )
      CALL PAR_CANCL( 'QFILE', STATUS )
      FILE=NAME

* Check whether file already exists - if so loop round for new filename

      INQUIRE( FILE = FILE, EXIST = EXIST )
      IF ( EXIST ) THEN
         CALL MSG_OUT( ' ', 'This file already exists - please enter'//
     :                      ' a new filename', STATUS )
       GOTO 100           ! Loop round for new filename
      ENDIF

* This call replaces the statement :
*      OPEN (UNIT=3,FILE=FILE,STATUS='NEW',
*     :      CARRIAGECONTROL='LIST')
*
      CALL FILEOPEN(3, FILE, 'NEW', ' ', ' ', .TRUE., 0, .FALSE.,
     :     STATUS )

      IF (STATUS .NE. SAI__OK) GOTO 900
C
C      Now read a single equinox for ALL the following positions
C
               EQUINOX='1950.0'
  130          CONTINUE
               TEMP=EQUINOX
               CALL PAR_DEF0C( 'EQUINOX', EQUINOX, STATUS )
               CALL PAR_GET0C( 'EQUINOX', EQUINOX, STATUS )
               CALL CHR_UCASE( EQUINOX )
C
C            Note that a response of TOD(AY) will pick up
C            today's date and write it in the correct format.
C
               IF (EQUINOX(1:3).EQ.'TOD') THEN
                  CALL TODAY(DATE, STATUS )
                  WRITE(EQUINOX,'(F10.5)') DATE
               END IF
               CALL CHR_CTOR( EQUINOX, DATE, STATUS )
               IF ( STATUS .NE. SAI__OK .OR. DATE .LE. 0.0 ) THEN
                  CALL ERR_REP( 'INVALEQ', 'Invalid equinox entered',
     :               STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( 'EQUINOX', STATUS )
                  EQUINOX=TEMP
                  GO TO 130
               END IF
C
C            After all this checking, write the entry to the file.
C
               WRITE (3,'(A)') EQUINOX
C
C   Loop around to here entering RA,DEC pairs.
C   Exit from the loop after 4 valid entries.
C
  200 CONTINUE
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', 'Enter RA,DEC in turn', STATUS )
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', 'N.B. Points will be plotted in the',
     :      STATUS )
         CALL MSG_OUT( ' ', 'order in which they are entered.',
     :      STATUS )
         CALL MSG_OUT( ' ', 'There are four points to be entered.',
     :      STATUS )
         CALL MSG_BLANK( STATUS )
      DO ICO=1,4
  210    CONTINUE
            RA=' '
            CALL PAR_GET0C( 'RA', RA, STATUS )
            CALL CHR_UCASE( RA )
C
C            The entered character string is converted into a number
C            and checked for various forms of error.
C
               CALL CONVRA( RA, 1, 80, ANGLE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PAR_CANCL( 'RA', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  GO TO 210
               END IF
C
C            The same process is repeated for the DEC.
C
  220          CONTINUE
               DEC=' '
               CALL PAR_GET0C( 'DEC', DEC, STATUS )
               CALL CHR_UCASE( DEC )
               CALL CONVDEC( DEC, 1, 80, ANGLE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PAR_CANCL( 'DEC', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  GO TO 220
               END IF
C
C            The user is given the chance to reject the entry
C            at this stage.
C
               RESPONSE = .TRUE.
               CALL PAR_DEF0L( 'OK', RESPONSE, STATUS )
               CALL PAR_GET0L( 'OK', RESPONSE, STATUS )
               IF (RESPONSE) THEN
C
C               If accepted the values are written to the file.
C
                  WRITE (3,'(A)') RA
                  WRITE (3,'(A)') DEC
                  CALL MSG_OUT( ' ', 'Entry written to disk', STATUS )
               END IF
C
C            Cancel existing parameters and return for more.
C
               CALL PAR_CANCL( 'RA', STATUS )
               CALL PAR_CANCL( 'DEC', STATUS )
               CALL PAR_CANCL( 'OK', STATUS )
C
C           This rather poor logic says that if the entry was NOT
C           satisafactory, and so not written to disk, then go
C           back to the start of the loop without updating ICO
C           and hence counting this entry as one of the four needed.
C           (The default for th eresponse to OK is also set back
C            to true - ever optimistic.)
C
               IF (.NOT.RESPONSE) THEN
                   RESPONSE=.TRUE.
                   GO TO 210
               END IF
            END DO
C
C   Close the file to which the entries have been written
C
      CLOSE (UNIT=3)

 900  CONTINUE

*  Cancel the remaining parameters.
      CALL PAR_CANCL( 'QFILE', STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'QCREATE_ERR',
     :   'QCREATE: Failed to define error box.',
     :   STATUS )
      END IF

      END
