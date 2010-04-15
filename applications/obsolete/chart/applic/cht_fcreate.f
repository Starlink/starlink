      SUBROUTINE CHT_FCREATE( STATUS )
*+
*  Name:
*     CHT_FCREATE

*  Purpose:
*     Create a set of field centres

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_FCREATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Allows the user to enter a set of field centres which are stored
*     in a disk file for subsequent use by the catalogue searching
*     programs.  It is equivalent to the BATCH mode in the earlier
*     version of CHART.

*  Usage:
*     FCREATE {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  External Routines Used:
*     {facility_or_package}...

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1983 (KFH):
*        Original version.
*      9-DEC-1991 (PMA):
*        Changed call to CTOR to CHR_CTOR
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_FCREATE.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     4-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statements, with calls to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the CARRIAGECONTROL specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.dat' to filenames.
*     26-APR-1993 (AJJB):
*        Took bits out which change filename which user enters to
*        uppercase and strips off extension.
*     26-APR-1993 (AJJB):
*        If the user has just entered a carriage return, exit from
*        the loop.
*     27-APR-1993 (AJJB):
*        Added calls to ERR_FLUSH at points where an invalid value has
*        been entered by user, so that STATUS gets restored to OK value
*        before prompting user again for value, as it was getting into
*        infinite loops.
*     12-MAY-1993 (AJJB):
*        Changed the bit that gets the filename from the user and opens
*        the file: it now prompts for the filename, and if the file
*        doesn't already exist it, it opens it with STATUS = 'NEW'. If
*        it does exist, it asks the user if he/she wants to add to the
*        existing file. If they answer yes, it OPENs it with STATUS =
*        'OLD', and if they answer no it loops back to the start for
*        another filename.
*     7-JUN-1993 (AJJB):
*        Commented out unused local variables REPLY and L
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
      CHARACTER * ( 80 ) TEXT    ! A title for the list
      CHARACTER * ( 80 ) EQUINOX !
      CHARACTER * ( 40 ) NAME    !
*     CHARACTER * ( 40 ) REPLY   !
      CHARACTER * ( 40 ) TEMP    !
      CHARACTER * ( 44 ) FILE    !
      DOUBLE PRECISION ANGLE     ! [local_variable_description]
      LOGICAL RESPONSE           ! [local_variable_description]
*     INTEGER L                  ! [local_variable_description]
      INTEGER J                  ! [local_variable_description]
      REAL DATE                  ! [local_variable_description]
      LOGICAL EXIST              ! Flags whether file exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   Set up the common block CONVF
C
      CALL CON_FACTOR( STATUS )
      RESPONSE=.TRUE.
      EQUINOX='1950.0'
      NAME='CENT'
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '        Create list of field centres',
     :      STATUS )
         CALL MSG_BLANK( STATUS)
C
C   First get the name of the file - default is CENT.DAT
C
  100 CONTINUE
      TEMP=NAME
      CALL PAR_DEF0C( 'FFILE', NAME, STATUS )
      CALL PAR_GET0C( 'FFILE', NAME, STATUS )
      CALL PAR_CANCL( 'FFILE', STATUS )
      FILE=NAME

*  Check whether file already exists

      INQUIRE( FILE = FILE, EXIST = EXIST )

      IF (.NOT. EXIST) THEN

* Open the file with STATUS = 'NEW'
*
* This call replaces this statement :
*
*     OPEN (UNIT=3,FILE=FILE,STATUS='NEW',
*    :      CARRIAGECONTROL='LIST')

         CALL FILEOPEN( 3, FILE, 'NEW', ' ', ' ', .TRUE., 0, .FALSE.,
     :   STATUS )
         IF (STATUS .NE. SAI__OK) GOTO 500

* Else if it does exist, ask use whether they want to add to this file

      ELSE

         RESPONSE = .TRUE.
         CALL MSG_OUT( ' ','File with this name already exists',STATUS )
         CALL MSG_OUT( ' ','Do you wish to add to this file?', STATUS )
         CALL PAR_GET0L( 'OK', RESPONSE, STATUS )
         CALL PAR_CANCL( 'OK', STATUS )

* If they do wish to add to it, OPEN it

         IF ( RESPONSE ) THEN

* This call replaces this statement :
*
*     OPEN (UNIT=3,FILE=FILE,STATUS='OLD',
*    :      CARRIAGECONTROL='LIST',ERR=500)
* (see History)

            CALL FILEOPEN( 3, FILE, 'OLD', ' ', ' ', .TRUE., 0, .FALSE.,
     :      STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 500

* and move the file pointer to the end of the file, ready to append

            DO J=1,1000000
               READ (3,'(A)',END=115) TEXT
            ENDDO
 115        CONTINUE

* Otherwise, loop round again for another filename

         ELSE

            GOTO 100

         ENDIF

      ENDIF

C
C      It loops around to here reading in turn:-
C             RA
C             DEC
C             EQUINOX
C             TITLE
C      until a null response is given to the RA prompt.
C
  200 CONTINUE
         CALL MSG_OUT( ' ', 'Enter RA, DEC in turn', STATUS )
         CALL MSG_OUT( ' ', 'A null response to RA terminates', STATUS )
  210    CONTINUE
            CALL PAR_GET0C( 'RA', RA, STATUS )
            CALL PAR_CANCL( 'RA', STATUS )
            CALL CHR_UCASE( RA )
C

* Check for a non-null response to RA and that all is OK before
* proceeding.
            IF ( RA .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
C
C            Convert the characters into a number, and return on
C            any of three error conditions.
C            (Suitable error messages are written by CONVRA)
C
               CALL CONVRA( RA, 1, 80, ANGLE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )
                  GO TO 210
               END IF
C
C            Repeat the process for the DEC
C
  220          CONTINUE
               CALL PAR_GET0C( 'DEC', DEC, STATUS )
               CALL PAR_CANCL( 'DEC', STATUS )
               CALL CHR_UCASE( DEC )
               CALL CONVDEC( DEC, 1, 80, ANGLE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )
                  GO TO 220
               END IF
C
C            Now pick up the equinox, with a deafult of 1950.0
C            the first time around. The previous value becomes the
C            the new default.
C
  230          CONTINUE
               TEMP=EQUINOX
               CALL PAR_DEF0C( 'EQUINOX', EQUINOX, STATUS )
               CALL PAR_GET0C( 'EQUINOX', EQUINOX, STATUS )
               CALL PAR_CANCL( 'EQUINOX', STATUS )
               CALL CHR_UCASE( EQUINOX )
C
C            Note that a response of TOD(AY) will pick up
C            today's date and write it in the correct format.
C
               IF (EQUINOX(1:3).EQ.'TOD') THEN
                  CALL TODAY(DATE, STATUS )
                  WRITE(EQUINOX,'(F10.5)') DATE
               END IF
C
C            Check for a valid REAL number
C
               CALL CHR_CTOR( EQUINOX, DATE, STATUS )
               IF ( STATUS .NE. SAI__OK .OR. DATE .LT. 0.0 ) THEN
                  CALL ERR_REP( 'INVALEQ', 'Invalid Equinox', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  EQUINOX=TEMP
                  GO TO 230
               END IF
C
C            Finally pick up a title (default is a blank record).
C
  240          CONTINUE
               TEXT=' '
               CALL PAR_DEF0C( 'TITLE', TEXT, STATUS )
               CALL PAR_GET0C( 'TITLE', TEXT, STATUS )
               CALL PAR_CANCL( 'TITLE', STATUS )
C
C            The user then has the option to accept these values
C
               RESPONSE = .TRUE.
               CALL PAR_DEF0L( 'OK', RESPONSE, STATUS )
               CALL PAR_GET0L( 'OK', RESPONSE, STATUS )
               CALL PAR_CANCL( 'OK', STATUS )
C
C            and if satisfactory they are written to the file.
C
               IF (RESPONSE) THEN
                  WRITE (3,'(A)') RA
                  WRITE (3,'(A)') DEC
                  WRITE (3,'(A)') EQUINOX
                  WRITE (3,'(A)') TEXT
                  CALL MSG_OUT( ' ', 'Entry written to disk', STATUS )
               END IF

*  All the parameters have been cancelled, but leave the values as
*  before so that, e.g. EQUINOX need only be entered once.
               RESPONSE=.TRUE.
C
C            and return for a new set.
C
               GO TO 210
C
C         This is the end of the long IF-block which is executed
C         only if a valid response was given to the RA prompt.
C
            END IF
C
C      Close the file to which the entries have been written.
C
      CLOSE (UNIT=3)

*  If an error occurred, then report a contextual message.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FCREATE_ERR',
     :   'FCREATE: Failed to create file of field centres.',
     :   STATUS )
      END IF

      END
