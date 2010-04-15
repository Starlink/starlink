      SUBROUTINE CHT_ECREATE( STATUS )
*+
*  Name:
*     CHT_ECREATE

*  Purpose:
*     Create a file of extra object to be plotted

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_ECREATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This allows the creation of a list of extra objects which may be
*     superimposed on a standard plot.  It is equivalent to the SUPP
*     option in the original CHART program.

*  Usage:
*     ECREATE {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  Notes:
*     {routine_notes}...

*  External Routines Used:
*     CHR:
*        CHR_CTOR, CHR_UCASE
*     [facility_or_package]...

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     KFH: Ken Hartley (RGO)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1983 (KFH):
*        Original version.
*      9-DEC-1991 (PMA):
*        Changed calls to CTOR to CHR_CTOR
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task
*     26-FEB-1993 (PMA):
*        Change name of routine to be CHT_ECREATE.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     19-MAR-1993 (AJJB):
*        Replaced OPEN statements, with calls to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the CARRIAGECONTROL specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.dat' to filenames.
*     26-APR-1993 (AJJB):
*        Took bits out which change filenames to uppercase and strip off
*        extension as they were a load of fetid dingo kidneys.
*     26-APR-1993 (AJJB):
*        Exit from the main loop by testing if RA is an empty string.
*     27-APR-1993 (AJJB):
*        Added calls to ERR_FLUSH at points where an invalid value has
*        been entered by user, so that STATUS gets restored to OK value
*        before prompting user again for value, as it was getting into
*        infinite loops.
*     12-MAY-1993 (AJJB):
*        Changed code which handles opening/setting up of the file so
*        that it now checks whether the filename specified by the user
*         exists or not, and if it
*        does'nt, it opens it and prompts for and writes the equinox
*        to it. If
*        it does exist the user is asked whether they want to add to it
*        or not. If they answer yes, the equinox is read and displayed,
*        and if they answer no it loops round again and prompts for
*        another filename.
*     7-JUN-1993 (AJJB):
*        Commented out unused local variables REPLY and L.
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
      CHARACTER * ( 80 ) TEXT    ! [local_variable_description]
      CHARACTER * ( 40 ) NAME    ! [local_variable_description]
      CHARACTER * ( 40 ) TEMP    ! [local_variable_description]
      CHARACTER * ( 44 ) FILE    ! [local_variable_description]
      CHARACTER * ( 40 ) EQUINOX ! [local_variable_description]
*     CHARACTER * ( 40 ) REPLY   ! [local_variable_description]
      DOUBLE PRECISION ANGLE     ! [local_variable_description]
      LOGICAL RESPONSE           ! [local_variable_description]
      INTEGER J                  !
*     INTEGER L                  !
      REAL DATE                  !
      LOGICAL EXIST              ! Flags whether file already exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   Set up the common block CONVF
C
      CALL CON_FACTOR( STATUS )
      RESPONSE = .TRUE.
      NAME = 'EXTRA'
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ',
     :      '        Create list of extra (supplementary) objects',
     :       STATUS )
         CALL MSG_BLANK( STATUS )
C
C   First get the name of the file in which they are to be stored
C   and open the file.
C
  100 CONTINUE
      TEMP=NAME
      CALL PAR_DEF0C( 'EFILE', NAME, STATUS )
      CALL PAR_GET0C( 'EFILE', NAME, STATUS )
      CALL PAR_CANCL( 'EFILE', STATUS )
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

* Read and validate the equinox

         EQUINOX = '1950.0'
  130    CONTINUE
         TEMP = EQUINOX
         CALL PAR_DEF0C( 'EQUINOX', EQUINOX, STATUS )
                              EQUINOX = '1950.0'
         CALL PAR_GET0C( 'EQUINOX', EQUINOX, STATUS )
         CALL PAR_CANCL( 'EQUINOX', STATUS )
         CALL CHR_UCASE( EQUINOX )
C
C            Note that a response of TOD( AY ) will pick up
C            today's date and write it in the correct format.
C
         IF ( EQUINOX( 1:3 ).EQ.'TOD' ) THEN
            CALL TODAY( DATE , STATUS )
            WRITE( EQUINOX,'( F10.5 )' ) DATE
         END IF
         CALL CHR_CTOR( EQUINOX, DATE, STATUS )
         IF ( STATUS .NE. SAI__OK .OR. DATE .LE. 0.0 ) THEN
            CALL ERR_REP( 'INVALEQ', 'Invalid entry for equinox',
     :         STATUS )
            CALL ERR_FLUSH( STATUS )
            EQUINOX = TEMP
            GO TO 130
         END IF
C
C            After passing all these checks, write it to the file.
C
         WRITE ( 3,'( A )' ) EQUINOX

* Else if the file does exist, ask use whether they want to add to it

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

* Read and display the equinox for this file

            READ ( 3,'( A )' ) EQUINOX
            CALL MSG_OUT ( ' ', 'Present Equinox  =  '//EQUINOX( 1:10 ),
     :         STATUS )

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
C   Loop around to here entering RA,DEC pairs.
C   Exit from the loop is by a null response to the RA prompt.
C
         CALL MSG_OUT( ' ', 'Enter RA,DEC in turn',STATUS )
         CALL MSG_OUT( ' ', 'A null response to RA terminates', STATUS )
  210    CONTINUE
            CALL PAR_GET0C( 'RA', RA, STATUS )
            CALL PAR_CANCL( 'RA', STATUS )
            CALL CHR_UCASE( RA )
C
C         This is the start of a long IF-block which allows
C         entry of values unless a null response was given to RA and
C         also includes a check that all is OK before proceeding.
C
            IF ( .NOT. (RA .EQ. ' ') .AND. STATUS .EQ. SAI__OK ) THEN

C            The entered character string is converted into a number
C            and checked for various forms of error.
C            ( Suitable error messages are written by CONVRA )
C
               CALL CONVRA( RA, 1, 80, ANGLE, STATUS )

*  If invalid entry, flush error messages / restore status and go back

               IF ( STATUS .NE. SAI__OK ) THEN
                 CALL ERR_FLUSH( STATUS )
                 GO TO 210
               ENDIF
C
C            The same process is repeated for the DEC.
C
  220          CONTINUE
               CALL PAR_GET0C( 'DEC', DEC, STATUS )
               CALL PAR_CANCL( 'DEC', STATUS )
               CALL CHR_UCASE( DEC )
               CALL CONVDEC( DEC, 1, 80, ANGLE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                 CALL ERR_FLUSH( STATUS )
                 GO TO 220
               ENDIF
C
C            The user is given the chance to reject the entry
C            at this stage.
C
               RESPONSE = .TRUE.
               CALL PAR_DEF0L( 'OK', RESPONSE, STATUS )
               CALL PAR_GET0L( 'OK', RESPONSE, STATUS )
               CALL PAR_CANCL( 'OK', STATUS )
               IF ( RESPONSE ) THEN
C
C               If accepted the values are written to the file.
C
                  WRITE ( 3,'( A )' ) RA
                  WRITE ( 3,'( A )' ) DEC
                  CALL MSG_OUT( ' ', 'Entry written to disk', STATUS )
               END IF
C
C            Return for more parameters.
C
               RESPONSE = .TRUE.
               GO TO 210
C
C            This is the end of the long IF-block ( null response
C            to RA prompt).
C
            END IF
C
C   Close the file to which the entries have been written
C
      CLOSE ( UNIT = 3 )

*  If an error occurred, then report a contextual message.

 500  CONTINUE

      IF (  STATUS .NE. SAI__OK  ) THEN
         CALL ERR_REP( 'ECREATE_ERR',
     :   'ECREATE: Failed to create list of extra objects.',
     :   STATUS )
      END IF

      END
