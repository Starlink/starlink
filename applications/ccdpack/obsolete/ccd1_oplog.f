      SUBROUTINE CCD1_OPLOG( STATUS )
*+
*  Name:
*     CCD1_OPLOG

*  Purpose:
*     Intialises the CCDPACK logging system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_OPLOG( STATUS )

*  Description:
*     The routine accesses the ADAM parameters LOGTO and LOGFILE.
*     LOGTO can take the values 'NEITHER", 'TERMINAL', LOGFILE', or
*     'BOTH'.  If the return is NONE then no log system initialisation
*     takes place and logging output isd not echoed. If 'LOGFILE' or
*     'BOTH' are chosen then a name for the logfile is requested
*     through parameter LOGFILE.  If logfile is then returned as ! then
*     the log file system is initialised to not echo any messages to
*     the log file. IF the return 'TERMINAL' is given then the logging
*     system is initialised to just write its output to the users
*     terminal.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1991 (PDRAPER):
*        Original version.
*     20-JUN-1991 (PDRAPER):
*        Changed to use ADAM logfiles.
*     15-JAN-1992 (PDRAPER):
*        Changed to use LOGTO parameter.
*     15-SEP-1993 (PDRAPER):
*        Changed AIF_ call to PAR_ call  to remove dynamic default
*        setting by AIF_ routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Global Variables:
      INCLUDE 'CCD1_CLOG'        ! The CCDPACK log file system common
                                 ! block.
*        CCD1_BUFF = CHARACTER * ( MSG__SZMSG ) (Write)
*           Character buffer for output strings.
*        CCD1_ILEV = INTEGER (Write)
*           Log system interaction level.
*           0 - no output from the CCDPACK logging system
*           1 - output to terminal only
*           2 - output to logfile only
*           3 - output to logfile and terminal

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 132 ) FILNAM ! Name of log file.
      CHARACTER * ( 80 ) MESS    ! Message string from log system.
      CHARACTER * ( 8 ) LOGTO    !
      LOGICAL INIT               ! Whether log file system has been
                                 ! initialised or not
      INTEGER NTRY               ! Number of attempts to access logfile

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the control flags.
      INIT = .FALSE.

*  Get the interaction level.
      CALL PAR_CHOIC( 'LOGTO', ' ', 'BOTH,LOGFILE,TERMINAL,NEITHER',
     :                .FALSE., LOGTO, STATUS )

*  Change the LOGTO value into the 'older' log system interaction level.
*  LOGINT must be in the range 0 to 3. The various levels have the
*  meanings
*  0 - no output from the CCDPACK logging system (NEITHER)
*  1 - output to terminal only                   (TERMINAL)
*  2 - output to logfile only                    (LOGFILE)
*  3 - output to logfile and terminal            (BOTH)

      IF ( LOGTO .EQ. 'NEITHER' ) THEN
         CCD1_ILEV = 0
      ELSE IF ( LOGTO .EQ. 'TERMINAL' ) THEN
         CCD1_ILEV = 1
      ELSE IF ( LOGTO .EQ. 'LOGFILE' ) THEN
         CCD1_ILEV = 2
      ELSE

*  Hard luck default.
         CCD1_ILEV = 3
      END IF

*  Set up an error mark.
      CALL ERR_MARK

*  If required find out the user requirements for the log file.
      IF ( CCD1_ILEV .GE. 2 ) THEN
         NTRY = 0
 1       CONTINUE
         NTRY = NTRY + 1

*  If required get a logfile and open it.
         CALL PAR_GET0C( 'LOGFILE', FILNAM, STATUS )

*  Check for valid status return SAI__OK.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the logging system and attempt to open the log file.
            CALL LOG_INIT_WRITE( 'CCDPACK', STATUS )
            INIT = .TRUE.

*  Try to open the log file.
            CALL LOG_OPEN_WRITE( FILNAM, MESS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( NTRY .LE. 5 ) THEN

*  Annul status.
                  CALL ERR_ANNUL( STATUS )

*  Cannot open the log file... issue message and reprompt.
                  CALL MSG_OUT( ' ', MESS, STATUS )
                  CALL MSG_OUT( ' ', '  CCD1_OPLOG: Cannot open log'//
     :                         'file - improper file type, directory'//
     :                         ' does not exist or access violation',
     :                          STATUS )
                  CALL PAR_CANCL( 'LOGFILE', STATUS )

*  Try again
                  GO TO 1
               ELSE

*  Give up -- issue error message
                  CALL ERR_REP( 'CCD1_OPLOG1',
     :            '  CCD1_OPLOG: Cannot open log file', STATUS )
               END IF

            END IF
         END IF

*  If we're here and status is set null ,but the log file system has
*  been initialised then make sure that it is closed down.
         IF ( STATUS .EQ. PAR__NULL ) THEN

*  Annul the status and remove any error messages.
            CALL ERR_ANNUL( STATUS )
            IF ( INIT ) THEN

*  Close down log system.
               CALL LOG_CLOSE_WRITE( INIT, STATUS )
            END IF

*  User has returned a ! character for the file name. For this to be a
*  valid return LOGINT should be set to 1, do so.
            CCD1_ILEV = 1

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
*  Other BAD status value indicate error with logfile, make sure that no
*  log output can be performed.
            CCD1_ILEV = 1
            IF ( INIT ) THEN
               CALL LOG_CLOSE_WRITE( INIT, STATUS )
            END IF
            GO TO 99
         END IF
      END IF

*  Release the error context
 99   CONTINUE
      CALL ERR_RLSE

      END
* $Id$
