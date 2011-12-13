      SUBROUTINE GRP1_GTFIL( PARAM, MODE, NULL, UNIT, OPEN, STATUS )
*+
*  Name:
*     GRP1_GTFIL

*  Purpose:
*     Open a text file specified by a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GTFIL( PARAM, MODE, NULL, UNIT, OPEN, STATUS )

*  Description:
*     This routine opens a formatted file specified by the environment.
*     If MODE is 'WRITE', the file is first created. On VMS systems
*     it is opened with CARRIAGECONTROL='LIST', on UNIX systems the
*     default carriage control features are used.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name.
*     MODE = CHARACTER * ( * ) (Given)
*        Access mode; 'READ', 'WRITE' or 'UPDATE'. Note, READ and UPDATE
*        are equivalent; no write protection is applied to the file if
*        READ is specified.
*     NULL = LOGICAL (Given)
*        If true then a null value is not interpreted as an error,
*        but causes OPEN to be returned false.
*     UNIT = INTEGER (Returned)
*        The Fortran IO unit number on which the file has been opened.
*     OPEN = LOGICAL (Returned)
*        True if a file has been successfully opened.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error values.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP errors.

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER MODE*(*)
      LOGICAL NULL

*  Arguments Returned:
      INTEGER UNIT
      LOGICAL OPEN

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXLOOP             ! Max. no. of re-tries.
      PARAMETER ( MXLOOP = 4 )

*  Local Variables:
      CHARACTER FILENM*(GRP__SZFNM)! File name.
      CHARACTER FSTAT*3          ! File status.
      INTEGER IOS                ! Fortran IO status value.
      INTEGER IPAR               ! SUBPAR parameter index.
      INTEGER LOOP               ! No. of re-tries.
      LOGICAL LOOPAG             ! True if a re-try is required.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that error reports are deferred.
      CALL ERR_MARK

*  Store the file status.
      IF( MODE( : 5 ) .EQ. 'WRITE' ) THEN
         FSTAT = 'NEW'

      ELSE IF( MODE( : 4 ) .EQ. 'READ' .OR.
     :         MODE( : 6 ) .EQ. 'UPDATE' ) THEN
         FSTAT = 'OLD'

      ELSE
         STATUS = GRP__INTER
         CALL MSG_SETC( 'M', MODE )
         CALL ERR_REP( 'GRP1_GTFIL_ERR1',
     :   'GRP1_GTFIL: Internal GRP error; illegal file access mode '//
     :   'specified; ^M', STATUS )
         GO TO 999

      END IF

*  Get a free unit number.
      CALL GRP1_LUNIT( UNIT, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise things.
      LOOP = 0
      LOOPAG = .TRUE.
      OPEN = .FALSE.

*  Loop until a file has obtained, or the maximum number of attempts
*  have been made.
      DO WHILE ( LOOPAG )

*  Get a character string from the environment.
         CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
         CALL SUBPAR_GETNAME( IPAR, FILENM, STATUS )

*  If succesful, try to open the file.
         IF( STATUS .EQ. SAI__OK ) THEN
            OPEN( UNIT, FILE = FILENM( : CHR_LEN( FILENM ) ),
     :            STATUS = FSTAT, IOSTAT=IOS )

*  If an IO error was detected, report an error.
            IF( IOS. NE. 0 ) THEN
               STATUS = GRP__FIOER
               CALL MSG_SETI( 'UNIT', UNIT )
               CALL MSG_SETC( 'F', FILENM )
               CALL ERR_FIOER( 'TEXT', IOS )
               CALL ERR_REP( 'GRP1_GTFIL_ERR2',
     :'GRP1_GTFIL: Error opening file ^F on Fortran unit ^UNIT - '//
     :'"^TEXT"', STATUS )

*  Otherwise, indicate that the file is open.
            ELSE
               OPEN = .TRUE.

            END IF

*  If an abort was requested or a null value given, quit immediately.
         ELSE IF ( STATUS .EQ. PAR__ABORT .OR.
     :             STATUS .EQ. PAR__NULL ) THEN
            GO TO 999

         END IF

*  If an error occurred attempting to open the file, flush the error
*  and try again, so long as the maximum number of re-tries has not been
*  reached.
         IF ( STATUS .NE. SAI__OK ) THEN

            LOOP = LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN

               CALL ERR_REP( 'GRP1_GTFIL_ERR3',
     :           'Could not open text file - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )

            ELSE
               LOOPAG = .FALSE.

            END IF

            CALL PAR_CANCL( PARAM, STATUS )

*  If no error occurred, exit loop.
         ELSE
            LOOPAG = .FALSE.

         END IF

      END DO

*  Jump to here if an error occurred.
 999  CONTINUE

*  If a null parameter value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If a parameter null value is not acceptable, re-report the error with
*  a more friendly report.
         IF( .NOT. NULL ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'GRP1_GTFIL_ERR4',
     :  'GRP1_GTFIL: Null file name given for the %^P parameter.',
     :                    STATUS )
         END IF

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'GRP1_GTFIL_ERR5',
     :    'GRP1_GTFIL: Aborted attempt to associate a file name with '//
     :    'the %^P parameter.', STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'GRP1_GTFIL_ERR6',
     :    'GRP1_GTFIL: Unable to open a text file using parameter %^P.',
     :                 STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
