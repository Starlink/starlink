      SUBROUTINE MSG_TUNE( PARAM, VALUE, STATUS )
*+
*  Name:
*     MSG_TUNE

*  Purpose:
*     Set an MSG tuning parameter

*  Language:
*     Starlink Fortran 77

*  Description:
*     The value of the MSG tuning parameter is set appropriately, according
*     to the value given. MSG_TUNE may be called multiple times for the same
*     parameter.
*
*     The given value can be overridden by setting an environment variable,
*     MSG_PARAM (where PARAM is the tuning parameter name in upper case),
*     at run time.

*  Invocation:
*     CALL MSG_TUNE( PARAM, VALUE, STATUS )

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The tuning parameter to be set (case insensitive).
*     VALUE = INTEGER (Given)
*        The desired value (see Notes).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     1. The following values of PARAM may be used:
*
*        'FILTER' Specifies the required MSG conditional message reporting
*            level. VALUE may be 1, 2, 3 or 4, corresponding with quiet, normal
*            (the default), verbose and debug levels respectively.
*
*        'SZOUT' Specifies a maximum line length to be used in the line wrapping
*            process. By default the message output by MSG is split into chunks 
*            of no more than the maximum line length, and each chunk is written
*            on a new line. The split is made at word boundaries if possible.
*            The default maximum line length is 79 characters.
*
*            If VALUE is set to 0, no wrapping will occur. If it is set greater
*            than 0, it specifies the maximum output line length.
*
*        'STREAM' Specifies whether or not MSG should treat its output 
*            unintelligently as a stream of characters.
*            If VALUE is set to 0 (the default) all non-printing characters are
*            replaced by blanks, and line wrapping occurs (subject to SZOUT). 
*            If VALUE is set to 1, no cleaning or line wrapping occurs.
*
*        'ENVIRONMENT' This is not a true tuning parameter name but causes
*            the environment variables associated with all the true tuning 
*            parameters to be used if set. If the environment variable is 
*            not set, the tuning parameter is not altered. The VALUE argument
*            is not used.
*
*     2. The tuning parameters for MSG and ERR operate partially at the EMS
*        level and may conflict in their requirements of EMS.
*
*     3. The use of SZOUT and STREAM may be affected by the message delivery
*        system in use. For example there may be a limit on the the size of a
*        line output by a Fortran WRITE and automatic line wrapping may occur.
*        In particular, a NULL character will terminate a message delivered by
*        the ADAM message system.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUL-1999 (AJC):
*        Original version.
*      3-SEP-1999 (AJC):
*        Added 'ENVIRONMENT' parameter
*     22-SEP-1999 (AJC):
*        Added FILTER parameter
*        Improve error messages
*     20-FEB-2001 (AJC):
*        EMS1_TUNE renamed EMS_TUNE
*     02-MAY-2008 (TIMJ):
*        Add MSG__DEBUG level.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'MSG_ERR'          ! MSG_ error codes

*  Local Constants:
      INTEGER MAX_PARS           ! Number of possible tuning parameters
      PARAMETER ( MAX_PARS = 3 )

*  Global Variables:
      INCLUDE 'MSG_CMN'          ! MSG_ output filter level

*  Arguments Given:
      CHARACTER*(*) PARAM
      INTEGER VALUE

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NPARS                ! Number of parameters to be processed
      INTEGER I                    ! Loop index
      INTEGER EVALUE               ! Value from environment variable
      INTEGER UVALUE               ! Value actually used
      INTEGER LEVEL                ! The required reporting level
      INTEGER LEVELS(4)            ! The possible reporting levels
      CHARACTER*20 UPARAM          ! PARAM in upper case
      CHARACTER*20 TRANS           ! Translation of the environment variable
      CHARACTER*20 PARNAMES(MAX_PARS)  ! Tuning parameter names
      LOGICAL ENV                  ! Whether PARAM is 'ENVIRONMENT'
      LOGICAL SET                  ! Whether value is to be set
      LOGICAL ENVVAL               ! Whether value came from env variable

*  Local Data:
      DATA LEVELS/ MSG__QUIET, MSG__NORM, MSG__VERB, MSG__DEBUG /
      DATA PARNAMES/ 'SZOUT', 'STREAM', 'FILTER' /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Force parameter name to upper case
      UPARAM = PARAM
      CALL CHR_UCASE( UPARAM )

*  Check for 'ENVIRONMENT'
      IF ( UPARAM .EQ. 'ENVIRONMENT' ) THEN
         ENV = .TRUE.
         NPARS = MAX_PARS
      ELSE
         ENV = .FALSE.
         NPARS = 1
      END IF

*  Now for each required parameter
      I = 1
      DOWHILE ( ( STATUS .EQ. SAI__OK ) .AND. ( I .LE. NPARS ) )

         IF ( ENV ) THEN
            UPARAM = PARNAMES( I )
            SET = .FALSE.
         ELSE
            SET = .TRUE.
         END IF
         I = I + 1

*     See if the associated environment variable is set
*     If so, override the given VALUE
         ENVVAL = .FALSE.
         CALL EMS_MARK
         CALL PSX_GETENV( 'MSG_' // UPARAM, TRANS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL EMS_ANNUL( STATUS )
            UVALUE = VALUE

         ELSE
            CALL CHR_CTOI( TRANS, EVALUE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = MSG__BDENV
               CALL EMS_SETC( 'EV', 'MSG_' // UPARAM )
               CALL EMS_SETC( 'TRANS', TRANS )
               CALL EMS_REP( 'MSG_TUNE_BDENV',
     :           'MSG_TUNE: Failed to convert environment variable ' //
     :           '^EV (^TRANS) to integer', STATUS )
            ELSE
               SET = .TRUE.
               ENVVAL = .TRUE.
               UVALUE = EVALUE
            END IF
         
         END IF
         CALL EMS_RLSE

         IF ( ( STATUS .EQ. SAI__OK ) .AND. SET ) THEN
*        Check that the given parameter name is acceptable
*        and handle it.
            IF ( UPARAM .EQ. 'SZOUT' ) THEN
               IF ( UVALUE .EQ. 0 ) THEN
                  MSGWSZ = MSG__SZMSG
                  CALL EMS_TUNE( 'SZOUT', MSGWSZ, STATUS )
               ELSE IF ( UVALUE .GT. 0 ) THEN
                  MSGWSZ = MIN( UVALUE, MSG__SZMSG )
                  CALL EMS_TUNE( 'SZOUT', MSGWSZ, STATUS )
               ELSE
                  STATUS = MSG__BTUNE

               END IF

            ELSE IF ( UPARAM .EQ. 'STREAM' ) THEN
               IF ( UVALUE .EQ. 0 ) THEN
                  MSGSTM = .FALSE.
                  CALL EMS_TUNE( 'STREAM', UVALUE, STATUS )
               ELSE IF ( UVALUE .EQ. 1 ) THEN
                  MSGSTM = .TRUE.
                  CALL EMS_TUNE( 'STREAM', UVALUE, STATUS )
               ELSE
                  STATUS = MSG__BTUNE
               END IF

            ELSE IF ( UPARAM .EQ. 'FILTER' ) THEN
               IF ( ( UVALUE .GT. 0 ) .AND. ( UVALUE .LT. 5 ) ) THEN
                  LEVEL = LEVELS( UVALUE )
                  CALL MSG_IFSET( LEVEL, STATUS )
               ELSE
                  STATUS = MSG__BTUNE
               END IF

            ELSE
*           The given tuning parameter was not in the available set.
*           Set status and report an error message.
*           We mark and rlse to prevent possible token name clash
               CALL EMS_MARK
               STATUS = MSG__BDPAR
               CALL EMS_SETC( 'PARAM', PARAM )
               CALL EMS_REP( 'MSG_TUNE_PAR',
     :         'MSG_TUNE: Invalid tuning parameter: ^PARAM', 
     :         STATUS )
               CALL EMS_RLSE

            END IF

            IF ( STATUS .EQ. MSG__BTUNE ) THEN
*           The given tuning parameter value was invalid
*           Report message
*           We mark and rlse to prevent possible token name clash
               CALL EMS_MARK
               CALL EMS_SETC( 'PARAM', UPARAM )
               CALL EMS_SETI( 'VALUE', UVALUE )
               IF ( ENVVAL ) THEN
                 CALL EMS_SETC( 'SOURCE', 'from environment variable' )
               ELSE
                 CALL EMS_SETC( 'SOURCE', ' ' )
               END IF
               CALL EMS_REP( 'MSG_TUNE_INV',
     :         'MSG_TUNE: ^PARAM invalid value ^VALUE ^SOURCE', STATUS )
               CALL EMS_RLSE
            END IF

         END IF

      END DO

      END
