      SUBROUTINE ERR_TUNE( PARAM, VALUE, STATUS )
*+
*  Name:
*     ERR_TUNE

*  Purpose:
*     Set an ERR tuning parameter

*  Language:
*     Starlink Fortran 77

*  Description:
*     The value of the ERR tuning parameter is set appropriately, according
*     to the value given. ERR_TUNE may be called multiple times for the same
*     parameter. 
*
*     The given value can be overridden by setting an environment variable,
*     ERR_<PARAM> (where <PARAM> is the tuning parameter name in upper case),
*     at run time.
*
*     The routine will attempt to execute regardless of the given value of
*     STATUS. If the given value is not SAI__OK, then it is left unchanged,
*     even if the routine fails to complete. If the STATUS is SAI__OK on 
*     entry and the routine fails to complete, STATUS will be set and an
*     error report made.

*  Invocation:
*     CALL ERR_TUNE( PARAM, VALUE, STATUS )

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The tuning parameter to be set (case insensitive).
*     VALUE = INTEGER (Given)
*        The desired value (see Notes).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     1. The following values of PARAM may be used
*     
*        'SZOUT' Specifies a maximum line length to be used in the line wrapping
*            process. By default the message to be output is split into chunks
*            of no more than the maximum line length, and each chunk is written
*            on a new line. The split is made at word boundaries if possible.
*            The default maximum line length is 79 characters.
*
*            If VALUE is set to 0, no wrapping will occur. If it is set greater
*            than 6, it specifies the maximum output line length. Note that the
*            minimum VALUE is 7, to allow for exclamation marks and indentation.
*
*        'STREAM' Specifies whether or not ERR should treat its output 
*            unintelligently as a stream of characters.
*            If VALUE is set to 0 (the default) all non-printing characters are
*            replaced by blanks, and line wrapping occurs (subject to SZOUT). 
*            If VALUE is set to 1, no cleaning or line wrapping occurs.
*
*        'REVEAL' Allows the user to display all error messages cancelled
*            when ERR_ANNUL is called. This is a diagnostic tool which enables
*            the programmer to see all error reports, even those 'handled' 
*            by the program. If VALUE is set to 0 (the default) annulling 
*            occurs in the normal way. If VALUE is set to 1, the message 
*            will be displayed.
*
*        'ENVIRONMENT' This is not a true tuning parameter name but causes
*            the environment variables associated with all the true tuning 
*            parameters to be used if set.  If the environment variable is
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
*
*     4. With REVEAL, messages are displayed at the time of the ANNUL.
*        As REVEAL operates at the EMS level they are displayed with Fortran
*        WRITE statements so, depending upon the delivery mechanism for
*        normal messages, they may appear out of order.

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
*        Improve error messages
*     20-FEB-2001 (AJC):
*        EMS1_TUNE now EMS_TUNE
*     31-JUL-2008 (TIMJ):
*        Use accessor functions to update global state.
*        No longer call EMS_TUNE for STREAM or SZOUT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ERR_PAR'          ! MSG_ public constants
      INCLUDE 'ERR_ERR'          ! MSG_ error codes

*  Local Constants:
      INTEGER MAX_PARS           ! Number of possible tuning parameters
      PARAMETER ( MAX_PARS = 3 )

*  Arguments Given:
      CHARACTER*(*) PARAM
      INTEGER VALUE

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ISTAT                ! Internal status
      INTEGER NPARS                ! Number of parameters to be processed
      INTEGER I                    ! Loop index
      INTEGER EVALUE               ! Value from environment variable
      INTEGER UVALUE               ! Value actually used
      CHARACTER*20 UPARAM          ! PARAM in upper case
      CHARACTER*20 TRANS           ! Translation of the environment variable
      CHARACTER*20 PARNAMES(MAX_PARS)  ! Tuning parameter names
      LOGICAL ENV                  ! Whether PARAM is 'ENVIRONMENT'
      LOGICAL SET                  ! Whether value is to be set
      LOGICAL ENVVAL               ! Whether value came from env variable

      LOGICAL ERRRVL               ! New REVEAL mode
      LOGICAL ERRSTM               ! New STREAM mode
      INTEGER ERRWSZ               ! New SZOUT

*  Local Data:
      DATA PARNAMES/ 'SZOUT', 'STREAM', 'REVEAL' /
*.

*  Initialise internal status
      ISTAT = SAI__OK

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
      DOWHILE ( ( ISTAT .EQ. SAI__OK ) .AND. ( I .LE. NPARS ) )

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
         CALL PSX_GETENV( 'ERR_' // UPARAM, TRANS, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL EMS_ANNUL( ISTAT )
            UVALUE = VALUE
         ELSE
            CALL CHR_CTOI( TRANS, EVALUE, ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               ISTAT = ERR__BDENV
               CALL EMS_SETC( 'EV', 'ERR_' // UPARAM )
               CALL EMS_SETC( 'TRANS', TRANS )
               CALL EMS_REP( 'ERR_TUNE_BDENV',
     :           'ERR_TUNE: Failed to convert environment variable ' //
     :           '^EV (^TRANS) to integer', ISTAT )
            ELSE
               SET = .TRUE.
               ENVVAL = .TRUE.
               UVALUE = EVALUE
            END IF
         
         END IF
         CALL EMS_RLSE

         IF ( ( ISTAT .EQ. SAI__OK ) .AND. SET ) THEN
*        Check that the given parameter name is acceptable
*        and handle it.
            IF ( UPARAM .EQ. 'SZOUT' ) THEN
               IF ( UVALUE .EQ. 0 ) THEN
                  ERRWSZ = ERR__SZMSG
                  CALL ERR1_PTWSZ( ERRWSZ )
               ELSE IF ( UVALUE .GT. 6 ) THEN
                  ERRWSZ = MIN( UVALUE, ERR__SZMSG )
                  CALL ERR1_PTWSZ( ERRWSZ )
               ELSE
                  ISTAT = ERR__BTUNE
               END IF


            ELSE IF ( UPARAM .EQ. 'STREAM' ) THEN
               IF ( UVALUE .EQ. 0 ) THEN
                  ERRSTM = .FALSE.
                  CALL ERR1_PTSTM( ERRSTM )
               ELSE IF ( UVALUE .EQ. 1 ) THEN
                  ERRSTM = .TRUE.
                  CALL ERR1_PTSTM( ERRSTM )
               ELSE
                  ISTAT = ERR__BTUNE
               END IF

            ELSE IF ( UPARAM .EQ. 'REVEAL' ) THEN
               IF ( UVALUE .EQ. 0 ) THEN
                  ERRRVL = .FALSE.
                  CALL EMS_TUNE( 'REVEAL', UVALUE, ISTAT )
                  CALL ERR1_PTRVL( ERRRVL )
               ELSE IF ( UVALUE .EQ. 1 ) THEN
                  ERRRVL = .TRUE.
                  CALL EMS_TUNE( 'REVEAL', UVALUE, ISTAT )
                  CALL ERR1_PTRVL( ERRRVL )
               ELSE
                  ISTAT = ERR__BTUNE
               END IF

            ELSE
*           The given tuning parameter was not in the available set.
*           Set status and report an error message.
*           We  mark and rlse to prevent possible token name clash
               CALL EMS_MARK
               ISTAT = ERR__BDPAR
               CALL EMS_SETC( 'PARAM', PARAM )
               CALL EMS_REP( 'ERR_TUNE_PAR',
     :           'ERR_TUNE: Invalid tuning parameter: ^PARAM', 
     :           ISTAT )
               CALL EMS_RLSE

            END IF

            IF ( ISTAT .EQ. ERR__BTUNE ) THEN
*           The given tuning parameter value was invalid
*           Report an error message
*           We  mark and rlse to prevent posible token name clash
               CALL EMS_MARK
               CALL EMS_SETC( 'PARAM', UPARAM )
               CALL EMS_SETI( 'VALUE', UVALUE )
               IF ( ENVVAL ) THEN
                 CALL EMS_SETC( 'SOURCE', 'from environment variable' )
               ELSE
                 CALL EMS_SETC( 'SOURCE', ' ' )
               END IF
               CALL EMS_REP( 'ERR_TUNE_INV',
     :         'ERR_TUNE: ^PARAM invalid value ^VALUE ^SOURCE', ISTAT )
               CALL EMS_RLSE
            END IF

         END IF

      END DO

*  Set return status
      IF ( STATUS .EQ. SAI__OK ) STATUS = ISTAT

      END
