      SUBROUTINE SUBPAR_PWHLP ( TOPIC, LIBRARY, FLAG, STATUS)
*+
*  Name:
*     SUBPAR_PWHLP

*  Purpose:
*     Output help.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PWHLP (TOPIC, LIBRARY, FLAG, STATUS)

*  Description:
*     Outputs help information to the terminal using the portable help
*     system

*  Arguments:
*     TOPIC  =CHARACTER*(*) (given)
*        the topic within the library on which help is sought
*     LIBRARY=CHARACTER*(*) (given)
*        the help library to be accessed. which
*        must be as described in SUN 124.3.
*     FLAG   =INTEGER       (given)
*        non-zero if help library search is required
*     STATUS =INTEGER       (given and returned)
*        status

*  Algorithm:
*     Use system-dependent routine SUBPAR_SCRNSZ to obtain the
*     width and height of the screen or window being used.
*     Call HLP_OUTHLP specifying routine SUBPAR_OPUT and SUBPAR_IPUT
*     to handle line output and topic prompts respectively. Use the
*     FLAG argument to determine if it is required to stay in the help
*     system or return immediately.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield  (STARLINK)
*     KFH: K F Hartley (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03-JUL-1990 (AJC):
*        Original - mod of UFACE_WRHELP
*     16-JUL-1991 (AJC):
*        Use SAI__OK not ADAM__OK
*     16-JUL-1991 (AJC):
*        CONVERT TO DUMMY
*     03-SEP-1991 (KFH):
*        Re-written as portable version
*     28-APR-1992 (AJC):
*        Added error handling etc.
*     14-MAY-1992 (AJC):
*        Add STATUS to SCRNSZ
*     11-JUN-1992 (AJC):
*        Obtain unused unit number
*     19-AUG-1992 (AJC):
*        Change to use HLP_HELP
*     21-OCT-1992 (AJC):
*        Initialize line counter properly
*      1-MAR-1993 (AJC):
*        Revise to standard prologue
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-FEB-1994 (AJC):
*        Report library name if error
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      CHARACTER*(*)  TOPIC,LIBRARY
      INTEGER FLAG

*  Status:
      INTEGER        STATUS

*  External References:
      INTEGER        SUBPAR_IPUT, SUBPAR_OPUT
      EXTERNAL       SUBPAR_IPUT, SUBPAR_OPUT
      INTEGER        SUBPAR_NAMETR
      EXTERNAL       SUBPAR_NAMETR
      INTEGER        HLP_HELP
      EXTERNAL       HLP_HELP

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER        FLAGS          ! Flags for HELP display routine
      INTEGER        SCREEN_WIDTH   ! Screen width
      INTEGER        ISTAT          ! Local status
      INTEGER        LUIN           ! Logical unit for reading library
      LOGICAL        OPEN           ! Whether LUIN already open
      CHARACTER*50   MES            ! Help system error message

      IF (STATUS .NE. SAI__OK) RETURN

*   Obtain an unused Fortran unit number - start looking arbitrarily at 10
      DO 10 LUIN = 10, 99
         INQUIRE ( UNIT=LUIN, OPENED=OPEN )
*      If Unit LUIN is available - proceed
         IF ( .NOT. OPEN ) THEN

*         Set help flag
            IF ( FLAG.EQ.0 ) THEN
               FLAGS = 0
            ELSE
               FLAGS = 1
            ENDIF

*         Get width and height of screen
*         (the routine called is system dependent)
            CALL SUBPAR_SCRNSZ( SCREEN_WIDTH, SUBPARPGSZ, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
*           Initialize the line count
               SUBPARLCNT = SUBPARPGSZ
*           and output help
               ISTAT = HLP_HELP( SUBPAR_OPUT, SCREEN_WIDTH, TOPIC,
     :                        LUIN, LIBRARY, FLAGS, SUBPAR_IPUT,
     :                        SUBPAR_NAMETR )

*           Set STATUS
               IF (ISTAT .EQ. 1) THEN
                  STATUS = SAI__OK
               ELSE
                  STATUS = SUBPAR__HLPER
                  CALL EMS_REP( 'SUP_PWHLP1',
     :            'SUBPAR: Failure on obtaining multi-line help',
     :             STATUS )
                  CALL EMS_SETC( 'LIB', LIBRARY )
                  CALL EMS_SETC( 'TOPIC', TOPIC )
                  CALL EMS_REP( 'SUP_PWHLP2',
     :            'from help file: ^LIB, Topic: ^TOPIC',
     :            STATUS )
                  CALL HLP_ERRMES( ISTAT, MES )
                  CALL EMS_SETC( 'HLPMES', MES )
                  CALL EMS_REP( 'SUP_PWHLP3', '^HLPMES',
     :             STATUS )
               END IF

            END IF

*         Exit loop
            GOTO 100

         END IF

10    CONTINUE

*   If drops out of loop,
*   all unit numbers already in use - report
      STATUS = SUBPAR__HLPER
      CALL EMS_REP( 'SUP_PWHLP4',
     :'SUBPAR: No Fortran unit number available for the help system',
     : STATUS )

100   CONTINUE

      END
