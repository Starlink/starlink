      SUBROUTINE UFACE_PWHLP ( TOPIC, LIBRARY, FLAG, STATUS)
*+
*  Name:
*     UFACE_PWHLP

*  Purpose:
*     Output help

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL UFACE_PWHLP (TOPIC, LIBRARY, FLAG, STATUS)

*  Description:
*     Outputs help information to the terminal using the portable help
*     system

*  Arguments:
*     LIBRARY=CHARACTER*(*) (given)
*            the help library to be accessed. which
*            must be as described in SUN 124.3.
*     TOPIC  =CHARACTER*(*) (given)
*            the topic within the library on which help is sought
*     FLAG   =INTEGER       (given)
*            non-zero if help library search is required
*     STATUS =INTEGER       (given and returned)
*            status

*  Algorithm:
*     Use system-dependent routine UFACE_SCRNSZ to obtain the
*     width and height of the screen or window being used.
*     Call HLP_HELP specifying routine UFACE_OPUT and UFACE_IPUT
*     to handle line output and topic prompts respectively. Use the
*     FLAG argument to determine if it is required to stay in the help
*     system or return immediately.

*  Copyright:
*     Copyright (C) 1992-1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A. J. Chipperfield  (RLVAD::AJC)
*     KFG: K. F. Hartley (kfh@uk.ac.rl.inf)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (AJC):
*       Original copy of SUBPAR_PWHLP
*       with SUBPAR changed to UFACE and SUP to UFC
*       and change HLP_OUTHLP to HLP_HELP etc
*     09-DEC-1993 (AJC):
*       For Unix comment out SCREEN sizing
*     15-FEB-1994 (AJC):
*       Report library name if error
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*    Global constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'UFACE_ERR'
*  Arguments Given:
      CHARACTER*(*)  TOPIC,LIBRARY
      INTEGER FLAG

*  Status:
      INTEGER        STATUS

*    External references :
      EXTERNAL       UFACE_IPUT, UFACE_OPUT
      INTEGER        UFACE_IPUT, UFACE_OPUT
      INTEGER        HLP_HELP
      EXTERNAL       HLP_HELP
      EXTERNAL       UFACE_NAMETR
      external hlp_nametr

*  Global Variables:
      INCLUDE 'UFACE_CMN'

*  Local Variables:
      INTEGER        FLAGS          ! Flags for HELP routine
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
!            CALL UFACE_SCRNSZ( SCREEN_WIDTH, UFACEPGSZ, STATUS )
             SCREEN_WIDTH = 80
             UFACEPGSZ = 24
            IF ( STATUS .EQ. SAI__OK ) THEN
*            Initialize line count
               UFACELCNT = UFACEPGSZ
*            and output help
               ISTAT = HLP_HELP( UFACE_OPUT, SCREEN_WIDTH, TOPIC, LUIN,
     :                     LIBRARY, FLAGS, UFACE_IPUT, UFACE_NAMETR )

*            Set STATUS
               IF (ISTAT .EQ. 1) THEN
                  STATUS = SAI__OK
               ELSE
                  STATUS = UFACE__HLPER
                  CALL EMS_REP( 'UFC_PWHLP1',
     :            'UFACE: Failure on obtaining multi-line help',
     :            STATUS )
                  CALL EMS_SETC( 'LIB', LIBRARY )
                  CALL EMS_SETC( 'TOPIC', TOPIC )
                  CALL EMS_REP( 'UFC_PWHLP2',
     :            'from help file: ^LIB, Topic: ^TOPIC',
     :            STATUS )
                  CALL HLP_ERRMES( ISTAT, MES )
                  CALL EMS_SETC( 'HLPMES', MES )
                  CALL EMS_REP( 'UFC_PWHLP3',
     :            '^HLPMES', STATUS )
               END IF

            END IF

*         Exit loop
            GOTO 100

         END IF

10    CONTINUE

*   If drops out of loop,
*   all unit numbers already in use - report
      STATUS = UFACE__HLPER
      CALL EMS_REP( 'UFC_PWHLP4',
     :'UFACE: No Fortran unit number available for the help system',
     : STATUS )

100   CONTINUE

      END
