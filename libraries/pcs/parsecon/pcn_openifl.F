#if HAVE_CONFIG_H
#include "config.h"
#endif
      SUBROUTINE PARSECON_OPENIFL ( IFLNAM, LUCON, STATUS )
*+
*  Name:
*     PARSECON_OPENIFL

*  Purpose:
*     Open interface file and return its channel number

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL PARSECON_OPENIFL ( IFLNAM, LUCON, STATUS )

*  Description:
*     Open the interface file and return its logical unit number.

*  Arguments:
*     IFLNAM=CHARACTER*(*) (given)
*           name of interface file
*     LUCON=INTEGER (returned)
*           logical unit number of interface file
*     STATUS=INTEGER

*  Algorithm:
*     A free FORTRAN unit number is obtained, and the file is opened.

*  Copyright:
*     Copyright (C) 1984-1985, 1991 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1984 (REVAD::BDK):
*        Original
*     25-OCT-1984: assume file is in default directory rather than
*                  ADAM_IFL (REVAD::BDK)
*     23-MAY-1985 (REVAD::BDK):
*        Use EXENAME instead of TASKNAME
*     17-JUN-1991 (RLVAD::AJC):
*        Portably obtain Fortran unit number
*     16-JUL-1991 (RLVAD::AJC):
*        Assume full file name is given
*     02-OCT-1991 (RLVAD::AJC):
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Prefix Error Reports With Parsecon:
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'

*  Arguments Given:
      CHARACTER*(*) IFLNAM             ! name of execution module

*  Arguments Returned:
      INTEGER LUCON                     ! FORTRAN unit number

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ISTAT                     ! Local status
      LOGICAL OPEN                      ! Whether unit number is in use

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain an unused unit number
      DO LUCON = 1,99
*     For numbers 1 to 99 inquire if number is in use
         INQUIRE (UNIT=LUCON, OPENED=OPEN )
         IF ( .NOT. OPEN ) THEN
*        If number is not in use, use it to open the file
            ISTAT = 0
            OPEN ( UNIT = LUCON, FILE = IFLNAM, STATUS = 'OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :           ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :           READONLY,
#endif
     :           IOSTAT = ISTAT )

*        If failed, report reason
            IF ( ISTAT .NE. 0 ) THEN
               STATUS = PARSE__IFLOPN
               CALL EMS_SETC( 'FILE', IFLNAM )
               CALL EMS_REP( 'PCN_OPENIFL1',
     :          'PARSECON: Failed to open interface file ^FILE',
     :           STATUS )
               CALL EMS_FIOER( 'FIOSTAT', ISTAT )
               CALL EMS_REP( 'PCN_OPENIFL2', '^FIOSTAT', STATUS )
            ENDIF

*        and exit
            GOTO 100

         ENDIF

      ENDDO

*  No unit number available - report
      STATUS = PARSE__IFLOPN
      CALL EMS_SETC( 'FILE', IFLNAM )
      CALL EMS_REP( 'PCN_OPENIFL3',
     : 'PARSECON: Failed to open interface file ^FILE', STATUS )
      CALL EMS_REP( 'PCN_OPENIFL4',
     : 'No Fortran unit numbers available', STATUS )

100   CONTINUE

      END
