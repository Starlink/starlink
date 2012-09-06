#if HAVE_CONFIG_H
#include "config.h"
#endif
      SUBROUTINE SUBPAR_OPENIFC ( IFCNAM, LUCON, STATUS )
*+
*  Name:
*     SUBPAR_OPENIFC

*  Purpose:
*     Open compiled interface file and return channel number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_OPENIFC ( IFCNAM, LUCON, STATUS )

*  Description:
*     Open the compiled interface file and return its logical unit number.

*  Arguments:
*     IFCNAM=CHARACTER*(*) (Given)
*        name of execution module, with the file type removed, from
*        which the name of the interface file can be deduced
*     LUCON=INTEGER (Returned)
*        logical unit number of interface file
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1984 (BDK):
*        Original
*     23-MAY-1985 (BDK):
*        Use IFCNAM instead of TASKNAME
*     17-JUN-1991 (AJC):
*        Get Fortran unit number portably
*     16-JUL-1991 (AJC):
*        Assume full file name given
*     27-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      CHARACTER*(*) IFCNAM              ! name of execution module


*  Arguments Returned:
      INTEGER LUCON                     ! FORTRAN unit number


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER ISTAT                     ! Local status
      LOGICAL OPEN                      ! Whether chosen unit in use


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain an unused unit number
      DO LUCON = 1,99
*     For numbers 1 to 99 inquire if number is in use
         INQUIRE (UNIT=LUCON, OPENED=OPEN )
         IF ( .NOT. OPEN ) THEN
*        If number is not in use, use it to open the file
            ISTAT = 0
            OPEN ( UNIT = LUCON, FILE = IFCNAM, STATUS = 'OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :           ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :           READONLY,
#endif
     :           FORM = 'UNFORMATTED', IOSTAT = ISTAT )

*        If failed, report reason
            IF ( ISTAT .NE. 0 ) THEN
               STATUS = SUBPAR__IFCOPN
               CALL EMS_SETC( 'FILE', IFCNAM )
               CALL EMS_REP( 'SUP_OPENIFC1',
     :          'SUBPAR: Failed to open interface file ^FILE',
     :           STATUS )
               CALL EMS_FIOER( 'FIOSTAT', ISTAT )
               CALL EMS_REP( 'SUP_OPENIFC2', '^FIOSTAT', STATUS )
            ENDIF

*        and exit
            GOTO 100

         ENDIF

      ENDDO

*  No unit number available - report
      STATUS = SUBPAR__IFCOPN
      CALL EMS_SETC( 'FILE', IFCNAM )
      CALL EMS_REP( 'SUP_OPENIFC3',
     : 'SUBPAR: Failed to open interface file ^FILE', STATUS )
      CALL EMS_REP( 'SUP_OPENIFC4',
     : 'No Fortran unit numbers available', STATUS )

100   CONTINUE

      END
