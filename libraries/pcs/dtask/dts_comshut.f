      SUBROUTINE DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT,
     :  AKEY, VALUE, STATUS )
*+
*  Name:
*     DTASK_COMSHUT

*  Purpose:
*     Shut-down communications for a transaction

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT,
*     :  AKEY, VALUE, STATUS )

*  Description:
*     Shut-down communications for a transaction, including sending the
*     final acknowledgement. Operate even on bad entry status.

*  Arguments:
*     PATH=INTEGER (given)
*           message path needed for reply
*     MESSID=INTEGER given)
*           transaction number needed for reply
*     MESSTATUS=INTEGER ( given)
*           status to be returned in completion message
*     CONTEXT=INTEGER (given)
*           context to be returned in completion message
*     AKEY=CHARACTER*(*) (given)
*           action keyword
*     VALUE=CHARACTER*(*) (given and returned)
*           string to be returned in completion message
*     STATUS=INTEGER (returned)
*           status is returned OK if at all possible.

*  Algorithm:
*     Flush the ERR and MSG systems. Send the final acknowledgment.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1991 (REVAD::BDK):
*        Original
*     28-MAY-1991 (REVAD::BDK):
*        Use ERR_CLEAR
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
*     11-JUN-1991 (REVAD::BDK):
*        Report failure in shutdown
*     25-NOV-1991 (REVAD::BDK):
*        Use ADAM_ACKNOW
*     14-OCT-1992 (RLVAD::AJC):
*        Get ^STATUS via DTASK_ESETK
*     11-JUN-2001: call AMS_REPLY/PLOOKUP (FAMS) directly
*                  ADAM_PRCNAM now DTASK_PRCNAM (AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER PATH               ! message path needed for reply
      INTEGER MESSID             ! transaction number needed for reply
      INTEGER MESSTATUS          ! status to be returned in completion
                                 ! message
      INTEGER CONTEXT            ! context to be returned in completion
                                 ! message
      CHARACTER*(*) AKEY         ! keyword of action required
      CHARACTER*(*) VALUE        ! command line parameter string

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MESLEN                     ! length of value
      CHARACTER*(MESSYS__TNAME) MYNAME   ! name of this task
      INTEGER NLENGTH                    ! actual length of MYNAME
      CHARACTER*(MESSYS__TNAME) BADNAME  ! name of other task when
                                         ! failed to close
                                         ! communications
      INTEGER ISTAT                      ! local status
*.


*
*   Clear-out the error reporting system, then send the final
*   acknowledgement which shuts down the communications for
*   the transaction.
*
      CALL ERR_CLEAR ( STATUS )
      MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSTATUS,
     :  CONTEXT, AKEY, MESLEN, VALUE, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
*
*      Bad status from closing-down transaction
*
         ISTAT = SAI__OK
         CALL DTASK_PRCNAM ( MYNAME, NLENGTH, ISTAT )
         CALL DTASK_ESETK ( 'STAT', STATUS )
         CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :     ' failed to return acknowledgment, ^STAT', STATUS)
         ISTAT = SAI__OK
         CALL FAMS_PLOOKUP ( PATH, BADNAME, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            BADNAME = 'unknown'
         ENDIF
         CALL MSG_SETC ( 'BADNAME', BADNAME )
         CALL MSG_SETC ( 'AKEY', AKEY )
         IF ( VALUE .EQ. ' ' ) THEN
            CALL ERR_REP ( ' ', 'to task ^BADNAME, action '//
     :        '^AKEY', STATUS )
         ELSE
            CALL MSG_SETC ( 'VALUE', VALUE )
            CALL ERR_REP ( ' ', 'to task ^BADNAME, action '//
     :        '^AKEY, value ^VALUE', STATUS )
         ENDIF
*
*      Clear-out ERR and MSG.
*
         CALL ERR_CLEAR ( STATUS )
      ENDIF

      END
