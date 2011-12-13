
      SUBROUTINE MAG1_FXPOS(TP, FILE, START, BLOCK, STATUS)
*+
*  Name:
*     MAG1_FXPOS

*  Purpose:
*     Fix tape position, even if it means rewinding tape.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_FXPOS(TD, FILE, START, BLOCK, STATUS)

*  Description:
*     Ensure that the tape position is known, even if it means rewinding the
*     tape to its load point.

*  Arguments:
*     TD=INTEGER (Given)
*        Tape descriptor.
*     FILE=INTEGER (Returned)
*        Variable to receive the tape file number.   This is returned
*        zero if not known.
*     START=LOGICAL (Returned)
*        Variable to receive whether the tape is positioned at the start
*        of a file.   It is only returned if BLOCK is non-zero.
*     BLOCK=INTEGER (Returned)
*        Variable to receive the block number.   If START is TRUE
*        then this is relative to the start of the file;  otherwise
*        it is relative to the end of the file.   It is returned zero
*        if not known.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     If the file number is undefined then the tape is rewound.
*     If the file position is still not known then give up.

*  Copyright:
*     Copyright (C) 1981, 1983, 1986, 1991, 1993 Science & Engineering Research Council.
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
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     23-OCT-1981:  Original.  (UCL::JRG)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     05-JUN-1986:  Make message non environment dependent to permit
*           MAG_MOVE and this to be in MAG not MAGPAR  (RLVAD::AJC)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$FXPOS

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      INTEGER TP                ! tape descriptor

*  Arguments Returned:
      INTEGER FILE              ! file number
      LOGICAL START             ! block origin
      INTEGER BLOCK             ! block number
*    Status return :
      INTEGER STATUS            ! status return

*  Local Variables:
      LOGICAL MOVED             ! has tape been moved ?

*.


C     print *,'mag1_fxpos:tp,status',tp,status
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MAG_POS(TP, FILE, START, BLOCK, MOVED, STATUS)
         IF ( STATUS.NE.SAI__OK ) THEN
            CALL MAG1_ERRTP(TP, STATUS)
         ELSE IF ( FILE.LE.0 ) THEN
            CALL MSG_OUT('MAG_NOPOS', 'Position of Tape not known - ' //
     :                   'Rewinding tape', STATUS)
            CALL MAG_REW(TP, STATUS)
            IF ( STATUS.EQ.SAI__OK ) THEN
               CALL MAG_POS(TP, FILE, START, BLOCK, MOVED, STATUS)
               IF ( STATUS.NE.SAI__OK ) THEN
                  CALL MAG1_ERRTP(TP, STATUS)
               ELSE IF ( FILE.LE.0 ) THEN
                  STATUS = MAG__NOPOS
                  CALL MAG1_ERRTP(TP, STATUS)
               END IF
            END IF
         END IF
      END IF

C      print *,'mag1_fxpos:status,file,start,block',
C    :                 status,file,start,block
      END
