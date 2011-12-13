
      SUBROUTINE MAG_MOVE(TP, FILE, START, BLOCK, STATUS)
*+
*  Name:
*     MAG_MOVE

*  Purpose:
*     Move to a specified file and block on tape.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_MOVE(TD, FILE, START, BLOCK, STATUS)

*  Description:
*     Assuming that the current tape position is known, move to a
*     position on the tape specified by a file and block number.
*     The block number can be relative either to the start or the
*     end of the specified file.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     FILE=INTEGER (Given)
*        Expression specifying the file number to which the tape
*        is to be moved.
*     START=LOGICAL (Given)
*        Expression indicating whether the block number is relative
*        to the start or end of the specified file.
*     BLOCK=INTEGER (Given)
*        Expression specifying the block number within the file at
*        which the tape is to be positioned. Note that when START
*        is false i.e. BLOCK is relative to the end of the file, its
*        value is what would be expected if the tape were being read
*        backwards. This means that in order to position the tape to
*        read the last block in the file, BLOCK must be set to 2 and
*        not 1.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     The current file and block positions are obtained. If they are
*     not already defined in the MAG_IO Common Block, the tape will be
*     rewound to fix the position.
*     The tape is then positioned at the start or end of the required
*     file by skipping tape marks forwards or backwards as required by
*     the value of FILE relative to the current file number and the
*     value of START (or by rewinding if a position relative to the
*     start of file 1 is required).
*     The required block position is then found by skipping blocks
*     forwards or backwards as required by the values of BLOCK and
*     START.

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
*     15-OCT-1981:  Original  (UCL::JRG)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     11-JUN-1986:  Explain Block numbers and Method (RLVAD::AJC)
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

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
      INTEGER FILE              ! required file
      LOGICAL START             ! Whether at start or end of file
      INTEGER BLOCK             ! block number
*    Status return :
      INTEGER STATUS            ! status return

*  Local Variables:
      INTEGER CFILE             ! current file number
      INTEGER CSTART            ! current file offset position
      INTEGER CBLOCK            ! current block number
      LOGICAL MOVED             ! has tape been moved ?
      INTEGER NSKIP             ! number of files/blocks to be skipped
      INTEGER DIR               ! direction of file/block skips

*.


      IF ( STATUS.NE.SAI__OK ) RETURN

*    Get current position and check requested arguments
      CALL MAG_POS(TP, CFILE, CSTART, CBLOCK, MOVED, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         RETURN
      ELSE IF ( FILE.LT.1 ) THEN
         STATUS = MAG__NOFIL
         CALL MAG1_ERRTP(TP, STATUS)
      ELSE
         IF ( CFILE.LE.0 ) CALL MAG1_FXPOS(TP, CFILE, CSTART, CBLOCK,
     :        STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
*       skip to requested file
            IF ( FILE.EQ.1 .AND. START ) THEN
               CALL MAG_REW(TP, STATUS)
               IF ( STATUS.EQ.SAI__OK ) CBLOCK = 1
            ELSE IF ( CFILE.LE.0 ) THEN
               STATUS = MAG__NOPOS
               CALL MAG1_ERRTP(TP, STATUS)
            ELSE IF ( (FILE.GT.CFILE.AND.START) .OR.
     :                (FILE.LT.CFILE.AND..NOT.START) ) THEN
               NSKIP = FILE - CFILE
               CALL MAG_SKIP(TP, NSKIP, STATUS)
               IF ( STATUS.EQ.SAI__OK ) CBLOCK = 1
            ELSE IF ( (FILE.GT.CFILE.AND..NOT.START) .OR.
     :                (FILE.LT.CFILE.AND.START) ) THEN
               IF ( FILE.GT.CFILE ) THEN
                  DIR = 1
               ELSE
                  DIR = -1
               END IF
               NSKIP = FILE - CFILE + DIR
               CALL MAG_SKIP(TP, NSKIP, STATUS)
               IF ( STATUS.EQ.SAI__OK ) THEN
                  CALL MAG_SKIP(TP, -DIR, STATUS)
                  IF ( STATUS.EQ.SAI__OK ) CBLOCK = 1
               END IF
            END IF
         END IF
      END IF

*    Skip to specified block if all is correct up to this point
      IF ( STATUS.EQ.SAI__OK ) THEN
         IF ( BLOCK.GT.0 ) THEN
            IF ( START ) THEN
               CALL MAG_JUMP(TP, BLOCK-CBLOCK, STATUS)
            ELSE IF ( .NOT.START ) THEN
               CALL MAG_JUMP(TP, CBLOCK-BLOCK, STATUS)
            ELSE
               STATUS = MAG__NOPOS
               CALL MAG1_ERRTP(TP, STATUS)
            END IF
         END IF
      END IF

      RETURN
      END
