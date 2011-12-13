      SUBROUTINE MAG_ASSOC(PNAME, ACMODE, TD, STATUS)
*+
*  Name:
*     MAG_ASSOC

*  Purpose:
*     Open a tape device defined by a parameter and return a descriptor

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_ASSOC(DEVICE, MODE, TD, STATUS)

*  Description:
*     Get a descriptor for a tape device specified by its parameter name.

*  Arguments:
*     DEVICE=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device Parameter.
*     MODE=CHARACTER*(*) (Given)
*        Expression specifying the access mode: 'READ', 'WRITE' or
*        'UPDATE' (read and write).
*     TD=INTEGER (Returned)
*        A Variable to contain the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Use PAR_GET0C to get the Device or Logical Name of the tape
*     drive.
*     MAG_OPEN is used to assign a channel to the drive and return a
*     tape descriptor.

*  Copyright:
*     Copyright (C) 1983, 1986, 1988, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     16-Apr-1983:  Original.  (UCL::SLW)
*     16-Jul-1986:  Retain locator to USRDEVDATA in MAGPA common
*                   To allow HDS_CLOSE in MAG_ANNUL
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*      4-Nov-1988:  Correct MODE documentation - remove UPDATE. (RAL::AJC)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Add INCLUDE 'DAT_PAR'
*        Add INCLUDE 'PAR_PAR'
*      1-OCT-1996 (AJC):
*        Restore UPDATE mode
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'PAR_ERR'         ! PAR Errors
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors
      INCLUDE 'MAGSCL_PAR'

*  Arguments Given:
      CHARACTER*(*) PNAME       ! Tape Parameter Name
      CHARACTER*(*) ACMODE      ! Tape access mode

*  Arguments Returned:
      INTEGER TD                ! Tape descriptor
*    Status return :
      INTEGER STATUS            ! status return

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC   ! locator to Tape dataset
      CHARACTER*(MAG__SZNAM) TAPE  ! Tape Device or Logical Name
      INTEGER TP                ! Parameter Descriptor
      INTEGER RTP               ! relative Parameter Descriptor
      INTEGER FILE              ! current file number
      LOGICAL START             ! relative to start ?
      INTEGER BLOCK             ! current block number

*.


C      print *,'mag_assoc:status,pname,acmode ',status,pname,acmode
*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Initialised ?
C      print *,'mag_assoc:Magslp ', Magslp
      IF ( MAGSLP ) THEN
         CALL MAG_ACTIV(STATUS)
C         print *,'mag_assoc:  mag_activ => status ', status
         IF ( STATUS.NE.SAI__OK ) RETURN
      END IF

*    Get a Tape descriptor
      CALL MAG1_GETTP(PNAME, TP, RTP, STATUS)
      IF ( STATUS.EQ.MAG__ISACT ) THEN
         TD = PDESC(RTP)
         STATUS = SAI__OK
         RETURN
      ELSE IF ( STATUS.NE.SAI__OK ) THEN
         CALL MAG1_ERNAM(PNAME, STATUS)
      ELSE
         DO WHILE ( .TRUE. )
*          Get locator to tape dataset.
            CALL MAG1_CKTDS(PNAME, PLOC(RTP), LOC, STATUS)
            IF ( STATUS.EQ.PAR__NULL .OR. STATUS.EQ.PAR__ABORT ) THEN
               GO TO 1
            ELSE IF ( STATUS.EQ.SAI__OK ) THEN
               CALL MAG1_RDTDS(LOC, TAPE, FILE, START, BLOCK, STATUS)
               IF ( STATUS.EQ.SAI__OK ) THEN
                  CALL MAG_OPEN(TAPE, ACMODE, TD, STATUS)
                  IF ( STATUS.EQ.SAI__OK ) THEN
                     CALL MAG_SET(TD, FILE, START, BLOCK, STATUS)
                     PTNAME(RTP) = PNAME
                     PACMOD(RTP) = ACMODE
                     PDESC(RTP) = TD
                     CALL DAT_CLONE(LOC, PDLOC(RTP), STATUS)
                     PFREE(RTP) = .FALSE.
                  END IF
                  IF ( FILE.EQ.0 .AND. CHR_SIMLR(ACMODE,'UPDATE') )
     :                 CALL MAG1_FXPOS(TD, FILE, START, BLOCK, STATUS)
               END IF
               CALL DAT_ANNUL(LOC, STATUS)
            END IF
            IF ( STATUS.EQ.SAI__OK ) GO TO 1
            CALL PAR_CANCL(PNAME, STATUS)
            CALL ERR_FLUSH(STATUS)
         END DO
 1       CONTINUE
      END IF

C      print *,'mag_assoc:  => status ', status

      RETURN
      END
