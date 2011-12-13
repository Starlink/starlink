      SUBROUTINE MAG_DISM(PNAME, UNLOAD, STATUS)
*+
*  Name:
*     MAG_DISM

*  Purpose:
*     Dismount tape from drive.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_DISM(PARAM, UNLOAD, STATUS)

*  Description:
*     Dismount a tape from the tape drive specified by the parameter
*     and set the entry for the drive in the user's device dataset
*     (USREDEVDATA) to unknown position.
*     If the dismount fails, the entry will be unaltered.

*  Arguments:
*     PARAM=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device Parameter.
*     UNLOAD=LOGICAL (Returned)
*        Expression specifying whether the tape is to be unloaded.
*        If .TRUE. the tape will be unloaded; if .FALSE. it will not.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Obtain a physical tape name for the tape drive and
*     use MIO_DISM to dismount the tape on this drive.

*  Copyright:
*     Copyright (C) 1983, 1986, 1988, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*     17-Jul-1986:  For ADAM HDS_CLOSE USRDEVDATA on exit  (RAL::AJC)
*        6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*        1-Nov-1988:  Correct documentation to include UNLOAD  (RAL::AJC)
*     18-Jan-1990:  Re-cast to exit for most errors (RAL::AJC)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 ({author_identifier}):
*        Add INCLUDE 'DAT_PAR'
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_ERR'         ! PAR Errors
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG error constants

*  Arguments Given:
      CHARACTER*(*) PNAME       ! Tape Parameter Name
      LOGICAL UNLOAD            ! unload tape ?

*  Arguments Returned:
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Local Variables:
      CHARACTER*(DAT__SZLOC) ELOC   ! locator to USRDEVDATA
      CHARACTER*(DAT__SZLOC) LOC    ! locator to Tape dataset
      CHARACTER*(MAG__SZNAM) TAPE   ! Tape Device or Logical Name
      INTEGER TP                ! Parameter Descriptor
      INTEGER RTP               ! relative Parameter Descriptor
      INTEGER FILE              ! current file number
      LOGICAL START             ! relative to start ?
      INTEGER BLOCK             ! current block number
      INTEGER ISTAT             ! Local status
      LOGICAL FINISHED          ! Action complete flag

*.


*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Initialised ?
      IF ( MAGSLP ) CALL MAG_ACTIV(STATUS)

*    Get a Parameter descriptor
      CALL MAG1_GETTP(PNAME, TP, RTP, STATUS)
      IF ( STATUS.NE.SAI__OK ) CALL MAG1_ERNAM(PNAME, STATUS)

      FINISHED = .FALSE.
      DO WHILE ( (STATUS.EQ.SAI__OK) .AND. (.NOT.FINISHED) )
*       Get device name associated with parameter
         CALL MAG1_CKTDS(PNAME, ELOC, LOC, STATUS)

         IF ( STATUS.EQ.SAI__OK ) THEN
            CALL MAG1_RDTDS(LOC, TAPE, FILE, START, BLOCK, STATUS)

            IF ( STATUS.EQ.SAI__OK ) THEN
*            Dismount the tape
               CALL MIO_DISM(TAPE, UNLOAD, STATUS)
               IF ( STATUS.EQ.SAI__OK ) THEN
*               If successful, cancel the device entry
                  ISTAT = SAI__OK
                  CALL MAG1_CNTDS(LOC, ISTAT)
               ELSE
*               Otherwise report an error
                  CALL MAG1_ERNAM(PNAME, STATUS)
               END IF
            END IF
*         Annul locators to device dataset
            CALL DAT_ANNUL(LOC, STATUS)
            CALL HDS_CLOSE(ELOC, STATUS)
         END IF

*      If No entry in DEVDATASET or no such device on machine,
         IF ( (STATUS.EQ.MAG__UNKDV) .OR. (STATUS.EQ.MAG__NSHDV) ) THEN
*         Clear the deckes and try again
            CALL PAR_CANCL(PNAME, STATUS)
            CALL ERR_FLUSH(STATUS)
         ELSE
*         Otherwise exit
            FINISHED = .TRUE.
         END IF

      END DO

      END
