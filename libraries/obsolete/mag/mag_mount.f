      SUBROUTINE MAG_MOUNT(PNAME, ACMODE, STATUS)
*+
*  Name:
*     MAG_MOUNT

*  Purpose:
*     Mount tape on drive.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_MOUNT(PARAM, MODE, STATUS)

*  Description:
*     Mount a tape on the tape drive specified by the parameter and
*     initialise the entry for the drive in the user's device dataset
*     (USRDEVDATA).
*     If the mount fails, but a device entry was found, the entry will
*     be cancelled (i.e. set to unknown position) unless the failure
*     was MAG__DVMNT (device already mounted), in which case it will
*     be unaltered.

*  Arguments:
*     PARAM=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device Parameter.
*     MODE=CHARACTER*(*) (given)
*        Expression specifying the access mode:  'READ', 'WRITE' or
*        'UPDATE' (read and write), as appropriate.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Obtain a physical tape name for the tape drive and
*     use MIO_MOUNT to mount the tape on this drive.
*     Operation is analagous to DAT_CREAT ie. normally need MAG_ASSOC
*     subsequently.
*     If the specified tape entry cannot be found in DEVDATASET
*     or there is no tape of the name specified by the DEVDATASET entry,
*     cancel the parameter, flush any error messages and try again.

*  Implementation Deficiencies:
*     No use is made of the MODE argument at present.

*  Copyright:
*     Copyright (C) 1983, 1986, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*     17-Jul-1986:  HDS_CLOSE USRDEVDATA at end  (RAL::AJC)
*        6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     18-Jan-1990:  Re-cast to exit for most errors (RAL::AJC)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Add INCLUDE 'DAT_PAR'
*     12-NOV-1996 (AJC):
*        Remove declaration of I
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
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      CHARACTER*(*) PNAME       ! Tape Parameter Name
      CHARACTER*(*) ACMODE      ! Tape access mode

*  Arguments Returned:
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Local Variables:
      CHARACTER*(DAT__SZLOC) ELOC  ! locator to USRDEVDATA
      CHARACTER*(DAT__SZLOC) LOC   ! locator to Tape dataset
      CHARACTER*(MAG__SZNAM) TAPE  ! Tape Device or Logical Name
      INTEGER TP                ! Parameter Descriptor
      INTEGER RTP               ! relative Parameter Descriptor
      INTEGER FILE              ! current file number
      LOGICAL START             ! relative to start ?
      INTEGER BLOCK             ! current block number
      INTEGER ISTAT             ! Local status
      LOGICAL FINISHED          ! Job done flag

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
*          Get physical device name from the USRDEVDATA entry
            CALL MAG1_RDTDS(LOC, TAPE, FILE, START, BLOCK, STATUS)

            IF ( STATUS.EQ.SAI__OK ) THEN
*             Mount the device
               CALL MIO_MOUNT(TAPE, ACMODE, STATUS)

               IF ( STATUS.EQ.SAI__OK ) THEN
*                Mount OK
*                Initialize device dataset
                  CALL MAG1_WRTDS(LOC, 1, .TRUE., 1, STATUS)

               ELSE
*                Convert to MAG status and print message
                  CALL MAG1_ERNAM(PNAME, STATUS)
*                Cancel tape dataset -
*                unless error was 'device already mounted'
                  IF ( STATUS.NE.MAG__DVMNT ) THEN
                     ISTAT = SAI__OK
                     CALL MAG1_CNTDS(LOC, ISTAT)
                  END IF
               END IF
            END IF
*         Annul device dataset locators in case they exist (we can't be
*         sure)
*         Annul user's device data locator
            CALL DAT_ANNUL(LOC, STATUS)
*         Force container file close
            ISTAT = SAI__OK
            CALL HDS_CLOSE(ELOC, ISTAT)
            IF ( STATUS.EQ.SAI__OK ) STATUS = ISTAT
         END IF

*       Check on errors which may be corrected
         IF ( (STATUS.EQ.MAG__UNKDV) .OR. (STATUS.EQ.MAG__NSHDV) ) THEN
*         Clear the decks and try again
            CALL PAR_CANCL(PNAME, STATUS)
            CALL ERR_FLUSH(STATUS)

         ELSE
*         Otherwise exit
            FINISHED = .TRUE.
         END IF

      END DO

      END
