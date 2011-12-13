      SUBROUTINE MAG_ALOC(PNAME, STATUS)
*+
*  Name:
*     MAG_ALOC

*  Purpose:
*     Allocate tape device.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_ALOC(PARAM, STATUS)

*  Description:
*     Allocate the tape drive specified by the parameter for continued
*     use.

*  Arguments:
*     PARAM=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device Parameter.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     The tape drive pointed to by the parameter name is allocated to
*     the calling process using MIO_ALOC.
*     If the specified tape entry cannot be found in DEVDATASET
*     or there is no tape of the name specified by the DEVDATASET entry,
*     cancel the parameter, flush any error messages and try again.

*  Copyright:
*     Copyright (C) 1983, 1986, 1987, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     A J Chipperfield (RAL::AJC)
*     {enter_new_authors_here}

*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*        6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     13-Jul-1987:  Add USRDEVDATA locator to MAG1_CKTDS arguments
*        (RAL::AJC)
*     18-Jan-1990:  Re-cast to exit for most errors (RAL::AJC)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Added INCLUDE 'DAT_PAR'
*        Added INCLUDE 'PAR_PAR'
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
      INCLUDE 'MAGSCL_PAR'      ! MAG parameter constants
      INCLUDE 'MAG_ERR'         ! MAG error constants

*  Arguments Given:
      CHARACTER*(*) PNAME       ! Tape Parameter Name

*  Arguments Returned:
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch
      INCLUDE 'MAGPA_SCL'       ! MAG parameter table

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC   ! locator to Tape dataset
      CHARACTER*(MAG__SZNAM) TAPE  ! Tape Device or Logical Name
      INTEGER TP                ! Parameter Descriptor
      INTEGER RTP               ! relative Parameter Descriptor
      INTEGER FILE              ! current file number
      LOGICAL START             ! relative to start ?
      INTEGER BLOCK             ! current block number
      LOGICAL FINISHED          ! action completed

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
         CALL MAG1_CKTDS(PNAME, PLOC(RTP), LOC, STATUS)

         IF ( STATUS.EQ.SAI__OK ) THEN
*         Get physical device name from the DEVDATASET entry
            CALL MAG1_RDTDS(LOC, TAPE, FILE, START, BLOCK, STATUS)

            IF ( STATUS.EQ.SAI__OK ) THEN
*            Allocate the device
               CALL MIO_ALOC(TAPE, STATUS)
               IF ( STATUS.NE.SAI__OK ) CALL MAG1_ERNAM(PNAME, STATUS)
            END IF
*         Annul the device dataset entry locator
            CALL DAT_ANNUL(LOC, STATUS)
         END IF

*      If No entry in DEVDATASET or no such device on machine,
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
