
      SUBROUTINE MAG1_CKTDS(PARAM, ELOC, LOC, STATUS)
*+
*  Name:
*     MAG1_CKTDS

*  Purpose:
*     check tape dataset.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CKTDS(PARAM, ELOC, LOC, STATUS)

*  Description:
*     To check the existence of the tape device parameter name

*  Arguments:
*     PARAM=CHARACTER*(*) (Given)
*        The name of the tape device parameter
*     ELOC=CHARACTER*(*) (Returned)
*        The locator to the container file of user's device data (USRDEVDATA)
*     LOC=CHARACTER*(*) (Returned)
*        The locator to the data structure which contains the tape device
*        parameter
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     The existence of the mag tape datastructure is checked using data system
*     routines.
*     If it does not exist, it is created.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright         22-Apr-82
*     A.Chipperfield     3-Jun-86   ADAM version
*     A.Chipperfield     16-Jul-86  Add eloc to parameters
*     K F Hartley         8-Nov-91  Remove commented out code
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$CKTDS

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_ERR'         ! PAR Error codes
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      CHARACTER*(*) PARAM       ! parameter name

*  Arguments Returned:
      CHARACTER*(*) ELOC        ! locator to user's device dataset
      CHARACTER*(*) LOC         ! locator to tape device dataset
      INTEGER STATUS            ! status return

*  Local Variables:
      CHARACTER*(MAG__SZNAM) MNAME
      INTEGER NAMECODE
      LOGICAL FINISHED

*.


*     Get locator to user's store of device data. (Create if reqd.)
      CALL MAG1_GTMPL(ELOC, STATUS)
*
*     Look-up the parameter name
*
      CALL SUBPAR_FINDPAR(PARAM, NAMECODE, STATUS)
C     print *,'SUBPAR_FINDPAR status ',status
*
*     Loop trying to get the name of the graphics device
*
      FINISHED = .FALSE.

      DO WHILE ( (.NOT.FINISHED) .AND. (STATUS.EQ.SAI__OK) )
*
*     Get the user's name for the tape device from the parameter
*     system
*
         CALL SUBPAR_GETNAME(NAMECODE, MNAME, STATUS)

         IF ( (STATUS.EQ.PAR__NULL) .OR. (STATUS.EQ.PAR__ABORT) .OR.
     :        (STATUS.EQ.PAR__NOUSR) ) THEN
            FINISHED = .TRUE.

         ELSE IF ( STATUS.EQ.SAI__OK ) THEN
*           Get locator to required user's tape device data
            CALL DAT_FIND(ELOC, MNAME, LOC, STATUS)

            IF ( STATUS.NE.SAI__OK ) THEN
*              This device is not in user's store. Copy from system store
               CALL ERR_ANNUL(STATUS)
               CALL MAG1_CPTDS(ELOC, MNAME, LOC, STATUS)
            END IF

            FINISHED = .TRUE.
         END IF

      END DO

      END
