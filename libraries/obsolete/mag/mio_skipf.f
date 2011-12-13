
      SUBROUTINE MIO_SKIPF(TD, NFILE, STATUS)
*+
*  Name:
*     MIO_SKIPF

*  Purpose:
*     Skip file marks.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_SKIPF(TD, NFILE, STATUS)

*  Description:
*     Skip the specified number of file marks

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     NFILE=INTEGER (Given)
*        Number of files to skip.
*        A negative number indicates that the tape is to be moved
*        in the reverse direction (towards its load point).
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Check for a valid tape descriptor and that the tape is open, if so, the
*     tape descriptor is used to get a device channel number and the ioc_skipf
*     routine is used to skipover each file in turn.
*     On encountering an MIO__EOV condition, the tape is left positioned
*     between the two tape marks; that is, the skipping request has not
*     completed.
*     An EOV condition can only be cleared by the MIO_SKIPV routine.

*  Copyright:
*     Copyright (C) 1980, 1983, 1986, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     14-APR-1980:  Original.  (UCL::SLW)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     10-May-1983:  Tidy up for Starlink version. (UCL::SLW)
*     14-Jul-1986:  Check return status of QIOW. (RAL::AJC)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     17-Jan-1992:  Changed to use ioc_skipb for Unix version.
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     This is the Unix version.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MIO_SYS'         ! MIO Internal Constants
      INCLUDE 'MIO_ERR'         ! MIO Errors

*  Arguments Given:
      INTEGER TD                ! tape descriptor
      INTEGER NFILE             ! number of files to skip

*  Status:
      INTEGER STATUS            ! status return

*  Local Variables:
      INTEGER MAGCN             ! channel number
      INTEGER NLEFT             ! number of tape marks to be skipped
      INTEGER DIR               ! direction of skip

*.


C      print *,'mio_skipf:status,td,nfile',status,td,nfile
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
         IF ( STATUS.NE.SAI__OK ) THEN
            RETURN
         ELSE IF ( NFILE.EQ.0 ) THEN
            RETURN
         ELSE
            IF ( NFILE.LT.0 ) THEN
               DIR = -1
            ELSE
               DIR = 1
            END IF
            NLEFT = NFILE
            DO WHILE ( NLEFT.NE.0 )
               NLEFT = NLEFT - DIR
               CALL IOC_SKIPF(MAGCN, DIR, STATUS)
            END DO
         END IF
      END IF

C      print *,'mio_skipf:status',status

      END
