      SUBROUTINE MIO1_GETTD(TD, STATUS)
*+
*  Name:
*     MIO1_GETTD

*  Purpose:
*     get a tape descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_GETTD(TD, STATUS)

*  Description:
*     Get the next available tape descriptor.

*  Arguments:
*     TD=INTEGER (Returned)
*        A variable to contain the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Search the table of tape descriptors for one that is not being used.

*  Copyright:
*     Copyright (C) 1980, 1983, 1991, 1993 Science & Engineering Research Council.
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
*     30-Jul-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     15-Nov-1991: Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MIO_$GETTD

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MIO_SYS'         ! MIO Internal symbols and errors.
      INCLUDE 'MIO_ERR'         ! MIO Internal symbols and errors.

*  Arguments Returned:
      INTEGER TD                ! tape descriptor

*  Status:
      INTEGER STATUS            ! status return

*  External References:
      EXTERNAL MIO1_BLK          ! Block data subprogram that
                                 ! initializes MIOINT
*  Global Variables:
      INCLUDE 'MIOFIL_CMN'

*  Local Variables:
      INTEGER I                 ! loop index

*.


      IF ( STATUS.NE.SAI__OK ) RETURN

      TD = 0
      DO 100 I = 1, MIO__MXDEV
         IF ( MFREE(I) ) THEN
            TD = I
            GO TO 1
         END IF
 100  CONTINUE
      STATUS = MIO__TOOTD

 1    CONTINUE
C      print *,'mio1_gettd:td,status',td,status
      RETURN
      END
