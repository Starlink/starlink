
      SUBROUTINE MIO_SETRB(TD, BLKSZ, RECSZ, STATUS)
*+
*  Name:
*     MIO_SETRB

*  Purpose:
*     set record and block size.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_SETRB(TD, BLKSZ, RECSZ, STATUS)

*  Description:
*     Set the record and block size for accessing the tape with MIO_READ and
*     MIO_WRITE .

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     BLKSZ=INTEGER (Given)
*        An Expression giving the size of tape blocks in bytes.
*     RECSZ=INTEGER (Given)
*        An Expression giving the required record size in bytes.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Check that the tape descriptor is valid, and that the tape is open.
*     The specified block and record sizes are saved in the MIO_BUF table
*     provided they are sensible.

*  Copyright:
*     Copyright (C) 1980, 1983, 1984, 1991 Science & Engineering Research Council.
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
*     Jon Fairclough (RAL::IPMAF)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     22-Nov-1984: Modified MIO_BUF, block-spanning permitted. (RAL::IPMAF)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     {enter_further_changes_here}

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
      INTEGER BLKSZ             ! block size in bytes
      INTEGER RECSZ             ! record size in bytes

*  Status:
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MIOBUF_CMN'

*  Local Variables:
      INTEGER MAGCN             ! channel number

*.


C      print *,'mio_setrb:status,td,blksz,recsz',status,td,blksz,recsz
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
            IF ( BLKSZ.LE.14 .OR. BLKSZ.GE.MIO__SZBUF ) THEN
               STATUS = MIO__IVBSZ
            ELSE IF ( RECSZ.LE.0 .OR. RECSZ.GT.BLKSZ ) THEN
               STATUS = MIO__IVRSZ
            ELSE
               MBLKSZ(TD) = BLKSZ
               MRECSZ(TD) = RECSZ
            END IF
         END IF
      END IF

C      print *,'mio_setrb:status',status
      RETURN
      END
