      SUBROUTINE MIO_FLUSH(TD, STATUS)
*+
*  Name:
*     MIO_FLUSH

*  Purpose:
*     flush out I/O buffers.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_FLUSH(TD, STATUS)

*  Description:
*     Flush buffers to a tape.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Check for a valid tape descriptor and that the tape is open for writing.
*     Flush any buffers to the tape.

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
*     {enter_new_authors_here}

*  History:
*     06-Aug-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     22-Nov-1984: Write a short buffer. (RAL::IPMAF)
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

*  Status:
      INTEGER STATUS            ! status return

*  External References:
      LOGICAL CHR_SIMLR
      EXTERNAL MIO1_BLK          ! Block data subprogram that
                                 ! initializes MIOINT
*  Global Variables:
      INCLUDE 'MIOBUF_CMN'
      INCLUDE 'MIOFIL_CMN'

*  Local Variables:
      INTEGER MAGCN             ! channel number
      INTEGER NWRIT             ! number of bytes written

*.


C      print *,'mio_flush:status,td',status,td
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
            IF ( CHR_SIMLR(MACMOD(TD),'WRITE') ) THEN
               IF ( MNBYTE(TD).NE.0 ) THEN
                  CALL MIO_BWRIT(TD, MNBYTE(TD), MBLOCK(1,TD), NWRIT,
     :                           STATUS)
                  MNBYTE(TD) = 0
               END IF
            END IF
         END IF
      END IF

C      print *,'mio_flush:status,nwrit:',status,nwrit
      RETURN
      END
