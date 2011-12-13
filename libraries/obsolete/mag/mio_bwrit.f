      SUBROUTINE MIO_BWRIT(TD, BUFSIZ, BUFFER, NWRIT, STATUS)
*+
*  Name:
*     MIO_BWRIT

*  Purpose:
*     write magnetic tape Block.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_BWRIT(TD, NVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     A supplied byte array is written to tape as a single block.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     NVAL=INTEGER (Given)
*        Expression specifying the number of bytes to be written
*        to tape as a single block.
*     VALUES(NVAL)=BYTE (Given)
*        Array containing the data to be written to tape.
*     ACTVAL=INTEGER (Returned)
*        Variable to receive the actual number of bytes read.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Check for a valid tape descriptor and that the tape is open, if so, the
*     tape descriptor is used to obtain a tape channel and the SYS$QIOW System
*     Service is used to write a physical block (record) to the tape.

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
*     06-Aug-1980: Original. (UCL::SLW)
*     01-FEB-1983: Fortran 77 Version. (UCL::JRG)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     14-Jul-1986: Check return status of QIOW. (RAL::AJC)
*     21-Oct-1991: Treat IOSB(2) as unsigned. (RAL::AJC)
*     15-Nov-1991: Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     15-Jan_1992: Changed to use ioc_write for Unix version.
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
      INTEGER BUFSIZ            ! size of buffer in bytes
      BYTE BUFFER(*)            ! buffer containing bytes to be written

*  Arguments Returned:
      INTEGER NWRIT             ! number of bytes written
*    Status return :
      INTEGER STATUS            ! status return

*  External References:
      EXTERNAL IOC_WRITE        ! write routine in C
      LOGICAL CHR_SIMLR
      EXTERNAL MIO1_BLK          ! Block data subprogram that
                                 ! initializes MIOINT
*  Global Variables:
      INCLUDE 'MIOFIL_CMN'

*  Local Variables:
      INTEGER MAGCN             ! channel number

*.


C      print *,'mio_bwrit:status,td,bufsiz',status,td,bufsiz
      IF ( STATUS.EQ.SAI__OK ) THEN
         IF ( CHR_SIMLR(MACMOD(TD),'READ') ) THEN
            STATUS = MIO__ILLAC
         ELSE IF ( BUFSIZ.LT.14 .OR. BUFSIZ.GT.65535 ) THEN
            STATUS = MIO__BUFTB
         ELSE
            CALL MIO1_CHAN(TD, MAGCN, STATUS)
            IF ( STATUS.EQ.SAI__OK )
     :           CALL IOC_WRITE(MAGCN, BUFSIZ, BUFFER, NWRIT, STATUS)
         END IF
      END IF

C      print *,'mio_bwrit:status,nwrit',status,nwrit
      END
