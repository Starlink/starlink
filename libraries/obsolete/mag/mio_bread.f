
      SUBROUTINE MIO_BREAD(TD, BUFSIZ, BUFFER, NREAD, STATUS)
*+
*  Name:
*     MIO_BREAD

*  Purpose:
*     read magnetic tape Block.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_BREAD(TD, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     Read a tape block into the array provided, and return its length in basic
*     machine units (bytes).

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     MAXVAL=INTEGER (Given)
*        Expression specifying the size of the array (in bytes)
*        into which data are to be read from tape.
*     VALUES(MAXVAL)=BYTE (Returned)
*        Array to receive the binary contents of a tape block.
*        It must be of sufficient size to contain the entire block.
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
*     tape descriptor is used to obtain a tape channel and ioc_read
*     is used to read a physical block (record) from the tape.

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
*     21-Oct-1991: treat IOSB(1) as unsigned (RAL::AJC)
*     15-Nov-1991: Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     15-Jan-1992: sys$qiow replaced by ioc_read
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     This is the Unix vevsion.

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

*  Arguments Returned:
      BYTE BUFFER(*)            ! buffer to take bytes read
      INTEGER NREAD             ! number of bytes read
*    Status return :
      INTEGER STATUS            ! status return

*  External References:
      EXTERNAL IOC_READ         ! read routine in C

*  Local Variables:
      INTEGER MAGCN             ! channel number

*.


C      print *,'mio_bread:status,td,bufsiz',status,td,bufsiz
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
            IF ( BUFSIZ.LT.14 .OR. BUFSIZ.GT.65535 ) THEN
               STATUS = MIO__BUFTB
            ELSE
               CALL IOC_READ(MAGCN, BUFSIZ, BUFFER, NREAD, STATUS)
            END IF
         END IF
      END IF

C      print *,'mio_read:status,nread',status,nread
      END
