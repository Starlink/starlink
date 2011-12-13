
      SUBROUTINE MIO_READ(TD, BUFSIZ, BUF, NCHAR, STATUS)
*+
*  Name:
*     MIO_READ

*  Purpose:
*     read magnetic tape record.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_READ(TD, MAXVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     Read a record from the tape with the specified tape descriptor.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     MAXVAL=INTEGER (Given)
*        Expression specifying the size of the array (in bytes)
*        into which data are to be read from tape.
*     VALUES(MAXVAL)=BYTE (Returned)
*        Array to receive the binary contents of a tape record.
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
*     Check for a valid tape descriptor and that the tape is open.   If the
*     current block is not exhausted then the next record is copied from it;
*     otherwise a new block is obtained and the first record extracted.

*  Copyright:
*     Copyright (C) 1980, 1983, 1984, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     06-Aug-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     22-Nov-1984: Rewritten to permit block-spanning and different
*           sized blocks to that specified. (RAL::IPMAF)
*     15-Nov-1991: Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     15-Jan-1992: Changed to remove use of lib$movc3 (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     This is a completely portable version.
*     (May be less efficient than VMS version.)

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
      INTEGER BUFSIZ            ! buffer size

*  Arguments Returned:
      BYTE BUF(*)               ! buffer to receive data
      INTEGER NCHAR             ! size of data read in

*  Status:
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MIOBUF_CMN'

*  Local Variables:
      INTEGER MAGCN             ! channel number
      INTEGER NREAD             ! number of bytes read in
      INTEGER I                 ! Offset into block buffer
      INTEGER K                 ! Loop index
      INTEGER BUFP              ! Offset into record buffer
C     character*(MIO__SZBUF) bufcopy

*.


C     print *,'mio_read:status,td,bufsiz',status,td,bufsiz
      NCHAR = 0
      IF ( STATUS.EQ.SAI__OK ) THEN
*
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
*
         IF ( STATUS.EQ.SAI__OK ) THEN
*
*          Load Block buffer if necessary
*
            IF ( MNBYTE(TD).LE.0 ) THEN ! No bytes in buffer
               CALL MIO_BREAD(TD, MBLKSZ(TD), MBLOCK(1,TD), NREAD,
     :                        STATUS)
               IF ( STATUS.EQ.SAI__OK ) THEN
                  IF ( NREAD.LE.14 .OR. NREAD.GE.MIO__SZBUF ) THEN
                     STATUS = MIO__IVBSZ
                  ELSE
                     MBLKSZ(TD) = NREAD
                     BUFP = 0
                  END IF
               END IF
*
*          Reload block buffer if necessary
*
            ELSE IF ( MNBYTE(TD).GT.MBLKSZ(TD)-MRECSZ(TD) ) THEN
*
*            Move part record
*
               BUFP = MBLKSZ(TD) - MNBYTE(TD)
               IF ( BUFP.GT.0 ) THEN
                  DO 5 K = 1, BUFP
                     BUF(K) = MBLOCK(MNBYTE(TD)+K, TD)
 5                CONTINUE
               END IF
*
*            Read next block
*
               CALL MIO_BREAD(TD, MBLKSZ(TD), MBLOCK(1,TD), NREAD,
     :                        STATUS)
               IF ( STATUS.EQ.SAI__OK ) THEN
                  IF ( NREAD.LE.14 .OR. NREAD.GE.MIO__SZBUF ) THEN
                     STATUS = MIO__IVBSZ
                  ELSE
                     MBLKSZ(TD) = NREAD
                     MNBYTE(TD) = 0
                  END IF
               END IF
            ELSE
*
*          Start next record
*
               BUFP = 0
            END IF
*
*          Load record into record buffer
*
            I = MRECSZ(TD) - BUFP
            DO 20 K = 1, I
               BUF(BUFP+K) = MBLOCK(MNBYTE(TD)+K, TD)
 20         CONTINUE
            NCHAR = MRECSZ(TD)
            MNBYTE(TD) = MNBYTE(TD) + I
*
         END IF
      END IF
*
      END
