      SUBROUTINE MIO_WRITE(TD, BUFSIZ, BUF, NCHAR, STATUS)
*+
*  Name:
*     MIO_WRITE

*  Purpose:
*     write magnetic tape record.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MIO_WRITE(TD, NVAL, VALUES, ACTVAL, STATUS)

*  Description:
*     Write a buffer to a tape.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     NVAL=INTEGER (Given)
*        Expression specifying the number of bytes to be written
*        to tape as a single record.
*     VALUES(NVAL)=BYTE (Given)
*        Array containing the data to be written to tape.
*     ACTVAL=INTEGER (Given)
*        Variable to receive the actual number of bytes read.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Check for a valid tape descriptor and that the tape is open.   If the
*     current block is not full the record is added to it.   Otherwise the
*     block is flushed and a new one started.

*  Copyright:
*     Copyright (C) 1980, 1983, 1984, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     Jon Fairclogh (RAL::IPMAF)
*     {enter_new_authors_here}

*  History:
*     06-Aug-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     22-Nov-1984: Rewritten to permit block spanning.(RAL::IPMAF)
*     15-Nov-1991: Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     17-Jan-1992: Changes to portable version by removing
*           use of lib$movc3 and lib$movc5
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     12-NOV-1996: Remove redundant definitions
*                  Use ICHAR to set character into byte (RAL::AJC)
*     {enter_further_changes_here}

*  Notes:
*     This is a completely portable version (but is probably
*     less efficient than the VMS version).

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
      INTEGER BUFSIZ            ! number of bytes to be written
      BYTE BUF(*)               ! Buffer containing data
      INTEGER NCHAR             ! number of bytes actually written

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
      INTEGER I                 ! Offset into block buffer
      INTEGER K                 ! Loop index
      INTEGER BSP               ! space left in record buffer
*.


C     print *,'mio_write:status,td,bufsiz',status,td,bufsiz
      NCHAR = 0
      IF ( STATUS.EQ.SAI__OK ) THEN
*
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
*
         IF ( STATUS.EQ.SAI__OK ) THEN
            IF ( CHR_SIMLR(MACMOD(TD),'READ') ) THEN
               STATUS = MIO__ILLAC
            ELSE
*             Get space left in buffer
               BSP = MBLKSZ(TD) - MNBYTE(TD)
*             Load bytes into remaining space
               I = MIN(BSP, MRECSZ(TD))
               DO 10 K = 1, I
                  MBLOCK(MNBYTE(TD)+K, TD) = BUF(K)
 10            CONTINUE
               MNBYTE(TD) = MNBYTE(TD) + I
*             If the buffer is full, write it out and initialise it with spaces
               IF ( MNBYTE(TD).GE.MBLKSZ(TD) ) THEN
                  CALL MIO_BWRIT(TD, MBLKSZ(TD), MBLOCK(1,TD), NWRIT,
     :                           STATUS)
                  MNBYTE(TD) = 0
                  I = MRECSZ(TD) - BSP
                  DO 15 K = 1, MBLKSZ(TD)
                     MBLOCK(K, TD) = ICHAR(' ')
 15               CONTINUE
                  IF ( BSP.LT.MRECSZ(TD) ) THEN
                     DO 16 K = 1, I
                        MBLOCK(MNBYTE(TD)+K, TD) = BUF(BSP+K)
 16                  CONTINUE
                  END IF
                  MNBYTE(TD) = MNBYTE(TD) + I
               END IF
            END IF
         END IF
      END IF

C      print *,'mio_write:status,nchar',status,nchar
      END
