      SUBROUTINE FTS1_DREAD ( LU, BLKSIZ, ACTSIZ, LINIT, BUFFER,
     :                        OFFSET, RECORD, STATUS )
*+
*  Name:
*     FTS1_DREAD

*  Purpose:
*     Obtains a FITS record from a disk file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_DREAD ( LU, BLKSIZ, ACTSIZ, LINIT, BUFFER, OFFSET,
*    :                  RECORD, STATUS )

*  Description:
*     This routine reads the byte stream from the FITS disk file
*     and extracts a FITS record of 2880 bytes.  The blocksize of
*     the disk file is arbitrary save that it be no more than the
*     maximum FITS blocksize of 28800.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number for the disk-FITS file.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the file block.
*     ACTSIZ = INTEGER (Given and Returned)
*        The actual block size.  This need not be a multiple of the
*        FITS record length of 2880 bytes.  It must be known on input.
*     LINIT = LOGICAL (Given)
*        If true, the current record counter (RECNUM) is reset to one.
*        Should be true if a file is being read for the first time.
*     BUFFER( ACTSIZ ) = BYTE (Given and Returned)
*        The buffer containing the block of data. This is only read
*        when %OFFSET does not equal %ACTSIZ, i.e. there are some
*        non-header data within it.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted.
*     RECORD( 2880 ) = BYTE (Returned)
*        The current FITS record.  Successive calls will read of the
*        FITS records in sequence.
*     STATUS  = INTEGER(Given and Returned)
*        Global status value.

*  Algorithm:
*     There are two modes of operation: where the tape blocks are
*     smaller than the FITS record length of 2880 bytes, or where the
*     block length is greater or equal to the FITS record.
*     -  If the latter, copy a whole FITS record from the file provided
*     there are sufficient bytes remaining, incrementing the offset by
*     a record length.  Otherwise fill the record in two stages, the
*     second after reading a new file block.  The new offset is
*     calculated.
*     -  If the former, copy any remainder of a disk block to the FITS
*     record.  Find the number of whole disk blocks will fit in the
*     remainder of the FITS record.  Read this number of disk blocks
*     and copy them to the FITS record.  If the FITS record is not
*     complete read the next disk record, and fill the record. Finally,
*     compute the offset within the current disk block.

*  Prior requirements:
*     -  The disk file should already be open and the first block
*     read.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     {enter_new_authors_here}

*  History:
*     1990 November 30 (MJC):
*        Original version.
*     1992 December (RDS):
*        Modified for portability. It now finds the operating system
*        and then either makes a sequential access read if being run
*        on the VAX or otherwise a direct access read. The current
*        record number is stored as a local variable. This can be
*        reset to one by setting the input argument LINIT to true.
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  BLKSIZ,                ! The maximum allowed blocksize in the
                               ! FITS file
     :  LU                     ! Logical unit number

*  Arguments Given and Returned:
      INTEGER
     :  ACTSIZ,                ! The actual blocksizes on the FITS tape
     :  OFFSET                 ! The number of bytes of the input
                               ! block that must be skipped on input, so
                               ! on exit it is the number of bytes
                               ! copied from the current file block into
                               ! the FITS record
      LOGICAL
     :  LINIT                  ! Zero the record number counter ?

      BYTE
     :  BUFFER( BLKSIZ )       ! The input buffer from the file

*  Arguments Returned:
      BYTE
     :  RECORD( 2880 )         ! The FITS record

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      INTEGER
     :  DISP,                  ! Displacement pointer
     :  IOERR,                 ! Fortran I/O error number
     :  LEFT,                  ! Number of bytes remaining in the input
                               ! file block to be copied to the FITS
                               ! record
     :  I,                     ! Loop counter
     :  J,                     ! Loop counter
     :  NWDB                   ! Number of whole disk blocks

      CHARACTER
     :  MACHIN * ( 24 ),       ! Machine name
     :  NODE * ( 20 ),         ! Node name
     :  RELEAS * ( 10 ),       ! Release of operating system
     :  SYSNAM * ( 10 ),       ! Operating system
     :  VERSIO * ( 10 )        ! Sub-version of operating system

      LOGICAL
     :  OVMS                   ! Is the operating system VMS or RSX ?

*  UNIX requires the FITS file to be opened with 'Direct access'.  This
*  means that to keep track of which block is next we have to save a
*  local variable (RECNUM). If a new file is to be read, RECNUM needs
*  to be reset to one - this is specified by the LINIT argument
      INTEGER RECNUM
      SAVE RECNUM

*.

*    Check for an error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Reset the record counter to zero if reading a file for the
*    first time.
      IF ( LINIT ) RECNUM = 1

*    Find if the operating system is VMS/RSX or whether it is something
*    else.  It is not efficient to check this each time but it is
*    probably not a big overhead.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
      CALL CHR_UCASE( SYSNAM )

      OVMS = INDEX( SYSNAM, 'VMS' ) .NE. 0 .OR.
     :       INDEX( SYSNAM, 'RSX' ) .NE. 0

*    There are two modes of operation: where the tape blocks are smaller
*    than the FITS record length of 2880 bytes, or where the block
*    length is greater or equal to the FITS record.

*    File blocks are at least as long as the FITS record length.
*    ===========================================================

      IF ( ACTSIZ .GE. 2880 ) THEN

*       Find the number of bytes remaining in the file block.

         LEFT = ACTSIZ - OFFSET

*       If the displacement pointer is at the end of the file block a
*       new block of data is read from the file.

         IF ( LEFT .EQ. 0 ) THEN

            IF ( OVMS ) THEN
               READ( UNIT=LU, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
            ELSE
               READ( UNIT=LU, REC=RECNUM, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
               RECNUM = RECNUM + 1
            END IF

            IF ( IOERR .NE. 0 ) GOTO 999
            OFFSET = 0
            LEFT = ACTSIZ
         END IF

*       Are there sufficient bytes in the file block to fill a FITS
*       record?

         IF ( LEFT .GE. 2880 ) THEN

*          Yes. Copy part of the file block to the FITS record.

            DO  I = 1, 2880
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO

*          Update the offset.

            OFFSET = OFFSET + 2880

         ELSE

*          There were too few bytes.  Copy the remaining file bytes to
*          make a partially filled FITS record.

            DO  I = 1, LEFT
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO

*          Read a new file block to obtain the remainder of the bytes
*          belonging to the partially filled FITS record.

            IF ( OVMS ) THEN
               READ( UNIT=LU, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
            ELSE
               READ( UNIT=LU, REC=RECNUM, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
               RECNUM = RECNUM + 1
            END IF

            IF ( IOERR .NE. 0 ) GOTO 999

*          Fill the rest of the FITS record from the file buffer.

            DO  I = 1, 2880 - LEFT
               RECORD( I + LEFT ) = BUFFER( I )
            END DO

*          Update the offset.

            OFFSET = 2880 - LEFT

         END IF


*    File blocks are smaller than FITS record length.
*    ================================================

      ELSE

*       Find the number of bytes remaining in the file block.

         LEFT = ACTSIZ - OFFSET

*       Is there a partially used file block?

         IF ( LEFT .GT. 0 ) THEN

*          Yes. Copy the remainder of the file block to the FITS record.

            DO  I = 1, LEFT
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO
         END IF

*       Find how many whole disk blocks will fit into the remainder of
*       the FITS record.

         NWDB = ( 2879 - LEFT ) / ACTSIZ

*       Define the displacement within the FITS record.

         DISP = LEFT

*       Copy the disk blocks in turn.
*       =============================

         DO  J = 1, NWDB

*          Read in a disk block.

            IF ( OVMS ) THEN
               READ( UNIT=LU, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
            ELSE
               READ( UNIT=LU, REC=RECNUM, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
               RECNUM = RECNUM + 1
            END IF

            IF ( IOERR .NE. 0 ) GOTO 999

*          Copy the disk block into the FITS record.

            DO  I = 1, ACTSIZ
               RECORD( I + DISP ) = BUFFER( I )
            END DO

*          Increment the displacement within the FITS record.

            DISP = DISP + ACTSIZ
         END DO

*       Compute the offset within the final disk block that makes
*       up the FITS record.

         OFFSET = 2880 - DISP

*       Is the FITS record complete?

         IF ( OFFSET .GT. 0 ) THEN

*          Read the disk block that will used to fill the FITS record.

            IF ( OVMS ) THEN
               READ( UNIT=LU, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
            ELSE
               READ( UNIT=LU, REC=RECNUM, IOSTAT=IOERR )
     :                      ( BUFFER( I ), I = 1, ACTSIZ )
               RECNUM = RECNUM + 1
            END IF

            IF ( IOERR .NE. 0 ) GOTO 999

*          Copy the disk block into the FITS record.

            DO  I = 1, OFFSET
               RECORD( I + DISP ) = BUFFER( I )
            END DO
         END IF

*    End of the check for the mode.

      END IF

*    Report any error.

  999 CONTINUE
      IF ( IOERR .NE. 0 ) THEN
         CALL FIO_SERR( IOERR, STATUS )
         CALL MSG_SETI( 'UNIT', LU )
         CALL ERR_FIOER( 'MESSAGE', IOERR )
         CALL ERR_REP( 'FTS1_DREAD_READ',
     :     'Error reading FITS file on Fortran '/
     :     /'unit ^UNIT - ^MESSAGE.', STATUS )
      END IF

      END
