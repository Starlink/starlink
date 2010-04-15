      SUBROUTINE FTS1_TREAD ( FD, BLKSIZ, ACTSIZ, BUFFER, OFFSET,
     :                        RECORD, STATUS )
*+
*  Name:
*     FTS1_TREAD

*  Purpose:
*     Obtains a FITS record from a tape file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_TREAD ( FD, BLKSIZ, ACTSIZ, BUFFER, OFFSET, RECORD,
*    :                  STATUS )

*  Description:
*     This routine reads the byte stream from the FITS tape file
*     and extracts a FITS record of 2880 bytes.  The blocksize of
*     the tape file is arbitrary save that it be no more than the
*     maximum FITS blocksize of 28800.

*  Arguments:
*     FD = INTEGER (Given)
*        The tape descriptor.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the tape block.
*     ACTSIZ = INTEGER (Given and Returned)
*        The actual block size.  This need not be a multiple of the
*        FITS record length of 2880 bytes.  It must be known on input.
*     BUFFER( ACTSIZ ) = BYTE (Given and Returned)
*        The buffer containing the block of data. This is only read
*        when %OFFSET does not equal %ACTSIZ, i.e. there are some
*        non-header data within it.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted.
*     RECORD( 2880 ) = BYTE (Returned)
*        The current FITS record.  Successive calls will read of the
*        FITS records in sequence.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     There are two modes of operation: where the tape blocks are
*     smaller than the FITS record length of 2880 bytes, or where the
*     block length is greater or equal to the FITS record.
*     -  If the latter, copy a whole FITS record from the tape provided
*     there are sufficient bytes remaining, incrementing the offset by
*     a record length.  Otherwise fill the record in two stages, the
*     second after reading a new tape block.  The new offset is
*     calculated.
*     -  If the former, copy any remainder of a tape block to the FITS
*     record.  Find the number of whole tape blocks will fit in the
*     remainder of the FITS record.  Read this number of tape blocks
*     and copy them to the FITS record.  If the FITS record is not
*     complete read the next tape record, and fill the record. Finally,
*     compute the offset within the current tape block.

*  Prior requirements:
*     -  The tape file should already be open and the first block
*     read.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 November 30 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions


*  Arguments Given:
      INTEGER
     :  BLKSIZ,                ! The maximum allowed blocksize in the
                               ! FITS file
     :  FD                     ! Tape descriptor

*  Arguments Given and Returned:

      INTEGER
     :  ACTSIZ,                ! The actual blocksizes on the FITS tape
     :  OFFSET                 ! The number of bytes of the input
                               ! block that must be skipped on input, so
                               ! on exit it is the number of bytes
                               ! copied from the current tape block into
                               ! the FITS record

      BYTE
     :  BUFFER( BLKSIZ )       ! The input buffer from the tape

*  Arguments Returned:

      BYTE
     :  RECORD( 2880 )         ! The FITS record


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  DISP,                  ! Displacement pointer
     :  LEFT,                  ! Number of bytes remaining in the input
                               ! tape block to be copied to the FITS
                               ! record
     :  I,                     ! Loop counter
     :  J,                     ! Loop counter
     :  NWDB                   ! Number of whole tape blocks

*.


*    Check for error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    There are two modes of operation: where the tape blocks are smaller
*    than the FITS record length of 2880 bytes, or where the block
*    length is greater or equal to the FITS record.

*    Tape blocks are at least as long as the FITS record length.
*    ===========================================================

      IF ( ACTSIZ .GE. 2880 ) THEN

*       Find the number of bytes remaining in the tape block.

         LEFT = ACTSIZ - OFFSET

*       If the displacement pointer is at the end of the tape block a
*       new block of data is read from the tape.

         IF ( LEFT .EQ. 0 ) THEN
            CALL MAG_READ( FD, BLKSIZ, BUFFER, ACTSIZ, STATUS )
            OFFSET = 0
            LEFT = ACTSIZ
         END IF

*       Are there sufficient bytes in the tape block to fill a FITS
*       record?

         IF ( LEFT .GE. 2880 ) THEN

*          Yes. Copy part of the tape block to the FITS record.

            DO  I = 1, 2880
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO

*          Update the offset.

            OFFSET = OFFSET + 2880

         ELSE

*          There were too few bytes.  Copy the remaining tape bytes to
*          make a partially filled FITS record.

            DO  I = 1, LEFT
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO

*          Read a new tape block to obtain the remainder of the bytes
*          belonging to the partially filled FITS record.

            CALL MAG_READ( FD, BLKSIZ, BUFFER, ACTSIZ, STATUS )

*          Fill the rest of the FITS record from the tape buffer.

            DO  I = 1, 2880 - LEFT
               RECORD( I + LEFT ) = BUFFER( I )
            END DO

*          Update the offset.

            OFFSET = 2880 - LEFT

         END IF


*    tape blocks are smaller than FITS record length.
*    ================================================

      ELSE

*       Find the number of bytes remaining in the tape block.

         LEFT = ACTSIZ - OFFSET

*       Is there a partially used tape block?

         IF ( LEFT .GT. 0 ) THEN

*          Yes. Copy the remainder of the tape block to the FITS record.

            DO  I = 1, LEFT
               RECORD( I ) = BUFFER( I + OFFSET )
            END DO
         END IF

*       Find how many whole tape blocks will fit into the remainder of
*       the FITS record.

         NWDB = ( 2879 - LEFT ) / ACTSIZ

*       Define the displacement within the FITS record.

         DISP = LEFT

*       Copy the tape blocks in turn.
*       =============================

         DO  J = 1, NWDB

*          Read in a tape block.

            CALL MAG_READ( FD, BLKSIZ, BUFFER, ACTSIZ, STATUS )

*          Copy the tape block into the FITS record.

            DO  I = 1, ACTSIZ
               RECORD( I + DISP ) = BUFFER( I )
            END DO

*          Increment the displacement within the FITS record.

            DISP = DISP + ACTSIZ
         END DO

*       Compute the offset within the final tape block that makes
*       up the FITS record.

         OFFSET = 2880 - DISP

*       Is the FITS record complete?

         IF ( OFFSET .GT. 0 ) THEN

*          Read the tape block that will used to fill the FITS record.

            CALL MAG_READ( FD, BLKSIZ, BUFFER, ACTSIZ, STATUS )

*          Copy the tape block into the FITS record.

            DO  I = 1, OFFSET
               RECORD( I + DISP ) = BUFFER( I )
            END DO
         END IF

*    End of the check for the mode.

      END IF

      END
