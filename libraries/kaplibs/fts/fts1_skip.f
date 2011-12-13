      SUBROUTINE FTS1_SKIP( MEDIUM, MD, SIZE, BPV, GCOUNT, PCOUNT,
     :                      BLKSIZ, ACTSIZ, BUFFER, OFFSET, RECORD,
     :                      RDISP, STATUS )
*+
*  Name:
*     FTS1_SKIP

*  Purpose:
*     Skips over the data of a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_SKIP( MEDIUM, MD, SIZE, BPV, GCOUNT, PCOUNT, BLKSIZ,
*    :                ACTSIZ, BUFFER, OFFSET, RECORD, RDISP, STATUS )

*  Description:
*     This routine skips over the data in a FITS file by reading the
*     data blocks from the FITS file.  The file may be basic FITS,
*     groups or an extension.

*  Arguments:
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported is
*        'DISK' for a disk file.
*     MD     = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     SIZE   = INTEGER (Given)
*        The number of elements in the data array.
*     BPV    = INTEGER (Given)
*        The number of bytes per data value.
*     GCOUNT = INTEGER (Given)
*        The number of groups in the file.
*     PCOUNT = INTEGER (Given)
*        The number of parameters per group in the file.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the tape buffer.
*     ACTSIZ = INTEGER (Given and Returned)
*        The actual block size (a multiple of the FITS record length of
*        2880 bytes).  It is only an input argument for
*        %MEDIUM = 'DISK'.
*     BUFFER( BLKSIZ ) = BYTE (Given and Returned)
*        The buffer containing the block of data. This is only read
*        when %OFFSET does not equal %ACTSIZ, i.e. there are some
*        non-header data within it.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted.
*     RDISP = INTEGER (Given and Returned)
*        The number of bytes in the current record already interpreted.
*        If this displacement is equal to the record length then a new
*        FITS record will be obtained.  The displacement will be updated
*        during processing of the group parameters and data, and
*        therefore can have an arbitrary value between 0 and 2880.  Do
*        not modify this argument outside this routine once initialised.
*     RECORD( 2880 ) = BYTE (Given and Returned)
*        The buffer to hold the current FITS record.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Compute number of bytes of data to be skipped
*     Initialise count of data-array bytes read
*     While not end of data
*        If buffer contains only previously processed information
*           Read a data block into the buffer
*           If an error occurred, report context and exit
*           Zero the offset
*        Endif
*        Find number of bytes in current block remaining to be skipped
*        Increment byte count
*        Sum offset of data within input buffer
*     End while
*     End

*  Copyright:
*     Copyright (C) 1989, 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1989 Nov 26 (MJC):
*        Original version.
*     1990 Jan 11 (MJC):
*        Offset on exit is now a multiple of the record length.
*     1990 November 20 (MJC):
*        Renamed from FITSKP, and converted to the SST prologue style.
*     1990 November 30 (MJC):
*        Added MEDIUM, RDISP and RECORD arguments; and permitted
*        disk-file access; altered to process FITS records rather than
*        blocks (needed for disk access but made consistent by doing the
*        same for tape).
*     1992 December (RDS):
*        Added extra argument to FTS1_DREAD calls.
*     2010 April 19 (TIMJ):
*        Remove TAPE option.
*     2010 August 22 (MJC):
*        Modern commenting style.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) MEDIUM
      INTEGER MD
      INTEGER SIZE
      INTEGER BPV
      INTEGER GCOUNT
      INTEGER PCOUNT
      INTEGER BLKSIZ

*  Arguments Given and Returned:
      INTEGER ACTSIZ
      BYTE BUFFER( BLKSIZ )
      INTEGER OFFSET
      BYTE RECORD( 2880 )
      INTEGER RDISP

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER RECLEN             ! FITS record length
      PARAMETER ( RECLEN = 2880 )

*  Local Variables:
      INTEGER ACTNBT             ! Number of bytes in input buffer
                                 ! actually skipped
      INTEGER DISP               ! Displacement pointer
      INTEGER NBT                ! Maximum number of bytes in input
                                 ! buffer to be skipped
      INTEGER NBYTES             ! Number of bytes in the file

*.

*  Check for an error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure the medium is permitted.
      IF ( MEDIUM .NE. 'DISK' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MEDIUM', MEDIUM )
         CALL ERR_REP( 'FTS1_SKIP_MEDNAV',
     :     'Probable programming error.  ^MEDIUM is not available.',
     :     STATUS )
         GOTO 999
      END IF

*  Calculate the number of bytes in the FITS file.
      NBYTES = BPV * GCOUNT * ( PCOUNT + SIZE )

*  Initialise the displacement pointer.
      DISP = 0

*  Read the blocks of data.
*  ========================
      DO WHILE ( DISP .LT. NBYTES )

*  Are there data already read in the last block of
*  header cards, waiting to be transferred to the array?
         IF ( RDISP .EQ. RECLEN ) THEN

*  No the buffer has been exhausted.

*  Read the disk file.
            CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE., BUFFER,
     :                       OFFSET, RECORD, STATUS )

*  Error reading the FITS file.  Report context and abort.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'DISP', DISP )
               CALL ERR_REP( 'FTS1_SKIP_READ',
     :           'Error reading the tape after ^DISP '/
     :           /'bytes read.', STATUS )
               GOTO 999
            END IF

*  Start of a new record so reset the record displacement.
            RDISP = 0

         END IF

*  Determine the number of bytes remaining in the current record.
         NBT = RECLEN - RDISP

*  Now the actual number to be transferred.
         ACTNBT = MIN( NBT, NBYTES - DISP )

*  Increment the displacement.
         DISP = DISP + ACTNBT

*  Compute the offset in the current record for any further data,
*  both within the primary data array and in any FITS groups.  If
*  this is equal to RECLEN further data will be in a new record.
         RDISP = RDISP + ACTNBT
      END DO

 999  CONTINUE

      END
