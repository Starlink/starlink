      SUBROUTINE FTS1_RGRDA ( MEDIUM, MD, SIZE, BPV, REVERS, PCOUNT,
     :                        BLKSIZ, ACTSIZ, BUFFER, OFFSET, RECORD,
     :                        RDISP, PARAM, DARRAY, STATUS )
*+
*  Name:
*     FTS1_RGRDA

*  Purpose:
*     Reads the data of a FITS file in group format from disk or tape.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_RGRDA ( MEDIUM, MD, SIZE, BPV, REVERS, PCOUNT, BLKSIZ,
*    :                  ACTSIZ, BUFFER, OFFSET, RECORD, RDISP, PARAM,
*    :                  DARRAY, STATUS )

*  Description:
*     This routine reads a data block from the FITS tape or disk file
*     that has byte stream of data in the groups format, and writes the
*     data into an array.  The values of the parameters associated with
*     the data array are also obtained.  The bytes may be reversed for
*     VAX/VMS.

*  Arguments:
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported is
*        'DISK' for a disk file.
*     MD     = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     SIZE   = INTEGER (Given)
*        Number of elements in the data array.  Note this is not the
*        same as the number of bytes.
*     BPV    = INTEGER (Given)
*        The number of bytes per data value.
*     REVERS = LOGICAL (Given)
*        If true the FITS data bytes are to be reversed within each
*        word (when BPV is 2) or each integer (when BPV is 4) etc.  If
*        BPV=1 this flags makes no difference.  Normally, only 2's
*        complement integer data need be reversed.  Floating-point data
*        require adjacent bytes to be swapped.  Note that the group
*        parameters are by definition in 2's complement integers, so are
*        reversed regardless of the sense of this flag.
*     PCOUNT    = INTEGER (Given)
*        The number of parameters associated with the data array.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the tape/disk buffer.
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
*     RECORD( 2880 ) = BYTE (Given and Returned)
*        The buffer to hold the current FITS record.
*     RDISP = INTEGER (Given and Returned)
*        The number of bytes in the current record already interpreted.
*        If this displacement is equal to the record length then a new
*        FITS record will be obtained.  The displacement will be updated
*        during processing of the group parameters and data, and
*        therefore can have an arbitrary value between 0 and 2880.  Do
*        not modify this argument outside this routine once initialised.
*     PARAM( PCOUNT * BPV ) = BYTE (Returned)
*        The parameters associated with the data array.
*     DARRAY( SIZE * BPV ) = BYTE (Returned)
*        The data array.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Compute number of bytes of parameters to be read
*     Initialise count of parameter bytes read
*     While not end of parameter data
*        If buffer contains only previously processed information
*           Read a data block into the buffer
*           If an error occurred, report context and exit
*           Zero the offset
*        Endif
*        Find number of bytes in current block remaining to be processed
*        Put the parts of the buffer containing parameters into the
*          parameter array
*        Increment byte count
*        Increment offset of data within input buffer
*     End while
*     Compute number of bytes of data to be read
*     Initialise count of data-array bytes read
*     While not end of data
*        If buffer contains only previously processed information
*           Read a data block into the buffer
*           If an error occurred, report context and exit
*           Zero the offset
*        Endif
*        Find number of bytes in current block remaining to be processed
*        Put the parts of the buffer containing parameters into the
*          data array
*        Increment byte count
*        Increment offset of data within input buffer
*     End while
*     End

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering
*     Research Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1988 Sep 22 (MJC):
*        Original version.
*     1989 Jul 17 (MJC):
*        Fixed a bug that computed the offset number bytes on exit
*        erroneously.
*     1989 Nov 24 (MJC):
*        Change to use OFFSET=ACTSIZ meaning block is exhausted rather
*        than the counter-intuitive 0, now has only one offset counter.
*     1990 November 19 (MJC):
*        Renamed from FITSRG, and converted to the SST prologue style.
*     1990 November 30 (MJC):
*        Added MEDIUM, RDISP and RECORD arguments; and permitted
*        disk-file access; altered to process FITS records rather than
*        blocks (needed for disk access but made consistent by doing the
*        same for tape).
*     1991 Jul 11 (MJC):
*        Added REVERS argument so that IEEE data are not reversed.
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
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) MEDIUM
      INTEGER MD
      INTEGER SIZE
      INTEGER BPV
      LOGICAL REVERS
      INTEGER PCOUNT
      INTEGER BLKSIZ

*  Arguments Given and Returned:
      INTEGER ACTSIZ
      BYTE BUFFER( BLKSIZ )
      INTEGER OFFSET
      BYTE RECORD( 2880 )
      INTEGER RDISP

*  Arguments Returned:
      BYTE PARAM( * )
      BYTE DARRAY( * )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER RECLEN             ! FITS record length
      PARAMETER ( RECLEN = 2880 )

*  Local Variables:
      INTEGER ACTNBT             ! Number of bytes in input buffer
                                 ! actually transferred to the data array
      INTEGER DISP               ! Displacement pointer
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER NBT                ! Number of bytes left in the current
                                 ! record yet to be transferred to the
                                 ! data array or parameters
      INTEGER NBYTES             ! Number of bytes in data array or
                                 ! parameters

*.

*  Check for an error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure the medium is permitted.
      IF ( MEDIUM .NE. 'DISK' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MEDIUM', MEDIUM )
         CALL ERR_REP( 'FTS1_RGRDA_MEDNAV',
     :     'Probable programming error.  ^MEDIUM is not available.',
     :     STATUS )
         GOTO 999
      END IF

*  Calculate the number of bytes in the parameters associated with
*  the data array.
      NBYTES = BPV * PCOUNT

*  Initialise the displacement pointer.
      DISP = 0

*  Read the blocks of data.
*  ========================
      DO WHILE ( DISP .LT. NBYTES )

*  Are there data already read in the last block of header cards,
*  waiting to be transferred to the parameter array?
         IF ( RDISP .EQ. RECLEN ) THEN

*  No the buffer has been exhausted.

*  Read the disk file.
            CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE., BUFFER,
     :                       OFFSET, RECORD, STATUS )

*  Error reading the FITS file.  Report context and abort.
            IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Start of a new record so reset the record displacement.
            RDISP = 0
         END IF

*  Determine the number of bytes remaining in the current record.
         NBT = RECLEN - RDISP

*  Now the actual number to be transferred.
         ACTNBT = MIN( NBT, NBYTES - DISP )

*  Copy the data from the record to the data array, swapping bytes
*  where requested since a Vax stores bytes in the reverse order
*  to what is in the FITS file.
         IF ( REVERS ) THEN
            DO I = 1, ACTNBT, BPV
               DO J = 1, BPV
                  PARAM( DISP + I + J - 1 ) = RECORD( I + BPV - J +
     :                                                RDISP )
               END DO
            END DO
         ELSE
            DO I = 1, ACTNBT
               PARAM( DISP + I ) = RECORD( I + RDISP )
            END DO
         END IF
         DISP = DISP + ACTNBT

*  Compute the offset in the current record for the group sub-array.
*  If this is equal to RECLEN further data will be in a new record.
         RDISP = RDISP + ACTNBT

      END DO

*  Calculate the number of bytes in the data.
      NBYTES = BPV * SIZE

*  Initialise the displacement pointer.
      DISP = 0

*  Read the blocks of data.
*  ========================
      DO WHILE ( DISP .LT. NBYTES )

*  Are there data already read in the last block of header cards,
*  waiting to be transferred to the array?
         IF ( RDISP .EQ. RECLEN ) THEN

*  No the buffer has been exhausted.

*  Read the disk file.
            CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE., BUFFER,
     :                       OFFSET, RECORD, STATUS )

*  Error reading the FITS file.  Report context and abort.
            IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Start of a new record so reset the record displacement.
            RDISP = 0
         END IF

*  Determine the number of bytes remaining in the current record.
         NBT = RECLEN - RDISP

*  Now the actual number to be transferred.
         ACTNBT = MIN( NBT, NBYTES - DISP )

*  Copy the data from the record to the data array, swapping bytes
*  where requested since a Vax stores bytes in the reverse order
*  to what is in the FITS file.
         IF ( REVERS ) THEN
            DO I = 1, ACTNBT, BPV
               DO J = 1, BPV
                  DARRAY( DISP + I + J - 1 ) = RECORD( I + BPV - J +
     :                                                 RDISP )
               END DO
            END DO
         ELSE
            DO I = 1, ACTNBT
               DARRAY( DISP + I ) = RECORD( I + RDISP )
            END DO
         END IF
         DISP = DISP + ACTNBT

*  Compute the offset in the current record for any further group-format
*  arrays and their parameters.  If this is equal to RECLEN further data
*  will be in a new record.
         RDISP = RDISP + ACTNBT
      END DO

      GOTO 999

 980  CONTINUE
      CALL MSG_SETI( 'DISP', DISP )
      CALL ERR_REP( 'FTS1_RGRDA_READ',
     :  'Error reading the file after ^DISP bytes read.', STATUS )

 999  CONTINUE

      END
