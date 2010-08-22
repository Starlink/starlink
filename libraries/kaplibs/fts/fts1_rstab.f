      SUBROUTINE FTS1_RSTAB ( TABLE, MEDIUM, MD, PNTABL, TABNAM, AUTO,
     :                        AXIS1, AXIS2, BLKSIZ, ACTSIZ, BUFFER,
     :                        OFFSET, CURREC, RECORD, STATUS )
*+
*  Name:
*     FTS1_RSTAB

*  Purpose:
*     Reads the table format from a FITS file and write to a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_RSTAB ( TABLE, MEDIUM, MD, PNTABL, TABNAM, AUTO,
*                       AXIS1, AXIS2, BLKSIZ, ACTSIZ, BUFFER,
*                       OFFSET, CURREC, RECORD, STATUS )

*  Description:
*     This routine reads the data blocks from a FITS table-format tape
*     or disk file, and creates an ASCII file into which it writes the
*     table.

*  Arguments:
*     TABLE = CHARACTER * ( * ) (Returned)
*        The work buffer to store a line of the table, i.e. must be
*        AXIS1+1 bytes long.
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported is
*        'DISK' for a disk file.
*     MD     = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     PNTABL = CHARACTER * ( * ) (Given)
*        The parameter name used to create and associate the table
*        file.
*     TABNAM = CHARACTER * ( * ) (Given)
*        The suggested name of the table file, unless %AUTO is true
*        when it is the actual name of the table file to be created.
*     AUTO = LOGICAL (Given)
*        If true the supplied file name is used to open the table file
*        rather than obtaining the file name via the parameter system.
*     AXIS1 = INTEGER (Given)
*        Number of characters in a line of the table.
*     AXIS2 = INTEGER (Given)
*        The number of lines in the table.
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
*     CURREC = LOGICAL (Given and Returned)
*        If true the current FITS record is to be used immediately, i.e.
*        it has alrady been read from tape or disk into %RECORD.
*     RECORD( 2880 ) = BYTE (Given and Returned)
*        The buffer to hold the current FITS record.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Compute number of bytes of data to be read
*     Open the table file having suggested a name to the user
*     If an error occurred then abort
*     If the buffer contains only previously processed information
*        Read a new record
*        If an error occurred then report context and abort
*     Endif
*     Initialise count of data-array bytes read and other variables
*     While not end of data
*        Find number of bytes in current block remaining to be processed
*        Determine which elements of the data are to be copied to the
*          line of the table depending on whether it is complete, the
*          beginning or end of a line broken across a block.
*        Put the parts of the buffer containing data into the line of
*          the table
*        If the line in the table is complete, i.e. does not span blocks
*          then
*           Increment line count
*           Write line of the table to the file
*           If an error occurred report context and abort
*           Increment byte count
*           Reset offset of data within input buffer
*        Endif
*        If the buffer contains only previously processed information
*           Read a data block into the buffer
*           If an error occurred, report context and exit
*        Endif
*     Endwhile
*     End

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC:Malcolm J. Currie  (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1989 Aug  1 (MJC):
*        Original version.
*     1989 Nov 24 (MJC):
*        Change to use OFFSET=ACTSIZ meaning block is exhausted rather
*        than the counter-intuitive 0; bug fixes for lines spanning
*        FITS blocks.
*     1990 Jan  3 (MJC):
*        Previous change not completely implemented leaving a bug when
*        there are an integer number catalogue lines per tape record
*        --- offset incorrectly computed; the offset on exit is the end
*        of the current record, i.e. blank lines are ignored.
*     1990 Feb  7 (MJC):
*        Corrected yet another bug when table lines span FITS blocks.
*     1990 Feb 20 (MJC):
*        Added auto mode and extra parameter, AIF_OPFIO renamed
*        AIF_ASFIO, and added AIF_FLNAM call.
*     1990 November 19 (MJC):
*        Renamed from FITSRT, and converted to the SST prologue style.
*     1990 November 30 (MJC):
*        Added MEDIUM, CURREC and RECORD arguments; and permitted
*        disk-file access; altered to process FITS records rather than
*        blocks (needed for disk access but made consistent by doing the
*        same for tape).
*     1992 December (RDS):
*        Reordered arguments to put the mapped character array first.
*        Included extra argument in calls to FTS1_DREAD.
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
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Arguments Given:
      CHARACTER * ( * ) MEDIUM
      INTEGER MD
      CHARACTER * ( * ) PNTABL
      CHARACTER * ( * ) TABNAM
      LOGICAL AUTO
      INTEGER AXIS1
      INTEGER AXIS2
      INTEGER BLKSIZ

*  Arguments Given and Returned:
      INTEGER ACTSIZ
      BYTE BUFFER( BLKSIZ )
      INTEGER OFFSET
      LOGICAL CURREC
      BYTE RECORD( 2880 )

*  Arguments Returned:
      CHARACTER * ( * ) TABLE    ! Should appear last but one in API

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER RECLEN             ! FITS record length
      PARAMETER ( RECLEN = 2880 )

*  Local Variables:
      INTEGER DISP               ! Displacement pointer
      INTEGER FCH                ! Number of the first character in the
                                 ! table's line of data to be copied
                                 ! (for a line spanning two records)
      INTEGER FD                 ! Table file description
      INTEGER I                  ! Index
      INTEGER J                  ! Loop counter
      INTEGER NBT                ! Number of bytes in input buffer
                                 ! to be transferred to the data array
      INTEGER NBYTES             ! Number of bytes in data array
      INTEGER NCTC               ! Number of characters to copy from the
                                 ! record
      INTEGER NREC               ! Current number of the line in the table
      LOGICAL OPEN               ! Is output table file open?
      LOGICAL PRTIAL             ! Only part of the line of data has
                                 ! been copied from the record?
      INTEGER RDISP              ! The displacement within the current
                                 ! FITS record

*.

*  Check for an error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure the medium is permitted.
      IF ( MEDIUM .NE. 'DISK' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MEDIUM', MEDIUM )
         CALL ERR_REP( 'FTS1_SCTAB_MEDNAV',
     :     'Probable programming error.  ^MEDIUM is not available.',
     :     STATUS )
         GOTO 999
      END IF

*  The FITS data must start in a new record, so set the displacement
*  to the end of the FITS record.  This will cause a new record be
*  read.  The exception is when the current FITS record is to be used.
      IF ( CURREC ) THEN
         RDISP = 0
         CURREC = .FALSE.
      ELSE
         RDISP = RECLEN
      END IF

*  Calculate the number of bytes in the data.
      NBYTES = AXIS1 * AXIS2

      IF ( AUTO ) THEN

*  Open the table file, since its name is known.
         CALL AIF_OPFIO( TABNAM, 'WRITE', 'FORTRAN', AXIS1 + 1, FD,
     :                   OPEN, STATUS )

         IF ( STATUS .NE. SAI__OK .OR. .NOT. OPEN ) GOTO 980
      ELSE

*  Help the user because there is no dynamic default.
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_OUT( 'TABLEN', 'The name of the table file itself '/
     :     /'should be the name of the FACTS description', STATUS )
         CALL MSG_OUT( 'TABLEN1', 'file less the DSCF prefix.  The '/
     :     /'suggested name for the file containing the', STATUS )
         CALL MSG_SETC( 'TABLENAME', TABNAM )
         CALL MSG_OUT( 'TABLEN2', 'table is ^TABLENAME', STATUS )

*  Attempt to obtain and open a catalogue file.
         CALL FIO_ASSOC( PNTABL, 'WRITE', 'FORTRAN', AXIS1 + 1, FD,
     :                   STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            TABNAM = ' '
            CALL FIO_CANCL( PNTABL, STATUS )
            GOTO 999
         END IF

*  Obtain the file name associated with the parameter.
         CALL AIF_FLNAM( PNTABL, TABNAM, STATUS )
      END IF

      CALL MSG_SETC( 'TABLENAME', TABNAM )
      CALL MSG_OUT( 'TABLEINF', 'The table file is being '/
     :  /'written to ^TABLENAME', STATUS )

*  Are there data already read in the last block of header cards,
*  waiting to be transferred to the array?
      IF ( RDISP .EQ. RECLEN ) THEN

*  No, so must read another record.

*  Read the disk file.
         CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE., BUFFER,
     :                    OFFSET, RECORD, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'FTS1_RSTAB_READ1',
     :        'Error reading the first record containing the table.',
     :        STATUS )
            GOTO 980
         END IF

*  Start of a new record so reset the record displacement.
         RDISP = 0
      END IF

*  Initialise the displacement pointer.
      DISP = 0
      PRTIAL = .FALSE.
      FCH = 1
      NREC = 0

*  Read the blocks of data.
      DO WHILE ( DISP .LT. NBYTES )

*  Determine the maximum number of bytes of data in current
*  record to be transferred to the data array.
         NBT = RECLEN - RDISP

*  Determine which elements are to be copied.
         IF ( NBT .LT. AXIS1 ) THEN

*  This is the start of a broken line.
            PRTIAL = .TRUE.
            FCH = 1
            NCTC = NBT

         ELSE

*  This is the end of a broken line.
            IF ( PRTIAL ) THEN
               FCH = NCTC + 1
            ELSE
               FCH = 1
               TABLE( 1:1 ) = ' '
            END IF
            PRTIAL = .FALSE.
            NCTC = AXIS1 + 1 - FCH
         END IF

*  Copy the data from the FITS record to the character buffer.
         DO  J = FCH, NCTC + FCH - 1
            I = RECORD( J + 1 - FCH + RDISP )
            TABLE( J+1:J+1 ) = CHAR( I )
         END DO

*  The line is complete so write it to the catalogue.
         IF ( .NOT. PRTIAL ) THEN
            NREC = NREC + 1
            CALL FIO_WRITE( FD, TABLE, STATUS )

*  Report the error context.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'N', NREC )
               CALL ERR_REP( 'FTS1_RSTAB_WRTAB',
     :           'Error writing the table file at line ^N',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
               GOTO 980
            END IF

*  Keep count of the number of bytes of the catalogue read so far.
            DISP = DISP + AXIS1

*  Update the displacement in bytes within the current record.  The
*  "1 - FCH" term allows for broken lines.  It is zero normally.
            RDISP = MOD( RDISP + AXIS1 + 1 - FCH, RECLEN )
            IF ( RDISP .EQ. 0 ) RDISP = RECLEN
         ELSE

*  The remainder of the record is now at the start of the next line of
*  the table.  Therefore, the record is exhausted, and so the
*  displacement is adjusted so that a new record is read immediately.
            RDISP = RECLEN
         END IF

         IF ( NREC .LT. AXIS2 .AND. RDISP .EQ. RECLEN ) THEN

*  The current block is exhausted, so read in the next when there are
*  more lines of the table to read.

*  Read the disk file.
            CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE., BUFFER,
     :                       OFFSET, RECORD, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'DISP', DISP )
               CALL ERR_REP( 'FTS1_RSTAB_READ',
     :           'Error reading the file after ^DISP bytes read.',
     :           STATUS )
               GOTO 980
            END IF

*  Start of a new record so reset the record displacement.
            RDISP = 0

         END IF

      END DO

*  Close the table file.
  980 CONTINUE
      IF ( AUTO ) THEN
         CALL FIO_CLOSE( FD, STATUS )

*  Cancel the table parameter as we may be in a loop.
      ELSE
         CALL FIO_CANCL( PNTABL, STATUS )
      END IF

  999 CONTINUE

      END
