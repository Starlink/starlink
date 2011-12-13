      SUBROUTINE FTS1_PHEAD( BFPNTR, RCPNTR, RECORD, MEDIUM, MD, LOC,
     :                       BLKSIZ, MAXHDR, REPORT, ACTSIZ, OFFSET,
     :                       CURREC, HSTART, HDNUM, EXTEND, NHEADS,
     :                       STATUS )
*+
*  Name:
*     FTS1_PHEAD

*  Purpose:
*     Processes the headers in a FITS file on tape or disk.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_PHEAD( BFPNTR, RCPNTR, RECORD, MEDIUM, MD, LOC, BLKSIZ,
*    :                 MAXHDR, REPORT, ACTSIZ, OFFSET, CURREC, HSTART,
*    :                 HDNUM, EXTEND, NHEADS, STATUS )

*  Description:
*     This routine reads selected files off a FITS tape or disk and
*     writes the header data into an HDS file, and they are reported to
*     the user.  The dimension of the HDS buffer array in the HDS
*     structure is enlarged if the buffer is filled.  The number of
*     bytes of header cards in the last data block (of the current
*     file) containing header data is returned.  This is to enable
*     other routines to know where the data array starts in that
*     buffer.  If this number equals the actual blocksize this means a
*     new data block needs to be read to access the data array.

*  Arguments:
*     BFPNTR = INTEGER  (Given)
*        Pointer to the BUFFER byte array
*     RCPNTR = INTEGER  (Given)
*        Pointer to the RECORD byte array
*     RECORD( 36 ) = CHARACTER * ( 80 ) (Given and Returned)
*        The buffer to hold the current FITS record.  Note that this
*        out of order for SGP/16, but it is needed here so that a
*        mapped array may be passed.  This is a temporary kludge until
*        the FITS readers are redesigned.
*     MEDIUM = CHARACTER * ( * ) (Given)
*        The medium containing the FITS file.  Currently supported is
*        'DISK' for a disk file.
*     MD = INTEGER (Given)
*        The tape or file descriptor depending on the value of %MEDIUM.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        The locator to the HDS object that will contain the header
*        cards.
*     BLKSIZ = INTEGER (Given)
*        The maximum blocksize and dimension of the data buffer.
*     MAXHDR = INTEGER (Given)
*        The maximum number of header sections in the current sub-file.
*     REPORT = LOGICAL (Given)
*        If true the header cards are reported to the user.
*     ACTSIZ = INTEGER (Given and Returned)
*        The actual block size (a multiple of the FITS record length of
*        2880 bytes).  It is only an input argument for
*        %MEDIUM = 'DISK'.
*     OFFSET = INTEGER (Given and Returned)
*        The number of bytes in the current block already interpreted
*        as the header plus any earlier sub files.
*     CURREC = LOGICAL (Given and Returned)
*        If true the current FITS record is to be used immediately, i.e.
*        it has alrady been read from tape or disk into %RECORD.  On
*        exit it is true when a new record has been read to determine
*        whether or not there is an extension, but no extension is
*        found.
*     HSTART( MAXHDR ) = INTEGER (Returned)
*        The number of the header card where each FITS header starts.
*     HDNUM( MAXHDR ) = INTEGER (Returned)
*        The number of header records processed and stored in the
*        output data structure, one per header section in the current
*        sub-file.
*     EXTEND = LOGICAL (Returned)
*        If true there are extensions in the FITS sub-file.
*     NHEADS = INTEGER (Returned)
*        The number of header sections in the sub-file.  This includes
*        dummy FITS header sections.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Initialise variables including number of the first card, total
*       number of cards, start and count of cards per header section
*     Find the size of the preallocated header area
*     If an error then
*        Report context and abort
*     Endif
*     If the block has been fully processed then
*        Read a new block from the tape or record from disk
*        If an error has occurred, report it and abort
*        Reset number of the card within block
*     Endif
*     Initialise loop flag
*     See if header belongs to an extension
*     For all cards
*        If the last card switch loop flag
*        If primary header see if extensions could follow
*        Increment number of header cards---total and per section
*        Report record to the user if requested
*        If preallocated object's size is too small then
*           Increase its size
*           If an error has occurred, report it and abort
*        Endif
*        Write card to the data structure
*        If an error has occurred, report it and abort
*        If the last card of the data block has been processed then
*           Read a new block from the tape or FITS record from the file
*           If an error has occurred, report it and abort
*           Reset number of card within block
*        Else
*           Increment number of card within block
*        Endif
*     Endfor
*     If there is space to store possible extensions then
*        Skip to the end of the record, incrementing the card counter
*        If the last card of the data block has been processed then
*           Read a new block from the tape or FITS record from the file
*           If an error has occurred, report it and abort
*           Set flag to indicate a new block has been read in which
*             there may be no extension
*           Reset number of card within block
*        Endif
*        If the first card indicates an extension header follows then
*           Increment header count and set extension-present flag
*           Initialise card count for the new header section and find
*             start card for the section
*           Switch new-block flag off as it contains an extension header
*           For all cards
*              If the last card switch loop flag
*              Increment number of header cards---total and per section
*              Report record to the user if requested
*                 If preallocated object's size is too small then
*                 Increase its size
*                 If an error has occurred, report it and abort
*              Endif
*              Write card to the data structure
*              If the last card of the data block has been processed
*                then
*                 Read a new block from the tape or FITS record from
*                   the file
*                 If an error has occurred, report it and abort
*                 Reset number of card within block
*              Else
*                 Increment number of card within block
*              Endif
*           Enddo
*        Else
*           More data to follow so exit from the loop
*        Endif
*     Endfor
*     Find number of bytes in the current block that are header
*       information, noting the special case when a new block has been
*       read, but does not begin with a header section.
*     Truncate the size of the header object
*     If an error has occurred report it
*     End

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 1987, 1988, 1989, 1990, 1992 Science & Engineering
*                   Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*                   Councils.
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
*     PMA: Peter Allan  (Manchester University)
*     MJC: Malcolm J. Currie (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1987 Jul 8 (PMA):
*        Original version.
*     1988 May   (MJC):
*        Tidied to KAPPA style, adding comments and error reports.
*     1988 Sep 19 (MJC):
*        Converted to blocked FITS tapes and reordered the arguments.
*     1988 Nov 3  (MJC):
*        Added report option and completed the description.
*     1989 Jul 21 (MJC):
*        Allowed for extensions.
*     1989 Nov 22 (MJC):
*        Reads OFFSET to determine starting card for sub files; allow
*        for tape buffer to start at an extension, i.e. it has followed
*        some data; and completed Method section .
*     1989 Nov 24 (MJC):
*        Change to use OFFSET=ACTSIZ meaning block is exhausted rather
*        than the counter-intuitive 0; fixed computation of number of
*        blank cards to skip; and corrected offset computation for the
*        case when an extension header was looked for, but not found.
*     1990 Jan 11 (MJC):
*        Moved misplaced parenthesis when input offset is non-zero.
*     1990 Oct 29 (MJC):
*        An additional block could be read if the END statement of an
*        extension occurs at the last line of a block due to a missing
*        logical check---now corrected.
*     1990 November 19 (MJC):
*        Renamed from FITSPH; HEADER, BUFSIZ arguments removed and
*        HSTART added, and converted to the SST prologue style.
*     1990 November 30 (MJC):
*        Added MEDIUM, CURREC and RECORD arguments; and permitted
*        disk-file access; altered to process FITS records rather than
*        blocks (needed for disk access but made consistent by doing the
*        same for tape).
*     1992 December (RDS):
*        Changed the argument order to help with portability.  Instead
*        of passing BUFFER in as a character array it now just gets the
*        pointer to the data. RECORD is still passed in a character
*        array but the pointer to the RECORD array is also passed in.
*        The first call to the Disc-FITS reading subroutine initialises
*        the record number in direct-access reads.
*     1996 July 25 (MJC):
*        Obtain the value of EXTEND correctly, i.e. not assume that it
*        is column 30.
*     1996 November 24 (MJC):
*        Revised FTS1_GKEYx calls.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     2010 April 19 (TIMJ):
*        No longer allow tape.
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER BFPNTR             ! Pointers to the buffer area
      INTEGER RCPNTR             ! Pointers to the record area
      CHARACTER * ( * ) MEDIUM   ! Data medium
      INTEGER MD                 ! Tape/file descriptor
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to header structure
      INTEGER BLKSIZ             ! The maximum allowed blocksize on
                                 ! the FITS tape/file
      INTEGER MAXHDR             ! Maximum number of header sections
      LOGICAL REPORT             ! The header information is to be
                                 ! reported



*  Arguments Given and Returned:
      CHARACTER * ( 80 ) RECORD( 36 ) ! A FITS record
      INTEGER ACTSIZ             ! Actual size of block read
      INTEGER OFFSET             ! The number of bytes of the last data
                                 ! block that contains header
                                 ! information
                                 ! Equal to ACTSIZ means the current
                                 ! block is wholly header
      LOGICAL CURREC             ! Use the current FITS record
                                 ! immediately?

*  Arguments Returned:
      INTEGER HSTART( MAXHDR )   ! Start card in each header section
      INTEGER HDNUM( MAXHDR )    ! No. of cards in each header section
      LOGICAL EXTEND             ! File contains extensions?
      INTEGER NHEADS             ! Number of header sections found in
                                 ! the sub-file

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NOCARD             ! Number of cards in a FITS record
      PARAMETER ( NOCARD = 36 )  ! 2880 / 80 characters

*  Local Variables:
      INTEGER CARD               ! Number of card image in current block
      CHARACTER * ( 48 ) COMENT  ! Comment from card (not used)
      INTEGER FCARD              ! Number of EXTEND card (not used)
      CHARACTER * ( DAT__SZLOC ) HD1LOC ! Locator for an individual
                                 ! header record
      LOGICAL LVALUE             ! Value of EXTEND keyword
      LOGICAL MORE               ! More header cards to come?
      LOGICAL MOREXT             ! More extensions may yet be read?
      INTEGER SIZE               ! Size of the preallocated header area
      INTEGER TCARD              ! Total number of header cards in the
                                 ! sub-file
      LOGICAL THERE              ! EXTEND keyword present in current
                                 ! header?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure the medium is permitted.
      IF ( MEDIUM .NE. 'DISK' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MEDIUM', MEDIUM )
         CALL ERR_REP( 'FTS1_PHEAD_MEDNAV',
     :     'Probable programming error.  ^MEDIUM is not available.',
     :     STATUS )
         GOTO 999
      END IF

*  A header section must start in a new record, so set the card number
*  to beyond the maximum number of cards in a record.  This will cause
*  a new record be read.  The exception is when the current record is
*  to be used.
      IF ( CURREC ) THEN
         CARD = 1
      ELSE
         CARD = 37
      END IF

*  Initialise some variables.
      MORE = .TRUE.
      EXTEND = .FALSE.
      CURREC = .FALSE.
      TCARD = 0

*  Define the start of the first extension in the sub-file, and
*  initialise the count of headers in the section.
      NHEADS = 1
      HDNUM( NHEADS ) = 0
      HSTART( NHEADS ) = 1

*  Find the size of the preallocated header area.
      CALL DAT_SIZE ( LOC, SIZE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_PHEAD_HDRSIZ',
     :     'Error determining the size of the header in the '/
     :     /'output data structure.', STATUS )
         GOTO 999
      END IF

*  See if the current header block has been processed fully and there
*  are more header cards to follow.
      IF ( CARD .GE. NOCARD .AND. MORE ) THEN

*  Read the disk file.
         CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .TRUE.,
     :                    %VAL( CNF_PVAL( BFPNTR ) ),
     :                    OFFSET, %VAL( CNF_PVAL( RCPNTR ) ),
     :                    STATUS )

*  Error reading the FITS file.  Report context and abort.
         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Back to the first card image as this is a new record.
         CARD = 1
      END IF

*  The first record may be an extension following some data in a simple
*  FITS header or another extension.
      IF ( RECORD( CARD )(1:8) .EQ. 'XTENSION' ) EXTEND = .TRUE.

      MORE = .TRUE.
      MOREXT = .FALSE.

*  Print out the descriptors and store them for later.
      DO WHILE ( MORE )

*  Have we reached the end of the header section?
         IF ( RECORD( CARD )(1:4) .EQ. 'END ' ) MORE = .FALSE.

*  Could an extension follow?  Note this will only apply to a basic
*  FITS header; an extension would not have this card, but ensure the
*  earlier setting of EXTEND=T will not be overturned.
         IF ( RECORD( CARD )(1:7) .EQ. 'EXTEND ' ) THEN

*  See if the current card is the EXTEND
            CALL FTS1_GKEYL( 1, RECORD( CARD ), CARD, 'EXTEND', 1,
     :                       THERE, LVALUE, COMENT, FCARD, STATUS )

            IF ( LVALUE .OR. EXTEND ) MOREXT = .TRUE.
         END IF

*  Keep a count on the number of cards in the header.
         HDNUM( NHEADS ) = HDNUM( NHEADS ) + 1

*  Increment the total number of cards.
         TCARD = TCARD + 1

*  Report the headers to the user.
         IF ( REPORT ) THEN
            CALL MSG_SETC ( 'DESCR', RECORD( CARD ) )
            CALL MSG_OUT ( 'HEADER', '^DESCR ', STATUS )
         END IF

*  Should the data structure be too small then increase its size.
         IF ( TCARD .GT. SIZE ) THEN
            SIZE = SIZE + 100
            CALL DAT_ALTER( LOC, 1, SIZE, STATUS )

*  Report the context of an error.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'FTS1_PHEAD_HDRSIZ',
     :           'Error increasing the size of the header in '/
     :           /'the output data structure.', STATUS )
               GOTO 999
            END IF
         END IF

*  Write the header card to the data structure for later use.
         CALL DAT_CELL ( LOC, 1, TCARD, HD1LOC, STATUS )
         CALL DAT_PUT0C ( HD1LOC, RECORD( CARD ), STATUS )
         CALL DAT_ANNUL ( HD1LOC, STATUS )

*  Report the context of the error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'FTS1_PHEAD_WRTHDR',
     :        'Error writing the header record into the '/
     :        /'output data structure.', STATUS )
            GOTO 999
         END IF

*  See if the current header block has been processed fully and
*  there are more header cards to follow.
         IF ( CARD .GE. NOCARD .AND. MORE ) THEN

*  Read the disk file.
            CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE.,
     :                       %VAL( CNF_PVAL( BFPNTR ) ), OFFSET,
     :                       %VAL( CNF_PVAL( RCPNTR ) ),
     :                       STATUS )

*  Error reading the FITS file.  Report context and abort.
            IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Back to the first card image as this is a new record.
            CARD = 1
         ELSE

*  Onto the next card in the record.
            CARD = CARD + 1
         END IF
      END DO

      MORE = .TRUE.

*  Could there be further extensions, and space to store them?
      DO WHILE ( NHEADS .LT. MAXHDR .AND. MOREXT )

*  Skip over trailing blank card images to the end of the record by
*  reading the next record.

*  Read the disk file.
         CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE.,
     :                    %VAL( CNF_PVAL( BFPNTR ) ), OFFSET,
     :                    %VAL( CNF_PVAL( RCPNTR ) ),
     :                    STATUS )

*  Error reading the FITS file.  Report context and abort.
         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Back to the first card image as this is a new record.
         CARD = 1

*  A new record has been read, so set flag to indicate this and that
*  here no extension header has yet been found within it.
         CURREC = .TRUE.

*  Look to see whether the data follow or an extension.
         IF ( RECORD( CARD ) ( 1:8 ) .EQ. 'XTENSION' ) THEN

*  One more header section in the sub-file.
            NHEADS = NHEADS + 1

*  Define the start of the next extension in the sub-file.
            HSTART( NHEADS ) = HDNUM( NHEADS - 1 ) + 1

*  Start the count of header cards in the new section.
            HDNUM( NHEADS ) = 0

*  The header section is an extension.
            EXTEND = .TRUE.

*  The new block has an extension.
            CURREC = .FALSE.

            MORE = .TRUE.
            DO WHILE ( MORE )

*  Have we reached the end of the header section?
               IF ( RECORD( CARD )(1:4) .EQ. 'END ' ) MORE = .FALSE.

*  Keep a count on the number of cards in the header.
               HDNUM( NHEADS ) = HDNUM( NHEADS ) + 1

*  Increment the total number of cards.
               TCARD = TCARD + 1

*  Report the extension headers to the user.
               IF ( REPORT ) THEN
                  CALL MSG_SETC ( 'DESCR', RECORD( CARD ) )
                  CALL MSG_OUT ( 'HEADER', '^DESCR ', STATUS )
               END IF

*  Should the data structure be too small then increase its size.
               IF ( TCARD .GT. SIZE ) THEN
                  SIZE = SIZE + 100
                  CALL DAT_ALTER( LOC, 1, SIZE, STATUS )

*  Report the context of an error.
                  IF ( STATUS .NE. SAI__OK ) THEN
                    CALL ERR_REP( 'FTS1_PHEAD_HDRSIZ',
     :                'Error increasing the size of the header in '/
     :                 /'the output data structure.', STATUS )
                     GOTO 999
                  END IF
               END IF

*  Write the header card to the data structure for later use.
               CALL DAT_CELL ( LOC, 1, TCARD, HD1LOC, STATUS )
               CALL DAT_PUT0C ( HD1LOC, RECORD( CARD ), STATUS )
               CALL DAT_ANNUL ( HD1LOC, STATUS )

*  Report the context of the error.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'FTS1_PHEAD_WRTHDR',
     :              'Error writing the header record into the '/
     :              /'output data structure.', STATUS )
                  GOTO 999
               END IF

*  See if the current header block has been processed fully.
               IF ( CARD .GE. NOCARD .AND. MORE ) THEN

*  Read the disk file.
                  CALL FTS1_DREAD( MD, BLKSIZ, ACTSIZ, .FALSE.,
     :                             %VAL( CNF_PVAL( BFPNTR ) ),
     :                             OFFSET,
     :                             %VAL( CNF_PVAL( RCPNTR ) ),
     :                             STATUS )

*  Error reading the FITS file.  Report context and abort.
                  IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Back to the first card image as this is a new record.
                  CARD = 1
               ELSE

*  Onto the next card in the record.
                  CARD = CARD + 1
               END IF
            END DO
         ELSE

*  The data follow, so exit from the loop.
            MOREXT = .FALSE.
         END IF
      END DO

*  Set the size of the header vector descriptor to the size of the data
*  within it.
      CALL DAT_ALTER ( LOC, 1, TCARD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_PHEAD_TRNHDR',
     :     'Error truncating the size of the header in the '/
     :     /'output data structure.', STATUS )
      END IF
      GOTO 999

*  Only come here following an error condition whilst reading the tape.
 980  CONTINUE

*  Error reading the tape.  Report context and abort.
      CALL MSG_SETI( 'HDNUM', HDNUM( NHEADS ) )
      CALL MSG_SETI( 'NHEADS', NHEADS )
      CALL ERR_REP( 'FTS1_PHEAD_READ',
     :  'Error while reading the header at line '/
     :  /'^HDNUM in header section ^NHEADS.', STATUS )

 999  CONTINUE

      END
