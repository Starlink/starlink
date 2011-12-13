      SUBROUTINE CON_PFITH( FD, FORM, LOC, MAXHDR, REPORT, HSTART,
     :                      HDNUM, EXTEND, NHEADS, STATUS )
*+
*  Name:
*     CON_PFITH

*  Purpose:
*     Process the headers in a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_PFITH( FD, FORM, LOC, MAXHDR, REPORT, HSTART, HDNUM,
*    :                EXTEND, NHEADS, STATUS )

*  Description:
*     This routine reads a FITS header from an ASCII or unformatted
*     file and writes the header data into an HDS file; the headers are
*     optionally reported to the user.  The dimension of the HDS buffer
*     array in the HDS structure is enlarged if the buffer is filled.

*  Arguments:
*     FD     = INTEGER (Given)
*        The descriptor of the file containing the FITS header.
*     FORM = CHARACTER * ( * ) (Given)
*        The format of the file.  FORM = 'FORMATTED' gives an ASCII
*        formatted file; anything else results in an unformatted file.
*     LOC    = CHARACTER * ( DAT__SZLOC ) (Given)
*        The locator to the HDS object that will contain the header
*        cards.
*     MAXHDR = INTEGER (Given)
*        The maximum number of header sections in the current sub-file.
*     REPORT = LOGICAL (Given)
*        If true the header cards are reported to the user.
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

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 17 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-sytem constants

*  Arguments Given:
      INTEGER FD                 ! File descriptor
      CHARACTER * ( * ) FORM     ! Format of the file
      INTEGER MAXHDR             ! Maximum number of header sections
      LOGICAL REPORT             ! The header information is to be
                                 ! reported, if true
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to header structure

*  Arguments Returned:
      LOGICAL EXTEND             ! True if, file contains extensions

      INTEGER HDNUM( MAXHDR )    ! Number of cards in each header
                                 ! section
      INTEGER HSTART( MAXHDR )   ! Start card in each header section
      INTEGER NHEADS             ! Number of header sections found in
                                 ! the sub-file

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ASCII              ! True if output file is formatted
      CHARACTER * ( 80 ) BUFFER  ! Accommodates header records
      CHARACTER * ( DAT__SZLOC ) HD1LOC ! Locators for individual
                                 ! header records
      INTEGER FIOSTA             ! Fortran I/O status
      INTEGER LUN                ! Fortran logical-unit number
      INTEGER NCHAR              ! Number of characters read from
                                 ! formatted-file record
      INTEGER SIZE               ! Size of the preallocated header area
      INTEGER TCARD              ! Total number of header cards in the
                                 ! sub-file

                                 ! True if:
      LOGICAL LAST               ! Previous card had an END keyword
      LOGICAL MORE               ! More header cards to come
      LOGICAL MOREXT             ! More extensions may yet be read

*.


*  Check for the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      MORE = .TRUE.
      EXTEND = .FALSE.
      TCARD = 0
      MOREXT = .FALSE.
      LAST = .FALSE.

*  Define the start of the first extension in the sub-file, and
*  initialise the count of headers in the section.
      NHEADS = 1
      HDNUM( NHEADS ) = 0
      HSTART( NHEADS ) = 1

*  Find the size of the preallocated header area.
      CALL DAT_SIZE ( LOC, SIZE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_PFITH_HDRSIZ',
     :     'Error determining the size of the header in the '/
     :     /'output data structure.', STATUS )
         GOTO 999
      END IF

*  Find file-access attributes.
*  ============================

*  Determine the file format.
      ASCII = FORM .EQ. 'FORMATTED'

*  Obtain the logical-unit number associated with the file.
      CALL FIO_UNIT( FD, LUN, STATUS )

*  Main loop.
*  ==========
*  ==========

*  Loop until the FITS-like header ends.
      DO WHILE ( MORE )

*  Read a record from the file.
*  ============================

*  Read the formatted file.
         IF ( ASCII ) THEN
            CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )
         ELSE

*  Read this header from the unformatted file.
            READ ( LUN, IOSTAT=FIOSTA ) BUFFER

*  Watch for an error.  There is one case where an error is likely, but
*  not important, and that is when reading the first data record
*  following the END card; it is possible to have shorter data records
*  that FITS records.  It is not possible to check for this condition
*  portably.  Therefore ignore any error when reading this record.  The
*  only danger here is that the END card is the termination of a dummy
*  header section, and that there is an error in the first record of
*  the next header section.  This is unlikely.  Further lengthen the
*  odds by checking for the start of an XTENSION keyword.
            IF ( FIOSTA .NE. 0 ) THEN
               IF ( .NOT. ( LAST .AND. BUFFER( 1:1 ) .NE. 'X' ) ) THEN
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error reading a '/
     :              /'header record from file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )
               END IF
            END IF
         END IF

*  Exit if something has gone wrong.
         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  See if this is the end of the header.
*  =====================================

*  Test whether there is an extension following an END.  The EXTEND=T
*  header must be present.
         IF ( LAST ) THEN
            IF ( BUFFER ( 1:8 ) .EQ. 'XTENSION' .AND. MOREXT ) THEN

*  There is another header section in the full header.
               NHEADS = NHEADS + 1

*  Define the start of the next extension in the sub-file.
               HSTART( NHEADS ) = HDNUM( NHEADS - 1 ) + 1

*  Start the count of header cards in the new section.
               HDNUM( NHEADS ) = 0

*  The header section is an extension.
               EXTEND = .TRUE.

*  Since there is another header section so switch off the flag
*  indicating the end.
               LAST = .FALSE.
            ELSE

*  This really is the end of the line so set the flag indicating that
*  more headers should be read, and backspace the file, so that the
*  data array is ready to be read.
               MORE = .FALSE.
               BACKSPACE( LUN )
            END IF

*  Test for certain important cards.
*  =================================
         ELSE

*  Have we reached the end of the header section?  Only need test this
*  when the flag is false.
            IF ( BUFFER(1:4) .EQ. 'END ' ) LAST = .TRUE.

*  Could an extension follow?  Note this will only apply to a
*  basic FITS header; an extension would not have this card, but
*  ensure the earlier setting of EXTEND=T will not be overturned.
            IF ( BUFFER(1:7) .EQ. 'EXTEND ' .AND.
     :           BUFFER(30:30) .EQ. 'T' .AND. .NOT. EXTEND )
     :        MOREXT = .TRUE.
         END IF

*  Output and count the header.
*  ============================

*  Check to see if the record is part of the header to be stored,
*  reported and counted.  In other words has an END statement not
*  followed by an XTENSION card been encountered?
         IF ( MORE ) THEN

*  Keep a count on the number of cards in the header.
            HDNUM( NHEADS ) = HDNUM( NHEADS ) + 1

*  Increment the total number of cards.
            TCARD = TCARD + 1

*  Report the headers to the user.
            IF ( REPORT ) THEN
               CALL MSG_SETC ( 'DESCR', BUFFER )
               CALL MSG_OUT ( 'HEADER', '^DESCR ', STATUS )
            END IF

*  Should the data structure be too small then increase its size.
            IF ( TCARD .GT. SIZE ) THEN
               SIZE = SIZE + 100
               CALL DAT_ALTER( LOC, 1, SIZE, STATUS )

*  Report the context of an error.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'CON_PFITH_HDRSIZ',
     :              'Error increasing the size of the header in '/
     :              /'the output data structure.', STATUS )
                  GOTO 999
               END IF
            END IF

*  Write the header card to the data structure for later use.
            CALL DAT_CELL ( LOC, 1, TCARD, HD1LOC, STATUS )
            CALL DAT_PUT0C ( HD1LOC, BUFFER, STATUS )
            CALL DAT_ANNUL ( HD1LOC, STATUS )

*  Report the context of the error.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'CON_PFITH_WRTHDR',
     :           'Error writing the header record into the '/
     :           /'output data structure.', STATUS )
               GOTO 999
            END IF
         END IF
      END DO

*  Set the size of the header vector descriptor to the size of the data
*  within it.
      CALL DAT_ALTER ( LOC, 1, TCARD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_PFITH_TRNHDR',
     :     'Error truncating the size of the header in the '/
     :     /'output data structure.', STATUS )
      END IF
      GOTO 999

*  Only come here following an error condition whilst reading the file.
 980  CONTINUE

*  Error reading the file.  Report context and abort.
      CALL MSG_SETI( 'HDNUM', HDNUM( NHEADS ) )
      CALL MSG_SETI( 'NHEADS', NHEADS )
      CALL ERR_REP( 'CON_PFITH_READ',
     :  'Error while reading the header at line '/
     :  /'^HDNUM in header section ^NHEADS.', STATUS )

 999  CONTINUE

      END
