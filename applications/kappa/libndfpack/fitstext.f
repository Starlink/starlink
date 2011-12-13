      SUBROUTINE FITSTEXT( STATUS )
*+
*  Name:
*     FITSTEXT

*  Purpose:
*     Creates an NDF FITS extension from a text file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSTEXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes a version of a FITS header stored in a
*     text file, and inserts it into the FITS extension of an NDF.  The
*     header is not copied verbatim as some validation of the headers
*     as legal FITS occurs.  An existing FITS extension is removed.

*  Usage:
*     fitstext ndf file

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The name of the NDF to store the FITS header information.
*     FILE = FILENAME (Read)
*        The text file containing the FITS headers.  Each record should
*        be the standard 80-character `card image'.  If the file has
*        been edited care is needed to ensure that none of the cards
*        are wrapped onto a second line.

*  Examples:
*     fitstext hh73 headers.lis
*        This places the FITS headers stored in the text file called
*        headers.lis in the FITS extension of the NDF called hh73.

*  Notes:
*     - The validation process performs the following checks on each
*     header `card':
*       a) the length of the header is no more than 80 characters,
*       otherwise it is truncated;
*       b) the keyword only contains uppercase Latin alphabetic
*       characters, numbers, underscore, and hyphen (the header will
*       not be copied to the extension except when the invalid
*       characters are lowercase letters);
*       c) value cards have an equals sign in column 9 and a space in
*       column 10;
*       d) quotes enclose character values;
*       e) single quotes inside string values are doubled;
*       f) character values are left justified to column 11 (retaining
*       leading blanks) and contain at least 8 characters (padding with
*       spaces if necessary);
*       g) non-character values are right justified to column 30, except
*       for non-mandatory keywords which have a double-precision value
*       requiring more than 20 digits;
*       h) the comment delimiter is in column 32 or two characters
*       following the value, whichever is greater;
*       i) an equals sign in column 9 of a commentary card is replaced
*       by a space; and
*       j) comments begin at least two columns after the end of the
*       comment delimiter.
*
*     - The validation issues warning messages at the normal reporting
*     level for violations a), b), c), d), and i).
*
*     -  The validation can only go so far.  If any of your header lines
*     are ambiguous, the resulting entry in the FITS extension may not
*     be what you intended.  Therefore, you should inspect the
*     resulting FITS extension using the command FITSLIST before
*     exporting the data.  If there is something wrong, you may find it
*     convenient to use command FITSEDIT to make minor corrections.

*  Related Applications:
*     KAPPA: FITSEDIT, FITSEXP, FITSLIST; CONVERT: NDF2FITS.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 September 8 (MJC):
*        Original version.
*     1996 July 26 (MJC):
*        Uses improved validation routine.
*     4-JUN-1998 (DSB):
*        Corrected typo in final context error message.
*     8-JUL-2008 (DSB):
*        Make the file read buffer bigger to allow lines that are longer
*        than 80 chars to be read (e.g. lines padded with spaces).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'FIO_ERR'          ! Fortran I/O error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSLN             ! Length in characters of a FITS header
      PARAMETER ( FITSLN = 80 )

*  Local Variables:
      CHARACTER * 255 BUFFER     ! Buffer for reading the file
      CHARACTER * ( FITSLN ) CARD ! Reconsistituted header card
      INTEGER FD                 ! File descriptor
      INTEGER FDIM( 1 )          ! Dimension of the FITS extension
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to the FITS extension
      CHARACTER * ( DAT__SZLOC ) HLOC ! Locator to an element of the
                                 ! FITS extension
      INTEGER NCARD              ! Number of valid cards
      INTEGER NC                 ! Number of characters in record from
                                 ! the file
      INTEGER NDF                ! NDF identifier
      LOGICAL THERE              ! True when the FITS extension exists

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the text file.
      CALL FIO_ASSOC( 'FILE', 'READ', 'LIST', FITSLN, FD, STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Access the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Look for an existing FITS extension.
      THERE = .FALSE.
      CALL NDF_XSTAT( NDF, 'FITS', THERE, STATUS )

*  If there a FITS extension, delete it.
      IF ( THERE ) CALL NDF_XDEL( NDF, 'FITS', STATUS )

*  Create a new FITS extension with a reasonable initial size.  It will
*  be enlarged if necessary during reading of the text file, and
*  trimmed at the end.
      FDIM( 1 ) = 128
      CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, FDIM, FLOC, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the count of the number of header cards.
         NCARD = 0

*  Come here to read another line in the text file.
   10    CONTINUE

*  Read a line from the text file.
         CALL FIO_READ( FD, BUFFER, NC, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Validate the line, and attempt to extract the essence of the card,
*  returning a valid FITS header card when status is good.  An invalid
*  line will cause status to return bad.
            CALL FTS1_VHEAD( BUFFER, .TRUE., CARD, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Keep a tally of valid cards.
               NCARD = NCARD + 1

*  Should the data structure be too small then increase its size.
*  The increment is arbitrary, so have chosen four FITS records of 2880
*  bytes, which is also a multiple of 512.
               IF ( NCARD .GT. FDIM( 1 ) ) THEN
                  FDIM( 1 ) = FDIM( 1 ) + 128
                  CALL DAT_ALTER( FLOC, 1, FDIM( 1 ), STATUS )

*  Report the context of an error.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'FITSTEXT_HDRSIZ',
     :                 'Error enlarging the FITS extension.', STATUS )
                     GOTO 999
                  END IF
               END IF

*  Write the validated header card to the FITS extension.
               CALL DAT_CELL( FLOC, 1, NCARD, HLOC, STATUS )
               CALL DAT_PUT0C( HLOC, CARD, STATUS )
               CALL DAT_ANNUL( HLOC, STATUS )

*  Report the context of the error.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'FITSTEXT_WRTHDR',
     :              'Error writing the header record into the '/
     :             /'FITS extension.', STATUS )
               END IF

*  Report invalid cards.
            ELSE

*  Flush the error to continue.
               CALL ERR_FLUSH( STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END IF

*  Loop back to read another line from the text file.
         IF ( STATUS .EQ. SAI__OK ) THEN
            GOTO 10

*  Look for the end-of-file condition.
         ELSE IF ( STATUS .EQ. FIO__EOF ) THEN

*  This is the expected error condition that can be handled, so annul
*  the bad status.
            CALL ERR_ANNUL( STATUS )

*  Trim the FITS extension.
            FDIM( 1 ) = NCARD
            CALL DAT_ALTER( FLOC, 1, FDIM( 1 ), STATUS )
         END IF
      END IF

 999  CONTINUE

*  Tidy the NDF resources and annul the locator to the FITS extension.
      CALL DAT_ANNUL( FLOC, STATUS )
      CALL NDF_END( STATUS )

*  Close the text file.
      CALL FIO_CLOSE( FD, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSTEXT_ERR',
     :     'FITSTEXT: Unable to transfer the FITS header information '/
     :     /'from the text file to the NDF FITS extension.', STATUS )
      END IF

      END
