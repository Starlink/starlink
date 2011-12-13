      SUBROUTINE COI_HEADS( IMDESC, IRAFIL, NDF, NHEAD, RETAIN,
     :                      PROHIS, STATUS )
*+
*  Name:
*     COI_HEADS

*  Purpose:
*     Creates the FITS airlock in an NDF from IRAF headers and history.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_HEADS( IMDESC, IRAFIL, NDF, NHEAD, RETAIN, PROHIS,
*                     STATUS )

*  Description:
*     This routine creates a FITS airlock in the supplied NDF, and fills
*     it with IRAF OIF-format headers.  If the FITS mandatory headers
*     are missing, they are inserted prior to the copy.  Following the
*     headers, records from the IRAF history information are written
*     as HISTORY cards in the airlock (any records > 70 characters will
*     be truncated with ...).  Next some information (application and
*     time) concerning the conversion process are appended to the
*     airlock HISTORY cards.  Finally, an END card is written.
*
*     To prevent HISTORY headers that originated from an NDF's HISTORY
*     records from duplicating and multiplying in the airlock, and to
*     indicate any other headers we wish not to move to the airlock,
*     there is a logical array specifying the cards (in order) to copy.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     IRAFIL = CHARACTER * ( * ) (Given)
*        The name of the IRAF file.
*     NDF = INTEGER (Given)
*        The identifier of the output NDF.
*     NHEAD = INTEGER (Given)
*        Number of IRAF headers.
*     RETAIN( NHEAD ) = LOGICAL (Given)
*        Flags to indicate whether or not to propagate each IRAF header
*        line to the NDF's FITS airlock.
*     PROHIS = LOGICAL (Given)
*        If .TRUE., NDF-style HISTORY records are restored to HISTORY
*        records.  If .FALSE., any such headers are merely propagated
*        to the FITS airlock.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     Both the NDF and IRAF must be open, with the former having write
*     access.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 July 22 (MJC):
*        Original version.
*     1997 November 18 (AJC):
*        Remove checks for bad history records.
*          Last line needn't be terminated with newline
*          Overlong lines are truncated with ...
*        Blank lines are removed correctly.
*        Remove use of CHR_MOVE (obsolete).
*        Remove unnecessary initialisations of CARD.
*     1997 November 23 (MJC):
*        Propagate blank lines from the IRAF history records.  Wrap
*        comments at 72.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IMDESC
      CHARACTER * ( * ) IRAFIL
      INTEGER NDF
      INTEGER NHEAD
      LOGICAL PROHIS
      LOGICAL RETAIN( NHEAD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Finds the length of a string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER FITSLN             ! Length of a FITS header
      PARAMETER ( FITSLN = 80 )

*  Local Variables:
      LOGICAL ADFITS             ! Are there FITS keywords not present
                                 ! in the IRAF image which must be
                                 ! added?
      INTEGER BITPIX             ! FITS BITPIX value
      CHARACTER * ( FITSLN ) CARD ! FITS card image
      INTEGER DIMS( NDF__MXDIM ) ! Length of axes
      INTEGER DTYPE              ! IRAF data-type code
      INTEGER ERR                ! IRAF error indicator
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to the FITS extension
      INTEGER FIPNTR( 1 )        ! Pointer to FITS extension
      CHARACTER * ( FITSLN ) HISTRN ! Line from the IRAF image history
                                 ! area
      INTEGER K                  ! Loop counter
      INTEGER LINENO             ! Line number
      INTEGER MDIM               ! Number of axes
      INTEGER NAMLEN             ! Length of output message
      INTEGER NCHARS             ! Number of characters in the IRAF
                                 ! image's history string
      INTEGER NFITSA             ! Number of next entry in the FITS
                                 ! airlock
      INTEGER NHISTL             ! Number of IRAF history lines
      INTEGER NREJEC             ! Number of rejected FITS cards
      INTEGER NSKIP              ! Number of header lines excluded
      INTEGER NTICKS             ! Returned by PSX_TIME
      CHARACTER * ( 25 ) TIMEST  ! The time in an ASCII string
      INTEGER XDIMS( 1 )         ! Number of dimensions in FITS
                                 ! extension
      INTEGER XLINES             ! Number of NDF extension lines

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the dimensions and pixeltype of the IRAF image.
      CALL IMGSIZ( IMDESC, DIMS, MDIM, DTYPE, ERR )

*  Call RAHM's SPP routine nhist.x to discover the number of history
*  lines in the image.  These are records of what IRAF has done to this
*  particular file.  They become FITS HISTORY lines.
*
*  The HISTORY area of an IRAF image is a fixed number of characters
*  long (see imhdr.h SZ_IMHIST).  It contains several items detailing
*  what has happened to an image, e.g. `New copy of gal1' etc.  Each
*  item is terminated by a newline, while the whole history area is
*  terminated by a SPP end-of-string (EOS) character.  The NCHARS
*  parameter reveals how many characters are in the IRAF history area.
*  Once the history area is full, later additions are truncated or
*  lost.
      CALL NHIST( IMDESC, NHISTL, NCHARS )

*  Report the number of IRAF history records when verbose reporting is
*  switched on.
      CALL MSG_SETI( 'NHISTL', NHISTL )
      CALL MSG_OUTIF( MSG__VERB, ' ', 'Number of IRAF history '/
     :     /'lines is ^NHISTL', STATUS )

*  Calculate the size of the FITS extension.
*  =========================================

*  Get the first header line to see if it contains
*  SIMPLE =        T  / Comment.....
*
*  Otherwise, will have to add it.  The FITS extension should, after
*  all, contain lines conforming to the FITS standard.  Only carried
*  out if a header was found.
      CARD=' '
      IF ( NHEAD .GT. 0 ) CALL GETLIN( IMDESC, 1, CARD )
      ADFITS = .FALSE.
      IF ( INDEX( CARD( 1:8 ), 'SIMPLE' ) .EQ. 0 .OR.
     :     CARD( 30:30 ) .NE. 'T' ) THEN
         CALL MSG_OUTIF( MSG__VERB, ' ',
     :      'Inserting mandatory FITS keywords not present in the '/
     :      /'IRAF image into the NDF''s FITS extension.', STATUS )
         ADFITS = .TRUE.
      END IF

*  Count the number of header lines containing NDF-style HISTORY
*  records.
      NSKIP = 0
      DO K = 1, NHEAD
         IF ( .NOT. RETAIN( K ) ) NSKIP = NSKIP + 1
      END DO

*  XLINES is the number of lines there will be in the final extension.
*  We always need one extra for the mandatory END card image, and two
*  for the extra history cards that say IRAF2NDF was used.  When the
*  mandatory card images are added there needs to be three lines for
*  SIMPLE, BITPIX, and NAXIS cards, and one card for each dimension in
*  MDIM (NAXISn keyword).  In practice some of the mandatory FITS
*  headers may not be present, and the extension will need to be
*  truncated at the end.
      XLINES = NHEAD + NHISTL + 2 + 1 - NSKIP

      IF ( ADFITS ) XLINES = XLINES + 3 + MDIM

*  Create and map the FITS extension.
*  ==================================

*  Set the first member of an array to the dimensionality of the
*  80-character array required.
      XDIMS( 1 ) = XLINES

*  Create the NDF extension.
      CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, XDIMS, FITLOC, STATUS )

*  Map the array so that it can be passed to subroutines for filling.
      CALL DAT_MAPV( FITLOC, '_CHAR*80', 'WRITE', FIPNTR, XLINES,
     :               STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Fill the FITS extension with the user-area headers.
*  ===================================================

*  Need to ascertain the type of the IRAF image to know what value to
*  give the FITS BITPIX keyword.
      IF ( DTYPE .EQ. 3 ) THEN
         BITPIX = 16
      ELSE
         BITPIX = -32
      END IF

*  Write the mandatory FITS headers which appear at the start of the
*  header records.  Since the character array is mapped the length must
*  be passed for this to work under UNIX.  It has no effect under VMS.
      IF ( ADFITS ) THEN
         CALL CON_WFMAN( XLINES, MDIM, DIMS, BITPIX,
     :                   %VAL( CNF_PVAL( FIPNTR( 1 ) ) ), STATUS,
     :                   %VAL( CNF_CVAL( FITSLN ) ) )

         IF ( STATUS .NE. SAI__OK) GOTO 999

*  If the FITS stuff has been written, then next line is (4+MDIM).
         LINENO = 4 + MDIM
      ELSE

*  Otherwise start at the first line.
         LINENO = 1
      END IF

*  Initialise the number of rejected header cards, and the next free
*  element in the FITS airlock
      NREJEC = 0
      NFITSA = LINENO

*  Loop for each line of the FITS header (after any mandatory keywords
*  have been written).
      DO K = LINENO, LINENO + NHEAD - 1

*  Only obtain the card if it has not already been rejected or used,
*  say for NDF HISTORY records.
         IF ( RETAIN( K - LINENO + 1 ) ) THEN

*  Extract each line in the user area.
            CALL GETLIN( IMDESC, K - LINENO + 1, CARD )

*  Do we need to write this card to the FITS extension?  No, if it is
*  the END card, as that will be written after the history cards; no,
*  if it is a mandatory keyword already written to the start of the
*  header.  Also exclude any MWCS headers.  Report FITS card image in
*  verbose-message mode.
            IF ( CARD( 1:8 ) .NE. 'END     ' .AND. .NOT. ( ADFITS .AND.
     :         ( CARD( 1:6 ) .EQ. 'SIMPLE' .OR.
     :           CARD( 1:5 ) .EQ. 'NAXIS'  .OR.
     :           CARD( 1:6 ) .EQ. 'WCSDIM' .OR.
     :           CARD( 1:6 ) .EQ. 'DC-FLAG' .OR.
     :         ( CARD( 1:3 ) .EQ. 'WAT' .AND. CARD( 5:5 ) .EQ. '_') .OR.
     :           CARD( 1:6 ) .EQ. 'BITPIX' ) )  ) THEN

               CALL MSG_SETC( 'CARD', CARD )
               CALL MSG_OUTIF( MSG__VERB, ' ', '^CARD', STATUS )

*  Put it into the FITS extension.  Note again that the length of the
*  mapped character array is passed by value for UNIX.
               CALL CON_PCARD( CARD, NFITSA, XLINES,
     :                         %VAL( CNF_PVAL( FIPNTR( 1 ) ) ), STATUS,
     :                         %VAL( CNF_CVAL( FITSLN ) ) )

*  Keep a tally of the entries used in the FITS airlock.
               NFITSA = NFITSA + 1
            ELSE

*  Count the number of mandatory cards excluded.
               NREJEC = NREJEC + 1
            END IF

         END IF

      END DO

*  Update the position in the FITS header to index the first empty
*  array element after inserting the header lines.
      LINENO = NFITSA

*  Append the history records in the FITS extension.
*  =================================================

*  Loop for each history line.
      DO K = 1, NHISTL

*  Extract the history line.
         CALL GETHIS( IMDESC, K , HISTRN )
         NCHARS = CHR_LEN( HISTRN )

*  Construct the FITS HISTORY card.   Use an ellipsis to indicate where
*  the IRAF history record is too long to fit within a FITS card.
         CARD = 'HISTORY  ' // HISTRN( 1:FITSLN-10 )
         IF ( NCHARS .GT. FITSLN-10 ) CARD( 78: ) = '...'

*  Report the FITS HISTORY line in verbose-message mode.
         CALL MSG_SETC( 'CARD', CARD )
         CALL MSG_OUTIF( MSG__VERB, ' ', '^CARD', STATUS )

*  Put it into the FITS extension.  Note again that the length of the
*  mapped character array is passed by value for UNIX.
         CALL CON_PCARD( CARD, LINENO, XLINES,
     :                   %VAL( CNF_PVAL( FIPNTR( 1 ) ) ),
     :                   STATUS, %VAL( CNF_CVAL( FITSLN ) ) )
         LINENO = LINENO + 1
      END DO

*  Insert a history record in the FITS extension for the conversion.
*  =================================================================

*  Add the history information from the IRAF image by adding a FITS-
*  format HISTORY line to the FITS extension to say where the image
*  came from.

*  Copy information to a string
      CARD = 'HISTORY Image converted using STARLINK utility IRAF2NDF'/
     :        /' from the IRAF image'

*  Add the line to the image.
      CALL CON_PCARD( CARD, LINENO, XLINES,
     :                %VAL( CNF_PVAL( FIPNTR( 1 ) ) ), STATUS,
     :                %VAL( CNF_CVAL( FITSLN ) ) )

*  Report the HISTORY line in verbose-message mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', CARD, STATUS )

*  Increment the position in the FITS header.
      LINENO =  LINENO + 1

*  Find the current time count.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL MSG_SETC( 'IRAFIL', IRAFIL )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the date and time.
         CALL PSX_CTIME( NTICKS, TIMEST, STATUS )

*  Create the history text.
         CALL MSG_SETC( 'TIME', TIMEST )
         CALL MSG_LOAD( ' ', 'HISTORY ^IRAFIL.imh on ^TIME', CARD,
     :                  NAMLEN, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', 'HISTORY ^IRAF_NAME.', CARD, NAMLEN,
     :                  STATUS )
      END IF

*  Add the line to the image.
      CALL CON_PCARD( CARD, LINENO, XLINES,
     :                %VAL( CNF_PVAL( FIPNTR( 1 ) ) ), STATUS,
     :                %VAL( CNF_CVAL( FITSLN ) ) )

*  Report the HISTORY line in verbose-message mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', CARD, STATUS )

*  Increment the position in the FITS header.
      LINENO =  LINENO + 1

*  Write the termination card image in the FITS extension.
*  =======================================================

*  Write the card.
      CARD = 'END'

*  Add the line to the image.
      CALL CON_PCARD( CARD, LINENO, XLINES,
     :                %VAL( CNF_PVAL( FIPNTR( 1 ) ) ), STATUS,
     :                %VAL( CNF_CVAL( FITSLN ) ) )

*  Report the line in verbose-message mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', CARD, STATUS )

*  Tidy the extension.
*  ===================

*  Truncate the extension where necessary.  Note that the size argument
*  is a vector, and that the array must be unmapped first.
      IF ( LINENO .LT. XLINES ) THEN
         CALL DAT_UNMAP( FITLOC, STATUS )
         XDIMS( 1 ) = LINENO
         CALL DAT_ALTER( FITLOC, 1, XDIMS, STATUS )
      END IF

*  Annul the locator.
  999 CONTINUE
      CALL DAT_ANNUL( FITLOC, STATUS )

      END
