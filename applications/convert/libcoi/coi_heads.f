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
*     as HISTORY cards in the airlock.  Next some information
*     (application and time) concerning the conversion process are
*     appended to the airlock HISTORY cards.  Finally, an END card is
*     written.
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
*        records.  If .FALSE., any such headers are merely propagated to
*        the FITS airlock.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     Both the NDF and IRAF must be open, with the former having write
*     access.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 22 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

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

      INTEGER FITSHI             ! Length of a FITS HISTORY value
      PARAMETER ( FITSHI = 70 )

      INTEGER LENHIS             ! Length of IRAF image history area 
      PARAMETER ( LENHIS = 511 )

*  Local Variables:
      LOGICAL ADFITS             ! Are there FITS keywords not present
                                 ! in the IRAF image which must be
                                 ! added?
      INTEGER BITPIX             ! FITS BITPIX value
      CHARACTER * ( FITSLN ) CARD ! FITS card image
      INTEGER DIMS( NDF__MXDIM ) ! Length of axes
      INTEGER DTYPE              ! IRAF data-type code
      INTEGER EMPTY              ! Number of empty IRAF History lines
      INTEGER ERR                ! IRAF error indicator
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to the FITS extension
      INTEGER FIPNTR( 1 )        ! Pointer to FITS extension
      CHARACTER * ( FITSHI ) HISTRN ! Somewhere to put the whole
                                 ! history string of the IRAF image
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
*  The HISTORY area of an IRAF image is currently 511 characters long.
*  It contains several items detailing what has happened to an image,
*  e.g. `New copy of gal1' etc.  Each item is terminated by a newline,
*  while the whole history area is terminated by a SPP end-of-string
*  (EOS) character.  The NCHARS parameter reveals how many characters
*  are in the IRAF history area, currently (1992 October) this is a
*  maximum of 511.  Once the history area is full, later additions are
*  truncated or lost.
      CALL NHIST( IMDESC, NHISTL, NCHARS )
     
*  Check for empty lines.
      IF ( NHISTL .GT. 0 ) THEN
         EMPTY = 0
         DO 5 K = 1, NHISTL
            CARD = ' '
            CALL GETHIS( IMDESC, K, HISTRN )
            IF ( CHR_LEN( HISTRN ) .EQ. 0 ) EMPTY = EMPTY + 1
    5    CONTINUE

*  Derive the number of non-blank header lines
         NHISTL = NHISTL - EMPTY
      END IF

*  Check whether each history entry is properly terminated by a newline
*  character.  In the history area, each entry should be terminated
*  with a newline character.  We should never go more than FITSHI
*  characters without encountering a newline.  Any more than FITSHI
*  chars will not fit into a FITS standard HISTORY line.
      IF ( NCHARS .GT. FITSHI .OR. NHISTL .LE. 1 ) THEN
         NHISTL = 0
         CALL MSG_OUT( ' ', 'HISTORY parsing error.  The history '/
     :     /'information in the IRAF image is too long or not '/
     :     /'delimited by newlines.', STATUS )

*  Report the number of IRAF history records when verbose reporting is
*  switched on.
      ELSE
         CALL MSG_SETI( 'NHISTL', NHISTL )
         CALL MSG_OUTIF( MSG__VERB, ' ', 'Number of IRAF history '/
     :     /'lines is ^NHISTL', STATUS )
      END IF
     
*  Calculate the size of the FITS extension.
*  =========================================

*  Get the first header line to see if it contains
*  SIMPLE =        T  / Comment.....
*
*  Otherwise, will have to add it. The FITS extension should, after
*  all, contain lines conforming to the FITS standard. Only carried out if 
*  a header was found.
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
     :                   %VAL( FIPNTR( 1 ) ), STATUS, %VAL( FITSLN ) )

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

*  Only obtain the card if it has not already been rejected or used, say
*  for NDF HISTORY records.
         IF ( RETAIN( K - LINENO + 1 ) ) THEN

*  Initialise the card.
            CARD = ' '

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
     :                         %VAL( FIPNTR( 1 ) ), STATUS,
     :                         %VAL( FITSLN ) )

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
*      LINENO = LINENO + NHEAD - NREJEC - NSKIP
      LINENO = NFITSA

*  Append the history records in the FITS extension.
*  =================================================

*  Loop for each history line.
      DO K = LINENO, LINENO + NHISTL - 1

*  Initialise the card.
         CARD = ' '

*  Extract the history line.
         CALL GETHIS( IMDESC, K - LINENO + 1 , HISTRN )

*  Prefix the FITS HISTORY keyword.
         CALL CHR_MOVE( 'HISTORY', CARD( 1: ) )
         CALL CHR_MOVE( HISTRN, CARD( 10: ) )

*  Report the FITS HISTORY line in verbose-message mode.
         CALL MSG_SETC( 'CARD', CARD )      
         CALL MSG_OUTIF( MSG__VERB, ' ', '^CARD', STATUS )

*  Put it into the FITS extension.  Note again that the length of the
*  mapped character array is passed by value for UNIX.
         CALL CON_PCARD( CARD, K, XLINES, %VAL( FIPNTR( 1 ) ), STATUS,
     :                   %VAL( FITSLN ) )
      END DO

*  Update LINENO to point to the last element of the array.
      LINENO = LINENO + NHISTL

*  Insert a history record in the FITS extension for the conversion.
*  =================================================================

*  Add the history information from the IRAF image by adding a FITS-
*  format HISTORY line to the FITS extension to say where the image
*  came from.

*  Initialise the card.
      CARD = ' '

*  Copy information to a string
      CALL CHR_MOVE( 'HISTORY Image converted using STARLINK'/
     :  /' utility IRAF2NDF from the IRAF image', CARD ) 

*  Add the line to the image.
      CALL CON_PCARD( CARD, LINENO, XLINES, %VAL( FIPNTR( 1 ) ), STATUS,
     :                %VAL( FITSLN ) )

*  Report the HISTORY line in verbose-message mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', CARD, STATUS )
      
*  Increment the position in the FITS header.
      LINENO =  LINENO + 1
      
*  Initialise the line.
      CARD = ' '
      
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
      CALL CON_PCARD( CARD, LINENO, XLINES, %VAL( FIPNTR( 1 ) ), STATUS,
     :                %VAL( FITSLN ) )

*  Report the HISTORY line in verbose-message mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', CARD, STATUS )
      
*  Increment the position in the FITS header.
      LINENO =  LINENO + 1

*  Write the termination card image in the FITS extension.
*  =======================================================      

*  Initialise the line.
      CARD = ' '

*  Write the card.      
      CALL CHR_MOVE( 'END', CARD )

*  Add the line to the image.
      CALL CON_PCARD( CARD, LINENO, XLINES, %VAL( FIPNTR( 1 ) ), STATUS,
     :                %VAL( FITSLN ) )

*  Report the HISTORY line in verbose-message mode.
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
