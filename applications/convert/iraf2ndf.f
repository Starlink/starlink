      SUBROUTINE IRAF2NDF( STATUS )
*+
*  Name:
*     IRAF2NDF

*  Purpose:
*     Converts an IRAF image to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRAF2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an IRAF image to an NDF.  See the Notes
*     for details of the conversion.

*  Usage:
*     iraf2ndf in out

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        The name of the IRAF image.  Note that this excludes the
*        extension.
*     OUT = NDF (Write).
*        The name of the NDF to be produced.

*  Examples:
*     iraf2ndf ell_galaxy new_galaxy
*        Converts the IRAF image ell_galaxy (comprising files
*        ell_galaxy.imh and ell_galaxy.hdr) to an NDF called new_galaxy.

*  Notes:
*     The rules for the conversion are as follows:
*     -  The NDF data array is copied from the ".pix" file.
*     -  The title of the IRAF image (object i_title in the ".imh"
*     header file) becomes the NDF title.
*     -  Lines from the IRAF image header file are transferred to the
*     FITS extension of the NDF, any compulsory FITS keywords that are
*     missing are added.
*     -  If there is a FITS extension in the NDF, then the elements up
*     to the first END keyword of this are added to the `user area' of
*     the IRAF header file.
*     -  Lines from the HISTORY section of the IRAF image are also
*     extracted and added to the NDF's FITS extension as FITS HISTORY
*     lines.  Two extra HISTORY lines are added to record the original
*     name of the image and the date of the format conversion.

*  Related Applications:
*     NDF2IRAF

*  Pitfalls:
*     -  Bad pixels in the IRAF image are not replaced.
*     -  Some of the routines required for accessing the IRAF header
*     file are written in SPP. Macros are used to find the start of the
*     header line section, this constitutes an `Interface violation' as
*     these macros are not part of the IMFORT interface specification.
*     It is possible that these may be changed in the future, so
*     beware.

*  Implementation Status:
*     -  Only handles one-, two-, and three-dimensional IRAF files.
*     -  The NDF produced has type _WORD or _REAL corresponding to the
*     type of the IRAF image.  (The IRAF imfort FORTRAN subroutine
*     library only supports these data types: signed words and real.)
*     The pixel type of the image can be changed from within IRAF using
*     the 'chpixtype' task in the 'images' package.
*
*  Implementation Deficiencies:
*     -  Does not support wildcards.
*     -  There is no facility for taking an IRAF bad-pixel-mask file,
*     to set bad pixels in the NDF.

*  External Routines Used:
*     IRAF IMFORT subroutine library:
*        IMOPEN(), IMGSIZ(), IMGKWC(), IMCLOS(), IMEMSG()
*     Home grown routines:
*        LIN2MAP(), NLINES(), NHIST(), GETLIN(), PREFITS(),
*        PUTLIN(), GETHIS()

*  References:
*     -  IRAF User Handbook Volume 1A
*     A User's Guide to FORTRAN Programming in IRAF, The IMFORT
*     Interface, by Doug Tody

*  Machine-specific features used:
*     -  Linking
*     See MMS file or makefile

*  Keywords:
*     CONVERT, IRAF

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1992 (RAHM):
*        Original version.
*     23-NOV-1992 (RAHM):
*        Produces NDFs of type UWORD from IRAF short integer
*        type images.
*     9-JUL-1993 (RAHM):
*        Added VERBOSE parameter and tidied up.
*     1993 July 23 (MJC):
*        Reworked the prologue to a standard arrangement.  Tidied, made
*        to conform to SGP/16.  Made to work for cubes.  Fixed some
*        bugs.
*     1993 July 28 (MJC):
*        Removed the VERBOSE parameter.  This functionality should be
*        provided by a global parameter.
*     1993 September 30 (MJC):
*        Do not copy standard FITS headers already added to the FITS
*        extension.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'DAT_ERR'          ! HDS error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PSX_ERR'          ! PSX error constants
      INCLUDE 'CMP_ERR'          ! CMP error constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Finds the length of a string less
                                 ! trailuing blanks

*  Local Constants:
      INTEGER FITSLN             ! Length of a FITS header
      PARAMETER ( FITSLN = 80 )

      INTEGER FITSHI             ! Length of a FITS HISTORY value
      PARAMETER ( FITSHI = 70 )

      INTEGER LENHIS             ! Length of IRAF image history area 
      PARAMETER ( LENHIS = 511 )

      INTEGER NDIM               ! Maximum number of dimensions the 
      PARAMETER ( NDIM = 3 )     ! application can handle

      INTEGER STRLEN             ! Length of output title string.
      PARAMETER ( STRLEN = 80 )
        
*  Local Variables:
      INTEGER ACCESS             ! IRAF access mode
      LOGICAL ADFITS             ! Are there FITS keywords not present
                                 ! in the IRAF image which must be
                                 ! added?
      INTEGER BITPIX             ! FITS BITPIX value
      INTEGER ERR                ! IRAF error indicator
      CHARACTER * ( FITSLN ) CARD ! FITS card image.
      INTEGER DIMS( NDF__MXDIM ) ! Length of axes.
      INTEGER DTYPE              ! IRAF data-type code
      INTEGER EL                 ! Number of pixels in image
      INTEGER EMPTY              ! Number of empty IRAF History lines
      INTEGER FIPNTR( 1 )        ! Pointer to FITS extension
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to the FITS extension
      CHARACTER * ( FITSHI ) HISTRN ! Somewhere to put the whole
                                 ! history string of the IRAF image
      INTEGER IMDESC             ! Image descriptor returned by
                                 ! IMOPEN() for IMFORT routines
      CHARACTER * ( STRLEN ) IMERRM ! IMFORT error message text
      CHARACTER * ( STRLEN ) IRAFIL ! Input IRAF image name
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Type of the NDF
      INTEGER JUNKIT             ! Junk variable for unwanted numbers
      INTEGER K                  ! Loop counter
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF axes
      INTEGER LINENO             ! Line number
      INTEGER LIPNTR             ! Pointer to a line of pixels
      INTEGER MDIM               ! Number of axes
      INTEGER NAMLEN             ! Length of output message
      INTEGER NBANDS             ! Number of bands
      INTEGER NCHARS             ! Number of characters in the IRAF
                                 ! image's history string
      INTEGER NCOLS              ! Number of columns
      INTEGER NDF                ! NDF identifier of output NDF
      INTEGER NHDRLI             ! Number of IRAF header lines
      INTEGER NHISTL             ! Number of IRAF history lines
      INTEGER NREJEC             ! Number of rejected FITS cards
      INTEGER NROWS              ! Number of rows
      INTEGER NTICKS             ! Returned by PSX_TIME
      INTEGER PNTR( 1 )          ! Pointer to NDF data array
      CHARACTER * ( 132 ) TITLE  ! Title of the NDF 
      CHARACTER * ( 25 ) TIMEST  ! The time in an ascii string.
      INTEGER TSTRUCT            ! The time structure from psx_localtime
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF axes
      INTEGER XLINES             ! Number of NDF extension lines
      INTEGER XDIMS( 1 )         ! Number of dimensions in FITS
                                 ! extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the IRAF status.
      ERR = 0

*  Access the input IRAF image.
*  ============================

*  Get the name of the IRAF image.
      CALL PAR_GET0C( 'IN', IRAFIL, STATUS )
            
*  Convert file name to lower case.
*      CALL CHR_LCASE( IRAFIL )

*  Access mode is 1 for read only and 3 for read and write access.
      ACCESS = 1

*  Open the IRAF image
      CALL IMOPEN( IRAFIL, ACCESS, IMDESC, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Obtain the shape of the IRAF image.
*  ===================================

*  Obtain the dimensions and pixeltype of the IRAF image.
      CALL IMGSIZ( IMDESC, DIMS, MDIM, DTYPE, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Check the data type of the input image.  It must be real (6) or
*  signed word (3).
      IF ( DTYPE .NE. 6 .AND. DTYPE .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IF', IRAFIL )
         CALL ERR_REP( 'IRAF2NDF_DATATYPE',
     :     'IRAF2NDF: The data type of image ^IF.imh is not supported '/
     :     /'by the IRAF IMFORT subroutine library.  Use the IRAF '/
     :     /'task images.chpixtype to change the pixel type.', STATUS )
         GOTO 999
      END IF

*  Validate the number of dimensions.
      IF ( MDIM .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IF', IRAFIL )
         CALL MSG_SETI( 'MDIM', MDIM )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL ERR_REP( 'IRAF2NDF_DIMS',
     :     'IRAF2NDF: Cannot process image ^IF since it has ^MDIM '/
     :     /'dimensions.  The maximum permitted dimension is ^NDIM.',
     :     STATUS )
         GOTO 999
      END IF

*  Define the bounds.
      NCOLS = DIMS( 1 )
      NROWS = DIMS( 2 )
      NBANDS = DIMS( 3 )

      LBND( 1 ) = 1
      LBND( 2 ) = 1
      LBND( 3 ) = 1

      UBND( 1 ) = NCOLS
      UBND( 2 ) = NROWS
      UBND( 3 ) = NBANDS

*  Set the type of the NDF.
*  ========================

*  Only signed words or real is supported by IMFORT.  Match the NDF type
*  to the IRAF type.
      IF ( DTYPE .EQ. 3 ) THEN
         ITYPE = '_WORD'
      ELSE
         ITYPE = '_REAL'
      END IF

*  Create the output NDF.
*  ======================

*  Start an NDF context.
      CALL NDF_BEGIN
      
*  Create an NDF with the required dimensions and type.
      CALL NDF_CREAT( 'OUT', ITYPE, MDIM, LBND, UBND, NDF, STATUS )

*  Map the data component to a memory array
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Use PSX_CALLOC() to obtain dynamic storage for each line.
      CALL PSX_CALLOC( NCOLS, ITYPE, LIPNTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980
      
*  Pass the Image descriptor, image dimensions, line buffer,
*  dimensionality and the mapped array to a subroutine. Each line of
*  the IRAF image will be extracted and propagated to the NDF array.
      IF ( DTYPE .EQ. 3 ) THEN
         CALL CON_CI2DW( MDIM, NCOLS, NROWS, NBANDS, IMDESC, ERR,
     :                   %VAL( PNTR( 1 ) ), %VAL( LIPNTR ), ERR,
     :                   STATUS )
      ELSE
         CALL CON_CI2DR( MDIM, NCOLS, NROWS, NBANDS, IMDESC, ERR,
     :                   %VAL( PNTR( 1 ) ), %VAL( LIPNTR ), ERR,
     :                   STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 980
      
*  Get and validate header and history records.
*  ============================================

*  Call RAHM's SPP routine nlines.x (translated later to nlines.f or
*  nlines.for by the IRAF SPP compiler xc).  This routine tries to
*  discover the number of header lines in the image.  These can be
*  transferred directly to the FITS extension of the NDF.
      CALL NLINES( IMDESC, NHDRLI, ERR )
      CALL MSG_SETI( 'NH', NHDRLI )
      CALL MSG_OUTIF( MSG__VERB, ' ', 'There are ^NH header lines '/
     :  /'to propagate.', STATUS )

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
      EMPTY = 0
      DO 5 K = 1, NHISTL
         CARD = ' '
         CALL GETHIS( IMDESC, K, HISTRN )
         IF ( CHR_LEN( HISTRN ) .EQ. 0 ) EMPTY = EMPTY + 1
    5 CONTINUE

*  Derive the number of non-blank header lines, and report it when
*  verbose reporting is switched on.
      NHISTL = NHISTL - EMPTY
      CALL MSG_SETI( 'NHISTL', NHISTL )
      CALL MSG_OUTIF( MSG__VERB, ' ',
     :  'There are ^NHISTL history lines.', STATUS )
      
*  Check whether each history entry is properly terminated by a newline
*  character.  In the history area, each entry should be terminated
*  with a newline character.  We should never go more than FITSHI
*  characters without encountering a newline. Any more than FITSHI
*  chars will not fit into a FITS standard HISTORY line.
      IF ( NCHARS .GT. FITSHI .AND. NHISTL .LE. 1 ) THEN
         NHISTL = 0
         CALL MSG_OUT( ' ', 'HISTORY parsing error.  The HISTORY '/
     :     /'information in the IRAF image is too long or not '/
     :     /'delimited by newlines.', STATUS )
      ELSE
         CALL MSG_SETI( 'NHISTL', NHISTL )
         CALL MSG_OUTIF( MSG__VERB, ' ', 'Number of HISTORY lines '/
     :     /'in the header is ^NHISTL', STATUS )
      END IF

*  Calculate the size of the FITS extension.
*  =========================================

*  Get the first header line to see if it contains
*  SIMPLE =        T  / Comment.....
*
*  Otherwise, will have to add it. The FITS extension should, after
*  all, contain lines conforming to the FITS standard.
      CALL GETLIN( IMDESC, 1, CARD )

      ADFITS = .FALSE.
      IF ( INDEX( CARD( 1:8 ), 'SIMPLE' ) .EQ. 0 .OR.
     :     CARD( 30:30 ) .NE. 'T' ) THEN
         CALL MSG_OUTIF( MSG__VERB, ' ',
     :      'Inserting mandatory FITS keywords not present in the '/
     :      /'IRAF image into the NDF''s FITS extension.', STATUS )
         ADFITS = .TRUE.
      END IF

*  XLINES is the number of lines there will be in the final extension.
*  We always need one extra for the mandatory END card image, and two
*  for the extra history cards that say IRAF2NDF was used.  When the
*  mandatory card images are added there needs to be three lines for
*  SIMPLE, BITPIX, and NAXIS cards, and one card for each dimension in
*  MDIM (NAXISn keyword).  In practice some of the mandatory FITS
*  headers may be present, and the extension will need to be truncated
*  at the end.
      XLINES = NHDRLI + NHISTL + 2 + 1

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
      IF ( STATUS .NE. SAI__OK ) GOTO 980

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

*  Initialise the number of rejected header cards.
      NREJEC = 0

*  Loop for each line of the FITS header (after any mandatory keywords
*  have been written).
      DO K = LINENO, LINENO + NHDRLI - 1

*  Initialise the card.
         CARD = ' '

*  Extract each line in the user area.
         CALL GETLIN( IMDESC, K - LINENO + 1, CARD )

*  Do we need to write this card to the FITS extension?  No, if it is
*  the END card, as that will be written after the history cards; no,
*  if it is a mandatory keyword already written to the start of the
*  header.  Report FITS card image in verbose-message mode.
         IF ( CARD( 1:8 ) .NE. 'END     ' .AND. .NOT. ( ADFITS .AND.
     :        ( CARD( 1:6 ) .EQ. 'SIMPLE' .OR.
     :          CARD( 1:5 ) .EQ. 'NAXIS' .OR.
     :          CARD( 1:6 ) .EQ. 'BITPIX' ) )  ) THEN

            CALL MSG_SETC( 'CARD', CARD )
            CALL MSG_OUTIF( MSG__VERB, ' ', '^CARD', STATUS )

*  Put it into the FITS extension.  Note again that the length of the
*  mapped character array is passed by value for UNIX.
            CALL CON_PCARD( CARD, K - NREJEC, XLINES,
     :                      %VAL( FIPNTR( 1 ) ), STATUS,
     :                      %VAL( FITSLN ) )
         ELSE

*  Count the number of cards excluded.
*  array element after inserting the header lines.
             NREJEC = NREJEC + 1
         END IF

      END DO

*  Update the position in the FITS header to index the first empty
*  array element after inserting the header lines.
      LINENO = LINENO + NHDRLI - NREJEC


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
     :                 %VAL( FITSLN ) )

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
         CALL PSX_LOCALTIME( NTICKS, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       JUNKIT, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       TSTRUCT, STATUS )
         CALL PSX_ASCTIME( TSTRUCT, TIMEST, STATUS )

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
     :                 %VAL( FITSLN ) )

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
     :                 %VAL( FITSLN ) )

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
      CALL DAT_ANNUL( FITLOC, STATUS )

*  Write the IRAF image's title to the NDF title.
*  ==============================================      

*  Get the title of the IRAF image using imfort routine imgkwc.
      CALL IMGKWC( IMDESC, 'i_title', TITLE, ERR )

*  Ignore a bad status.  Just do not write a title.
      IF ( ERR .NE. 0 ) THEN
         ERR = 0

*  Write the title to the NDF.
      ELSE
         CALL NDF_CPUT( TITLE, NDF, 'TITLE', STATUS )
      END IF

*  Closedown sequence.
*  ===================

*  Unmap the NDF.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

 980  CONTINUE

*  Tidy the NDF context.
      CALL NDF_END( STATUS )

 999  CONTINUE

*  Close the IRAF image
      CALL IMCLOS(IMDESC, ERR )

*  Check for error from IMFORT, ie err is not equal to 0.
      IF ( ERR .NE. 0 ) THEN

*  Convert the IMFORT error status into the appropriate error text.
         CALL IMEMSG( ERR, IMERRM )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IERR', IMERRM )
         CALL ERR_REP( 'IRAF2NDF_IRAFERR',
     :      'IRAF2NDF: There has been an error reading the IRAF file. '/
     :      /'The error text is: "^IERR".  Check the pixel type as '/
     :      /'IRAF IMFORT subroutines can only deal with REAL or '/
     :      /'SHORT images', STATUS )
      END IF

*   Issue the standard error message if something has failed.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRAF2NDF_ERR',
     :     'IRAF2NDF: Unable to convert the IRAF image into an NDF.',
     :     STATUS )
      END IF

      END

