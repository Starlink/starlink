      SUBROUTINE CON_WFMAN( EL, NDIM, DIMS, BITPIX, FITSAR, STATUS )
*+
*  Name:
*     CON_WFMAN

*  Purpose:
*     Writes the mandatory FITS header card images to the start of a
*     character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_WFMAN( EL, NDIM, DIMS, BITPIX, FITSAR, STATUS )

*  Description:
*     Adds 3, 80 character lines of text to the start of the FITSAR
*     array. The format of the lines is that required by the FITS
*     starndard.
*     Adds to the start of array FITSAR, the three lines required
*     by the FITS standard to start every FITS file. Namely the
*     SIMPLE= T, BITPIX= , and NAXIS = lines.

*  Arguments:
*     EL = INTEGER (Given)
*        The size of FITSAR.  It must be at least NDIM + 4.
*     NDIM = INTEGER (Given)
*        The number of axes to be written to the NAXIS keyword.
*     DIMS( NDIM ) = INTEGER (Given)
*        Array containing the lengths of each of the NDIM dimensions.
*     BITPIX = INTEGER (Given)
*        The number of bits per data value to write to the BITPIX
*        keyword.  Allowed values are 8, 16, 32, -32, or -64.
*     FITSAR( EL ) = CHARACTER * ( 80 ) (Returned)
*        The array to contain the mandatory FITS headers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 22 (MJC):
*        Original version based upon RAHM's PREFITS.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      
*  Arguments Given:
      INTEGER EL      
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      INTEGER BITPIX
      
*  Arguments Given and Returned:
      CHARACTER * ( * ) FITSAR( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CARDLE             ! Length of each FITS header card-image
      PARAMETER ( CARDLE = 80 )

*  Local Variables:
      CHARACTER * 1 BUFDIM       ! Buffer for dimension number
      CHARACTER * 10 BUFFER      ! Buffer for text manipulation
      INTEGER IDIM               ! Loop counter
      INTEGER LINENO             ! Line number
      INTEGER NCHARS             ! Dummy to absorb unwanted number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the size of the array.
      IF ( EL .LT. MAX( 1, NDIM ) + 4 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'EL', EL )
         CALL MSG_SETI( 'MEL', MAX( 1, NDIM ) + 4 )
         CALL ERR_REP( 'CON_WFMAN_DIMS',
     :    'The FITS header array has only ^EL elements.  It must have '/
     :    /'at least ^MEL.', STATUS )
         GOTO 999
      END IF

*  Write the SIMPLE header.
*  ========================

*  Initialise the first element of the 80-character FITS array.
      LINENO = 1
      FITSAR( LINENO ) = ' '

*  Write the 'SIMPLE  = T' in the first line of the FITS headers.
      CALL CHR_MOVE( 'SIMPLE',          FITSAR( LINENO ) )
      CALL CHR_MOVE( '=',               FITSAR( LINENO )( 9:9 ) )
      CALL CHR_MOVE( 'T',               FITSAR( LINENO )( 30:30 ) )

*  Append the comment.
      CALL CHR_MOVE( '/ FITS standard', FITSAR( LINENO )( 32: ) )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( LINENO ), STATUS )

*  Write the BITPIX header.
*  ========================

*  Initialise the second element of the 80-character FITS array.
      LINENO = 2
      FITSAR( LINENO ) = ' '

*  Write the BITPIX keyword.
      CALL CHR_MOVE( 'BITPIX', FITSAR( LINENO )( 1: ) )

*  Now right justify the value using a buffer to convert the numerical
*  value to a string.
      CALL CHR_MOVE( '=',      FITSAR( LINENO )( 9:9 ) )
      CALL CHR_ITOC( BITPIX, BUFFER, NCHARS )
      CALL CHR_MOVE( BUFFER( :NCHARS ), FITSAR( LINENO )( 31-NCHARS: ) )

*  Append the comment.
      CALL CHR_MOVE( '/ Data type (bits per element)',
     :               FITSAR( LINENO )( 32: ) )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( LINENO ), STATUS )

*  Write the NAXIS header.
*  =======================

*  Initialise the third element of the 80-character FITS array.
      LINENO = 3
      FITSAR( LINENO ) = ' '

*  Write the NAXIS keyword.
      CALL CHR_MOVE( 'NAXIS', FITSAR( LINENO )( 1: ) )

*  Write the number of dimensions to the line.
      CALL CHR_MOVE( '=',     FITSAR( LINENO )( 9:9 ) )
      CALL CHR_ITOC( NDIM, FITSAR( LINENO )( 30:30 ), NCHARS )

*  Append the comment.
      CALL CHR_MOVE( '/ Number of dimensions', FITSAR( LINENO )( 32: ) )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( LINENO ), STATUS )

*  Write the NAXISn headers.
*  =========================

*  Loop for the NDIM dimensions starting at the next (the fourth) card
*  image.
      DO IDIM = 1, NDIM
         LINENO = IDIM + 3

*  Initialise the element of the 80-character FITS array.
         FITSAR( LINENO ) = ' '

*  Write the keyword appending the dimension number into a buffer.
         CALL CHR_MOVE( 'NAXIS', FITSAR( LINENO )( 1: ) )
         CALL CHR_ITOC( IDIM, BUFDIM, NCHARS )

*  Remove any leading blanks in the buffer and copy it to the next
*  element of the array of FITS headers.
         CALL CHR_LDBLK( BUFDIM )
         CALL CHR_MOVE( BUFDIM, FITSAR( LINENO )( 6: ) )

*  Append the equals sign and the number of elements along the current
*  dimension.
         CALL CHR_MOVE( '=', FITSAR( LINENO )( 9:9 ) )
         CALL CHR_ITOC( DIMS(IDIM), BUFFER, NCHARS )

*  Right justify the length.      
         CALL CHR_MOVE( BUFFER, FITSAR( LINENO )( 31-NCHARS: ) )

*  Append the comment.
         CALL CHR_MOVE( '/ Size of dimension',
     :                  FITSAR( LINENO )( 32: ) ) 
         CALL CHR_MOVE( BUFDIM, FITSAR( LINENO )( 52: ) ) 

*  Report the value in verbose mode.
         CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( LINENO ), STATUS )
      END DO

  999 CONTINUE

      END
