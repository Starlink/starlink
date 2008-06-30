      SUBROUTINE COF_FPWCS( FUNIT, INDF, ENCOD, NATIVE, STATUS )
*+
*  Name:
*     COF_FPWCS

*  Purpose:
*     Uses coordinate system information from the NDF WCS component
*     to create FITS headers in the current header and data unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FPWCS( FUNIT, INDF, ENCOD, NATIVE, STATUS )

*  Description:
*     The AST FrameSet (see SUN/210) describing the coordinate systems 
*     of the supplied NDF is obtained. Any Frames which can be generated
*     automatically are removed from this FrameSet (i.e. the PIXEL Frame, 
*     and also the AXIS Frame if it is equivalent to the PIXEL Frame). If
*     more than one Frame (i.e. the GRID frame) remains, a FITS header is 
*     created containing desriptions (known as "encodings") of the FrameSet. 

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number to which the FITS header cards are written.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     ENCOD = CHARACTER * ( * ) (Given)
*        The encoding to use. If this is blank, then a default encoding 
*        is chosen based on the contents of the FITS extension. The
*        supplied string should be a recognised AST encoding such as 'DSS', 
*        'FITS-WCS', 'NATIVE', etc (or a blank string).
*     NATIVE = LOGICAL (Given)
*        Include a NATIVE encoding in the header?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must already be opened with the FITSIO library.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1997 (DSB):
*        Original version.
*     9-NOV-1998 (DSB):
*        Replaced arguments NENCOD and ENCODS by NATIVE.
*     22-JUN-1999 (DSB):
*        Added ENCOD argument.
*     2-FEB-2000 (DSB):
*        Get the current number of header cards before emptying the
*        header.
*     11-APR-2000 (DSB):
*        Updated description of ENCOD argument.
*     12-FEB-2003 (DSB):
*        Reduce the value of the FitsDigits attribute from 20 to 10 
*        to avoid lots of spurious decimal places.
*     20-MAY-2003 (DSB):
*        Issue warning if WCS headers cannot be produced.
*     10-SEP-2004 (TIMJ):
*        Initialise HEADER to fix valgrind warning
*     30-JUN-2008 (DSB):
*        Delete cards from end of list to avoid massive CFITSIO overheads.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FUNIT
      INTEGER INDF
      CHARACTER ENCOD*(*)
      LOGICAL NATIVE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER COF_WCSEX

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Local Variables:
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      INTEGER FC                 ! Identifier for AST FitsChan
      INTEGER FSTAT              ! FITSIO status
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER KEYADD             ! Number of headers that can be added
      INTEGER NHEAD              ! Number of FITS headers
      LOGICAL THERE              ! Does the object exist?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialisation
      HEADER = ' '

*  Begin as AST context.
      CALL AST_BEGIN( STATUS )

*  Create an AST FitsChan. This is an object which acts as a buffer to 
*  hold a set of FITS header cards to be used by other AST routines.
*  Setting FitsDigits to a negative value ensures that FitsChan never
*  uses more than the number of digits allowed by the FITS standard when 
*  formatting floating point values.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, 'FITSDIGITS=-10', STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Find the number of headers (not including the final END) in the
*  current FITSIO header.
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FPWCS_NHEAD', 'FTGHSP',
     :                   'Error obtaining the number of header cards.',
     :                   STATUS )
         GO TO 999
      END IF

*  Increment the number of cards by 1 to include the final END card.
      NHEAD = NHEAD + 1

*  Copy each card from the FITSIO header into the FitsChan.
      DO IHEAD = 1, NHEAD

*  Obtain the header. If an error occurred getting the header, flush
*  the FITSIO error stack, but carry on to process any remaining
*  headers.
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG

*  Add this header into the FitsChan. If an error occurs, annul the
*  error, and continue to process any remaining headers.
         ELSE 
            CALL AST_PUTFITS( FC, HEADER, 1, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         END IF
      END DO

*  Now export any WCS information from the NDF into the FitsChan. This
*  may over-write any WCS information which already existed in the 
*  FITSIO header on entry. Only modify the supplied FITSIO header if at 
*  least one Object was written to the FitsChan.
      IF( COF_WCSEX( FC, INDF, ENCOD, NATIVE, STATUS ) .GE. 1 ) THEN

*  See how many cards there are in the FITSIO header now.
          CALL FTGHPS( FUNIT, NHEAD, KEYADD, FSTAT )

*  Empty the FITSIO header. For efficiency, delete from the end of the
*  list to the start of the list.
          DO IHEAD = NHEAD, 1, -1
             CALL FTDREC( FUNIT, IHEAD, FSTAT )
             IF( FSTAT .NE. FITSOK ) THEN
                FSTAT = FITSOK
                CALL FTCMSG
             END IF
          END DO

* Now copy the contents of the FitsChan into the empty FITSIO header.
         CALL AST_CLEAR( FC, 'Card', STATUS )
         DO WHILE( AST_FINDFITS( FC, '%f', HEADER, .TRUE., STATUS ) )
            CALL FTPREC( FUNIT, HEADER, FSTAT )
            IF( FSTAT .NE. FITSOK ) THEN
               FSTAT = FITSOK
               CALL FTCMSG
            END IF
         END DO

*  If the NDF has a WCS component but it could not be written out, issue a 
*  warning.
      ELSE 

         CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )
         IF( THERE ) THEN
            IF( ENCOD .NE. ' ' ) THEN
               CALL MSG_SETC( 'ENC', ENCOD )
               CALL MSG_OUT( 'COF_FPWCS_MSG1', '  WARNING: Unable '//
     :                       'to export WCS information using the '//
     :                       'specified FITS encoding ''^ENC''.', 
     :                       STATUS )
            ELSE
               CALL MSG_OUT( 'COF_FPWCS_MSG2', '  WARNING: Unable '//
     :                       'to export WCS information using any '//
     :                       'of the supported FITS encodings.', 
     :                       STATUS )
            END IF

         END IF
      END IF

*  Jump to here if an error occurs. 
  999 CONTINUE
      
*  End the AST context.
      CALL AST_END( STATUS )

      END
