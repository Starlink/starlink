      SUBROUTINE FTS1_FTWCS( NCARD, HEADER, SCARD, INDF, NENCOD, ENCODS, 
     :                       STATUS )
*+
*  Name:
*     FTS1_FTWCS

*  Purpose:
*     Uses coordinate system information in the supplied FITS headers 
*     to create WCS and AXIS components in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_FTWCS( NCARD, HEADER, SCARD, INDF, NENCOD, ENCODS, 
*                      STATUS )

*  Description:
*     This constructs an AST FrameSet from the supplied FITS headers 
*     and adds it into the existing WCS information in the supplied NDF. 
*     It can also create AXIS structures (see below).
*
*     The information needed to create the FrameSet can be stored several
*     times in a single FITS header, using different keywords each time.
*     Each of these descriptions is known as an "encoding" and AST supports 
*     several different encoding schemes (i.e. FITS-WCS, DSS, NATIVE).
*     If the supplied FITS header contains more than one encoding then we
*     need to choose which one to use. This decision is important because
*     is is possible for encodings to be inconsistent (i.e. software may
*     modify one encoding without making equivalent modifications to the
*     other encodings). The simplest way to make this decision is to hand
*     responsibility for it over to the user. In this case, the user
*     supplies a list of preferred encodings, and the first of these encodings
*     which exists in the FITS header gets used. If the user does not
*     know which encoding to use, then we can make an intelligent guess by
*     comparing the encodings to see which ones are consistent and which
*     ones are not. 
*
*     In addition to the WCS component, this routine also creates AXIS 
*     Centre, Label and Units components in the NDF, but only if they do 
*     not already exist, and if the FrameSet read from the FITS header 
*     contains an AXIS Frame. NDF2FITS does not write out the AXIS Frame 
*     if it is equivalent to pixel coordinates, and so no AXIS structures 
*     will be created by this routine in this case. Also, if the AXIS Frame 
*     represents linear axis coordinates, then there will already be AXIS 
*     structures in the NDF (created earlier within FITS2NDF), and so again 
*     no AXIS  structures will be created by this routine. Thus, this routine 
*     will only create AXIS structures in the cases where the axis coordinates 
*     are non-linear.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards in the array of headers, from the start of
*        the first header section to the end of the current one.
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     SCARD = INTEGER (Given)
*        The number of the card from where searches will begin, and 
*        copying of the headers to the FITS extension.   Therefore
*        NCARD - SCARD + 1 headers will appear in the extension. This
*        argument is needed because the headers make contain a dummy
*        header prior to an extension.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     NENCOD = INTEGER (Given)
*        The number of encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The user's preferred AST encodings. If NENCOD is zero, then this
*        is ignored, and an intelligent guess is made as to which encoding
*        to use (see FTS1_WCSIM).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JUN-1998 (DSB):
*        Original version.
*     14-SEP-1998 (DSB):
*        Changed to report the first 3 bad header cards.
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
      INTEGER NCARD              
      CHARACTER HEADER( NCARD )*80
      INTEGER SCARD            
      INTEGER INDF
      INTEGER NENCOD
      CHARACTER ENCODS( NENCOD )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER MXBADH             ! Max. no. of bad header card indices to save
      PARAMETER (MXBADH = 3)    

*  Local Variables:
      INTEGER BADH( MXBADH )     ! Indices of bad header cards
      INTEGER FC                 ! Identifier for AST FitsChan
      INTEGER I                  ! Loop counter for bad headers
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER NBAD               ! Number of bad header cards

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin as AST context.
      CALL AST_BEGIN( STATUS )

*  Create an AST FitsChan. This is an object which acts as a buffer to 
*  hold a set of FITS header cards to be used by other AST routines.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the number of bad header cards.
      NBAD = 0

*  Loop through the usable headers.
      DO IHEAD = SCARD, NCARD

*  Add this header into the FitsChan. If an error occurs, annul the
*  error, increment the number of bad headers, and continue to process any
*  remaining headers.
         CALL AST_PUTFITS( FC, HEADER( IHEAD ), 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            NBAD = NBAD + 1
            IF( NBAD .LE. MXBADH ) BADH( NBAD ) = IHEAD 
         END IF

      END DO

*  Issue a warning if any bad header cards were encountered.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBAD .NE. 0 ) ) THEN
         CALL ERR_MARK
         STATUS = SAI__ERROR

         IF( NBAD .EQ. 1 ) THEN
            CALL ERR_REP( 'FTS1_FTWCS_W1', 'WARNING: The following '//
     :                    'FITS header card could not be read and '//
     :                    'will be ignored:', STATUS )
            CALL MSG_SETC( 'CARD', HEADER( BADH( 1 ) ) )
            CALL ERR_REP( 'FTS1_FTWCS_W2', '^CARD', STATUS )

         ELSE IF( NBAD .LE. MXBADH ) THEN
            CALL ERR_REP( 'FTS1_FTWCS_W3', 'WARNING: The following '//
     :                    'FITS header cards could not be read and '//
     :                    'will be ignored:', STATUS )
            DO I = 1, NBAD
               CALL MSG_SETI( 'CARD', HEADER( BADH( I ) ) )
               CALL ERR_REP( 'FTS1_FTWCS_W4', '^CARD', STATUS )
            END DO

         ELSE
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_SETI( 'MXBADH', MXBADH )
            CALL ERR_REP( 'FTS1_FTWCS_W5', 'WARNING: ^NBAD FITS '//
     :                    'header cards could not be read and will be'//
     :                    ' ignored. The first ^MXBADH such cards '//
     :                    'were:', STATUS )

            DO I = 1, MXBADH
               CALL MSG_SETI( 'CARD', HEADER( BADH( I ) ) )
               CALL ERR_REP( 'FTS1_FTWCS_W6', '^CARD', STATUS )
            END DO

         END IF

         CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE

      END IF

*  Rewind the FitsChan by clearing the Card attribute so that the first
*  header card will be read by FTS1_WCSIM.
      CALL AST_CLEAR( FC, 'Card', STATUS )

*  Now import any WCS information from the FitsChan into the NDF.
      CALL FTS1_WCSIM( FC, INDF, NENCOD, ENCODS, STATUS )

*  Jump to here if an error occurs. Add a context message.
  999 CONTINUE
      IF( STATUS .NE. SAI__OK )THEN
         CALL ERR_REP( 'FTS1_FTWCS_ERR1', 'Error importing World '//
     :                 'Coordinate information.', STATUS )
         CALL ERR_REP( 'FTS1_FTWCS_ERR2', 'The output NDF will not '//
     :                 'contain any WCS information.', STATUS )

*  A useful conversion may still be possible even if the WCS
*  information cannot be imported. So if an error occurred, flush it.
         CALL ERR_FLUSH( STATUS )
      END IF
      
*  End the AST context.
      CALL AST_END( STATUS )

      END
