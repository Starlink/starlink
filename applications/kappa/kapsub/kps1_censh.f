      SUBROUTINE KPS1_CENSH( CERROR, CFRM, NPOS, ID, LOGPOS, FDL, QUIET, 
     :                       NDIMS, NAXC, GUESS, OUTCO, FDO, NWAS, 
     :                       ERROR, PIXPOS, CURPOS, CURWAS, I, STATUS ) 
*+
*  Name:
*     KPS1_CENSH

*  Purpose:
*     Format and display a centroid position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CENSH( CERROR, CFRM, NPOS, ID, LOGPOS, FDL, QUIET, 
*                      NDIMS, NAXC, GUESS, OUTCO, FDO, NWAS, ERROR, 
*                      PIXPOS, CURPOS, CURWAS, I, STATUS ) 

*  Description:
*     This routine displays the results fo a single centroiding
*     operation to a log file and the screen.

*  Arguments:
*     CERROR = INTEGER (Given) 
*        Should the error on the centroid positions be found and dislayed?
*        This requires the DNF to have a variance component.
*     CFRM = INTEGER (Given) 
*        A pointer to the current Frame of the NDF. 
*     NPOS = INTEGER (Given) 
*        The number of centroid positions to be found.
*     ID = INTEGER (Given) 
*        An integer identifier for the position to be displayed with the 
*        centroid position.
*     LOGPOS = LOGICAL (Given) 
*        Should the results be written to a log file?
*     FDL = INTEGER (Given) 
*        The file descriptor for the log file. Ignored if LOGPOS is
*        .FALSE.
*     QUIET = INTEGER (Given) 
*        If .FALSE., the results are written to standard output. If .TRUE.
*        nothing is written to standard output.
*     NDIMS = INTEGER (Given) 
*        The number of significant axes in the NDF (i.e. axes spanning
*        more than a single pixel).
*     NAXC = INTEGER (Given) 
*        The number of axes in CFRM.
*     GUESS = LOGICAL (Given)
*        Should the original guess positions be included in the information
*        displayed on the screen and/or written to the log file?
*     OUTCO = LOGICAL (Given) 
*        Should the pixel co-ordinates of the centroids be written to an
*        output text file?
*     FDO = INTEGER (Given) 
*        Teh file descriptor for the output text file. Ignored if OUTCO
*        is .FALSE.
*     NWAS = INTEGER (Given) 
*        The first dimension of the CURWAS array. 
*     ERROR( NPOS, NAXC ) = DOUBLE PRECISION (Given) 
*        The errors on the centroid positions, given in the current Frame
*        of the NDF.
*     PIXPOS( NPOS, NDIMS ) = DOUBLE PRECISION (Given) 
*        The centroid positions, given in the PIXEL Frame of the NDF.
*     CURPOS( NPOS, NAXC ) = DOUBLE PRECISION (Given) 
*        The centroid positions, given in the current Frame of the NDF.
*     CURWAS( NWAS, NAXC ) = DOUBLE PRECISION (Given) 
*        The initial guesses at the centroid positions, in the current 
*        Frame. Only accessed if GUESS is .TRUE.
*     I = INTEGER (Given)
*        The index of the position to be displayed (i.e. the first array
*        index within the arrays ERROR, PIXPOS, CURPOS and CURWAS).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      LOGICAL CERROR 
      INTEGER CFRM 
      INTEGER NPOS 
      INTEGER ID
      LOGICAL LOGPOS 
      INTEGER FDL 
      LOGICAL QUIET 
      INTEGER NDIMS 
      INTEGER NAXC
      LOGICAL GUESS
      LOGICAL OUTCO 
      INTEGER FDO 
      INTEGER NWAS 
      DOUBLE PRECISION ERROR( NPOS, NAXC )
      DOUBLE PRECISION PIXPOS( NPOS, NDIMS )
      DOUBLE PRECISION CURPOS( NPOS, NAXC )
      DOUBLE PRECISION CURWAS( NWAS, NAXC )
      INTEGER I

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants: 
      INTEGER MXBUF              ! Length of screen buffer
      PARAMETER ( MXBUF = 80 )

*  Local Variables:
      CHARACTER AXVAL*50         ! A formatted axis value
      CHARACTER LINE1*(MXBUF)    ! Buffer for output text
      CHARACTER LINE2*(MXBUF)    ! Buffer for output text
      CHARACTER LINE3*(MXBUF)    ! Buffer for output text
      INTEGER IAT1               ! Number of characters currently in LINE1
      INTEGER IAT2               ! Number of characters currently in LINE2
      INTEGER IAT3               ! Number of characters currently in LINE3
      INTEGER J                  ! Axis index
      LOGICAL OK                 ! Is this position OK?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the pixel co-ordinates at the centroid to the output co-ordinate 
*  file if required, so long as all pixel axis values are good.
      IF( OUTCO ) THEN

         OK = .TRUE.
         DO J = 1, NDIMS
            IF( PIXPOS( I, J ) .EQ. AST__BAD ) OK = .FALSE.
         END DO

         IF( OK ) THEN         
            IAT1 = 0
            LINE1 = ' '
            DO  J = 1, NDIMS
               CALL CHR_PUTR( REAL( PIXPOS( I, J ) ), LINE1, IAT1 )
               CALL CHR_PUTC( '  ', LINE1, IAT1 )
            END DO
            CALL FIO_WRITE( FDO, LINE1( :IAT1 ), STATUS )
         END IF

      END IF

*  Now do the screen and log file output. Format a line containing 
*  the position identifier, followed by a colon, and the centroid axis
*  values.
      IAT1 = 0
      LINE1 = ' '
      CALL CHR_PUTI( ID, LINE1, IAT1 )
      CALL CHR_APPND( ':', LINE1, IAT1 )

      IAT1 = 5

      DO J = 1, NAXC 
         CALL CHR_APPND( AST_FORMAT( CFRM, J, CURPOS( I, J ), 
     :                               STATUS ), LINE1, IAT1 )
         IF( J .NE. NAXC ) IAT1 = IAT1 + 1
      END DO

*  Format a line containing the errors if required.
      IF( CERROR ) THEN
         LINE2 = '  +/-  '
         IAT2 = 7

         DO J = 1, NAXC
            CALL CHR_APPND( AST_FORMAT( CFRM, J, ERROR( I, J ), 
     :                                  STATUS ), LINE2, IAT2 )
            IF( J .NE. NAXC ) IAT2 = IAT2 + 1
         END DO

      ELSE
         LINE2 = ' '
         IAT2 = 1
      END IF

*  Format a line containing the original guesses (if required).
      IF( GUESS ) THEN
         LINE3 = '  (was '
         IAT3 = 7
       
         DO J = 1, NAXC
            CALL CHR_APPND( AST_FORMAT( CFRM, J, CURWAS( I, J ), 
     :                                  STATUS ), LINE3, IAT3 )
            IF( J .NE. NAXC ) IAT3 = IAT3 + 1
         END DO

         CALL CHR_APPND( ')', LINE3, IAT3 )

      ELSE
         LINE3 = ' '
         IAT3 = 1
      END IF

*  Now display these 3 strings. Put as many as possible on the same line.
      IF( IAT1 + IAT2 + IAT3 .LT. MXBUF ) THEN
         CALL CHR_APPND( LINE2( : IAT2 ), LINE1, IAT1 )
         CALL CHR_APPND( LINE3( : IAT3 ), LINE1, IAT1 )
         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE1( : IAT1 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE1( : IAT1 ), STATUS )

      ELSE IF( IAT1 + IAT2 .LT. MXBUF ) THEN
         CALL CHR_APPND( LINE2( : IAT2 ), LINE1, IAT1 )
         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE1( : IAT1 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE1( : IAT1 ), STATUS )

         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE3( : IAT3 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE3( : IAT3 ), STATUS )

      ELSE
         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE1( : IAT1 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE1( : IAT1 ), STATUS )

         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE2( : IAT2 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE2( : IAT2 ), STATUS )

         IF( .NOT. QUIET ) CALL MSG_OUT( ' ', LINE3( : IAT3 ), STATUS )
         IF( LOGPOS ) CALL FIO_WRITE( FDL, LINE3( : IAT3 ), STATUS )
      END IF

      END
