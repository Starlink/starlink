      SUBROUTINE SPHOZ1( COLLO, COLHI, ROWLO, ROWHI, DATA, SCALE,
     :                   PIXSOL, BACK, X, Y, XSIZE, YSIZE, LOGING, FD,
     :                   IDA, SCS, SHAPE, MEAN, FLUXD, SIGMA, STATUS )
*+
*  Name:
*     SPHOZ1

*  Purpose:
*     Integrate flux density in a rectangular or elliptical aperture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPHOZ1( COLLO, COLHI, ROWLO, ROWHI, DATA, SCALE, PIXSOL,
*                  BACK, X, Y, XSIZE, YSIZE, LOGING, FD, IDA, SCS,
*                  SHAPE, MEAN, FLUXD, SIGMA, STATUS )

*  Description:
*     The flux density (after subtraction of the supplied background
*     surface brightness) within the aperture specified by the supplied
*     argument values is integrated. The results are displayed on the
*     standard output device, logged (optionally) to a log file, and
*     returned in the arguments MEAN, FLUXD and SIGMA. This routine
*     does not handle polygonal apertures.

*  Arguments:
*     COLLO = INTEGER (Given)
*        Lower bound on column number in the image.
*     COLHI = INTEGER (Given)
*        Upper bound on column number in the image.
*     ROWLO = INTEGER (Given)
*        Lower bound on row number in the image.
*     ROWHI = INTEGER (Given)
*        Upper bound on row number in the image.
*     DATA( COLLO:COLHI, ROWLO:ROWHI ) = REAL (Given)
*        The input data array.
*     SCALE = REAL (Given)
*        The factor which converts input data values into units of
*        Jy/pixel.
*     PIXSOL = REAL (Given)
*        Solid angle of a pixel, in steradians.
*     BACK = REAL (Given)
*        Background surface brightness, in the same units as DATA.
*     X = REAL (Given)
*        X image coordinate of the aperture centre.
*     Y = REAL (Given)
*        Y image coordinate of the aperture centre.
*     XSIZE = REAL (Given)
*        Half X dimension (in pixels) of the aperture (either half the
*        length of the side of the horizontal side of the rectangle, or
*        half the length of the horizontal axis of the ellipse).
*     YSIZE = REAL (Given)
*        Half Y dimension (in pixels) of the aperture (either half the
*        length of the side of the vertical side of the rectangle, or
*        half the length of the vertical axis of the ellipse).
*     LOGING = LOGICAL (Given)
*        True if a log file is to be written to.
*     FD = INTEGER (Given)
*        FIO file descriptor for log file. Not used if LOGING is false.
*     IDA = INTEGER (Given)
*        Identifier for astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        Sky coordinate system for displaying sky coordinate values.
*     SHAPE = CHARACTER * ( * ) (Given)
*        Shape of the aperture; "RECTANGLE" or "ELLIPSE".
*     MEAN = REAL (Returned)
*        The mean surface brightness, in MJy/sr.
*     FLUXD = REAL( Returned)
*        The total flux density, in Jy.
*     SIGMA = REAL (Returned)
*        The standard deviation of the surface brightness, in MJy/sr.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1993 (DSB):
*        Original version.
*     22-SEP-1993 (DSB):
*        MEAN, FLUXD and SIGMA added to argument list.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER COLLO
      INTEGER COLHI
      INTEGER ROWLO
      INTEGER ROWHI
      REAL DATA( COLLO:COLHI, ROWLO:ROWHI )
      REAL SCALE
      REAL PIXSOL
      REAL BACK
      REAL X
      REAL Y
      REAL XSIZE
      REAL YSIZE
      LOGICAL LOGING
      INTEGER FD
      INTEGER IDA
      CHARACTER SCS*(*)
      CHARACTER SHAPE*(*)

*  Arguments Returned:
      REAL MEAN
      REAL FLUXD
      REAL SIGMA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :     ATEXT*(IRA__SZFSC),   ! Formatted longitude value
     :     BTEXT*(IRA__SZFSC),   ! Formatted latitude value
     :     TEXT*80               ! Buffer for output text

      DOUBLE PRECISION
     :     A,                    ! Longitude value
     :     B                     ! Latitude value

      INTEGER
     :     I,                    ! Column index
     :     IAT,                  ! Position of last non-blank character
     :     J,                    ! Row index
     :     NGOOD                 ! No. of good pixel values in aperture

      REAL
     :     DATVAL,               ! Input data value (minus background)
     :     SUM1,                 ! Sum of data values 
     :     SUM2,                 ! Sum of squared data values 
     :     XHI,                  ! Upper X limit for current row
     :     XLO                   ! Lower X limit for current row
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the supplied image coordinates to sky coordinates.
      CALL IRA_TRANS( 1, DBLE( X ), DBLE( Y ), .TRUE., SCS, IDA, A, B,
     :                STATUS )

*  Format the sky coordinates.
      CALL IRA_DTOC( A, B, SCS, 0, ATEXT, BTEXT, STATUS )

*  Display the sky coordinates.
      TEXT = '    Centre                 : '      
      IAT = 29
      CALL CHR_APPND( ATEXT, TEXT, IAT )
      CALL CHR_APPND( ', '//BTEXT, TEXT, IAT )
      CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
      IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  Initialise things.
      NGOOD = 0
      SUM1 = 0.0
      SUM2 = 0.0

*  If the aperture has zero size, leave things as they are.
      IF( XSIZE*YSIZE .NE. 0.0 ) THEN

*  If dealing with rectangular apertures...
         IF( SHAPE .EQ. 'RECTANGLE' ) THEN

*  Loop round all rows in the aperture which fall within the image area.
            DO J = NINT( Y + 0.5 - YSIZE ), NINT( Y + 0.5 + YSIZE )
               IF( J .GE. ROWLO .AND. J .LE. ROWHI ) THEN

*  Loop round all columns in the aperture which fall within the image
*  area.
                  DO I = NINT( X + 0.5 - XSIZE ),
     :                   NINT( X + 0.5 + XSIZE )
                     IF( I .GE. COLLO .AND. I .LE. COLHI ) THEN

*  Only include this pixel value if it is not bad.
                        DATVAL = DATA( I, J )
                        IF( DATVAL .NE. VAL__BADR ) THEN

*  Subtract the supplied background from the input pixel value.
                           DATVAL = DATVAL - BACK

*  Increment the statistics.
                           SUM1 = SUM1 + DATVAL            
                           SUM2 = SUM2 + DATVAL*DATVAL
                           NGOOD = NGOOD + 1

                        END IF
      
                     END IF
                  END DO

               END IF
            END DO

*  If dealing with elliptical apertures...
         ELSE

*  Loop round all rows in the aperture which fall within the image area.
            DO J = NINT( Y + 0.5 - YSIZE ), NINT( Y + 0.5 + YSIZE )
               IF( J .GE. ROWLO .AND. J .LE. ROWHI ) THEN

*  Calculate the upper and lower limits on column number.
                  XHI = ( XSIZE/YSIZE )*SQRT( MAX( 0.0, YSIZE**2 -
     :                  ( REAL( J ) - 0.5 - Y )**2 ) ) + X
                  XLO = 2.0*X - XHI 

*  Loop round all columns in the aperture which fall within the image
*  area.
                  DO I = NINT( XLO + 0.5 ), NINT( XHI + 0.5 )
                     IF( I .GE. COLLO .AND. I .LE. COLHI ) THEN

*  Only include this pixel value if it is not bad.
                        DATVAL = DATA( I, J )
                        IF( DATVAL .NE. VAL__BADR ) THEN

*  Subtract the supplied background from the input pixel value.
                           DATVAL = DATVAL - BACK

*  Increment the statistics.
                           SUM1 = SUM1 + DATVAL            
                           SUM2 = SUM2 + DATVAL*DATVAL
                           NGOOD = NGOOD + 1

                        END IF
      
                     END IF
                  END DO

               END IF
            END DO

         END IF

*  If no good values were found, tell the user.
         IF( NGOOD .EQ. 0 ) THEN
            TEXT = '    No good data found in aperture'
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT, STATUS )
            IF ( LOGING ) CALL FIO_WRITE( FD, TEXT, STATUS )

*  Otherwise, calculate the total flux density in the aperture, in Jy.
         ELSE
            FLUXD = SUM1*SCALE

*  Calculate the mean surface grightness in MJy/sr.
            MEAN = FLUXD/(NGOOD*PIXSOL*1.0E6)

*  Calculate the standard deviation of the surface brightness, in
*  MJy/sr.
            SIGMA = SQRT( MAX( 0.0, ( SUM2/NGOOD ) -
     :              ( SUM1/NGOOD )**2 ) )*SCALE/(PIXSOL*1.0E6)

*  Display the results.
            TEXT = '    Total flux density     : '
            IAT = 29
            CALL CHR_PUTR( FLUXD, TEXT, IAT )
            CALL CHR_APPND( ' Jy', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      
            TEXT = '    Mean surface brightness: '
            IAT = 29
            CALL CHR_PUTR( MEAN, TEXT, IAT )
            CALL CHR_APPND( ' MJy/sr', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

            TEXT = '    Standard deviation     : '
            IAT = 29
            CALL CHR_PUTR( SIGMA, TEXT, IAT )
            CALL CHR_APPND( ' MJy/sr', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

            TEXT = '    Used pixels in aperture: '
            IAT = 29
            CALL CHR_PUTI( NGOOD, TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

         END IF

*  If the aperture has zero size, tell the user.
      ELSE
         TEXT = '    Aperture has zero size'
         CALL MSG_OUTIF( MSG__NORM, ' ', TEXT, STATUS )
         IF ( LOGING ) CALL FIO_WRITE( FD, TEXT, STATUS )

      END IF

*  Seperate subsequent results by a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF( LOGING ) CALL FIO_WRITE( FD, ' ', STATUS )

      END
