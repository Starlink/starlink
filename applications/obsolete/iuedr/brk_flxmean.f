      REAL*8 FUNCTION BRK_FLXMEAN( IMW1, IMW2, NPTS, FLX, WV,
     :                             Q, OK )
*+
*  Name:
*     REAL*8 FUNCTION BRK_FLXMEAN

*  Purpose:
*     Determine mean flux in specified region of spectral order.

*  History:
*     Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*     Paul Rees          29-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6

*  Method:
*     See Barker (1984).

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER IMW1        ! Index of start point in WV array.
      INTEGER IMW2        ! Index of end point in WV array.
      INTEGER NPTS        ! Number of points in WV and FLX arrays.
      REAL*8 FLX( NPTS )  ! Array of fluxes.
      REAL*8 WV( NPTS )   ! Array of wavelengths.
      INTEGER Q( NPTS )   ! Array of quality flags (0=good).

*  Arguments Given and Returned:
      LOGICAL OK          ! Imported as T, unchanged on success.

*  Local Variables:
      INTEGER I
      INTEGER SUM
*.

*  Initialise accumulators.
      BRK_FLXMEAN = 0.0
      SUM = 0

*  Calculate summations.
      DO I = IMW1, IMW2
         IF ( Q( I ) .EQ. 0 ) THEN
            SUM = SUM + 1
            BRK_FLXMEAN = BRK_FLXMEAN + FLX( I )
         END IF
      ENDDO

*  Trap for no flux.
      IF ( SUM .LE. 0 ) THEN
         OK = .FALSE.
         BRK_FLXMEAN = 0.0

      ELSE
         BRK_FLXMEAN = BRK_FLXMEAN / DBLE( SUM )
      END IF

      END
