      SUBROUTINE SALIA5( IDAB, IDAA, SCS, LBNDX, UBNDX, LBNDY, UBNDY,
     :                   IB1, IB2, JB1, JB2, XAMAP, YAMAP, STATUS )
*+
*  Name:
*     SALIA5

*  Purpose:
*     Calculate input image coordinates using full projection mappings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA5( IDAB, IDAA, SCS, LBNDX, UBNDX, LBNDY, UBNDY, IB1,
*                  IB2, JB1, JB2, XAMAP, YAMAP, STATUS )

*  Description:
*     The section (IB1:IB2,JB1:JB2) of the arrays XAMAP and YAMAP are
*     filled using the full projection mappings. Fixed size double
*     precision work arrays are used to pass information to and from the
*     IRA transform routine.

*  Arguments:
*     IDAB = INTEGER (Given)
*        IRA identifier for the astrometry information defining the
*        output (reference) pixel grid.
*     IDAA = INTEGER (Given)
*        IRA identifier for the astrometry information defining the
*        input pixel grid.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system used by the reference astrometry
*        information.
*     LBNDX = INTEGER (Given)
*        The lower X bound of the output image.
*     UBNDX = INTEGER (Given)
*        The upper X bound of the output image.
*     LBNDY = INTEGER (Given)
*        The lower Y bound of the output image.
*     UBNDY = INTEGER (Given)
*        The upper Y bound of the output image.
*     IB1 = INTEGER (Given)
*        The lower X bound of the section to be filled.
*     IB2 = INTEGER (Given)
*        The upper X bound of the section to be filled.
*     JB1 = INTEGER (Given)
*        The lower Y bound of the section to be filled.
*     JB2 = INTEGER (Given)
*        The upper Y bound of the section to be filled.
*     XAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = REAL (Returned)
*        Holds the input X image coordinate corresponding to the centre
*        of each output pixel.
*     YAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = REAL (Returned)
*        Holds the input Y image coordinate corresponding to the centre
*        of each output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER IDAB
      INTEGER IDAA
      CHARACTER SCS*(*)
      INTEGER LBNDX
      INTEGER UBNDX
      INTEGER LBNDY
      INTEGER UBNDY
      INTEGER IB1
      INTEGER IB2
      INTEGER JB1
      INTEGER JB2

*  Arguments Given and Returned:
      REAL XAMAP( LBNDX:UBNDX, LBNDY:UBNDY )
      REAL YAMAP( LBNDX:UBNDX, LBNDY:UBNDY )

*  Status:
      INTEGER STATUS          ! Global status

*  Local Constants:
      INTEGER SIZE            ! Size of DOUBLE PRECISION working arrays.
      PARAMETER ( SIZE = 100 )

*  Local Variables:
      DOUBLE PRECISION
     :        AA( SIZE ),     ! Sky longitudes
     :        BB( SIZE ),     ! Sky latitudes
     :        XX( SIZE ),     ! X image coordinates
     :        YY( SIZE )      ! Y image coordinates

      INTEGER
     :        FILL,           ! Index of the current fill.
     :        I,              ! Output X pixel index.
     :        I0,             ! Output X pixel index.
     :        J,              ! Output Y pixel index.
     :        J0,             ! Output Y pixel index.
     :        K,              ! Loop count.
     :        N,              ! No. of points stored in the work arrays.
     :        NFILL,          ! No. of times the work arrays are filled.
     :        NTOT            ! Total no. of points to be transformed.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the row and column number of the first pixel to be
*  transformed.
      J0 = JB1
      J = JB1
      I0 = IB1
      I = IB1

*  Calculate the total no. of points to be transformed.
      NTOT = ( JB2 - JB1 + 1 )*( IB2 - IB1 + 1 )

*  Calculate the number of times this number of points would fill the
*  double precision working arrays.
      NFILL = 1 + ( NTOT - 1 )/SIZE

*  Loop round this number of times.
      DO FILL = 0, NFILL - 1

*  Calculate the number of points included in this "fill".
         N = MIN( SIZE, NTOT - FILL*SIZE )

*  Loop round this number of points, storing the image coordinates in
*  the double precision work arrays.
         DO K = 1, N
            YY( K ) = DBLE( J ) - 0.5D0
            XX( K ) = DBLE( I ) - 0.5D0

*  Increment the pixel indices.
            I = I + 1
            IF( I .GT. IB2 ) THEN
               I = IB1
               J = J + 1
            END IF

         END DO

*  Transform the points from output image coordinates to sky
*  coordinates.
         CALL IRA_TRANS( N, XX, YY, .TRUE., SCS, IDAB, AA, BB, STATUS )

*  Convert sky coordinates to input image coordinates.
         CALL IRA_TRANS( N, AA, BB, .FALSE., SCS, IDAA, XX, YY, STATUS )

*  Abort if an error occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now store the input image coordinates in the returned arrays.
         DO K = 1, N

            IF( XX( K ) .NE. VAL__BADD ) THEN
               XAMAP( I0, J0 ) = REAL( XX( K ) )
               YAMAP( I0, J0 ) = REAL( YY( K ) )
            ELSE
               XAMAP( I0, J0 ) = VAL__BADR
               YAMAP( I0, J0 ) = VAL__BADR
            END IF

*  Increment the pixel indices.
            I0 = I0 + 1
            IF( I0 .GT. IB2 ) THEN
               I0 = IB1
               J0 = J0 + 1
            END IF

         END DO

      END DO

 999  CONTINUE

      END
