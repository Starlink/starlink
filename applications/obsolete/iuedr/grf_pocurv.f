      SUBROUTINE GRF_POCURV( MASK, NPOINT, X, Y, DQ, SLABEL, NSL,
     :                       QUAL, ZERO, SELFLG, RELFLG )
*+
*  Name:
*     SUBROUTINE GRF_POCURV

*  Purpose:
*     Plot a ployline within set zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_POCURV( MASK, NPOINT, X, Y, DQ, SLABEL, NSL,
*    :                 QUAL, ZERO, SELFLG, RELFLG )

*  Arguments:
*     MASK = INTEGER (Given)
*        Data quality mask.
*     NPOINT = INTEGER (Given)
*        Number of points.
*     X = REAL( NPOINT ) (Given)
*        X-axis data.
*     Y = REAL( NPOINT ) (Given)
*        Y-axis data.
*     DQ = INTEGER( NPOINT ) (Given)
*        Data quality information.
*     SLABEL = CHARACTER*( * ) (Given)
*        Supplementary label for graph.
*     NSL = INTEGER (Given)
*        Length of supplementary label.
*     QUAL = LOGICAL (Given)
*        Whether to plot data qualities.
*     ZERO = LOGICAL (Given)
*        Whether to plot zero level.
*     SELFLG = LOGICAL (Given)
*        Select line type.
*     RELFLG = LOGICAL (Given)
*        Reset line type.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     19-MAY-81 (JRG):
*       AT4 version.
*     12-JAN-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     08-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMLINR'
      INCLUDE 'CMPAL'

*  Local Constants:
      INTEGER MAXPOINT       ! Maximum number of points.
      PARAMETER ( MAXPOINT = 27800 )

*  Arguments Given:
      INTEGER MASK           ! Data quality mask.
      INTEGER NPOINT         ! Number pf points.

      REAL X( NPOINT )       ! X-axis data
      REAL Y( NPOINT )       ! Y-axis data

      INTEGER DQ( NPOINT )   ! Data quality.

      CHARACTER*( * ) SLABEL ! Supplementary graph label.

      INTEGER NSL            ! Length of supplementary graph label.

      LOGICAL QUAL           ! Plot data qualities.
      LOGICAL ZERO           ! Plot zero level.
      LOGICAL SELFLG         ! Select line type.
      LOGICAL RELFLG         ! Reset line type.

*  External References:
      INTEGER DQ_AND         ! Data quality AND.

*  Local Variables:
      REAL XPOLY( MAXPOINT ) ! X-axis polyline array.
      REAL X1                ! XLIM( 1 ).
      REAL X2                ! XLIM( 2 ).
      REAL YGAP              ! 5% gap between Y-axis data and axes.
      REAL YPOLY( MAXPOINT ) ! Y-axis polyline array.
      REAL Y1                ! YLIM( 1 ).
      REAL Y2                ! YLIM( 2 ).

      INTEGER COUNT          ! Polyline point count.
      INTEGER FIRST          ! Index of first plot point in X/Y.
      INTEGER I              ! Loop index.
      INTEGER LAST           ! Index of last plot point in X/Y.
      INTEGER LASTQ          ! Index of last plotted data quality gap.
*.

*   Draw axes.
      CALL GRF_AXES( X1, X2, Y1, Y2, YGAP, SLABEL, NSL )

      CALL GRF_XYIND( MASK, NPOINT, X, Y, DQ, XLIM, YLIM, FIRST, LAST )

      IF ( LAST .GT. FIRST ) THEN
         LASTQ = 1
         COUNT = 0

*      Set line type and colour.
         IF ( SELFLG ) THEN
            CALL GRF_SELINE( 0, 0 )
         END IF

*      Loop to plot data.
         DO I = FIRST, LAST
            IF ( DQ_AND( DQ( I ), 1 ) .EQ. 0 ) THEN
               IF ( DQ_AND( DQ( I ), MASK ) .EQ. 0 ) THEN
                  IF ( LASTQ .EQ. 0 ) THEN

*                  If data are contiguous, then load polyline arrays.
                     COUNT = COUNT + 1
                     XPOLY( COUNT ) = X( I )
                     YPOLY( COUNT ) = Y( I )
                  ELSE

*                  If data are not contigous, plot polyline arrays
*                  and restart load.
                     IF ( COUNT .GT. 0 ) THEN
                        CALL AGCURV( XPOLY, 1, YPOLY, 1, COUNT, 0 )
                     END IF

                     COUNT = 1
                     XPOLY( COUNT ) = X( I )
                     YPOLY( COUNT ) = Y( I )
                  END IF
               END IF
            END IF

            LASTQ = DQ( I )
         END DO

*      Finish plotting.
         IF ( COUNT .GT. 0 ) THEN
            CALL AGCURV( XPOLY, 1, YPOLY, 1, COUNT, 0 )
         END IF
      END IF

*   Draw data quality flags.
      IF ( QUAL ) THEN
         CALL GSTXCI( TILUT( TITEXT ) )
         CALL GRF_DRQUAL( NPOINT, X, Y, DQ )
      END IF

*   Draw a base line at zero - DRZERO.
      IF ( ZERO ) THEN
         IF ( Y1.LT.0 .AND. Y2.GT.0 ) THEN
            CALL LINE( X1, 0.0, X2, 0.0 )
         END IF
      END IF

*   Flush graphics.
      CALL PLOTIT( 0, 0, 2 )
      CALL SGS_FLUSH

*   Reset line parameters.
      IF ( RELFLG ) THEN
         CALL GRF_RELINE
      END IF

*   Switch to SGS coordinates.
      CALL SNX_AGCS

      END
