      SUBROUTINE GRF_HICURV( MASK, NPOINT, X, Y, DQ, SLABEL, NSL,
     :                       QUAL, ZERO, SELFLG, RELFLG )
*+
*  Name:
*     SUBROUTINE GRF_HICURV

*  Purpose:
*     Plot a histogram within set zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_HICURV( MASK, NPOINT, X, Y, DQ, SLABEL, NSL,
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
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

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
      INTEGER NPOINT         ! Number of points.

      REAL X( NPOINT )       ! X-axis data.
      REAL Y( NPOINT )       ! Y-axis data.

      INTEGER DQ( NPOINT )   ! Data quality.

      CHARACTER*( * ) SLABEL ! Supplementary graph label.

      INTEGER NSL            ! Length of supplementary label.

      LOGICAL QUAL           ! Plot data qualities.
      LOGICAL ZERO           ! Plot zero level.
      LOGICAL SELFLG         ! Select line type.
      LOGICAL RELFLG         ! Reset line type.

*  External References:
      INTEGER DQ_FIND        ! Look up data quality.

*  Local Variables:
      REAL XHIST( MAXPOINT ) ! X-axis histogram array.
      REAL DXL               ! Left hand histogram bin size.
      REAL DXR               ! Right hand histogram bin size.
      REAL X1                ! XLIM( 1 ).
      REAL X2                ! XLIM( 2 ).
      REAL YGAP              ! 5% gap between Y-axis data and axes.
      REAL YHIST( MAXPOINT ) ! Y-axis histogram array.
      REAL Y1                ! YLIM( 1 ).
      REAL Y2                ! YLIM( 2 ).

      INTEGER COUNT          ! Histogram point count.
      INTEGER FIRST          ! Index of first plot point in X/Y.
      INTEGER ICUR           ! Current index in X/Y.
      INTEGER INEXT          ! Next index in X/Y.
      INTEGER IPREV          ! Previous index in X/Y.
      INTEGER LAST           ! Index of last plot point in X/Y.
*.

*   Draw axes.
      CALL GRF_AXES( X1, X2, Y1, Y2, YGAP, SLABEL, NSL )

      CALL GRF_XYIND( MASK, NPOINT, X, Y, DQ, XLIM, YLIM, FIRST, LAST )

      IF ( LAST .GT. FIRST ) THEN

*      Initialise history for first bin.
         ICUR = FIRST
         IPREV = FIRST
         INEXT = DQ_FIND( MASK, DQ( ICUR + 1 ), LAST - ICUR ) + ICUR

*      Set plot line type and colour.
         IF ( SELFLG ) THEN
            CALL GRF_SELINE( 0, 0 )
         END IF

*      Cycle through bins, avoiding those with bad data quality.
         COUNT = 0

         DO WHILE ( .TRUE. )

*         First bin width symmetric from right hand spacings.
            IF ( IPREV .EQ. ICUR ) THEN
               DXR = ( X( INEXT ) - X( ICUR ) ) /
     :               ( REAL( INEXT - ICUR ) * 2.0 )
               DXL = DXR

*         Last bin width symmetric from left hand spacings.
            ELSE IF ( ICUR .EQ. INEXT ) THEN
               DXL = DXR

*         Intermediate bin width based on spacings either side.
            ELSE
               DXL = DXR
               DXR = ( X( INEXT ) - X( ICUR ) ) /
     :               ( REAL( INEXT - ICUR ) * 2.0 )
            END IF

            IF ( IPREV .EQ. ICUR - 1 ) THEN

*            Increment histogram array and plot array if full.
               COUNT = COUNT + 1
               XHIST( COUNT ) = X( ICUR ) - DXL
               YHIST( COUNT ) = Y( ICUR )

*            Load the bin plateau.
               COUNT = COUNT + 1
               XHIST( COUNT ) = X( ICUR ) + DXR
               YHIST( COUNT ) = Y( ICUR )

*            Plot if COUNT = MAXPOINT.
               IF ( COUNT .EQ. MAXPOINT ) THEN
                  CALL AGCURV( XHIST, 1, YHIST, 1, COUNT, 0 )
                  XHIST( 1 ) = XHIST( COUNT )
                  YHIST( 1 ) = YHIST( COUNT )
                  COUNT = 1
               END IF

            ELSE

*            Plot histogram array and then begin new loading.
               IF ( COUNT .GT. 0 ) THEN
                  CALL AGCURV( XHIST, 1, YHIST, 1, COUNT, 0 )
               END IF

*            Increment histogram array.
               COUNT = 1
               XHIST( COUNT ) = X( ICUR ) - DXL
               YHIST( COUNT ) = Y( ICUR )

*            Load the bin plateau.
               COUNT = COUNT + 1
               XHIST( COUNT ) = X( ICUR ) + DXR
               YHIST( COUNT ) = Y( ICUR )
            END IF

*         Increment bin indices.
            IPREV = ICUR
            ICUR = INEXT

            IF ( ICUR .LT. LAST ) THEN
               INEXT = DQ_FIND( MASK, DQ( ICUR + 1), LAST - ICUR) + ICUR

            ELSE
               GO TO 100
            END IF
         END DO
 100     CONTINUE

*      Finish plotting.
         IF ( COUNT .GT. 0 ) THEN
            CALL AGCURV( XHIST, 1, YHIST, 1, COUNT, 0 )
         END IF
      END IF

*   Draw data quality flags.
      IF ( QUAL ) THEN
         CALL GRF_DRQUAL( NPOINT, X, Y, DQ )
      END IF

*   Draw a base line at zero - DRZERO.
      IF ( ZERO ) THEN
         IF ( Y1.LT.0 .AND. Y2.GT.0 ) THEN
            CALL GRF_SELINE( LINTYP( 2 ), TIAXES )
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
