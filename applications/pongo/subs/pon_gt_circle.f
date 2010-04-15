      SUBROUTINE PON_GT_CIRCLE( PROJECTION, RA0, DEC0, RA1, DEC1, RA2,
     :                          DEC2, ACUTE, STATUS )
*+
*  Name:
*     PON_GT_CIRCLE

*  Purpose:
*     Draw an arc of a great circle between two points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_GT_CIRCLE( PROJECTION, RA0, DEC0, RA1, DEC1, RA2, DEC2,
*    :                    ACUTE, STATUS )

*  Description:
*     Either the small great circle or the large great cirle between
*     the points (RA1,DEC1) and (RA2,DEC2) is drawn in the projection
*     type PROJECTION, with centre (RA0,DEC0).

*  Arguments:
*     PROJECTION = INTEGER (Given)
*        The projection code to be passed to the PROJ_ routines.
*     RA0 = DOUBLE PRECISION (Given)
*        Longitude centre of the projection (radians).
*     DEC0 = DOUBLE PRECISION (Given)
*        Latitude centre of the projection (radians).
*     RA1 = DOUBLE PRECISION (Given)
*        Longitude of 1st point (radians).
*     DEC1 = DOUBLE PRECISION (Given)
*        Latitude of 1st point (radians).
*     RA2 = DOUBLE PRECISION (Given)
*        Longitude of 2nd point (radians).
*     DEC2 = DOUBLE PRECISION (Given)
*        Latitude of 2nd point (radians).
*     ACUTE = LOGICAL (Given)
*        If TRUE, the smaller of the two possible great circle arcs
*        is drawn.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     3-JUN-1994 (PDRAPER):
*        Modifed type casts to be explicit.
*     16-JUN-1994 (PDRAPER):
*        Set parameters INCR and DECR. Used 2.001 and 0.501 as these values
*        are used in GRID. They were previously unassigned.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      INTEGER PROJECTION

      DOUBLE PRECISION RA0
      DOUBLE PRECISION DEC0
      DOUBLE PRECISION RA1
      DOUBLE PRECISION DEC1
      DOUBLE PRECISION RA2
      DOUBLE PRECISION DEC2

      LOGICAL ACUTE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DVDV  ! [internal_description]

*  Local Variables:
      LOGICAL DRAW               ! Whether to draw this line

      REAL STEPMAX               ! Maximum step to be taken on plot
      REAL XMINP                 ! Plotter limit
      REAL XMAXP                 ! Plotter limit
      REAL YMINP                 ! Plotter limit
      REAL YMAXP                 ! Plotter limit

      DOUBLE PRECISION DECR      ! Decrement factor for stepsize
      DOUBLE PRECISION DECT      ! Spherical position on great circle
      DOUBLE PRECISION INCR      ! Increment factor for stepsize
      DOUBLE PRECISION L         ! Projection plane coordinate
      DOUBLE PRECISION LLAST     ! Last value of L
      DOUBLE PRECISION LINR      ! Denotes whether point is within
                                 ! viewing surface
      DOUBLE PRECISION LMDIST    ! Step length on projection plane
      DOUBLE PRECISION LOS       ! Step length
      DOUBLE PRECISION M         ! Projection plane coordinate
      DOUBLE PRECISION MLAST     ! Last value of M
      DOUBLE PRECISION MINR      ! Denotes whether point is within
                                 ! viewing surface
      DOUBLE PRECISION MOS       ! Step length
      DOUBLE PRECISION POS( 3 )  ! Cartesian position on great circle
      DOUBLE PRECISION RAT       ! Spherical position on great circle
      DOUBLE PRECISION RMAT( 3, 3 ) ! Rotation matrix
      DOUBLE PRECISION SEP       ! separation between the specifier poin
      DOUBLE PRECISION STPAMAX   !
      DOUBLE PRECISION STPAMIN   !
      DOUBLE PRECISION THEMAX    ! Maximum angle on great circle
      DOUBLE PRECISION THESTEP   ! Angular step along great circle
      DOUBLE PRECISION THETA     ! Angle along great circle
      DOUBLE PRECISION V1( 3 )   ! 1st Cartesian vector
      DOUBLE PRECISION V2( 3 )   ! 2nd Cartesian vector
      DOUBLE PRECISION VT( 3 )   ! Temporary vector
      DOUBLE PRECISION ZMOD      ! Modulus of Z-axis

*  Local Data:
      DATA INCR / 2.001D0 /
      DATA DECR / 0.501D0 /

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  Determine the scale of the plot.
      CALL PGQWIN( XMINP, XMAXP, YMINP, YMAXP )
      STEPMAX = ( ( XMAXP-XMINP ) + ( YMAXP-YMINP ) )/10.0
      STPAMAX = DBLE( STEPMAX ) /10.0D0
      STPAMIN = DBLE( STPAMAX ) /5.0D0

*  Get cartesian vectors.
      CALL SLA_DCS2C( RA1, DEC1, V1 )
      CALL SLA_DCS2C( RA2, DEC2, V2 )

*  Calculate the separation (think about rejection).
      SEP = ACOS( SLA_DVDV( V1, V2 ) )

*  Calculate Z-axis.
      CALL SLA_DVXV( V1, V2, VT )

*  And normalise it.
      CALL SLA_DVN( VT, RMAT( 1, 3 ), ZMOD )

*  Calculate the Y-axis.
      CALL SLA_DVXV( RMAT( 1, 3 ), V1, RMAT( 1, 2 ) )
      RMAT( 1, 1 ) = V1( 1 )
      RMAT( 2, 1 ) = V1( 2 )
      RMAT( 3, 1 ) = V1( 3 )

*  Draw the first point.
      CALL PROJ_CONVPTLM( PROJECTION, RA0, DEC0, RA1, DEC1, L, M,
     :                    STATUS )

      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL PGMOVE( REAL( L ), REAL( M ) )
         LLAST = L
         MLAST = M
         DRAW = .TRUE.
      ELSE
         DRAW = .FALSE.
      END IF

*  Initialize the loop constants.
      VT( 3 ) = 0.0D+00

      IF ( ACUTE ) THEN
         THETA = 0.0D+00
         THEMAX = SEP
      ELSE
         THETA = SEP
         THEMAX = D2PI
         SEP = D2PI - SEP
      END IF

      THESTEP = SEP/100.0D+00

      DO WHILE ( THETA.LE.THEMAX )

*     Coordinates of vector in new frame.
         VT( 1 ) = COS( THETA )
         VT( 2 ) = SIN( THETA )

*     Rotate vector to normal coordinates (could speed up).
         CALL SLA_DMXV( RMAT, VT, POS )

*     Retrieve normal spherical coordinates.
         CALL SLA_DCC2S( POS, RAT, DECT )
         CALL PROJ_CONVPTLM( PROJECTION, RA0, DEC0, RAT, DECT, L, M,
     :                       STATUS )

         IF ( STATUS.EQ.SAI__OK ) THEN

*        Calculate the step size.
            LOS = MAX( ABS( L-LLAST ), 1.0D-15 )
            MOS = MAX( ABS( M-MLAST ), 1.0D-15 )
            LMDIST = SQRT( MOS*MOS + LOS*LOS )

            IF ( .NOT.DRAW
     :           .OR. ( SIGN( 1.0D0, L )*SIGN( 1.0D0, LLAST)
     :                  .LT.0.0D0 .AND. LOS.GT.DBLE(STEPMAX)/2.0D0 )
     :           .OR. ( SIGN( 1.0D0, M )*SIGN( 1.0D0, MLAST )
     :                  .LT.0.0D0 .AND. MOS.GT.DBLE(STEPMAX)/2.0D0 )
     :           ) THEN

*           If the step is too large just move: 'wrap around'
*           projection case.
               CALL PGMOVE( REAL( L ), REAL( M ) )
               DRAW = .TRUE.
               LLAST = L
               MLAST = M
            ELSE
               LINR = ABS((L - DBLE(XMINP)) / DBLE(XMAXP-XMINP) - 0.5D0)
               MINR = ABS((M - DBLE(YMINP)) / DBLE(YMAXP-YMINP) - 0.5D0)

               IF ( LINR.GT.1.0D+00 .OR. MINR.GT.1.0D+00 ) THEN

*              Point outside plotting surface (with border).
                  CALL PGMOVE( REAL( L ), REAL( M ) )
                  THESTEP = SEP/40.0D+00
                  LLAST = L
                  MLAST = M
               ELSE IF ( LMDIST.GT.STPAMAX
     :                   .AND. LMDIST.LT.4.0D+00*DBLE(STEPMAX) ) THEN

*              Decrease step size.
                  THETA = THETA - THESTEP
                  THESTEP = THESTEP * DECR
               ELSE IF ( LMDIST.LT.STPAMIN ) THEN

*              Increase step size.
                  THETA = THETA - THESTEP
                  THESTEP = THESTEP * INCR
               ELSE

*              Just draw to this point.
                  CALL PGDRAW( REAL( L ), REAL( M ) )
                  LLAST = L
                  MLAST = M
               END IF
            END IF
         ELSE
            DRAW = .FALSE.
         END IF

         THETA = THETA + THESTEP
      END DO

      END
* $Id$
