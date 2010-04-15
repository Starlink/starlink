      SUBROUTINE GREAT_CIRCLE( PROJECTION, RA0, DEC0, RA1, DEC1, RA2,
     :                         DEC2, ACUTE, STATUS )
*+
*  Name:
*     GREAT_CIRCLE

*  Purpose:
*     Draw an arc of a great circle between two points on the celestial
*     sphere

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GREAT_CIRCLE( PROJECTION, RA0, DEC0, RA1, DEC1, RA2, DEC2,
*    :   ACUTE, STATUS )

*  Description:
*     Draws the either the small greate gircle (acute=.true.)  or the
*     large great cirle between the points RA1,DEC1 and RA2,DEC2 (in
*     radians) in the projection type PROJECTION with centre RA0,DEC0

*  Arguments:
*     PROJECTION = INTEGER (Given)
*        the projection code to be passes to the proj library
*     RA0 = DOUBLE PRECISION (Given)
*        longitude centre of the projection (radians)
*     DEC0 = DOUBLE PRECISION (Given)
*        Latitude centre of the projection (radians)
*     RA1 = DOUBLE PRECISION (Given)
*        longitude of 1st point (radians)
*     DEC1 = DOUBLE PRECISION (Given)
*        latitude of 1st point (radians)
*     RA2 = DOUBLE PRECISION (Given)
*        longitude of 2nd point (radians)
*     DEC2 = DOUBLE PRECISION (Given)
*        latitude of 2nd point (radians)
*     ACUTE = LOGICAL (Given)
*        if true then the smaller of the two possible great circle arcs
*        is drawn
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     PAH: Paul Harrison (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (PAH):
*        Original version.
*     2-JUN-1994 (PDRAPER):
*        Added appropriate DBLE casts and DBLE constants.
*     20-JUN-1994 (PDRAPER):
*        Removed inappropriate STATUS uses (replaced with LSTAT)
*     2-MAY-1997 (PDRAPER):
*        Now always reaches the end of the circle.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER PROJECTION
      DOUBLE PRECISION RA0, DEC0, RA1, DEC1, RA2, DEC2
      LOGICAL ACUTE

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
      DOUBLE PRECISION INCR, DECR ! increment and decrement factors when
                                 ! adjusting stepsize
      PARAMETER (INCR=2.001D0, DECR= 0.501D0)

*  Local Variables:
      DOUBLE PRECISION L, M      ! coordinates in projection plane
      DOUBLE PRECISION LINR,MINR ! denote whether point is within
      DOUBLE PRECISION LLAST, MLAST ! Last values of L and M
      DOUBLE PRECISION LMDIST    ! step length on projection plane
      DOUBLE PRECISION LOS, MOS  ! step lengths in each direction
      DOUBLE PRECISION POS(3)    ! CARTESIAN position on great circle
      DOUBLE PRECISION RAT, DECT ! spherical position on great circle
      DOUBLE PRECISION RMAT( 3, 3 ) ! Rotation matrix
      DOUBLE PRECISION SEP       ! separation between specifier points
      DOUBLE PRECISION STPAMAX,STPAMIN ! the
      DOUBLE PRECISION THEMAX    ! max angle on great circle
      DOUBLE PRECISION THESTEP   ! angular step along great circle
      DOUBLE PRECISION THETA     ! angle along great circle
      DOUBLE PRECISION V1( 3 )   ! Cartesian 1st vector
      DOUBLE PRECISION V2( 3 )   ! Cartesian 2nd vector
      DOUBLE PRECISION VT( 3 )   ! TEMPORARY VECTOR
      DOUBLE PRECISION ZMOD      ! modulus of z axis
      LOGICAL DRAW               ! determines whether to draw this line
      LOGICAL ONCEMR             ! Do one more step to reach end
      REAL STEPMAX               ! maximum step to be taken on plot
      REAL XMINP,XMAXP,YMINP,YMAXP ! plotter limits
                                 ! viewing surface
      INTEGER LSTAT              ! Local status value

*  External References:
      EXTERNAL SLA_DVDV
      DOUBLE PRECISION SLA_DVDV

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out the scale of the plot
      CALL PGQWIN(XMINP, XMAXP, YMINP, YMAXP)
      STEPMAX=((XMAXP-XMINP)+(YMAXP-YMINP))/2/5
      STPAMAX = DBLE( STEPMAX / 10 )
      STPAMIN = STPAMAX / 5

*  get cartesian vectors
      CALL SLA_DCS2C(RA1, DEC1, V1)
      CALL SLA_DCS2C(RA2, DEC2, V2)

*  calculate the separation (think about rejection)
      SEP=ACOS(SLA_DVDV(V1, V2))

*  calculate z axis
      CALL SLA_DVXV(V1, V2, VT)

*  and normalise it
      CALL SLA_DVN(VT, RMAT(1,3), ZMOD)

*  calculate the y axis
      CALL SLA_DVXV(RMAT(1,3),V1,RMAT(1,2))
      RMAT(1,1)=V1(1)
      RMAT(2,1)=V1(2)
      RMAT(3,1)=V1(3)

*  draw first point
      LSTAT = SAI__OK
      CALL PROJ_CONVPTLM(PROJECTION, RA0, DEC0, RA1, DEC1, L, M, LSTAT)
      IF ( LSTAT .EQ. SAI__OK ) THEN
         CALL PGMOVE(REAL(L), REAL(M))
         LLAST=L
         MLAST=M
         DRAW=.TRUE.
      ELSE
         DRAW=.FALSE.
      END IF

*  initialize the loop constants
      VT(3)=0D0
      IF(ACUTE) THEN
         THETA=0D0
         THEMAX=SEP
      ELSE
         THETA=SEP
         THEMAX=D2PI
         SEP=D2PI-SEP
      ENDIF
      THESTEP=SEP/100

      ONCEMR = .FALSE.
      DO WHILE ( THETA-THEMAX .LT. 1.0D-5 .OR. ONCEMR )

*  coordinates of vector in new frame
         VT(1)=COS(THETA)
         VT(2)=SIN(THETA)

*  rotate vector to normal coordinates (could speed up)
         CALL SLA_DMXV(RMAT, VT, POS)

*  retrieve normal spherical coordinates
         CALL SLA_DCC2S(POS,RAT,DECT)
         LSTAT = SAI__OK
         CALL PROJ_CONVPTLM(PROJECTION, RA0, DEC0, RAT, DECT, L, M,
     :                      LSTAT )
         IF ( LSTAT .EQ. SAI__OK ) THEN

*  calculate the step size
              LOS=MAX(ABS(L-LLAST),1D-15)
              MOS=MAX(ABS(M-MLAST),1D-15)
              LMDIST=SQRT(MOS*MOS+LOS*LOS)
              IF(.NOT.DRAW
     :          .OR.  (SIGN(1D0,L)*SIGN(1D0,LLAST).LT.0
     :                           .AND. LOS.GT.STEPMAX/2)
     :          .OR.(SIGN(1D0,M)*SIGN(1D0,MLAST).LT.0
     :                           .AND. MOS.GT.STEPMAX/2)
     :          ) THEN

*  if step to large just move, 'wrap around' projection case
                 CALL PGMOVE(REAL(L), REAL(M))
                 DRAW=.TRUE.
                 LLAST=L
                 MLAST=M
              ELSE
                 LINR=ABS((L-DBLE(XMINP))/DBLE((XMAXP-XMINP)-0.5))
                 MINR=ABS((M-DBLE(YMINP))/DBLE((YMAXP-YMINP)-0.5))
                 IF(LINR.GT.1 .OR. MINR.GT.1) THEN
*   point outside plotting surface (with border)
                    CALL PGMOVE(REAL(L), REAL(M))
                    THESTEP=SEP/40
                    LLAST=L
                    MLAST=M
                 ELSE
                    IF(LMDIST.GT.STPAMAX .AND. LMDIST.LT.4*STEPMAX)
     :                                              THEN

*   decrease step size
                       THETA=THETA-THESTEP
                       THESTEP=THESTEP*DECR
                    ELSEIF(LMDIST.LT.STPAMIN) THEN
*   increase step size
                       THETA=THETA-THESTEP
                       THESTEP=THESTEP*INCR
                    ELSE
*   just draw to this point
                       CALL PGDRAW(REAL(L), REAL(M))
                       LLAST=L
                       MLAST=M
                    ENDIF
                 ENDIF
              ENDIF
         ELSE
            DRAW=.FALSE.
         ENDIF

*   If this is the last step then make it reach THEMAX.
         THETA=THETA+THESTEP
         IF ( THETA-THEMAX .GT. 1.0D-5 .AND. .NOT. ONCEMR ) THEN
            ONCEMR = .TRUE.
            THETA = THEMAX
         ELSE
            ONCEMR = .FALSE.
         END IF
      END DO

      END
* $Id$
