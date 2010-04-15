      SUBROUTINE GRID( STATUS )
*+
*  Name:
*     GRID

*  Purpose:
*     Draw a coordinate grid at specified intervals.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw a grid in the current projection at user specified intervals
*     in spherical coordinates. The intervals, start and end values
*     should all be specified in degrees. The defaults for the grid
*     separations normally produce desirable effects for all sky plots.
*     In specifying the grid intervals it is sometimes necessary to
*     take account of rounding errors that might occur, and to bear in
*     mind that in some geometries a single point on the celestial
*     sphere maps onto two points on the projected coordinates -- some
*     care is needed to ensure that the whole grid is drawn.

*  Usage:
*     grid [phimin] [phimax] [phistep] [themin] [themax] [thestep]

*  ADAM Parameters:
*     PHIMIN = _DOUBLE (Read and Write)
*        The start longitude in degrees for the coordinate grid.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     PHIMAX = _DOUBLE (Read and Write)
*        The end longitude in degrees for the coordinate grid.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 360.0.
*     PHISTEP = _DOUBLE (Read and Write)
*        The spacing between longitude grid lines in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 30.0.
*     THEMIN = _DOUBLE (Read and Write)
*        The start latitude in degrees for the coordinate grid.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to -90.0.
*     THEMAX = _DOUBLE (Read and Write)
*        The end latitude in degrees for the coordinate grid.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 90.0.
*     THESTEP = _DOUBLE (Read and Write)
*        The spacing between latitude grid lines in degrees.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 10.0.
*     PROJECTION = _LITERAL (Read)
*        The geometry to be used to plot the grid.  This is explained
*        in more detail in the section on projections.  Allowed values:
*        "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF", "MERCATOR" and
*        "STG".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.
*     RACENTRE = _LITERAL (Read)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*         This parameter is not specified on the command line. The
*         value of the global parameter PONGO_RACENTRE is used. If
*         PONGO_RACENTRE is not defined, the default value "0" is used.
*     DECCENTRE = _LITERAL (Read)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.

*  Notes:
*     - It is sometimes necessary to specify the grid intervals in a
*     manner which avoids rounding errors to obtain the last grid line.
*     e.g 9.9999 instead of 10. You may also need to sometimes draw
*     the grid twice using say 30.00001 as well as 29.99999 to get
*     this final line.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     22-JUN-1990 (JBVAD::PAH):
*        Improved algorithm so that the step size is adapted according
*        to the size of the viewport
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Added type casts for DBLE and REAL conversions, sorted out
*        illegal use of STATUS. Added checks for device opened.
*     2-MAY-1997 (PDRAPER):
*        Added changes to force lines to terminate at PHIMAX and THEMAX.
*        This makes latitude lines look much better.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT is opened

*  Local Variables:
      LOGICAL DRAW               ! switchs drawing of current line off
                                 ! and on
      LOGICAL ONCEMR             ! Whether to terminate line

      INTEGER PROJECTION         ! Projection type
      INTEGER ICOUNT             ! counter to chenck that too many
                                 ! decrements of the step size have not
                                 ! occured in a row
      INTEGER LSTAT              ! Local status flag

      REAL XMINP, XMAXP, YMINP, YMAXP ! new window settings
      REAL STEPMAX               ! maximum step that can be taken on the
                                 ! plotting surface for the line to be
                                 ! drawn

      DOUBLE PRECISION PHIMIN, PHIMAX, PHISTEP, THEMIN, THEMAX,
     :  THESTEP                  ! [local_variable_description]
      DOUBLE PRECISION RA0, DEC0 ! Projection centres
      DOUBLE PRECISION PHI, THETA ! spherical coordinates
      DOUBLE PRECISION LOS, MOS  ! last L and M
      DOUBLE PRECISION LMDIST    ! Current step length on plotting surface
      DOUBLE PRECISION STEP      ! step in each of the spherical coordinates
      DOUBLE PRECISION L, M      ! projective coordinates
      DOUBLE PRECISION FACT      ! division factor for max step size
      DOUBLE PRECISION INCR, DECR ! increment and decrement factors when
                                 ! adjusting stepsize
      DOUBLE PRECISION STPAMAX,STPAMIN ! the
      DOUBLE PRECISION LLAST, MLAST ! Last values of L and M
      DOUBLE PRECISION LINR,MINR ! denote whether point is within
                                 ! viewing surface

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is a graphics device open?
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Get the grid specification in spherical coordinates.
      CALL PAR_GET0D( 'PHIMIN', PHIMIN, STATUS )
      CALL PAR_GET0D( 'PHIMAX', PHIMAX, STATUS )
      CALL PAR_GET0D( 'PHISTEP', PHISTEP, STATUS )
      CALL PAR_GET0D( 'THEMIN', THEMIN, STATUS )
      CALL PAR_GET0D( 'THEMAX', THEMAX, STATUS )
      CALL PAR_GET0D( 'THESTEP', THESTEP, STATUS )

*  Get the projection type and centre.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                   PROJECTION, RA0, DEC0, STATUS )
      IF ( PROJECTION .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GRID_NONONE',
     :'This command can only draw grids for projections, use the '//
     :'boxframe command for ordinary plots', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find the current world coordinates in PGPLOT.
      CALL PGQWIN( XMINP, XMAXP, YMINP, YMAXP )

*  Calculate the maximum step size in world coordinates that should be
*  allowed so that the plot remains smooth.
      FACT = 5
      INCR = 2.001
      DECR = 0.501D0
      ICOUNT = 0
      STEPMAX = REAL(((XMAXP - XMINP) + (YMAXP - YMINP))/ 2/ REAL(FACT))
      STPAMAX = DBLE ( STEPMAX / 10 )
      STPAMIN = STPAMAX / 5

*  Set up the projection scales.
      CALL PROJ_SETPAR( 1D0, 1D0, 0D0 )
      PHI = PHIMIN
      LMDIST = 1

*  Draw the longitude grid.
      DO WHILE ( PHI-PHIMAX .LT. 1D-5 )
         LSTAT = SAI__OK
         DRAW=.FALSE.

*  Calculate the longitude step.
        STEP=(THEMAX-THEMIN)/100
        THETA=THEMIN
        CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0, PHI*DDEG2R,
     :                      THETA*DDEG2R, LLAST, MLAST, LSTAT )
        ONCEMR = .FALSE.
        DO WHILE( THETA-THEMAX.LT.1D-6 .OR. ONCEMR )
           LSTAT = SAI__OK

*  Calculate the projected coordinates.
           CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0, PHI*DDEG2R,
     :                         THETA*DDEG2R, L, M, LSTAT )

*  If the coordinates are legal then.
           IF ( LSTAT.EQ.SAI__OK ) THEN

*  Determine the step taken in projective coordinates.
              LOS=MAX(ABS(L-LLAST),1D-15)
              MOS=MAX(ABS(M-MLAST),1D-15)
              LMDIST=SQRT(MOS*MOS+LOS*LOS)

              IF(.NOT.DRAW
     :          .OR.  (SIGN(1D0,L)*SIGN(1D0,LLAST).LT.0
     :                           .AND. LOS.GT.STEPMAX/2)
     :          .OR.(SIGN(1D0,M)*SIGN(1D0,MLAST).LT.0
     :                           .AND. MOS.GT.STEPMAX/2)
     :          ) THEN

*   'Wrap around' projection case.
                 CALL PGMOVE(REAL(L), REAL(M))
                 DRAW=.TRUE.
                 LLAST=L
                 MLAST=M
              ELSE
                 LINR=ABS((L-DBLE(XMINP))/DBLE((XMAXP-XMINP))-0.5)
                 MINR=ABS((M-DBLE(YMINP))/DBLE((YMAXP-YMINP))-0.5)

                 IF(LINR.GT.1.0D0 .OR. MINR.GT.1.0D0) THEN

*   Point outside plotting surface (with border).
                    CALL PGMOVE(REAL(L), REAL(M))
                    STEP=(THEMAX-THEMIN)/50
                    LLAST=L
                    MLAST=M
                 ELSE

                    IF(LMDIST.GT.STPAMAX .AND. LMDIST.LT.4*STEPMAX)
     :                                              THEN

*   Decrease step size.
                       THETA=THETA-STEP
                       STEP=STEP*DECR
                    ELSEIF(LMDIST.LT.STPAMIN) THEN

*   Increase step size.
                       THETA=THETA-STEP
                       STEP=STEP*INCR
                    ELSE

*   Just draw to this point.
                       CALL PGDRAW(REAL(L), REAL(M))
                       LLAST=L
                       MLAST=M
                    ENDIF
                 ENDIF
              ENDIF
           ELSE
              DRAW=.FALSE.
           ENDIF
           THETA=THETA+STEP
           IF ( THETA-THEMAX.GT.1D-6 .AND. .NOT. ONCEMR ) THEN
              THETA = THEMAX
              ONCEMR = .TRUE.
           ELSE
              ONCEMR = .FALSE.
           END IF
        ENDDO
        PHI=PHI+PHISTEP
      ENDDO

*  Draw the latitude grid.
      THETA=THEMIN
      LMDIST=1

      DO WHILE(THETA-THEMAX.LT.1D-5)
        LSTAT = SAI__OK
        DRAW=.FALSE.
        ICOUNT=0
        STEP=(PHIMAX-PHIMIN)/100
        PHI=PHIMIN
        CALL PROJ_CONVPTLM(PROJECTION-1, RA0,
     :             DEC0, PHI*DDEG2R, THETA*DDEG2R,
     :             LLAST, MLAST, LSTAT )
        ONCEMR = .FALSE.
        DO WHILE(PHI-PHIMAX.LT.1D-6 .OR. ONCEMR)
           LSTAT = SAI__OK
           CALL PROJ_CONVPTLM(PROJECTION-1, RA0,
     :             DEC0, PHI*DDEG2R, THETA*DDEG2R, L, M, LSTAT )

           IF ( LSTAT .EQ.SAI__OK ) THEN
              LOS=MAX(ABS(L-LLAST),1D-15)
              MOS=MAX(ABS(M-MLAST),1D-15)
              LMDIST=SQRT(MOS*MOS+LOS*LOS)

              IF(.NOT.DRAW
     :          .OR.  (SIGN(1D0,L)*SIGN(1D0,LLAST).LT.0
     :                           .AND. LOS.GT.STEPMAX/2)
     :          .OR.(SIGN(1D0,M)*SIGN(1D0,MLAST).LT.0
     :                           .AND. MOS.GT.STEPMAX/2)
     :          ) THEN

*   'Wrap around' projection case.
                 CALL PGMOVE(REAL(L), REAL(M))
                 DRAW=.TRUE.
                 LLAST=L
                 MLAST=M
              ELSE
                 LINR=ABS((L-DBLE(XMINP))/DBLE((XMAXP-XMINP))-0.5)
                 MINR=ABS((M-DBLE(YMINP))/DBLE((YMAXP-YMINP))-0.5)

                 IF(LINR.GT.1.0D0 .OR. MINR.GT.1.0D0) THEN

*   Point outside plotting surface (with border).
                    CALL PGMOVE(REAL(L), REAL(M))
                    STEP=(PHIMAX-PHIMIN)/50
                    LLAST=L
                    MLAST=M
                 ELSE

                    IF(ICOUNT.GT.10) THEN
                       CALL PGMOVE(REAL(L), REAL(M))
                       LLAST=L
                       MLAST=M
                       ICOUNT=0
                    ELSE

                       IF(LMDIST.GT.STPAMAX.AND.LMDIST.LT.4*STEPMAX)
     :                                              THEN

*   Decrease step size.
                          PHI=PHI-STEP
                          STEP=STEP*DECR
                          ICOUNT=ICOUNT+1
                       ELSEIF(LMDIST.LT.STPAMIN) THEN

*   Increase step size.
                          PHI=PHI-STEP
                          STEP=STEP*INCR
                       ELSE

*   Just draw to this point.
                          CALL PGDRAW(REAL(L), REAL(M))
                          LLAST=L
                          MLAST=M
                          ICOUNT=0
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
           ELSE
              DRAW=.FALSE.
           ENDIF
           PHI=PHI+STEP
           IF ( PHI-PHIMAX.GT.1D-6 .AND. .NOT. ONCEMR ) THEN
              PHI = PHIMAX
              ONCEMR = .TRUE.
           ELSE
              ONCEMR = .FALSE.
           END IF
        ENDDO
        THETA=THETA+THESTEP
      ENDDO

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'GRID_END',
     :                              'GRID: Unable to draw ' //
     :                              'coordinate grid.', STATUS )

      END
* $Id$
