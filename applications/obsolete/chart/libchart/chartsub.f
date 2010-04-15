       SUBROUTINE CHARTSUB( STATUS )
*+
*   This subroutine controls all the plotting which is done.
*   Most of the work is done by the subroutines called.
*   All information required is obtained from the Common
*   Blocks,hence there are no parameters.
*
*

*  Arguments:
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*  Altered T.N.Wilkins Manchester 17/1/89 to change use of pen 6
*  to pen 3, since GKS 7 seems to use only up to pen 5
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CROSS call
*     3-MAR-1993 (AJJB):
*        STATUS argument added to ELLIPSE call
*     4-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     8-MAR-1993 (Andrew Broderick (AJJB)):
*        Type conversions such as REAL() added to some calculations to
*        enable the routine to pass cleanly through FORCHECK.
*     11-MAR-1993 (AJJB):
*        Changed JSIGN (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.
*     23-MAY-1993 (AJJB):
*        Took second argument out of routine quadt and hence removed it
*        also from the call to quadt in this routine, as it was
*        redundant. The second argument was a character string
*        containing the current value of the SYMBOL parameter.
*-
*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'
      INCLUDE 'PLOTDAT'
*  Status:
      INTEGER STATUS             ! Global status
      DOUBLE PRECISION ARG2, ARG3 ! D.P. arguements for PROJ
      DOUBLE PRECISION RAO,DECO,RANEXT,DECNEXT
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      INTEGER DECD,DECM,DECS,NDECD,NDECM,NDECS
      INTEGER RAH,RAM,RAS,NRAH,NRAM,NRAS
      REAL    DIF,DIFRANGE,XX(40),YY(40),XXR(4),YYR(4)
      LOGICAL CLASH,DISPLEFT,DISPRIGHT
      COMMON/DISPS/CLASH,DISPLEFT,DISPRIGHT
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      CHARACTER JSIGN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   HW  is the Half Size in mm. of the actual Chart
*   HA  is the half width of the zone (in mm.) that the Chart is plotted
*   HH  is the half height of the chart zone

      HW = SIZEMM/2.0
      HA = XDIAM/2.0
      HH = YDIAM/2.0

*   Set Zone to size of Overall Chart Area

      CALL SGS_SELZ (IZBASE,ISTAT)
      CALL SGS_ZONE (0.0,XDIAM,0.0,YDIAM,IZAREA,ISTAT)
      CALL SGS_SW (-HA,HA,-HH,HH,ISTAT)

*   Set Zone to exact size of chart

      CALL SGS_ZONE (-HW+5.0,HW+5.0,-HW,HW,IZCHART,ISTAT)
      CALL SGS_SW (-HW,HW,-HW,HW,ISTAT)

*   Select Chart Zone IZCHART for plotting

      CALL SGS_SELZ (IZCHART,ISTAT)

      IF (GRID(1:1).EQ.'Y'.OR.GRID(1:1).EQ.'M') THEN

*   If ARGS Set Pen Colour

         IF (COLOUR) CALL SGS_SPEN (4)

         CALL PLOTGRID (AO,DO,SCALE,GRID,DIRECT, STATUS )

*
*   Clear out SGS Buffer
*
         CALL SGS_FLUSH


      ENDIF

*   If ARGS Set Pen Colour

      IF (COLOUR) CALL SGS_SPEN (2)

*   Draw a Rectangle the size of the Chart

      CALL QUADT (HW, STATUS )

*   If ARGS Set Pen Colour for Stars etc.

      IF (COLOUR) CALL SGS_SPEN (1)

*
*   Plot the Star symbols - circles, spots or crosses
*
      CLASH=.FALSE.
      DO 100 N = 1,NUM
         RAO = STAR(1,IP(N))
         DECO = STAR(2,IP(N))
         CALL PROJ(1,RAO,DECO,X,Y, STATUS )
         IF (DIRECT) THEN
            XPLOT = -(X/( REAL( RDSA ) * SCALE ))
         ELSE
            XPLOT =  (X/( REAL( RDSA ) * SCALE ))
         END IF
         YPLOT = Y/( REAL( RDSA ) * SCALE)
*
*   Finally check that the Star is in Plotted Field, may be out
*   due to Truncation
*
         IF (ABS(XPLOT).GT.SIZEMM/2.0.OR.ABS(YPLOT).GT.SIZEMM/2.0)
     :      GOTO 100
*
         IF (.NOT.NONS) THEN
            RMAG = FLOAT(NSTAR(1,IP(N)))/10.0
            RMAG = AMIN1(RMAG,12.0)
*
*   Factor for scaling cross sizes is related
*   to density of stars. Standard Factor (FAC)
*   is 270.0 for 100 stars on the chart.
*   This is increased with the square root of the
*   number, it is never less than 270.0
*
            FAC = AMAX1(270.0*SQRT(FLOAT(NUM))/10.0,270.0)
            R = ((13.5 - RMAG)*SIZEMM)/FAC
         ELSE
            R = SIZEMM/90.0
         ENDIF

         IF (NONS) THEN
            CALL NSSYMB(XPLOT,YPLOT,R, STATUS )
            DISPLEFT=.FALSE.
            DISPRIGHT=.FALSE.
         ELSE

*   If SAO/Perth Star clashes possible, sort out offsets for numbering
*   With the possibility of new catalogues further clashes may occur
*   so we test all the time for clashes. J.V.Carey 1984 April 11
*            IF ((CATRUN).AND.(ICAT2.EQ.2).AND.(ICAT3.EQ.3).AND.
*     :                                           (N.NE.NUM)) THEN

*   If clash already found set variables to displace second number to the

               IF (CLASH) THEN
                  DISPRIGHT=.TRUE.
                  DISPLEFT=.FALSE.
               ELSE

*   else check for a clash

                  DISPRIGHT=.FALSE.
                  DISPLEFT=.FALSE.
                  CALL CONV (1,DECO,0,JSIGN,DECD,DECM,DECS,FS, STATUS )
                  CALL CONV (2,RAO,0,JSIGN,RAH,RAM,RAS,FS, STATUS )
                  RANEXT=STAR(1,IP(N+1))
                  DECNEXT=STAR(2,IP(N+1))
                  CALL CONV (1,DECNEXT,0,JSIGN,NDECD,NDECM,NDECS,FS,
     :            STATUS )
                  CALL CONV (2,RANEXT,0,JSIGN,NRAH,NRAM,NRAS,FS, STATUS)
                  IF ((NDECD.EQ.DECD).AND.(NDECM.EQ.DECM).AND.
     :                (NRAH.EQ.RAH).AND.(NRAM.EQ.RAM).AND.
     :                (ABS(NDECS-DECS).LE.2)) THEN
                     DIF=FLOAT(ABS(NRAS-RAS))
                     DIFRANGE= REAL( 30.0 * COS( ABS(
     :                         DMAX1( DECO, DECNEXT ) ) ) )
                     IF (DIF.LE.DIFRANGE) THEN

*   Clash only possible if stars are within 2 secs of dec of each other
*   and 30*cos(dec) secs of each other in RA.

                        CLASH=.TRUE.
                        DISPLEFT=.TRUE.
                        DISPRIGHT=.FALSE.
                     ENDIF
                  ENDIF
               ENDIF
*            ELSE
*               DISPLEFT=.FALSE.
*               DISPRIGHT=.FALSE.
*            ENDIF
            IF (SYMBOL(1:4).EQ.'CIRC') THEN
               CALL SGS_CIRCL (XPLOT,YPLOT,R)
               CALL SGS_OPOLY
            ELSE IF (SYMBOL(1:4).EQ.'SPOT') THEN
               CALL SPOT(XPLOT,YPLOT,R, STATUS )
            ELSE
               CALL CROSS (XPLOT,YPLOT,R, STATUS )
            ENDIF
         ENDIF
         CALL NUMB(XPLOT,YPLOT,R,N, STATUS )
100   CONTINUE
      CLASH=.FALSE.
      DISPLEFT=.FALSE.
      DISPRIGHT=.FALSE.
*
*   Plot the supplied objects (if any)
*
      IF (SUPP) THEN

*   If ARGS Set Pen Colour

        IF (COLOUR) CALL SGS_SPEN (5)

        DO N=1,NUMSUPP
           CALL PROJ(1,OWNOBJ(1,N),OWNOBJ(2,N),X,Y, STATUS )
           IF (DIRECT) THEN
                XPLOT = -(X/( REAL( RDSA ) * SCALE ))
           ELSE
                XPLOT =  (X/( REAL( RDSA ) * SCALE ))
           END IF
           YPLOT =   Y/(REAL( RDSA ) * SCALE)
           R = SIZEMM/90.0
           CALL OBJPLT(XPLOT,YPLOT,R,N, STATUS )
        END DO
      ENDIF
*
*   Plot the Error Box
*   may be Quadrilateral with Corners Specified in QEBC
*   or may be Square with Two Sides  2 * EBS long.
*
*   If error boxes have been prompted for by SEARCH then
*
      IF (ERDIAM(1).GT.0.0) THEN
        IF (COLOUR) CALL SGS_SPEN(3)
        XER = ERDIAM(1)/SCALE
        YER = ERDIAM(2)/SCALE
        CALL ELLIPSE(XER,YER,ERDIAM(3),XX,YY,40, STATUS )
        CALL SGS_BPOLY(XX(1),YY(1))
        DO I = 2,40
          CALL SGS_APOLY(XX(I),YY(I))
        ENDDO
        CALL SGS_OPOLY

      ELSE IF (ERDIAM(1).LT.0.0) THEN
        IF (COLOUR) CALL SGS_SPEN(3)
        XER = ERDIAM(1)/SCALE
        YER = ERDIAM(2)/SCALE
        CALL ELLIPSE(XER,YER,ERDIAM(3),XXR,YYR,4, STATUS )
        CALL SGS_BPOLY(XXR(1),YYR(1))
        DO I = 2,4
          CALL SGS_APOLY(XXR(I),YYR(I))
        ENDDO
        CALL SGS_APOLY(XXR(1),YYR(1))
        CALL SGS_OPOLY

*   If QUAD then First Convert RA's and Dec's to X,Y's

      ELSE IF (QEBOX) THEN

*   If ARGS Set Pen Colour

        IF (COLOUR) CALL SGS_SPEN (3)

        DO K=1,4
           ARG2 = DBLE( QEBC((K-1)*2 +1 )  ) ! Assign QEBC elements to
           ARG3 = DBLE( QEBC(K*2) )  ! D.P. vars as PROJ takes D.P args
           CALL PROJ(1, ARG2, ARG3 ,X ,Y , STATUS )
           QEBC((K-1)*2 +1 ) = REAL( ARG2 ) ! Move returned D.P values
           QEBC(K*2) = REAL( ARG3 ) ! back to original QEBC elements
           IF (DIRECT) THEN
              QEBC((K-1)*2 +1) = -X/( REAL( RDSA ) * SCALE)
           ELSE
              QEBC((K-1)*2 +1) =  X/( REAL( RDSA ) * SCALE)
           END IF
           QEBC(K*2)        = Y/( REAL( RDSA ) * SCALE)
        END DO

        CALL SGS_BPOLY (QEBC(1),QEBC(2))
        DO J = 3,9,2
           IF (J.EQ.9) THEN
              CALL SGS_APOLY (QEBC(J-8),QEBC(J-7))
           ELSE
              CALL SGS_APOLY (QEBC(J),QEBC(J+1))
           ENDIF
        END DO
        CALL SGS_OPOLY

      ELSE IF (ERRB.GT.1E-6) THEN

*   If ARGS Set Pen Colour

        IF (COLOUR) CALL SGS_SPEN (3)

        CALL SGS_CIRCL (0.0,0.0,ERRB/SCALE)
        CALL SGS_OPOLY

      ENDIF

*   Put Tick Marks at Plot Centre
*   but only if asked to do so

      IF (CENCROS) THEN

*   First, If ARGS Set Pen Colour

         IF (COLOUR) CALL SGS_SPEN (3)

         SHIFT=SIZEMM/60.0
         TICK=3.0*SHIFT

         CALL SGS_BPOLY (0.0,SHIFT)
         CALL SGS_APOLY (0.0,TICK)
         CALL SGS_OPOLY
         CALL SGS_BPOLY (SHIFT,0.0)
         CALL SGS_APOLY (TICK,0.0)
         CALL SGS_OPOLY
         CALL SGS_BPOLY (0.0,-SHIFT)
         CALL SGS_APOLY (0.0,-TICK)
         CALL SGS_OPOLY
         CALL SGS_BPOLY (-SHIFT,0.0)
         CALL SGS_APOLY (-TICK,0.0)
         CALL SGS_OPOLY
      END IF

*   Clear Out Buffer containing all Plotting Output.

         CALL SGS_FLUSH

*   Release Plotting Zones

         CALL SGS_SELZ (IZONID,ISTAT)
         CALL SGS_RELZ (IZCHART,ISTAT)
         CALL SGS_RELZ (IZAREA,ISTAT)
         CALL SGS_RELZ (IZBASE,ISTAT)
         CALL SGS_FLUSH

      END
