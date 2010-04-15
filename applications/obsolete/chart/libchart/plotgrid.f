      SUBROUTINE PLOTGRID(AO,DO,SCALE,GRID,DIRECT, STATUS )
*+
*   Draw a Grid over the Chart showing Lines of
*   equal RA and Dec.
*
*   Gets
*   ----
*     AO     - RA of Centre in Rads (at Output Equinox)
*     DO     - DEC of Centre in Rads.(at Output Equinox)
*     SCALE  - Scale of Plot in arc.secs./mm.
*     GRID   - says what type of GRID
*     DIRECT - says whether the plot is direct or reversed
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to DECMK call
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'PLOTDAT'

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*(*) GRID
      LOGICAL DIRECT

      DOUBLE PRECISION NERA,NEDEC,NWRA,NWDEC,SERA,SEDEC,SWRA,SWDEC
     : ,CENRA,CENDEC,RA,DECMAX,DECMIN,DEC,AO,DO,MMRD
      REAL NSCALE,XLEAST,XMOST,YLEAST,YMOST,XRANGE,YRANGE
      LOGICAL POLAR,NEARPOLE,FRST,ALTERNATE,NEXT
      DIMENSION DECGAP(5),RAGAP(8)
      DOUBLE PRECISION HALFPI,TWOPI,RDSA,RDST,RDDG

      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

      DATA DECGAP/360.0,720.0,1800.0,3600.0,7200.0/
      DATA RAGAP/30.0,60.0,120.0,300.0,600.0,1200.0,1800.0,3600.0/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      HW=SIZEMM/2.0
      MMRD = 1.0/(RDSA*SCALE)
*
*   WIDTH - Projected Distance (in Rads.) from Centre to edge
*
      WIDTH = REAL( SIZEMM/(2.0 * MMRD) )
      SIZE = SIZEMM/2.0
*
*   Decide if the Pole is on the Chart
*
      CALL PROJ(1,AO,DSIGN(HALFPI,DO),XPOLE,YPOLE, STATUS )
      POLAR =
     : ABS(XPOLE*MMRD).LE.SIZE.AND.ABS(YPOLE*MMRD).LE.SIZE
*
*   Find RA,Dec of each Corner of the Chart
*
      CALL PROJ(2,NWRA,NWDEC,-WIDTH,WIDTH, STATUS )
      CALL PROJ(2,NERA,NEDEC,WIDTH,WIDTH, STATUS )
      CALL PROJ(2,SWRA,SWDEC,-WIDTH,-WIDTH, STATUS )
      CALL PROJ(2,SERA,SEDEC,WIDTH,-WIDTH, STATUS )
*
*   and Whether it is 'Nearly' Polar Case (>= 86 Deg. on Chart)
*
      NEARPOLE =
     : DMAX1(ABS(SEDEC),ABS(NEDEC)).GE.8.6D1 * RDDG.OR.POLAR
*
*   Adjust for the Case where Chart Straddles 0H. RA
*
      IF (NWRA.GT.NERA) NWRA = NWRA - TWOPI
      IF (SWRA.GT.SERA) SWRA = SWRA - TWOPI
*
*   Polar Case. Extremes are Simply Set
*
      IF (POLAR) THEN
         RAMAX = REAL( TWOPI )
         RAMIN = 0.0
         RARAN = REAL( TWOPI )
         IF (DO.LT.0.0) THEN
            DECMAX = DMAX1(NWDEC,SWDEC)
            DECMIN = -HALFPI - (-HALFPI - DECMAX) * 0.1
         ELSE
            DECMIN = DMIN1(NWDEC,SWDEC)
            DECMAX = HALFPI - (HALFPI - DECMIN) * 0.1
         ENDIF
         DECRAN = REAL( DECMAX - DECMIN )
         GOTO 150
      ENDIF
*
*   Find Extreme Dec. on Chart
*   Centre Top (Northern Chart) or Centre Bottom (South)
*
      IF (DO.LT.0.0) THEN
         POS = -WIDTH
      ELSE
         POS = WIDTH
      ENDIF
      CALL PROJ(2,CENRA,CENDEC,0.0,POS, STATUS )
*
*   Work out Range and Extremes of RA ( all Radians )
*   bearing in mind that the plot may be reversed
*
      IF (DO.LT.0.0) THEN
C        IF (DIRECT) THEN
            RAMAX = REAL( SERA )
            RAMIN = REAL( SWRA )
C        ELSE
C           RAMAX = SWRA
C           RAMIN = SERA
C        END IF
      ELSE
C         IF (DIRECT) THEN
            RAMAX = REAL( NERA )
            RAMIN = REAL( NWRA )
C        ELSE
C           RAMAX = NWRA
C           RAMIN = NERA
C        END IF
      ENDIF
      RARAN = RAMAX - RAMIN
*
*   Dec. Range and Extremes
*
      IF (DO.LT.0.0) THEN
         DECMIN = CENDEC
         DECMAX = NWDEC
      ELSE
         DECMIN = SWDEC
         DECMAX = CENDEC
      ENDIF
      DECRAN = REAL( DECMAX - DECMIN )
*
*   Choose Grid-Line Interval in RA
*   ( all Calculations in Seconds of Time)
*
150   CONTINUE
      RASR = REAL( RARAN/RDST )
*
*   Gap is about 1/5th. of the Full Width
*
      REXGAP = RASR/5.0
*
*   Find nearest 'Rounded' Figure to the Exact Value
*
      CALL NEAR(REXGAP,RAGAP,8,J, STATUS )
      RAINT = RAGAP(J)
*
*   Choose Grid-Line Gap for Dec.
*   (Calculations in secs. of Arc.)
*
      DECSR = REAL( DECRAN/RDSA )
      DEXGAP = DECSR/5.0
*
*   Find 'Rounded' Figure for Dec Gap
*
      CALL NEAR(DEXGAP,DECGAP,5,J, STATUS )
      DECINT = DECGAP(J)
*
*   One option allows only a mini-grid to be plotted,
*   by omitting the actaul grid lines.
*
      IF (GRID(1:1).EQ.'Y') THEN
*
*   Plot the Meridians of Equal RA
*
*
         RASEC = AMOD(ROUNUP(RAMIN/RDST,RAINT),86400.0)
300      CONTINUE
         RA = RASEC * RDST
         CALL PROJ(1,RA,DECMAX,X1,Y1, STATUS )
         X1 = REAL( X1 * MMRD )
         IF (.NOT.DIRECT) X1=-X1
         Y1 = REAL( Y1 * MMRD )
         CALL SGS_BPOLY (-X1,Y1)
         CALL PROJ(1,RA,DECMIN,X2,Y2, STATUS )
         X2 = REAL( X2 * MMRD )
         IF (.NOT.DIRECT) X2 = -X2
         Y2 = REAL( Y2 * MMRD )
         CALL SGS_APOLY (-X2,Y2)
         CALL SGS_OPOLY
         RASEC = RASEC + RAINT
         IF (RASEC*RDST.LE.RAMAX) GOTO 300
*
*   Draw Parallels of Equal DEC
*
         DECSEC = ROUNUP(DECMIN/RDSA,DECINT)
         FRST=.TRUE.
370      CONTINUE
         DEC = DECSEC * RDSA
         RA = RAMIN
         CALL PROJ(1,RA,DEC,X,Y, STATUS )
         X = REAL( X * MMRD   )
         IF (.NOT.DIRECT) X=-X
         Y = REAL( Y * MMRD )
         CALL SGS_BPOLY (-X,Y)
*
*   Plot each line Across ,Increments
*   1/50th. The Width of the Side with Least RA Range
*
         IF (DO.LT.0.0) THEN
            RAINC = REAL( (NERA - NWRA)/50.0 )
         ELSE
            RAINC = REAL( (SERA - SWRA)/50.0 )
         ENDIF
         IF (FRST) THEN
            RAMAX = RAMAX + 0.5 * RAINC
            FRST=.FALSE.
         ENDIF
400      CONTINUE
         RA = RA + RAINC
         IF (RA.GT.RAMAX) GOTO 450
         CALL PROJ(1,RA,DEC,X,Y, STATUS )
         X = REAL( X * MMRD )
         IF (.NOT.DIRECT) X=-X
         Y = REAL( Y * MMRD )
         CALL SGS_APOLY (-X,Y)
         GOTO 400
450      CONTINUE
         CALL SGS_OPOLY
         DECSEC = DECSEC + DECINT
         IF (DECSEC*RDSA.LE.DECMAX) GOTO 370
         RAMAX = RAMAX - 0.5 * RAINC

      END IF

      CALL SGS_SELZ (IZAREA,ISTAT)
*
*   If ARGS then Reset Pen Colour
*
      IF (COLOUR) CALL SGS_SPEN (1)

*
*   Get Scale of E,W Sides (ESCALE)
*   and N,S,Sides (NSCALE,SSCALE)
*   in radians/mm.
*
      ESCALE = REAL( (NEDEC - SEDEC)/SIZEMM )
      NSCALE = REAL( (NERA - NWRA)/SIZEMM )
      SSCALE = REAL( (SERA - SWRA)/SIZEMM )
      IF (RAINT.LT.60.0) RAINT = 60.0
*
*   1.  Mark Dec. Scales up Sides
*       If near the Pole,due to Non-linear Side Dec Scale,
*       Marks are not down both edges,but on Centre Meridian
*       Only.
*
      LINDEC=0
      YLEAST=SIZE
      YMOST=-SIZE
      IF (NEARPOLE) THEN
         DECSEC = ROUNUP(DECMIN/RDSA,DECINT)
      ELSE
         DECSEC = ROUNUP(SEDEC/RDSA,DECINT)
      ENDIF
500   CONTINUE
      DEC = DECSEC * RDSA
      IF (.NOT.NEARPOLE) THEN
         Y = REAL( ((DEC - SEDEC)/ESCALE) - SIZE )
      ELSE
         CALL PROJ(1,AO,DEC,X,Y, STATUS )
         Y = REAL( Y * MMRD )
      ENDIF
      DECSEC = DECSEC + DECINT
      LINDEC=LINDEC+1
      IF (Y.LT.YLEAST) YLEAST=Y
      IF (Y.GT.YMOST) YMOST=Y
      IF (.NOT.NEARPOLE) THEN
         IF (DECSEC*RDSA.LE.NEDEC) GOTO 500
      ELSE
         IF (DECSEC*RDSA.LE.DECMAX) GOTO 500
      ENDIF
      YRANGE=YMOST-YLEAST
      IF ((LINDEC.GE.5).AND.(SIZEMM.LT.100.0)) THEN
         ALTERNATE=.TRUE.
         NEXT=.TRUE.
         IF ((LINDEC/2*2).NE.LINDEC) LINDEC=LINDEC+1
         LINDEC=LINDEC/2
      ELSE
         ALTERNATE=.FALSE.
         NEXT=.TRUE.
      ENDIF
      CH=20.0*6.0/(5.0*6.5)
      IF ((FLOAT(LINDEC)-1.0)*1.5*CH.GT.YRANGE) THEN
         CH=YRANGE/((FLOAT(LINDEC)-1.0)*1.5)
      ENDIF
      CW=5.0/6.0*CH
      CALL SGS_SHTX (CH)
      IF (NEARPOLE) THEN
         DECSEC = ROUNUP(DECMIN/RDSA,DECINT)
      ELSE
         DECSEC = ROUNUP(SEDEC/RDSA,DECINT)
      ENDIF
550   CONTINUE
      DEC = DECSEC * RDSA
      IF (.NOT.ALTERNATE) NEXT=.TRUE.
      IF (.NOT.NEARPOLE) THEN
         Y = REAL( ((DEC - SEDEC)/ESCALE) - SIZE )
         IF (NEXT) THEN
            CALL DECMK (-(SIZE+0.5*CW),Y,DEC, STATUS )
            NEXT=.FALSE.
         ELSE
            NEXT=.TRUE.
         ENDIF
      ELSE
         CALL PROJ(1,AO,DEC,X,Y, STATUS )
         Y = REAL( Y * MMRD )
         IF (ABS(Y).LE.SIZE) THEN
            IF (NEXT) THEN
               CALL DECMK(-0.5*CW,Y+SIZEMM/100.0,DEC, STATUS )
               NEXT=.FALSE.
            ELSE
               NEXT=.TRUE.
            ENDIF
         ENDIF
      ENDIF
      DECSEC = DECSEC + DECINT
      IF (.NOT.NEARPOLE) THEN
         IF (DECSEC*RDSA.LE.NEDEC) GOTO 550
      ELSE
         IF (DECSEC*RDSA.LE.DECMAX) GOTO 550
      ENDIF
*
*   2.   Mark RA Points on the N Side
*        Only if the Range in RA is Reasonable (<7H. R.A.)
*
      IF (NERA - NWRA.GT.(7.0/6.0)*HALFPI) GOTO 700
      LINRA=0
      XLEAST=SIZE
      XMOST=-SIZE
      RASEC  = AMOD(ROUNUP(NWRA/RDST,RAINT),86400.0)
600   CONTINUE
      RA = RASEC * RDST
      X = REAL( SIZE - ((RA - NWRA)/NSCALE) )
      IF (.NOT.DIRECT) X=-X
      IF (X.LT.XLEAST) XLEAST=X
      IF (X.GT.XMOST)  XMOST=X
      LINRA=LINRA+1
      RASEC = RASEC + RAINT
      IF (RASEC*RDST.LE.NERA) GOTO 600
      XRANGE=XMOST-XLEAST
      IF ((LINRA.GE.5).AND.(SIZEMM.LT.100.0)) THEN
         ALTERNATE=.TRUE.
         NEXT=.TRUE.
         IF ((LINRA/2*2).NE.LINRA) THEN
            LINRA=LINRA+1
         ELSE
            XRANGE=XRANGE*(FLOAT(LINRA)-2.0)/(FLOAT(LINRA)-1.0)
         ENDIF
         LINRA=LINRA/2
      ELSE
         ALTERNATE=.FALSE.
         NEXT=.TRUE.
      ENDIF
      CW=20.0/5.0
      IF ((FLOAT(LINRA)-1.0)*6.0*CW.GT.XRANGE) THEN
         CW=XRANGE/((FLOAT(LINRA)-1.0)*6.0)
      ENDIF
      CH=6.0/5.0*CW
      CALL SGS_SHTX (CH)
      RASEC  = AMOD(ROUNUP(NWRA/RDST,RAINT),86400.0)
650   CONTINUE
      RA = RASEC * RDST
      X = REAL( SIZE - ((RA - NWRA)/NSCALE) )
      IF (.NOT.DIRECT) X=-X
      IF (.NOT.ALTERNATE) NEXT=.TRUE.
      IF (NEXT) THEN
         CALL RAMK(X,SIZE+SIZEMM/100.0,RA, STATUS )
         NEXT=.FALSE.
      ELSE
         NEXT=.TRUE.
      ENDIF
      RASEC = RASEC + RAINT
      IF (RASEC*RDST.LE.NERA) GOTO 650
*
*   3.  Mark RA on Southern Side
*       only if the RA Range is Reasonable (< 7H RA )
*
700   CONTINUE
      IF (SERA - SWRA.GT.(7.0/6.0)*HALFPI) GOTO 870
      LINRA=0
      XLEAST=SIZE
      XMOST=-SIZE
      RASEC = AMOD(ROUNUP (SWRA/RDST,RAINT),86400.0)
750   CONTINUE
      RA = RASEC * RDST
      X = REAL( SIZE - ((RA - SWRA)/SSCALE) )
      IF (.NOT.DIRECT) X=-X
      IF (X.LT.XLEAST) XLEAST=X
      IF (X.GT.XMOST)  XMOST=X
      LINRA=LINRA+1
      RASEC = RASEC + RAINT
      IF (RASEC*RDST.LT.SERA) GOTO 750
      XRANGE=XMOST-XLEAST
      IF ((LINRA.GE.5).AND.(SIZEMM.LT.100.0)) THEN
         ALTERNATE=.TRUE.
         NEXT=.TRUE.
         IF ((LINRA/2*2).NE.LINRA) THEN
            LINRA=LINRA+1
         ELSE
            XRANGE=XRANGE*(FLOAT(LINRA)-2.0)/(FLOAT(LINRA)-1.0)
         ENDIF
         LINRA=LINRA/2
      ELSE
         ALTERNATE=.FALSE.
         NEXT=.TRUE.
      ENDIF
      CW=20.0/5.0
      IF ((FLOAT(LINRA)-1.0)*6.0*CW.GT.XRANGE) THEN
         CW=XRANGE/((FLOAT(LINRA)-1.0)*6.0)
      ENDIF
      CH=6.0/5.0*CW
      CALL SGS_SHTX (CH)
      RASEC = AMOD(ROUNUP (SWRA/RDST,RAINT),86400.0)
800   CONTINUE
      RA = RASEC * RDST
      X = REAL( SIZE - ((RA - SWRA)/SSCALE) )
      IF (.NOT.DIRECT) X=-X
      IF (.NOT.ALTERNATE) NEXT=.TRUE.
      IF (NEXT) THEN
         CALL RAMK(X,-(SIZE+SIZEMM/100.0),RA, STATUS )
         NEXT=.FALSE.
      ELSE
         NEXT=.TRUE.
      ENDIF
      RASEC = RASEC + RAINT
      IF (RASEC*RDST.LT.SERA) GOTO 800
870   CONTINUE
      CALL SGS_SELZ (IZCHART,ISTAT)

      END
