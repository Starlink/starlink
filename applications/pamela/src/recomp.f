*RECOMP
*
* RECOMP  -- recomputes an output file from PROFIT for a new spectrum
*            position and object region.
*
* recomp allows the fit from one star to be moved onto another. The
* fractions are normalised over the object region. I haven't used it a
* great deal and if anyone does, I would be interested in how well it
* worked.
*
* Parameters:
*
*  FRACT   -- The fraction file to be altered.
*
*  TRACK   -- Contains fit to new position of spectrum
*
*  YSTART, YEND -- The Y pixel limit of the alteration.
*
*RECOMP
      SUBROUTINE RECOMP(STATUS)
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, DIM(2), NDIM, NPOLY, NPOLS
      INTEGER YLO, YHI, FRACT, PPOLY, TRACK, NDPOLY
      INTEGER LBND(2), UBND(2), TPTR, XLO, XHI
      INTEGER NXS, NYS, FRPTR, EL, PPTR, OYLO, OYHI
      LOGICAL BASE
      REAL LEFT, RIGHT, XSHIFT
      DOUBLE PRECISION XREF, YREF, XD, YD, POLY, YPOS, TOFF
      CHARACTER*(DAT__SZLOC) LOC
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open fraction file
C
      CALL NDF_ASSOC('FRACT', 'UPDATE', FRACT, STATUS)
C
C     Read old left and right limits and X reference position
C
      CALL NDF_XGT0R(FRACT,'PAMELA','PROFIT.LEFT',LEFT,STATUS)
      CALL NDF_XGT0R(FRACT,'PAMELA','PROFIT.RIGHT',RIGHT,STATUS)
      CALL NDF_XGT0D(FRACT,'PAMELA','PROFIT.XREF',XREF,STATUS)
      CALL NDF_XGT0I(FRACT,'PAMELA','PROFIT.YLO',OYLO,STATUS)
      CALL NDF_XGT0I(FRACT,'PAMELA','PROFIT.YHI',OYHI,STATUS)
C
C     Get size of polynomial array
C
      CALL NDF_XLOC(FRACT,'PAMELA', 'READ',LOC,STATUS)
      CALL NDF_FIND(LOC, 'PROFIT.PPOLY', PPOLY, STATUS)
      CALL NDF_DIM(PPOLY, 2, DIM, NDIM, STATUS)
      NPOLY = DIM(1)
      NPOLS = DIM(2)
C
C     Get track file
C
      CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     What region of frame
C
      CALL NDF_ISBAS(FRACT, BASE, STATUS)
      IF(.NOT.BASE) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','The fraction file must be a base NDF',
     &        STATUS)
      END IF
      CALL NDF_BOUND(FRACT, 2, LBND, UBND, NDIM, STATUS)
      IF(NDIM.EQ.2) THEN
         CALL PAR_GDR0I('YSTART',LBND(2),1,10000,
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,10000,
     &        .FALSE.,YHI,STATUS)
      ELSE
         YLO = LBND(2)
         YHI = UBND(2)
      END IF

      IF(YLO.NE.OYLO .OR. YHI.NE.OYHI) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','You cannot alter the Y range of' //
     &        ' the original fraction file in recomp', STATUS)
      END IF
C
C     Compute new XREF and update LEFT and RIGHT values
C
      IF(STATUS.EQ.SAI__OK) THEN
         CALL GET_TRACK(DBLE(YLO+YHI)/2.D0,XD,STATUS)
         XSHIFT = REAL(XD-XREF)
         XREF   = XD
         LEFT   = LEFT  + XSHIFT
         RIGHT  = RIGHT + XSHIFT
      END IF
C
C     compute minimal range in X to use
C
      CALL XLIMS(.TRUE.,YREF,LEFT,RIGHT,YLO,YHI,XLO,XHI,STATUS)
C
C     Change bounds of fraction NDF
C
      IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &     YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
         LBND(1) = XLO
         UBND(1) = XHI
         LBND(2) = YLO
         UBND(2) = YHI
         CALL NDF_RESET(FRACT,'Data',STATUS)
         CALL NDF_SBND(2,LBND,UBND,FRACT,STATUS)
      END IF
C
C     Section dimensions
C
      NXS = UBND(1)-LBND(1)+1
      NYS = UBND(2)-LBND(2)+1
C
C     Map ndfs
C
      CALL NDF_MAP(FRACT,'Data','_REAL','WRITE',FRPTR,EL,STATUS)
      CALL NDF_MAP(PPOLY,'Data','_DOUBLE','READ',PPTR,EL,STATUS)
C
C     Re-compute fractions.
C
      CALL RE_COMP(%VAL(CNF_PVAL(FRPTR)), NXS, NYS, XLO, YLO,
     &     %VAL(CNF_PVAL(PPTR)), NPOLS, NPOLY, LEFT, RIGHT,
     &     XREF, STATUS)
C
C     Update left and right limits and reference positions
C
      CALL NDF_XPT0R(LEFT,FRACT,'PAMELA','PROFIT.LEFT',STATUS)
      CALL NDF_XPT0R(RIGHT,FRACT,'PAMELA','PROFIT.RIGHT',STATUS)
      CALL NDF_XPT0D(XREF,FRACT,'PAMELA','PROFIT.XREF',STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END
C
      SUBROUTINE RE_COMP(DATA, NXS, NYS, XLO, YLO, PROFILE,
     &     NPOLS, NPOLY, LEFT, RIGHT, XREF, STATUS)
C
C     Recomputes fractions from the polynomial coefficients stored in
C     PROFILE according to the new spectrum position defined by DISTORT
C
C     R*4 DATA(NXS,NYS)        -- Fractions are stored in this file
C     I*4 NXS, NYS             -- Frame size
C     I*4 XLO, YLO             -- Lower left corner of section
C     R*8 PROFILE(NPOLY,NPOLS) -- Polynomial coefficients.
C     I*4 NPOLS, NPOLY         -- Number of polynomials and number of terms
C                                 of each.
C     R*4 LEFT, RIGHT          -- New object limits
C
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS, I, J, IND, IX, IY, IXL, IXR, IX1, IX2
      INTEGER IP1, IP2, NXS, NYS, NDPOLY, NPOLS, NPOLY, XLO
      INTEGER YLO
      REAL DATA(NXS,NYS), LEFT, RIGHT, XADD
      REAL RLEFT, RRIGHT, SPIX, SAVE, SAVE2, X1, X2, XZ, XT
      REAL QFAC, Y, WEIGHT, XS
      DOUBLE PRECISION PROFILE(NPOLY,NPOLS)
      INTEGER MAXWORK
      PARAMETER (MAXWORK=1000)
      DOUBLE PRECISION WORK(MAXWORK), YD, POLY, XD
      DOUBLE PRECISION SUM1, SUM2, SUM3, XREF
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(NPOLS*NPOLY.GT.MAXWORK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Work array in RECOMP too small',
     &        STATUS)
        RETURN
      END IF
C
C     Compute value of SPIX = space per polynomial
C
      RLEFT  = LEFT  - 1.
      RRIGHT = RIGHT + 1.
      SPIX   = (RRIGHT-RLEFT)/REAL(NPOLS)
      SAVE   = 0.5-SPIX
      SAVE2  = 1.-(SAVE/SPIX)**2
C
C     Rearrange polynomial coefficients for computational
C     speed and convenience
C
      DO I = 1, NPOLY
         DO J = 1, NPOLS
            IND = NPOLS*(I-1)+J
            WORK(IND) = PROFILE(I,J)
         END DO
      END DO
C
C     Compute fractions
C
      DO IY = 1, NYS
C
C     Initialise fraction data
C
         DO IX = 1, NXS
            DATA(IX,IY) = 0.
         END DO
C
C     Compute X distortion of spectrum and X limits
C     of computation region.
C
         YD = DBLE(IY+YLO-1)
         CALL GET_TRACK(YD,XD,STATUS)
         XS    = REAL(XD - XREF)
         IXL   = NINT(LEFT+XS)-(XLO-1)
         IXR   = NINT(RIGHT+XS)-(XLO-1)
         IX1   = IXL - 2
         IX1   = MAX(1, IX1)
         IX2   = IXR + 2
         IX2   = MIN(NXS, IX2)
C
         XADD  = RLEFT + XS
         SUM3  = 0.
         DO IX = IX1, IX2
            X1  = REAL(IX+XLO-1)-XADD-0.5
            X2  = REAL(IX+XLO-1)-XADD+0.5
            IP1 = INT(X1/SPIX+0.5)
            IP2 = INT(X2/SPIX+1.5)
            IP1 = MAX(1, MIN(IP1, NPOLS))
            IP2 = MAX(1, MIN(IP2, NPOLS))
            SUM1 = 0.D0
            DO J = 0, NPOLY-1
               IND = NPOLS*J
               SUM2 = 0.D0
               DO I = IP1, IP2
                  XZ = XADD+SPIX*(REAL(I-1)+0.5)-REAL(IX+XLO-1)
                  XT = ABS(XZ)
                  IF(XT.LE.SAVE) THEN
                     QFAC = 1.
                  ELSE IF(XT.GT.SAVE .AND. XT.GE.-SAVE) THEN
                     Y = (XT-SAVE)/SPIX
                     IF(Y.LE.1) THEN
                        QFAC = 1. - Y*Y/2.
                     ELSE
                        QFAC = (2.-Y)**2./2.
                     END IF
                  ELSE
                     QFAC = SAVE2-(XZ/SPIX)**2
                  END IF
                  SUM2 = SUM2 + QFAC*WORK(IND+I)
               END DO
               SUM1 = SUM1 + SUM2*(YD**J)
            END DO
            DATA(IX,IY) = REAL(MAX(SUM1, 0.D0))
            IF(IX.GT.IXL .AND. IX.LT.IXR) THEN
               WEIGHT = 1.
            ELSE IF(IX.EQ.IXL) THEN
               WEIGHT = REAL(IXL+XLO-1)+0.5-LEFT-XS
            ELSE IF(IX.EQ.IXR) THEN
               WEIGHT = RIGHT+XS-REAL(IXR+XLO-1)+0.5
            ELSE
               WEIGHT = 0.
            END IF
            SUM3 = SUM3 + WEIGHT*DATA(IX,IY)
         END DO
         DO IX = IX1, IX2
            DATA(IX,IY) = REAL(DATA(IX,IY)/SUM3)
         END DO
      END DO
      RETURN
      END



