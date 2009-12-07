*SPLFIT
*
* SPLFIT  -- fits splines to 1D data. Bad pixels are rejected.
*
* The function is fitted only to good pixels but it is evaluated over
* the whole spectrum. Bad values at the ends are replaced by linear
* extrapolation from the last good values.
*
* Parameters:
*
*  INPUT        -- The input spectrum file
* 
*  OUTPUT       -- Output spectrum file.
*
*  NSPLINE      -- Number of splines
*
*  THRLO,THRHI  -- The reject thresholds for the fit.
*
*  NCYCLE       -- Maximum number of reject cycles
*  
*  PLOT         -- .TRUE. for option of plotting
*
*  DEVICE       -- Plot device
*
*SPLFIT
      SUBROUTINE SPLNFIT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, INPUT, OUTPUT, NXS, XLO, X
     :HI, NDIM
      INTEGER NSPLINE, NCYCLE, IDPTR, EL, IEPTR, ODPTR, NSET
      REAL THRLO, THRHI, LIMITS(2)
      LOGICAL ANYERR, PLOT, AUTO
      CHARACTER*64 DEVICE
C
C Start
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file 
C
      CALL NDF_ASSOC('INPUT', 'READ',INPUT, STATUS)
      CALL NDF_BOUND(INPUT, 1, XLO, XHI, NDIM, STATUS)
      NXS = XHI - XLO + 1
      CALL NDF_STATE(INPUT,'Variance',ANYERR,STATUS)
C
C     Get spectrum output file
C
      CALL NDF_PROP(INPUT,' ','OUTPUT',OUTPUT,STATUS)
      CALL NDF_RESET(OUTPUT,'Data,Variance',STATUS)
      CALL NDF_STYPE('_REAL',OUTPUT,'Data',STATUS)
C
C     Parameters
C
      CALL PAR_GDR0I('NSPLINE',1,1,100,.FALSE.,NSPLINE,STATUS)
      CALL PAR_GDR0R('THRLO',-3.,-1.E20,1.E20,.FALSE.,THRLO,STATUS)
      CALL PAR_GDR0R('THRHI',3.,THRLO,1.E20,.FALSE.,THRHI,STATUS)
      CALL PAR_GDR0I('NCYCLE',10,1,100000,.FALSE.,NCYCLE,STATUS)
      CALL PAR_GET0L('PLOT',PLOT,STATUS)
      IF(PLOT) THEN
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
         CALL PAR_GET0L('AUTO', AUTO, STATUS)
         IF(.NOT.AUTO) CALL PAR_GET1R('LIMITS',2,LIMITS,NSET,STATUS)
      END IF
C
C     Map
C
      CALL NDF_MAP(INPUT,'Data','_REAL','READ',IDPTR,EL,STATUS)
      IF(ANYERR) 
     &     CALL NDF_MAP(INPUT,'Error','_REAL','READ',IEPTR,EL,STATUS)
      CALL NDF_MAP(OUTPUT,'Data','_REAL','WRITE',ODPTR,EL,STATUS)
C
      CALL SPL_FIT(%VAL(CNF_PVAL(ODPTR)), %VAL(CNF_PVAL(IDPTR)), 
     :             %VAL(CNF_PVAL(IEPTR)), NXS,
     &     XLO, ANYERR, PLOT, NSPLINE, THRLO, THRHI, NCYCLE, DEVICE, 
     &     LIMITS(1), LIMITS(2), AUTO, STATUS)
C     
C     Store limits for next time
C     
      IF(AUTO) THEN
         CALL PAR_DEF1R('LIMITS',2,LIMITS,STATUS)
         CALL PAR_GET1R('LIMITS',2,LIMITS,NSET,STATUS)
      END IF
      CALL NDF_END(STATUS)
      RETURN
      END
        
      SUBROUTINE SPL_FIT(OUTPUT,SPECTRUM,ERRORS,NXS,XLO,ANYERR,
     &     PLOT,NSPLINE,THRLO,THRHI,NCYCLE,DEVICE,Y1,Y2,AUTO,STATUS)
C
C     Fits splines to a spectrum
C     
C     R*4 OUTPUT(NXS)       --- Output fit
C     R*4 SPECTRUM(NXS)     --- spectrum to be fitted
C     R*4 ERRORS(NXS)       --- 1-sigma uncertainties. Only used
C     if ANYERR. Set negative to ignore.
C     I*4 NXS               --- Number of points
C     I*4 XLO               --- First point
C     L ANYERR              --- Shows whether uncertainties are available
C     L PLOT                --- TRUE allows plot option 
C     I*4 NSPLINE           --- Number of polynomial coefficients
C     R*4 THRLO, THRHI      --- Upper and lower sigma reject thresholds
C     I*4 NCYCLE            --- Maximum number of cycles
C     C DEVICE              --- Plot device
C     R*4 Y1, Y2            --- Plot range
C     L AUTO                --- True for auto determination of range
C     I*4 STATUS            --- Error return. 0 is ok.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER STATUS, NXS, NSPLINE, NCYCLE, XLO, NOK
      INTEGER N1, N2, MAXFIT, IX, IFAIL, ID, I1, I2, I
      REAL OUTPUT(NXS), SPECTRUM(NXS), ERRORS(NXS)
      REAL THRLO, THRHI, Y1, Y2, X1, X2
      LOGICAL ANYERR, PLOT, AUTO
      PARAMETER (MAXFIT=16000)
      REAL PLOTX(MAXFIT), PLOTY(MAXFIT), PLOTZ(MAXFIT)
      INTEGER PGOPEN
      CHARACTER*(*) DEVICE

      INTEGER NCAP7
      DOUBLE PRECISION XKNOT(1)
      DOUBLE PRECISION CSPLINE(1)
      COMMON/SPLFIT0/NCAP7
      COMMON/SPLFIT1/XKNOT
      COMMON/SPLFIT2/CSPLINE

C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(NXS.GT.MAXFIT) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many points for internal buffers',
     &        STATUS)
         RETURN
      END IF
C
C     Load X and Z arrays
C
      NOK = 0
      DO IX = 1, NXS
         PLOTX(IX) = REAL(XLO+IX-1)
         IF(ANYERR) THEN
            IF(SPECTRUM(IX).EQ.VAL__BADR .OR. 
     &           ERRORS(IX).EQ.VAL__BADR) THEN
               PLOTZ(IX) = -1.
            ELSE
               PLOTZ(IX) = ERRORS(IX)
               NOK = NOK + 1
            END IF
         ELSE
            IF(SPECTRUM(IX).EQ.VAL__BADR) THEN
               PLOTZ(IX) = -1.
            ELSE
               PLOTZ(IX) = 1.
               NOK = NOK + 1
            END IF
         END IF
      END DO
      IF(NOK.LT.NSPLINE) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too few good pixels to fit.',
     &        STATUS)
         RETURN
      END IF
C
C     Evaluate start and end points of good data
C
      N1 = 1
      DO WHILE(PLOTZ(N1).LT.0.)
         N1 = N1 + 1
      END DO
      N2 = NXS
      DO WHILE(PLOTZ(N2).LT.0.)
         N2 = N2 - 1
      END DO
C
C     Fit
C
      CALL SPLFITA( N2-N1+1, PLOTX(N1), SPECTRUM(N1), PLOTZ(N1),
     &     PLOTY(N1), NSPLINE, NCYCLE, THRHI, THRLO, IFAIL )
C
C     Evaluate fit beyond good data regions
C
      IF(N1.GT.1)   CALL SPLCALC(N1-1, PLOTX, XKNOT, CSPLINE, NCAP7,
     &     PLOTY, IFAIL)
      IF(N2.LT.NXS) CALL SPLCALC(NXS-N2, PLOTX(N2+1), XKNOT, CSPLINE,
     &     NCAP7, PLOTY(N2+1), IFAIL)
C
C     Plot fit
C
      IF(PLOT) THEN
         IF(AUTO) THEN
            Y1 =  1.E30
            Y2 = -1.E30
            DO I = 1, NXS
               IF(SPECTRUM(I).NE.VAL__BADR) THEN
                  Y1 = MIN(SPECTRUM(I),Y1)
                  Y2 = MAX(SPECTRUM(I),Y2)
               END IF
            END DO
         END IF
         X1 = REAL(XLO-1)
         X2 = REAL(XLO+NXS)
         ID = PGOPEN(DEVICE)
         IF(ID.GT.0) THEN
            CALL PGSCI(5)
            CALL PGENV(X1, X2, Y1, Y2, 0, 1)
            CALL PGSCI(7)
            CALL PGLAB('Pixel number', 'Spectrum + fit', ' ')
C
C     Plot good parts of spectrum
C
            CALL PGSCI(1)
            I2 = 0
            DO WHILE(I2.LT.NXS)
               I1 = I2 + 1
               DO WHILE(I1.LE.NXS .AND. SPECTRUM(I1).EQ.VAL__BADR)
                  I1 = I1 + 1
               END DO
               IF(I1.LT.NXS) THEN
                  I2 = I1 + 1
                  DO WHILE(I2.LE.NXS .AND. SPECTRUM(I2).NE.VAL__BADR)
                     I2 = I2 + 1
                  END DO
                  I2 = I2 - 1
               ELSE IF(I1.GE.NXS) THEN
                  I2 = I1
               END IF
               IF(I1.LE.NXS) 
     &              CALL PGBIN(I2-I1+1, PLOTX(I1), SPECTRUM(I1),.TRUE.)
            END DO
C     
C     Plot points rejected during fit Xs
C     
            CALL PGSCI(2)
            DO I = 1, NXS
               IF(PLOTZ(I).LE.0. .AND. SPECTRUM(I).NE.VAL__BADR) 
     &              CALL PGPOINT(1,PLOTX(I),SPECTRUM(I),5)
            END DO
C
            CALL PGSCI(3)
            CALL PGLINE(NXS, PLOTX, PLOTY)
            CALL PGCLOS
         END IF
      END IF
C
C     Load fit into output array
C
      DO I = 1, NXS
         OUTPUT(I) = PLOTY(I)
      END DO
      RETURN
      END
