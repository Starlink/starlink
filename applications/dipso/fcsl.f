      SUBROUTINE FCSL
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      DIMENSION FUNS(20),PS(10,10),FLR(400),FMX(10),WSS(10)
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION FLXS(20,10),NPARS(10),IABST(20),KOP(2)
      DATA KOP/3H NO,3HYES/
      IF(NUMFIT.EQ.0) THEN
        WRITE(*, '(''   ELFCSL:  Stack is empty'')')
      ELSE
        WRITE(6,100)
  100   FORMAT(1X,'   Stack  Number of  Optimised      Wavelength'
     :        /1X,'   member   Lines                     limits')
        DO I=1,NUMFIT
          NDI4=4*NDIM
          IOP=0
          DO J=1,NDI4
            IF(AS(J,I,2).NE.0) IOP=IOP+1
          ENDDO
          IF(IOP.NE.0) IOP=1
          IOP=IOP+1
          AMAXX = -1.E+20
          AMINX = +1.E+20
          DO IAN = 1, NLS(I)
             TTEST = AS(IAN,I,2)
             IF (TTEST.GT.AMAXX) AMAXX=TTEST
             IF (TTEST.LT.AMINX) AMINX=TTEST
          ENDDO
         IF (ABS(AMAXX-AMINX).GT.(AMAXX*1.E-6)) THEN
           IF (ABS(AMINX).LT.1.0E5 .AND. ABS(AMAXX).LT.1.0E5) THEN
          WRITE(6,101) I,NLS(I),KOP(IOP),
C    :    FMX(I)
     :    AMINX, AMAXX
           ELSE
          WRITE (6,103) I,NLS(I),KOP(IOP),AMINX,AMAXX
           ENDIF
         ELSE
           IF (ABS(AMINX).LT.1.0E5 .AND. ABS(AMAXX).LT.1.0E5) THEN
          WRITE(6,102) I,NLS(I),KOP(IOP),AMINX
           ELSE
          WRITE(6,104) I,NLS(I),KOP(IOP),AMINX
           ENDIF
         ENDIF
  101     FORMAT(5X,I2,7X,I2,8X,A3,4X,4PE13.5,'  -',E13.5)
  102     FORMAT(5X,I2,7X,I2,8X,A3,6X,4PE13.5)
  103     FORMAT(5X,I2,7X,I2,8X,A3,4X,G13.5,'  -',G12.5)
  104     FORMAT(5X,I2,7X,I2,8X,A3,6X,G13.5)
        ENDDO
      ENDIF
      RETURN
      END
