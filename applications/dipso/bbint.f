       REAL FUNCTION BBINT(WINT,TEMP,OK)

       IMPLICIT NONE

       REAL WINT, TEMP
       REAL RN, WN, FAC1, FAC2, TEST
       REAL HKT, U0, SUM1, SUM2

       LOGICAL OK

       U0 = 1.43883E+08/WINT/TEMP
       SUM1 = 0.0
       SUM2 = 0.0

       RN = 0.0
  100  CONTINUE
       RN = RN + 1.0
       IF (RN.GT.50.0) THEN
          OK = .FALSE.
          BBINT = SUM2
          WRITE (*,'(''   ZANSTRA:  failure to converge'')')
          RETURN
       ENDIF
       WN = RN*U0
       FAC1 = (WN+2.0)*WN + 2.0
       FAC1 = LOG(FAC1)
       FAC2 = -WN - 3.0*LOG(RN)
       FAC1 = FAC1 + FAC2
       SUM1 = SUM2
       SUM2 = SUM2 + EXP(FAC1)
       TEST = SUM1/SUM2
       IF (ABS(TEST-1.0).GT.0.001) GOTO 100
       OK = .TRUE.
       BBINT = SUM2
       RETURN
       END
