*-----------------------------------------------------------------------

      SUBROUTINE MOMENT (NQ, XSCALE, IFAIL)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

      CHARACTER STRING*80
      COMMON /STRING/ STRING

*     Local variables

      INTEGER   I
      INTEGER   J
      INTEGER   JDEF
      INTEGER   M
      INTEGER   MOM
      INTEGER   NLOW
      INTEGER   NHIGH
      INTEGER   NST
      REAL      AINTEG
      REAL      BINTEG
      REAL      X1, X2

      REAL      X(3)
      EQUIVALENCE  (X(1),XREF)

*     Functions

      INTEGER   NTOT

*  Ok, go...

      CALL SETXNEW (XSCALE, IFAIL)
      STRING = ' '
      IF (NXSFM.EQ.NXS)   STRING = '3(F6.1,1X)'
      CALL GEN_GETR4A ('Ref pt., gap, int.width?',
     &                  X, 3, STRING, X, JDEF)

C     Translate into channel numbers

      NST = NTOT (NQ-1)

      X2  = XREF - XGAP
      X1  = X2   - XINT
      DO I = 1, 2
        CALL NTRANS (XSCALE(NST+1), NPTS(NQ), X1, X2, XFAC(NQ), NLOW,
     &               NHIGH, IFAIL)
        IF (IFAIL.NE.0)   RETURN
        WRITE (ILOUT,4143) X1, X2

C       Do integration

        DO M = 1, 3
          MOM = M - 1
          AINTEG = 0.0
          DO J = NLOW, NHIGH
            IF (DATA(NST+J).NE.BADPIX_VAL) THEN
              AINTEG = AINTEG
     &                 + DATA(NST+J)*ABS(XREF-XSCALE(NST+J))**MOM
            END IF
          END DO

C         Give in terms of current X-units

          BINTEG = AINTEG * ABS(XFAC(NQ))
          WRITE (ILOUT,4144) BINTEG, XAXIS_UNITS, M
        END DO

C       Repeat for second interval

        X1 = XREF + XGAP
        X2 = X1   + XINT
      END DO

      NXSFM = NXS
      RETURN

 4143 FORMAT(/' For range ('F6.1','F6.1')')
 4144 FORMAT(1H ,T30,F8.2' KELVIN*('A6')**'I1)

      END

*-----------------------------------------------------------------------
