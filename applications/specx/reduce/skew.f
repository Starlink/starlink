*  History:
*     22 Nov 1993 (hme):
*        Remove TABs. Remove redundant () in WRITE with implied DO. Add
*        commas after X format specifiers in statement 1000.
*-----------------------------------------------------------------------

      SUBROUTINE SKEW (NQ, XSCALE, BUF, IFAIL)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*     Common blocks:

      INCLUDE  'STACKCOMM'
      INCLUDE  'FLAGCOMM'

*     Local variables:

      INTEGER   I
      INTEGER   INS(2)
      INTEGER   JDEF
      INTEGER   MOM
      INTEGER   N
      INTEGER   NCEN
      INTEGER   NST
      REAL      AKURT
      REAL      AREA
      REAL      BETA1,  BETA2
      REAL      FRAC
      REAL      SKWNSS
      REAL      XMOM1
      REAL      XMOM(5)
      REAL      XOFF
      REAL      XX
      CHARACTER FDEF*4
      LOGICAL   MMEAN

      EQUIVALENCE      (INS(1),NS1)

*     Functions

      INTEGER   NTOT
      REAL      AMOM1
      REAL      XTRANS
      REAL      XSNART

* Ok, go...

*     Initialize all moments to zero

      DO MOM =1, 5
        XMOM(MOM) = 0.0
      END DO

      NST = NTOT (NQ-1) + 1

*     Get the region over which moments to be fitted

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      NPR = 1
      CALL GETPTS (INS, 1, 1, NPR, NPR, XSCALE(NST), DATA(NST),
     &             NPTS(NQ), IFAIL)
      IF (IFAIL.NE.0) RETURN

*     Moments about mean?

      CALL GEN_YESNO ('Moments about mean?', .TRUE., MMEAN, JDEF)

      IF (.NOT.MMEAN)   THEN
        FDEF = 'F8.2'
        IF (NXSSK.NE.NXS) FDEF = ' '
        XX = XTRANS (XSCALE(NST), XBAR, NPTS(NQ), IFAIL)
        CALL GEN_GETR4 ('Reference position? ('
     &                   //XAXIS_UNITS//')', XX, FDEF, XBAR, JDEF)
        IF (JDEF.EQ.0)   THEN
          XBAR = XSNART (XSCALE(NST), XBAR, NPTS(NQ), IFAIL)
          IF (IFAIL.NE.0)   RETURN
        END IF

      ELSE
        XBAR = AMOM1 (NS1, NS2, BADPIX_VAL, DATA(NST))

      END IF

      DO N = NS1, NS2
        XOFF = FLOAT(N) - XBAR
        DO MOM = 1, 5
          XMOM(MOM) = XMOM(MOM) + DATA(NST+N-1)*XOFF**(MOM-1)
        END DO
      END DO

      AREA  = XMOM(1)
      NCEN  = XBAR
      FRAC  = XBAR - NCEN
      XMOM1 = (1.-FRAC)*XSCALE(NST+NCEN-1) + FRAC*XSCALE(NST+NCEN)
      DO MOM = 1,5
        XMOM(MOM) = XMOM(MOM) * XFAC(NQ)**(MOM-1) / AREA
      END DO

      BETA1  = XMOM(4)**2/XMOM(3)**3
      SKWNSS = SIGN(SQRT(ABS(BETA1)),XMOM(4))
      BETA2  = (XMOM(5)/XMOM(3)**2)
      AKURT  = BETA2/3.
      AREA   = AREA*ABS(XFAC(NQ))
      XMOM1  = XMOM1+XMOM(2)

      WRITE(ILOUT,1001) AREA, XAXIS_UNITS, XMOM1, XAXIS_UNITS
      WRITE(ILOUT,1000) (I-1, XMOM(I), XAXIS_UNITS,I-1, I=1,3),
     &              SKWNSS, BETA1, AKURT, BETA2
      NXSSK = NXS

 1000 FORMAT(' Moments of line profile;'3(/,1X,'Mu('I1') :'
     $ F8.1' K-'A6'('I1')'),/,1X,'Skewness :',F7.2,5X,'Beta1 :',
     $ F8.3,/,1X,'Kurtosis :',F7.2,5X,'Beta2 :'F8.3)
 1001 FORMAT(' Integrated profile :'F6.1' K-'A6,/,' Mean position :'
     $ F5.1,1X,A6,/)

      RETURN
      END

*-----------------------------------------------------------------------
