C-----------------------------------------------------------------------

      SUBROUTINE DELCHN (NQ,IFAIL)

C   Routine to drop unequal numbers of points from the ends of each
C   quadrant of data. Useful if excess noise in correlator data e.g.

C   History:
C      6-JUN-2000 (AJC):
C        Replace 'TYPE *' with 'PRINT *'

      IMPLICIT  NONE

*     Formal parameters
      INTEGER   NQ
      INTEGER   IFAIL

*     Include files:
      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

*     Local variables:

      INTEGER   J
      INTEGER   JDEF
      INTEGER   N
      INTEGER   NQ1, NQ2
      INTEGER   NPTSNEW(8)
      INTEGER   NST, NOFF

      INTEGER   IDC(2)
      EQUIVALENCE (IDC(1),IDC1)

*     Functions
      INTEGER   NTOT
      LOGICAL   DOQUAD

      IFAIL = 0

      PRINT *,          'Delete channels from ends of quadrant(s)'
      CALL GEN_GETI4A ('No of points to remove? (low and high)',
     &                  IDC, 2, '2(I4,1X)', IDC, JDEF)

      CALL INITNPNEW (NPTSNEW)
      CALL QLIM      (NQ, NQ1, NQ2)

      DO N = NQ1, NQ2
        IF (DOQUAD(N))  THEN

          IF ((IDC1+IDC2) .GE. NPTS(N))   THEN
            IFAIL = 5
            PRINT *,'Warning, not enough points in quadrant', N

          ELSE
            NPTSNEW(N)  = NPTS(N)-IDC1-IDC2
            ACHAN       = FLOAT(IDC1-IDC2)/2.

            CALL LSRCOR (LSRFLG, VSL, VES, VTE, VLSR,
     &                   IDATE, ITIME, IUTFLG, RA, DEC,
     &                   JFREST(N),  JFCEN(N),  LOFREQ(N),
     &                   IFFREQ(N),  ACHAN,     JFINC(N))

            NST         = NTOT (N-1)
            NOFF        = IDC1
            DO J = 1, NPTSNEW(N)
              DATA(NST+J) = DATA(NST+J+NOFF)
            END DO
          END IF
        END IF
      END DO

      CALL COMPRESSQ (NPTSNEW)

      RETURN
      END


