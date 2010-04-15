      REAL*8 FUNCTION SWPITF(WAVE, F0)

*+
*
*   Name:
*      REAL*8 FUNCTION SWPITF
*
*   Description:
*      Correct for SWP ITF SNAFFU.
*
*   History:
*      Jack Giddings     ??-???-??     IUEDR Vn. 1.0
*      Paul Rees         08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      CORRECTS IN AN APPROXIMATE FASHION FOR THE 25% ERROR IN TH 20%
*      FLAT FIELD AND THE 4% AND 2.5% ERRORS IN THE 10% AND 20% FLAT
*      FIELDS.
*      Code adapted from Toon Snijders SWPLORES.FOR program.
*
*-

*   Implicit:
      IMPLICIT NONE

*    Import:
      REAL*8 WAVE        ! wavelength

      REAL*8 F0          ! pixel FN = (TN-2000)

*   Export:
*     float swpitf     ! corrected pixel FN

*   Local variables:
      INTEGER JD
      INTEGER JU

      REAL*8 COR
      REAL*8 DCDW
      REAL*8 FN
      REAL*8 W
      REAL*8 X

*   Initialisations:
      REAL*8 WDELTA(7)
      DATA WDELTA/1120., 1200., 1300., 1400., 1800., 1900., 2000./

      REAL*8 DELTA(7)
      DATA DELTA/835.0, 757.27, 727.75, 699.29, 699.29, 754.29, 754.29/

      REAL*8 X4P2
      REAL*8 X40
      REAL*8 X20
      REAL*8 X10
      DATA X4P2/6291.1/, X40/4291.1/, X20/2141.1/, X10/1084.3/

      REAL*8 E10
      REAL*8 E20
      REAL*8 E2000
      REAL*8 CORA
      DATA E10/0.9606/, E20/0.9748/, E2000/2000./, CORA/6.5925E-6/

      REAL*8 CORB
      REAL*8 DI21
      REAL*8 DI42
      DATA CORB/7.5608E-6/, DI21/9.4630E-4/, DI42/4.6511E-4/

*   Interpolate a value of "COR" in wavelength space.
      JU = 2

 100  CONTINUE

      IF (.NOT.(JU.LT.7)) GO TO 200

      IF (WAVE.LE.WDELTA(JU)) GO TO 200
      JU = JU + 1

      GO TO 100

 200  CONTINUE

      JD = JU - 1
      W = MAX(WDELTA(JD), MIN(WDELTA(JU), WAVE))
      DCDW = (DELTA(JU) - DELTA(JD))/(WDELTA(JU) - WDELTA(JD))
      COR = (W - WDELTA(JD))*DCDW + DELTA(JD)

*   Use interpolated value of "COR" to correct F0 and store in FN
      X = F0

      IF (X.LE.X10) THEN

         FN = X*E10

      ELSE IF (X.LE.X20) THEN

*   CORRECT FOR 2.5% AND 4% ERRORS IN 20% AND 40% ITF ENTRIES,
*   CORB=(0.0394-0.0252)/D21,D21=X20-X10,DI21=1.0/D21
         FN = X - (X - X10)*DI21*COR
         FN = FN*(E20 - (X20 - X)*CORB)

      ELSE IF (X.LE.X40) THEN

*   CORRECT 2.5% EXPOSURE LEVEL ERROR IN 20% FLAT FIELD,
*   IGNORE ERRORS IN HIGHER ENTRIES,CORA=0.0252/D42,D42=X40-X20,
*   DI42=1/D42
         FN = X - (X40 - X)*DI42*COR
         FN = FN*(1.0 - (X40 - X)*CORA)

      ELSE

         FN = X

      END IF

      SWPITF = FN

      END
