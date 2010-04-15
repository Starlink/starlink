      REAL*8 FUNCTION LWRITF(WV, FN)

*+
*
*   Name:
*      REAL*8 FUNCTION LWRITF
*
*   Description:
*      LWR ITF correction from IUE3.FOR.
*
*   History:
*      Jack Giddings     ??-???-??     IUEDR Vn. 1.0
*      Paul Rees         08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      LWR ITF TABLE CORRECTION VALID FOR THE PERIOD SUMMER 1978,FALL
*      1982, SOURCE HOLM AND SNIjdERS 1982,THIS ASSUMES A POSITION
*      INDEPENDANT CORRECTION.
*      LWR ITF correction valid for the period Summer 1978 to Fall 1982.
*      The new ITF levels are from "Holm and Snijders, 1982", and
*      are assumed position-independent.
*
*      The fn0 and fnc are "FN" leveles corresponding to the old and
*      new ITF tables respectivly.
*      They relate to the table of "1PC" numbers in the header (called
*      ITFNs), through:  FN = ITFN*17/(0.283*100) where the
*      "17" and "0.283" constants are defined in the header.
*      Interpolation below and within the table is done linearly.
*      FN values above and below the ITF table boundaries are
*      extrapolated linearly.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 WV           ! pixel wavelength
      REAL*8 FN           ! pixel FN = TN - 2000 (GPHOT)

*   Export:
*     float lwritf      ! correct pixel FN

*   Local variables:
      INTEGER JD
      INTEGER JU

*   Initialisations:
      REAL*8 FN0(12)      ! FN values for old (2nd) ITF
      DATA FN0/0., 1382., 2441., 4805., 6044., 7127., 9530., 12089.,
     :     14683., 17635., 20600., 25233./

      REAL*8 FNC(12)      ! FN values for new ITF
      DATA FNC/0., 1205., 2193., 4533., 5842., 7062., 9706., 12455.,
     :     15164., 17934., 20531., 25233./

      REAL*8 GRAD(12)     ! gradients between two ITFs
      DATA GRAD/0.8719, 0.9330, 0.9898, 1.0565, 1.1265, 1.1003, 1.0742,
     :     1.0443, 0.9383, 0.8759, 1.0149, 1.0/

*   Find range in old ITF table.
      JU = 2

 100  CONTINUE

      IF (.NOT.(JU.LE.12)) GO TO 200
      IF (FN.LE.FN0(JU)) GO TO 200
      JU = JU + 1
      GO TO 100

 200  CONTINUE

*   Interpolate using tabulated gradient for chosen range
      JD = JU - 1
      LWRITF = FNC(JD) + (FN - FN0(JD))*GRAD(JD)

      END
