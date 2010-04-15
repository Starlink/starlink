*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION XSNART8 (X, V, NX, IFAIL)

C  Routine to return postion in X array (i.e. fractional points)
C  corresponding to the input value V.
C  Algorithm:
C      o  find approximate starting value
C      o  apply correction until V lies between translations
C         of two adjacent channels.
C      o  linear interpolation to finish off.
C
C  Error return:
C      o  IFAIL = 0;  success
C      o  IFAIL = 26; input point not in range
C
C  For the curious, "XSNART" is the reverse operation to "XTRANS"

      IMPLICIT NONE

*     Formal parameters

      REAL*8   X(*)         ! current scaled x-axis of plot
      REAL*8   V            ! value to be translated
      INTEGER  NX           ! length of xscale array
      INTEGER  IFAIL        ! error return

*     Local variables

      INTEGER  N1
      REAL*8   DX
      REAL*8   XL,  XR
      REAL*8   XLL, XRR

C  Ok, go...

      IFAIL  = 0
      XSNART8 = 0.0

C     XSCALE is defined on channel numbers 1 --> NX
C     For consistency need to work with values of X defined at the channels,
C       so need to define notional channels 0 and NX+1 which are only half
C       the distance from 0 or NX as the next adjacent channel
C     XL and XR represent these channel values.

      XL = X(1)  - 0.505*( X(2) -X(1)    )
      XR = X(NX) + 0.505*( X(NX)-X(NX-1) )
CD    PRINT *, ' -- xsnart8 --'
CD    PRINT *, '  left and right limits: ', xl, xr

*     test that input value, V, lies in range of axis...

      IF (V.LT.MIN(XL,XR) .OR. V.GT.MAX(XL,XR))   THEN
        IFAIL = 26
        RETURN
      END IF

*     first guess: linear interpolation. Need to determine which *interval*
*       V lies in, not which channel it is associated with. So, given that
*       we have rejected anything out of the range (XL,XR), N1 must be
*       in range (0,NX)

      N1 = INT ((FLOAT(NX) * (V-XL) / (XR-XL)) + 0.5)

      IF (N1.EQ.0) THEN
        XLL = XL
        XRR = X(1)
      ELSE IF (N1.EQ.NX) THEN
        XLL = 0.5*X(NX)
        XRR = XR
      ELSE
        XLL = X(N1)
        XRR = X(N1+1)
      END IF

      DX = XRR - XLL

CD    PRINT *, '  first guess channel: ', n1
CD    PRINT *, '  channel boundaries xll and xrr: ', xll, xrr
CD    PRINT *, '  channel increment dx: ', dx

*     If V lies outside edges of current channel, iterate until
*       we know location to within one channel

      DO WHILE (V.LT.MIN(XLL,XRR) .OR. V.GT.MAX(XLL,XRR))

        IF ((V-XLL)/DX .GT. 0) THEN
          N1 = N1 + 1
        ELSE
          N1 = N1 -1
        END IF

        IF (N1 .GE. NX .OR. N1. LT. 0) THEN
          PRINT *, ' -- XSNART8 -- Program error; iterated out of range'
          IFAIL = 26
          RETURN
        ELSE IF (N1 .EQ. 0) THEN
          XLL = XL
          XRR = X(1)
        ELSE IF (N1. EQ. NX) THEN
          XLL = X(NX)
          XRR = XR
        ELSE
          XLL = X(N1)
          XRR = X(N1+1)
        END IF

        DX  = XRR - XLL

CD      PRINT *, '  next guess channel: ', n1
CD      PRINT *, '  channel boundaries xll and xrr: ', xll, xrr
CD      PRINT *, '  channel increment dx: ', dx

      END DO

*     final interpolation

      IF (N1 .EQ. 0) THEN
        XSNART8 = 1.0 + (V-X(1))/(2.*DX)
      ELSE IF (N1 .EQ. NX) THEN
        XSNART8 = NX  + (V-X(NX))/(2.*DX)
      ELSE
        XSNART8 = FLOAT(N1) + (V-X(N1))/DX
      END IF

CD    PRINT *, '  final value: ', xsnart8

      RETURN
      END

*-----------------------------------------------------------------------
