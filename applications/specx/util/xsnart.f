*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*      12 May 2003 (timj):
*        Protect accesso to X(0)
*-----------------------------------------------------------------------

      REAL FUNCTION XSNART (X, V, NX, IFAIL)

C  Routine to return postion in X array (i.e. fractional points)
C  corresponding to the input value V.
C  Algorithm:
C      o  find approximate starting value
C      o  apply correction until V lies between translations
C         of two adjacent channels.
C      o  linear interpolation to finish off.
C  For the curious, "XSNART" is the reverse operation to "XTRANS"

      IMPLICIT NONE

*     Formal parameters

      REAL     X(*)         ! current scaled x-axis of plot
      REAL     V            ! value to be translated
      INTEGER  NX           ! length of xscale array
      INTEGER  IFAIL        ! error return

*     Local variables

      INTEGER  N1
      REAL     DX
      REAL     XL,  XR
      REAL     XLL, XRR

C  Ok, go...

      IFAIL  = 0
      XSNART = 0.0

C     XSCALE is defined on channel numbers 1 --> NX
C     Set up a notional scale from 0 (left edge of channel 1)
C       to NX (right edge of channel NX), limits XL to XR

      XL = X(1)  - 0.5*( X(2) -X(1)    )
      XR = X(NX) + 0.5*( X(NX)-X(NX-1) )
CD    print *, ' -- xsnart --'
CD    print *, '  left and right limits: ', xl, xr

*     test that value lies in range of axis...

      IF (V.LT.MIN(XL,XR) .OR. V.GT.MAX(XL,XR))   THEN
        IFAIL = 26
        RETURN
      END IF

*     first guess: linear interpolation. Result N1 is guaranteed to be in
*     range (0, NX)

      N1 = (FLOAT(NX) * (V-XL) / (XR-XL))

      IF (N1.EQ.0) THEN
        XLL = XL
        XRR = 0.5*(X(2)+X(1))
      ELSE IF (N1 .EQ. 1) THEN
        XLL = 0.5*X(N1) ! Assume X(N1-1)=0
        XRR = 0.5*(X(N1)+X(N1+1))
      ELSE IF (N1.EQ.NX) THEN
        XLL = 0.5*(X(NX)+X(NX-1))
        XRR = XR
      ELSE
*     Can not use this for N1=1 since that would address index 0
        XLL = 0.5*(X(N1)+X(N1-1))
        XRR = 0.5*(X(N1)+X(N1+1))
      END IF

      DX = XRR - XLL

CD    print *, '  first guess channel: ', n1
CD    print *, '  channel boundaries xll and xrr: ', xll, xrr
CD    print *, '  channel increment dx: ', dx

*     iterate until we know location to within one channel

      DO WHILE (V.LT.MIN(XLL,XRR) .OR. V.GT.MAX(XLL,XRR))

        N1 = N1 + SIGN (1.0, (V-0.5*(XLL+XRR))/DX)

        IF (N1.LE.0) THEN
          XLL = XL
          XRR = 0.5*(X(2)+X(1))
        ELSE IF (N1.EQ.1) THEN
          XLL = 0.5*X(N1) ! Assume X(N1-1)=0
          XRR = 0.5*(X(N1)+X(N1+1))
        ELSE IF (N1.GE.NX) THEN
          XLL = 0.5*(X(NX)+X(NX-1))
          XRR = XR
        ELSE
C     Can not use this with N1=1 since that uses index=0
          XLL = 0.5*(X(N1)+X(N1-1))
          XRR = 0.5*(X(N1)+X(N1+1))
        END IF

        DX  = XRR - XLL

CD      print *, '  next guess channel: ', n1
CD      print *, '  channel boundaries xll and xrr: ', xll, xrr
CD      print *, '  channel increment dx: ', dx

      END DO

*     final interpolation

      IF (N1 .NE. 0) THEN
         XSNART = N1 + (V-X(N1))/DX
      ELSE
*     must protect addressing index 0
         XSNART = V/DX
      END IF
CD    print *, '  final value: ', xsnart

      RETURN
      END

*-----------------------------------------------------------------------
