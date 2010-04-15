      PROGRAM GDPEX
*                      GKS Example Program 2.12

*                      The following variable(s) are defined in the
*                      included file
*                      GHATCH
      INCLUDE 'GKS_PAR'
      CHARACTER*80 NODAT(1)
      REAL XC, YC, RADIUS
      REAL XA(3), YA(3), THETAD(3)
      DATA XC, YC, RADIUS/0.5, 0.5, 0.375/
      DATA THETAD/150.0, 115.0, 95.0/

*                      Open GKS, open and activate workstation.

      WRITE(*,1000)
 1000 FORMAT(' Connection identifier?')
      READ(*,'(I2)') ICONID
      WRITE(*,1010)
 1010 FORMAT(' Workstation type?')
      READ(*,'(I4)') IWTYPE

      CALL GOPKS (0, -1)
      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)
*                      End of standard opening sequence
*---------------------------------------------------------------------

*                      Calculate PI
      PI = 4.0 * ATAN (1.0)
*                      Set Representations for 3 sectors of pie chart
      CALL GSFAR (1, 1, GHATCH, -1, 1)
      CALL GSFAR (1, 2, GHATCH, -2, 1)
      CALL GSFAR (1, 3, GHATCH, -3, 1)
*                      Calculate first point of first sector
      TH = 0.0
      XA(1) = XC + RADIUS * SIN (TH)
      YA(1) = YC + RADIUS * COS (TH)
*                      Draw three sectors of pie chart
      DO 20 I=1,3
*                      Calculate half sector angle in radians
         THETAR = THETAD(I) * PI / 360.0
*                      Calculate two other points for sector
         DO 10 J=2,3
            TH = TH + THETAR
            XA(J) = XC + RADIUS * SIN (TH)
            YA(J) = YC + RADIUS * COS (TH)
   10    CONTINUE
*                      Set fill area bundle index
         CALL GSFAI (I)
*                      Draw sector (no data in data record)
         CALL GGDP (3, XA, YA, -3, 1, NODAT)
*                      Set first point of next sector to last point of this
         XA(1) = XA(3)
         YA(1) = YA(3)
   20 CONTINUE

*---------------------------------------------------------------------
*                      Update workstation and await operator action
*                      before finishing
      CALL GUWK(1, 1)
      PAUSE
*                      Deactivate and close workstation, close GKS
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      END
