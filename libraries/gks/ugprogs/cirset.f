      PROGRAM CIRSET
*                      GKS Example Program 2.4

*                      The following variable(s) are defined in the
*                      included file
*                      GLSOLI, GLDASH
      INCLUDE 'GKS_PAR'
*                      XA and YA arrays hold the POLYLINE coordinates
      REAL XA(0:360), YA(0:360)
*                      RADFAC is factor that converts degrees to radians
      PARAMETER (RADFAC = 3.14159 / 180.0)
*                      LNTYPE and WIDTH are the desired attributes
*                      for each of the 4 bundles
      INTEGER LNTYPE(4)
      REAL WIDTH(4)
      DATA LNTYPE/GLSOLI, GLDASH, GLSOLI, GLDASH/
      DATA WIDTH/1.0, 1.0, 2.0, 2.0/

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

* Set bundle representations
      DO 10 KPLI=1,4
         CALL GSPLR (1, KPLI, LNTYPE(KPLI), WIDTH(KPLI), 1)
   10 CONTINUE
*                      Loop over bundles, changing radius
      DO 30 KPLI=1,4
         RADIUS = FLOAT (KPLI) * 0.1
*                      Generate POLYLINE for circle
         DO 20 J=0,360
            RADIAN = FLOAT (J) * RADFAC
            XA(J) = RADIUS * COS(RADIAN)+0.5
            YA(J) = RADIUS * SIN(RADIAN)+0.5
   20    CONTINUE
*                      Set POLYLINE bundle index
         CALL GSPLI (KPLI)
*                      Draw the circle
         CALL GPL   (361, XA, YA)
   30 CONTINUE
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
