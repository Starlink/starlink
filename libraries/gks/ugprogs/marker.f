      PROGRAM MARKER
*                      GKS Example Program 2.6

*                      XTEMP and YTEMP hold scaled SHIP
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XTEMP(NSHIP), YTEMP(NSHIP)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
     :     YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/

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

*                      Set scaling factor for ship's outline
      DO 20 KPMI=2,5
         FACTOR = 0.5 + FLOAT (KPMI-1) * 0.1
         DO 10 J=1,NSHIP
            XTEMP(J) = XSHIP(J) * FACTOR
            YTEMP(J) = YSHIP(J) * FACTOR+1.0-FLOAT(KPMI-1)*0.25
   10    CONTINUE
*                      Set POLYMARKER bundle index
         CALL GSPMI (KPMI)
*                      Draw the outline as solid POLYLINE
         CALL GPL (NSHIP, XTEMP, YTEMP)
*                      Mark each vertex with a marker
*                      No need to do last point, which is a
*                      duplicate of the first in this case
         CALL GPM (NSHIP-1, XTEMP, YTEMP)
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
