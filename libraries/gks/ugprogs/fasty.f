      PROGRAM FASTY
*                      GKS Example Program 2.10

*                      The following variable(s) are defined in the
*                      included file
*                      GSOLID, GPATTR, GHOLLO
      INCLUDE 'GKS_PAR'
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XNSHIP(NSHIP), YNSHIP(NSHIP)
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

*                      Set Fill Area Representations for different interior
*                      styles
      CALL GSFAR (1, 1, GSOLID, 0, 1)
      CALL GSFAR (1, 2, GHATCH, -1, 1)
      CALL GSFAR (1, 3, GHOLLO, 0, 1)
*                      Draw three ships at different sizes in different
*                      styles
      CALL GSFAI (1)
      CALL GFA (NSHIP, XSHIP, YSHIP)
      DO 10 I=1,NSHIP
         XNSHIP(I) = XSHIP(I) * 0.5 + 0.5
         YNSHIP(I) = YSHIP(I) * 0.5 + 0.5
   10 CONTINUE
      CALL GSFAI (2)
      CALL GFA (NSHIP, XNSHIP, YNSHIP)
      DO 20 I=1,NSHIP
         XNSHIP(I) = XSHIP(I) * 0.375
         YNSHIP(I) = YSHIP(I) * 0.375 + 0.75
   20 CONTINUE
      CALL GSFAI (3)
      CALL GFA (NSHIP, XNSHIP, YNSHIP)
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
