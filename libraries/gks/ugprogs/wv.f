      PROGRAM WV
*                      GKS Example Program 3.1

*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/20.0,10.0,40.0,42.0,50.0,52.0,58.0,56.0,64.0,
     :           66.0,72.0,70.0,78.0,78.0,92.0,92.0,90.0,20.0/,
     :     YSHIP/12.0,22.0,20.0,26.0,26.0,32.0,32.0,26.0,26.0,
     :           32.0,32.0,26.0,26.0,20.0,20.0,14.0,12.0,12.0/

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

*                      Set window and viewport
      CALL GSWN(1, 0.0,100.0,  10.0,35.0)
      CALL GSVP(1, 0.0,1.0,    0.75,1.0)
*                      Select and draw
      CALL GSELNT(1)
      CALL GPL(NSHIP,XSHIP,YSHIP)

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
