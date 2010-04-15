      PROGRAM MULT
*                      GKS Example Program 3.3

*                      (XC2,YC2) is the centre and EXT2 the extent of the
*                      portion of the ship that is seen in view 2. (XC3,YC3
*                      and EXT3 have the same role in view 3.
      REAL XC2,YC2, XC3,YC3, EXT2,EXT3, HALF
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/20.0,10.0,40.0,42.0,50.0,52.0,58.0,56.0,64.0,
     :           66.0,72.0,70.0,78.0,78.0,92.0,92.0,90.0,20.0/,
     :     YSHIP/12.0,22.0,20.0,26.0,26.0,32.0,32.0,26.0,26.0,
     :           32.0,32.0,26.0,26.0,20.0,20.0,14.0,12.0,12.0/
      DATA XC2,YC2, EXT2/60.0,36.0,28.0/
      DATA XC3,YC3, EXT3/15.0,16.0,14.0/

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

*                      Set up view 1
      CALL GSWN(1, 0.0,100.0, 10.0,35.0)
      CALL GSVP(1, 0.0,1.0,    0.7,0.95)
*                      Set up view 2 on left of picture
      HALF=EXT2/2.0
      CALL GSWN(2, XC2-HALF,XC2+HALF, YC2-HALF,YC2+HALF)
      CALL GSVP(2,    0.0,   0.5,        0.1,    0.6)
*                      Set up view 3 on right of picture
      HALF=EXT3/2.0
      CALL GSWN(3, XC3-HALF,XC3+HALF, YC3-HALF,YC3+HALF)
      CALL GSVP(3,    0.5,   1.0,        0.1,    0.6)
*                      Now draw the different pieces of ship, by selecting
*                      different views in turn.
      CALL GSELNT(1)
      CALL GPL(NSHIP,XSHIP,YSHIP)
      CALL GSELNT(2)
      CALL GPL(NSHIP,XSHIP,YSHIP)
      CALL GSELNT(3)
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
