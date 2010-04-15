      PROGRAM WISS
*                      GKS Example Program 6.4

      REAL MATRX1(2,3), MATRX2(2,3)
      INTEGER NBOAT, NSAIL, NSHIP
      PARAMETER (NBOAT = 17, NSAIL = 5, NSHIP = 18)
      REAL XBOAT(NBOAT), YBOAT(NBOAT)
      REAL XSAIL(NSAIL), YSAIL(NSAIL)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
*
      DATA XBOAT /0.20,0.25,0.31,0.39,0.43,0.46,0.50,0.53,0.56,
     :            0.60,0.63,0.66,0.70,0.72,0.76,0.80,0.20/
      DATA YBOAT /0.30,0.22,0.15,0.10,0.08,0.10,0.08,0.10,0.08,
     :            0.10,0.08,0.10,0.08,0.10,0.15,0.20,0.30/
      DATA XSAIL /0.50,0.50,0.30,0.76,0.50/
      DATA YSAIL /0.25,0.80,0.35,0.26,0.80/
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
      DATA YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/
*

*                      OPEN GKS, Open and Activate WISS as workstation 2.
      CALL GOPKS (0, -1)
      CALL GOPWK (2 , 12, 3)
      CALL GACWK (2)

*                      Create segment 1 containing a boat and then close it
      CALL GCRSG(1)
      CALL GPL(NBOAT,XBOAT,YBOAT)
      CALL GPL(NSAIL,XSAIL,YSAIL)
      CALL GCLSG
*                      Create segment 2 containing a ship and then close it
      CALL GCRSG(2)
      CALL GPL(NSHIP,XSHIP,YSHIP)
      CALL GCLSG

*                      OPEN and ACTIVATE WORKSTATION.

      WRITE(*,1000)
 1000 FORMAT(' Connection identifier?')
      READ(*,'(I2)') ICONID
      WRITE(*,1010)
 1010 FORMAT(' Workstation type?')
      READ(*,'(I4)') IWTYPE

      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)

*                      Evaluate transformation matrices
      CALL GEVTM(0.0,0.0, 0.00,0.00, 0.0, 0.5,0.5, 0, MATRX1)
      CALL GEVTM(0.0,0.0, 0.50,0.75, 0.0, 0.5,0.5, 0, MATRX2)
*                      Insert segments: a small boat is drawn in the lower
*                      left hand corner of the frame and a small ship is
*                      drawn in the top right hand corner of the frame
      CALL GINSG(1,MATRX1)
      CALL GINSG(2,MATRX2)
*                      Deactivate and close WISS
      CALL GDAWK(2)
      CALL GCLWK(2)

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
