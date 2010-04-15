      PROGRAM POLY2
*                      GKS Example Program 2.2

      REAL XA(3), YA(3), XDIFF, YDIFF
      DATA XA/0.1,0.0,0.9/ YA/0.1,0.325,0.3/
      DATA XDIFF/0.1/ YDIFF/0.025/

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

      DO 10 J=1,9
         XA(2) = XA(2) + XDIFF
         YA(2) = YA(2) - YDIFF
         CALL GPL (3 , XA , YA)
   10 CONTINUE

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
