      PROGRAM CELLEX
*                      GKS Example Program 2.11

      INTEGER KCOLA(8,8)
*                      FORTRAN unit number for input
      INTEGER IUNIT
      PARAMETER (IUNIT = 12)

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

*                      Read data row by row
      OPEN (UNIT=IUNIT, FILE='cellex.dat',
     : STATUS='OLD')
      READ (IUNIT, '(8I1)') KCOLA
*                      Draw cell array
      CALL GCA (0.4, 0.6, 0.6, 0.4, 8, 8, 1, 1, 8, 8, KCOLA)

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
