      PROGRAM DRASTR
*                      GKS Example Program 5.1

*                      The following variable(s) are defined in the
*                      included file
*                      GHIGHR, GNONE
      INCLUDE 'GKS_PAR'
      INTEGER MP, JSTAT, NPOINT, KSKDNR, KTNR
      PARAMETER (MP=100)
      REAL XSK(MP), YSK(MP)

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

      KSKDNR = 1
*                      Set up a window, and viewport input priority
      CALL GSWN (1, 0.0, 100.0, 0.0, 100.0)
      CALL GSELNT (1)
      CALL GSVPIP (1, 0, GHIGHR)
*                      Input the stroke
      CALL GRQSK (1, KSKDNR, MP, JSTAT, KTNR, NPOINT,
     :                                             XSK, YSK)
*                      Number of points is in NPOINT.  Mark each point.
      IF (JSTAT .NE. GNONE) CALL GPM (NPOINT, XSK, YSK)

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
