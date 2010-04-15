      PROGRAM SEG2
*                      GKS Example Program 6.2

*                      The following variable(s) are defined in the
*                      included file
*                      GWC,GPERFO
      INCLUDE 'GKS_PAR'
*
      REAL MATOUT (3,2)
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
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

*                                      Create segment number 1
      CALL GCRSG (1)
*                                      Draw picture
      CALL GPL (NSHIP,XSHIP,YSHIP)
*                                      Close segment
      CALL GCLSG
*                                      Change segment transformation
*                                      to give scale, rotation and shift
      PI = 3.14159
      CALL GEVTM (0.0, 0.0, 0.2, 0.0, PI/12, 0.5, 0.5, GWC, MATOUT)
      CALL GSSGT (1, MATOUT)
      CALL GUWK(1,GPERFO)

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
