      PROGRAM SEGPRI
*                      GKS Example Program 6.3

*                      The following variable(s) are defined in the
*                      included file
*                      GPOSTP, GPATTR, GWC
      INCLUDE 'GKS_PAR'
      REAL MATOUT(2,3),HORX(6),HORY(6),STARX(10),STARY(10)
      INTEGER JASF(13)
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
     :     YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/
      DATA HORX/0.0, 0.2, 0.4, 0.6, 0.8, 1.0/
      DATA HORY/0.56, 0.56, 0.54, 0.56, 0.54, 0.55/
      DATA STARX/0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.55/
      DATA STARY/0.7, 0.65, 0.92, 0.68, 0.84, 0.77, 0.86, 0.95,
     :           0.62, 0.66/
      DATA JASF/0,0,0,1,0,0,0,0,0,0,1,1,0/

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

*                      Set ASFs for individual patterns and markers
      CALL GSASF (JASF)
*                      Set fill area style patterned
      CALL GSFAIS (GPATTR)
*                      Draw ship in segment 1
      CALL GCRSG (1)
      CALL GSFASI (1)
      CALL GFA (NSHIP, XSHIP, YSHIP)
      CALL GCLSG
*                      Draw a smaller ship in segment 2
      CALL GCRSG (2)
      CALL GEVTM (0.0, 0.0, 0.4, 0.2, 0.0, 0.4, 0.6, GWC, MATOUT)
      CALL GSSGT (2, MATOUT)
      CALL GSFASI (2)
      CALL GFA (NSHIP, XSHIP, YSHIP)
      CALL GCLSG
*                      Draw horizon in segment 3
      CALL GCRSG (3)
      CALL GPL (6, HORX, HORY)
      CALL GCLSG
*                      Draw some stars outside a segment
      CALL GSMK (3)
      CALL GPM (10, STARX, STARY)
*                      Set segment priorities 1>2>3
      CALL GSSGP (1,0.8)
      CALL GSSGP (2, 0.6)
      CALL GSSGP (3, 0.1)
*                      Update workstation: postpone
      CALL GUWK (1, GPOSTP)
*                      Redraw all segments in priority order (stars go)
      CALL GRSGWK (1)

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
