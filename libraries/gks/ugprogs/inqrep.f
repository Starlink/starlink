      PROGRAM INQREP
*                      GKS Example Program 8.4


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

      CALL SHIPWD(1)

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

      SUBROUTINE SHIPWD(KWKID)
*                      Draw a ship using a workstation independent
*                      polyline representation. This routine produces
*                      the desired effect on workstation KWKID only.
*                      Any other active workstation will draw the ship
*                      according to its current bundle tables.
*
*                      The following variable(s) are defined in the
*                      included file
*                      GREALI, GLSOLI
      INCLUDE 'GKS_PAR'
      INTEGER KWKID
      INTEGER KPLI, KPLCOL, LNTYPE, KERROR
      REAL WIDTH
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
     :     YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/
*                      KPLI    Polyline index
*                      KPLCOL    Saved Polyline Colour index
*                      LNTYPE  Saved line type
*                      KERROR  Error indicator
*                      WIDTH   Saved linewidth scale factor
*                      Find current settings
      CALL GQPLI(KERROR, KPLI)
      IF (KERROR.GT.0) GO TO 1
      CALL GQPLR(KWKID, KPLI, GREALI, KERROR, LNTYPE, WIDTH, KPLCOL)
      IF (KERROR.GT.0) GO TO 1
*                      Set representation
      CALL GSPLR(KWKID, KPLI, GLSOLI, 2.0, 1)
*                      Draw the ship
      CALL GPL(NSHIP, XSHIP, YSHIP)
*                      Reset to original settings
      CALL GSPLR(KWKID, KPLI, LNTYPE, WIDTH, KPLCOL)
    1 CONTINUE
      END
