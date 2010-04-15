      PROGRAM INDIV
*                      GKS Example Program 8.3


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

      CALL SHIPWD

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



      SUBROUTINE SHIPWD
*                      Draw a ship using individual
*                      attributes.
*                      The following variable(s) are defined in the
*                      included file
*                      GINDIV, GLSOLI
      INCLUDE 'GKS_PAR'

*                      Set up parameters with names for the AFSs
      INTEGER GALN, GALWSC, GAPLCI
      PARAMETER (GALN=1, GALWSC=2, GAPLCI=3)
      INTEGER JASF(13), LASF1, LASF2, LASF3, KPLCOL, LNTYPE, KERROR
      REAL WIDTH
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
     :     YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/
*
*                      JASF    Aspect Source Flags
*                      LASF1-3 SAVED VALUES OF LASF
*                      KPLCOL    Saved Polyline Colour index
*                      LNTYPE   Saved line type
*                      KERROR  Error indicator
*                      WIDTH   Saved linewidth scale factor
*                      Find current settings
      CALL GQASF (KERROR,JASF)
      IF (KERROR.GT.0) GO TO 1
      LASF1 = JASF(GALN)
      LASF2 = JASF(GALWSC)
      LASF3 = JASF(GAPLCI)
      CALL GQLN (KERROR, LNTYPE)
      IF (KERROR.GT.0) GO TO 1
      CALL GQPLCI (KERROR, KPLCOL)
      IF (KERROR.GT.0) GO TO 1
      CALL GQLWSC (KERROR, WIDTH)
      IF (KERROR.GT.0) GO TO 1
*                      Set individual attributes
      JASF(GALN)   = GINDIV
      JASF(GALWSC) = GINDIV
      JASF(GAPLCI) = GINDIV
      CALL GSASF (JASF)
      CALL GSLN  (GLSOLI)
      CALL GSPLCI (1)
      CALL GSLWSC (2.0)
*                      Draw the ship
      CALL GPL(NSHIP, XSHIP, YSHIP)
*                      Reset to original settings
      JASF(GALN)   = LASF1
      JASF(GALWSC) = LASF2
      JASF(GAPLCI) = LASF3
      CALL GSASF(JASF)
      CALL GSLN   (LNTYPE)
      CALL GSPLCI (KPLCOL)
      CALL GSLWSC (WIDTH)
    1 CONTINUE
      END
