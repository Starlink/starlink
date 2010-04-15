      PROGRAM TEXTEX
*                      GKS Example Program 2.8

*                      The following variable(s) are defined in the
*                      included file
*                      GINDIV, GACHXP, GACHSP, GSTRKP, GACENT, GABASE
      INCLUDE 'GKS_PAR'

*                      Set up parameters with names for the AFSs
      INTEGER    GALN,      GALWSC,    GAPLCI,   GAMK,     GAMKSC
      PARAMETER (GALN=1,    GALWSC=2,  GAPLCI=3, GAMK=4,   GAMKSC=5)
      INTEGER    GAPMCI,    GATXFP,    GACHXP,   GACHSP,   GATXCI
      PARAMETER (GAPMCI=6,  GATXFP=7,  GACHXP=8, GACHSP=9, GATXCI=10)
      INTEGER    GAFAIS,    GAFASI,    GAFACI
      PARAMETER (GAFAIS=11, GAFASI=12, GAFACI=13)
*                      JASF is an array that holds the ASFS
      INTEGER JASF(13)

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

*                      Set ASPECT SOURCE FLAG for CHARACTER EXPANSION
*                      FACTOR and CHARACTER SPACING to INDIVIDUAL
      CALL GQASF (KERROR, JASF)
      JASF(GACHXP) = GINDIV
      JASF(GACHSP) = GINDIV
      CALL GSASF (JASF)
*                      Set bundle representations
      CALL GSTXR (1, 1, -104, GSTRKP, 1.0, 0.0, 1)
      CALL GSTXR (1, 2, -105, GSTRKP, 1.0, 0.0, 1)
      CALL GSTXR (1, 3, -106, GSTRKP, 1.0, 0.0, 1)
*                      Increase character spacing for all strings
      CALL GSCHSP (0.1)
*                      Set alignment to centre strings
      CALL GSTXAL (GACENT, GABASE)
*                      Set larger characters
      CALL GSCHH (0.09)
*                      Draw text with different bundle indices
      CALL GSTXI (2)
      CALL GTX (0.5, 0.75, 'DIFFERENT')
      CALL GSTXI (1)
      CALL GTX (0.5, 0.55, 'BUNDLES')
*                      Set smaller characters
      CALL GSCHH (0.05)
      CALL GTX (0.5, 0.4, 'FOR')
*                      Restore larger characters
      CALL GSCHH (0.09)
      CALL GSTXI (3)
      CALL GTX (0.5, 0.2, 'EMPHASIS')

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
