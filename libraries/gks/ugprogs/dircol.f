      PROGRAM DIRCOL
*                      GKS Example Program 2.15

*                      The following variable(s) are defined in the
*                      included file
*                      GINDIV
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
 1010 FORMAT(' Workstation type for colour device?')
      READ(*,'(I4)') IWTYPE

      CALL GOPKS (0, -1)
      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)
*                      End of standard opening sequence
*---------------------------------------------------------------------

*                      Check that the workstation supports colour
      CALL GQCF (IWTYPE, KERROR, NCOL, JCOLAV, NPCI)
      IF (JCOLAV .EQ. GCOLOR) THEN
*                      Device supports colour: check number of colours
         IF (NCOL .GE. 3) THEN
*                      Set ASPECT SOURCE FLAG for TEXT COLOUR
*                      INDEX to INDIVIDUAL
            CALL GQASF (KERROR, JASF)
            JASF(GATXCI) = GINDIV
            CALL GSASF (JASF)
*                      Now set colour representations
            CALL GSCR (1, 1, 1.0, 0.0, 0.0)
            CALL GSCR (1, 2, 0.0, 1.0, 0.0)
            CALL GSCR (1, 3, 0.0, 0.0, 1.0)
*                      Now proceed as before, setting text
*                      colour index before each line of text
            CALL GSCHH (0.1)
            CALL GSTXCI (1)
            CALL GTX (0.1, 0.7, 'Pretty')
            CALL GSTXCI (2)
            CALL GTX (0.1, 0.5, 'Coloured')
            CALL GSTXCI (3)
            CALL GTX (0.1, 0.3, 'Text')
         ENDIF
      ELSE
*                      Device doesn't support colour
         WRITE (0,*) 'Example DIRCOL: workstation does not',
     :   ' support colour'
      ENDIF

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
