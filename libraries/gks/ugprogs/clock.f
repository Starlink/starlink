      PROGRAM CLOCK
*                      GKS Example Program 2.9

*                      The following variable(s) are defined in the
*                      included file
*                      GSTRKP, GACENT, GATOP, GAHALF, GAVNOR, GABOTT, GACAP
      INCLUDE 'GKS_PAR'
      CHARACTER*39 NUM
      INTEGER NUMLEN(12), JASF(13), ICONID, IWTYPE, ISTART, IHOUR, IEND
      REAL HANGLE, HX, HY
      PARAMETER(HANGLE=3.14159/6.0)
      DATA NUM/'I II III IIII V VI VII VIII IX X XI XII'/
      DATA NUMLEN /1,2,3,4,1,2,3,4,2,1,2,3/
      DATA JASF /13*GINDIV/

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

*                      Set ASPECT SOURCE FLAGS to INDIVIDUAL
      CALL GSASF (JASF)
*                      Set font
      CALL GSTXFP (-104,GSTRKP)
*                      Set attributes for numerals on clock face
      CALL GSCHH (0.07)
      CALL GSCHXP (0.5)
      CALL GSTXAL (GACENT,GATOP)
      ISTART = 1
*                      Loop to draw each Roman numeral appropriately sloped
      DO 10 IHOUR=1,12
*                      Calculate and set character up vector for numeral
         HX = SIN (IHOUR*HANGLE)
         HY = COS (IHOUR*HANGLE)
         CALL GSCHUP (HX,HY)
*                      Get numeral from numeral string and draw it
         IEND = ISTART + NUMLEN(IHOUR) - 1
         CALL GTX (0.35*HX + 0.5, 0.35*HY + 0.5,NUM(ISTART:IEND))
         ISTART = IEND + 2
  10  CONTINUE
*                      Set attributes and draw Hour hand
      CALL GSCHH(0.02)
      CALL GSCHXP (1.5)
      CALL GSCHSP (-0.1)
      CALL GSTXAL (GAHALF,GAHALF)
      CALL GTX(0.5,0.5,'HOURS ')
*                      Set attributes and draw Minute hand
      CALL GSTXAL (GAVNOR,GABOTT)
      CALL GSTXP (GDOWN)
      CALL GTX(0.5,0.5,'MINUTES ')
*                      Set attributes and draw centre
      CALL GSTXAL (GACAP,GAHALF)
      CALL GSCHXP (1.0)
      CALL GTX(0.5,0.5,'*')

*---------------------------------------------------------------------
*                      Update workstation and await operator action
      CALL GUWK(1, 1)
      PAUSE
*                      Deactivate and close workstation, close GKS
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      END

