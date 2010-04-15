      PROGRAM CIRIND
*                      GKS Example Program 2.5

*                      The following variable(s) are defined in the
*                      included file
*                      GINDIV, GLSOLI, GLDASH
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
*                      XA and YA arrays hold the POLYLINE coordinates
      REAL XA(0:360), YA(0:360)
*                      RADFAC is factor that converts degrees to radians
      REAL RADFAC
      PARAMETER (RADFAC = 3.14159 / 180.0)
*                      LNTYPE and WIDTH are the desired attributes
      INTEGER LNTYPE(4)
      REAL WIDTH(4)
      DATA LNTYPE/GLSOLI, GLDASH, GLSOLI, GLDASH/
      DATA WIDTH/1.0, 1.0, 2.0, 2.0/

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

*                      Inquire current ASFs and set LINETYPE and
*                      LINEWIDTH SCALE FACTOR to be INDIVIDUAL
      CALL GQASF (KERROR, JASF)
      JASF(GALN) = GINDIV
      JASF(GALWSC) = GINDIV
      CALL GSASF (JASF)
*                      Loop over linetypes and linewidths, changing radius
      DO 20 KPLI=1,4
         RADIUS = FLOAT (KPLI) * 0.1
*                      Generate POLYLINE for circle
         DO 10 J=0,360
            RADIAN = FLOAT (J) * RADFAC
            XA(J) = RADIUS * COS(RADIAN)+0.5
            YA(J) = RADIUS * SIN(RADIAN)+0.5
   10    CONTINUE
*                      Set linetype and linewidth
         CALL GSLN   (LNTYPE(KPLI))
         CALL GSLWSC (WIDTH(KPLI))
*                      Draw the circle
         CALL GPL (361 , XA , YA)
   20 CONTINUE

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
