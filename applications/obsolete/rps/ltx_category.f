*+LTX_CATEGORY     Finds where to put the Category X
      
      SUBROUTINE LTX_CATEGORY(IFIELD,IFILE,STRING,XS,YS,STATUS)
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER IFIELD			! In	Database Field no.
      INTEGER IFILE			!	   ::	 section
      CHARACTER*(*) STRING		! In	Holds Value
      CHARACTER*6 XS,YS			! Out	Strings with X,Y values
      INTEGER STATUS			!	Status, 0 = OK, else -1
 
*  Global Variables
      INCLUDE 'com_form_latex.inc'
 
*  Function
      REAL MDH_CTOR
*-
 
*  Local Variables
      REAL RY_START,RY_STEP, RX_START, RX_STEP, RX, RY
      INTEGER ICAT, ICOL, ISUBY
 
*  Ececutable Code

      READ(STRING,*) ICAT
 
      IF (ICAT.LE.10) THEN
 
         IF (ICAT.LE.4) THEN
 
            ICOL = 1
            ISUBY = 1
 
         ELSE IF (ICAT.LE.7) THEN
 
            ICOL = 2
            ISUBY = 5
 
         ELSE
 
            ICOL = 3
            ISUBY = 8

         END IF
 
         RY_START = MDH_CTOR(YSTART(IFIELD,1,IFILE))	! Initial value, page 1
         RY_STEP = MDH_CTOR(YSTART(IFIELD,2,IFILE))	! Step size under page 2
         RY = RY_START - RY_STEP * REAL(ICAT-ISUBY)
         WRITE(YS,'(F6.2)') RY
 
         RX_START = MDH_CTOR(XSTART(IFIELD,1,IFILE))	! Initial value, page 1
         RX_STEP = MDH_CTOR(XSTART(IFIELD,2,IFILE))	! Step size under page 2
         RX = RX_START + RX_STEP * REAL(ICOL-1)
         WRITE(XS,'(F6.2)') RX
         STATUS = 0
 
      ELSE
         STATUS = -1
      END IF
 
      END
