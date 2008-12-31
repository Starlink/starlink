*+LTX_TYPE     Finds where to put the Type X
*  Dec 1992	M. Duesterhaus		Create original
*****************************************************************      
      SUBROUTINE LTX_TYPE(IFIELD,IFILE,STRING,XS,YS,STATUS)
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
      REAL RY_START,RY_STEP, RX, RY
      INTEGER ICAT, ICOL, ISUBY
 
*  Ececutable Code

      READ(STRING,*) ICAT

      IF (ICAT.EQ.0) THEN
         ICAT = 3
      ELSE IF (ICAT.EQ.4) THEN
         ICAT = 1
      ELSE IF (ICAT.EQ.7) THEN
         ICAT = 4
      END IF 
      IF (ICAT.LE.4) THEN

         ICOL = 1
         ISUBY = 1
 
         RY_START = MDH_CTOR(YSTART(IFIELD,1,IFILE))	! Initial value, page 1
         RY_STEP = MDH_CTOR(YSTART(IFIELD,2,IFILE))	! Step size under page 2
         RY = RY_START - RY_STEP * REAL(ICAT-ISUBY)
         WRITE(YS,'(F6.2)') RY
 
         RX = MDH_CTOR(XSTART(IFIELD,1,IFILE))	! Initial value, page 1
C         RX_STEP = MDH_CTOR(XSTART(IFIELD,2,IFILE))	! Step size under page 2
C         RX = RX_START + RX_STEP * REAL(ICOL-1)
         WRITE(XS,'(F6.2)') RX
         STATUS = 0
 
      ELSE
         STATUS = -1
      END IF
 
      END
