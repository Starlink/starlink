


      SUBROUTINE GK0QXL(X1,Y1,X2,LVIS)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT  Original Versatec driver
*                    AJC  Modifications for Printronix P300
*                    PTW  Short line bug fixed
*                    PLP  Modifications for PRIME
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Draws a line parallel to the X axis
*
*  ARGUMENTS
*  ---------
*     INP X1,Y1  - Starting point of line
*     INP X2     - end point of line
*     INP LVIS   - Whether line is invisible
*
      REAL X1,X2,Y1
      INTEGER LVIS
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC NINT,AMAX1,AMIN1,MAX0,MIN0
*
*     External functions declaration
*
      CHARACTER GKAN1
      INTEGER   GK0QLC
*
*  LOCALS
*  ------
*
      INTEGER IY, IX1, IX2, IX3, IX4, I
      INTEGER IVAL
*
*
*--------------------------------------------------------------------

*     The line is plotted in 3 pieces, from the start to first byte
*     boundary, from the first to last byte boundary and from the last
*     byte boundary to the end

      IY = NINT(Y1)

      IX1 = NINT(AMIN1(X1,X2))
      IX4 = NINT(AMAX1(X1,X2))
      IX2 = ((IX1+5)/6) * 6
      IX3 = (IX4/6) * 6 - 1

      DO 100 I = IX1,MIN0(IX2-1,IX4)
         IF (LVIS.EQ.0) THEN
            CALL GK0QCB(I,IY)
         ELSE
            CALL GK0QSB(I,IY)
         END IF
  100 CONTINUE

*     The middle section is plotted setting six bits at a time
      IF (IX4-IX1.GE.6) THEN
         IF (LVIS.EQ.0) THEN
            IVAL = 64
         ELSE
            IVAL = 127
         END IF
         DO 200 I = IX2,IX3,6
            CHP(GK0QLC(I,IY))=GKAN1(IVAL)
  200    CONTINUE
      END IF

      DO 300 I = MAX0(IX3+1,IX1),IX4
         IF (LVIS.EQ.0) THEN
            CALL GK0QCB(I,IY)
         ELSE
            CALL GK0QSB(I,IY)
         END IF
  300 CONTINUE

      END
