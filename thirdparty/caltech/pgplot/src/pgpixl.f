C*PGPIXL -- draw pixels
C%void cpgpixl(const int *ia, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float x1, float x2, float y1, float y2);
C+
      SUBROUTINE PGPIXL (IA, IDIM, JDIM, I1, I2, J1, J2, 
     1                   X1, X2, Y1, Y2)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL    X1, X2, Y1, Y2
C
C Draw lots of solid-filled (tiny) rectangles aligned with the
C coordinate axes. Best performance is achieved when output is
C directed to a pixel-oriented device and the rectangles coincide
C with the pixels on the device. In other cases, pixel output is
C emulated.
C
C The subsection of the array IA defined by indices (I1:I2, J1:J2)
C is mapped onto world-coordinate rectangle defined by X1, X2, Y1
C and Y2. This rectangle is divided into (I2 - I1 + 1) * (J2 - J1 + 1)
C small rectangles. Each of these small rectangles is solid-filled
C with the color index specified by the corresponding element of 
C IA.
C
C On most devices, the output region is "opaque", i.e., it obscures
C all graphical elements previously drawn in the region. But on
C devices that do not have erase capability, the background shade
C is "transparent" and allows previously-drawn graphics to show
C through.
C
C Arguments:
C  IA     (input)  : the array to be plotted.
C  IDIM   (input)  : the first dimension of array A.
C  JDIM   (input)  : the second dimension of array A.
C  I1, I2 (input)  : the inclusive range of the first index
C                    (I) to be plotted.
C  J1, J2 (input)  : the inclusive range of the second
C                    index (J) to be plotted.
C  X1, Y1 (input)  : world coordinates of one corner of the output
C                    region
C  X2, Y2 (input)  : world coordinates of the opposite corner of the
C                    output region
C--
C 16-Jan-1991 - [GvG]
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
C Check inputs.
C
      IF (PGNOTO('PGPIXL')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GT.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GT.J2) THEN
         CALL GRWARN('PGPIXL: invalid range I1:I2, J1:J2')
      ELSE
C
C Call lower-level routine to do the work.
C
         CALL PGBBUF
         CALL GRPIXL(IA, IDIM, JDIM, I1, I2, J1, J2, X1, X2, Y1, Y2)
         CALL PGEBUF
      END IF
C-----------------------------------------------------------------------
      END
