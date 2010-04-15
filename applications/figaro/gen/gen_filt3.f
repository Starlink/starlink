C+
      SUBROUTINE GEN_FILT3 (NX,NY,CVAL,EVAL,ROW,ARRAY)
C
C     G E N _ F I L T 3
C
C     Convolves a 2d array with a symmetrical 3x3 array - ie one
C     with a specified center value and a specified single value
C     used for the 8 edge elements.  So if C is the central value
C     and E the edge, the input array is convolved with the 3x3
C     array
C                        E  E  E
C                        E  C  E
C                        E  E  E
C
C     to give the output array.  This routine also works for a 1D
C     input array, which it will convolve with the 1D array  E C E.
C     The effect is that of a spatial 3 by 3 filter - hence the name.
C
C     Parameters -   (">" input, "W" workspace, "<" output)
C
C     (>) NX       (Integer) Number of columns in ARRAY
C     (>) NY       (Integer) Number of rows in ARRAY
C     (>) CVAL     (Real) The central value for the convolution array.
C     (>) EVAL     (Real) The edge value for the convolution array.
C     (W) ROW      (Real array ROW(NX,2)) Workspace.  Not used
C                  in the 1D case (NX or NY =1, see below).
C     (<) ARRAY    (Real array ARRAY(NX,NY)) The output data.
C
C     Subroutines / functions used - None
C
C     Note: 1) Edge effects.  This routine operates as though
C              there were extra pixels outside the actual array,
C              all containing zero.  So some of the data in the
C              edge pixels can be thought of as being transferred
C              into these non-existent pixels and so is effectively
C              lost.
C           2) This routine is based on code taken from the routine
C              GEN_ASMOTH.
C
C                                       KS / AAO 30th Oct 1987
C     Modifications:
C
C     16 Sep 1994  hme / UoE, Starlink. This routine would always
C                  be called with input and output array the same.
C                  However it is not permitted to use two dummy
C                  arguments for the same actual one. So now this
C                  routine always works in situ on ARRAY rather than
C                  from IN into OUT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY
      REAL ARRAY(NX,NY),ROW(NX*2),CVAL,EVAL
C
C     Local variables
C
      INTEGER IOFF,IOFFL,IX,IY
      REAL VCURR,VLAST,VNEXT
C
C     NOTE: this version treats ARRAY as 2D array.  More
C     efficient code would probably linearise them. (Actually,
C     this is a moot point, since the compiler generates good
C     code for the main loop as it is.) ROW is treated as a
C     1D array, however.
C
C     There are two special cases, where NX=1 and/or NY=1.  These
C     are treated separately.  First test for the general case.
C
      IF ((NX.GE.2).AND.(NY.GE.2)) THEN
C
C        Starting with the first row
C
         ROW(1)=ARRAY(1,1)*CVAL
     :         +(ARRAY(1,2)+ARRAY(2,1)+ARRAY(2,2))*EVAL
         DO IX=2,NX-1
            ROW(IX)=ARRAY(IX,1)*CVAL
     :             +(ARRAY(IX-1,1)+ARRAY(IX,2)+ARRAY(IX+1,1)
     :               +ARRAY(IX-1,2)+ARRAY(IX+1,2))*EVAL
         END DO
         ROW(NX)=ARRAY(NX,1)*CVAL
     :          +(ARRAY(NX-1,1)+ARRAY(NX,2)+ARRAY(NX-1,2))*EVAL
C
C        Then the intermediate rows.  Note that ROW is split into two
C        halves, each holding a row of the output, and used cyclically.
C
         IOFFL=0
         IOFF=NX
         DO IY=2,NY-1
            ROW(IOFF+1)=ARRAY(1,IY)*CVAL
     :                 +(ARRAY(1,IY-1)+ARRAY(1,IY+1)+ARRAY(2,IY)
     :                   +ARRAY(2,IY-1)+ARRAY(2,IY+1))*EVAL
            DO IX=2,NX-1
               ROW(IOFF+IX)=ARRAY(IX,IY)*CVAL
     :                     +(ARRAY(IX-1,IY-1)+ARRAY(IX-1,IY+1)
     :                       +ARRAY(IX+1,IY-1)+ARRAY(IX+1,IY+1)
     :                       +ARRAY(IX-1,IY)+ARRAY(IX+1,IY)
     :                       +ARRAY(IX,IY+1)+ARRAY(IX,IY-1))*EVAL
            END DO
            ROW(IOFF+NX)=ARRAY(IX,IY)*CVAL
     :                  +(ARRAY(NX-1,IY)+ARRAY(NX,IY+1)+ARRAY(NX,IY-1)
     :                    +ARRAY(NX-1,IY-1)+ARRAY(NX-1,IY+1))*EVAL
C
C           Copy the previous half of ROW into ARRAY, and reset the ROW
C           pointers
C
            DO IX=1,NX
               ARRAY(IX,IY-1)=ROW(IOFFL+IX)
            END DO
            IOFFL=IOFF
            IF (IOFF.EQ.0) THEN
               IOFF=NX
            ELSE
               IOFF=0
            END IF
         END DO
C
C        And then the final row
C
         ROW(IOFF+1)=ARRAY(1,NY)*CVAL
     :              +(ARRAY(1,NY-1)+ARRAY(2,NY)+ARRAY(2,NY-1))*EVAL
         DO IX=2,NX-1
            ROW(IOFF+IX)=ARRAY(IX,NY)*CVAL
     :                  +(ARRAY(IX-1,NY)+ARRAY(IX+1,NY)+ARRAY(IX,NY-1)
     :                    +ARRAY(IX-1,NY-1)+ARRAY(IX+1,NY-1))*EVAL
         END DO
         ROW(IOFF+NX)=ARRAY(NX,NY)*CVAL
     :               +(ARRAY(NX-1,NY)+ARRAY(NX,NY-1)
     :                 +ARRAY(NX-1,NY-1))*EVAL
C
C        Copy the final contents of ROW into ARRAY
C
         DO IX=1,NX
            ARRAY(IX,NY-1)=ROW(IOFFL+IX)
            ARRAY(IX,NY)=ROW(IOFF+IX)
         END DO
C
C        End of the general case.  Now the special cases.
C
      ELSE IF ((NX.GT.1).AND.(NY.EQ.1)) THEN
C
C        Special case 1:  NX>1, NY=1
C
         VCURR=ARRAY(1,1)
         VLAST=0.
         DO IX=1,NX-1
            VNEXT=ARRAY(IX+1,1)
            ARRAY(IX,1)=VCURR*CVAL+(VNEXT+VLAST)*EVAL
            VLAST=VCURR
            VCURR=VNEXT
         END DO
         ARRAY(NX,1)=VCURR*CVAL+VLAST*EVAL
C
      ELSE IF ((NX.EQ.1).AND.(NY.GT.1)) THEN
C
C        Special case 2:  NX=1, NY>1  (Note that if ARRAY and
C        ARRAY were 1-dimensional arrays, the 2 special cases
C        would be the same.)
C
         VCURR=ARRAY(1,1)
         VLAST=0.
         DO IY=1,NY-1
            VNEXT=ARRAY(1,IY+1)
            ARRAY(1,IY)=VCURR*CVAL+(VNEXT+VLAST)*EVAL
            VLAST=VCURR
            VCURR=VNEXT
         END DO
         ARRAY(1,NY)=VCURR*CVAL+VLAST*EVAL
      END IF
C
      END
