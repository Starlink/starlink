C+
      SUBROUTINE GEN_ASMOTH (NX,NY,FRAC,ROW,ARRAY)
C
C     G E N _ A S M O T H
C
C     Smooths the data in a 2D array, using an extension of the
C     1D 3-point smoothing algorithm.
C
C     In a standard 1D 3-point smooth, each pixel is replaced by
C     1/2 it's contents + 1/4 of the contents of each of the two
C     adjacent pixels.  In this more general 2D case, the contents
C     of a given pixel are distributed as follows: a certain fraction
C     stays in the pixel, and the remainder is distributed among the
C     8 adjacent pixels in such a way that the side pixels receive
C     1.414 times the value of the corner pixels (ie an amount
C     inversely proportional to their distance from the center pixel).
C
C     Parameters -   (">" input, "W" workspace, "<" output)
C
C     (>)  NX       (Integer) Number of columns in ARRAY
C     (>)  NY       (Integer) Number of rows in ARRAY
C     (>)  FRAC     (Real) Fraction of the contents of each pixel
C                   to be distributed amongst its neighbours.
C     (W)  ROW      (Real array ROW(NX,2)) Workspace.  Not used
C                   in the 1D case (NX or NY =1, see below).
C     (><) ARRAY    (Real array ARRAY(NX,NY)) The output data.
C
C     Subroutines / functions used - None
C
C     Note: 1) Edge effects.  This routine operates as though
C              there were extra pixels outside the actual array,
C              all containing zero.  So some of the data in the
C              edge pixels can be thought of as being transferred
C              into these non-existent pixels and so is effectively
C              lost.
C           2) Special cases: The case where the data to be smoothed
C              is actually 1 dimensional (ie either NX=1 or NY=1) is
C              treated as a special case, exactly as a 1D 3-point
C              smooth.
C
C                                       KS / CIT 21st June 1983
C
C     Modifications:
C
C     16 Sep 1994  hme / UoE, Starlink. Quite often this routine would
C                  be called with input and output array the same. With
C                  the change to ISMOOTH this is always the case.
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
      REAL ARRAY(NX,NY),ROW(NX*2),FRAC
C
C     Local variables
C
      INTEGER IOFF,IOFFL,IX,IY
      REAL FC,FREM,FS,VCURR,VLAST,VNEXT
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
C        Constants giving the fractions taken from different pixels
C        in ARRAY and put into ARRAY:  FREM is the amount taken from the
C        corresponding pixel, FC is that taken from a side pixel, FC
C        is that taken from a corner pixel.
C
         FREM=1.0-FRAC
         FC=FRAC/(4+4*1.414)
         FS=FC*1.414
C
C        Starting with the first row
C
         ROW(1)=ARRAY(1,1)*FREM+(ARRAY(1,2)+ARRAY(2,1))*FS
     :         +ARRAY(2,2)*FC
         DO IX=2,NX-1
            ROW(IX)=ARRAY(IX,1)*FREM
     :             +(ARRAY(IX-1,1)+ARRAY(IX,2)+ARRAY(IX+1,1))*FS
     :             +(ARRAY(IX-1,2)+ARRAY(IX+1,2))*FC
         END DO
         ROW(NX)=ARRAY(NX,1)*FREM
     :          +(ARRAY(NX-1,1)+ARRAY(NX,2))*FS
     :          +ARRAY(NX-1,2)*FC
C
C        Then the intermediate rows.  Note that ROW is split into two
C        halves, each holding a row of the output, and used cyclically.
C
         IOFFL=0
         IOFF=NX
         DO IY=2,NY-1
            ROW(IOFF+1)=ARRAY(1,IY)*FREM
     :                 +(ARRAY(1,IY-1)+ARRAY(1,IY+1)+ARRAY(2,IY))*FS
     :                 +(ARRAY(2,IY-1)+ARRAY(2,IY+1))*FC
            DO IX=2,NX-1
               ROW(IOFF+IX)=ARRAY(IX,IY)*FREM
     :                     +(ARRAY(IX-1,IY-1)+ARRAY(IX-1,IY+1)
     :                       +ARRAY(IX+1,IY-1)+ARRAY(IX+1,IY+1))*FC
     :                     +(ARRAY(IX-1,IY)+ARRAY(IX+1,IY)
     :                       +ARRAY(IX,IY+1)+ARRAY(IX,IY-1))*FS
            END DO
            ROW(IOFF+NX)=ARRAY(IX,IY)*FREM
     :                  +(ARRAY(NX-1,IY)+ARRAY(NX,IY+1)
     :                    +ARRAY(NX,IY-1))*FS
     :                  +(ARRAY(NX-1,IY-1)+ARRAY(NX-1,IY+1))*FC
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
         ROW(IOFF+1)=ARRAY(1,NY)*FREM
     :               +(ARRAY(1,NY-1)+ARRAY(2,NY))*FS
     :               +ARRAY(2,NY-1)*FC
         DO IX=2,NX-1
            ROW(IOFF+IX)=ARRAY(IX,NY)*FREM
     :                  +(ARRAY(IX-1,NY)+ARRAY(IX+1,NY)
     :                    +ARRAY(IX,NY-1))*FS
     :                  +(ARRAY(IX-1,NY-1)+ARRAY(IX+1,NY-1))*FC
         END DO
         ROW(IOFF+NX)=ARRAY(NX,NY)*FREM
     :               +(ARRAY(NX-1,NY)+ARRAY(NX,NY-1))*FS
     :               +ARRAY(NX-1,NY-1)*FC
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
         FREM=1.0-FRAC
         FC=FRAC*.5
         VCURR=ARRAY(1,1)
         VLAST=0.
         DO IX=1,NX-1
            VNEXT=ARRAY(IX+1,1)
            ARRAY(IX,1)=VCURR*FREM+(VNEXT+VLAST)*FC
            VLAST=VCURR
            VCURR=VNEXT
         END DO
         ARRAY(NX,1)=VCURR*FREM+VLAST*FC
C
      ELSE IF ((NX.EQ.1).AND.(NY.GT.1)) THEN
C
C        Special case 2:  NX=1, NY>1  (Note that if ARRAY and
C        ARRAY were 1-dimensional arrays, the 2 special cases
C        would be the same.)
C
         FREM=1.0-FRAC
         FC=FRAC*.5
         VCURR=ARRAY(1,1)
         VLAST=0.
         DO IY=1,NY-1
            VNEXT=ARRAY(1,IY+1)
            ARRAY(1,IY)=VCURR*FREM+(VNEXT+VLAST)*FC
            VLAST=VCURR
            VCURR=VNEXT
         END DO
         ARRAY(1,NY)=VCURR*FREM+VLAST*FC
      END IF
C
      END
