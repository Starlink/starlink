C+
      SUBROUTINE GEN_ICONV (ARRAY,NX,NY,BOX,NBX,NBY,OUTPUT)
C
C     G E N _ I C O N V
C
C     This is a very general convolution routine that convolves an image
C     with a rectangular box. The box can be any size (although the routine
C     will become very inefficient if the box is large, particularly in the
C     Y-direction on a virtual memory machine, since it is a literal
C     convolution that is performed (nothing clever, no fourier transforms,
C     just a straight convolution of the two data arrays).
C
C     Parameters: (">" input, "<" output)
C
C     (>) ARRAY   (Real array, ARRAY(NX,NY)) The input array.
C     (>) NX      (Integer) The Y-dimension of the array.
C     (>) NY      (Integer) The Y-dimension of the array.
C     (>) BOX     (Real array BO(NBX,NBY)) The convolution array.
C     (>) NBX     (Integer) The Y-dimension of the convolution array.
C     (>) NBY     (Integer) The Y-dimension of the convolution array.
C     (>) OUTPUT  (Real array OUTPUT(NX,NY)) The resulting output array.
C
C     Support: Keith Shortridge, AAO
C
C     Version date: 11th March 1994.
C-
C     History:
C        11th Mar 1994.  Original version. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,NBX,NBY
      REAL ARRAY(NX,NY),BOX(NBX,NBY),OUTPUT(NX,NY)
C
C     Local variables
C
      INTEGER IX,IY,IBXST,IBYST,IBXEN,IBYEN,IDBYST,IDBYEN
      INTEGER IDBXST,IDBXEN,IBX,IBY,NBX2,NBY2,IDBY,IDBX
      REAL TOTAL
C
C     There are a number of opportunities for optimisation of this code,
C     particularly spotting the cases where NBX or NBY are 1, and for
C     being clever about doing the edges separately.  For the moment, however
C     this heavy-handed code will do.
C
      NBX2=NBX/2
      NBY2=NBY/2
      DO IY=1,NY
         IBYST=1
         IBYEN=NBY
         IDBYST=IY-NBY2
         IDBYEN=IY+NBY2
         IF (IDBYST.LT.1) THEN
            IBYST=1+(1-IDBYST)
            IDBYST=1
         END IF
         IF (IDBYEN.GT.NY) THEN
            IBYEN=NBY-(IDBYEN-NY)
            IDBYEN=NY
         END IF
         DO IX=1,NX
            IBXST=1
            IBXEN=NBX
            IDBXST=IX-NBX2
            IDBXEN=IX+NBX2
            IF (IDBXST.LT.1) THEN
               IBXST=1+(1-IDBXST)
               IDBXST=1
            END IF
            IF (IDBXEN.GT.NX) THEN
               IBXEN=NBX-(IDBXEN-NX)
               IDBXEN=NX
            END IF
C
C           This is where we work out the convolved data value for the
C           current pixel (IX,IY). This is obtained by imagining the
C           convolution box centered on this pixel. Normally, all the box
C           will be used, since it will all lie within the bounds of ARRAY.
C           Box pixels from IBXST to IBXEN in X and between IBYST and IBYEN
C           in Y will be used. Normally, these will be 1,NBX,1,NBY respectively.
C           The box pixel (IBXST,IBYST) corresponds (lies on top of) the
C           data array pixel (IDBXST,IDBYST).
C
            TOTAL=0.0
            IDBY=IDBYST
            DO IBY=IBYST,IBYEN
               IDBX=IDBXST
               DO IBX=IBXST,IBXEN
                  TOTAL=TOTAL+ARRAY(IDBX,IDBY)*BOX(IBX,IBY)
                  IDBX=IDBX+1
               END DO
               IDBY=IDBY+1
            END DO
            OUTPUT(IX,IY)=TOTAL
         END DO
      END DO
C
      END



