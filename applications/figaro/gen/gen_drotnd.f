C+
      SUBROUTINE GEN_DROTND (NELMS,NDIM,DIMS,ROTS,DATA,OUTPUT)
C
C     G E N _ D R O T N D
C
C     Rotates the data in an n-dimensional array by a specified integer
C     number of pixels in each axis.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (>) NELMS    (Integer) The total number of elements in the array.
C     (>) NDIM     (Integer) The number of dimensions for DATA.  (Must
C                  be no more than 10.)
C     (>) DIMS     (Integer array DIMS(NDIM)) The dimensions of DATA.
C     (>) ROTS     (Integer array ROTS(NDIM)) The number of pixels by which
C                  each axis is to be rotated.  A positive number indicates
C                  a shift towards higher numbered elements.
C     (>) DATA     (Double precision array DATA(NELMS)) The array to be
C                  rotated. Its real dimensions are given by DIMS; it
C                  it treated as linear here for generality.
C     (<) OUTPUT   (Double precision array OUTPUT(NELMS)) The resulting
C                  rotated array.  Dimensions are as for DATA.  Note that
C                  DATA and OUTPUT cannot be the same arrays.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_MOVE     Fast transfer of bytes between two arrays
C
C                                             KS / AAO 8th Oct 1986
C
C     Note: In all honesty, it has to be said that this routine has
C     only been properly tested in the NDIM=1 and NDIM=2 cases.
C
C     Modified:
C
C     30th Jun 1993.  KS/AAO Unused variables removed.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELMS, NDIM, DIMS(NDIM), ROTS(NDIM)
      DOUBLE PRECISION DATA(NELMS), OUTPUT(NELMS)
C
C     Local variables
C
      LOGICAL MORE, REPEAT
      INTEGER IDIM, IDIMS(10), IFROM, ITARGT
      INTEGER ITO, NELM
C
C     Initial setup
C
      DO IDIM=1,NDIM
         IDIMS(IDIM)=1
      END DO
      MORE=.TRUE.
      DO WHILE (MORE)
C
C        At this point IDIMS indicate an element of the array.  It
C        is an element at the start of one of the 1D strips of the array
C        ie IDIMS(1)=1.  We want to know a) the element number of DATA
C        (treated as a 1D array) that this corresponds to (IFROM), and
C        we also want the element number of the start of the 1D strip
C        that it becomes in the output array (ITO).
C
         IFROM=1
         NELM=DIMS(1)
         DO IDIM=2,NDIM
            IFROM=IFROM+(IDIMS(IDIM)-1)*NELM
            NELM=NELM*DIMS(IDIM)
         END DO
         ITO=1
         NELM=DIMS(1)
         DO IDIM=2,NDIM
            ITARGT=ROTS(IDIM)+IDIMS(IDIM)
            IF (ITARGT.GT.DIMS(IDIM)) ITARGT=ITARGT-DIMS(IDIM)
            IF (ITARGT.LE.0) ITARGT=DIMS(IDIM)+ITARGT
            ITO=ITO+(ITARGT-1)*NELM
            NELM=NELM*DIMS(IDIM)
         END DO
C
C        Now transfer this 1D strip from DATA to OUTPUT, rotating it
C        as we go.
C
         IF (ROTS(1).GE.0) THEN
            CALL GEN_MOVE((DIMS(1)-ROTS(1))*8,DATA(IFROM),
     :                                          OUTPUT(ROTS(1)+ITO))
            CALL GEN_MOVE(ROTS(1)*8,DATA(DIMS(1)-ROTS(1)+IFROM),
     :                                                  OUTPUT(ITO))
         ELSE IF (ROTS(1).LT.0) THEN
            CALL GEN_MOVE((DIMS(1)+ROTS(1))*8,DATA(IFROM-ROTS(1)),
     :                                                  OUTPUT(ITO))
            CALL GEN_MOVE(-ROTS(1)*8,DATA(IFROM),
     :                                  OUTPUT(DIMS(1)+ROTS(1)+ITO))
         END IF
C
C        Now increment IDIMS to point to the start of the next 1D strip
C
         IDIM=2
         REPEAT=IDIM.LE.NDIM
         MORE=REPEAT
         DO WHILE (REPEAT)
            IDIMS(IDIM)=IDIMS(IDIM)+1
            IF (IDIMS(IDIM).GT.DIMS(IDIM)) THEN
               IDIMS(IDIM)=1
               IDIM=IDIM+1
               REPEAT=IDIM.LE.NDIM
               MORE=REPEAT
            ELSE
               REPEAT=.FALSE.
            END IF
         END DO
      END DO
C
      END
