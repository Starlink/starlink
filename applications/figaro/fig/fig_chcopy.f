C+
      SUBROUTINE FIG_CHCOPY(NDIM,DIMS0,DIMS,NELM0,NELM,DATA0,DDATA)
C
C     F I G _ C H C O P Y
C
C     FFT utility.  Copies from a real input array into a double
C     precision array which has different dimensions.
C     This routine is not particularly efficient, since there is no
C     way to let the compiler know the dimensions of the arrays,
C     and so they have to be treated as linear.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NDIM     (Integer) The number of dimensions in the arrays.
C     (>) DIMS0    (Integer array DIMS0(NDIM)) The dimensions of the
C                  first array (DATA0).
C     (>) DIMS     (Integer array DIMS(NDIM)) The dimensions of the
C                  second array (DDATA).
C     (>) NELM0    (Integer) The number of elements in DATA0.
C     (>) NELM     (Integer) The number of elements in DDATA.
C     (>) DATA0    (Real array DATA0(NELM0)) The input array.
C     (<) DDATA    (Double precision array DDATA(NELM)) The output array.
C
C                                                 KS / AAO 2nd Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM,DIMS0(NDIM),DIMS(NDIM),NELM0,NELM
      REAL DATA0(NELM0)
      DOUBLE PRECISION DDATA(NELM)
C
C     Local variables
C
      LOGICAL CARRY, REPEAT
      INTEGER COUNT, IDIM, IDIMS(10), IPTR, IPTR0, NM
C
C     Start by filling the output array with zeros.
C
      CALL GEN_FILL(NELM*8,0,DDATA)
C
C     The technique used is to start with element [1,1,1...,1] in the
C     input array.  (These element numbers are in IDIMS).  Work out the
C     starting element in the input array and the corresponding element
C     in the output array.  Then copy the data for the first dimension
C     (ie [1,1,1..,1] through [n,1,1...,1]) into the output array.
C     Then increment the second index and repeat.  Continue, each time
C     incrementing the second index and carrying over to higher indices
C     as necessary, until all the data is copied.
C
      DO IDIM=1,NDIM
         IDIMS(IDIM)=1
      END DO
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
C
C        IPTR0 is the element in the linear array DATA0 corresponding
C        to the element defined by IDIMS.
C
         NM=1
         IPTR0=1
         DO IDIM=2,NDIM
            NM=NM*DIMS0(IDIM-1)
            IPTR0=IPTR0+NM*(IDIMS(IDIM)-1)
         END DO
C
C        Similarly for IPTR and DDATA.
C
         NM=1
         IPTR=1
         DO IDIM=2,NDIM
            NM=NM*DIMS(IDIM-1)
            IPTR=IPTR+NM*(IDIMS(IDIM)-1)
         END DO
C
C        Copy the 1D strip of data starting at these elements.
C
         DO COUNT=1,MIN(DIMS0(1),DIMS(1))
            DDATA(IPTR)=DATA0(IPTR0)
            IPTR0=IPTR0+1
            IPTR=IPTR+1
         END DO
C
C        Increment the IDIMS array.
C
         IDIM=2
         CARRY=.TRUE.
         DO WHILE (CARRY)
            IDIMS(IDIM)=IDIMS(IDIM)+1
            IF (IDIMS(IDIM).GT.(MIN(DIMS0(IDIM),DIMS(IDIM)))) THEN
               IDIMS(IDIM)=1
               IDIM=IDIM+1
               IF (IDIM.GT.NDIM) THEN
                  CARRY=.FALSE.
                  REPEAT=.FALSE.
               END IF
            ELSE
               CARRY=.FALSE.
            END IF
         END DO
      END DO
C
      END
