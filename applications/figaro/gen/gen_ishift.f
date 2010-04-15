C+
      SUBROUTINE GEN_ISHIFT (INPUT,NX,NY,DX,DY,OUTPUT)
C
C     G E N _ I S H I F T
C
C     Moves the elements in a 2D array by an integer number of
C     elements in both dimensions.  The array is treated here as
C     integer, but this routine will work on any array with 4 byte
C     elements, so a real array can be handled as well.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) INPUT   (Integer array INPUT(NX,NY)) The input array.
C     (>) NX      (Integer) The first (X) dimension of INPUT.
C     (>) NY      (Integer) The second (Y) dimension of INPUT.
C     (>) DX      (Integer) The number of elements the data is to
C                 be shifted in the X direction.  A positive
C                 DX indicates a shift to higher numbered elements.
C     (>) DY      (Integer) The number of elements the data is to be
C                 shifted in the Y direction.
C     (<) OUTPUT  (Integer array OUTPUT(NX,NY)) The output array.
C                 Elements of OUTPUT that do not map onto elements
C                 of INPUT are set to zero.
C
C     Note: INPUT and OUTPUT may be the same array, but if they
C     share storage at all, they should be exactly the same array -
C     'clever' overlapping will produce strange results.
C
C     This routine should behave properly even in crazy cases such
C     as ABS(DY)>NY etc.
C
C     Subroutines / functions used -
C
C     GEN_MOVE   (GEN_ package) Fast byte move from memory to memory
C     GEN_FILL   ( "     "    ) Fast memory fill with a byte value
C
C                                     KS / CIT 11th Sept 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,DX,DY
      INTEGER INPUT(NX,NY),OUTPUT(NX,NY)
C
C     Local variables
C
      INTEGER FBYTES,FXSTO,FYSTO,IXSTI,IXSTO,IY,NBYTES
C
C     Number of elements that can be moved for each row depends
C     on the value of DX, as does the first element in the
C     input and output arrays for the moved data, and the number
C     of bytes to be filled with zeros.
C
      IF (DX.LT.NX) THEN
         NBYTES=(NX-ABS(DX))*4
      ELSE
         NBYTES=0
      END IF
      FBYTES=MIN(ABS(DX),NX)*4
      IF (DX.GE.0) THEN
         IXSTO=DX+1
         IXSTI=1
         FXSTO=1
      ELSE
         IXSTO=1
         IXSTI=1-DX
         FXSTO=NX+DX+1
      END IF
C
C     Operation goes in different directions for DY +ve and -ve
C
      IF (DY.GE.0) THEN
C
C        +VE DY.  Data moves up, so go back from highest numbered
C        rows of the array.
C
         DO IY=NY,DY+1,-1
C
C           Move the data in the row
C
            CALL GEN_MOVE(NBYTES,INPUT(IXSTI,IY-DY),OUTPUT(IXSTO,IY))
C
C           Zero fill the un-mapped elements
C
            CALL GEN_FILL(FBYTES,0,OUTPUT(FXSTO,IY))
C
         END DO
C
C        Zero fill the un-mapped rows
C
         CALL GEN_FILL(MIN(DY,NY)*NX*4,0,OUTPUT)
C
      ELSE
C
C        -ve DY, data moves down, so start moving data into first
C        row and then into successive rows.
C
         DO IY=1,NY+DY
C
C           Move the data in the row
C
            CALL GEN_MOVE(NBYTES,INPUT(IXSTI,IY-DY),OUTPUT(IXSTO,IY))
C
C           Zero fill the un-mapped elements
C
            CALL GEN_FILL(FBYTES,0,OUTPUT(FXSTO,IY))
C
         END DO
C
C        Zero fill the un-mapped rows
C
         FYSTO=MAX(1,NY+DY+1)
         CALL GEN_FILL(MIN(-DY,NY)*NX*4,0,OUTPUT(1,FYSTO))
C
      END IF
C
      END
