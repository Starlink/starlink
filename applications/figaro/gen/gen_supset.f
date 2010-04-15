C+
      SUBROUTINE GEN_SUPSET (IN,NX,NY,NXOUT,NYOUT,IXST,IYST,OUT)
C
C     G E N _ S U P S E T
C
C     Given two two-dimensional arrays that overlap (in a nice simple
C     way, with one element in one mapping directly onto one element
C     in the other), copies the data from the overlapping portion of
C     the input array into the corresponding elements of the output
C     array.  All other elements of the output array are set to zero.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN      (Real array IN (NX,NY)) Input array
C     (>) NX      (Integer) First dimension of IN
C     (>) NY      (Integer) Second dimension of IN
C     (>) NXOUT   (Integer) First dimension of OUT
C     (>) NYOUT   (Integer) Second dimension of OUT
C     (>) IXST    (Integer) X (first dimension) element number in OUT
C                 on which element (1,1) of IN is mapped.
C     (>) IYST    (integer) Y (second dimension) element number in
C                 OUT on which element (1,1) of IN is mapped.  If the
C                 overlap is such that the first element of IN does
C                 not map to any element of OUT, the values of IXST
C                 and IYST can be set to 0 or -ve values.
C     (<) OUT     (Real array OUT(NXOUT,NYOUT)) Output array.
C
C     Common variables used -   None
C
C     Subroutines / functions used -
C
C     GEN_FILL    (GEN_ package) Set array of bytes to a given value
C     GEN_MOVE    ( "      "   ) Copy one byte array to another
C
C                                          KS / CIT 9th August 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NXOUT, NYOUT, IXST, IYST
      REAL    IN(NX,NY), OUT(NXOUT,NYOUT)
C
C     Local variables
C
      INTEGER BYTES, CBYTES, IX1, IX2, IXIN, IY, IY1, IY2, IYIN
      INTEGER LBYTES, RBYTES
C
C     Check that there really is an overlap.  If not, set all
C     of OUT to zero.
C
      IF (((IYST.LE.0).AND.((NY+IYST-1).LE.0)).OR.
     :    ((IXST.LE.0).AND.((NX+IXST-1).LE.0)).OR.
     :    (IYST.GT.NYOUT).OR.(IXST.GT.NXOUT)) THEN
          BYTES=NXOUT*NYOUT*4
          CALL GEN_FILL(BYTES,0,OUT)
          GO TO 600
      END IF
C
C     If any of the lower Y values in OUT don't map onto
C     the input array, set those lines to zero in one go.
C     Also start to calculate the limit variables.  Note
C     IY1 and IY2 are respectively first and last Y element
C     numbers in OUT to which data is copied from IN.  IYIN
C     is the line in IN corresponding to line IY1 in OUT.
C     (A 'line' is a cross-section of constant Y value.)
C
      IF (IYST.GT.0) THEN
         IY1=IYST
         IYIN=1
         BYTES=(IYST-1)*NXOUT*4
         CALL GEN_FILL(BYTES,0,OUT)
      ELSE
         IY1=1
         IYIN=2-IYST
      END IF
      IY2=MIN(NYOUT,NY+IYST-1)
C
C     Calculate similar variables for X.  Also, LBYTES, CBYTES
C     and RBYTES are the number of bytes in each row of OUT to be
C     set to zero, copied from IN, and set to zero.
C
      IF (IXST.GT.0) THEN
         IX1=IXST
         LBYTES=(IXST-1)*4
         IXIN=1
      ELSE
         IX1=1
         IXIN=2-IXST
         LBYTES=0
      END IF
      IX2=MIN(NXOUT,NX+IXST-1)
      RBYTES=(NXOUT-IX2)*4
      CBYTES=(IX2-IX1+1)*4
C
C     Loop through all the lines in the overlap section, setting
C     the appropriate sections to zero or copying them from IN.
C
      DO IY=IY1,IY2
         IF (LBYTES.GT.0) CALL GEN_FILL(LBYTES,0,OUT(1,IY))
         CALL GEN_MOVE(CBYTES,IN(IXIN,IYIN),OUT(IX1,IY))
         IF (RBYTES.GT.0) CALL GEN_FILL(RBYTES,0,OUT(IX2+1,IY))
         IYIN=IYIN+1
      END DO
C
C     If any lines are left in OUT, zero them in one go.
C
      IF (IY2.LT.NYOUT) THEN
         BYTES=(NYOUT-IY2)*NXOUT*4
         CALL GEN_FILL(BYTES,0,OUT(1,IY2+1))
      END IF
C
  600 CONTINUE
C
      END
