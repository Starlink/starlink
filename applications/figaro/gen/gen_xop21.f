C+
      SUBROUTINE GEN_XOP21(OP,VEXIST,NX,NY,XYARRAY,XYVAR,
     :                     XARRAY,XVAR,RESULT,RVAR)
C
C     G E N _ X O P 2 1
C
C     This routine performs a number of operations on a 2D array
C     and a 1D array the same length as the first dimension of the
C     2D array, the result always being a 2D array.  (The 'X' refers
C     to the first dimension of the array, if one thinks of it as
C     having X and Y dimensions.)  The operation (add,divide,multiply
C     or divide) is performed on each cross-section through the
C     array in the X-direction.  For the case of division, if the
C     spectrum has any zero elements the corresponding result elements
C     are set to zero.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) OP      (Character) Specifies the operation to be preformed.
C                 Only the first character is used, and is case-
C                 independent.
C                 'A' => add      (Result=image+spect)
C                 'D' => divide   (Result=image/spect)
C                 'M' => multiply (Result=image*spect)
C                 'S' => subtract (Result=image-spect)
C                 None of these => No operation is performed
C     (>) VEXIST  (Logical) True if variances are to be worked on.
C     (>) NX      (Integer) The X, or first, dimension of the 2D array.
C     (>) NY      (Integer) The Y, or second, dimension of the array
C     (>) XYARRAY (Real array XYARRAY(NX,NY)) The 2D array
C     (>) XYVAR   (Real array XYVAR(NX,NY)) The 2D array variances
C     (>) XARRAY  (Real array XARRAY(NX)) The 1D array
C     (>) XVAR    (Real array XVAR(NX)) The 1D array variances
C     (<) RESULT  (Real array RESULT(NX,NY)) The 2D result.  Note that
C                 RESULT and XYARRAY may be the same array.
C     (<) RVAR    (Real array RVAR(NX,NY)) The 2D result variances.
C
C                                       KS / CIT  18th Feb 1983
C+
      IMPLICIT NONE
C
C     Parameters  (1D arrays are used for efficiency, rather
C     than 2D arrays, for XYARRAY and RESULT)
C
      INTEGER NX,NY
      LOGICAL VEXIST
      REAL XYARRAY(NX*NY),XARRAY(NX),RESULT(NX*NY)
      REAL XYVAR(NX*NY),XVAR(NX),RVAR(NX*NY)
      CHARACTER*(*) OP
C
C     Local variables
C
       INTEGER IPTR,VPTR,IX,IY
       REAL TEMP
C
C     Initialise pointer to 2D arrays
C
      IPTR=1
      VPTR=1
C
      IF ((OP(1:1).EQ.'A').OR.(OP(1:1).EQ.'a')) THEN
C
C        Add
C
         DO IY=1,NY
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)+XARRAY(IX)
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              DO IX=1,NX
                 RVAR(VPTR)=XYVAR(VPTR)+XVAR(IX)
                 VPTR=VPTR+1
              END DO
           END DO
         ENDIF
C
      ELSE IF ((OP(1:1).EQ.'D').OR.(OP(1:1).EQ.'d')) THEN
C
C        Divide
C
         DO IY=1,NY
            DO IX=1,NX
               IF (XARRAY(IX).EQ.0.) THEN
                  RESULT(IPTR)=0.
               ELSE
                  RESULT(IPTR)=XYARRAY(IPTR)/XARRAY(IX)
               END IF
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              DO IX=1,NX
                 IF (XARRAY(IX).EQ.0..OR.XYARRAY(VPTR).EQ.0.) THEN
                    RVAR(VPTR)=0.
                 ELSE
                    TEMP = XYVAR(VPTR)/(XYARRAY(VPTR)**2.)
                    TEMP = TEMP + (XVAR(IX)/(XARRAY(IX)**2.))
                    RVAR(VPTR)=TEMP
                 END IF
                 VPTR=VPTR+1
              END DO
           END DO
         ENDIF
C
      ELSE IF ((OP(1:1).EQ.'M').OR.(OP(1:1).EQ.'m')) THEN
C
C        Multiply
C
         DO IY=1,NY
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)*XARRAY(IX)
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              DO IX=1,NX
                 TEMP = XYVAR(VPTR)*(XARRAY(IX)**2.)
                 TEMP = TEMP + XVAR(IX)*(XYARRAY(VPTR)**2.)
                 RVAR(VPTR)= TEMP
                 VPTR=VPTR+1
              END DO
         END DO
         ENDIF
C
      ELSE IF ((OP(1:1).EQ.'S').OR.(OP(1:1).EQ.'s')) THEN
C
C        Subtract
C
         DO IY=1,NY
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)-XARRAY(IX)
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              DO IX=1,NX
                 RVAR(VPTR)=XYVAR(VPTR)+XVAR(IX)
                 VPTR=VPTR+1
              END DO
           END DO
         ENDIF
C
      END IF
C
      END
