C+
      SUBROUTINE GEN_YOP21(OP,VEXIST,NX,NY,XYARRAY,XYVAR,
     :                     YARRAY,YVAR,RESULT,RVAR)
C
C     G E N _ Y O P 2 1
C
C     This routine performs a number of operations on a 2D array
C     and a 1D array the same length as the second dimension of the
C     2D array, the result always being a 2D array.  (The 'Y' refers
C     to the second dimension of the array, if one thinks of it as
C     having X and Y dimensions.)  The operation (add,divide,multiply
C     or divide) is performed on each cross-section through the
C     array in the Y-direction.  For the case of division, if the
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
C     (>) VEXIST  (Logical) TRUE if their are variances in both arrays
C     (>) NX      (Integer) The X, or first, dimension of the 2D array.
C     (>) NY      (Integer) The Y, or second, dimension of the array
C     (>) XYARRAY (Real array XYARRAY(NX,NY)) The 2D array
C     (>) XYVAR   (Real array XYARRAY(NX,NY)) The 2D variances
C     (>) YARRAY  (Real array YARRAY(NY)) The 1D array
C     (>) YVAR    (Real array YARRAY(NY)) The 1D variances
C     (<) RESULT  (Real array RESULT(NX,NY)) The 2D result.  Note that
C                 RESULT and XYARRAY may be the same array.
C     (<) RVAR    (Real array RESULT(NX,NY)) The 2D resulting variances.
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
      REAL XYARRAY(NX*NY),YARRAY(NY),RESULT(NX*NY)
      REAL XYVAR(NX*NY),YVAR(NY),RVAR(NX*NY)
      CHARACTER*(*) OP
C
C     Local variables
C
      INTEGER IPTR,VPTR,IX,IY
      REAL VALUE
C
C     Initialise pointer to 2D arrays
C
      IPTR=1
C
      IF ((OP(1:1).EQ.'A').OR.(OP(1:1).EQ.'a')) THEN
C
C        Add
C
         DO IY=1,NY
            VALUE=YARRAY(IY)
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)+VALUE
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              VALUE=YVAR(IY)
              DO IX=1,NX
                 RVAR(VPTR)=XYARRAY(VPTR)+VALUE
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
            VALUE=YARRAY(IY)
            IF (VALUE.EQ.0.) THEN
               DO IX=1,NX
                  RESULT(IPTR)=0.
                  IPTR=IPTR+1
               END DO
               IF (VEXIST) THEN
                 DO IX=1,NX
                    RVAR(VPTR)=0.
                    VPTR=VPTR+1
                 END DO
               ENDIF
            ELSE
               VALUE=1./VALUE
               DO IX=1,NX
                  RESULT(IPTR)=XYARRAY(IPTR)*VALUE
                  IPTR=IPTR+1
               END DO
               IF (VEXIST) THEN
                 VALUE=1./VALUE
                 DO IX=1,NX
                    RVAR(VPTR)=XYVAR(VPTR)/(XYARRAY(VPTR)**2.)
                    RVAR(VPTR)=RVAR(VPTR)+(YVAR(IY)/YARRAY(IY)**2.)
                    VPTR=VPTR+1
                 END DO
               ENDIF
            END IF
         END DO
C
      ELSE IF ((OP(1:1).EQ.'M').OR.(OP(1:1).EQ.'m')) THEN
C
C        Multiply
C
         DO IY=1,NY
            VALUE=YARRAY(IY)
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)*VALUE
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
           DO IY=1,NY
              VALUE=YARRAY(IY)
              DO IX=1,NX
                 RVAR(VPTR)=XYVAR(VPTR)*(VALUE**2.)
                 RVAR(VPTR)=RVAR(VPTR)+(YVAR(IY)*(XYARRAY(VPTR)**2.))
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
            VALUE=YARRAY(IY)
            DO IX=1,NX
               RESULT(IPTR)=XYARRAY(IPTR)-VALUE
               IPTR=IPTR+1
            END DO
         END DO
         IF (VEXIST) THEN
         DO IY=1,NY
            VALUE=YARRAY(IY)
            DO IX=1,NX
               RVAR(VPTR)=XYVAR(VPTR) + YVAR(IY)
               VPTR=VPTR+1
            END DO
         END DO
         ENDIF
C
      END IF
C
      END
