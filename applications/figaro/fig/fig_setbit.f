C+
      SUBROUTINE FIG_SETBIT (BYT,BITNUM,BITVAL,STATUS)
C
C     F I G _ S E T B I T
C
C     Sets or unsets one bit of a byte value without affecting the others.
C     Note this uses bitwise manipulation routines which do not conform
C     to the FORTRAN 77 standard.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) BYT     (Byte) Byte to be modified
C     (>) BITNUM  (Integer) Number of the bit to be modified (0-7)
C     (>) BITVAL  (Logical) .TRUE. to set bit, .FALSE. to unset it
C     (!) STATUS  (Integer) The global status
C
C     Common variables used - None
C
C     Functions / subroutines used
C
C     IOR, IAND, NOT - bitwise manipulation routines.
C
C                                            MBT / IoA 21st Jul 1998
C+
      IMPLICIT NONE
C
C     Parameters
C
      BYTE BYT
      INTEGER BITNUM,STATUS
      LOGICAL BITVAL
C
C     Local variables
C
      BYTE MASK

      STATUS=0
C
C     Validate BITNUM
C
      IF (BITNUM.LT.0.OR.BITNUM.GT.7) THEN
         CALL PAR_WRUSER('Invalid value for bit number',STATUS)
         STATUS=1
         GO TO 500
      END IF
C
C     Set mask
C
      MASK=2**BITNUM
C
C     Modify value
C
      IF (BITVAL) THEN
         BYT=IOR(MASK,BYT)
      ELSE
         BYT=IAND(NOT(MASK),BYT)
      END IF
C
C
C     Return
C
  500 CONTINUE
      END
