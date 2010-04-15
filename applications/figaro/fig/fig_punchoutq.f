C+
      SUBROUTINE FIG_PUNCHOUTQ (QUAL,NX,NY,IX1,IY1,IX2,IY2,BITNUM,
     :                          BITVAL)
C
C     F I G _ P U N C H O U T Q
C
C     Flags an area of an image as invalid, by setting a selected bit
C     in the quality array appropriately.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) QUAL    (Byte array QUAL(NX,NY)) Quality array to be affected
C     (>) NX      (Integer) The first dimension of QUAL
C     (>) NY      (Integer) The second dimension of QUAL
C     (>) IX1     (Integer) The first X value for the zapped area
C     (>) IY1     (Integer) The   "   Y   "    "   "    "     "
C     (>) IX2     (Integer) The last  X   "    "   "    "     "
C     (>) IY2     (Integer) The   "   Y   "    "   "    "     "
C     (>) BITNUM  (Integer) Bit number to be set
C     (>) BITVAL  (Logical) Value bit is to be set to
C
C     Common variables used - None
C
C     Functions / subroutines used
C
C     FIG_SETBIT (FIG_ package) Write to one bit of a BYTE variable.
C
C     Adapted 1998 from FIG_PUNCHOUT.
C
C                                          MBT / IoA 30th Jul 1998
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX1,IY1,IX2,IY2,BITNUM
      BYTE    QUAL(NX,NY)
      LOGICAL BITVAL
C
C     Local variables
C
      INTEGER IX,IY,STATUS
C
C     Zap the area
C
      DO IY=MAX(1,IY1),MIN(NY,IY2)
         DO IX=MAX(1,IX1),MIN(NX,IX2)
            CALL FIG_SETBIT(QUAL(IX,IY),BITNUM,BITVAL,STATUS)
            IF (STATUS.NE.0) GO TO 99
         END DO
      END DO
C
   99 CONTINUE

      END
