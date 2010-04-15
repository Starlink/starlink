C+
      SUBROUTINE GEN_ROT2D (IN,IX,IY,OUT)
C
C     G E N _ R O T 2 D
C
C     Rotates a 2D array through 90 degrees.  The array can
C     be any numeric type with 4 byte elements (ie REAL or
C     INTEGER but not INTEGER*2 or REAL*8).  This routine
C     attempts to operate efficiently by manipulating the
C     working set and adjusting the way it handles the data
C     according to the size of the working set.  This
C     routine is VAX specific, both in coding and concept.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN      (Real or Integer array IN(IX,IY)) The data
C                 to be rotated.
C     (>) IX      (Integer) The first dimension of the input
C                 array.
C     (>) IY      (Integer) The second dimension of the input
C                 array.
C     (<) OUT     (Real or Integer array OUT(IY,IX)) The
C                 rotated data.
C
C                                        KS / CIT  23rd Nov 1982
C     History:
C     23rd Nov 1982  KS/CIT. Original version.
C     11th Sep 1992  HME/UoE, Starlink. Remove VAX SYS$ calls.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX,IY
      INTEGER IN(1),OUT(1)
C
C     Local variables
C
      INTEGER SMALL,NEWRK,SMINC,LGINC,IYINC,IXINC
      INTEGER IXRES1,IXRES2,IYRES1,IYRES2,I,J
      INTEGER OUTBASE,INBASE,OUTINDEX,ININDEX
C
C     Get the smaller of the two array dimensions
C
      SMALL=MIN(IX,IY)
C
C     Find the current working set size.
C
C     CALL SYS$ADJWSL(,CURWRK)
C     CALL SYS$ADJWSL(%VAL(2048),NEWRK)
      NEWRK = 2048
C
C     The rotation is going to be performed in blocks SMINC
C     by LGINC. The criterion for an efficient rotation is that
C     the size of this block should be such that all the elements
C     can be held in the working set. 128 is the number of 4 byte
C     elements that fit into a page.
C
      IF (SMALL.LT.128) THEN
         SMINC=SMALL
      ELSE
         SMINC=128
      END IF
      LGINC=(REAL(NEWRK)*.75-REAL(SMINC))/(REAL(SMINC)*2./128.+1)
      IF (LGINC.GT.128) LGINC=(LGINC/128)*128
C
C     Now perform the rotation
C
      IF (SMALL.EQ.IY) THEN
         IYINC=SMINC
         IXINC=LGINC
      ELSE
         IXINC=SMINC
         IYINC=LGINC
      END IF
C
      DO IXRES1=1,IY,IYINC
         IXRES2=MIN(IY,IXRES1+IYINC-1)
         DO IYRES1=1,IX,IXINC
            IYRES2=MIN(IX,IYRES1+IXINC-1)
C
C            The section of code that follows is exactly
C            equivalent to -
C
C            do i=ixres1,ixres2
C               do j=iyres1,iyres2
C                  iyin=iy-i+1
C                  out(i,j)=in(j,iyin)
C               end do
C            end do
C
C            if IN is IN(IX,IY) and OUT is OUT(IY,IX), but
C            the compiler generates much better code from this
C            linear array version.
C
            OUTBASE=(IYRES1-1)*IY-1
            INBASE=(IY-IXRES1)*IX-1
            DO I=IXRES1,IXRES2
               OUTINDEX=I+OUTBASE
               ININDEX=IYRES1+INBASE
               DO J=IYRES1,IYRES2
                  OUT(OUTINDEX+1)=IN(ININDEX+1)
                  OUTINDEX=OUTINDEX+IY
                  ININDEX=ININDEX+1
               END DO
               INBASE=INBASE-IX
            END DO
C
         END DO
      END DO
C
C     Restore the old working set limit
C
C     CALL SYS$ADJWSL(%VAL(NEWRK-CURWRK),CURWRK)
C
      END
