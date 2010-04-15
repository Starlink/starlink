C+
      SUBROUTINE FIT_WRAYS (DATA,NELM,STATUS)
C
C     F I T _ W R A Y S
C
C     Writes an integer*2 array to tape as part of a FITS image.
C
C     This routine simply copies the array element by element into a
C     buffer, writing the buffer to tape whenever it fills.  This means
C     that this routine may be called as many times as necessary - for
C     example, if all the data is not available in memory at one time.
C     This routine should be called for the first time immediately
C     following the FIT_WEND call that terminated the FITS header.  Once
C     all the data is output, FIT_CLOSE should be called to flush out
C     the data buffer.
C
C     Parameters -   (">" input, "W" workspace, "!" modified, "<" output)
C
C     (>) DATA     (Integer*2 array DATA(NELM)) The data array to be output.
C                  DATA may be multi-dimensional, but it is treated
C                  as linear here for generality.
C     (>) NELM     (Integer) The number of elements in DATA.
C     (<) STATUS   (Integer) Returned status code.  0 => OK, non-zero
C                  values indicate a tape I/O error and can be
C                  decoded using FIT_ERROR.
C
C     Common variables used -
C
C     (W) FBUFF    (Byte array) The main FITS I/O buffer
C     (W) FBUFFS   (Integer*2 array) The same buffer as FBUFF.
C     (!) FPTR     (Integer) Byte level pointer to FBUFF.
C     (>) NOSWAP   (Logical) Used to suppress byte swapping
C
C                   All defined in the file COMF.INC
C
C     Subroutines / functions used -
C
C     GEN_BSWAP    (GEN_ package) Swap order of bytes in words
C     FIT_MWRIT    (FIT_    "   ) Write FBUFF buffer to tape
C
C                                       KS / CIT 14th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM,STATUS
      INTEGER*2 DATA(NELM)
C
C     Common blocks
C
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER FPTRO,FPTRS,I
C
C     Write out as signed 16 bit integers.  The call to GEN_BSWAP
C     is needed because the VAX byte order is not that required
C     by the FITS standard.
C
      FPTRS=FPTR/2+1
      FPTRO=FPTRS
      DO I=1,NELM
         FBUFFS(FPTRS)=DATA(I)
         FPTRS=FPTRS+1
         IF (FPTRS.GT.1440) THEN
            IF (.NOT.NOSWAP)
     :            CALL GEN_BSWAP(FBUFFS(FPTRO),FPTRS-FPTRO)
            CALL FIT_MWRIT(STATUS)
            FPTR=1
            FPTRS=1
            FPTRO=1
            IF (STATUS.NE.0) GO TO 600
         END IF
      END DO
      IF ((.NOT.NOSWAP).AND.FPTRS.GT.1)
     :       CALL GEN_BSWAP(FBUFFS(FPTRO),FPTRS-FPTRO)
      FPTR=(FPTRS-1)*2+1
C
  600 CONTINUE
      END
