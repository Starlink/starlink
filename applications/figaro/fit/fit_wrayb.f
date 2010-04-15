C+
      SUBROUTINE FIT_WRAYB (DATA,NELM,STATUS)
C
C     F I T _ W R A Y B
C
C     Writes an array of bytes to tape as part of a FITS image.
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
C     (>) DATA     (Byte array DATA(NELM)) The data array to be output.
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
C     (!) FPTR     (Integer) Byte level pointer to FBUFF.
C
C                   All defined in the file COMF.INC
C
C     Subroutines / functions used -
C
C     FIT_MWRIT    (FIT_    "   ) Write FBUFF buffer to tape
C
C     Note: The byte array is written unchanged to the tape, but
C     the user should note that under the FITS standard it will be
C     interpreted as an UNsigned byte array, while in VAX Fortran
C     a byte array is normally regarded as signed.
C
C                                       KS / CIT 11th Oct 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM,STATUS
      BYTE DATA(NELM)
C
C     Common blocks
C
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER I
C
C     Write out as unsigned bytes.
C
      DO I=1,NELM
         FBUFF(FPTR)=DATA(I)
         FPTR=FPTR+1
         IF (FPTR.GT.2880) THEN
            CALL FIT_MWRIT(STATUS)
            FPTR=1
            IF (STATUS.NE.0) GO TO 600
         END IF
      END DO
C
  600 CONTINUE
      END
