C+
      SUBROUTINE FIT_WRAYD (DATA,NELM,BITPIX,BSCALE,BZERO,STATUS)
C
C     F I T _ W R A Y D
C
C     Writes a double precision array to tape as part of a FITS image,
C     converting it to one of a) 8 bit unsigned integers, b) 16 bit
C     signed integers, c) 32 bit signed integers, by application of a
C     scale and zero value.
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
C     (>) DATA     (Double precision array DATA(NELM)) The data array to
C                  be output. DATA may be multi-dimensional, but it is
C                  treated as linear here for generality.
C     (>) NELM     (Integer) The number of elements in DATA.
C     (>) BITPIX   (Integer) Determines the number of bits per pixel
C                  to be used for the data.  Allowed values are
C                  8 (unsigned), 16 (signed), and 32 (signed).  If
C                  BITPIX is none of these, 32 will be assumed.
C     (>) BSCALE   (Double precision) The scale factor to apply to the
C                  data to convert it to integers.
C     (>) BZERO    (Double precision) The offset to apply to the data to
C                  convert it to integers.  The integers are calculated as
C                  integer = ( real - bzero ) / bscale + .5
C                  FIT_SCALD provides a means of calculating BSCALE
C                  and BZERO.
C     (<) STATUS   (Integer) Returned status code.  0 => OK, non-zero
C                  values indicate a tape I/O error and can be
C                  decoded using FIT_ERROR.
C
C     Common variables used -
C
C     (W) FBUFF    (Byte array) The main FITS I/O buffer
C     (W) FBUFFS   (Integer*2 array) The same buffer as FBUFF.
C     (W) FBUFFI   (Integer array) The same buffer as FBUFF.
C     (!) FPTR     (Integer) Byte level pointer to FBUFF.
C     (>) NOSWAP   (Logical) Used to suppress byte swapping.
C
C                   All defined in the file COMF.INC
C
C     Subroutines / functions used -
C
C     GEN_BSWAP    (GEN_ package) Swap order of bytes in words
C     GEN_WBSWAP   ( "      "   ) Swap order of bytes in longwords
C     FIT_MWRIT    (FIT_    "   ) Write FBUFF buffer to tape
C
C     Note: This routine assumes that the values for BSCALE and
C     BZERO will not generate data outside the integer range
C     used.  If this is not true, arithmetic faults will occur.
C
C                                       KS / CIT 14th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM,BITPIX,STATUS
      DOUBLE PRECISION DATA(NELM),BSCALE,BZERO
C
C     Common blocks
C
      INCLUDE 'COMF'
C
C     Local variables
C
      BYTE IBYTE
      INTEGER FPTRI,FPTRO,FPTRS,I,IVAL
      DOUBLE PRECISION SCALE
C
C     The equivalence is to get round the problem that a VAX byte is
C     signed, and so cannot be assigned to an integer in the range 0..255
C
      EQUIVALENCE (IBYTE,IVAL)
C
      SCALE=1./BSCALE
C
C     Operation depends on BITPIX
C
      IF (BITPIX.EQ.8) THEN
C
C        Write out as 8 bit unsigned integers.  Note the sneaky
C        use of IVAL and IBYTE.
C
         DO I=1,NELM
            IVAL=(DATA(I)-BZERO)*SCALE+0.5
            FBUFF(FPTR)=IBYTE
            FPTR=FPTR+1
            IF (FPTR.GT.2880) THEN
               CALL FIT_MWRIT(STATUS)
               FPTR=1
               IF (STATUS.NE.0)  GO TO 600
            END IF
         END DO
C
      ELSE IF (BITPIX.EQ.16) THEN
C
C        Write out as signed 16 bit integers.  The call to GEN_BSWAP
C        is needed because the VAX byte order is not that required
C        by the FITS standard.
C
         FPTRS=FPTR/2+1
         FPTRO=FPTRS
         DO I=1,NELM
            FBUFFS(FPTRS)=(DATA(I)-BZERO)*SCALE+0.5
            FPTRS=FPTRS+1
            IF (FPTRS.GT.1440) THEN
               IF (.NOT.NOSWAP)
     :             CALL GEN_BSWAP(FBUFFS(FPTRO),FPTRS-FPTRO)
               CALL FIT_MWRIT(STATUS)
               FPTR=1
               FPTRS=1
               FPTRO=1
               IF (STATUS.NE.0) GO TO 600
            END IF
         END DO
         IF ((.NOT.NOSWAP).AND.FPTRS.GT.1)
     :                 CALL GEN_BSWAP(FBUFFS(FPTRO),FPTRS-FPTRO)
         FPTR=(FPTRS-1)*2+1
C
      ELSE
C
C        Write out as signed 32 bit integers.  As for 16 bit integers
C        the byte order has to be reversed, here using GEN_WBSWAP.
C
         FPTRI=FPTR/4+1
         FPTRO=FPTRI
         DO I=1,NELM
            FBUFFI(FPTRI)=(DATA(I)-BZERO)*SCALE+0.5
            FPTRI=FPTRI+1
            IF (FPTRI.GT.720) THEN
               IF (.NOT.NOSWAP)
     :              CALL GEN_WBSWAP(FBUFFI(FPTRO),FPTRI-FPTRO)
               CALL FIT_MWRIT(STATUS)
               FPTR=1
               FPTRI=1
               FPTRO=1
               IF (STATUS.NE.0)  GO TO 600
            END IF
         END DO
         IF ((.NOT.NOSWAP).AND.FPTRI.GT.1)
     :           CALL GEN_WBSWAP(FBUFFI(FPTRO),FPTRI-FPTRO)
         FPTR=(FPTRI-1)*4+1
      END IF
C
  600 CONTINUE
      END
