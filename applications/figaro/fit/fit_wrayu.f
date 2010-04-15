C+
      SUBROUTINE FIT_WRAYU (DATA,NELM,STATUS)
C
C     F I T _ W R A Y U
C
C     Writes an unsigned array of 16bit integers to tape as part of a
C     FITS image, converting it to 16 bit signed integers, by application
C     of a scale and zero value.  The scale and zero value used is that
C     returned by FIT_SCALU, and it is assumed that the calling routine
C     has already arranged for these values to be written to the FITS
C     header, and that the data is being written with BITPIX=16.
C
C     This routine simply copies the array element by element into a
C     buffer, first flipping the sign bit (a trick that, in combination
C     with the BZERO value of 32768.0 returned by FIT_SCALU, produces
C     signed 16bit data that will give the correct values when read back)
C     and then writing the buffer to tape whenever it fills.  This means
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
C                  as linear here for generality.  Note that this is not
C                  strictly INTEGER*2 data in the VAX sense, since it is
C                  unsigned.
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
C     (>) NOSWAP   (Logical) Set to inhibit byte swapping
C
C                   All defined in the file COMF.INC
C
C     Subroutines / functions used -
C
C     GEN_BSWAP    (GEN_ package) Swap order of bytes in words
C     FIT_MWRIT    (FIT_    "   ) Write FBUFF buffer to tape
C
C                                       KS / AAO  1st Feb 1990
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
      INTEGER FPTRO,FPTRS,I,IVAL4
C
C     Write out as signed 16 bit integers, flipping the sign bit on
C     the way.  The call to GEN_BSWAP is needed because the VAX byte order
C     is not that required by the FITS standard.
C
C     Note how the sign bit flip combines with the BZERO value of 32768.0
C     to give the correct result.
C
C     Data      Data as hex     Flip sign bit   Interpret as     Add BZERO
C                                               a signed value   (32768.0)
C
C       0         0000             8000          -32768              0
C     32767       EFFF             FFFF            -1              32767.0
C     65535       FFFF             EFFF           32767            65535.0
C
C     Note that all this assumes that the data is held as 2's complement.
C     as does the rather dirty code involving the use of long integers
C     and adding or subtracting 32768.  (Personally, I think a better way
C     to do this would be to call a C subroutine to do the work, but
C     didn't want to add multi-lingual complications as well.)  The flipping
C     of the sign bit works as follows:
C
C     Data as hex       Interpreted as    Add 32768 if <0    Result in hex
C                          signed         Sub 32768 if >=0
C
C     0000                   0               -32768             8000
C     EFFF                 32767               -1               FFFF
C     FFFF                  -1                32767             EFFF
C
C     So this should work on any machine that uses two's complement
C     integers and supports INTEGER*2 and INTEGER (with INTEGER able
C     to hold the value 32768)
C
      FPTRS=FPTR/2+1
      FPTRO=FPTRS
      DO I=1,NELM
         IVAL4=DATA(I)
         IF (IVAL4.GE.0) THEN
            IVAL4=IVAL4-32768
         ELSE
            IVAL4=IVAL4+32768
         END IF
         FBUFFS(FPTRS)=IVAL4
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
