C+
      SUBROUTINE FIT_SCALU (BSCALE,BZERO)
C
C     F I T _ S C A L U
C
C     If unsigned 16bit integers are to be written to the tape,
C     presumably by FIT_WRAYU, the BSCALE and BZERO keywords need
C     to be set properly.  This routine returns the values of
C     BSCALE and BZERO that should be applied to such data. FIT_WRAYU
C     will assume that these are the values that have been written to
C     the FITS header.
C
C     Parameters -   (">" input, "<" output)
C
C     (<) BSCALE   (Double pecision) The value of BSCALE that
C                  should be used for this data.
C     (<) BZERO    (Double precision) the value of BZERO that
C                  should be used for this data.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                     KS / AAO  1st Feb 1990
C+
      IMPLICIT NONE
C
C     Parameters
C
      DOUBLE PRECISION BSCALE,BZERO
C
C     Return the values of BSCALE and BZERO that FIT_WRAYU will assume.
C
      BSCALE=1.0D0
      BZERO=32768.0D0
C
      END
