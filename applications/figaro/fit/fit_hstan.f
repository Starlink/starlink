C+
      SUBROUTINE FIT_HSTAN (BITPIX,NAXIS,NAXES,BSCALE,BZERO,STATUS)
C
C     F I T _ H S T A N
C
C     This routine creates the FITS header entries for the
C     mandatory keywords SIMPLE, NAXIS, NAXIS1..NAXISn, and
C     the optional keywords BSCALE and BZERO.  This routine
C     is merely a packaged set of calls to FIT_WLOG, FIT_WINT
C     and FIT_WREAL, and the user may opt to use these directly
C     (for example, to force SIMPLE=F, or to explicitly comment
C     some of the keywords).  If this routine is used, it
C     should be called directly after FIT_INIT.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) BITPIX    (Integer) The number of bits per pixel.  If
C                   this is 8, 16, or 32 FIT_HSTAN will write a
C                   header with SIMPLE=T.  Otherwise, SIMPLE=F
C                   will be used.
C     (>) NAXIS     (Integer) The number of axes in the data array.
C                   This can be zero, if there is to be no data
C                   array written.
C     (>) NAXES     (Integer array NAXES(NAXIS)) Gives the dimensions
C                   of the array axes.  The information in this
C                   array is used to generate the keywords NAXIS1
C                   through NAXISn.  If NAXIS=0, a dummy argument
C                   should be provided.
C     (>) BSCALE    (Real) Scale factor used to convert tape pixel
C                   values to true values.  (True = [tape * BSCALE]
C                   + BZERO).  Will be omitted if set to zero.
C     (>) BZERO     (Real) Offset applied to true pixel values. See
C                   above.  Will be omitted if BSCALE set to zero.
C     (<) STATUS    (Integer) Return status.  0 => OK, non-zero values
C                   imply a tape I/O error and may be decoded using
C                   FIT_ERROR.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIT_WLOG   (FIT_ package) Write a logical keyword to header.
C     FIT_WINT   ( "     "    )   "   an integer   "     "    "
C     FIT_WREAL  ( "     "    )   "   a real       "     "    "
C
C                                            KS / CIT 10th Oct 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER BITPIX,NAXIS,NAXES(*),STATUS
      REAL BSCALE,BZERO
C
C     Local variables
C
      LOGICAL SIMPLE
      INTEGER I,IGNORE,NFMT
      CHARACTER FMT*12,NAME*10
C
C     Generate SIMPLE and BITPIX
C
      SIMPLE=(BITPIX.EQ.8).OR.(BITPIX.EQ.16).OR.(BITPIX.EQ.32)
      CALL FIT_WLOG('SIMPLE',SIMPLE,' ',STATUS)
      IF (STATUS.NE.0) GO TO 600
      CALL FIT_WINT('BITPIX',BITPIX,' ',STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Generate NAXIS and the various NAXISn keywords
C
      CALL FIT_WINT('NAXIS',NAXIS,' ',STATUS)
      IF (STATUS.NE.0) GO TO 600
      NFMT=1
      FMT='(''NAXIS'',I1)'
      DO I=1,NAXIS
         IF (MOD(I,10).EQ.0) THEN
            NFMT=NFMT+1
            FMT(11:11)=CHAR(NFMT+ICHAR('0'))
         END IF
         WRITE (NAME,FMT,IOSTAT=IGNORE) I
         CALL FIT_WINT(NAME,NAXES(I),' ',STATUS)
         IF (STATUS.NE.0) GO TO 600
      END DO
C
C     Generate BSCALE and BZERO
C
      IF (BSCALE.NE.0.) THEN
         CALL FIT_WREAL('BSCALE',BSCALE,' ',STATUS)
         IF (STATUS.NE.0) GO TO 600
         CALL FIT_WREAL('BZERO',BZERO,' ',STATUS)
      END IF
C
  600 CONTINUE
      END
