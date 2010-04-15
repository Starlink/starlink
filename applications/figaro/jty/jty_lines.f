      SUBROUTINE JTY_LINES(N,I1,I2,SIGMIN,FRACMIN,DATA,NT,SCRATCH,
     :                                          NFOUND,TABLE,ISTAT)
* N       = number of pixels in the data array
* I1      = first pixel to search for a line
* I2      = last pixel to search for a line
* SIGMIN  = minimum sigma to consider a line
* FRACMIN = minimum fraction to consider a line
* DATA    = R*4 array of data
* NT      = number of possible table entries (TABLE(NT,xxx))
* SCRATCH = R*4 scratch array, N elements long
* NFOUND  = number of lines found
* TABLE   = data for lines found
*              1.  Pixel of center
*              2.  Line height above the continuum
*              3.  Line width (FWHM in pixels)
*              4.  Continuum at line center
*              5.  Slope of continuum (counts / pixel)
*              6.  Significance of line in "sigma"
* **************************CONVENTION******************************
* The first element of the array is taken to be pixel 0.0 to 1.0,
* that is, its centroid is at pixel 0.5.
* ******************************************************************
* IEND    = number of end pixels to avoid
* IBASE   = number of pixels to search for the baseline minimum
* NFIT    = n where 2n+1 is the number of pixels fit for line parameters
* NCONT   = number of whole points fit at the baseline minimum
* ISTAT   = status return code.  0 => OK, 1 => too many lines found
*
* Original version by J.Tonry.  Modified by KS to use real arrays
* for data and scratch, and SCRATCH and ISTAT parameters added.
*
*                                        KS / CIT 12th July 1983
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      PARAMETER ( IEND=10, IBASE=10 )
      PARAMETER ( NCONT=1, NFIT=2 )
      REAL DATA(N)
      REAL SCRATCH(N)
      REAL TABLE(NT,6)
        REAL MAXL,MAXR,MAXNEIGH,MINNEIGH,MINL,MINR,MINPL,MINPR,IVAL


C Compute a buffer of maxima of three
      DO 5 JPIX = 2,N-1
5     SCRATCH(JPIX) = MAX(MAX(DATA(JPIX-1),DATA(JPIX)),DATA(JPIX+1))

      NFOUND = 0

C Main loop: examine each pixel for being a line
      DO 1 JPIX = MAX(IEND,I1), MIN(N-IEND,I2)
      MAXL = SCRATCH(JPIX-2)
      MAXR = SCRATCH(JPIX+2)
      MAXNEIGH = MAX(MAXL,MAXR)
      MINNEIGH = MIN(MAXL,MAXR)
      IF(DATA(JPIX).LT.MAXNEIGH.OR.DATA(JPIX).LE.MINNEIGH) GOTO 1
C We've got a line candidate
C Find the baseline...
C Run down the line and look for an upturn.
      MINL = MAXNEIGH
      MINR = MAXNEIGH
      MINPL = 0.
      MINPR = 0.
      DO 20 I = 3,IBASE+3
      IVAL = (DATA(JPIX+I-1) + 2*DATA(JPIX+I) + DATA(JPIX+I+1))/4
      IF(MINPR.EQ.0.) THEN
          IF(IVAL.LT.MINR) THEN
              MINR = IVAL
          ELSE
              MINPR = JPIX+I-1
          ENDIF
      ENDIF
      IVAL = (DATA(JPIX-I-1) + 2*DATA(JPIX-I) + DATA(JPIX-I+1))/4
      IF(MINPL.EQ.0.) THEN
          IF(IVAL.LT.MINL) THEN
              MINL = IVAL
          ELSE
              MINPL = JPIX-I+1
          ENDIF
      ENDIF
      IF((MINPL.GT.0.).AND.(MINPR.GT.0.)) GOTO 21
20    CONTINUE
21    CONTINUE
C Now compute a linear polynomial (SLOPE,CONST) that fits the continuum.
      IF(MINPL.EQ.0.OR.MINPR.EQ.0) THEN
          SLOPE = 0
          CONST = 0
      ELSE
          VALUEL = (DATA(INT(MINPL)+1) + DATA(INT(MINPL)-NCONT))/2
          VALUER = (DATA(INT(MINPR)-1) + DATA(INT(MINPR)+NCONT))/2
          DO 30 I = 0,NCONT-1
          VALUEL = VALUEL + DATA(INT(MINPL)-I)
30        VALUER = VALUER + DATA(INT(MINPR)+I)
          VALUEL = VALUEL / (NCONT+1)
          VALUER = VALUER / (NCONT+1)
          PIXL = MINPL - 0.5 - (NCONT-3) / 2.
          PIXR = MINPR - 0.5 + (NCONT-3) / 2.
          SLOPE = (VALUEL - VALUER) / (PIXL - PIXR)
          CONST = VALUEL - SLOPE * PIXL
      ENDIF
C Verify that the line really satisfies the SIGMA and FRAC criterion.
      PIX = JPIX - 0.5
      CONTIN = SLOPE * PIX + CONST
      AVHT = (DATA(JPIX-1) + DATA(JPIX) + DATA(JPIX+1)) / 3. - CONTIN
      IF(CONTIN.EQ.0) CONTIN = 1
      SIGMA = AVHT / SQRT(ABS(CONTIN))
      FRAC = AVHT / CONTIN
C Does it statisfy the criteria?
      IF(SIGMA.LT.SIGMIN.AND.FRAC.LT.FRACMIN) GOTO 1
C Compute the parameters of the line
      CALL JTY_PARABOLA(JPIX,NFIT,SLOPE,CONST,CENTER,HEIGHT,WIDTH,DATA)
      IF(WIDTH.EQ.0) GOTO 1
C Write the line parameters

      CONTIN = SLOPE * CENTER + CONST
      NFOUND = NFOUND + 1
        IF (NFOUND.GT.NT) THEN
           ISTAT=1
           RETURN
        END IF
      TABLE(NFOUND,1) = CENTER
      TABLE(NFOUND,2) = HEIGHT
      TABLE(NFOUND,3) = WIDTH
      TABLE(NFOUND,4) = CONTIN
      TABLE(NFOUND,5) = SLOPE
      TABLE(NFOUND,6) = SIGMA

*     WRITE(2,1000) CENTER, HEIGHT, WIDTH, CONTIN, SLOPE, SIGMA
*1000 FORMAT(1X,F10.2,F9.1,F9.3,F9.1,F9.3,F9.1)

1     CONTINUE

        ISTAT=0
      RETURN
      END
