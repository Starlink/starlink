      SUBROUTINE  DAOPK (PAR, MAXPAR, PSF, MAXPSF, MAXEXP, F,
     .     MAXBOX, WATCH, FITRAD, PERERR, PROERR)
C
C=======================================================================
C
C Single-star profile-fitting routine.
C
C              OFFICIAL DAO VERSION:  1991 April 18
C
C This subroutine reads in an array around each star in an input
C photometry file and fits the point-spread function to the observed
C stellar profile by least-squares.  The three parameters solved for
C are the brightness ratio between the PSF and the program star, and
C the coordinates of the centroid of the program star.
C
C Arguments
C
C  WATCH (INPUT) governs whether information relating to the progress
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C FITRAD (INPUT) is the fitting radius.  Only pixels within FITRAD of
C        the current estimate of a star's centroid will be included in
C        the least-squares determination of its centroid and magnitude.
C
C All are user-definable optional parameters.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAXPSF, MAXEXP, MAXBOX, MAXPAR
C
C Parameters
C
C MAXPSF is the length of the side of the largest PSF look-up table
C        allowed.  (Note:  half-pixel grid size)
C
C MAXBOX is the length of the side of the largest box within which the
C        PSF can be evaluated.  (MAXBOX = (MAXPSF-7)/2 )
C
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER LINE*80, EXTEND*30, CASE*3
      REAL F(MAXBOX,MAXBOX)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), PAR(MAXPAR)
      INTEGER NINT, MIN0
C
      REAL LOBAD, AMAG, SHARP, CHI, ERRMAG, DELTAX, DELTAY
      REAL APMAG, RADIUS, FRAD, READNS, AP1, THRESH, HIBAD
      REAL PROERR, PERERR, FITRAD, WATCH, PHPADU, RONOIS
      REAL PERR, PKERR
      REAL X, Y, PSFMAG, BRIGHT, XPSF, YPSF, SCALE, SKY
      INTEGER RDPSF, NITER, IST, LX, LY, NX, NY
      INTEGER NFRAC, IBEG, ISTAR, NEXP, NPAR, NPSF, IPSTYP
      INTEGER IDUM, NL, ISTAT, NCOL, NROW, NTERM
C
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
      COMMON /ERROR/ PHPADU, RONOIS, PERR, PKERR
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Ascertain the name of the file containing coordinates and magnitude
C estimates for the program stars.  Open it and read the header.
C
      CALL TBLANK
  950 CALL GETNAM ('File with aperture results:', MAGFIL)
      IF ((MAGFIL .EQ. 'END OF FILE') .OR.
     .     (MAGFIL .EQ. 'GIVE UP')) THEN
         MAGFIL = ' '
         RETURN
      END IF
      CALL INFILE (2, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         MAGFIL = 'GIVE UP'
         GO TO 950
      END IF
C
      CALL RDHEAD (2, NL, IDUM, IDUM, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
      RONOIS=READNS**2
C
C Ascertain the name of the PSF file, open it, and read the PSF.
C
  900 CALL GETNAM ('File with the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END OF FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         PSFFIL = ' '
         CALL CLFILE (2)
         RETURN
      END IF
C
      ISTAT = RDPSF (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (ISTAT .NE. 0) THEN
         PSFFIL = 'GIVE UP'
         GO TO 900
      END IF
C
      NTERM = NEXP+NFRAC
      PERR = 0.01*PERERR
      PKERR = 0.01*PROERR/(PAR(1)*PAR(2))**2
C
C Get ready to do the PEAK fitting:  get the name of the output file.
C Open it and write the header.
C
      PROFIL=SWITCH(MAGFIL, CASE('.pk'))
  960 CALL GETNAM ('File for PEAK results:', PROFIL)
      IF ((PROFIL .EQ. 'END OF FILE') .OR.
     .     (PROFIL .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         PROFIL = ' '
         RETURN
      END IF
      PROFIL = EXTEND(PROFIL, CASE('pk'))
      CALL OUTFIL (3, PROFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//PROFIL)
         PROFIL = 'GIVE UP'
         GO TO 960
      END IF
C
      RADIUS = AMIN1( FITRAD, (REAL(NPSF-1)/2. - 1.)/2. )
      CALL WRHEAD (3, 1, NCOL, NROW, 7, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, RADIUS)
      IF (WATCH .GT. 0.5) CALL TBLANK               ! Type a blank line
      IBEG=1
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Reduce the stars, one by one.
C
C Read data for the next star and initialize things.
C
 2000 CALL RDSTAR (2, NL, ISTAR, X, Y, APMAG, SKY)
      IF (ISTAR .LT. 0) GO TO 9000            ! End-of-file encountered
      IF (ISTAR .EQ. 0) GO TO 2000            ! Blank line encountered
      IF (APMAG .GE. 99.) APMAG=PSFMAG+5.     ! The ol' college try
      DELTAX=(X-1.)/XPSF-1.
      DELTAY=(Y-1.)/YPSF-1.
      LX=MAX0(1, INT(X-RADIUS)+1)
      LY=MAX0(1, INT(Y-RADIUS)+1)
      NX=MIN0(NCOL, INT(X+RADIUS)) - LX + 1
      NY=MIN0(NROW, INT(Y+RADIUS)) - LY + 1
      CALL RDARAY ('DATA', LX, LY, NX, NY, MAXBOX, F, IST)
C
C At this point LX, LY are the coordinates in the big picture of the
C first pixel in the subarray.  The dimensions of the subarray are
C NX, NY.
C
      X=X-LX+1
      Y=Y-LY+1
C
C X,Y are now the coordinates of the star's centroid in the subframe
C (where (x,y)=(1.0,1.0) are the coordinates of the center of the first
C pixel and (x,y)=(NX,NY) are the coordinates of the center of the last
C pixel in the subarray).
C
C Display the subarray on the terminal, if desired, and type out headers
C if appropriate.
C
      IF (WATCH .GT. 1.5) CALL SHOW (F, F(NINT(X),NINT(Y)), 0.9*SKY,
     .     NX, NY, MAXBOX)
      IF ((WATCH .GT. 1.5) .OR. ((WATCH .GT. 0.5) .AND. (IBEG .EQ. 1)))
     .     WRITE (6,620)
  620 FORMAT (9X, 'STAR', 6X, 'X', 8X, 'Y', 8X, 'MAGNITUDE', 7X,
     .     'CHI  SHARP   IT')
      IBEG=2
      SCALE=10.**(-0.4*(APMAG-PSFMAG))                 ! Starting value
      CALL PKFIT (F, NX, NY, MAXBOX, X, Y, SCALE, SKY, RADIUS, LOBAD,
     .     HIBAD, BRIGHT, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     DELTAX, DELTAY, ERRMAG, CHI, SHARP, NITER)
      IF (NITER .GT. 0) GO TO 2010
C
C A singular matrix occurred during the least-squares solution.
C
      WRITE (LINE,621) ISTAR
  621 FORMAT (8X, I5, ' had a singular matrix.')
      CALL STUPID (LINE)
      GO TO 2000
C
C Everything went fine.
C
 2010 X=X+LX-1.
      Y=Y+LY-1.
      AMAG=PSFMAG-2.5*ALOG10(SCALE)
      ERRMAG=AMIN1(2.0, 1.086*ERRMAG/SCALE)
      IF (WATCH .GT. 0.5) WRITE (6,622) ISTAR, X, Y, AMAG, ERRMAG,
     .     CHI, SHARP, NITER
  622 FORMAT (8X, I5, 2F9.2, F9.3, ' +- ', F5.3, 2F7.2, I5)
      WRITE (3,320) ISTAR, X, Y, AMAG, ERRMAG, SKY, FLOAT(NITER), CHI,
     .     SHARP
  320 FORMAT (I6, 5F9.3, F9.0, F9.2, F9.3)
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLFILE (3)
      CALL CLFILE (2)
      CALL STUPID ('    Done.')
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  PKFIT (F, NX, NY, MAXBOX, X, Y, SCALE, SKY, RADIUS,
     .     LOBAD, HIBAD, BRIGHT, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     DELTAX, DELTAY, ERRMAG, CHI, SHARP, NITER)
C
C=======================================================================
C
C This is the subroutine which does the actual one-star least-squares
C profile fit for PEAK.
C
C           OFFICIAL DAO VERSION:  1991 April 6
C
C Arguments
C      F (INPUT) is an NX by NY array containing actual picture data.
C
C MAXBOX (INPUT) is the maximum value allowable for either NX or NY,
C        needed for the dimension statements below.  PEAK and PSF will
C        provide different values of MAXBOX.
C
C  SCALE (INPUT/OUTPUT) is the initial estimate of the brightness of
C        the star, expressed as a fraction of the brightness of the
C        PSF.  Upon return, the final computed value of SCALE will
C        be passed back to the calling routine.
C
C   X, Y (INPUT/OUTPUT) are the initial estimates of the centroid of
C        the star relative to the corner (1,1) of the subarray.  Upon
C        return, the final computed values of X and Y will be passed
C        back to the calling routine.
C
C    SKY (INPUT) is the local sky brightness value, carried on from
C        PHOTOMETRY via the data files.
C
C RADIUS (INPUT) is the fitting radius-- only pixels within RADIUS of
C        the instantaneous estimate of the star's centroid will be
C        included in the fit.
C
C LOBAD and HIBAD (INPUT) are bad pixel limits-- any pixel whose
C        brightness value falls outside this range will be presumed to
C        be bad, and will be ignored.
C
C BRIGHT (INPUT) contains the brightness normalization of the model
C        analytic point spread function
C
C    PAR (INPUT) contains the values of the remaining NPARAM parameters
C        defining the analytic function which approximates the core of
C        the PSF.
C
C    PSF (INPUT) is an NPSF by NPSF by NEXP+NFRAC look-up table
C        containing corrections from the analytic approximation of the
C        PSF to the true PSF.
C
C ERRMAG (OUTPUT) is the estimated standard error of the value of SCALE
C        returned by this routine.
C
C    CHI (OUTPUT) is the estimated goodness-of-fit statistic:  the ratio
C        of the observed pixel-to-pixel mean absolute deviation from
C        the profile fit, to the value expected on the basis of the
C        read-out noise and the photons/ADU (which are brought in
C        through COMMON block /ERROR/).
C
C  SHARP (OUTPUT) is a goodness-of-fit statistic describing how much
C        broader the actual profile of the object appears than the
C        profile of the PSF.
C
C  NITER (OUTPUT) is the number of iterations the solution required to
C        achieve convergence.  If NITER = 25, the solution did not
C        converge.  If for some reason a singular matrix occurs during
C        the least-squares solution, this will be flagged by setting
C        NITER = -1.
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers, very small numbers and very
*     large numbers with their PRM_PAR replacements.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXPSF, MAXEXP, MAXBOX, MAXPAR
C
C Parameter
C
C MAXPSF is the length of the side of the largest PSF look-up table
C        allowed.  (Note:  half-pixel grid spacing)
C
      REAL C(3,3), V(3), CLAMP(3), DTOLD(3)
      REAL F(MAXBOX,MAXBOX), T(3), DT(3), NUMER
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), PAR(MAXPAR)
      REAL USEPSF, AMAX1, AMIN1, ABS
      INTEGER MIN0, MAX0
C
      REAL LOBAD, DF, WT, DFDSIG, RHOSQ, RELERR, SIG, SIGSQ
      REAL FPOS, DVDXC, DVDYC, RSQ, DX, DXSQ, DY, DYSQ, DATUM
      REAL DENOM, SUMWT, CHIOLD, RADSQ, SHARP, CHI, ERRMAG
      REAL DELTAX, DELTAY, X, Y, PHPADU, RONOIS, PERR, PKERR
      REAL SCALE, SKY, RADIUS, HIBAD, BRIGHT
      INTEGER I, J, IX, IY, IXLO, IXHI, IYLO, IYHI, ISTAT
      INTEGER NPIX, NFRAC, NEXP, NITER, NPAR, NPSF, IPSTYP
      INTEGER NX, NY, NCOL, NROW
      LOGICAL CLIP, REDO
C
      COMMON /SIZE/ NCOL, NROW
      COMMON /ERROR/ PHPADU, RONOIS, PERR, PKERR
C
C-----------------------------------------------------------------------
C
C Initialize a few things for the solution.
C
      RADSQ=RADIUS**2
      DO 1010 I=1,3
      CLAMP(I)=1.
 1010 DTOLD(I)=0.0
      CHIOLD=1.
      NITER=0
      SHARP=0.
      CLIP = .FALSE.
C
C-----------------------------------------------------------------------
C
C Here begins the big least-squares loop.
C
 2000 NITER=NITER+1
C
C Initialize things for this iteration.  CHI and SHARP will be
C goodness-of-fit indices.  CHI will also be used in determining the
C weights of the individual pixels.  As the solution iterates, the new
C value of CHI will be built up in the variable CHI, while a smoothed
C value of CHI computed from the previous iteration will be carried
C along in CHIOLD.
C
      CHI=0.0
      SUMWT=0.0
      NUMER=0.0
      DENOM=0.0
      DO 2010 I=1,3
      V(I)=0.0                          ! Zero the vector of residuals
      DO 2010 J=1,3
 2010 C(I,J)=0.0                        ! Zero the normal matrix
C
C Choose the little box containing points inside the fitting radius.
C
      IXLO=MAX0(1, INT(X-RADIUS))
      IYLO=MAX0(1, INT(Y-RADIUS))
      IXHI=MIN0(NX, INT(X+RADIUS)+1)
      IYHI=MIN0(NY, INT(Y+RADIUS)+1)
C
C Now build up the normal matrix and vector of residuals.
C
      NPIX=0
      DO 2090 IY=IYLO,IYHI
      DY=FLOAT(IY)-Y
      DYSQ=DY**2
      DO 2090 IX=IXLO,IXHI
      DATUM=F(IX,IY)
      IF ((DATUM .LT. LOBAD) .OR. (DATUM .GT. HIBAD)) GO TO 2090
      DX=FLOAT(IX)-X
      DXSQ=DX**2
C
C DX and DY are the distance of this pixel from the centroid of the
C star.  Is this pixel truly inside the fitting radius?
C
      RSQ=(DXSQ+DYSQ)/RADSQ
      IF (1.-RSQ .LE. 2.E-6) GO TO 2090   ! Prevents floating underflows
C
C The fitting equation is of the form
C
C Observed brightness =
C     [SCALE + delta(SCALE)] * [PSF + delta(Xcen)*d(PSF)/d(Xcen) +
C                                           delta(Ycen)*d(PSF)/d(Ycen) ]
C
C and is solved for the unknowns delta(SCALE) ( = the correction to
C the brightness ratio between the program star and the PSF) and
C delta(Xcen) and delta(Ycen) ( = corrections to the program star's
C centroid).
C
C The point-spread function is equal to the sum of the integral under
C a two-dimensional analytic profile plus values interpolated from
C a look-up table.
C
      T(1)=USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF, NPSF, NPAR, NEXP,
     .     NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
      IF ((SCALE*T(1)+SKY .GT. HIBAD) .AND. (NITER.GE.3)) GO TO 2090
      T(2)=SCALE*DVDXC
      T(3)=SCALE*DVDYC
      DF=F(IX,IY)-SKY-SCALE*T(1)
C
C DF is the residual of the brightness in this pixel from the PSF fit.
C
C The expected random error in the pixel is the quadratic sum of
C the Poisson statistics, plus the readout noise, plus an estimated
C error of PERERR% of the total brightness for the difficulty of flat-
C fielding and bias-correcting the chip, plus an estimated error of
C some fraction of the fourth derivative at the peak of the profile,
C to account for the difficulty of accurately interpolating within the
C point-spread function.  The fourth derivative of the PSF is roughly
C proportional to H/sigma**4 (sigma is the width parameter for
C the stellar core); using the geometric mean of sigma(x) and sigma(y),
C this becomes H/[sigma(x)*sigma(y)]**2.  The ratio of the fitting
C error to this quantity is estimated from a good-seeing CTIO frame to
C be approximately 0.027.  NOWADAYS, PAR(1) and PAR(2) are the HWHM,
C not the sigma, so the coefficient is now of order 0.05 = 5.0%.
C
      FPOS=AMAX1(0., F(IX,IY)-DF)
C
C FPOS = raw data minus residual = model-predicted value of the
C intensity at this point (which presumably is non-negative).
C
      SIGSQ=FPOS/PHPADU+RONOIS+(PERR*FPOS)**2+(PKERR*(FPOS-SKY))**2
      SIG=SQRT(SIGSQ)
      RELERR=ABS(DF/SIG)
C
C SIG is the anticipated standard error of the intensity in this pixel,
C including readout noise, Poisson photon statistics, and an estimate
C of the standard error of interpolating within the PSF.
C
      WT=5./(5.+RSQ/(1.-RSQ))             ! Weight as function of radius
C
C Now add this pixel into the quantities which go to make up the SHARP
C index.
C
      RHOSQ=DXSQ/PAR(1)**2+DYSQ/PAR(2)**2
C
C Include in the sharpness index only those pixels within six
C HWHMs of the centroid of the star.  (This saves time and
C floating underflows by excluding pixels which contribute less than
C about one part in a million to the index.)
C
      IF (RHOSQ .LE. 36.) THEN
         RHOSQ=0.6931472*RHOSQ
         DFDSIG=EXP(-RHOSQ)*(RHOSQ-1.)
         FPOS=AMAX1(0., F(IX,IY)-SKY)+SKY
         NUMER=NUMER+DFDSIG*DF/SIGSQ
         DENOM=DENOM+DFDSIG**2/SIGSQ
      END IF
C
C Derive the weight of this pixel.  First of all, the weight depends
C upon the distance of the pixel from the centroid of the star-- it
C is determined from a function which is very nearly unity for radii
C much smaller than the fitting radius, and which goes to zero for
C radii very near the fitting radius.  Then reject any pixels with
C 10-sigma residuals (after the first iteration).
C
      CHI=CHI+WT*RELERR
      SUMWT=SUMWT+WT
C
C Now the weight is scaled to the inverse square of the expected mean
C error.
C
      WT=WT/SIGSQ
C
C Reduce the weight of a bad pixel.  A pixel having a residual of 2.5
C sigma gets reduced to half weight; a pixel having a residual of 5.
C sigma gets weight 1/257.
C
      IF (CLIP) WT=WT/(1.+(0.4*RELERR/CHIOLD)**8)
C
C Now add the pixel into the vector of residuals and the normal matrix.
C
      DO 2030 I=1,3
      V(I)=V(I)+DF*T(I)*WT
      DO 2030 J=1,3
 2030 C(I,J)=C(I,J)+T(I)*T(J)*WT
C
      NPIX=NPIX+1
 2090 CONTINUE                                 ! End of loop over pixels
C
C Compute the (robust) goodness-of-fit index CHI.
C
      IF (SUMWT .GT. 3.) THEN
         CHI=1.2533141*CHI/SQRT(SUMWT*(SUMWT-3.))
C
C CHI is pulled toward its expected value of unity before being stored
C in CHIOLD to keep the statistics of a small number of pixels from
C completely dominating the error analysis.
C
         CHIOLD=((SUMWT-3.)*CHI+3.)/SUMWT
      ELSE
         CHI = 1.
         CHIOLD = 1.
      END IF
C
C Compute the parameter corrections and check for convergence.
C
      IF (NPIX .LT. 3) THEN
         NITER=-1
         RETURN
      END IF
      CALL INVERS (C, 3, 3, ISTAT)
      IF (ISTAT .EQ. 0) GO TO 2100
      NITER=-1
      RETURN
C
C Everything OK so far.
C
 2100 CALL VMUL (C, 3, 3, V, DT)
C
C In the beginning, the brightness of the star will not be permitted
C to change by more than two magnitudes per iteration (that is to say,
C if the estimate is getting brighter, it may not get brighter by
C more than 525% per iteration, and if it is getting fainter, it may
C not get fainter by more than 84% per iteration).  The x and y
C coordinates of the centroid will be allowed to change by no more
C than one-half pixel per iteration.  Any time that a parameter
C correction changes sign, the maximum permissible change in that
C parameter will be reduced by a factor of 2.
C
      DO 2110 I=1,3
      IF (DTOLD(I)*DT(I) .LT. -VAL__SMLR) THEN
         CLAMP(I)=0.5*CLAMP(I)
      ELSE
         CLAMP(I)=MIN(1., 1.1*CLAMP(I))
      END IF
 2110 DTOLD(I)=DT(I)
C
      SCALE=SCALE+DT(1)/
     .  (1.+AMAX1(DT(1)/(5.25*SCALE),-DT(1)/(0.84*SCALE))/CLAMP(1))
      X=X+DT(2)/(1.+ABS(DT(2))/(0.4*CLAMP(2)))
      Y=Y+DT(3)/(1.+ABS(DT(3))/(0.4*CLAMP(3)))
      IF (NITER .LE. 1) GO TO 2000
      REDO=.FALSE.
C
C Convergence criteria:  if the most recent computed correction to the
C brightness is larger than 0.01% or than 0.05 * sigma(brightness),
C whichever is larger, OR if the absolute change in X or Y is
C greater than 0.001 pixels, convergence has not been achieved.
C
      ERRMAG=CHIOLD*SQRT(C(1,1))
      IF (CLIP) THEN
         IF (ABS(DT(1)) .GT.
     .        AMAX1( 0.05*ERRMAG, 0.0001*SCALE )) THEN
            REDO=.TRUE.
         ELSE IF (AMAX1(ABS(DT(2)),ABS(DT(3))) .GT. 0.001) THEN
            REDO=.TRUE.
         END IF
      ELSE
         IF (ABS(DT(1)) .GT.
     .        AMAX1( ERRMAG, 0.002*SCALE )) THEN
            REDO = .TRUE.
         ELSE IF (AMAX1(ABS(DT(2)),ABS(DT(3))) .GT. 0.02) THEN
            REDO = .TRUE.
         END IF
      END IF
C
      IF (REDO) GO TO 2000
      IF ((NITER .LT. 50) .AND. (.NOT. CLIP)) THEN
         CLIP = .TRUE.
         DO I=1,3
            DTOLD(I) = 0.
            CLAMP(I) = AMAX1(CLAMP(I), 0.25)
         END DO
         GO TO 2000
      ELSE
         SHARP=2.*PAR(1)*PAR(2)*NUMER/(BRIGHT*SCALE*DENOM)
         SHARP=AMIN1(99.999,AMAX1(SHARP,-99.999))
         RETURN
      END IF
C
      END!
