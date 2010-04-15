      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*        Replaced very negative, very large or very small numbers
*        with their PRM_PAR equivalents.
*     21-MAR-2000 (MBT)
*        Moved definition of some constants to external include file, and
*        initialised some string variables (to avoid garbage in defaults).
*     6-JUN-2000 (MBT)
*        Modified for dynamic array allocation.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

      INTEGER PSFMAX, NOPT

      PARAMETER (PSFMAX=35, NOPT=11)
C
C PSFMAX is the maximum PSF radius.  A value of 35 is consistent with
C        DAOPHOT.
C
C   NOPT is just the number of user-definable options (see below).
C
      CHARACTER*26 LBL(NOPT)
      REAL OPT(NOPT), OMIN(NOPT), OMAX(NOPT)
      INTEGER IPDAT, IPSUB, IPSIG, IPID, IPNP, IPSK, IPLA, IPR(14)
C
      CHARACTER*30 INPICT, OPTFIL, CASE
      REAL PERERR, PROERR
      INTEGER NCOL, NROW, MAXSTR, ISTAT, I
C     INTEGER MADRID                              ! MIDAS
      LOGICAL OPEN, CENTER
C
C     INCLUDE 'MID_INCLUDE:ST_DEF.INC'            ! MIDAS
C     COMMON /VMR/ MADRID                         ! MIDAS
      COMMON /SIZE/ NCOL, NROW
C     INCLUDE 'MID_INCLUDE:ST_DAT.INC'            ! MIDAS
C
      DATA OPEN /.FALSE./
      DATA LBL/'            FITTING RADIUS',     ! 1
     .         '    CE (CLIPPING EXPONENT)',     ! 2
     .         '     REDETERMINE CENTROIDS',     ! 3
     .         '       CR (CLIPPING RANGE)',     ! 4
     .         '            WATCH PROGRESS',     ! 5
     .         '        MAXIMUM GROUP SIZE',     ! 6
     .         '      PERCENT ERROR (in %)',     ! 7
     .         '      PROFILE ERROR (in %)',     ! 8
     .         '     IS (INNER SKY RADIUS)',     ! 9
     .         '     OS (OUTER SKY RADIUS)',     ! 10
     .         '    MS (MAX STARS IN FILE)'/     ! 11
      DATA OPT / 2.5, 6., 1., 2.5, 1., 50., 0.75, 5., 0., 0., 5000./
      DATA OMIN  / 1.6, 0.0, 0.0, 0., 0.0, 1.0, 0., 0., 0., 0., 10./
      DATA OMAX  / 10., 8.0, 1.0, 10., 2.0, 100., 100., 100.,
     .           35., 50., 1000000./
C
C Set up the values of the optional parameters.
C
C Call OPTION, first with OPTFIL = 'allstar.opt' to set initial
C values for the optional parameters.  If the file isn't there, the
C routine will check that the default values (specified in the data
C statement above) are valid, and return here with those values intact.
C
C     CALL STSPRO ('-1')                             ! MIDAS
C     CALL STECNT('PUT', 1, 0, 0)                    ! MIDAS
      CALL FABORT
      OPTFIL=CASE('allstar.opt')
      CALL OPTION (OPTFIL, NOPT, LBL, OPT, OMIN, OMAX, 'OPT>', ISTAT)
      CALL TBLANK
      IF (OPT(3) .GE. 0.5) THEN
         CENTER=.TRUE.
      ELSE
         CENTER=.FALSE.
      END IF
      PERERR = 0.01*OPT(7)
      PROERR = 0.01*OPT(8)
      MAXSTR = INT(OPT(11))
C
      INPICT = 'END OF FILE'
 1900 CALL GETNAM ('Input image name:', INPICT)
      IF ((INPICT .EQ. 'END OF FILE') .OR. (INPICT .EQ. 'EXIT'))
     .     CALL BYEBYE
      CALL ATTACH (INPICT, OPEN)
      IF (.NOT. OPEN) THEN
         INPICT = 'EXIT'
         GO TO 1900
      END IF
C
C Allocate memory for use by the ALLSTR routine.
C
      CALL DAO_ALLOC( '_REAL', NCOL*NROW, IPDAT )
      CALL DAO_ALLOC( '_REAL', NCOL*NROW, IPSUB )
      CALL DAO_ALLOC( '_REAL', NCOL*NROW, IPSIG )
      DO I = 1, 14
         CALL DAO_ALLOC( '_REAL', MAXSTR+1, IPR(I) )
      END DO
      CALL DAO_ALLOC( '_INTEGER', MAXSTR+1, IPID )
      CALL DAO_ALLOC( '_INTEGER', MAXSTR+1, IPNP )
      CALL DAO_ALLOC( '_LOGICAL', MAXSTR+1, IPSK )
      CALL DAO_ALLOC( '_LOGICAL', MAXSTR+1, IPLA )
C
C Call the routine to do the work.
C
      CALL ALLSTR (%VAL(CNF_PVAL(IPDAT)), NCOL, NROW,
     :             %VAL(CNF_PVAL(IPSUB)), %VAL(CNF_PVAL(IPSIG)),
     .     %VAL(CNF_PVAL(IPR(1))), %VAL(CNF_PVAL(IPR(2))),
     :     %VAL(CNF_PVAL(IPR(3))), %VAL(CNF_PVAL(IPR(4))),
     .     %VAL(CNF_PVAL(IPR(5))), %VAL(CNF_PVAL(IPR(6))),
     :     %VAL(CNF_PVAL(IPR(7))), %VAL(CNF_PVAL(IPR(8))),
     .     %VAL(CNF_PVAL(IPR(9))), %VAL(CNF_PVAL(IPR(10))),
     :     %VAL(CNF_PVAL(IPR(11))), %VAL(CNF_PVAL(IPR(12))),
     .     %VAL(CNF_PVAL(IPR(13))), %VAL(CNF_PVAL(IPR(14))),
     .     %VAL(CNF_PVAL(IPID)), %VAL(CNF_PVAL(IPNP)),
     :     %VAL(CNF_PVAL(IPSK)), %VAL(CNF_PVAL(IPLA)), MAXSTR,
     .     OPT(1), OPT(5), OPT(4), NINT(OPT(2)), CENTER,
     .     NINT(OPT(6)), PERERR, PROERR, OPT(9), OPT(10))
C
C Deallocate the workspace.
C
      CALL DAO_DEALL( IPLA )
      CALL DAO_DEALL( IPSK )
      CALL DAO_DEALL( IPNP )
      CALL DAO_DEALL( IPID )
      DO I = 14, 1, -1
         CALL DAO_DEALL( IPR(I) )
      END DO
      CALL DAO_DEALL( IPSIG )
      CALL DAO_DEALL( IPSUB )
      CALL DAO_DEALL( IPDAT )
C
C Close down and exit.
C
      CALL CLPIC ('DATA')
      CALL BYEBYE
      END!
C
C#######################################################################
C
      SUBROUTINE  ALLSTR  (DATA, NCOL, NROW, SUBT, SIGMA,
     .     XC, YC, MAG, SKY, RPIXSQ, CHI, SUMWT, NUMER, DENOM, MAGERR,
     .     DXOLD, DYOLD, XCLAMP, YCLAMP, ID, NPIX, SKIP, LAST, MAXSTR,
     .     FITRAD, WATCH, HALF, IEXP, CENTER,
     .     MAXGRP, PERERR, PROERR, SKYIN, SKYOUT)
C
C=======================================================================
C
C Photometry for many stars by simultaneous multiple PSF fits iterating
C the entire star list simultaneously.
C
C              OFFICIAL DAO VERSION:  1987 June 30
C
C Input arguments:
C
C       DATA is the input image, stored as a two-dimensional REAL
C            array.
C
C NCOL, NROW are the dimensions of the input image.
C
C       SUBT is the address of some working space where ALLSTAR can
C            keep a scratch copy of the image.
C
C     SIGMA is the address of some working space where ALLSTAR can
C            keep track of the standard errors of the individual pixels
C
C     FITRAD is the user-definable parameter "Fitting radius."  It
C            governs how many pixels out from the centroid of the star
C            will actually be considered in computing the least-
C            squares profile fits.
C
C      WATCH is the 'watch progress' parameter specified by the user.
C            If WATCH > 0, information relating to the progress of
C            the reductions will be typed on the terminal during
C            execution.
C
C HALF, IEXP are the "Clipping radius" and "Clipping exponent"
C            parameters which specify the degree to which discordant
C            pixels are to be ignored.
C
C     CENTER is a logical specifying whether the user wants the stellar
C            centroids to be redetermined.
C
C     MAXGRP is the largest group size for which the solution is to be
C            performed.
C
C=======================================================================
C
      IMPLICIT NONE


*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXSTR, MAXPSF, MAXIT, MAXMAX, MAXPAR, MAXEXP
      PARAMETER (MAXPSF=145, MAXIT=200, MAXMAX=100,
     .     MAXPAR=6, MAXEXP=6)
C
C Parameters:
C
C MAXSTR The maximum number of stars in a frame.
C
C MAXPSF the largest PSF look-up table that can be accomodated.  If
C        PSFMAX is the largest acceptable PSF radius (see main-line
C        program above), then MAXPSF = 2*[2*(PSFMAX+1)]+7.
C
C MAXMAX is the largest group for which a solution will ever be
C        attempted = maximum permissible value of MAXGRP.
C
      INTEGER NCOL, NROW
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER LINE*80, SUBPIC*30, EXTEND*30, CASE*5
      REAL C(3*MAXMAX+1,3*MAXMAX+1), V(3*MAXMAX+1), X(3*MAXMAX+1)
      REAL DATA(NCOL,NROW), SUBT(NCOL,NROW), SIGMA(NCOL,NROW)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), PAR(MAXPAR)
      REAL XC(MAXSTR+1), YC(MAXSTR+1), MAG(MAXSTR+1), SKY(MAXSTR+1)
      REAL RPIXSQ(MAXSTR), CHI(MAXSTR), SUMWT(MAXSTR)
      REAL NUMER(MAXSTR), DENOM(MAXSTR)
      REAL MAGERR(MAXSTR), DXOLD(MAXSTR), DYOLD(MAXSTR)
      REAL XCLAMP(MAXSTR), YCLAMP(MAXSTR)
      INTEGER ID(MAXSTR+1)
      INTEGER NPIX(MAXSTR)
      LOGICAL SKIP(MAXSTR), LAST(MAXSTR)
C
      REAL AMIN1, AMAX1, ABS, SQRT, USEPSF
      INTEGER MIN0, MAX0, INT, RDPSF
C
      REAL LOBAD, HIBAD, ERR, SEP, THRESH, AP1, PHPADU, RONOIS, DUM
      REAL FAINT, CLMPMX, WCRIT, RADIUS, XMIN, XMAX, YMIN, YMAX
      REAL PERERR, PROERR, SKYIN, SKYOUT, SKYISQ, SKYOSQ, RELERR
      REAL PSFMAG, BRIGHT, XPSF, YPSF, SEPCRT, SEPMIN, PSFRAD, PEAK
      REAL FITRAD, WATCH, HALF, PKERR, PSFRSQ, RADSQ, RSQ, DX, DYSQ
      REAL SIGSQ, WT, VAL, RHOSQ, DWT, SKYBAR, CHIGRP, SUMRES, GRPWT
      REAL DPOS, DFDSIG, XKWT, DF, DIFF, SHARP, DELTAX, DELTAY, D, DY
      REAL DVDXC, DVDYC, Y
      INTEGER I, J, K, L, IFAINT, IDUM, IY, IX, NSTAR, NCONV, NITER
      INTEGER MAXGRP, ITSKY, MINSKY, MAXUNK, LX, LY, ISTAT, INTRVL
      INTEGER IPSTYP, NPSF, NPAR, NEXP, NFRAC, NL
      INTEGER IEXP, IXMIN, IXMAX, IYMIN, IYMAX, MX, MY, NDISAP
      INTEGER I3, J3, LSTAR, NSTR, NTERM, I3M2, ISTAR, N
      LOGICAL CENTER, CLIP, OMIT, REDO
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C Set some default values for strings which may be used as prompts.
C
      COOFIL = 'END OF FILE'
      MAGFIL = 'END OF FILE'
      PSFFIL = 'END OF FILE'
      PROFIL = 'END OF FILE'
      GRPFIL = 'END OF FILE'
C
C Set initial value for SUMWT.
C
      DO I = 1, MAXSTR
         SUMWT(I) = 1.
      END DO
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready, get set, . . .
C
      SKYISQ = SKYIN**2
      SKYOSQ = AMIN1(SKYOUT**2, SKYISQ + (MAXSTR-1)/3.1415927)
      SKYOUT = AMIN1(SKYOUT, SQRT(SKYOSQ))
      ITSKY = 3
      MINSKY = 100.
      MAXUNK=MAXMAX*3+1            ! Largest possible number of unknowns
      CALL TBLANK                                  ! Type a blank line
C
C Read the point-spread function into memory.
C
  950 CALL GETNAM ('File with the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END OF FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) RETURN
C
      ISTAT = RDPSF (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (ISTAT .NE. 0) THEN
         PSFFIL = 'GIVE UP'
         GO TO 950
      END IF
      PEAK = USEPSF(IPSTYP, 0., 0., BRIGHT, PAR, PSF, NPSF,
     .     NPAR, NEXP, NFRAC, 0., 0., DVDXC, DVDYC)
C
C Stars will be checked for merger if they are separated by less than
C 1 FWHM of the image core.
C
C     Crit. sep. = 2.355*sigma, where
C          sigma = SQRT [ (sigma(X)**2 + sigma(Y)**2)/2 ]
C
      SEPCRT=2.*(PAR(1)**2+PAR(2)**2)
C
C Stars will be considered unconditionally merged if they are separated
C by less than about 0.375 * FWHM.
C
      SEPMIN=AMIN1(1.,0.14*SEPCRT)
C
C SEPCRT contains the square of the critical separation.
C SEPMIN  contains the square of the minimum separation.
C
      PKERR=PROERR/(PAR(1)*PAR(2))**2      ! See fitting errors below
C
      PSFRAD = (REAL(NPSF-1)/2.-1.)/2.
      PSFRSQ = PSFRAD**2
C
C Ascertain the name of the file with the input photometry, and open it.
C
  960 CALL GETNAM ('Input file:', MAGFIL)
      IF ((MAGFIL .EQ. 'END OF FILE') .OR.
     .     (MAGFIL .EQ. 'GIVE UP')) RETURN
C
      CALL INFILE (2, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         GO TO 960
      END IF
C
      CALL RDHEAD (2, NL, IDUM, IDUM, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, RONOIS, DUM)
      IF ((NL .LT. 1) .OR. (NL .GT. 3)) THEN
         CALL STUPID ('Not a valid input file.')
         CALL CLFILE (2)
         MAGFIL = 'GIVE UP'
         GO TO 960
      END IF
C
C Inquire the name of the output file, and open it.
C
      PROFIL=SWITCH(MAGFIL, CASE('.als'))
  970 CALL GETNAM ('File for results:', PROFIL)
      IF ((PROFIL .EQ. 'END OF FILE') .OR.
     .     (PROFIL .EQ. 'GIVE UP')) RETURN
C
      PROFIL = EXTEND(PROFIL, CASE('als'))
      CALL OUTFIL (1, PROFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//PROFIL)
         PROFIL = 'GIVE UP'
         GO TO 970
      END IF
      CALL WRHEAD (1, 1, NCOL, NROW, 7, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, RONOIS, FITRAD)
C
C Name for the output image.
C
      SUBPIC=SWITCH(PROFIL, CASE('s'))
      CALL GETNAM ('Name for subtracted image:', SUBPIC)
      IF (SUBPIC(1:11) .EQ. 'END OF FILE') CALL TBLANK
C
C We need to keep track of the anticipated standard error of the
C brightness value in a given pixel as the reductions proceed.
C
      RONOIS=RONOIS**2
C
      LX = 1
      L = 1
      DO IY=1,NROW
         LY = IY
         CALL RDARAY ('DATA', LX, LY, NCOL, L, NCOL, DATA(1,IY), ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error reading picture.')
            RETURN
         END IF
C
         DO IX=1,NCOL
            IF ((DATA(IX,IY) .GT. HIBAD) .OR.
     .           (DATA(IX,IY) .LT. LOBAD)) THEN
               SIGMA(IX,IY) = VAL__MINR
            ELSE
               SIGMA(IX,IY) = RONOIS
            END IF
         END DO
      END DO
C
      RADSQ=FITRAD**2
      NSTAR=0
C
C Read in all the stars.
C
      FAINT=PSFMAG+12.5
      CLMPMX=0.25*FITRAD
      I=0
 1110 I=I+1
      IF (I .GT. MAXSTR) THEN
         CALL STUPID (
     .        'Too many stars - increase MS parameter and try again')
         CALL CLPIC ('DATA')
         CALL BYEBYE
      END IF
 1120 CALL RDSTAR (2, NL, ID(I), XC(I), YC(I), MAG(I), SKY(I))
      IF (ID(I) .LT. 0) GO TO 2000        ! End-of-file was encountered
      IF (ID(I) .EQ. 0) GO TO 1120        ! A blank line was encountered
      IF (MAG(I) .LT. FAINT) THEN
         MAG(I)=10.**(0.4*(PSFMAG-MAG(I)))
      ELSE
         MAG(I)=0.003
      END IF
      DXOLD(I)=0.
      DYOLD(I)=0.
      XCLAMP(I)=CLMPMX
      YCLAMP(I)=CLMPMX
      GO TO 1110
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C GO.
C
 2000 NSTAR=I-1                                      ! Number of stars
      CALL CLFILE (2)
      WRITE (6,5552) NSTAR
 5552 FORMAT (/1X, I5, ' stars.  <<')
      WRITE (6,610)
  610 FORMAT (//' I = iteration number',
     .        //' R = number of stars that remain'
     .        //' D = number of stars that disappeared'
     .        //' C = number of stars that converged'
     .       //)
      CALL OVRWRT ('     I     R     D     C', 1)
C
C Initialize accumulators and constraints on parameter corrections.
C
      NCONV=0
      NITER=0
      CALL STRIP (ID, XC, YC, MAG, SKY, SKIP, MAXSTR,
     .     NSTAR, NDISAP, SQRT(SEPMIN), NUMER, DENOM)
      IF (NDISAP .GT. 0) THEN
         WRITE (LINE,682) NITER, NSTAR, NDISAP, NCONV
         CALL OVRWRT (LINE(1:28), 3)
      END IF
C
 2100 NITER = NITER+1
      CLIP = (IEXP .NE. 0) .AND. (NITER .GE. 4)
C
C Set up critical errors for star rejection.
C
      WCRIT = 0.
      IF (NITER .GE. 5) WCRIT = 1.0
      IF (NITER .GE. 10) WCRIT = 1.5
      IF (NITER .GE. 15) WCRIT = 2.0
C
C Sort stars by y-coordinate, for minimum paging.
C
      CALL QUICK (YC, NSTAR, NUMER)
      CALL RECTFY (ID, NSTAR, NUMER, DENOM)
      CALL RECTFY (XC, NSTAR, NUMER, DENOM)
      CALL RECTFY (MAG, NSTAR, NUMER, DENOM)
      CALL RECTFY (SKY, NSTAR, NUMER, DENOM)
      CALL RECTFY (SUMWT, NSTAR, NUMER, DENOM)
      CALL RECTFY (DXOLD, NSTAR, NUMER, DENOM)
      CALL RECTFY (DYOLD, NSTAR, NUMER, DENOM)
      CALL RECTFY (XCLAMP, NSTAR, NUMER, DENOM)
      CALL RECTFY (YCLAMP, NSTAR, NUMER, DENOM)
C
C Determine how much of the image we need to copy into the working area.
C
C First determine the big rectangular area that contains all the pixels
C within one working radius of any star.  The "working radius" is
C either the fitting radius or the outer sky radius (if this is an
C iteration during which the sky is to be determined), whichever is
C larger.  Set their SIGMAs negative.
C
      IF ((MOD(NITER, ITSKY) .EQ. 0) .AND. (SKYOUT .GT. FITRAD))
     .     THEN
         RADIUS = SKYOUT
      ELSE
         RADIUS = FITRAD
      END IF
C
      XMIN=NCOL+RADIUS
      XMAX=-RADIUS
      DO I=1,NSTAR
         XMIN=AMIN1(XMIN, XC(I))
         XMAX=AMAX1(XMAX, XC(I))
      END DO
C
      IXMIN=MAX0(1, MIN0(NCOL, INT(XMIN-RADIUS)+1))
      IXMAX=MAX0(1, MIN0(NCOL, INT(XMAX+RADIUS)))
      IYMIN=MAX0(1, MIN0(NROW, INT(YC(1)-RADIUS)+1))
      IYMAX=MAX0(1, MIN0(NROW, INT(YC(NSTAR)+RADIUS)))
C
      IF (WATCH .GT. 0.5) THEN
C
C Set up reporting intervals for row counter.
C
         D=AMAX1(0.1, ALOG10(FLOAT(IYMAX-IYMIN+1)/10.))
         INTRVL=INT(D)
         D=D-FLOAT(INTRVL)
         IF (D .GT. 0.8) THEN
            INTRVL=10**(INTRVL+1)
         ELSE IF (D .GT. 0.5) THEN
            INTRVL=5*10**INTRVL
         ELSE IF (D .GT. 0.2) THEN
            INTRVL=2*10**INTRVL
         ELSE
            INTRVL=10**INTRVL
         END IF
      END IF
C
C Beginning of big loop over y-coordinates.
C
      RSQ = RADIUS**2
      N = 0
      L = 0
      DO 2190 IY=IYMIN,IYMAX
         Y = REAL(IY)
         DO IX=IXMIN,IXMAX
            SIGMA(IX,IY) = -ABS(SIGMA(IX,IY))
         END DO
C
C OK.  Now find all the points within one working radius of each
C star.  Set the sigmas of these pixels positive again and copy
C them from DATA to SUBT.
C
C N points at the last star that has such a low x-coordinate
C that it can be omitted.
C
         DO 2120 I=N+1,NSTAR
            DY = Y - YC(I)
C
            IF (DY .GT. RADIUS) THEN
               N = I
               GO TO 2120
            ELSE IF (DY .LT. -RADIUS) THEN
               GO TO 2150
            ELSE
               DYSQ = (REAL(IY) - YC(I))**2
               LX=MAX0(1, MIN0(NCOL, INT(XC(I)-RADIUS)+1))
               MX=MAX0(1, MIN0(NCOL, INT(XC(I)+RADIUS)))
               DO IX=LX,MX
                  DX = REAL(IX) - XC(I)
                  IF (DX**2+DYSQ .LE. RSQ) THEN
                     IF (SIGMA(IX,IY) .GT. VAL__MINR) THEN
                        SIGMA(IX,IY) = ABS(SIGMA(IX,IY))
                        SUBT(IX,IY) = DATA(IX,IY)
                     END IF
                  ELSE
                     IF (DX .GE. 0.) GO TO 2120
                  END IF
               END DO
            END IF
 2120    CONTINUE
C
C Subtract from the working copy of the image (SUBT) each star which
C has not yet converged by subtracting the shifted, scaled PSF
C according to the current best parameter estimates; do this only
C for pixels with positive SIGMAs, indicating that
C they will actually be needed for this iteration.
C
C L points at the last star that has such a low y-coordinate
C that it can be omitted.
C
 2150    CONTINUE
         DO 2170 I=L+1,NSTAR
            DY = Y - YC(I)
            IF (DY .GT. PSFRAD) THEN
               L = I
               GO TO 2170
            ELSE IF (DY .LT. -PSFRAD) THEN
               GO TO 2180
            ELSE
               LX=MAX0(IXMIN, MIN0(IXMAX, INT(XC(I)-PSFRAD)+1))
               MX=MAX0(IXMIN, MIN0(IXMAX, INT(XC(I)+PSFRAD)))
               DELTAX=(XC(I)-1.)/XPSF-1.
               DELTAY=(YC(I)-1.)/YPSF-1.
               DYSQ=DY**2
               DO 2160 IX=LX,MX
                  IF (SIGMA(IX,IY) .GT. 0.) THEN
                     DX=FLOAT(IX)-XC(I)
                     IF (DX**2+DYSQ .LT. PSFRSQ) THEN
                        SUBT(IX,IY)=SUBT(IX,IY)-
     .                       MAG(I)*USEPSF(IPSTYP, DX, DY, BRIGHT,
     .                       PAR, PSF, NPSF, NPAR, NEXP, NFRAC,
     .                       DELTAX, DELTAY, DVDXC, DVDYC)
                     ELSE
                        IF (DX .GT. 0.) GO TO 2170
                     END IF
                  END IF
 2160          CONTINUE
            END IF
 2170    CONTINUE
 2180    IF ((WATCH .GT. 0.5) .AND.
     .        (INTRVL*(IY/INTRVL) .EQ. IY)) THEN
            WRITE (LINE, 5551) IYMIN, IY, IYMAX
 5551       FORMAT (3I5)
            CALL OVRWRT (LINE(1:15), 2)
         END IF
 2190 CONTINUE
C
C If sky values are to be redetermined this iteration, do it now.
C
      IF ((SKYOUT .GT. 0.5) .AND. (MOD(NITER, ITSKY) .EQ. 0))
     .     THEN
         DO 2198 ISTAR=1,NSTAR
            LX=MAX0(IXMIN, MIN0(IXMAX, INT(XC(ISTAR)-SKYOUT)+1))
            MX=MAX0(IXMIN, MIN0(IXMAX, INT(XC(ISTAR)+SKYOUT)))
            LY=MAX0(IYMIN, MIN0(IYMAX, INT(YC(ISTAR)-SKYOUT)+1))
            MY=MAX0(IYMIN, MIN0(IYMAX, INT(YC(ISTAR)+SKYOUT)))
            N = 0
c           nbad = 0
            DO 2196 J=LY,MY
               DYSQ = (REAL(J) - YC(ISTAR))**2
               DO 2194 I = LX,MX
                  DX = REAL(I) - XC(ISTAR)
                  RSQ = DX**2 + DYSQ
                  IF (RSQ .GT. SKYOSQ) THEN
                     IF (DX .GT. 0.) THEN
                        GO TO 2196
                     ELSE
                        GO TO 2194
                     END IF
                  END IF
C
                  IF (RSQ .GE. SKYISQ) THEN
                     IF (SIGMA(I,J) .GT. VAL__MINR) THEN
                        N = N+1
                        NUMER(N) = SUBT(I,J)
                        IF (N .GE. MAXSTR) GO TO 2197
c                       else
c                          nbad = nbad+1
                     END IF
                  END IF
C
 2194          CONTINUE
 2196       CONTINUE
C
 2197       IF (N .GT. MINSKY) THEN
               CALL QUICK (NUMER, N, DENOM)
               J = NINT(0.2*N)
               DX = 0.
               DO I=(N+1)/2-J,(N/2)+1+J
                  DX = DX + NUMER(I)
               END DO
               SKY(ISTAR) = DX/REAL((N/2)+2*J+2-(N+1)/2)
            END IF
 2198    CONTINUE
      END IF
C
C Create provisional star groups.
C
      CALL REGRP (ID, XC, YC, MAG, SKY, SUMWT, DXOLD, DYOLD,
     .     XCLAMP, YCLAMP, MAXSTR, NSTAR, FITRAD, LAST, NUMER,
     .     DENOM)
C
C Now get ready to do the next iteration, group by group.
C
      IF (WATCH .GT. 0.5) THEN
C
C Set up reporting intervals for star counter.
C
         D=AMAX1(0.1, ALOG10(FLOAT(NSTAR)/10.))
         INTRVL=INT(D)
         D=D-FLOAT(INTRVL)
         IF (D .GT. 0.8) THEN
            INTRVL=10**(INTRVL+1)
         ELSE IF (D .GT. 0.5) THEN
            INTRVL=5*10**INTRVL
         ELSE IF (D .GT. 0.2) THEN
            INTRVL=2*10**INTRVL
         ELSE
            INTRVL=10**INTRVL
         END IF
      END IF
      RADIUS=FITRAD
      ISTAR=1
 2200 CONTINUE
C
C Find the last star in the current group.
C
      DO LSTAR = ISTAR,NSTAR
         IF (LAST(LSTAR)) GO TO 2210
      END DO
      LSTAR=NSTAR
 2210 NSTR=LSTAR-ISTAR+1
C
C Start crunching on this group.
C
      IF (NSTR .GT. MAXGRP) THEN
         IF (WATCH .GT. 1.5) THEN
            WRITE (LINE,61) NSTR, RADIUS
   61       FORMAT ('Group too large:', I5, '  (', F5.2, ')')
            CALL OVRWRT (LINE(1:30), 3)
         END IF
         RADIUS=0.95*RADIUS
         IF (CENTER .AND. (NITER .GE. 2)) THEN
            IF (RADIUS .LT. 1.2) THEN
               IF (WATCH .GT. 1.5) THEN
                  WRITE (LINE,62)
   62             FORMAT ('Group too dense to reduce.')
                  CALL OVRWRT (LINE(1:26), 3)
               END IF
C
C Mark the faintest star in this group for deletion.
C
               FAINT=100.
               DO I=ISTAR,LSTAR
                  IF (MAG(I) .LT. FAINT) THEN
                     FAINT=MAG(I)
                     L=I
                  END IF
               END DO
               IF (WATCH .GT. 1.5) THEN
                  WRITE (6,63) ID(L), XC(L), YC(L),
     .                 PSFMAG-1.085736*ALOG(MAG(L)), ' group too dense'
   63             FORMAT (1X, I5, 2F9.3, F9.3, 9X, A, F6.3)
               END IF
               SKIP(L)=.TRUE.                        ! Flag for deletion
               NDISAP=NDISAP+1
               GO TO 3000
            END IF
         ELSE
            IF (RADIUS .LT. 0.8) THEN
               IF (WATCH .GT. 1.5) THEN
                  WRITE (LINE,62)
                  CALL OVRWRT (LINE(1:26), 3)
               END IF
C
C Mark the faintest star in this group for deletion.
C
               FAINT=100.
               DO I=ISTAR,LSTAR
                  IF (MAG(I) .LT. FAINT) THEN
                     FAINT=MAG(I)
                     L=I
                  END IF
               END DO
               IF (WATCH .GT. 1.5) WRITE (6,63) ID(L), XC(L), YC(L),
     .              PSFMAG-1.085736*ALOG(MAG(L)), ' group too dense'
               SKIP(L)=.TRUE.                        ! Flag for deletion
               NDISAP=NDISAP+1
               GO TO 3000
            END IF
         END IF
         L = NSTR
         CALL REGRP (ID(ISTAR), XC(ISTAR), YC(ISTAR), MAG(ISTAR),
     .        SKY(ISTAR), SUMWT(ISTAR), DXOLD(ISTAR), DYOLD(ISTAR),
     .        XCLAMP(ISTAR), YCLAMP(ISTAR), L, NSTR, RADIUS,
     .        LAST(ISTAR), NUMER(ISTAR), DENOM(ISTAR))
         GO TO 2200
      END IF
C
      RADIUS=FITRAD
      XMIN=NCOL+FITRAD
      XMAX=-FITRAD
      YMIN=NROW+FITRAD
      YMAX=-FITRAD
C
C Get the extent of the rectangular area of the frame relevant to this
C group.  At the same time, determine the average SKY and the average
C CHI value for the group, and zero the various statistical
C accumulators.  What we would REALLY like to do is to compute the
C anticipated error for each pixel the same way as PEAK and NSTAR do.
C We can't really do that, because stellar groups aren't defined the
C same way as in NSTAR: here they change as stars move from one group
C to another, or converge and are subtracted from the frame.  Therefore,
C there is no consistent way to define a group's CHI value the same as
C was done in NSTAR.  I will take the cheap way out:  using the
C individual smoothed CHI values for the stars which were obtained
C during the previous iteration (which should still be stored in the
C array SUMWT) I will simply average those values and call the mean the
C group's effective smoothed CHI.  This way, at least, the reductions
C will be the same as before for isolated stars.  (Note that,
C for the first iteration, the SUMWT's have been set to 1.0 by a DATA
C statement above.)
C
      SKYBAR = 0.
      CHIGRP = 0.
      DO I=ISTAR,LSTAR
         XMIN = AMIN1(XMIN, XC(I))
         XMAX = AMAX1(XMAX, XC(I))
         YMIN = AMIN1(YMIN, YC(I))
         YMAX = AMAX1(YMAX, YC(I))
         SKYBAR = SKYBAR+SKY(I)
         CHIGRP = CHIGRP+SUMWT(I)
         CHI(I) = 0.
         SUMWT(I) = 0.
         NUMER(I) = 0.
         DENOM(I) = 0.
         NPIX(I) = 0
      END DO
      SKYBAR = SKYBAR/FLOAT(NSTR)
      CHIGRP = CHIGRP/FLOAT(NSTR)
      IF (CENTER .AND. (NITER .GE. 2)) THEN
         NTERM=3*NSTR
      ELSE
         NTERM=NSTR
      END IF
C
C If sky is to be determined: NTERM=NTERM+1
C
C Now... on with the iteration.
C
      IXMIN=MIN0(NCOL, MAX0(1, INT(XMIN-FITRAD)+1))
      IXMAX=MIN0(NCOL, MAX0(1, INT(XMAX+FITRAD)))
      IYMIN=MIN0(NROW, MAX0(1, INT(YMIN-FITRAD)+1))
      IYMAX=MIN0(NROW, MAX0(1, INT(YMAX+FITRAD)))
C
C IXMIN, IXMAX, IYMIN, and IYMAX are now the limits of a rectangular
C array containing all pixels within one fitting radius of any star in
C the group.
C
C Zero the normal matrix and the vector of residuals.
C
      DO 2270 J=1,NTERM
      V(J)=0.0
      DO 2270 I=J,NTERM
 2270 C(I,J)=0.0
      SUMRES=0.
      GRPWT=0.
C
C Now deal with the pixels one by one.
C
      DO 2390 IY=IYMIN,IYMAX
      DO 2380 IX=IXMIN,IXMAX
      IF (SIGMA(IX,IY) .LE. 0.) GO TO 2380
C
C If this pixel is within one fitting radius of at least one star in
C the current group, include it in the calculation.  Otherwise, skip
C it.  While figuring this out, compute the squared distance of this
C pixel from the centroid of each star in the group.
C
      OMIT=.TRUE.
      DO I=ISTAR,LSTAR
         RPIXSQ(I)=(FLOAT(IX)-XC(I))**2+(FLOAT(IY)-YC(I))**2
         IF (RPIXSQ(I) .LT. RADSQ) THEN
            OMIT=.FALSE.
         END IF
      END DO
      IF (OMIT) GO TO 2380                      ! Do not need this pixel
C
      DO I=ISTAR,LSTAR
         IF (RPIXSQ(I) .LT. RADSQ) THEN
            SKIP(I)=.FALSE.
         ELSE
            SKIP(I)=.TRUE.
         END IF
      END DO
C
C The expected random error in the pixel is the quadratic sum of
C the Poisson statistics, plus the readout noise, plus an estimated
C error of 0.75% of the total brightness for the difficulty of flat-
C fielding and bias-correcting the chip, plus an estimated error of
C some fraction of the fourth derivative at the peak of the profile,
C to account for the difficulty of accurately interpolating within the
C point-spread function.  The fourth derivative of the PSF is
C proportional to H/sigma**4 (sigma is the Gaussian width parameter for
C the stellar core); using the geometric mean of sigma(x) and sigma(y),
C this becomes H/[sigma(x)*sigma(y)]**2.  The ratio of the fitting
C error to this quantity is estimated from a good-seeing CTIO frame to
C be approximately 0.027 (see definition of PKERR above.)
C
      D=SUBT(IX,IY)-SKYBAR                      ! Residual of this pixel
      DPOS=AMAX1(0., DATA(IX,IY)-D)
      IF ((DPOS .GT. HIBAD) .AND. (NITER .GE. 4)) GO TO 2380
C
C    DPOS = raw data minus residual
C         = model-predicted brightness in this pixel, consisting of sky
C           plus all stellar profiles, which presumably is non-negative.
C
C The four error sources in our noise model are:
C     (1) Readout noise
C     (2) Poisson noise
C     (3) Flat-field errors
C     (4) Errors in the PSF
C
C Numerically, the squares of these quantities are
C     (1) RONOIS = SIGMA(IX,IY)   (initially, at least)
C     (2) DPOS/PHPADU where DPOS = sum of the stellar profiles plus sky
C     (3) constant x DPOS**2
C     (4) constant x the sum of (stellar profile)**2
C
C Here's the thing:  at this point we don't have the
C sum of (stellar profile)**2 available to us.  All we have is
C (sum of stellar profile)**2, which isn't the same thing at all.
C Nevertheless, we have to assume that
C
C     sum of (stellar profile)**2 = (sum of stellar profile)**2;
C
C otherwise I'd have to use a whole new REAL array as big as the
C original CCD image, and I don't want to do that.  This should be
C good enough for pixels where either (a) the other three error sources
C dominate, .OR. (b) one of the stars putting light into that pixel is
C much brighter than all the rest.  Thus, the approximation should be
C good UNLESS the pixel is near the center of two or more bright
C stars --- in which case things will be hairy anyway.
C
      SIGSQ = SIGMA(IX,IY) + DPOS/PHPADU + (PERERR*DPOS)**2 +
     .     (PKERR*(DPOS-SKYBAR))**2
      RELERR=ABS(D)/SQRT(SIGSQ)
      IF (CLIP .AND. (RELERR .GT. 100.)) GO TO 2380
      WT=0.
C
C Now include this pixel in the fitting equations for the group.
C
      DO 2320 I=ISTAR,LSTAR
      IF (SKIP(I)) GO TO 2320
      RSQ=RPIXSQ(I)/RADSQ
      IF (RSQ .GE. 0.999999) GO TO 2320  ! Safety check vs divide by 0
      WT=AMAX1(WT, 5./(5.+RSQ/(1.-RSQ)))
C
C The condition equation for pixel (IX,IY) is of the form
C
C data(IX,IY)-summation{scale*psf(IX-Xcenter,IY-Ycenter)}-sky=residual
C
C Then we will jigger the scale's, Xcenter's, and Ycenter's such that
C
C                Summation{weight * residual**2}
C
C is minimized.  'weight' will be a function (1) of the distance of this
C pixel from the center of the nearest star, (2) of the model-predicted
C brightness of the pixel (taking into consideration the readout noise,
C the photons/ADU, and the interpolation error of the PSF), and (3) of
C the size of the residual itself.  (1) is necessary to prevent the
C non-linear least-squares solution from oscillating:  oft-times it will
C come to pass that if you include a pixel in the solution, then the
C predicted shift of the centroid will cause that pixel to be excluded
C in the next iteration, and the new predicted shift of the centroid
C will cause that pixel to be included again.  This could go on ad
C infinitum.  The cure is to have the weight of a pixel go
C continuously to zero as its distance from the stellar centroid
C approaches the fitting radius.  In a case like that just described,
C the solution can then find a real minimum of the sum of the
C weighted squared residuals with that pixel at some low-weight position
C just inside the fitting radius.  (2) is just sensible weighting.
C (3) is just a crude attempt at making the solution more robust against
C bad pixels.
C
      VAL = USEPSF(IPSTYP, FLOAT(IX)-XC(I), FLOAT(IY)-YC(I), BRIGHT,
     .     PAR, PSF, NPSF, NPAR, NEXP, NFRAC, (XC(I)-1.)/XPSF-1.,
     .     (YC(I)-1.)/YPSF-1., DVDXC, DVDYC)
      IF (NTERM .GT. NSTR) THEN
         I3=(I-ISTAR+1)*3
         K=I3-2
         X(K)=-VAL
         K=I3-1
         X(K)=-MAG(I)*DVDXC
         X(I3)=-MAG(I)*DVDYC
      ELSE
         K=I-ISTAR+1
         X(K)=-VAL
      END IF
      RHOSQ=((XC(I)-FLOAT(IX))/PAR(1))**2+
     .     ((YC(I)-FLOAT(IY))/PAR(2))**2
      IF (RHOSQ .LE. 36.) THEN
         RHOSQ = 0.6931472*RHOSQ
         DFDSIG = EXP(-RHOSQ)*(RHOSQ-1.)
         NUMER(I) = NUMER(I)+DFDSIG*D/SIGSQ
         DENOM(I) = DENOM(I)+DFDSIG**2/SIGSQ
      END IF
 2320 CONTINUE
C
C At this point, the vector X contains the first derivative of
C the condition equation for pixel (IX,IY) with respect to each of
C the fitting parameters for all of the stars. Now these derivatives
C will be added into the normal matrix and the vector of residuals.
C
C Add this residual into the weighted sum of the absolute relative
C residuals.
C
      DWT=WT*RELERR
      SUMRES=SUMRES+DWT
      GRPWT=GRPWT+WT
C
C SUMRES is the weighted sum of [ABS(residual)/sigma] for all the
C pixels in the group.  Now also add the weighted value of
C [ABS(residual)/sigma] into the accumulating sum for each of the
C stars.
C
      DO 2330 I=ISTAR,LSTAR
      IF (SKIP(I)) GO TO 2330
      CHI(I)=CHI(I)+DWT
      SUMWT(I)=SUMWT(I)+WT
      NPIX(I)=NPIX(I)+1
 2330 CONTINUE
C
C Up until now, WT represents only the radial weighting profile.  Now
C figure in the anticipated standard error of the pixel.  Reject any
C pixel with a 100-sigma residual after iteration 2.
C
      WT=WT/SIGSQ
      IF (CLIP) WT=WT/(1.+(RELERR/(CHIGRP*HALF))**IEXP)
C
C Now work this pixel into the normal matrix.
C
      DWT=D*WT
C     If sky is to be determined: C(NTERM,NTERM)=C(NTERM,NTERM)+WT
C     If sky is to be determined: V(NTERM)=V(NTERM)-DWT
      DO 2370 I=ISTAR,LSTAR
      IF (SKIP(I)) GO TO 2370
      IF (NTERM .GT. NSTR) THEN
         I3=(I-ISTAR+1)*3
         I3M2=I3-2
         DO 2340 K=I3M2,I3
C        If sky is to be determined: C(NTERM,K)=C(NTERM,K)-X(K)*WT
 2340    V(K)=V(K)+X(K)*DWT
         DO 2360 J=ISTAR,I
         IF (SKIP(J)) GO TO 2360
         J3=(J-ISTAR+1)*3
         DO 2350 K=I3M2,I3
         XKWT=X(K)*WT
         DO 2350 L=J3-2,MIN0(K, J3)
 2350    C(K,L)=C(K,L)+X(L)*XKWT
 2360    CONTINUE
      ELSE
         K=I-ISTAR+1
         V(K)=V(K)+X(K)*DWT
         XKWT=X(K)*WT
C        If sky is to be determined: C(NTERM,K)=C(NTERM,K)-XKWT
         DO 2365 J=ISTAR,I
         IF (SKIP(J)) GO TO 2365
         L=J-ISTAR+1
         C(K,L)=C(K,L)+X(L)*XKWT
 2365    CONTINUE
      END IF
 2370 CONTINUE
C
 2380 CONTINUE
 2390 CONTINUE
C
C Reflect the normal matrix across the diagonal.
C
      IF (NTERM .GT. 1) THEN
         DO 2410 L=2,NTERM
         DO 2410 K=1,L-1
 2410    C(K,L)=C(L,K)
      END IF
C
C Compute the estimate of the standard deviation of the residuals for
C the group as a whole, and for each star.  This estimate starts out as
C SQRT(PI/2)*{SUM[weight*ABS(residual/sigma)]/SUM(weight)} and then gets
C corrected for bias by SQRT(no. of pixels/(no. of pixels - degrees of
C freedom)).
C
      IF (GRPWT .GT. NTERM) THEN
         CHIGRP=1.2533141*SUMRES/SQRT(GRPWT*(GRPWT-NTERM))
C
C But then I drive the value toward unity, depending on exactly how
C many pixels were involved:  if CHI is based on exactly a total
C weight of 3, then it is extremely poorly determined, and we just
C want to keep CHI = 1.  The larger GRPWT is, the better determined
C CHI is, and the less we want to force it toward unity.  So,
C just take the weighted average of CHI and unity, with weights
C GRPWT-3 and 1, respectively.
C
         CHIGRP=((GRPWT-3.)*CHIGRP+3.)/GRPWT
      ELSE
         CHIGRP=1.
      END IF
C
C CHIGRP has been pulled toward its expected value of unity to keep the
C statistics of a small number of pixels from compeletely dominating
C the error analysis.  Similarly, the photometric errors for the
C individual stars will be pulled toward unity now.  Later on, if the
C number of stars in the group is greater than one, CHI(I) will be
C nudged toward the group average.  In order to work optimally, of
C course, this requires that PHPADU, RONOIS, and the other noise
C contributors which I have postulated properly represent the true
C errors expected in each pixel.
C
C At the same time, be sure that every star in the group contains at
C least 3 valid pixels if recentroiding is being performed, 1 valid
C pixel if not.  If any star in the group fails to meet this criterion
C mark that star for deletion, set REDO to true, and skip ahead to the
C next group.
C
      REDO=.FALSE.
      DO I=ISTAR,LSTAR
         IF (CENTER .AND. (NITER .GE. 2)) THEN
            IF (NPIX(I) .LT. 3) THEN
               REDO=.TRUE.
               IF (WATCH .GT. 1.5) WRITE (6,63) ID(I), XC(I), YC(I),
     .              PSFMAG-1.085736*ALOG(MAG(I)),
     .              ' too few valid pixels'
               SKIP(I)=.TRUE.                    ! Flag for deletion
               NDISAP=NDISAP+1
            ELSE
               SKIP(I)=.FALSE.                   ! Flag for retention
               I3=(I-ISTAR+1)*3-2
               IF (SUMWT(I) .GT. 3.) THEN
                  CHI(I)=1.2533141*CHI(I)/SQRT(SUMWT(I)*(SUMWT(I)-3.))
C
C Store a smoothed CHI value for the star in SUMWT.
C
                  SUMWT(I) = ((SUMWT(I)-3.)*CHI(I) + 3.)/SUMWT(I)
               ELSE
                  CHI(I)=CHIGRP
               END IF
            END IF
         ELSE
            IF (NPIX(I) .LT. 1) THEN
               REDO=.TRUE.
               SKIP(I)=.TRUE.                    ! Flag for deletion
               NDISAP=NDISAP+1
               IF (WATCH .GT. 1.5) WRITE (6,63) ID(I), XC(I), YC(I),
     .              PSFMAG-1.085736*ALOG(MAG(I)),
     .              ' too few valid pixels'
            ELSE
               SKIP(I)=.FALSE.                   ! Flag for retention
               I3=I-ISTAR+1
               IF (SUMWT(I) .GT. 1.) THEN
                  CHI(I)=1.2533141*CHI(I)/SQRT(SUMWT(I)*(SUMWT(I)-1.))
C
C Store a smoothed CHI value for the star in SUMWT.
C
                  SUMWT(I) = ((SUMWT(I)-3.)*CHI(I) + 3.)/SUMWT(I)
               ELSE
                  CHI(I)=CHIGRP
               END IF
            END IF
         END IF
      END DO
      IF (REDO) THEN
         GO TO 3000
      END IF
      CALL INVERS (C, MAXUNK, NTERM, ISTAT)
      DO J=1,NTERM
         IF (C(J,J) .LE. 0.) THEN
C
C Uh-oh.  Zero on the diagonal. Booma Booma!
C
            IF (CENTER .AND. (NITER .GE. 2)) THEN
               I = (J+2)/3
            ELSE
               I = J
            END IF
C
C I now points at the (first, at least) star that caused the problem.
C
            SKIP(I) = .TRUE.
            NDISAP=NDISAP+1
            IF (WATCH .GT. 1.5) WRITE (6,63) ID(I), XC(I), YC(I),
     .           PSFMAG-1.085736*ALOG(MAG(I)), ' singular matrix'
            GO TO 3000
         END IF
      END DO
      CALL VMUL (C, MAXUNK, NTERM, V, X)
C If sky is to be determined: SKYBAR=SKYBAR-X(NTERM)
C If sky is to be determined: IF(ABS(X(NTERM)).GT.0.01)REDO=.TRUE.
C
C In the beginning, the brightness of each star will be permitted to
C change by no more than two magnitudes per iteration, and the x,y
C coordinates of each centroid will be permitted to change by no more
C than 0.4 pixel per iteration.  Any time that the parameter
C correction changes sign from one iteration to the next, the maximum
C permissible change will be reduced by a factor of two.  These
C clamps are released any time a star in the group disappears.
C
      DO 2520 I=ISTAR,LSTAR
         IF (CENTER .AND. (NITER .GE. 2)) THEN
C
C Correct both magnitude and position.
C
            L=3*(I-ISTAR+1)
            K=L-1
            J=L-2
C
C Note that the sign of the correction is such that it must be
C SUBTRACTED from the current value of the parameter to get the
C improved parameter value.  This being the case, if the correction
C to the brightness is negative (the least-squares thinks that the
C star should be brighter) a change of 1 magnitude is a change of a
C factor of 2.5; if the brightness correction is positive (the star
C should be fainter) a change of 1 magnitude is a change of 60%.
C
            DWT=DXOLD(I)*X(K)
            IF (DWT .LT. 0.) THEN
               XCLAMP(I)=AMAX1(0.001,0.5*XCLAMP(I))
            ELSE
               XCLAMP(I)=AMIN1(CLMPMX,1.2*XCLAMP(I))
            END IF
            XC(I)=XC(I)-X(K)/(1.+ABS(X(K)/XCLAMP(I)))
            DXOLD(I)=X(K)
            DWT=DYOLD(I)*X(L)
            IF (DWT .LT. 0.) THEN
               YCLAMP(I)=AMAX1(0.001,0.5*YCLAMP(I))
            ELSE
               YCLAMP(I)=AMIN1(CLMPMX,1.2*YCLAMP(I))
            END IF
            YC(I)=YC(I)-X(L)/(1.+ABS(X(L)/YCLAMP(I)))
            DYOLD(I)=X(L)
            MAG(I)=MAG(I)-X(J)/
     .           (1.+AMAX1( X(J)/(0.84*MAG(I)) , -X(J)/(5.25*MAG(I)) ))
            MAGERR(I) = SUMWT(I)*SQRT(C(J,J))
            IF (NITER .GE. 4) THEN
               REDO = .FALSE.
               IF (ABS(X(J)) .GT.
     .              AMAX1( 0.1*MAGERR(I) , 0.0005*MAG(I) )) THEN
                  REDO=.TRUE.
               ELSE
                  DF = (0.1*SUMWT(I))**2
                  IF (X(K)**2 .GT. AMAX1(DF*C(K,K), 4.E-6)) THEN
                     REDO=.TRUE.
                  ELSE IF (X(L)**2 .GT. AMAX1(DF*C(L,L), 4.E-6)) THEN
                     REDO=.TRUE.
                  END IF
               END IF
            ELSE
               REDO = .TRUE.
            END IF
         ELSE
            J=I-ISTAR+1
C
C Correct magnitude only. Since this is an easy, linear problem,
C elaborate clamps are not needed.  A simple one will do to keep
C the brightness from ever going negative.
C
            MAG(I)=MAG(I)-X(J)/(1. + 1.2*ABS( X(J)/MAG(I) ))
            MAGERR(I) = SUMWT(I)*SQRT(C(J,J))
            IF (NITER .GE. 2) THEN
               REDO=.FALSE.
               IF (ABS(X(J)) .GT.
     .              AMAX1( 0.1*MAGERR(I), 0.0005*MAG(I) )) REDO = .TRUE.
            ELSE
               REDO=.TRUE.
            END IF
         END IF
C
C No star will be allowed to converge with a signal-to-noise ratio
C worse than 2.0.  Such stars must be retained until either they get
C better than S/N = 2, or they get eliminated some time after
C iteration 5 (if S/N < 1.0), 10 (if S/N < 1.5), or 15 (if S/N < 2.0).
C However, anything which is lucky enough to survive until the last
C iteration will be written out regardless.
C
         IF (MAG(I) .LT. 2.0*MAGERR(I)) REDO = .TRUE.
         IF (NITER .GE. MAXIT) REDO=.FALSE.
C
C If this star converged, write out the results for it,
C flag it for deletion from the star list (SKIP = .TRUE.), and
C subtract it from the reference copy of the image (DATA).
C
         IF (.NOT. REDO) THEN
            NCONV=NCONV+1
C
C Subtract from the reference copy of the image (DATA).
C
            LX=MAX0(0, MIN0(NCOL, INT(XC(I)-PSFRAD)))+1
            MX=MAX0(1, MIN0(NCOL, INT(XC(I)+PSFRAD)))
            LY=MAX0(0, MIN0(NROW, INT(YC(I)-PSFRAD)))+1
            MY=MAX0(1, MIN0(NROW, INT(YC(I)+PSFRAD)))
            DELTAX=(XC(I)-1.)/XPSF-1.
            DELTAY=(YC(I)-1.)/YPSF-1.
            DO 2195 J=LY,MY
               DY=FLOAT(J)-YC(I)
               DYSQ=DY**2
               DO 2195 K=LX,MX
                  IF (SIGMA(K,J) .GT. VAL__MINR) THEN
                     DX=FLOAT(K)-XC(I)
                     IF (DX**2+DYSQ .GE. PSFRSQ) GO TO 2195
                     DIFF=MAG(I)*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR,
     .                    PSF, NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY,
     .                    DVDXC, DVDYC)
                     DATA(K,J)=DATA(K,J)-DIFF
C
C We must add this star's contribution to the pixel's noise into the
C noise map SIGMA (which, of course, actually contains sigma**2).
C Please note that of the four contributors to the variance
C
C     (1) RONOIS = SIGMA(IX,IY)   (initially, at least)
C     (2) DPOS/PHPADU where DPOS = sum of the stellar profiles plus sky
C     (3) constant x DPOS**2
C     (4) constant x the sum of (the stellar profiles)**2
C
C (1) is already included in SIGMA, and we can calculate this star's
C contribution to (2) and (4) directly.  (3) is tricker.  (3) is
C (sum of sky plus all stellar profiles)**2, and we need to determine
C the contribution of (this stellar profile).  We need to use algebra:
C
C (sum of sky and all OTHER stellar profiles + this stellar profile)**2
C
C = (sum of sky plus all OTHER stellar profiles)**2 +
C  2 x (sum of sky plus OTHER stellar profiles) x (this stellar profile)
C   + (this stellar profile)**2.
C
C So we must add
C
C 2 x (sum of sky plus OTHER stellar profiles) x (this stellar profile)
C + (this stellar profile)**2
C
C into SIGMA.  That way, later on when the solution has forgotten that
C this star ever existed, so it only adds
C
C (sum of sky plus all OTHER stellar profiles)**2
C
C into the noise expected for the pixel, it will get the noise right.
C Since we have already subtracted this star's contribution to the image
C (array DATA) above, I will use the current DATA as my working
C approximation for (sum of sky plus all OTHER stellar profiles).
C I really shouldn't do this; I should really be calculating each star's
C model-predicted contribution to this pixel, but that would take too
C long.
C
                     IF (DIFF .GT. 0.) THEN
                       DIFF = DIFF/PHPADU +
     .                      PERERR*2.*AMAX1(0., DATA(K,J))*DIFF
     .                      + (PKERR*DIFF)**2
                       SIGMA(K,J)=SIGN(ABS(SIGMA(K,J))+DIFF, SIGMA(K,J))
                     END IF
                  END IF
 2195       CONTINUE                          ! End of loops over pixels
            SHARP=1.4427*PAR(1)*PAR(2)*NUMER(I)/(MAG(I)*PEAK*DENOM(I))
            SHARP=AMIN1( 99.999, AMAX1( SHARP, -99.999 ))
            ERR=1.085736*MAGERR(I)/MAG(I)
            MAG(I)=PSFMAG-1.085736*ALOG(MAG(I))
            WRITE (1,321) ID(I), XC(I), YC(I), MAG(I), ERR, SKY(I),
     .                                    FLOAT(NITER), CHI(I), SHARP
  321       FORMAT (I6, 5F9.3, F9.0, F9.2, F9.3)
            SKIP(I)=.TRUE.                    ! Remove from star list
         END IF
 2520 CONTINUE
C
C If there is more than one star remaining in this group, check to see
C whether any two of them have merged.  This means, find the closest
C pair and see whether they are TOO close.
C
      K = 0
      RSQ = SEPCRT
      IF (NSTR .GT. 1) THEN
         DO 2230 I=ISTAR+1,LSTAR
         IF (SKIP(I)) GO TO 2230
C
         DO 2220 J=ISTAR,I-1
            IF (SKIP(J)) GO TO 2220
            SEP=(XC(I)-XC(J))**2+(YC(I)-YC(J))**2
            IF (SEP .GE. RSQ) GO TO 2220
C
C Two stars are overlapping.  Identify the fainter of the two.
C
            RSQ = SEP
            IF (MAG(I) .LT. MAG(J)) THEN
               K = I
               L = J
            ELSE
               K = J
               L = I
            END IF
 2220    CONTINUE
C
 2230    CONTINUE
C
C No two stars have merged if K still equals zero.
C
         IF (K .LE. 0) GO TO 2260
C
C The K-th star is now the fainter of the two, the L-th, the brighter.
C
C Now eliminate the fainter of the two if they are TOO close.
C
         IF ((RSQ .GT. SEPMIN) .AND.
     .        (MAG(K) .GT. WCRIT*MAGERR(K))) GO TO 2260
C
C Now replace the centroid of the L-th star with the weighted mean of
C the most recent estimates of the centroids of the L-th and K-th
C stars, and the brightness of the L-th with the sum of the brightnesses
C of the L-th and K-th.
C
         XC(L)=XC(L)*MAG(L)+XC(K)*MAG(K)
         YC(L)=YC(L)*MAG(L)+YC(K)*MAG(K)
         MAG(L)=MAG(L)+MAG(K)
         XC(L)=XC(L)/MAG(L)
         YC(L)=YC(L)/MAG(L)
C
C Remove the K-th star from the group.
C
         SKIP(K)=.TRUE.                       ! Remove from star list
         IF (WATCH .GT. 1.5) THEN
            WRITE (6,63) ID(K), XC(K), YC(K),
     .           PSFMAG-1.085736*ALOG(MAG(K)), ' blended with'
            WRITE (6,63) ID(L), XC(L), YC(L),
     .           PSFMAG-1.085736*ALOG(MAG(L))
         END IF
         NDISAP=NDISAP+1
C
C Loosen the clamps of every star in the group.
C
         DO I=ISTAR,LSTAR
            DXOLD(I)=0.
            DYOLD(I)=0.
            XCLAMP(I)=AMAX1(0.5*CLMPMX, XCLAMP(I))
            YCLAMP(I)=AMAX1(0.5*CLMPMX, YCLAMP(I))
         END DO
      END IF
C
C If the number of iterations completed is less than or equal to 3,
C perform another iteration no questions asked.
C
 2260 IF (NITER .LE. 3) GO TO 3000
C
C > IF NO STAR HAS BEEN REMOVED FROM THIS GROUP DURING THIS ITERATION <
C
C Check whether any of the stars is too faint (more than 12.5
C magnitudes fainter than the PSF star).  If several stars are
C too faint, delete the faintest one, and set the brightnesses of
C the other faint ones exactly to 12.5 mag below the PSF star.
C That way on the next iteration we will see whether these stars
C want to grow or to disappear.
C
      FAINT=1.0E-5
      IFAINT=0
C
      DO I=ISTAR,LSTAR
         IF (SKIP(I)) GO TO 3000
         IF (MAG(I) .LT. FAINT) THEN
            FAINT=MAG(I)
            IFAINT=I
         END IF
         IF (MAG(I) .LT. 1.E-5) MAG(I)=1.E-5
      END DO
C
C If at least one star is more than 12.5 mag. fainter than the
C PSF, then  IFAINT  is the index of the faintest of them, and FAINT
C is the relative brightness of the faintest of them.
C
      IF (IFAINT .GT. 0) THEN
         SKIP(IFAINT)=.TRUE.                     ! Remove from star list
         NDISAP=NDISAP+1
         IF (WATCH .GT. 1.5) WRITE (6,63) ID(IFAINT), XC(IFAINT),
     .           YC(IFAINT), PSFMAG-1.085736*ALOG(MAG(IFAINT)),
     .           ' too faint'
C
C Loosen the clamps of every star in the group.
C
         DO I=ISTAR,LSTAR
            DXOLD(I)=0.
            DYOLD(I)=0.
            XCLAMP(I)=AMAX1(0.5*CLMPMX, XCLAMP(I))
            YCLAMP(I)=AMAX1(0.5*CLMPMX, YCLAMP(I))
         END DO
      ELSE IF (NITER .GE. 5) THEN
C
C If no star in this group is more than 12.5 mag. fainter than the PSF,
C then after the fifth iteration delete the least certain star if it
C is less than a one-sigma detection; after the tenth iteration delete
C the least certain star if it is less than a 1.5-sigma detection;
C after the fifteenth iteration delete the least certain star if it is
C less than a two-sigma detection.
C
         FAINT=VAL__MAXR
         IFAINT=0
C
         DO 2550 I=ISTAR,LSTAR
            WT=MAG(I)/MAGERR(I)
            IF (WT .LT. FAINT) THEN
               FAINT=WT
               IFAINT=I
            END IF
 2550    CONTINUE
C
         IF (FAINT .LT. WCRIT) THEN
            SKIP(IFAINT)=.TRUE.                  ! Remove from star list
            IF (WATCH .GT. 1.5) WRITE (6,63) ID(IFAINT), XC(IFAINT),
     .              YC(IFAINT), PSFMAG-1.085736*ALOG(MAG(IFAINT)),
     .              ' error too big: ',
     .              1.085736*MAGERR(IFAINT)/MAG(IFAINT)
            NDISAP=NDISAP+1
C
C Loosen the clamps of every star in the group.
C
            DO I=ISTAR,LSTAR
               DXOLD(I)=0.
               DYOLD(I)=0.
               XCLAMP(I)=AMAX1(0.5*CLMPMX, XCLAMP(I))
               YCLAMP(I)=AMAX1(0.5*CLMPMX, YCLAMP(I))
            END DO
         END IF
      END IF
 3000 CONTINUE
      IF (WATCH .GT. 0.5) THEN
         IF (LSTAR/INTRVL .GT. (ISTAR-1)/INTRVL) THEN
            WRITE (LINE,682) NITER, INTRVL*(LSTAR/INTRVL),
     .           NDISAP, NCONV
            CALL OVRWRT (LINE(1:24), 2)
         END IF
      END IF
      ISTAR=LSTAR+1
      IF (ISTAR .LE. NSTAR) GO TO 2200           ! Go on to next group
C
C We've gone through the entire star list for this iteration.
C
C Find the last star in the list that still needs more work
C (SKIP=.FALSE.).  If there aren't any, then we're done!  If there is
C one, point NSTAR at it.
C
      ISTAR=0
 3005 CONTINUE
      IF (SKIP(NSTAR)) THEN
         NSTAR=NSTAR-1
         IF (NSTAR .GT. 0) THEN
            GO TO 3005
         ELSE
            WRITE (LINE,682) NITER, NSTAR, NDISAP, NCONV
            CALL OVRWRT (LINE(1:28), 3)
            GO TO 9000                       ! None left!! We can go now
         END IF
      END IF
C
C Well, at this point we've established that there are at least some
C stars in the list that still need more work.  Remove any stars with
C which we are finished (SKIP = .TRUE.) by overwriting the first such
C star with the star at position NSTAR in the list (we have just
C confirmed that this is the last star in the list that still needs
C work).  Then decrement NSTAR and go back up to 3005 to find the new
C last star in the list that still needs work.
C
 3010 ISTAR=ISTAR+1
      IF (ISTAR .GE. NSTAR) THEN
         WRITE (LINE,682) NITER, NSTAR, NDISAP, NCONV
  682    FORMAT (4I6, '  <<')
         CALL OVRWRT (LINE(1:28), 3)
         GO TO 2100        ! The list is ready.  Start another iteration
      END IF
C
      IF (SKIP(ISTAR)) THEN
         ID(ISTAR)=ID(NSTAR)
         XC(ISTAR)=XC(NSTAR)
         YC(ISTAR)=YC(NSTAR)
         MAG(ISTAR)=MAG(NSTAR)
         SKY(ISTAR)=SKY(NSTAR)
         SUMWT(ISTAR)=SUMWT(NSTAR)
         DXOLD(ISTAR)=DXOLD(NSTAR)
         DYOLD(ISTAR)=DYOLD(NSTAR)
         XCLAMP(ISTAR)=XCLAMP(NSTAR)
         YCLAMP(ISTAR)=YCLAMP(NSTAR)
         SKIP(ISTAR)=.FALSE.
         NSTAR=NSTAR-1
         GO TO 3005
      ELSE
         GO TO 3010
      END IF
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CONTINUE
      CALL CLFILE (1)
C
C Copy the input picture verbatim into the output picture.
C
      IF (SUBPIC(1:11) .NE. 'END OF FILE') THEN
C        CALL COPPIC (SUBPIC, SUBT, NCOL, NROW, ISTAT)
         CALL COPPIC (SUBPIC, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error opening output picture.')
            RETURN
         END IF
         CALL WRARAY ('COPY', 1, 1, NCOL, NROW, NCOL, DATA, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error writing picture.')
            RETURN
         END IF
         CALL CLPIC ('COPY')
      END IF
      CALL STUPID ('    Done.  ')
      CALL CLFILE (2)
      RETURN
      END
