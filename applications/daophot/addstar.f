      SUBROUTINE  ADDSTR  (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .     F, NCOL, NROW, WATCH)
C
C=======================================================================
C
C This subroutine generates random x,y coordinates and magnitudes,
C appropriately scales the point-spread function, and adds these
C artificial stars into a copy of the original picture at the
C appropriate locations.  As an alternative, it will read positions
C and magnitudes in from a disk file, and add synthetic stars with
C simulated photon noise.
C
C             OFFICIAL DAO VERSION:  1991 April 18
C
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     19-Feb-1992 (NE)
*     Updated the argument list in one of the calls to COPPIC.
*     17-Mar-1995 (GJP)
*     Replaced very negative, very large or very small numbers with
*     their PRM_PAR equivalents.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXPSF, MAXPAR, MAXEXP, NCOL, NROW
C
C Parameters
C
C MAXPSF is the largest permissible number of elements in the look-up
C        table for the point-spread function.
C
      REAL F(NCOL,NROW), PSF(MAXPSF,MAXPSF,MAXEXP)
      REAL PAR(MAXPAR), RMAG(2)
      INTEGER ISEED(3)
C
      REAL SQRT, AMAX1, DAORAN, USEPSF
      INTEGER RDPSF, MIN0, MAX0, NINT
C
      CHARACTER*80 LINE
      CHARACTER*30 ADDPIC, OUTSTM, ADDFIL, EXTEND
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER CASE*5, NTOCHR*2
      DOUBLE PRECISION SUMPHOT, SUMERR
      REAL LOBAD, NRML, STRMAG, X, Y, DIFMAX, DIFSQ, DX, DY, DYSQ
      REAL DIFF, SCALE, DELTAX, DELTAY, DVDXC, DVDYC, ERR
      REAL HIBAD, THRESH, AP1, DUMMY, READNS, FRAD, XWIDE, YWIDE
      REAL PSFRSQ, WATCH, PSFMAG, BRIGHT, XPSF, YPSF, PSFRAD, PHPADU
      REAL DATUM, SKY
      INTEGER I, J, ID, ISTAR, IDUM, IFRAME, LX, LY, NX, NY, ISTAT
      INTEGER IPSTYP, NPSF, NPAR, NEXP, NFRAC, NSTAR
      INTEGER NFRAME, NL
      LOGICAL RANDOM
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C-----------------------------------------------------------------------
C
      CALL TBLANK                                   ! Type a blank line
  950 CALL GETNAM ('File with the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END OF FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         PSFFIL = ' '
         RETURN
      END IF
      PSFFIL=EXTEND(PSFFIL, CASE('psf'))
C
C Read in the PSF.
C
      ISTAT = RDPSF(PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (ISTAT .NE. 0) THEN
         PSFFIL = 'GIVE UP'
         GO TO 950
      END IF
C
      PSFRAD = (REAL(NPSF-1)/2. - 1.)/2.
      PSFRSQ = PSFRAD**2
      CALL SEED3 (ISEED)
      CALL GETDAT ('Photons per ADU:', PHPADU,1)
      IF (PHPADU .LE. 0) RETURN
      ADDFIL='RANDOM STARS'
 1015 CALL GETNAM ('Input data file:', ADDFIL)
      IF ((ADDFIL .EQ. 'END OF FILE') .OR. (ADDFIL .EQ. 'EXIT') .OR.
     .     (ADDFIL .EQ. 'GIVE UP')) THEN
         RETURN
      ELSE IF (ADDFIL .EQ. 'RANDOM STARS') THEN
         RANDOM=.TRUE.
         WRITE (6,610) PSFMAG
  610    FORMAT (/' Magnitude of PSF star is', F7.3/)
         CALL GETDAT ('Minimum, maximum magnitudes desired:', RMAG, 2)
         IF (RMAG(1) .LE. VAL__MINR) RETURN            ! CTRL-Z was entered
C
         CALL GETDAT ('Number of stars to add to each frame:',
     .        DATUM, 1)
         IF (DATUM .LT. -VAL__SMLR) RETURN             ! CTRL-Z was entered
         NSTAR=MAX0(1, NINT(DATUM))
C
         CALL GETDAT ('Number of new frames:', DATUM, 1)
         IF (DATUM .LE. VAL__MINR) RETURN              ! CTRL-Z was entered
         NFRAME=MAX0(1, MIN0(NINT(DATUM), 99))
         OUTSTM=' '
         CALL GETNAM ('File-name stem:', OUTSTM)
         IF (OUTSTM .EQ. 'END OF FILE') RETURN     ! CTRL-Z was entered
         OUTSTM=EXTEND(OUTSTM, CASE('add'))
C
      ELSE
         ADDFIL = EXTEND(ADDFIL, CASE('add'))
         CALL INFILE (2, ADDFIL, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error opening input file '//ADDFIL)
            ADDFIL = 'GIVE UP'
            GO TO 1015
         END IF
         ADDPIC=SWITCH(ADDFIL, ' ')
         CALL GETNAM ('Output picture name:', ADDPIC)
         NL=-1
         CALL RDHEAD (2, NL, IDUM, IDUM, LOBAD, HIBAD, THRESH, AP1,
     .        DUMMY, READNS, FRAD)
         IF (NL .LE. 0) NL=1
      END IF
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Do it.
C
      IF (RANDOM) THEN
         IF (WATCH .GT. 0.5) THEN
            CALL TBLANK
            CALL OVRWRT (
     .      '  Star       Picture                         Data file', 1)
         END IF
C
C Beginning of loop over output frames.
C
         DO 2900 IFRAME=1,NFRAME
C
C Build up output stellar-data filename, and open file.
C
         ADDFIL=NTOCHR(IFRAME)//CASE('.add')
         ADDFIL=SWITCH(OUTSTM,ADDFIL)
  952    CALL OUTFIL (3, ADDFIL, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error opening output file '//ADDFIL)
            ADDFIL = 'GIVE UP'
            CALL GETNAM ('New output file name:', ADDFIL)
            IF ((ADDFIL .EQ. 'END OF FILE') .OR.
     .           (ADDFIL .EQ. 'GIVE UP')) RETURN
            ADDFIL = EXTEND(ADDFIL, CASE('add'))
            GO TO 952
         END IF
         CALL WRHEAD (3, 1, NCOL, NROW, 7, 0., 0., 0., 0., 0., 0., 0.)
C
C Create output picture filename.
C
         ADDPIC=SWITCH(ADDFIL, ' ')
C
C Copy the input picture verbatim into the output picture.
C
C        CALL COPPIC (ADDPIC, F, NCOL, NROW, ISTAT)
         CALL COPPIC (ADDPIC, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error creating output picture.')
            RETURN
         END IF
         LX = 1
         LY = 1
         NX = NCOL
         NY = NROW
         CALL RDARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
C
C Beginning of loop over artificial stars.
C
         XWIDE = REAL(NCOL)-1.
         YWIDE = REAL(NROW)-1.
         DO 2500 ISTAR=1,NSTAR
C
C Make up centroid and magnitude.
C
         X=1.+XWIDE*DAORAN(ISEED(1))
         Y=1.+YWIDE*DAORAN(ISEED(2))
         STRMAG=RMAG(1)+DAORAN(ISEED(3))*(RMAG(2)-RMAG(1))
C
C Write them to the data file.
C
         WRITE (3,320) ISTAR+8999, X, Y, STRMAG
  320    FORMAT (1X, I5, 14F9.3)
         IF (WATCH .GT. 0.5) THEN
            WRITE (LINE,622) ISTAR, ADDPIC, ADDFIL
  622       FORMAT (I6, 7X, A30, 2X, A30)
            CALL OVRWRT (LINE(1:75), 2)
         END IF
         DELTAX=(X-1.)/XPSF - 1.
         DELTAY=(Y-1.)/YPSF - 1.
         LX = MAX0( 1, INT(X-PSFRAD)+1 )
         LY = MAX0( 1, INT(Y-PSFRAD)+1 )
         NX = MIN0( NCOL, INT(X+PSFRAD) )
         NY = MIN0( NROW, INT(Y+PSFRAD) )
         SCALE=10.**(0.4*(PSFMAG-STRMAG))
         DIFMAX=0.
         DIFSQ=0.
C
C Beginning of double loop over pixels in the subarray.
C
         SUMPHOT = 0.0D0
         SUMERR = 0.0D0
         DO 2210 J=LY,NY
            DY=FLOAT(J)-Y
            DYSQ=DY**2
C
            DO 2200 I=LX,NX
               DX=FLOAT(I)-X
               IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                  IF (DX .GT. 0.) GO TO 2210
               ELSE
                  DIFF=SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF,
     .                 NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC,
     .                 DVDYC)
C
C DIFF represents the value of the stellar profile at this pixel.
C Compute a Poisson random error using a normal approximation,
C sigma(DIFF)=sqrt(DIFF/PHPADU).
C
                  ERR=SQRT(AMAX1(0.,DIFF/PHPADU))*
     .                 NRML(DAORAN(ISEED(MOD(I+J,3)+1)))
                  SUMPHOT = SUMPHOT + DBLE(DIFF)
                  SUMERR = SUMERR + DBLE(ERR)
                  F(I,J)=F(I,J)+DIFF+ERR
               END IF
 2200       CONTINUE
 2210    CONTINUE
 2500    CONTINUE                              ! End of loop over stars
         LX = 1
         LY = 1
         NX = NCOL
         NY = NROW
         CALL WRARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
         CALL CLPIC ('COPY')                   ! Close copy
         CALL CLFILE (3)
         CALL OVRWRT (' ', 1)
C
 2900    CONTINUE                              ! End of loop over frames
      ELSE
C
C Update the argument list in the call to COPPIC
C Nick Eaton, Durham University, 19 Feb 1992
C
C        CALL COPPIC (ADDPIC, F, NCOL, NROW, ISTAT)
         CALL COPPIC (ADDPIC, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error creating output picture.')
            RETURN
         END IF
         IF (WATCH .GT. 0.5) THEN
            CALL TBLANK
            CALL OVRWRT ('  Star', 1)
         END IF
         LX = 1
         LY = 1
         NX = NCOL
         NY = NROW
         CALL RDARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
C
C Loop over stars.
C
         ISTAR=0
 3000    ISTAR=ISTAR+1
 3010    CALL RDSTAR (2, NL, ID, X, Y, STRMAG, SKY)
         IF (ID .LT. 0) GO TO 3900             ! End-of-file encountered
         IF (ID .EQ. 0) GO TO 3010             ! Ignore a blank line
         IF (STRMAG .GE. 99.) GO TO 3000       ! Ignore a bad star
         IF (WATCH .GT. 0.5) THEN
            WRITE (LINE,622) ISTAR
            CALL OVRWRT (LINE(1:6), 2)
         END IF
         DELTAX=(X-1.)/XPSF-1.
         DELTAY=(Y-1.)/YPSF-1.
         SCALE=10.**(0.4*(PSFMAG-STRMAG))
C
C Add the shifted scaled PSF
C
         LX = MAX0(1, INT(X-PSFRAD)+1 )
         LY = MAX0(1, INT(Y-PSFRAD)+1 )
         NX = MIN0(NCOL, INT(X+PSFRAD) )
         NY = MIN0(NROW, INT(Y+PSFRAD) )
         SUMPHOT = 0.0D0
         SUMERR = 0.0D0
         DO 3030 J=LY,NY
            DY=FLOAT(J)-Y
            DYSQ=DY**2
            DO 3020 I=LX,NX
               DX=FLOAT(I)-X
               IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                  IF (DX .GT. 0.) GO TO 3030
               ELSE
                  DIFF=SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF,
     .                 NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC,
     .                 DVDYC)
C
C DIFF represents the value of the stellar profile at this pixel.
C Compute a Poisson random error using a normal approximation,
C sigma(DIFF)=sqrt(DIFF/PHPADU).
C
                  ERR=DAORAN(ISEED(MOD(I+J,3)+1))
                  ERR=SQRT(AMAX1(0.,DIFF/PHPADU))*NRML(ERR)
                  SUMPHOT = SUMPHOT + DBLE(DIFF)
                  SUMERR = SUMERR + DBLE(ERR)
                  F(I,J)=F(I,J)+DIFF+ERR
               END IF
 3020       CONTINUE
 3030    CONTINUE
         GO TO 3000                           ! Go to next star
C
 3900    LX = 1
         LY = 1
         NX = NCOL
         NY = NROW
         CALL WRARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
         CALL CLPIC ('COPY')
         CALL CLFILE (2)
         ADDFIL='EXIT'
         CALL OVRWRT (' ', 1)
         GO TO 1015
      END IF
C
      RETURN
C
      END!
C
C#######################################################################
C
      CHARACTER*2  FUNCTION  NTOCHR (N)
C
C Converts an integer in the range 1-99 to two characters representing
C the number.
C
      IMPLICIT NONE
      INTEGER N, ITENS
      IF ((N .GT. 0) .AND. (N .LT. 100)) GO TO 1010
      NTOCHR='00'
      RETURN
C
 1010 ITENS=N/10
      NTOCHR=CHAR(48+ITENS)//CHAR(48+N-10*ITENS)
      RETURN
C
      END!
C
C###################################################################
C
      SUBROUTINE  SEED3 (ISEED)
C
C Seed the random number generator, based on a floating-point number
C entered by the user.
C
      IMPLICIT NONE
      INTEGER ISEED(3)
C
      REAL DAORAN
      INTEGER INT
C
      REAL SEED
      INTEGER I
C
      CALL GETDAT ('Seed (any integer):', SEED, 1)
      I = NINT(SEED)
      ISEED(1) = INT(1073741823.*DAORAN(I)) + 1
      ISEED(2) = INT(1073741823.*DAORAN(I)) + 1
      ISEED(3) = INT(1073741823.*DAORAN(I)) + 1
      RETURN
C
      END!
C
C######################################################################
C
      REAL  FUNCTION  NRML  (RANNUM)
C
C Convert a uniform probability distribution to a Gaussian distribution
C with mean zero and standard deviation unity.
C
      IMPLICIT NONE
C
      REAL SQRT, ALOG
C
      REAL P, RANNUM, SIGN, T
 1000 P=RANNUM
      SIGN=-1.
      IF (P .GT. 0.5) THEN
         P=P-0.5
         SIGN=1.
      ELSE IF (P .LE. 0.) THEN
         NRML = -1.E20
         RETURN
      END IF
      T=SQRT(ALOG(1/P**2))
      NRML=SIGN*(T- (2.30753+.27061*T) / (1.+T*(.99229+T*.04481)) )
      RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION DAORAN (IDUM)
C
C RAN2 from Numerical Recipes
C
      INTEGER IR(97)
      DATA IFF /0/
      DATA M/714025/, IA/1366/, IC/150889/, RM/1.400511E-6/
      SAVE IFF, IY, M, IR

 1000 CONTINUE
      IF ((IDUM .LT. 0) .OR. (IFF .EQ. 0)) THEN
         IFF = 1
         IDUM = MOD(IABS(IC-IDUM), M)
         DO J=1,97
            IDUM = MOD(IA*IDUM+IC,M)
            IR(J) = IDUM
         END DO
         IDUM = MOD(IA*IDUM+IC,M)
         IY = IDUM
      END IF
      J = 1+(97*IY)/M
      IF ((J .GT. 97) .OR. (J .LT. 1)) PAUSE
      IY = IR(J)
      DAORAN = IY*RM
      IDUM = MOD(IA*IDUM+IC,M)
      IR(J) = IDUM
      IF (DAORAN .LE. 0.) GO TO 1000              ! Stetson's modification
C
      RETURN
      END!
