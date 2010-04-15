      SUBROUTINE  SUBSTR  (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .     F, NCOL, NROW, WATCH)
C
C=======================================================================
C
C This subroutine scales and shifts the point-spread function according
C to each star's magnitude and centroid, and subtracts the resulting
C profile from a copy of the original picture.
C
C             OFFICIAL DAO VERSION:  1991 April 18
C
C Arguments
C
C  WATCH (INPUT) governs whether information relating to the progress
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAXBOX, MAXPSF, MAXPAR, MAXEXC, MAXEXP
      PARAMETER  (MAXBOX=69, MAXEXC=200)
C
C Parameters
C
C MAXBOX is the side of the square subarray containing the largest
C        (circular) PSF that can be subtracted from the picture.
C
C MAXPSF is the largest permissible number of elements on a side of the
C        (square) look-up table for the point-spread function.
C
C        MAXBOX = (MAXPSF-7)/2.
C
      INTEGER NCOL, NROW
      REAL PAR(MAXPAR)
      REAL F(NCOL,NROW), PSF(MAXPSF,MAXPSF,MAXEXP)
      INTEGER IEXC(MAXEXC)
C
      REAL USEPSF
      INTEGER RDPSF, MAX0, MIN0, INT
C
      CHARACTER*80 LINE
      CHARACTER*30 SUBPIC
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER CASE*5, ANSWER*1
      REAL LOBAD, X, Y, STRMAG, SKY, DX, DY, DYSQ, SCALE, COL, ROW
      REAL DELTAX, DELTAY, DVDXC, DVDYC, XPSF, YPSF, PSFRAD, DUM
      REAL HIBAD, THRESH, AP1, PHPADU, READNS, FRAD, WATCH, PSFMAG
      REAL BRIGHT, PSFRSQ
      INTEGER I, J, L, NL, IDUM, NEXC, LX, LY, NX, NY, ISTAR, ID
      INTEGER ISTAT, IPSTYP, NPSF, NPAR, NEXP, NFRAC
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get file names and set up the needed numerical constants.
C
      CALL TBLANK                                   ! Type a blank line
  950 CALL GETNAM ('File with the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END OF FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         PSFFIL = ' '
         RETURN
      END IF
      ISTAT = RDPSF (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (ISTAT .LT. 0) THEN
         PSFFIL = 'GIVE UP'
         GO TO 950
      END IF
C
      PSFRAD=(REAL(NPSF-1)/2. - 1.)/2.
      PSFRSQ=PSFRAD**2
      COL=FLOAT(NCOL)
      ROW=FLOAT(NROW)
C
      CALL CLFILE (2)
C
 1015 CALL GETNAM ('File with photometry:', PROFIL)
      IF ((PROFIL .EQ. 'END OF FILE') .OR.
     .     (PROFIL .EQ. 'GIVE UP')) THEN
         PROFIL = ' '
         RETURN
      END IF
      CALL INFILE (2, PROFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//PROFIL)
         PROFIL = 'GIVE UP'
         GO TO 1015
      END IF
C
      CALL GETYN ('Do you have stars to leave in?', ANSWER)
      IF (ANSWER .EQ. 'Y') THEN
         SUBPIC = SWITCH(PROFIL, CASE('.lst'))
 1025    CALL GETNAM ('File with star list:', SUBPIC)
         IF ((SUBPIC .EQ. 'END OF FILE') .OR. (SUBPIC .EQ.
     .        'GIVE UP')) THEN
            CALL CLFILE (2)
            RETURN
         END IF
         CALL INFILE (3, SUBPIC, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error opening file '//SUBPIC)
            SUBPIC = 'GIVE UP'
            GO TO 1025
         END IF
         NL = -1
         CALL RDHEAD (3, NL, IDUM, IDUM, DUM, DUM, DUM, DUM,
     .        DUM, DUM, DUM)
         NEXC = 0
 1035    NEXC = NEXC+1
 1036    CALL RDSTAR (3, NL, IEXC(NEXC), DUM, DUM, DUM, DUM)
         IF (IEXC(NEXC) .EQ. 0) GO TO 1036
         IF (IEXC(NEXC) .LT. 0) THEN
            NEXC = NEXC-1
         ELSE
            IF (NEXC .LT. MAXEXC) GO TO 1035
         END IF
         IF (NEXC .LE. 0) ANSWER = 'N'
         CALL CLFILE (3)
      END IF
C
      CALL RDHEAD (2, NL, IDUM, IDUM, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
C
      SUBPIC=SWITCH(PROFIL, CASE('s'))
      CALL GETNAM ('Name for subtracted image:', SUBPIC)
      IF (SUBPIC .EQ. 'END OF FILE') GO TO 9010    ! CTRL-Z was entered
C
C Copy the input picture verbatim into the output picture.
C
C     CALL COPPIC (SUBPIC, F, NCOL, NROW, ISTAT)
      CALL COPPIC (SUBPIC, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error creating output image.')
         CALL CLFILE (2)
         RETURN
      END IF
C
      IF (WATCH .GT. 0.5) THEN
         CALL TBLANK
         CALL OVRWRT (' Star', 1)
      END IF
C
C Read the entire image in from the copy.
C
      LX = 1
      LY = 1
      NX = NCOL
      NY = NROW
      CALL RDARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Loop over stars.
C
      ISTAR=0
 2000 ISTAR=ISTAR+1
 2010 CALL RDSTAR (2, NL, ID, X, Y, STRMAG, SKY)
      IF (ID .LT. 0) GO TO 9000                ! End-of-file encountered
      IF (ID .EQ. 0) GO TO 2010                ! Ignore a blank line
      IF ((1.-X .GT. PSFRAD) .OR. (X-COL .GT. PSFRAD) .OR.
     .    (1.-Y .GT. PSFRAD) .OR. (Y-ROW .GT. PSFRAD)) GO TO 2000
      IF (ANSWER .EQ. 'Y') THEN
         L = NEXC
         DO I=1,L
            IF (ID .EQ. IEXC(I)) THEN
               IEXC(I) = IEXC(NEXC)
               NEXC = NEXC-1
               IF (NEXC .EQ. 0) ANSWER = 'N'
               GO TO 2010
            END IF
         END DO
      END IF
      IF (STRMAG .GE. 99.) GO TO 2000          ! Ignore a bad star
      IF (WATCH .GT. 0.5) THEN
         WRITE (LINE,620) ISTAR
  620    FORMAT (I5)
         CALL OVRWRT (LINE(1:5), 2)
      END IF
      DELTAX=(X-1.)/XPSF-1.
      DELTAY=(Y-1.)/YPSF-1.
      LX=MAX0(1, INT(X-PSFRAD)+1)
      LY=MAX0(1, INT(Y-PSFRAD)+1)
      NX=MIN0(NCOL, INT(X+PSFRAD))
      NY=MIN0(NROW, INT(Y+PSFRAD))
      SCALE=10.**(0.4*(PSFMAG-STRMAG))
C
C Subtract the shifted scaled PSF
C
      DO 2030 J=LY,NY
         DY=FLOAT(J)-Y
         DYSQ=DY**2
         DO 2020 I=LX,NX
            IF (F(I,J) .GT. HIBAD) GO TO 2020
            DX=FLOAT(I)-X
            IF (DX**2+DYSQ .GE. PSFRSQ) THEN
               IF (DX .GT. 0.) GO TO 2030
            ELSE
               F(I,J)=F(I,J)-SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR,
     .              PSF, NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY,
     .              DVDXC, DVDYC)
            END IF
 2020    CONTINUE
 2030 CONTINUE
      GO TO 2000                              ! Go to next star
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CONTINUE
      IF (WATCH .GT. 0.5) CALL OVRWRT (' ', 4)
C
C Write the modified image back into the copy.
C
      LX = 1
      LY = 1
      NX = NCOL
      NY = NROW
      CALL WRARAY ('COPY', LX, LY, NX, NY, NCOL, F, ISTAT)
      CALL CLPIC ('COPY')
      CALL STUPID ('    Done.  ')
 9010 CALL CLFILE (2)
      RETURN
C
      END!
