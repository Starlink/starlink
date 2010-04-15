      SUBROUTINE  PCKPSF  (ID, X, Y, M, S, INDEX, MAX, FITRAD, PSFRAD)
C
C=======================================================================
C
C Subroutine to read in any of the data files created by DAOPHOT and
C to select reasonable candidates for PSF stars:
C
C   (1) More than a critical distance from the edge of the frame, and
C   (2) Having no brighter star within a critical distance.
C
C              OFFICIAL DAO VERSION:  1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAX
C
C MAX is the maximum number of stars permitted in a data file.
C
      REAL X(MAX), Y(MAX), M(MAX), S(MAX)
      INTEGER ID(MAX), INDEX(MAX)
C
      REAL ABS
C
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER FILE*30, CASE*4
      REAL PSFRAD, FITRAD, RADSQ, DY, RADIUS, LOBAD, HIBAD, THRESH
      REAL AP1, PHPADU, READNS, FRAD, XYMIN, XMAX, YMAX
      INTEGER I, J, N, NREQ, ISTAR, JSTAR, ISTAT, NL, NCOL, NROW
      INTEGER ITEMS, NSTAR
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get input file name, open the file, and read its header.
C
      CALL TBLANK
      CALL GETNAM ('Input file name:', MAGFIL)
      IF ((MAGFIL .EQ. 'END OF FILE') .OR.
     .     (MAGFIL .EQ. 'EXIT')) THEN
         MAGFIL = ' '
         RETURN
      END IF
C
      CALL INFILE (2, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//MAGFIL)
         RETURN
      END IF
C
      CALL GETDAT ('Desired number of PSF stars:', DY, 1)
      IF (DY .LE. 0.5) THEN
         CALL CLFILE (2)
         RETURN
      END IF
      NREQ = NINT(DY)
C
C Generate output file name and open the file.
C
      FILE = SWITCH (MAGFIL, CASE('.lst'))
      CALL GETNAM ('Output file name:', FILE)
      CALL OUTFIL (3, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//FILE)
         CALL CLFILE (2)
         RETURN
      END IF
C
      NL = 0
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
      IF (NL .EQ. 0) NL = 1
      IF (NL .GT. 3) NL = 1
C
C Copy input file header to output file.
C
      ITEMS = 6
      IF (FRAD .GT. 0.) ITEMS = 7
      CALL WRHEAD (3, 3, NCOL, NROW, ITEMS, LOBAD, HIBAD, THRESH,
     .     AP1, PHPADU, READNS, FRAD)
      XYMIN = FITRAD + 1.
      XMAX = REAL(NCOL) - FITRAD
      YMAX = REAL(NROW) - FITRAD
      RADIUS = PSFRAD+FITRAD+2.
      RADSQ = RADIUS**2
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the input file in star by star.
C
      I=0
 2000 I=I+1                                   ! Begin loop over stars
C
 2010 CALL RDSTAR (2, NL, ID(I), X(I), Y(I), M(I), S(I))
      IF (ID(I) .LT. 0) GO TO 2100            ! END OF FILE encountered
      IF (ID(I) .EQ. 0) GO TO 2010            ! Blank line encountered
      IF (M(I) .GT. 90.) M(I) = -M(I)
      IF (I .LT. MAX) GO TO 2000
C
      CALL STUPID ('*** WARNING ***  Too many stars in input file.')
      WRITE (6,61)
   61 FORMAT ('Increase the MS parameter to raise the limit')
      WRITE (6,6) MAX
    6 FORMAT (I10, ' stars have been read.  I will work with these.')
      I = I+1
C
C Perform the selection.
C
 2100 NSTAR=I-1                                      ! Number of stars
      CLOSE (2)
      CALL QUICK (M, NSTAR, INDEX)
      I = INDEX(1)
      IF ((M(1) .GT. -90.) .AND. (X(I) .GE. XYMIN) .AND.
     .     (Y(I) .GE. XYMIN) .AND. (X(I) .LE. XMAX) .AND.
     .     (Y(I) .LE. YMAX)) THEN
         WRITE (3,321) ID(I), X(I), Y(I), M(1), S(I)
  321    FORMAT (I6, 4F9.3)
         N = 1
      ELSE
         N = 0
      END IF
C
      DO 2195 ISTAR=2,NSTAR
         I = INDEX(ISTAR)
         IF ((M(ISTAR) .LT. -90.) .OR. (X(I) .LT. XYMIN) .OR.
     .        (Y(I) .LT. XYMIN) .OR. (X(I) .GT. XMAX) .OR.
     .        (Y(I) .GT. YMAX)) GO TO 2195
C
         DO 2190 JSTAR=1,ISTAR-1
            J = INDEX(JSTAR)
            DY = ABS(Y(J)-Y(I))
            IF (DY .GE. RADIUS) GO TO 2190
            DY = DY**2 + (X(J)-X(I))**2
            IF (DY .LT. RADSQ) GO TO 2195
 2190    CONTINUE
C
         WRITE (3,321) ID(I), X(I), Y(I), M(ISTAR), S(I)
         N = N+1
         IF (N .GE. NREQ) GO TO 2200
 2195 CONTINUE
C
 2200 CLOSE (3)
      WRITE (6,7) N
    7 FORMAT (/I10, ' suitable candidates were found.'/)
      RETURN
C
      END!
