      SUBROUTINE  FIND (D, H, JCYLN, G, SKIP, MAX, MAXBOX, MAXCOL,
     .     MAXSKY, OPT, NOPT)
      IMPLICIT NONE
C
C=======================================================================
C
C This subroutine is supposed to find small, positive brightness
C perturbations in a two-dimensional image.
C
C                OFFICIAL DAO VERSION:  1991 April 18
C
C First, FIND reads in several rows' worth of image data.  For each
C pixel it computes a least-squares fit of an analytic Gaussian function
C to a roughly circular array of pixels surrounding the pixel in
C question.  The overall bias level (sky brightness in that vicinity)
C is removed by the calculation and, since the function is
C symmetric about the central pixel, a smooth gradient in the sky
C brightness cancels out exactly.  This means that the user does
C not have to specify an absolute brightness threshold for star
C detection, and if the mean background brightness varies over the
C frame, to the extent that the variations are smooth and large-scale,
C to first order they will have no effect on the detection limit.
C    The derived peak heights of the Gaussian functions are stored in a
C scratch disk image file.  Later they will be read back in, and local
C maxima in the peak values will be sought.  After undergoing a few
C tests designed to select against bad pixels and bad columns, these
C local maxima will be considered to be astronomical objects, better
C image centroids will be computed, and the objects will be assigned
C sequential ID numbers and will be written to a disk data file.
C    The user is asked to specify a "lowest good data-value"-- any pixel
C whose value is found to fall below this level or above the HIBAD
C value which is passed as an argument is presumed bad, and
C is ignored during all computations in this routine.  The numerical
C value of this bad pixel ceiling will be written out in the header
C of the output data file, and will be used in other DAOPHOT routines
C as well.
C
C Arguments
C
C     FWHM (INPUT) is the estimated full width at half-maximum of the
C          objects for which the algorithm is to be optimized.  It will
C          be used (a) to determine the size of the roughly circular
C          array which will be used to compute the brightness
C          enhancements and to define local maxima, and (b) to define
C          the coefficient assigned to each pixel in the computation
C          of the brightness enhancements.
C
C    WATCH (INPUT) governs whether information relating to the progress
C          of the star-finding is to be typed on the terminal screen
C          during execution.
C
C SHRPLO, SHRPHI (INPUT) are numerical cutoffs on the image-sharpness
C          statistic, designed to eliminate brightness maxima which
C          appear to be due to bad pixels, rather than to astronomical
C          objects.
C
C RNDLO, RNDHI (INPUT) are numerical cutoffs on the image-roundness
C          statistic, designed to eliminate brightness maxima which
C          appear to be due to bad rows or columns, rather than to
C          astronomical objects.
C
C HIBAD  is the highest valid data-value-- the level above which the
C          CCD chip is presumed to be non-linear.
C
C All of the above arguments are user-definable optional parameters,
C whose numerical values may be changed by a DEFAULT.OPT file, or by
C the OPTION command.  (WATCH may also the set by the MONITOR and
C NOMONITOR commands.)
C
C=======================================================================
C
C Parameters:
C
      INTEGER MAX, MAXBOX, MAXCOL, MAXSKY, NOPT
C
C MAXBOX is the length of the side of the largest subarray that you plan
C        to need for computing the brightness enhancement in each pixel.
C
C MAX/MAXBOX is the length in the x-direction of the largest picture
C        you can try to reduce.
C
C-----------------------------------------------------------------------

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

C
C DIMENSIONS
C
C Arrays
C
      REAL D(MAXCOL,MAXBOX), H(MAXCOL,MAXBOX), DATA(2), OPT(NOPT)
      REAL G(MAXBOX,MAXBOX), AMAX1
      INTEGER JCYLN(MAX)
      LOGICAL SKIP(MAXBOX,MAXBOX)
C
C Variables
C
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, CONPIC
      CHARACTER SWITCH*30, EXTEND*30, LINE*5, CASE*4, ANSWER*1
      REAL PIXELS, RADIUS, FWHM, SIGSQ, RSQ, RELERR, SKYLVL, TEMP
      REAL HMIN, LOBAD, HIBAD, WATCH, P, DATUM, HEIGHT, DENOM, SGOP
      REAL SHARP, ROUND, SHRPLO, SHRPHI, RNDLO, RNDHI
      REAL SUMG, SUMGSQ, SUMGD, SUMD, SG, SGSQ, SGD, SD, WT, HX, HY
      REAL DGDX, SDGDX, SDGDXS, SDDGDX, SGDGDX
      REAL XCEN, YCEN, DX, DY, PHPADU, READNS, SKYMOD
      INTEGER IP1, IP2, IP3
      INTEGER NHALF, NBOX, MIDDLE, LASTCL, LASTRO, NCOL, NROW, JSQ
      INTEGER ISTAT, NROWS, NSTAR
      INTEGER I, J, K, N, IX, IY, JX, JY, KX, LX, LY
C
C Common
C
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
      HIBAD = OPT(4)
      FWHM = OPT(5)
      SHRPLO = OPT(7)
      SHRPHI = OPT(8)
      RNDLO = OPT(9)
      RNDHI = OPT(10)
      WATCH = OPT(11)
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Setup the necessary variables and arrays, particularly the constants
C to be used in the convolutions.
C
C The brightness enhancement will be computed on the basis only of those
C pixels within 1.5 sigma = 0.637*FWHM of the central pixel.  However,
C in the limit of infinitely small FWHM the brightness enhancement will
C be based on no fewer than the following subarray of pixels:
C
C                                .
C                                .
C                                .
C
C                          -  -  +  -  -
C                          -  +  +  +  -
C                 .  .  .  +  +  X  +  +  .  .  .
C                          -  +  +  +  -
C                          -  -  +  -  -
C                                .
C                                .
C                                .
C
C This represents a 5 x 5 subarray taken out of the original picture.
C The X represents the pixel for which the brightness enhancement is
C currently being computed and the +'s represent other pixels included
C in the calculation; the -'s and all pixels lying outside this 5 x 5
C subarray will not be used in computing the brightness enhancement in
C the central pixel.  In the limit of infinitely large FWHM, only those
C pixels lying within a MAXBOX x MAXBOX square subarray centered on the
C pixel in question will be used in computing its brightness
C enhancement.
C
C Compute the size of the subarray needed.  The radius of the circular
C area desired is MAX (2.0, 0.637*FWHM), so the distance from the
C central pixel to the center of an edge pixel is the integer smaller
C than this.
C
      RADIUS=AMAX1(2.001, 0.637*FWHM)
      NHALF=MIN0((MAXBOX-1)/2, INT(RADIUS))
      NBOX=2*NHALF+1                ! Length of the side of the subarray
      MIDDLE=NHALF+1
C
C Just for future reference--
C
C MIDDLE is the index of the central pixel of the box in both x and y,
C        where the corner of the box is considered to be at (1,1).
C
C  NHALF is the number of pixels between the central pixel (exclusive)
C        and the edge of the box (inclusive).  For example, if NBOX = 7,
C        MIDDLE = 4 and NHALF = 3.  Note that all the way around the
C        picture being reduced there will be a border NHALF pixels wide
C        where define brightness enhancements can't be defined, because
C        the box would extend beyond the boundaries of the frame.  We
C        will thus be able to compute brightness enhancements only for
C        MIDDLE <= x <= LASTCL,   MIDDLE <= y <= LASTRO, where...
C
      LASTRO=NROW-NHALF
      LASTCL=NCOL-NHALF
C
C-----------------------------------------------------------------------
C
C Compute the values of a bivariate circular Gaussian function with
C unit height and the specified value of the FWHM.
C
      SIGSQ=(FWHM/2.35482)**2
      RADIUS=RADIUS**2
C
C RADIUS is now the square of the radius of the circle to be used.
C
C-----------------------------------------------------------------------
C
C EXPLANATION:
C
C The approach taken by this star-finding algorithm is defined by this
C question:  "Assuming for the moment that there is a star with a
C Gaussian light distribution centered in the central pixel of this
C subarray, then how bright is it?"  Having answered that question for
C every pixel MIDDLE <= x <= LASTCL, MIDDLE <= y <= LASTRO, we will
C then go through the picture looking for places where the numerical
C answer to the question achieves local maxima.  For the region around
C each pixel, then, we want to solve this equation via least squares:
C
C                     D(i,j) = h * G(i,j) + s
C
C where D is the observed brightness in some pixel of the subarray, G
C is the value of the Gaussian function of unit central height in the
C in that pixel
C
C G(i,j) = exp{[(i-MIDDLE)**2 + (j-MIDDLE)**2]/(2 * sigma**2)}, for
C
C                      (i-MIDDLE)**2 + (j-MIDDLE)**2 < (1.5 * sigma)**2
C
C (the center of the subarray has relative coordinates i = j = MIDDLE).
C
C      The parameters  h  (= central brightness of the hypothetical
C star centered in the central pixel of the subarray), and s (= the
C local sky background) are unknowns.  The least-squares solution
C for this system of equations is given by
C
C         [G*D] - [G] [D]/n
C    h =  ----------------- ,         s = {[D] - h [G]}/n
C         [G**2] - [G]**2/n
C
C where the square brackets denote summation (Gauss's notation).
C
C For use in solving for the many values of  h, we will save the
C array G(i,j) (= G(I,J)) and the constants [G] (= SUMG, meaning
C "sum of the Gaussian"), [G**2] (= SUMGSQ), n (= PIXELS); also the
C denominator of the fraction for  h (= DENOM), and [G]/n (= SGOP).
C [G*D] and [D] will have to be computed each time.
C
C It is possible to show that each of these least-squares problems can
C be reduced to a linear function of the image data D(i,j), and that the
C entire ensemble of least-squares problems is arithmetically identical
C with a convolution of the original image data with a truncated,
C lowered Gaussian function.  Hence, I will occasionally refer to the
C generation of the array of values h(i,j) as a "convolution."
C
C-----------------------------------------------------------------------
C
C Loop over the pixels in the subarray, computing the value of the
C Gaussian function G(i,j) at each point.  Also, accumulate the sum of
C the values of the Gaussian and the sum of the squares of the values
C of the Gaussian.  These will be held for later use in the convolution.
C
      SUMG=0.0
      SUMGSQ=0.0
      PIXELS=0.0
      DO J=1,NBOX
         JSQ=(J-MIDDLE)**2
C
         DO I=1,NBOX
            RSQ=FLOAT((I-MIDDLE)**2+JSQ)
            G(I,J)=EXP(-0.5*RSQ/SIGSQ)
            IF (RSQ .LE. RADIUS) THEN
               SKIP(I,J)=.FALSE.
               SUMG=SUMG+G(I,J)
               SUMGSQ=SUMGSQ+G(I,J)**2
               PIXELS=PIXELS+1.0
            ELSE
               SKIP(I,J)=.TRUE.
            END IF
         END DO
      END DO
      DENOM=SUMGSQ-(SUMG**2)/PIXELS
      SGOP=SUMG/PIXELS
C
C At this point the two-dimensional array G(I,J) contains the values of
C a unit Gaussian function, with the input value of FWHM, at each point
C in the SQUARE subarray.
C
C SUMG   contains the sum of the values of the Gaussian function over
C        the CIRCULAR area which will be used in the convolution.
C
C SUMGSQ contains the sum of the squares of the values of the Gaussian
C        function over the CIRCULAR area which will be used in the
C        convolution.
C
C PIXELS contains the number of pixels in the CIRCULAR area which will
C        be used in the convolution.
C
C DENOM  contains the denominator of the fraction defining  h.
C
C SGOP   contains [G]/n
C
C Using our knowledge of least squares, we can compute the standard
C error of the coefficient  h  in terms of the standard error of the
C brightness in a single pixel:
C
C      sigma**2(h) = sigma**2(1 pixel) / ([G**2] - [G]**2/n)
C
      RELERR=1.0/DENOM
      RELERR=SQRT(RELERR)

      CALL DAO_ALLOC( '_REAL', MAXSKY, IP1 )
      CALL DAO_ALLOC( '_REAL', MAXSKY, IP2 )
      CALL DAO_ALLOC( '_INTEGER', MAXSKY, IP3 )

      CALL SKY (%VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     .          %VAL(CNF_PVAL(IP3)), MAXSKY, HIBAD, READNS,
     .          PHPADU, SKYMOD, IX)

      CALL DAO_DEALL( IP3 )
      CALL DAO_DEALL( IP2 )
      CALL DAO_DEALL( IP1 )

      WRITE (6,610) RELERR
  610 FORMAT(23X, 'Relative error =', F5.2/)
C
C Now ask the user for a star-detection threshold and a bad pixel
C ceiling.
C
      CALL GETDAT ('Number of frames averaged, summed:', DATA, 2)
      IF ((DATA(1) .LT. 0.5) .OR. (DATA(2) .LT. 0.5)) RETURN
      READNS = OPT(1)**2*DATA(2)/DATA(1)
      PHPADU = OPT(2)*DATA(1)
      HMIN = SQRT(READNS + AMAX1(0.,SKYMOD)/PHPADU)
      LOBAD = 0.1*NINT(10.*(SKYMOD-OPT(3)*HMIN))
      HMIN = 0.01*NINT(100.*OPT(6)*RELERR*HMIN)
      READNS = SQRT(READNS)
C
C Later on, the threshold HMIN will be the minimum value of the local
C brightness enhancement that will be considered when searching for
C local maxima, and any pixel whose brightness value is less than LOBAD
C or greater than HIBAD will be ignored in the computations.
C
C Open the input and scratch disk files.
C
 2950 CALL GETNAM ('File for the positions:', COOFIL)
      IF ((COOFIL .EQ. 'END OF FILE') .OR.
     .     (COOFIL .EQ. 'GIVE UP')) THEN
         COOFIL = ' '
         RETURN
      END IF
C
C Open output data file for newly-discovered stars.
C
      COOFIL = EXTEND(COOFIL, CASE('coo'))
      CALL OUTFIL (3, COOFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//COOFIL)
         COOFIL = 'GIVE UP'
         GO TO 2950
      END IF
      IF (WATCH .GT. 0.5) THEN
         CALL TBLANK
         CALL OVRWRT('  Row', 1)
      END IF
      CONPIC=SWITCH(COOFIL, CASE('jnk'))
C     CALL COPPIC (CONPIC, D, NCOL, NROW, ISTAT)
      CALL COPPIC (CONPIC, ISTAT)
      IF (ISTAT .NE. 0) RETURN
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the raw image data in, holding only a few rows' worth of data in
C memory at any one time.  Convolve the data with the appropriate
C Gaussian function, and write the resulting numbers into the scratch
C disk picture.
C
C Only NBOX rows' worth of image data will be in memory at any one
C time.  The row numbered MIDDLE of the rows in memory is the one in
C which we will be looking for objects.  As we step through the image
C row by row, the data for the row MIDDLE-1 ( = NHALF) steps ahead of
C the new MIDDLE row will overwrite the data for the row MIDDLE
C ( = NHALF+1) steps behind.  This means that there is no practical
C upper limit to the size of the picture which can be run through
C this routine.
C
C First, load the first MIDDLE-1 rows of the cylinder buffer with
C an invalid brightness value.
C
      DO JY=1,MIDDLE-1
         DO IX=1,NCOL
            D(IX,JY) = VAL__MINR
         END DO
      END DO
C
C Now read the first NHALF rows into the cylinder buffer, putting
C row 1 of the image into row MIDDLE of the buffer.
C
      LX = 1
      LY = 1
      NROWS = NHALF
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .     D(1,MIDDLE), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading image data from disk file.')
         RETURN
      END IF
      NROWS=1
C
C The cylinder buffer D now contains the actual image data for the
C first NHALF rows of the picture.  We will soon create the file
C containing the derived values of  h  (see above) one row at a time.
C
C Now we will step through the picture row by row. JY remembers which
C row in the big picture we are working on.  For each row JY, the
C convolved data will be accumulated in the vector H(i,2), and then
C written into the JY-th row of the scratch picture.
C
      JY=0
 2020 JY=JY+1                              ! Increment image-row pointer
      IF (JY .GT. NROW) GO TO 2100         ! Have we reached the bottom?
C
C Note that at any given time we have only NBOX rows of the original
C image in memory, contained in the cylinder buffer D(i,j),
C j = 1, ..., NBOX, but not necessarily in that order.  For instance,
C if NBOX = 5, when JY = 1,
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     *    *    1    2    3     of the original picture which
C                                         is contained in
C      row:     1    2    3    4    5     of D.
C
C When row 1 of the picture is done, JY is set to 2, and row 4
C of the original picture is read into row 1 of the cylinder buffer,
C D, overwriting the null values which we put there before.
C Hence, when JY = 2,
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     *    1    2    3    4     of the original picture which
C                                         is contained in
C      row:     2    3    4    5    1     of D.
C
C As a final example, consider the situation for JY = 7:
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     5    6    7    8    9     of the original picture which
C                                         is contained in
C      row:     2    3    4    5    1     of D.
C
C In other words:
C
C      row:     1    2    3    4    5     variable J
C
C      row:     5    6    7    8    9     variable JY
C
C      row:     2    3    4    5    1     vector JCYLN(J)
C
C The cylinder buffer, D, just rolls down through the picture like a
C caterpillar tread, dropping off rows of data when they are no longer
C necessary and picking up new ones in their place.  The data are
C handled in this way (a) to minimize the amount of memory required,
C by storing only those rows that are immediately wanted, consistent
C with (b) minimizing the number of data transfers.  Now, for the
C CURRENT value of JY, which row of the cylinder buffer is to be fitted
C to each row of G?  The answers will be contained in the vector
C JCYLN.
C
C JCYLN(MIDDLE) is the row in the cylinder buffer where we will find
C the data for row JY of the big picture, which is to be fitted to row
C MIDDLE of G.  Similarly, JCYLN returns the position in the cylinder
C buffer of the row to be fitted to the J-th row of G
C (J = 1, ..., NBOX).
C
C Now that this is all straight, read in the data for row JY+NHALF
C (overwriting the data for row JY-NHALF-1, which is no longer needed).
C
      DO J=1,NBOX
         IY = JY + (J - MIDDLE)
C
C IY is that row of the big picture which is to be matched up against
C row J of the Gaussian function, during the convolution of this
C row JY of the big picture.
C
C Which row of the cylinder buffer contains row IY of the big picture?
C
         I = IY + NHALF
C
C I now represents the position that row IY of the big picture would
C have had in the cylinder buffer if the cylinder buffer were
C arbitrarily long, i.e. row 1 of the image in row 3 of D, row 2
C in row 4, row 3 in row 5, row 4 in row 6, ... in the examples
C above.  Now we wrap this around.
C
         JCYLN(J) = MOD(I-1,NBOX) + 1
      END DO
C
      LY = JY+NHALF
      IF (LY .LE. NROW) THEN
         CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .        D(1,JCYLN(NBOX)), ISTAT)
      ELSE
         K = JCYLN(NBOX)
         DO IX=1,NCOL
            D(IX,K) = VAL__MINR
         END DO
      END IF
C
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading image data from disk file.')
         RETURN
      END IF
C
C Compute the local brightness enhancement for each pixel in the row,
C The enhancement is computed from a circular region contained
C within an NBOX x NBOX array centered on the current pixel, using the
C array, G(I,J), and the constants SUMG, SUMGSQ, and PIXELS computed
C above.  (These constants will need to be modified if the circular
C region used in the calculation contains any bad pixels; we will use
C the variables SG, SGSQ, and P for temporary storage of these
C constants, and SGD and SD for the accumulation of [G*D] and [D] which
C are also needed.)
C
      DO 2050 JX=1,NCOL
C
      SGD=0.
      SD=0.
      SGSQ=SUMGSQ
      SG=SUMG
      P=PIXELS
C
      DO 2040 IX = JX-NHALF,JX+NHALF
        I = MIDDLE + (IX-JX)
        DO 2040 J=1,NBOX
          K = JCYLN(J)
          IF (SKIP(I,J)) GO TO 2040
            IF ((IX .GE. 1) .AND. (IX .LE. NCOL)) THEN
              DATUM = D(IX,K)
              IF ((DATUM .GE. LOBAD).AND.(DATUM .LE. HIBAD)) THEN
                SGD = SGD+G(I,J)*DATUM
                SD = SD+DATUM
                GO TO 2040
              END IF
            END IF
            SGSQ = SGSQ-G(I,J)**2
            SG = SG-G(I,J)
            P = P-1.
 2040 CONTINUE
C
C Compute the central height of the best fitting Gaussian function,
C temporarily storing it in the variable, then putting it into array
C element H(JX, 2).
C
      IF (P .GT. 1.5) THEN
         IF (P .LT. PIXELS) THEN
            SGSQ = SGSQ-(SG**2)/P
            IF (SGSQ .NE. 0.) THEN
               SGD = (SGD-SG*SD/P)/SGSQ
            ELSE
               SGD = 0.
            END IF
         ELSE
            SGD = (SGD-SGOP*SD)/DENOM
         END IF
      ELSE
         SGD = 0.
      END IF
      H(JX,2) = SGD
C
 2050 CONTINUE
C
C Write this newly-computed row of brightness enhancements to the
C scratch output picture.
C
      IF (WATCH .GT. 0.5) THEN
         WRITE (LINE,620) JY
  620    FORMAT(I5)
         CALL OVRWRT (LINE(1:5), 2)
      END IF
      CALL WRARAY ('COPY', LX, JY, NCOL, NROWS, MAXCOL, H(1,2), ISTAT)
      GO TO 2020
C
 2100 CONTINUE
      CALL OVRWRT (' ', 4)
C
C Later on, when we try to decide whether a local maximum represents
C a stellar profile or a delta function ( = bright bad pixel), we will
C compare the brightness of the central pixel to the average of the
C surrounding pixels.  To be ready for that, we here modify SKIP to
C skip over the central pixel, and set PIXELS equal to the number of
C pixels in the circular area not counting the central pixel.
C
      SKIP(MIDDLE,MIDDLE) = .TRUE.
      PIXELS = PIXELS-1.0
C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C Read in both the convolved data from the scratch disk file and the raw
C data from the original picture.  Search for local maxima in the
C convolved brightness data.  When these are found, compute image-shape
C statistics from the raw data to eliminate non-stellar brightness
C enhancements (as well as possible) and estimate the position of the
C centroid of the brightness enhancement.
C
 3000 CONTINUE
C
C Now the star search may begin.  The original image data will be read
C into the cylinder buffer D again, just as before.  At the same time,
C the brightness enhancements will be read from the scratch disk file
C into another cylinder buffer, H.  The brightness enhancements will
C then be searched for local maxima.  When these are found, functions
C of the original image data will be used to derive shape parameters
C designed to identify bad pixels and bad columns or rows.
C
      CALL WRHEAD (3, 1, NCOL, NROW, 6, LOBAD, HIBAD, HMIN,
     .     0., PHPADU, READNS, 0.)
      IF (WATCH .GT. 0.5) THEN
         CALL OVRWRT (' ', 4)
         WRITE (6,630)
  630    FORMAT(6X, '                              MAGS'/
     .     6X, '                              FROM '/
     .     6X, '       STAR     X      Y     LIMIT    SHARP    ROUND')
      END IF
C
C Read the first NHALF rows into each of the cylinder buffers,
C putting row 1 of the image into row MIDDLE of the buffer.
C
      DO JY=1,MIDDLE-1
         DO IX=1,NCOL
            D(IX,JY) = VAL__MINR
            H(IX,JY) = 0.
         END DO
      END DO
C
      LX = 1
      LY = 1
      NROWS = NHALF
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .     D(1,MIDDLE), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading image data from disk file.')
         CALL CLPIC ('COPY')
         RETURN
      END IF
      CALL RDARAY ('COPY', LX, LY, NCOL, NROWS, MAXCOL,
     .     H(1,MIDDLE), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading image data from scratch file.')
         CALL CLPIC ('COPY')
         RETURN
      END IF
      NROWS = 1
C
C Now step through the picture row by row.  Again JY is the image-row
C counter.
C
      NSTAR = 0
      JY = 0
 3020 JY = JY+1
C
C Have we just finished reducing the last row? If not, work on the
C next row.  If so, go on to Section 4.
C
      IF (JY .GT. NROW) GO TO 4000
C
C Determine the position in the cylinder buffers of all the rows
C contained in the box.
C
      DO J=1,NBOX
         IY = JY+(J-MIDDLE)+NHALF
         JCYLN(J) = MOD(IY-1,NBOX) + 1
      END DO
C
C Read in the data for row JY+NHALF.
C
      LY = JY+NHALF
      IF (LY .LE. NROW) THEN
         CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .        D(1,JCYLN(NBOX)), ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error reading image data from disk file.')
            CALL CLPIC ('COPY')
            RETURN
         END IF
         CALL RDARAY ('COPY', LX, LY, NCOL, NROWS, MAXCOL,
     .        H(1,JCYLN(NBOX)), ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error reading image data from scratch file.')
            CALL CLPIC ('COPY')
            RETURN
         END IF
      ELSE
         K = JCYLN(NBOX)
         DO IX=1,NCOL
            D(IX,K) = VAL__MINR
            H(IX,K) = 0.0
         END DO
      END IF
C
C Now step across the row, pixel by pixel.
C
      JX = 1
 3040 HEIGHT = H(JX,JCYLN(MIDDLE))
C
C Sieve to locate a local maximum in the brightness enhancement.  To
C be a local maximum, the brightness enhancement in a given pixel must
C be above the threshold, and it must also be greater than the
C brightness enhancement of any pixel within a radius equal to
C 1.5 sigma.
C
      IF (HEIGHT .LT. HMIN) GO TO 3300
      DO 3051 IX=JX-NHALF,JX+NHALF
         IF ((IX .LT. 1) .OR. (IX .GT. NCOL)) GO TO 3051
         I = MIDDLE + (IX-JX)
         DO 3050 J=1,NBOX
            K = JCYLN(J)
            IF (SKIP(I,J)) GO TO 3050
            IF (HEIGHT .LT. H(IX,K)) GO TO 3300
 3050    CONTINUE
 3051 CONTINUE
C
C The brightness enhancement of this pixel is now confirmed to be above
C the threshold, and to be larger than in any other pixel within a
C radius of 1.5 sigma.
C
C Now we derive the shape indices.  First, is the object much more
C sharply peaked than the input FWHM?  Compare the central pixel to
C the mean of the surrounding (non-bad) pixels.  If this difference is
C greater than the originally estimated height of the Gaussian or less
C than two-tenths the height of the Gaussian, reject the star
C (assuming SHRPLO and SHRPHI have the default values of 0.2 and
C 1.0; otherwise, muta mutandis.)
C
C
C ********** IF THE CENTRAL PIXEL IS BAD SKIP THIS TEST. **********
C
C
CD     TYPE *, JX, JY
CD     DO 1666 J=1,NBOX
CD1666 TYPE 6661, (JNINT(D(I,JCYLN(J))),
CD    .     I=MAX0(1,JX-NHALF),MIN0(NCOL,IX+NHALF)),
CD    .     (JNINT(H(I,JCYLN(J))), I=IX-NHALF,IX+NHALF)
CD6661 FORMAT(1X, <NBOX>I6, 1X, <NBOX>I6)
C
C As one final nuance, for this and subsequent calculations I propose
C to subtract off the modal sky level.  Otherwise, for faint stars on
C bright backgrounds in large boxes, it is barely possible that
C truncation error could affect the numerical results of the analysis.
C
      SHARP=0.
      DATUM=D(JX,JCYLN(MIDDLE))
      IF ((DATUM .LT. LOBAD) .OR. (DATUM .GT. HIBAD)) GO TO 3068
      P=0.
      DO 3061 IX=JX-NHALF,JX+NHALF
      IF ((IX .LT. 1) .OR. (IX .GT. NCOL)) GO TO 3061
         I = MIDDLE + (JX-IX)
         TEMP = 0.0
         DO 3060 J=1,NBOX
            K=JCYLN(J)
            IF (SKIP(I,J)) GO TO 3060
            DATUM=D(IX,K)
            IF ((DATUM .GE. LOBAD) .AND. (DATUM .LE. HIBAD)) THEN
               TEMP = TEMP+(DATUM-SKYMOD)
               P = P + 1.
            END IF
 3060    CONTINUE
         SHARP = SHARP+TEMP
 3061 CONTINUE
C
      SHARP=(D(JX,JCYLN(MIDDLE))-SKYMOD-SHARP/P)/HEIGHT
CD     TYPE *, ' SHARP= ', SHARP
      IF ((SHARP .LT. SHRPLO) .OR. (SHARP .GT. SHRPHI)) GO TO 3200
 3068 CONTINUE
C
C Now check to see whether the object is strongly elongated either
C along the row or along the column.  Compute the height of a Gaussian
C function of x and a Gaussian function of y by least-squares fits to
C the marginal distributions of the image data.  That is, fit the
C sum over y of the actual brightness values to the sum over y of the
C values of the array G, as functions of x.  If a bad pixel is found
C omit both the picture datum and the value of G for that pixel from
C their respective sums.  If the computed height of either the
C x-marginal or the y-marginal is non-positive, or if the central
c heights of the two marginals differ by more than their average
C (assuming that RNDLO and RNDHI have their default values
C of -1.0 and 1.0; otherwise, etc.), reject the star.
C
C We will now compute the height of the one-dimensional Gaussian
C distribution which best fits the x-marginal distribution of the
C brightness.  The equation which will be used will be the same as
C in the comments above ( h = ...) except that the symbol D in the
C equation now represents stands for the brightness data in the NBOX by
C NBOX square array summed over the y spatial direction, and the
C symbol G now stands for a one-dimensional Gaussian function (= the
C two-dimensional function G(i,j) also summed over the y spatial
C direction.  This sum is actually carried out numerically, rather
C than being done analytically, in order to permit the omission of
C "bad" pixels.)  At the same time, we will set up the necessary sums
C to permit the computation of a first-order correction to the centroid
C of the Gaussian profile in x:
C
C                -[G'*(D-G)]          [G*G']-[D*G']
C Delta x = -------------------- = -------------------,
C            [G'**2] - [G']**2/n   [G'**2] - [G']**2/n
C
C where G is the one-dimensional Gaussian profile, G' = (dG/dx), and
C D = the summed actual image data.  (There would normally be a
C [G']*[(D-G)]/n term in the numerator, but because G is already the
C "best fitting" Gaussian, [(D-G)] = 0.)  We will use
C
C SD      for the marginal sum of the actual image data
C                    (mnemonic:  "temporary sum of the data")
C SG      for the marginal sum of the 2-D Gaussian function
C                               ("temporary sum of the Gaussian")
C SUMGD   for [G*D]             ("sum of the Gaussian times the data")
C SUMG    for [G]               ("sum of the Gaussian")
C SUMD    for [D]               ("sum of the data")
C SUMGSQ  for [G**2]            ("sum of the Gaussian squared")
C SDGDX   for [G']              ("sum of d(Gaussian)/dx")
C SDGDXS  for [G'**2]           ("sum of {d(Gaussian)/dx}**2")
C SDDGDX  for [D*G']            ("sum of data times d(Gaussian)/dx")
C SGDGDX  for [G*G']            ("sum of Gaussian times d(Gaussian)/dx")
C
C In addition, for these calculations, pixels will arbitrarily be
C assigned weights ranging from unity at the corners of the box to
C MIDDLE**2 at the center (e.g. if NBOX = 5 or 7, the weights will be
C
C                                 1   2   3   4   3   2   1
C      1   2   3   2   1          2   4   6   8   6   4   2
C      2   4   6   4   2          3   6   9  12   9   6   3
C      3   6   9   6   3          4   8  12  16  12   8   4
C      2   4   6   4   2          3   6   9  12   9   6   3
C      1   2   3   2   1          2   4   6   8   6   4   2
C                                 1   2   3   4   3   2   1
C
C respectively).  This is done to desensitize the derived parameters to
C possible neighboring, brighter stars.
C
C The temporary variable P will be used to accumulate the sum of the
C weights, and N will count the number of points in the marginal
C distribution that actually get used.
C
C SKIP ALL THIS IF THE STAR IS TOO NEAR THE EDGE OF THE FRAME!
C
      IF ((JX .LT. MIDDLE) .OR. (JX .GT. LASTCL) .OR.
     .     (JY .LT. MIDDLE) .OR. (JY .GT. LASTRO)) THEN
         XCEN = REAL(JX)
         YCEN = REAL(JY)
         ROUND = 0.
         GO TO 3190
      END IF
C
      IX = JX-MIDDLE
C
      SUMGD=0.0
      SUMGSQ=0.0
      SUMG=0.0
      SUMD=0.0
      SDGDX=0.0
      SDGDXS=0.0
      SDDGDX=0.0
      SGDGDX=0.0
      P=0.
      N=0
      DO 3073 I=1,NBOX
         SG=0.
         SD=0.
         KX = IX+I
         DO 3070 J=1,NBOX
            WT=FLOAT(MIDDLE-ABS(J-MIDDLE))
            K=JCYLN(J)
            DATUM=D(KX,K)
            IF ((DATUM .GE. LOBAD) .AND. (DATUM .LE. HIBAD)) THEN
               SD=SD+(DATUM-SKYMOD)*WT
               SG=SG+G(I,J)*WT
            END IF
 3070    CONTINUE
         IF (SG .GT. 0.0) THEN
            WT=FLOAT(MIDDLE-ABS(I-MIDDLE))
            SUMGD=SUMGD+WT*SG*SD
            SUMGSQ=SUMGSQ+WT*SG**2
            SUMG=SUMG+WT*SG
            SUMD=SUMD+WT*SD
            P=P+WT
            N=N+1
            DGDX=SG*(MIDDLE-I)
            SDGDXS=SDGDXS+WT*DGDX**2
            SDGDX=SDGDX+WT*DGDX
            SDDGDX=SDDGDX+WT*SD*DGDX
            SGDGDX=SGDGDX+WT*SG*DGDX
         END IF
 3073 CONTINUE
C
C We need at least three points to estimate the height and position
C of the star, and the local sky brightness.
C
      IF (N .LE. 2) GO TO 3200
      HX=(SUMGD-SUMG*SUMD/P)/(SUMGSQ-(SUMG**2)/P)
C
C DX is the height of the best-fitting marginal Gaussian.  If this is
C non-positive, this is not an acceptable star.
C
      IF (HX .LE. 0.) GO TO 3200
C
C Compute the first-order correction to the x-centroid of the star.
C Note that a factor of HX/SIGSQ is missing from SDGDX, SDDGDX, and
C SGDGDX, and a factor of (HX/SIGSQ)**2 is missing from SDGDXS.
C
      SKYLVL=(SUMD-HX*SUMG)/P
      DX=(SGDGDX-(SDDGDX-SDGDX*(HX*SUMG+SKYLVL*P)))/(HX*SDGDXS/SIGSQ)
      XCEN=JX+DX/(1.+ABS(DX))
C
C If the best estimate of the star's center falls outside the image,
C reject it.
C
      IF ((XCEN .LT. 0.5) .OR. (XCEN .GT. NCOL+0.5)) GO TO 3200
C
C Compute the height of the y-marginal Gaussian distribution.
C
      SUMGD=0.
      SUMGSQ=0.
      SUMG=0.
      SUMD=0.
      SDGDX=0.
      SDGDXS=0.
      SDDGDX=0.
      SGDGDX=0.
      P=0.
      N=0
      DO 3078 J=1,NBOX
         K=JCYLN(J)
         SG=0.
         SD=0.
         DO 3076 I=1,NBOX
            WT=FLOAT(MIDDLE-ABS(I-MIDDLE))
            KX=IX+I
            DATUM=D(KX,K)
            IF ((DATUM .GE. LOBAD) .AND. (DATUM .LE. HIBAD)) THEN
               SD=SD+(DATUM-SKYMOD)*WT
               SG=SG+G(I,J)*WT
            END IF
 3076    CONTINUE
C
         IF (SG .GT. 0.0) THEN
            WT=FLOAT(MIDDLE-ABS(J-MIDDLE))
            SUMGD=SUMGD+WT*SG*SD
            SUMGSQ=SUMGSQ+WT*SG**2
            SUMG=SUMG+WT*SG
            SUMD=SUMD+WT*SD
            P=P+WT
            DGDX=SG*(MIDDLE-J)
            SDGDX=SDGDX+WT*DGDX
            SDGDXS=SDGDXS+WT*DGDX**2
            SDDGDX=SDDGDX+WT*SD*DGDX
            SGDGDX=SGDGDX+WT*SG*DGDX
            N=N+1
         END IF
C
 3078 CONTINUE
C
      IF (N .LE. 2) GO TO 3200
      HY=(SUMGD-SUMG*SUMD/P)/(SUMGSQ-(SUMG**2)/P)
      IF (HY .LE. 0.0) GO TO 3200
      SKYLVL=(SUMD-HY*SUMG)/P
      DY=(SGDGDX-(SDDGDX-SDGDX*(HY*SUMG+SKYLVL*P)))/(HY*SDGDXS/SIGSQ)
      YCEN=JY+DY/(1.+ABS(DY))
      IF ((YCEN .LT. 0.5) .OR. (YCEN .GT. NROW+0.5)) GO TO 3200
C
      ROUND=2.*(HX-HY)/(HX+HY)
CD     TYPE *, ' ROUND= ', ROUND
      IF ((ROUND .LT. RNDLO) .OR. (ROUND .GT. RNDHI)) GO TO 3200
C
C The fully verified and located star may now be dignified with its own
C ID number.
C
 3190 NSTAR=NSTAR+1
      HEIGHT=-2.5*ALOG10(HEIGHT/HMIN)
      IF (WATCH .GT. 0.5) WRITE (6,631) NSTAR, XCEN, YCEN, HEIGHT,
     .     SHARP, ROUND
  631 FORMAT(12X, I5, 2F7.1, F9.1, 2F9.2)
      WRITE (3,330) NSTAR, XCEN, YCEN, HEIGHT, SHARP, ROUND
  330 FORMAT(I6, 14F9.3)
 3200 CONTINUE
C
C If the sieve above (between statements 3040 and 3050) has detected a
C local maximum in the brightness enhancement, whether this enhancement
C was subsequently confirmed to be a star or not, then there is no need
C to check the other pixels in this row between JX+1 and JX+NHALF,
C inclusive, since we know there can't be a local maximum there.
C
      JX = JX+NHALF
 3300 JX = JX+1
C
C Have we passed the last pixel in the row?  If not, work on this
C pixel.  If so, go to next row.
C
      IF (JX .LE. NCOL) GO TO 3040
      GO TO 3020
C
C-----------------------------------------------------------------------
C
C SECTION 4
C
C Find out whether the user is happy.  If so, delete the scratch picture
C and close up shop.  If not, return to the beginning of Section 3.
C
 4000 IF (WATCH .LE. 0.5) WRITE (6,640) NSTAR
  640 FORMAT(//1X, I5, ' stars.')
      CALL CLFILE (3)
      CALL TBLANK                                    ! Type a blank line
      CALL TBLANK                                    ! Type a blank line
      CALL GETYN ('Are you happy with this?', ANSWER)
      IF ((ANSWER .EQ. 'Y') .OR. (ANSWER .EQ. 'E')) GO TO 9000
      WRITE (6,642) OPT(6), HMIN
  642 FORMAT(/' Your old threshold was', F8.2, ' sigma =',
     .     F8.2, ' ADU.'/)
      CALL GETDAT ('New threshold (in sigmas):', OPT(6), 1)
      IF (HMIN .LT. VAL__MINR) GO TO 9000              ! CTRL-Z was entered
 4030 CALL GETNAM ('Output file name:', COOFIL)
      IF ((COOFIL .EQ. 'END OF FILE') .OR.
     .     (COOFIL .EQ. 'GIVE UP')) THEN
         COOFIL = ' '
         GO TO 9000
      END IF
      COOFIL = EXTEND(COOFIL, CASE('coo'))
      CALL OUTFIL (3, COOFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//COOFIL)
         COOFIL = 'GIVE UP'
         GO TO 4030
      END IF
      HMIN = SQRT(READNS**2 + AMAX1(0.,SKYMOD)/PHPADU)
      HMIN = OPT(6)*RELERR*HMIN
      GO TO 3000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CONTINUE
      CALL CLPIC ('COPY')
      CALL DELPIC (CONPIC, ISTAT)
      RETURN
      END!
