C+
      SUBROUTINE CDIST
C
C     C D I S T
C
C     Performs an S-distortion correction for an image, using the
C     distortion analysis provided by SDIST.  If the SDIST analysis
C     was for a single spectrum, the the correction is '1-dimensional'
C     in that it consists simply of applying a shift in Y to the
C     data for each column of the image.  The shift is determined
C     separately for each column from the polynomial fitted by SDIST.
C     If the SDIST analysis was for more than one spectrum, the
C     correction is what might be termed '1.5-dimensional' - the
C     data is only redistributed within columns, but the amount of
C     shift varies along each column in a manner determined by fitting
C     a low order polynomial to the results of evaluating the
C     polynomials that SDIST fitted to each spectrum.  The 1.5D
C     algorithm actually reduces to the 1D case when there is only one
C     spectrum, but it simplifies things to think of them as distinct
C     cases.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the image to be corrected.
C
C     YSTART   (Numeric) The first Y value to be used.
C
C     YEND     (Numeric) The last Y value to be used.  Using YSTART
C              and YEND to limit the range of data corrected will
C              speed up the operation.
C
C     OUTPUT   (Character) The name of the resulting image.  This
C              can be the same as IMAGE, in which case the correction
C              will be performed in situ.
C
C     MAXDEGY  (Numeric) The maximum degree polynomial to fit to the
C              spectral positions in the case where there is more
C              than one spectrum covered by the SDIST analysis.
C
C     Command keywords -
C
C     ROTATE   If specified, the image to be corrected will be rotated
C              prior to the application of the correction.  This
C              minimises the number of page faults during the
C              correction, but at the expense of the overheads of the
C              rotation itself.
C
C     User variables - None
C
C     Input files -
C
C     SDIST.DAT contains the results of the fit(s), as written by
C               SDIST, in a format treated as follows -
C
C               3 header lines, all beginning with '*'
C               One line giving the number of spectra traced, in the
C               format 20X,I5.
C               Then, for each spectrum traced, one record giving
C               the spectrum number, and the leftmost and rightmost
C               pixels covered by the trace, in format
C               11X,I5,17X,I5,4X,I5, then 1 record giving the average
C               Y value in the spectrum, in format 16X,F13.7,
C               which is followed by 3 records giving the 11
C               polynomial coefficients for the fit, in 3D23.16.
C               Coefficients are given constant first, with any unused
C               coefficients set to zero.
C
C                                              KS / CIT 6th Feb 1984
C     Modified:
C
C     11th Nov 1986  KS / AAO.  ROTATE keyword and supporting code
C                    added.
C      7th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed.  Now uses DYN_ routines for dynamic-memory
C                    handling.
C      5th May 1988  KS / AAO.  FLOAT replaced by DBLE in one call to
C                    GEN_EPOLYD.  Bug pointed out by CKL/CIT.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     29th Mar 1991  KS / AAO. Fixed bug that was messing up the result
C                    image outside the Y bounds specified (ie if only
C                    a subset wqas corrected) if ROTATE was specified.
C                    Maximum number of spectra increased to 50 to match
C                    SDIST.
C     11th Sep 1992  HME / UoE, Starlink. Changed INCLUDE. TABs removed.
C                    Open input file with lowercase name, and get a
C                    free unit from DSA for it.
C                    Disable ROTATE option. Re-enable ROTATE.
C     12th Aug 1993  HME / UoE, Starlink.  FIG_CDIS2D still did not
C                    allow more than 20 spectra. Fixed that.
C     20th Mar 1996  HME / UoE, Starlink.  Add END= keywords to the READ
C                    statements. Before there were only ERR= keywords,
C                    but too short a file is not an error in that sense.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      INTEGER ICH_ENCODE, ICH_LEN
      INTEGER DSA_TYPESIZE
C
C     Maximum number of spectra that can be used
C
      INTEGER MAXSPEC
      PARAMETER (MAXSPEC=50)
C
C     Local variables
C
      DOUBLE PRECISION COEFFS(11,MAXSPEC) !
      INTEGER      DEGREES(MAXSPEC)!
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      FILE          ! Logical unit number of the input file
      LOGICAL      FOPEN         ! Logical unit number for input data
      INTEGER      I             !
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INVOKE        ! Used to invoke functions
      INTEGER      ISPECT        !
      INTEGER      IYEN          !
      INTEGER      IYST          !
      INTEGER      J             !
      INTEGER      MAXD          ! Maximum degree of polynomial to fit
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NBYTES        ! Number of bytes taken by 1 x-section
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NEXT          !
      INTEGER      NPIX          !
      INTEGER      NSPECT        !
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      PPTR          ! Dynamic-memory pointer to workspace
      LOGICAL      ROTATE        ! TRUE if ROTATE keyword was TRUE
      INTEGER      RPTR          ! Dynamic-memory pointer to workspace
      INTEGER      SPTR          ! Dynamic-memory pointer to workspace
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*64     ! Holds the output text
      REAL         VALUE         ! Temporary real number
      REAL         VMAX          ! Maximum value in Y data - not used
      REAL         VMIN          ! Minumum   "   "  "   "     "    "
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
      REAL         YBEST(MAXSPEC)!
      REAL         YPOSNS(MAXSPEC) !
      INTEGER      YPTR          ! Dynamic-memory pointer to workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial value
C
      FOPEN=.FALSE.
C
C     Get name of image and open it
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimensions of image data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Data is not 2-dimensional',IGNORE)
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Open and read distortion file data
C
      CALL DSA_GET_LU(FILE,STATUS)
      OPEN (UNIT=FILE,FILE='sdist.dat',STATUS='OLD',IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Unable to open distortion analysis file sdist.dat',STATUS)
         GO TO 500
      END IF
      FOPEN=.TRUE.
      READ (FILE,'(///20X,I5)',ERR=310,END=310,IOSTAT=STATUS) NSPECT
      DO ISPECT=1,NSPECT
         READ (FILE,'(/16X,F13.7)',ERR=310,END=310,IOSTAT=STATUS)
     :      YPOSNS(ISPECT)
         READ (FILE,'(3D23.16)',ERR=310,END=310,IOSTAT=STATUS)
     :      (COEFFS(J,ISPECT),J=1,11)
      END DO
      GO TO 320
C
  310 CONTINUE
         CALL PAR_WRUSER('I/O error from distortion file SDIST.DAT',
     :                                                       IGNORE)
         GO TO 500
  320 CONTINUE
C
C     See what degrees of fit were used
C
      DO ISPECT=1,NSPECT
         DEGREES(ISPECT)=0
         DO I=11,1,-1
            IF (COEFFS(I,ISPECT).NE.0.0) THEN
               DEGREES(ISPECT)=I-1
               GO TO 350
            END IF
         END DO
  350    CONTINUE
      END DO
C
C     Get limits on data range  (YSTART, YEND)
C
      CALL DSA_AXIS_RANGE('IMAGE',2,' ',.FALSE.,VMIN,VMAX,IYST,IYEN,
     :                                                       STATUS)
C
C     Get name of OUTPUT image
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     For 1.5D correction, get MAXDEGY
C
      IF (NSPECT.GT.1) THEN
         CALL PAR_RDVAL('MAXDEGY',0.,10.,5.,' ',VALUE)
         MAXD=VALUE
      END IF
C
C     See if data is to be rotated prior to correction.
C
      CALL PAR_RDKEY('ROTATE',.FALSE.,ROTATE)
C
C     Just so the user knows where all this came from..
C
      IF (NSPECT.EQ.1) THEN
         CALL PAR_WRUSER(
     :        'Correction is based on analysis of a single spectrum',
     :                                                         IGNORE)
      ELSE
         STRING='Correction is based on analysis of '
         INVOKE=ICH_ENCODE(STRING,FLOAT(NSPECT),36,0,NEXT)
         STRING(NEXT:)=' spectra'
         CALL PAR_WRUSER(STRING(:NEXT+7),IGNORE)
      END IF
C
C     Map the output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
C
C     Get workspace required.  The workspace varies depending on
C     the number of spectra analysed by SDIST.  In the 1D case,
C     a shift array NX elements long is needed, together with
C     a work array with twice as many elements as there are pixels
C     in the range YSTART through YEND.  In the 1.5D case, no shift
C     array is needed, but another work array of the same size as
C     the other is needed, as well as yet another double-precision
C     array with the same number of elements.  If rotation is used,
C     enough workspace for the rotated image is also required.
C
      NPIX=IYEN-IYST+1
      NBYTES=NELM*DSA_TYPESIZE('FLOAT',STATUS)

      CALL DSA_GET_WORK_ARRAY(NPIX,'DOUBLE',WPTR,WSLOT,STATUS)

      IF (NSPECT.EQ.1) THEN
         CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',SPTR,WSLOT,STATUS)
      ELSE
         CALL DSA_GET_WORK_ARRAY(NPIX,'DOUBLE',SPTR,WSLOT,STATUS)
         CALL DSA_GET_WORK_ARRAY(NPIX*2,'DOUBLE',PPTR,WSLOT,STATUS)
      END IF
      IF (ROTATE) THEN
         CALL DSA_GET_WORK_ARRAY(NELM,'FLOAT',RPTR,WSLOT,STATUS)
      END IF
C
C     If the data array is to be rotated, do that now.  Note that the
C     routines FIG_CDIS1D, FIG_CDIS2D have separate input and output
C     arrays, and they do not copy data that they don't correct
C     (outside the Y range specified) from input to output.  If we want
C     something sensible to appear in the uncorrected range - if we are
C     not correcting the whole range - we should make sure that the
C     output array contains a copy of the input we are going to give it.
C     (This only affects the rotated case - in the non-rotated case the
C     input and output arrays are the same.)
C
      IF (ROTATE) THEN
         CALL GEN_ROT2D(%VAL(CNF_PVAL(OPTR)),NX,NY,%VAL(CNF_PVAL(RPTR)))
         DPTR=RPTR
         IF ((IYST.NE.1).OR.(IYEN.NE.NY)) THEN
            CALL GEN_MOVE(NBYTES,%VAL(CNF_PVAL(RPTR)),
     :                    %VAL(CNF_PVAL(OPTR)))
         END IF
      ELSE
         DPTR=OPTR
      END IF
C
C     Now, tackle the two cases, 1D and 1.5D, separately.
C
      IF (NSPECT.EQ.1) THEN
C
C        --- 1D CASE ---
C
C        Fill up the shift array, using the coefficients from the
C        distortion file.
C
         CALL FIG_DISFIL(COEFFS,DEGREES(1),NX,%VAL(CNF_PVAL(SPTR)))
C
C        Perform the distortion correction
C
         CALL FIG_CDIS1D(%VAL(CNF_PVAL(DPTR)),NX,NY,ROTATE,IYST,IYEN,
     :                   %VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(WPTR)),
     :                   %VAL(CNF_PVAL(OPTR)))
      ELSE
C
C        --- 1.5D CASE ---
C
C        Work out the required positions for the various spectra
C        - note that all we really know is where they are, not where
C        they should be.  But we can guess..
C
         CALL FIG_YPGUES(NSPECT,YPOSNS,NX,COEFFS,DEGREES,YBEST)
C
C        Now perform the correction
C
         CALL FIG_CDIS2D(%VAL(CNF_PVAL(DPTR)),NX,NY,ROTATE,IYST,IYEN,
     :                   NSPECT,COEFFS,DEGREES,YPOSNS,YBEST,MAXD,
     :                   %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(SPTR)),
     :                   %VAL(CNF_PVAL(PPTR)),%VAL(CNF_PVAL(OPTR)))
      END IF
C
C     If data was rotated, rotate it back.  Note that the correction
C     routines have carefully positioned the data so that a single
C     rotate will get it into the correct position, and then it only
C     needs to be copied back into the output data array (the rotate
C     does not work on data in situ).
C
      IF (ROTATE) THEN
         CALL GEN_ROT2D(%VAL(CNF_PVAL(OPTR)),NY,NX,%VAL(CNF_PVAL(RPTR)))
         CALL GEN_MOVE(NBYTES,%VAL(CNF_PVAL(RPTR)),%VAL(CNF_PVAL(OPTR)))
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown output file and DSA_ routines
C
      IF (FOPEN) THEN
         CLOSE (UNIT=FILE,IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error closing down distortion file',IGNORE)
         END IF
      END IF
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_DISFIL (COEFFS,DEGREE,NX,SARRAY)
C
C     F I G _ D I S F I L
C
C     Utility for CDIST.  Fills the shift array with values
C     calculated from the distortion polynomial.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) COEFFS   (Double precision array COEFFS(DEGREE+1)) The
C                  polynomial coefficients to be used. Constant
C                  term should be first.
C     (>) DEGREE   (Integer) Degree of polynomial used.  Must be
C                  10 or less.
C     (>) NX       (Integer) Number of shift values to be calculated.
C     (<) SARRAY   (Real array SARRAY(NX)) Array to be filled with
C                  shift values.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     GEN_REVR8    (GEN_ package) Reverses a double precision array.
C     GEN_EPOLY    ( "     "    ) Evaluates a polynomial
C
C                                             KS / CIT 6th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER DEGREE,NX
      REAL SARRAY(NX)
      DOUBLE PRECISION COEFFS(DEGREE+1)
C
C     Functions used
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      LOGICAL SAME
      INTEGER I
      DOUBLE PRECISION RCOEFF(11)
C
C     Reverse the coefficients for GEN_EPOLYD
C
      SAME=.FALSE.
      CALL GEN_REVR8(COEFFS,DEGREE+1,1,SAME,RCOEFF)
C
C     Fill up the array
C
      DO I=1,NX
         SARRAY(I)=-GEN_EPOLYD(DBLE(I),RCOEFF,DEGREE+1)
      END DO
C
      END
C+
      SUBROUTINE FIG_CDIS1D(IN,NX,NY,ROTATE,IYST,IYEN,SARRAY,WORK,OUT)
C
C     F I G _ C D I S 1 D
C
C     Performs 1-D s-distortion correction on an array by shifting
C     each column by a specified amount.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN      (Real array IN(NX,NY)) The input image.
C     (>) NX      (Integer) First dimension of IN
C     (>) NY      (Integer) Second    "     "   "
C     (>) ROTATE  (Logical) True if input image has been rotated.
C     (>) IYST    (Integer) The first Y pixel number to be shifted
C     (>) IYEN    (Integer)  "  last  "   "     "     "  "    "
C     (>) SARRAY  (Real array SARRAY(NX)) The shifts to be applied
C     (>) WORK    (Real array WORK((IYEN-IYST+1)*2)) Workspace
C     (>) OUT     (Real array OUT(NX,NY)) The output image.  OUT and
C                 IN can be the same array in the calling routine.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIG_REBIN   (FIG_ package) Rebin data, in this case by shifting.
C
C                                             KS / CIT 6th Feb 1984
C     Modified:
C
C     11th Nov 1986.  KS / AAO.  ROTATE added.  IN is now accessed as a
C                     1D array to simplify the ROTATE code, as is OUT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ROTATE
      INTEGER NX,NY,IYST,IYEN
      REAL IN(NX*NY),SARRAY(NX),WORK((IYEN-IYST+1)*2),OUT(NX*NY)
C
C     Parameters for FIG_REBIN shift mode
C
      LOGICAL CFLUX, LOGW, LOGWR
      INTEGER IMODE, IQUAD, NADD
      REAL WAVES, WAVESR
      PARAMETER (CFLUX=.TRUE., LOGW=.FALSE., LOGWR=.FALSE.,
     :           IMODE=0, IQUAD=1, NADD=1, WAVES=0., WAVESR=0.)
C
C     Local variables
C
      INTEGER IBASE, IWPT, IX, IY
C
C     Loop through columns
C
      DO IX=1,NX
C
C        Get line to be shifted into work array.  Note that this routine
C        has to know the details of the operation performed by GET_ROT2D -
C        in particular it has to know the direction of the rotation.
C        See section at end of code for more details.
C
         IWPT=0
         IF (ROTATE) THEN
            IBASE=(IX*NY)+1
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               WORK(IWPT)=IN(IBASE-IY)       ! equivalent to IN(IX,IY)
            END DO
         ELSE
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               WORK(IWPT)=IN((IY-1)*NX+IX)   ! equivalent to IN(IX,IY)
            END DO
         END IF
C
C        Shift data - note, unshifted data is in first half of
C        WORK, shifted data appears in the second half.
C
         CALL FIG_REBIN(IMODE,IQUAD,WORK,IWPT,WORK(IWPT+1),IWPT,NADD,
     :                       SARRAY(IX),CFLUX,WAVES,WAVESR,LOGW,LOGWR)
C
C        Move data into output array.  If rotated, move it in such a way
C        that the correct data can be obtained by a single application
C        of GEN_ROT2D.
C
         IF (ROTATE) THEN
            IBASE=(NX-IX)*NY
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               OUT(IBASE+IY)=WORK(IWPT)            ! sets OUT(IX,IY)
            END DO
         ELSE
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               OUT((IY-1)*NX+IX)=WORK(IWPT)        ! sets OUT(IX,IY)
            END DO
         END IF
C
      END DO
C
C     Note: Access to rotated images.  GEN_ROT2D rotates an image in
C     an anti-clockwise direction, ie
C
C         |IX|
C     /\  ----------
C      |  |        |
C      |  |  *     | -                    <------NY------->
C      NY |        | |                    _________________ /\
C      |  |        | IY                   |              X|  |
C      |  |        | |     rotates to    _|               | NX
C      |  |       X| |                  IX|   *           |  |
C     \/  ---------- -                   ------------------ \/
C         <--NX--->                           |----IY-----|
C
C     When this routine accesses unrotated data, it simply wants the
C     (IX,IY) element of IN or OUT, which - since these are both treated
C     as 1D arrays, is just given by (IY-1)*NX+IX.  It is more complex
C     when IN represents an array that has been rotated, and OUT
C     represents an array that will be rotated.  In this case, this
C     routine wants to access the element in IN that was IN(IX,IY) in
C     the ORIGINAL unrotated image, and wants to set the element of
C     OUT that will become OUT(IX,IY) when OUT is rotated.
C
C     It may be obvious from the diagram above that if * represents
C     IN(IX,IY) in the original unrotated image, then its position
C     in the rotated image is going to be given by (IX-1)*NY + (NY-IY+1)
C     which is (elements in rows up to the row containing *) + (position
C     in row containing *), which reduces to (IX*NY)+1-IY which is
C     the expression IBASE-IY used in the code.  Similarly, consideration
C     of what happens when OUT is rotated AFTER this routine runs leads
C     to the expression (NX-IX)*NY+IY for the location of the element
C     which is destined to become OUT(IX,IY).
C
      END
C+
      SUBROUTINE FIG_YPGUES(NSPECT,YPOSNS,NX,COEFFS,DEGREES,YBEST)
C
C     F I G _ Y P G U E S
C
C     Given the average Y positions for the spectra analysed by
C     SDIST, and the polynomials it fitted to them, estimates the
C     actual positions for the straightened spectra.  This version
C     simply takes the average position of the central third of the
C     polynomial in each case.  The possibility that they should all
C     be equally spaced is a constraint that needs consideration.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NSPECT   (Integer) The number of spectra
C     (>) YPOSNS   (Real array YPOSNS(NSPECT)) The average positions
C                  of the spectra over the whole range.
C     (>) NX       (Integer) The row length of the image being corrected.
C     (>) COEFFS   (Double precision array (11,NSPECT)) The polynomial
C                  coefficients for the spectra.  For each spectrum,
C                  the constant term is the First.
C     (>) DEGREES  (Integer array DEGREES(NSPECT)) The degrees of the
C                  polynomials.
C     (<) YBEST    (Real array YBEST(NSPECT)) Where the spectra should
C                  be centered in Y.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_EPOLYD   (GEN_ package) Evaluate a double precision polynomial
C     GEN_REVR8    ( "     "    ) Reverse a double precision array.
C
C                                           KS / CIT 8th Feb 1984
C     Modified:
C
C     4th April 1985.  KS / AAO.  Coeffs were being used in wrong order.
C                      Call to GEN_REVR8 introduced.
C     5th May 1988.    KS / AAO.  FLOAT replaced by DBLE in call to
C                      GEN_EPOLYD.  Bug pointed out by CKL/CIT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NSPECT,NX,DEGREES(NSPECT)
      REAL YPOSNS(NSPECT),YBEST(NSPECT)
      DOUBLE PRECISION COEFFS(11,NSPECT)
C
C     Functions used
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      LOGICAL SAME
      INTEGER ISPECT, IX, IXEN, IXST
      REAL TOTAL
      DOUBLE PRECISION RCOEFF(11)
C
C     Limits on central third of spectra
C
      IXST=NX/3
      IXEN=IXST+IXST
C
C     Get averages over that range (note, coefficients have to be
C     reversed to get correct order for gen_epoly.)
C
      SAME=.FALSE.
      DO ISPECT=1,NSPECT
         TOTAL=0.0
         CALL GEN_REVR8(COEFFS(1,ISPECT),DEGREES(ISPECT)+1,1,
     :                                             SAME,RCOEFF)
         DO IX=IXST,IXEN
            TOTAL=TOTAL+GEN_EPOLYD(DBLE(IX),RCOEFF,
     :                                       DEGREES(ISPECT)+1)
         END DO
         YBEST(ISPECT)=TOTAL/FLOAT(IXEN-IXST+1)+YPOSNS(ISPECT)
      END DO
C
      END
C+
      SUBROUTINE FIG_CDIS2D(IN,NX,NY,ROTATE,IYST,IYEN,NSPECT,COEFFS,
     :                   DEGREES,YPOSNS,YBEST,MAXD,WORK,WORK2,P,OUT)
C
C     F I G _ C D I S 2 D
C
C     Performs the 2D (actually 1.5D) s-distortion correction.  Given
C     the polynomial fits and desired positions for the spectra
C     analysed by SDIST, shifts data in columns by an amount that
C     varies in a 2-D manner - ie with both column number and position
C     in column.  The variation over each column is determined by a
C     polynomial fit to the position for each spectrum given by its
C     polynomial against where it ought to be.  Is that clear?
C
C     Parameters -  (">" input, "<" output, "W" workspace)
C
C     (>) IN       (Real array IN(NX,NY)) The image to be corrected.
C     (>) NX       (Integer) The number of columns in IN.
C     (>) NY       (Integer) The number of rows in IN.
C     (>) ROTATE   (Logical) True if the image has been rotated.
C     (>) IYST     (Integer) The first row in the range to be fixed.
C     (>) IYEN     (Integer) The last row in the range to be fixed.
C     (>) NSPECT   (Integer) The number of distortion spectra.
C     (>) COEFFS   (Double precision COEFFS(11,NSPECT)) The polynomial
C                  coefficients.  For each spectrum, the constant term
C                  should be First.
C     (>) DEGREES  (Integer array DEGREES(NSPECT)) The degrees of the
C                  polynomials fitted to each of the spectra.
C     (>) YPOSNS   (Real array YPOSNS(NSPECT)) The average positions
C                  of the spectra over their whole range.
C     (>) YBEST    (Real array YBEST(NSPECT)) Where the spectra ought
C                  to be.
C     (>) MAXD     (Integer) The maximum degree polynomial to be
C                  fitted to the spectra.
C     (W) WORK     (Real array WORK((IYEN-IYST+1)*2)) Workspace.
C     (W) WORK2    (Real array WORK2((IYEN-IYST+1)*2)) Workspace.
C     (W) P        (Double precision array P((IYEN-IYST+1)*2))
C                  Workspace.
C     (<) OUT      (Real array OUT(NX,NY))
C
C     Common variables used - None
C
C
C                                          KS / CIT 10th Feb 1984
C     Modified:
C
C     4th April 1985.  KS / AAO.  Bug connected with use of degrees
C                      less than 10 fixed.  These now work.
C     4th April 1985.  KS / AAO.  Converted to use NAG routines.
C                      Calling sequence left unchanged, although P is
C                      now not used (its previous use was a mite suspect..)
C     11th Nov 1986.   KS / AAO.  ROTATE added.  IN and OUT are now treated
C                      as 1D arrays to simplify the ROTATE code.
C     12th Aug 1993.   HME / UoE, Starlink.  Comply with main routine
C                      and allow 50 spectra.
C      3rd Feb 1994.   HME / UoE, Starlink.  Use GEN_QFISORT (on YBEST
C                      instead of RPOSN) so that GEN_QDISORT can be discarded.
C      5th Apr 1995.   HME / UoE, Starlink.  No longer use NAG.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ROTATE
      INTEGER NX,NY,IYST,IYEN,NSPECT,DEGREES(NSPECT),MAXD
      REAL IN(NX*NY),YPOSNS(NSPECT),YBEST(NSPECT)
      REAL WORK((IYEN-IYST+1)*2),WORK2((IYEN-IYST+1)*2)
      REAL OUT(NX*NY)
      DOUBLE PRECISION COEFFS(11,NSPECT),P((IYEN-IYST+1)*2)
C
C     Functions
C
      REAL GEN_EPOLY
      DOUBLE PRECISION GEN_EPOLYD
C
C     Maximum degree possible for fit, and maximum number of spectra
C
      INTEGER MAXDEG, MAXSPEC
      PARAMETER (MAXDEG=10, MAXSPEC=50)
C
C     Local variables
C
      LOGICAL SAME
      INTEGER IBASE, I, ID, IFAIL, IFAIL2,
     :    IS, IWPT, IX, IY, MD, NDEG, NOFF
      INTEGER PTRS(MAXSPEC)
      REAL C(MAXDEG+1)
      DOUBLE PRECISION EPS, RFIT(MAXSPEC), A2(3*MAXSPEC+3*(MAXDEG+1))
      DOUBLE PRECISION CD(MAXDEG+1),
     :         DPOSN(MAXSPEC), RCOEFF(11,MAXSPEC), RPOSN(MAXSPEC),
     :         W(MAXSPEC), WK1(3*MAXSPEC)
C
C     Parameters for FIG_REBIN
C
      LOGICAL CFLUX, LOGW, LOGWR
      INTEGER IMODE, IQUAD, NADD
      REAL SHIFT
      PARAMETER (CFLUX=.TRUE.,LOGW=.FALSE.,LOGWR=.FALSE.,IMODE=1,
     :           IQUAD=1,NADD=1,SHIFT=0.0)
C
C     Get coefficients into right order for GEN_EPOLYD
C
      SAME=.FALSE.
      DO IS=1,NSPECT
         CALL GEN_REVR8(COEFFS(1,IS),DEGREES(IS)+1,1,SAME,RCOEFF(1,IS))
      END DO
C
C     Weight array never changes -
C
      DO IS=1,NSPECT
         W(IS)=1.0
      END DO
C
C     Loop through columns
C
      DO IX=1,NX
C
C        Fill up arrays with polynomial positions for spectra
C        and correct positions.
C
         DO IS=1,NSPECT
            DPOSN(IS)=GEN_EPOLYD(DBLE(IX),RCOEFF(1,IS),
     :                                DEGREES(IS)+1)+YPOSNS(IS)
            RPOSN(IS)=YBEST(IS)
         END DO
C
C        First, make sure points are ascending order of RPOSN, then
C        fit polynomial positions to correct positions, convert
C        from orthogonal coefficients to single precision power
C        series coeffs and reverse ready for GEN_EPOLY.
C
         CALL GEN_QFISORT(YBEST,NSPECT,PTRS)
         CALL GEN_DVSORT(PTRS,NSPECT,1,WK1,RPOSN)
         CALL GEN_DVSORT(PTRS,NSPECT,1,WK1,DPOSN)
         MD=MIN(MAXD,MAXDEG,NSPECT-1)
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NSPECT,RPOSN,DPOSN,W,MD,NDEG,EPS,RFIT,IFAIL,
     :      A2,IFAIL2)
         IF (NDEG.NE.MD.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
         ID=MD
         IFAIL2=0
         CALL PDA_DPCOEF(ID,0D0,CD,A2,IFAIL2)
         IF (IFAIL2.NE.0) GO TO 400
         DO I=1,ID+1
            C(I)=CD(I)
         END DO
         SAME=.TRUE.
         CALL GEN_REV2D(C,ID+1,1,SAME,C)
C
C        Fill work array with the distortion descriptors.
C        (note both work arrays are actually split in two halves: WORK
C        has its lower half filled with the distorted positions and
C        its top half filled with the correct positions - these are fed
C        to FIG_REBIN as 'wavelength' arrays to force the desired
C        rebinning.  WORK2 has its first half filled with the original
C        data and its top half is used for the rebinned data).  For
C        more details about access to rotated data, and an explanation
C        of the expression used for IBASE, see FIG_CDIS1D.
C
         NOFF=(IYEN-IYST+1)
         IWPT=0
         IF (ROTATE) THEN
            IBASE=IX*NY+1
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               WORK(IWPT+NOFF)=FLOAT(IY)
               WORK(IWPT)=GEN_EPOLY(FLOAT(IY),C,ID+1)
               WORK2(IWPT)=IN(IBASE-IY)
            END DO
         ELSE
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               WORK(IWPT+NOFF)=FLOAT(IY)
               WORK(IWPT)=GEN_EPOLY(FLOAT(IY),C,ID+1)
               WORK2(IWPT)=IN((IY-1)*NX+IX)
            END DO
         END IF
C
C        Rebin the data
C
         CALL FIG_REBIN(IMODE,IQUAD,WORK2,IWPT,WORK2(IWPT+1),IWPT,
     :                 NADD,SHIFT,CFLUX,WORK(IWPT+1),WORK,LOGW,LOGWR)
C
C        Then transfer over to output array
C
         IF (ROTATE) THEN
            IBASE=(NX-IX)*NY
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               OUT(IBASE+IY)=WORK2(IWPT)
            END DO
         ELSE
            DO IY=IYST,IYEN
               IWPT=IWPT+1
               OUT((IY-1)*NX+IX)=WORK2(IWPT)
            END DO
         END IF
C
      END DO
C
C     Bail out on NAG error comes here.
C
  400 CONTINUE
C
      END
