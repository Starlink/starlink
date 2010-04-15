C+
      SUBROUTINE CMPLXFILT
C
C     C M P L X F I L T
C
C     This routine produces a mid-pass complex filter, given a complex
C     structure as a template and low and high cutoff values.  The
C     filter is produced by the subtraction of two gaussians.  If no
C     low value is specified, the result is a single gaussian low-pass
C     filter.
C
C     Command parameters -
C
C     CDATA    (Character) The name of the template complex structure.
C     LOWCUT   (Numeric) The low cutoff value for the filter.  This is
C              specified in terms of the Nyquist frequency.  It is the
C              sigma of the low cut gaussian, ie the point at which the
C              rising edge of the filter reaches exp(-1/2) =~.6
C     HICUT    (Numeric) The high cutoff value for the filter.  This is
C              specified in terms of the Nyquist frequency.  It is the
C              sigma of the high cut gaussian, ie the point at which the
C              falling edge of the filter reaches exp(-1/2) =~.6
C     OUTPUT   (Character) The name of the resulting structure.  This
C              may be the same as CDATA, in which case the operation
C              is performed in situ.  Otherwise, a new file is created.
C
C     Command keywords - None
C
C     Output data -
C
C     The resulting complex data has a zero imaginary part, and a real
C     part given by F(X)=exp(-X**2/(2*V**2))-exp(-X**2/(2*U**2))
C     where U and V are the low and high frequency cutoffs respectively.
C     (Note that the actual data generated is symmetrical about the
C     mid point of the data, which is assumed to be the zero frequency
C     - the Figaro function FFT produces data in this form).
C
C                                         KS / AAO  9th Oct 1986.
C     Modified:
C
C     20th Feb 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     30th Mar 1991  KS / AAO. Removed source of FIG_CMPCHK - this
C                    common routine is now in the FIG library.
C     13th Dec 1991  HME / UoE, Starlink. Re-appended source of
C                    FIG_CMPCHK, because its object module had not
C                    made it into the library and shareable image.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Remove the
C                    source of FIG_CMPCHK again.
C     13th Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                    Map complex in single call.
C     29th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER   BDOUB            ! Number of bytes per item of type
                                 ! 'DOUBLE'
      INTEGER   DIMS(10)         ! Image dimensions
      LOGICAL   FAULT            ! Data are not valid complex structure
      REAL      HICUT            ! The low cutoff value for the filter
      INTEGER   IPTR             ! Dynamic-memory pointer for imaginary
                                 ! data
      REAL      LOWCUT           ! The low cutoff value for the filter
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image
      INTEGER   RPTR             ! Dynamic memory element for real data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines

C
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get value of CDATA and open file
C
      CALL DSA_INPUT ('CDATA','CDATA',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Check that this is a valid complex structure and get its
C     dimensions.
C
      CALL FIG_CMPCHK('CDATA',10,NDIM,DIMS,FAULT)
      IF (FAULT) GO TO 500
      CALL DSA_DATA_SIZE ('CDATA',10,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get the values of the low and high cutoffs.
C
      CALL PAR_RDVAL('LOWCUT',0.0,1.0,.1,'Nyquist frequency',LOWCUT)
      CALL PAR_RDVAL('HICUT',LOWCUT,1.0,.5,'Nyquist frequency',HICUT)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get name of OUTPUT structure, and if necessary create it by
C     copying the input structure.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','CDATA',0,0,STATUS)
C
C     Map the data arrays.
C
C     CALL DSA_MAP_IMAGINARY('OUTPUT','UPDATE','DOUBLE',IPTR,SLOT,
C    :                       STATUS)
C     CALL DSA_MAP_DATA('OUTPUT','UPDATE','DOUBLE',RPTR,SLOT,STATUS)
      CALL DSA_MAP_COMPLEX('OUTPUT','UPDATE','DOUBLE',
     :                     RPTR,IPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Zero out the imaginary array.
C
      BDOUB=DSA_TYPESIZE('DOUBLE',STATUS)
      IF(STATUS.NE.0)GOTO 500
      CALL GEN_FILL(NELM*BDOUB,0,%VAL(CNF_PVAL(IPTR)))
C
C     Set the real array values
C
      CALL FIG_RFILTV(NELM,NDIM,DIMS,LOWCUT,HICUT,%VAL(CNF_PVAL(RPTR)))

  500 CONTINUE

      IF (FAULT) CALL FIG_SETERR
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)

      END
C+
      SUBROUTINE FIG_RFILTV(NELMS,NDIM,DIMS,LOWCUT,HICUT,RDATA)
C
C     F I G _ R F I L T
C
C     CMPLXFILT utility.  Generates the mid-pass filter given the low
C     and high cutoff values.
C
C     Parameters -   (">" input, "<" output)
C
C     NELMS   (Integer) The number of elements in the data
C     NDIM    (Integer) The number of dimensions in the data (<=10)
C     DIMS    (Integer array DIMS(NDIM)) The dimensions of the data
C     LOWCUT  (Real) The low cutoff for the filter
C     HICUT   (Real) The high cutoff for the filter
C     RDATA   (Double precision array, RDATA(NELMS)) The real part of the
C             complex data that forms the filter.  Note that while RDATA
C             is passed as a linear array, it is treated as having
C             dimensions as specified by NDIM and DIMS.
C
C     Common variables used - None
C
C                                             KS / AAO 9th Oct 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELMS, NDIM, DIMS(NDIM)
      REAL LOWCUT, HICUT
      DOUBLE PRECISION RDATA(NELMS)
C
C     Local variables
C
      LOGICAL MORE, REPEAT
      INTEGER I, IDIM, IDIMS(10), ISTART, NELM
      REAL D, FACTOR, HIHI2, LOLO2, X, ZEROS(10)
C
C     Note: This routine could be sped up considerably in the NDIM>1
C     cases by calculating the 1D strip values first and storing them
C     in work space instead of re-calculating them afresh for each
C     strip.  If a lot of nD work is going to be done, that would
C     be worth while.
C
C     Initial setup.  The ZEROS array gives the element number in
C     each dimension that corresponds to zero frequency (for the case
C     where an axis has an even number of elements, the apparently
C     arbitrary choice of the lower of the two central elements comes
C     from knowing something of how the Figaro FFT routine works).
C
      DO IDIM=1,NDIM
         ZEROS(IDIM)=FLOAT((DIMS(IDIM)+1)/2)
      END DO
      HIHI2=HICUT*HICUT*2.0
      LOLO2=LOWCUT*LOWCUT*2.0
      DO IDIM=1,NDIM
         IDIMS(IDIM)=1
      END DO
      MORE=.TRUE.
      DO WHILE (MORE)
C
C        At this point IDIMS indicate an element of the array.  It
C        is an element at the start of one of the 1D strips of the array
C        ie IDIMS(1)=1.  We want to know a) the element number of DATA
C        (treated as a 1D array) that this corresponds to (ISTART), and
C        we also want the multiplicative factor to be applied to this
C        strip because of its position in the higher dimensions.
C
         ISTART=1
         NELM=DIMS(1)
         DO IDIM=2,NDIM
            ISTART=ISTART+(IDIMS(IDIM)-1)*NELM
            NELM=NELM*DIMS(IDIM)
         END DO
         FACTOR=1.0
         DO IDIM=2,NDIM
            D=(FLOAT(IDIMS(IDIM))-ZEROS(IDIM))*2.0/DIMS(IDIM)
            IF ((HICUT.NE.0.0).AND.(LOWCUT.NE.0)) THEN
               FACTOR=FACTOR*(EXP(-D*D/HIHI2)-EXP(-D*D/LOLO2))
            ELSE IF (LOWCUT.NE.0.0) THEN
               FACTOR=FACTOR*EXP(-D*D/LOLO2)
            ELSE IF (HICUT.NE.0.0) THEN
               FACTOR=FACTOR*EXP(-D*D/HIHI2)
            END IF
         END DO
C
C        Now we calculate the filter values along the 1D strip.
C        For efficiency at this point we treat the various possible
C        combinations of HICUT=0 and LOWCUT=0 separately.
C
         IF ((HICUT.NE.0.0).AND.(LOWCUT.NE.0.0)) THEN
C
C           Mid - pass filter case
C
            DO I=1,DIMS(1)
               X=(FLOAT(I)-ZEROS(1))*2.0/DIMS(1)
               RDATA(I+ISTART-1)=
     :                 (EXP(-X*X/HIHI2)-EXP(-X*X/LOLO2))*FACTOR
            END DO
C
         ELSE IF (LOWCUT.NE.0.0) THEN
C
C           No high cut specified.
C
            DO I=1,DIMS(1)
               X=(FLOAT(I)-ZEROS(1))*2.0/DIMS(1)
               RDATA(I+ISTART-1)=-EXP(-X*X/LOLO2)*FACTOR
            END DO
C
         ELSE IF (HICUT.NE.0.0) THEN
C
C           No low cutoff specified.
C
            DO I=1,DIMS(1)
               X=(FLOAT(I)-ZEROS(1))*2.0/DIMS(1)
               RDATA(I+ISTART-1)=EXP(-X*X/HIHI2)*FACTOR
            END DO
C
         ELSE
C
C           Silly case - both zero.  Still, we have to return something
C
            DO I=1,DIMS(1)
               RDATA(I+ISTART-1)=FACTOR
            END DO
         END IF
C
C        Now increment IDIMS to point to the start of the next 1D strip
C
         IDIM=2
         REPEAT=IDIM.LE.NDIM
         MORE=REPEAT
         DO WHILE (REPEAT)
            IDIMS(IDIM)=IDIMS(IDIM)+1
            IF (IDIMS(IDIM).GT.DIMS(IDIM)) THEN
               IDIMS(IDIM)=1
               IDIM=IDIM+1
               REPEAT=IDIM.LE.NDIM
               MORE=REPEAT
            ELSE
               REPEAT=.FALSE.
            END IF
         END DO
      END DO
C
      END
