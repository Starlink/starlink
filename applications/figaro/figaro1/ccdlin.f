C+
      SUBROUTINE CCDLIN
C
C     C C D L I N
C
C     Applies a linearity correction to data from the AAO RCA CCD.
C     The correction applied is that given by John Barton (RCA#5
C     Non-Linearity Correction, AAO Internal Document).
C
C     Command parameters -
C
C     IMAGE   (Character) The name of the structure containing the image.
C
C     ALPHA   (Numeric) The value of the constant alpha in the expression
C             giving the linearity correction.
C
C     CBIAS   (Numeric) The value of the bias level to be applied when
C             making the correction.  This is not a particularly critical
C             parameter.
C
C     OUTPUT  (Character) The name of the result of the operation.  This
C             can be the same as for IMAGE.  If not, a new structure
C             is created, with everything but the data a direct
C             copy of the input.
C
C                                      KS / AAO 10th Sept 1986
C     Modified:
C
C     27th Jun 1989  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     24th Jan 1991  JMS / AAO. Modified to check if FITS structures
C                    exist before prompting user. Set Max and Min values
C                    to +/- 3.0E18 respec. so as not to cause a floating
C                    point error in FIG_RCALIN.
C     18th Feb 1991  JMS / AAO. Corrected an error - now uses ELEMENTS
C                    instead of NELM when calling DSA_SEEK_FITS.
C     25th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_CLEAN
      INTEGER ICH_ENCODE
C
C     Local variables
C
      CHARACTER ACCESS*1  ! Single FITS character that determines routine used.
      REAL      ALPHA     ! the alpha used to evaluate the linearity correction.
      REAL      CBIAS     ! The value of the bias level to be applied
      CHARACTER COMENT    ! Comment associated with FITS item
      INTEGER   DIMS(10)  ! Image dimensions
      INTEGER   ELEMENTS  ! The number of elements in a FITS structure
      LOGICAL   EXIST1    ! True if item returned by DSA_SEEK_FITS exists
      LOGICAL   EXIST2    ! True if item returned by DSA_SEEK_FITS exists
      INTEGER   INVOKE    ! Used to format user messages
      INTEGER   IPTR      ! Dynamic memory element for image data
      INTEGER   LENGTH    ! Used to format user messages
      INTEGER   NDIM      ! Number of image dimensions
      INTEGER   NELM      ! # of elements in image & FITS structure- ignored
      INTEGER   NEXT      ! Used to format user messages
      INTEGER   PSTAT     ! Status for PAR routines
      INTEGER   SLOT      ! Slot number for mapped data - ignored
      INTEGER   STATUS    ! Running status for DSA routines
      CHARACTER STRING*80 ! Used to format user messages
      INTEGER   STRLEN    ! The number of characters returned by DSA_SEEK_FITS
C
C     Numeric parameter limits - set so as not to cause floating point
C     errors in FIG_RCALIN
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=3.0E18, FMIN=-3.0E18)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      EXIST1=.FALSE.
      EXIST2=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C

C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     See if there is any indication that this data has already been
C     corrected.
C
      CALL PAR_WRUSER(' ',PSTAT)
      CALL DSA_SEEK_FITS('IMAGE','INSTRUME',EXIST1,ACCESS,ELEMENTS,
     :                                                   STRLEN,STATUS)
      IF (EXIST1) THEN
         CALL DSA_GET_FITS_C('IMAGE','INSTRUME',0,STRING,COMENT,STATUS)
         LENGTH=ICH_CLEAN(STRING)
         CALL PAR_WRUSER('Data is from '//STRING(:LENGTH),PSTAT)
      ELSE
         CALL PAR_WRUSER('Warning - Unable to find ".FITS.INSTRUME" '//
     :                                'in the data structure.',PSTAT)
         CALL PAR_WRUSER('This may not be suitable data to which to '//
     :                              'apply this correction.',PSTAT)
         PSTAT=0
      END IF
C
      CALL PAR_WRUSER(' ',PSTAT)
C
      CALL DSA_SEEK_FITS('IMAGE','CCD_ALFA',EXIST1,ACCESS,ELEMENTS,
     :                                                   STRLEN,STATUS)
      IF (EXIST1) THEN
         CALL DSA_GET_FITS_F('IMAGE','CCD_ALFA',0,ALPHA,COMENT,STATUS)
         CALL DSA_SEEK_FITS('IMAGE','CCD_BIAS',EXIST2,ACCESS,
     :                                          ELEMENTS,STRLEN,STATUS)
         IF (EXIST2) THEN
            CALL DSA_GET_FITS_F('IMAGE','CCD_BIAS',0,CBIAS,COMENT,
     :                                                          STATUS)
            CALL PAR_WRUSER(
     :        'Warning - Data appears to have already been corrected,',
     :                                                          PSTAT)
            STRING='using an alpha of '
            INVOKE=ICH_ENCODE(STRING,ALPHA,19,7,NEXT)
            STRING(NEXT:)=' and a bias of '
            INVOKE=ICH_ENCODE(STRING,CBIAS,NEXT+15,7,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),PSTAT)
            CALL PAR_WRUSER(' ',PSTAT)
         END IF
      END IF
      STATUS=0
C
C     Get value of ALPHA and CBIAS
C
      CALL PAR_RDVAL('ALPHA',FMIN,FMAX,3.16E-6,' ',ALPHA)
      CALL PAR_RDVAL('CBIAS',FMIN,FMAX,180.0,' ',CBIAS)
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,NEW_FILE,STATUS)
C
C     Map data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',IPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Explain what's going to happen
C
      CALL PAR_WRUSER(' ',PSTAT)
      STRING='Correction will be applied using an alpha of '
      INVOKE=ICH_ENCODE(STRING,ALPHA,46,7,NEXT)
      CALL PAR_WRUSER(STRING(:NEXT),PSTAT)
      STRING='and a bias of '
      INVOKE=ICH_ENCODE(STRING,CBIAS,15,7,NEXT)
      CALL PAR_WRUSER(STRING(:NEXT),PSTAT)
      CALL PAR_WRUSER(' ',PSTAT)
C
C     Apply the linearity correction.
C
      CALL FIG_RCALIN(%VAL(CNF_PVAL(IPTR)),NELM,ALPHA,CBIAS)
C
C     Flag the data as having been corrected.
C
      CALL DSA_PUT_FITS_F('OUTPUT','CCD_ALFA',ALPHA,' ',STATUS)
      CALL DSA_PUT_FITS_F('OUTPUT','CCD_BIAS',CBIAS,' ',STATUS)

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_RCALIN (DATA,NELM,ALPHA,CBIAS)
C
C     F I G _ R C A L I N
C
C     Applies a linearity correction to AAO RCA CCD data.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (!) DATA    (Real array DATA(NELM)) The data to be corrected.
C                 Note that although this is treated here as a linear
C                 array (no pun intended) it can in fact have any
C                 dimensions.
C     (>) NELM    (Integer) The number of elements in DATA.
C     (>) ALPHA   (Real) The constant alpha used in the linearity
C                 correction.
C     (>) CBIAS   (Real) The bias value to be assumed in applying the
C                 linearity correction.
C
C     Common variables used - None.
C
C     Functions / subroutines used - None.
C
C                                               KS / AAO 10th Sept 1986.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL DATA(NELM), ALPHA, CBIAS
C
C     Local variables
C
      INTEGER I
C
C     John Barton's memo gives the linearity correction as
C     M=C*(1+alpha*C) where M is the measured intensity and C is the
C     corrected intensity (both with the bias subtracted).  The code
C     below solves this quadratic individually for each pixel, and
C     is less than ideal.  The way the bias is treated may lead to
C     rounding constant s, although with the expected values this is
C     unlikely.
C
      DO I=1,NELM
         DATA(I)=CBIAS+
     :     (SQRT(ABS(1.0+4.0*ALPHA*(DATA(I)-CBIAS)))-1.0)/(2.0*ALPHA)
      END DO
C
      END
