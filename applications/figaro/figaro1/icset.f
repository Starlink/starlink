C+
      SUBROUTINE ICSET
C
C     I C S E T
C
C     Figaro function to set a selected region of an image to a constant
C     value.  This is a modification of the standard FIGARO program
C     NCSET to handle 2D data arrays (which in turn is a non-interactive
C     version of CSET, but has the possible advantage of allowing the
C     region to be specified precisely in terms of the X values of the
C     data).
C
C     This program overcomes the limitations of CLEAN or BCLEAN (qv)
C     which inherently assume the data to be modified is in the rows of
C     the input array. This may not be the case for CCD images, and
C     certainly not for 2D spectra where modification of data over a
C     specified wavelength region is most often required.
C
C     USES:
C
C     Anticipated uses are patching out of dud columns in a CCD image,
C     or removal of data over a specified wavelength range (whose X
C     limits are supplied by the user). The latter may be used to remove
C     residuals after less-than-perfect sky subtraction.
C
C     Command parameters -
C
C     IMAGE       (Character) The image to be modified.
C     YSTART      (Numeric) The Y-value of the start of the region.
C     YEND        (Numeric) The Y-value of the end of the region.
C     XSTART      (Numeric) The X-value of the start of the region.
C     XEND        (Numeric) The X-value of the end of the region.
C     VALUE       (Numeric) The value to use for the selected region.
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the input
C                 image, the data will be modified in situ.
C
C     Command keywords - None
C
C     User variables used -  None
C
C                                              KS / CIT 27th March 1985
C
C     Modified:
C
C     31st Jul 1987  DJA /AAO. Revised DSA_ routines - some specs
C                    changed. Dynamic memory handling now though DYN_
C                    routines.
C     16th Aug 1989  MAS / UoM. Modified to handle 2D images rather than
C                    1D spectra.
C     31st Jul 1991  HME / UoE. Accept also YSTART/YEND. Constant
C                    defaults to 0.
C     2nd  Aug 1991  HME / UoE.  Call it ICSET.
C     6th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TAB removed.
C     26th Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checks.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Limits for VALUE - close to the VAX number limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      NX, NY        ! Axis dimensions of data arrays
      INTEGER      IXEN          ! Last pixel to be set constant
      INTEGER      IXST          ! First  "   "  "   "      "
      INTEGER      IYEN          ! Last pixel to be set constant
      INTEGER      IYST          ! First  "   "  "   "      "
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! No. of elements in data array
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number outputdata array
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALUE         ! Temporary real number
      REAL         XMAX          !
      REAL         XMIN          !
      REAL         YMAX          !
      REAL         YMIN          !
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of image file, and open it.
C
      CALL DSA_INPUT('IN','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of input data array
C
      CALL DSA_DATA_SIZE('IN',2,NDIM,DIMS,NELM,STATUS)
      NX = DIMS( 1 )
      NY = NELM/NX
      IF (STATUS.NE.0) GO TO 500
C
C     Check that the input image really is 2D!
C
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER ('Data is not a 2D image',IGNORE)
         GO TO 500
      END IF
C
C     NOW Get YSTART and YEND (axis number 2)
C
      CALL DSA_AXIS_RANGE('IN',2,' ',.FALSE.,YMIN,YMAX,IYST,
     :                    IYEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     NOW Get XSTART and XEND (axis number 1)
C
      CALL DSA_AXIS_RANGE('IN',1,' ',.FALSE.,XMIN,XMAX,IXST,
     :                    IXEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value for CONSTANT
C
      CALL PAR_RDVAL('CONSTANT',FMIN,FMAX,0.,' ',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get name for output file
C
      CALL DSA_OUTPUT('OUT','OUTPUT','IN',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array to receive modified image
C
      CALL DSA_MAP_DATA('OUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Set the region to the constant value
C
      CALL FILL( NX, NY, IXST, IXEN, IYST, IYEN, VALUE,
     :           NELM, %VAL(CNF_PVAL(OPTR)) )
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
      END

C+
      SUBROUTINE FILL( NX, NY, I1, I2, I3, I4, VALUE,
     :                 NELM, OUTPUT_ARRAY)
C
C     F I L L
C
C     Sets a specified range of elements in a real array
C     to a constant value.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NX      (Integer) First array dimension
C     (>) NY      (Integer) Last array dimension
C     (>) I1      (Integer) First x element of the array to be set
C     (>) I2      (Integer) Last x element of the array to be set
C     (>) I3      (Integer) First y element of the array to be set
C     (>) I4      (Integer) Last y element of the array to be set
C     (>) NELM    (Integer) number of elements in data array
C     (>) VALUE   (Real) Value to which elements are to be set
C     (<) OUTPUT_ARRAY (Real array DATA(1..>=I2)) output array in question.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / AAO 27th March 1985
C                                        HME / UoE 30th July 1991
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER I1, I2, I3, I4, NELM, NX, NY, IX, IY
      REAL VALUE, OUTPUT_ARRAY( NX,NY )
C
C     Set region
C
      DO IY=I3,I4
         DO IX=I1,I2
            OUTPUT_ARRAY(IX,IY) = VALUE
         END DO
      END DO
      END
