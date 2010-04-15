C+
      SUBROUTINE HIST
C
C     H I S T
C
C     Creates a one dimensional data structure containing a histogram
C     of the data distribution in another structure.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the data structure (not necessarily
C              two dimensional) to be histogrammed.
C     MINVAL   (Numeric) The minimum value for the histogram.
C     MAXVAL   (Numeric) The maximum value for the histogram.
C     BINS     (Numeric) The number of bins to be used for the histogram.
C     SPECTRUM (Character) The histogram produced from the input data.
C
C     Command keywords -  None
C
C                                     KS / CIT 29th June 1984
C     Modified:
C
C     02 Jan 1989  JM / RAL. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines
C     23 Jan 1991  JMS / AAO. Added PAR_ABORT to support user requested
C                  abort. Changed FMAX and FMIN to +/- 1.0E36 respectively,
C                  to allow the output spectrum to be plotted by SPLOT.
C     15 Feb 1991  KS / AAO. Added /NOCHECK to PAR_RDCHAR call to prevent
C                  it blocking the use of .SDF files.
C     05 Oct 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                  PAR_WRUSER rather than DSA_WRUSER.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Avoid _NAMED_ routines.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                  file names to 132 chars.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT in some places.
C     2005 June 7  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_CLEAN      ! Clip a string at first non-printing
                             ! character
      INTEGER ICH_ENCODE     ! Encodes a character string
C
C     Local variables
C
      CHARACTER CITEMS(2)*32 ! Axis character items retrieved
      INTEGER   DIMS(10)     ! Image dimensions
      REAL      DMAX         ! Upper limit of data range
      REAL      DMIN         ! Lower limit of data range
      CHARACTER IMAGE*132    ! Image filename
      INTEGER   IMPTR        ! Dynamic memory element for image data
      INTEGER   INVOKE       ! Used to format user messages
      INTEGER   IPTR         ! Used to format user messages
      INTEGER   NBINS        ! Number of bins in the histogram.
      INTEGER   NCITEMS      ! Number of axis character items retrieved
      INTEGER   NDIM         ! Number of image dimensions
      INTEGER   NELM         ! Number of elements in image - ignored
      INTEGER   NEXT         ! Used to format user messages
      DOUBLE PRECISION NITEMS(1)! Axis numeric items retrieved
      INTEGER   NNITEMS      ! Number of axis numeric items retrieved
      INTEGER   SLOT         ! Slot number for mapped data - ignored
      INTEGER   SPTR         ! Dynamic memory pointer for histogram
                             ! array
      INTEGER   STATUS       ! Running status for DSA_routines
      CHARACTER STRING*256   ! Used to format strings
      INTEGER   STRLEN       ! Length of a string
      CHARACTER UNITS*64     ! Units
      REAL      VALUE        ! NBINS as a REAL
      REAL      VMAX         ! Value at the center of last histogram
                             ! bin
      REAL      VMIN         ! Value at the center of first histogram
                             ! bin
      INTEGER   XPTR         ! Dynamic memory pointer for histogram
                             ! AXIS(1) array

C
C     Range limits (close to VAX floating point limits)
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.0E36, FMIN=-1.0E36)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      NNITEMS=0
      NCITEMS=2
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Open IMAGE file. (This is done with PAR_RDCHAR in order to
C     retrieve the name of the image for insertion in the title of the
C     output file. DSA_GET_ACTUAL_NAME returns the FULL VMS name
C     including the disk, directory etc of the file and is therefore
C     not really suitable.)
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_GET_ACTUAL_NAME ('IMAGE',IMAGE,STATUS)
C
C     Get the dimensions of the input data and the total
C     number of elements.
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the input data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IMPTR,SLOT,STATUS)
C
C     Get the data range, and data units, and tell the user.
C
      CALL DSA_GET_RANGE('IMAGE',DMIN,DMAX,STATUS)

      CALL DSA_GET_DATA_INFO('IMAGE',NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)
      IF(STATUS.NE.0)GOTO 500
      UNITS=CITEMS(1)
      STRLEN=ICH_CLEAN(UNITS)

      STRING='Data values '
      IPTR=13
      IF (STRLEN.GT.0) THEN
         STRING(IPTR:)='(in '//UNITS(:STRLEN)//') '
         IPTR=IPTR+6+STRLEN
      END IF
      STRING(IPTR:)='range from '
      INVOKE=ICH_ENCODE(STRING,DMIN,IPTR,3,NEXT)
      STRING(NEXT:)=' to '
      INVOKE=ICH_ENCODE(STRING,DMAX,NEXT+4,3,NEXT)
      CALL PAR_WRUSER(STRING(:NEXT),STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
C     Get the min and max values for the histogram range,
C     and number of bins.  (The 65536 as max number of bins is
C     quite arbitrary, as is the reset value.)
C
      CALL PAR_RDVAL('MINVAL',FMIN,FMAX,DMIN,UNITS,VMIN)
      CALL PAR_RDVAL('MAXVAL',VMIN,FMAX,DMAX,UNITS,VMAX)
      CALL PAR_RDVAL('BINS',1.0,65536.0,512.0,'bins',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      NBINS=VALUE
C
C     Get the name of the output file
C
      CALL DSA_OUTPUT('SPECT','SPECTRUM',' ',NO_DATA,NEW_FILE,STATUS)

      CALL DSA_COERCE_DATA_ARRAY('SPECT','FLOAT',1,NBINS,STATUS)
      CALL DSA_COERCE_AXIS_DATA('SPECT',1,'FLOAT',1,NBINS,STATUS)
C
      STRING='Histogram of data in '//IMAGE
      CALL DSA_SET_OBJECT('SPECT',STRING,STATUS)

      CITEMS(1)='Pixels'
      CITEMS(2)=' '
      CALL DSA_SET_DATA_INFO('SPECT',NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)

      CITEMS(1)=UNITS(:32)
      CITEMS(2)='Data values'
      CALL DSA_SET_AXIS_INFO('SPECT',1,NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)

C
C     Map the data arrays in the output structure.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'UPDATE','FLOAT',XPTR,
     :                       SLOT,STATUS)

      CALL DSA_MAP_DATA('SPECT','UPDATE','FLOAT',SPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Set the AXIS(1) data array
C
      CALL FIG_WFILL(VMIN,VMAX,.FALSE.,NBINS,%VAL(CNF_PVAL(XPTR)))
C
C     Work out the histogram, which goes into the output data array
C
      CALL FIG_CHIST(%VAL(CNF_PVAL(IMPTR)),NELM,VMIN,VMAX,NBINS,
     :               %VAL(CNF_PVAL(SPTR)))

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END

C+
      SUBROUTINE FIG_CHIST (DATA,NELM,VMIN,VMAX,NBINS,HIST)
C
C     F I G _ C H I S T
C
C     This routine generates a histogram of the data distribution
C     in an input array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) DATA     (Real array DATA(NELM)) The input data.
C     (>) NELM     (Integer) The number of elements in DATA.
C     (>) VMIN     (Real) The value at the center of the first
C                  histogram bin.
C     (>) VMAX     (Real) The value at the center of the last
C                  histogram bin.
C     (>) NBINS    (Integer) The number of bins in the histogram.
C     (<) HIST     (Real array HIST(NBINS)) Returns containing
C                  the distribution of counts between
C                  VMIN (corresponds to HIST(1)) and VMAX
C                  corresponds to HIST(NH))
C
C     Common variables used -  None
C
C     Functions / subroutines used - None
C
C                                    KS / AAO 10th Nov 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM,NBINS
      REAL VMIN,VMAX,HIST(NBINS),DATA(NELM)
C
C     Local variables
C
      INTEGER I,KBIN
      REAL INVWID,VALUE
C
      DO I=1,NBINS
         HIST(I)=0.
      END DO
      INVWID=FLOAT(NBINS-1)/(VMAX-VMIN)
      DO I=1,NELM
         VALUE=DATA(I)
         IF ((VALUE.LE.VMAX).AND.(VALUE.GE.VMIN)) THEN
            KBIN=NINT((VALUE-VMIN)*INVWID)+1
            IF ((KBIN.GE.1).AND.(KBIN.LE.NBINS)) THEN
               HIST(KBIN)=HIST(KBIN)+1.0
            END IF
         END IF
      END DO
C
      END
