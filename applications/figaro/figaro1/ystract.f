C+
      SUBROUTINE YSTRACT
C
C     Y S T R A C T
C
C     Adds a number of consecutive columns from an image to
C     produce a 1D data object.  (A 'column' is all the
C     pixels with a given AXIS(1) value.)
C
C     Command parameters -
C
C     'IMAGE'    The name of the image from which the rows
C                are to be taken.
C
C     'XSTART'   The AXIS(1)-value of the first row to be used.
C                If IMAGE has an AXIS(1) structure, the data from this
C                is used.  If not, the column numbers are used,
C                starting from 1.
C
C     'XEND'     The AXIS(1)-value of the last column to be used.
C
C     'SPECTRUM' The name of the resulting data.
C
C     Input data - an IMAGE
C
C     Output data -
C
C     SPECTRUM is created with the same structure as IMAGE,
C     except that the data-array will only have one dimension, and if
C     IMAGE has an AXIS(1) structure, this will be omitted.  Any AXIS(2)
C     structure will be copied unchanged.
C
C                                     KS / CIT 22nd March 1984
C     Modified:
C     21st Oct 1988  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER.
C     29th Sep 1994  HME / UoE, Starlink.  Map the output data for
C                    write access, not update.
C     1st April 1997 JJL/ Soton, Starlink. Handles Variances.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C
C     Local variables
C
      INTEGER   DIMS(2)          ! IMAGE dimensions
      LOGICAL   EXIST            ! Used to check for existence of
                                 ! AXIS(2) data
      INTEGER   IPTR             ! Dynamic-memory pointer to IMAGE data
      INTEGER   IXEN             ! Last image column to extract
      INTEGER   IXST             ! First image column to extract
      INTEGER   NDIM             ! Number of dimensions
      INTEGER   NELM             ! Number of elements - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   SLOT             ! Slot number - ignored
      INTEGER   SPTR             ! Dynamic-memory pointer to SPECT data
      INTEGER   STATUS           ! Running status for DSA_ routines
      INTEGER   SVPTR            ! Dynamic memory pointer to SPECT
                                 ! variances
      LOGICAL   VEXIST           ! Does IMAGE have a variance
                                 ! structure?
      INTEGER   VPTR             ! Dynamic-memory pointer to IMAGE
                                 ! variances
      REAL      XEND             ! Last AXIS(1) value used - ignored
      REAL      XSTART           ! First AXIS(1) value used - ignored
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      VEXIST=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get name of IMAGE file
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input data is not an image',STATUS)
         GO TO 500
      END IF
      NY=DIMS(2)
      NX=DIMS(1)
C
C     See if we have a data component giving AXIS(1)-values
C
      CALL DSA_AXIS_RANGE('IMAGE',1,' ',.FALSE.,XSTART,XEND,
     :                    IXST,IXEN,STATUS)

C
C     Check to see if a variance structure exists.
C
      CALL DSA_SEEK_VARIANCE('IMAGE',VEXIST,STATUS)
C
C     Create spectrum file modelled on IMAGE but without axes
C     and data structures
C
      CALL DSA_OUTPUT('SPECT','SPECTRUM','IMAGE',NO_DATA,
     :                 NEW_FILE,STATUS)
C
C     Now create the  data component in the output structure
C
      CALL DSA_RESHAPE_DATA('SPECT','IMAGE',1,NY,STATUS)
C
C     Map the input and output data, and their variances if needed
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',VPTR,SLOT,STATUS)
      END IF

      CALL DSA_MAP_DATA('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('SPECT','WRITE','FLOAT',SVPTR,SLOT,
     :                         STATUS)
      END IF
      IF(STATUS.NE.0)GOTO 500

C     Perform the extraction
C
      CALL FIG_YTRACT(%VAL(CNF_PVAL(IPTR)),NX,NY,IXST,IXEN,
     :                %VAL(CNF_PVAL(SPTR)))
C
C     If variances exist, pass these through FIG_YTRACT as well.
C
      IF (VEXIST) THEN
         CALL FIG_YTRACT(%VAL(CNF_PVAL(VPTR)),NX,NY,IXST,IXEN,
     :                   %VAL(CNF_PVAL(SVPTR)))
      END IF

C
C     If the original image had an AXIS(2) structure, then this
C     should be copied into the SPECTRUM AXIS(1) structure.
C
      CALL DSA_SEEK_AXIS('IMAGE',2,EXIST,STATUS)
      IF(EXIST) THEN
         CALL DSA_AXIS_SIZE('IMAGE',2,2,NDIM,DIMS,NELM,STATUS)
         CALL DSA_RESHAPE_AXIS('SPECT',1,'IMAGE',2,NDIM,DIMS,STATUS)
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
