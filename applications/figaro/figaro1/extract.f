C+
      SUBROUTINE EXTRACT
C
C     E X T R A C T
C
C     Adds a number of consecutive rows from an image to
C     produce a 1D data object.  (A 'row' is all the
C     pixels with a given y-value.)
C
C     Command parameters -
C
C     'IMAGE'    The name of the image from which the rows
C                are to be taken.
C
C     'YSTART'   The Y-value of the first row to be used.
C                If IMAGE has a Y axis structure, the data from this
C                is used.  If not, the row numbers are used,
C                starting from 1.
C
C     'YEND'     The Y-value of the last row to be used.
C
C     'SPECTRUM' The name of the resulting data.
C
C     Output data -
C
C     SPECTRUM is created with the same structure as IMAGE,
C     except that the data will only have one dimension, and if
C     IMAGE had a Y axis structure, this will be omitted.  Any X
C     axis structure will be copied unchanged.
C
C                                     KS / CIT 29th June 1984
C     Modified:
C
C     8th  Jul 1988  Rewritten to use DSA routines.  KS / AAO.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     19th Mar 1997  JJL / Southampton, Starlink. Error propagation included.
C     29th Jul 1997  MJCL / Starlink, UCL.  Initialised EXIST to .FALSE.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER   DIMS(2)            ! Image dimensions
      INTEGER   IPTR               ! Dynamic memory element for image data
      INTEGER   IVPTR              ! Dynamic memory element for image variance
      INTEGER   IYEN               ! Last image row to extract
      INTEGER   IYST               ! First image row to extract
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   NX                 ! First dimension of image
      INTEGER   NY                 ! Second dimension of image
      INTEGER   SLOT               ! Slot number for mapped data - ignored
      INTEGER   SPTR               ! Dynamic memory element for spectal data
      INTEGER   SVPTR              ! Dynamic memory element for spectal variance
      INTEGER   STATUS             ! Running status for DSA routines
      LOGICAL   EXIST              ! Does a Variance array exist?
      REAL      YEND               ! Last Y value used - ignored
      REAL      YSTART             ! First Y value used - ignored
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
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
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (NDIM.EQ.1) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
C
C     Get range of Y values.
C
      CALL DSA_AXIS_RANGE ('IMAGE',2,' ',.FALSE.,YSTART,YEND,
     :                                             IYST,IYEN,STATUS)
C
C     Check for the existance of errors
C
      EXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',EXIST,STATUS)
C
C     Create new spectrum file.
C
      CALL DSA_OUTPUT ('SPECT','SPECTRUM','IMAGE',NO_DATA,
     :                                              NEW_FILE,STATUS)
C
C     Create the new data and axis arrays in the spectrum file.
C
      CALL DSA_RESHAPE_DATA ('SPECT','IMAGE',1,NX,STATUS)
      CALL DSA_RESHAPE_AXIS ('SPECT',1,'IMAGE',1,1,NX,STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Map the variance array if needed
C
      IF (EXIST) THEN
          CALL DSA_MAP_VARIANCE ('IMAGE','READ','FLOAT',IVPTR,
     :                            SLOT,STATUS)
          CALL DSA_MAP_VARIANCE ('SPECT','WRITE','FLOAT',SVPTR,
     :                            SLOT, STATUS)
      END IF
C
C     Perform the extraction of the data
C
      CALL FIG_XTRACT(%VAL(CNF_PVAL(IPTR)),NX,NY,IYST,IYEN,
     :                %VAL(CNF_PVAL(SPTR)))

C
C     Perform the extraction of the variances if required
C
      IF (EXIST) THEN
      CALL FIG_XTRACT(%VAL(CNF_PVAL(IVPTR)),NX,NY,IYST,IYEN,
     :                %VAL(CNF_PVAL(SVPTR)))
      END IF
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
