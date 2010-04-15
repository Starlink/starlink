C+
      SUBROUTINE MASKEXT
C
C     M A S K E X T
C
C     Extracts spectra from an image using a mask image.  The mask
C     pixel values should be the numbers of the spectra that fall
C     in that pixel of the image.  This routine is intended for use
C     with echelle data, and the numbers in the image are expected
C     to be based on echelle order numbers.  The spectra are extracted
C     into an image whose Y-axis is the order number of the spectra,
C     and whose X-axis is that of the original data.  The actual
C     numbers in the image are expected to have the form
C     (echelle order number)*10 + (sub order number), sub orders
C     referring to separate spectra from the same order, as
C     produced, for example, by the UCL echelle at AAO.
C     MASKEXT will extract one specified sub-order from each
C     order, or will add all the sub-orders together for each order.
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the image containing the
C                spectra to be extracted
C     MASK       (Character) The name of the mask image.
C     MLOW       (Integer) The lowest order number to be extracted.
C     MHIGH      (Integer) The higest order number to be extracted.
C     SUBORD     (Integer) The sub-order number to be extracted.
C                If zero, all sub-orders are added for each order.
C     OUTPUT     (Character) The name of the resulting image.
C
C     Command keywords -
C
C     REVERSE    If specified, the image created will have the
C                spectral orders in reverse order - higher order
C                numbers being at the start of the image.
C
C     History -
C
C     29th May 1988  KS/AAO.  Original version.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C-
C     Note -
C
C     This version is an interim one - it really needs re-writing
C     using DSA_RESHAPE_DATA and DSA_RESHAPE_AXIS.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      CHARACTER AXIS_INFO(2)*16  ! Label & units for order axis
      INTEGER   DIMS(2)          ! Dimensions of data array
      DOUBLE PRECISION DUMMY     ! Axis numeric data - unused
      LOGICAL   FAULT            ! Indicates non-DSA error detected
      INTEGER   IPTR             ! Dynamic-mem pointer for image data
      INTEGER   MPTR             ! Dynamic-mem pointer for mask data
      INTEGER   MEND             ! Last order number to extract
      INTEGER   MSTART           ! First order number to extract
      INTEGER   NDIM             ! Number of data dimensions
      INTEGER   NELM             ! Number of image elements
      INTEGER   NX               ! First dimension of data
      INTEGER   NY               ! Second dimension of data
      INTEGER   NYOUT            ! Number of output orders
      INTEGER   OPTR             ! Dynamic-mem pointer for output data
      INTEGER   PSTAT            ! Parameter system status - ignored
      LOGICAL   REVERSE          ! Orders go from high to low if true
      INTEGER   SLOT             ! Map slot value - ignored
      INTEGER   STATUS           ! Running status for DSA_ routines
      INTEGER   SUBORD           ! Sub-order number to extract
      REAL      VALUE            ! Temporary real value
      INTEGER   WPTR             ! Dynamic-mem pointer for work array
      INTEGER   YPTR             ! Dynamic-mem pointer for Y axis
C
C     Parameters for DSA_OUTPUT
C
      INTEGER NO_COPY_DATA, NEW_FILE
      PARAMETER (NO_COPY_DATA=1,NEW_FILE=1)
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Start up DSA system
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
C
C     Open image with spectra to extract
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Open mask image
C
      CALL DSA_INPUT ('MASK','MASK',STATUS)
C
C     Check they match
C
      CALL DSA_MATCH_SIZES ('IMAGE','MASK',STATUS)
C
C     Get size of data array
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=DIMS(2)
      IF (NDIM.LT.2) THEN
         CALL PAR_WRUSER ('Image is not 2-dimensional.',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get order limits
C
      CALL PAR_RDVAL ('MLOW',1.0,1000.0,1.0,'Order #',VALUE)
      MSTART=VALUE
      CALL PAR_RDVAL ('MHIGH',FLOAT(MSTART),1000.0,FLOAT(MSTART),
     :                'Orders',VALUE)
      MEND=VALUE
      CALL PAR_RDVAL ('SUBORD',0.0,9.0,0.0,'Sub order #',VALUE)
      SUBORD=VALUE
      CALL PAR_RDKEY ('REVERSE',.TRUE.,REVERSE)
C
C     Open output image
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',NO_COPY_DATA,
     :                 NEW_FILE,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Force data size in output - this is naughty - it should be
C     done with DSA_RESHAPE_xxx routines, but they aren't ready yet.
C     For one thing, this looses any x-axis data.
C
      NYOUT=MEND-MSTART+1
      DIMS(2)=NYOUT
      CALL DSA_COERCE_DATA_ARRAY ('OUTPUT','FLOAT',2,DIMS,STATUS)
      CALL DSA_COERCE_AXIS_DATA ('OUTPUT',2,'FLOAT',1,NYOUT,STATUS)
C
C     Set the Y-axis array to the order numbers
C
      CALL DSA_MAP_AXIS_DATA ('OUTPUT',2,'WRITE','FLOAT',YPTR,
     :                        SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL FIG_ORDVAL (%VAL(CNF_PVAL(YPTR)),NYOUT,MSTART,MEND,REVERSE)
      AXIS_INFO(1)=' '
      AXIS_INFO(2)='Order number'
      CALL DSA_SET_AXIS_INFO ('OUTPUT',2,2,AXIS_INFO,0,DUMMY,STATUS)
C
C     Map all three data arrays
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('MASK','READ','FLOAT',MPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',OPTR,SLOT,STATUS)
C
C     Get the workspace needed by FIG_EXTMSK
C
      CALL DSA_GET_WORK_ARRAY (NYOUT,'FLOAT',WPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Create the extracted image
C
      CALL FIG_EXTMSK (%VAL(CNF_PVAL(IPTR)),%VAL(CNF_PVAL(MPTR)),NX,NY,
     :                 NYOUT,MSTART,MEND,SUBORD,REVERSE,
     :                 %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(OPTR)))
C
C     Close down
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_EXTMSK (IMAGE,MASK,NX,NY,NYOUT,MSTART,MEND,
     :                                    SUBORD,REVERSE,WORK,OUTPUT)
C
C     F I G _ E X T M S K
C
C     Performs the extraction of spectra from an image using a mask
C     array whose pixel values give the number of the spectum that
C     the pixel covers (as order*10 +sub-order).  It reports any
C     orders for which no data was found.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) IMAGE     (Real array IMAGE(NX,NY)) The image containing
C                   the data.
C     (>) MASK      (Real array MASK(NX,NY)) The mask array.
C     (>) NX        (Integer) The x-dimension of image, mask and output.
C     (>) NY        (Integer) The y-dimension of image and mask.
C     (>) NYOUT     (Integer) The y-dimension of the output image.
C     (>) MSTART    (Integer) The lowest spectrum number to extract
C     (>) MEND      (Integer) The highest spectrum number to extract
C     (>) SUBORD    (Integer) The sub-order number to extract.
C     (>) REVERSE   (Logical) True if spectra are to be appear in the
C                   output image in reverse order of spectrum number.
C     (W) WORK      (Real array WORK(NYOUT)) Workspace. Used to hold
C                   The number of pixels used for each order.
C     (<) OUTPUT    (Real array OUTPUT(NX,NYOUT)) The resulting image,
C                   with each cross-section a single extracted spectrum.
C
C     Common variables used -  None
C
C     External routines used -
C
C     GEN_FILL      Fill a byte array with a specified value.
C
C                                           KS/AAO 29th May 1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL REVERSE
      INTEGER NX, NY, NYOUT, MSTART, MEND, SUBORD
      REAL IMAGE(NX,NY), MASK(NX,NY), WORK(NYOUT), OUTPUT(NX,NYOUT)
C
C     Functions used
C
      CHARACTER ICH_CI*10
      INTEGER ICH_LEN
C
C     Local variables
C
      LOGICAL USE
      INTEGER IX, IY, MY, ORDER, PSTAT
      CHARACTER*64 STRING
C
      CALL GEN_FILL (NX*NYOUT*4,0,OUTPUT)
      CALL GEN_FILL (NYOUT*4,0,WORK)
C
      DO IY=1,NY
         DO IX=1,NX
            ORDER=INT(MASK(IX,IY))/10
            IF ((ORDER.GE.MSTART).AND.(ORDER.LE.MEND)) THEN
               IF (SUBORD.EQ.0) THEN
                  USE=.TRUE.
               ELSE
                  USE=SUBORD.EQ.MOD(INT(MASK(IX,IY)),10)
               END IF
               IF (USE) THEN
                  MY=ORDER-MSTART+1
                  WORK(MY)=WORK(MY)+1.0
                  IF (REVERSE) MY=NYOUT-(ORDER-MSTART)
                  OUTPUT(IX,MY)=OUTPUT(IX,MY)+IMAGE(IX,IY)
               END IF
            END IF
         END DO
      END DO
C
C     Report results
C
      DO IY=1,NYOUT
         IF (WORK(IY).EQ.0) THEN
            STRING='Warning: No data extracted for order '
     :                                         //ICH_CI(MSTART+IY-1)
            CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),PSTAT)
         END IF
      END DO
C
      END
C+
      SUBROUTINE FIG_ORDVAL (YDATA,NYOUT,MSTART,MEND,REVERSE)
C
C     F I G _ O R D V A L
C
C     Fills the axis array for the mask extracted spectrum.
C
C     Parameters -    (">" input, "<" output)
C
C     (<) YDATA     (Real array YDATA(NYOUT)) The axis data array.
C     (>) NYOUT     (Integer) The y-dimension of the output image.
C     (>) MSTART    (Integer) The lowest spectrum number extracted.
C     (>) MEND      (Integer) The highest spectrum number extracted.
C     (>) REVERSE   (Logical) True if spectra are to be appear in the
C                   output image in reverse order of spectrum number.
C
C     Common variables used -  None
C
C     External routines used - None
C
C                                           KS/AAO 29th May 1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL REVERSE
      INTEGER NYOUT, MSTART, MEND
      REAL YDATA(NYOUT)
C
C     Local variables
C
      INTEGER IY
C
      IF (REVERSE) THEN
         DO IY=1,NYOUT
            YDATA(IY)=MEND-IY+1
         END DO
      ELSE
         DO IY=1,NYOUT
            YDATA(IY)=MSTART+IY-1
         END DO
      END IF
C
      END
