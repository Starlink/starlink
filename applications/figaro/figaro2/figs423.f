C+
      SUBROUTINE FIGS423
C
C     F I G S 4 2 3
C
C     Given a FIGS image mode data hypercube, either sorted into
C     wavelength order (eg by FIGS424) or not, sums all the cycles
C     and wavelength planes within a specified wavelength range
C     to produce an image.  Note that it is probably best to have
C     performed the wavelength sort first.
C
C     Command parameters -
C
C     'HCUBE'    (Character) The name of the hypercube to be processed.
C     'CYSTART'  (Integer) The first cycle to be included.
C     'CYEND'    (Integer) The last cycle to be included.
C     'CUBE'     (Character) The name of the resulting cube..
C
C     Input data -
C
C     This routine assumes that the first axis of the hcube data
C     represents wavelength, that the second and third represent the
C     X and Y dimensions of the image (this is an unfortunate,
C     since it means that the .X axis of the hypercube represents
C     wavelength, the .Y represents the image X axis and so forth)
C     and the fourth axis represents scan cycle number.
C
C     Output data -
C
C     CUBE is created with the same structure as HCUBE
C     except that the dta array will only have 3 dimensions, and any
C     AXIS(4) sub-structures that HCUBE has will be deleted.
C
C                                     KS / AAO 19th May 1986
C
C     11th July 1989 JM / RAL. Modified to use DSA_ routines
C                    Dynamic-memory handling changed to use
C                    DYN_ routines
C     29th Mar 1991  JMS / AAO. Changed type from 'INTEGER' to 'INT'
C                    when mapping input data. Added STATUS checks to
C                    support user-requested aborts.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      CHARACTER COMENT*16        ! Comment associated with FITS items
      INTEGER   CUPTR            ! Dynamic-memory pointer for HCUBE data
      INTEGER   DIMS(10)         ! Image dimensions
      LOGICAL   EXIST            ! True if axis structure exists
      LOGICAL   FAULT            ! True if non-DSA problem occurs
      INTEGER   ICYEN            ! First cycle number to be used
      INTEGER   ICYST            ! Last cycle number to be used
      INTEGER   MODE             ! Mode of hypercube data
      INTEGER   NAXIS            ! Axis number
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NT               ! Third dimension of HCUBE
      INTEGER   NU               ! Fourth dimension of HCUBE
      INTEGER   NX               ! First dimension of HCUBE
      INTEGER   NY               ! Second dimension of HCUBE
      INTEGER   OPTR             ! Dynamic-memory pointer for OUTPUT
                                 ! data
      INTEGER   PSTAT            ! Status for PAR routines
      INTEGER   SFLAG            ! Test for sorted data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
      REAL      VALUE            ! Used by PAR_ calls to read in
                                 ! parameters
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Image mode parameter value
C
      INTEGER IMAGE1
      PARAMETER (IMAGE1=3)
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
C     Get name of HCUBE file
C
      CALL DSA_INPUT ('HCUBE','HCUBE',STATUS)
C
C     Get mode of hypercube data
C
      CALL DSA_GET_FITS_I('HCUBE','FIG_MODE',0,MODE,COMENT,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (MODE.NE.IMAGE1) THEN
         CALL PAR_WRUSER(
     :       'The data mode (FIG_MODE) is not the correct '
     :       //' value for image mode.',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get size of data in HCUBE
C
      CALL DSA_DATA_SIZE ('HCUBE',10,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (NDIM.NE.4) THEN
         CALL PAR_WRUSER('Input data is not a hypercube',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
      NU=DIMS(4)
      NT=DIMS(3)
      NY=DIMS(2)
      NX=DIMS(1)
      IF (NX.GT.416) THEN
         CALL PAR_WRUSER(
     :    '1st dimension of cube is greater than 416.  This cannot be',
     :                                                        PSTAT)
         CALL PAR_WRUSER(
     :    'data from the FIGS data acquisition system.',PSTAT)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     See if this is sorted data, ie data that has been processed
C     by FIGS424.
C
      CALL DSA_GET_FITS_I('HCUBE','FIG_SORT',0,SFLAG,COMENT,STATUS)
      IF ((STATUS.NE.0).OR.(SFLAG.EQ.0)) THEN
C
C        It is not sorted data. Issue a suitable warning.
C
         CALL PAR_WRUSER(
     :        'Warning: The hypercube has not been sorted in'//
     :        ' wavelength.',PSTAT)
         CALL PAR_WRUSER('(FIGS424 can perform this sort.)',PSTAT)
         STATUS=0
      END IF
C
C     Now deal with the cycle range.  Get CYSTART, CYEND and SPLIT
C
      CALL PAR_RDVAL('CYSTART',1.,FLOAT(NU),1.,'cycles',VALUE)
      ICYST=VALUE
      CALL PAR_RDVAL('CYEND',VALUE,FLOAT(NU),FLOAT(NU),'cycles',VALUE)
      ICYEN=VALUE
C
C     Get name of CUBE file to be created.
C
      CALL DSA_OUTPUT('OUTPUT','CUBE','HCUBE',NO_DATA,NEW_FILE,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the input data
C
      CALL DSA_MAP_DATA('HCUBE','READ','INT',CUPTR,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Now map the output data structure
C
      NDIM=3
      DIMS(1)=NX
      DIMS(2)=NY
      DIMS(3)=NT
      CALL DSA_RESHAPE_DATA('OUTPUT','HCUBE',NDIM,DIMS,STATUS)
C
C     Map the output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Copy the first three input axis structures to the output.
C     The AXIS(4) structure is lost.
C
      DO NAXIS=1,3
         CALL DSA_SEEK_AXIS('HCUBE',NAXIS,EXIST,STATUS)
         IF(EXIST)THEN
            CALL DSA_AXIS_SIZE('HCUBE',NAXIS,10,NDIM,DIMS,NELM,STATUS)
            CALL DSA_RESHAPE_AXIS('OUTPUT',1,'HCUBE',1,NDIM,DIMS,STATUS)
         END IF
      END DO
      IF(STATUS.NE.0)GOTO 500
C
C     Perform the basic processing of the hypercube down to
C     a cube.
C
      CALL FIG_FIGS423(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,NU,ICYST,ICYEN,
     :                 %VAL(CNF_PVAL(OPTR)))

  500 CONTINUE
C
C     Close down everything.
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR

      END
C+
      SUBROUTINE FIG_FIGS423(HCUBE,NX,NY,NT,NU,CY1,CY2,CUBE)
C
C     F I G _ F I G S 4 2 3
C
C     Sums all the cycles of a FIGS image mode hypercube to
C     produce an CUBE.
C
c     Parameters -  (">" input, "!" modified, "W" workspace)
C
C     (>) HCUBE     (Integer array HCUBE(NX,NY,NT,NU)) The data
C                   hypercube.
C     (>) NX        (Integer) First dimension of HCUBE.
C     (>) NY        (Integer) Second    "     "    "
C     (>) NT        (Integer) Third     "     "    "
C     (>) NU        (Integer) Fourth    "     "    "
C     (>) CY1       (Integer) First cycle (U value) to be included
C     (>) CY2       (Integer) Last cycle (U value) to be included
C     (<) CUBE      (Real array CUBE(NX,NY,NT)) The resulting CUBE.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FILL      Sets an array of bytes to a constant value
C
C                                            KS / AAO 19th May 1986
C     Modified:
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NT, NU, CY1, CY2, HCUBE(NX,NY,NT,NU)
      REAL    CUBE(NX,NY,NT)
C
C     Local variables
C
      INTEGER IT, IU, IX, IY
C
C     Set the CUBE to zero
C
      CALL GEN_FILL(NX*NY*NT*4,0,CUBE)
C
C     Add in the appropriate cube elements
C
      DO IU=CY1,CY2
         DO IT=1,NT
            DO IY=1,NY
               DO IX=1,NX
                  CUBE(IX,IY,IT)=CUBE(IX,IY,IT)+HCUBE(IX,IY,IT,IU)
               END DO
            END DO
         END DO
      END DO
C
      END
