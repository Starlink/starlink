C+
      SUBROUTINE ISUPER
C
C     I S U P E R
C
C     Supersets an image or a spectrum, creating a new image
C     with larger dimensions than the input, and with the input
C     data just a part of the output image.  If the AXIS(1) or
C     AXIS(2) arrays contain linear data, then output AXIS(1) and
C     AXIS(2) sub-structures will be created reflecting this data.
C     If they contain non-linear data, ISUPER will not attempt to
C     extrapolate the data values and will omit the AXIS(1) or AXIS(2)
C     sub-structure in question from the output structure.
C     Note: It is also capable of subsetting.
C
C     Command Parameters -
C
C     IMAGE    (Character) The name of the input image.
C     XSIZE    (Numeric) The AXIS(1)-dimension of the output image.
C     YSIZE    (Numeric) The AXIS(2)-dimension of the output image.
C     XPIXEL   (Numeric) The pixel number in AXIS(1) at which the input
C              image is to start.
C     YPIXEL   (Numeric) The pixel number in AXIS(2) at which the input
C              image is to start.  That is, pixel (1,1) of the input
C              image maps onto pixel (XPIXEL,YPIXEL) of the output.
C              These values may be such that not all the input image
C              appears in the output - they may even be negative.
C     OUTPUT   (Character) The name of the output image.
C
C     Command keywords -  None
C
C     User variables used -  None
C
C                                            KS / CIT 9th Aug 1984
C     Modified -
C
C     25th Mar 1985  KS / AAO.  Code to handle linear X and Y data
C                    added.
C     9th  May 1986  KS / AAO.  Use of DTA_MRVAR to map output data
C                    corrected.  DTA_MUVAR used instead.
C     25th Aug  1988 JM / AAO. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     15th Jan. 1991 JMS / AAO. Now informs user that it will not
C                    extrapolate 2D x-axis. Made minor changes to the
C                    assignment of the NX and NY variables.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER instead of DSA_WRUSER.
C     15th May 1995  HME / UoE, Starlink.  Change access to output data
C                    to 'WRITE'.
C     26th Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL    FIG_SCRCHK
      LOGICAL    GEN_CHKNSF
      REAL       GEN_ELEMF
      CHARACTER  ICH_CI*12
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      LOGICAL   AEXIST           ! True if axis exists
      CHARACTER CNAXIS*(1)       ! Axis number as a character
      INTEGER   DIMS(2)          ! Image dimensions
      INTEGER   ELEMENTS         ! Number of elements in axis array
      INTEGER   IPTR             ! Dynamic mem pointer for input data
      INTEGER   IXYST            ! IXST or IYST (used in loop)
      INTEGER   IXST             ! Pixel number in AXIS(1) at which the
                                 ! input image is to start
      INTEGER   IYST             ! Pixel number in AXIS(2) at which the
                                 ! input image is to start
      INTEGER   NAXIS            ! Axis number
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NDIMA            ! Number of dimensions of an axis
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of input image
      INTEGER   NY               ! Second dimension of input image
      INTEGER   NXOUT            ! First dimension of output image
      INTEGER   NYOUT            ! Second dimension of output image
      INTEGER   NXY              ! NX or NY (used in loop)
      INTEGER   NXYOUT           ! NXOUT or NYOUT (used in loop)
      INTEGER   OPTR             ! Dynamic mem pointer for output data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
      REAL      VALUE            ! Used to read in IXST and IYST
      REAL      VEND             ! Value of last axis element in scaled
                                 ! output array
      REAL      VSTART           ! Value of 1st axis element in scaled
                                 ! output array
      INTEGER   XYPTR            ! Dynamic mem pointer for input axis
                                 ! data
      INTEGER   XYOPTR           ! Dynamic mem pointer for output axis
                                 ! data
      REAL      XVALUE           ! Used to read in NXOUT
      DOUBLE PRECISION XY1       ! Value of first axis data element in
                                 ! input AXIS(1) or AXIS(2)
      DOUBLE PRECISION XYLST     ! Value of last axis data element in
                                 ! input AXIS(1) or AXIS(2)
      DOUBLE PRECISION XYDEL     ! Increment used in scaling linear axis
                                 ! data
      REAL      YVALUE           ! Used to read in NYOUT

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
      IF(STATUS.NE.0)GOTO 500
      NX=DIMS(1)
      NY=DIMS(2)
C
C     Get parameter values  (the 65536 maximum size is quite arbitrary)
C
      CALL PAR_RDVAL('XSIZE',1.,65536.,FLOAT(NX),'Pixels',XVALUE)
      NXOUT=XVALUE
      CALL PAR_RDVAL('YSIZE',1.,65536.,FLOAT(NY),'Pixels',YVALUE)
      NYOUT=YVALUE
      IF (NXOUT.GT.1) THEN
         CALL PAR_RDVAL('XPIXEL',-FLOAT(NX),XVALUE,1.,'Pixels',VALUE)
         IXST=VALUE
      ELSE
         IXST=1
      END IF
      IF (NYOUT.GT.1) THEN
         CALL PAR_RDVAL('YPIXEL',-FLOAT(NY),YVALUE,1.,'Pixels',VALUE)
         IYST=VALUE
      ELSE
         IYST=1
      END IF
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get name of OUTPUT and create file based on 'IMAGE' but
C     without data.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
C
C     Now create the output data structure, based on that of the
C     input file, but with with a data array of the appropriate dimensions.
C
      IF (NYOUT.EQ.1) THEN
         NDIM=1
      ELSE
         NDIM=2
      END IF
      DIMS(1)=NXOUT
      DIMS(2)=NYOUT
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,DIMS,STATUS)
C
C     Map input and output images
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)

      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Copy the overlapping pixels and zero others in output image
C
      CALL GEN_SUPSET(%VAL(CNF_PVAL(IPTR)),NX,NY,NXOUT,NYOUT,IXST,IYST,
     :                %VAL(CNF_PVAL(OPTR)))
C
C     Deal with the axes structures.  This loop is executed
C     twice, once for AXIS(1), once for AXIS(2), unless the output
C     structure is 1-dimensional.
C
      NXY=NX
      IXYST=IXST
      NXYOUT=NXOUT

      DO NAXIS=1,NDIM
C
C        Try to map the input axis data array.
C
         CALL DSA_SEEK_AXIS('IMAGE',NAXIS,AEXIST,STATUS)
         IF (AEXIST) THEN
            CALL DSA_AXIS_SIZE('IMAGE',NAXIS,2,NDIMA,DIMS,ELEMENTS,
     :                          STATUS)
            IF (NDIMA.EQ.2) THEN
               CALL PAR_WRUSER(
     :         'X-axis is 2-dimensional. Will not attempt to '//
     :         'extrapolate in both dimensions. Result will have '//
     :         '1D axis based on 1st. cross section of input axis.',
     :         STATUS)
            END IF
            CALL DSA_MAP_AXIS_DATA('IMAGE',NAXIS,'READ','FLOAT',XYPTR,
     :                              SLOT,STATUS)
C
C           See if linear.
C
            IF (FIG_SCRCHK(NXY,%VAL(CNF_PVAL(XYPTR)))) THEN
C
C              It is, so copy the structure - except for the data
C
               CALL DSA_RESHAPE_AXIS('OUTPUT',NAXIS,'IMAGE',NAXIS,
     :                                1,NXYOUT,STATUS)
C
C              Map it, and fill it up.  Extrapolate the data values
C              unless they are just the numbers 1..N, in which case
C              just number the output elements too.
C
               CALL DSA_MAP_AXIS_DATA('OUTPUT',NAXIS,'UPDATE','FLOAT',
     :                                 XYOPTR,SLOT,STATUS)
               IF(STATUS.NE.0)GOTO 500

               IF (GEN_CHKNSF(%VAL(CNF_PVAL(XYPTR)),NXY)) THEN
                  CALL GEN_NFILLF(NXYOUT,%VAL(CNF_PVAL(XYOPTR)))
               ELSE
                  XY1=GEN_ELEMF(%VAL(CNF_PVAL(XYPTR)),1)
                  XYLST=GEN_ELEMF(%VAL(CNF_PVAL(XYPTR)),NXY)
                  XYDEL=(XYLST-XY1)/DBLE(NXY-1)
                  VSTART=XY1-XYDEL*(IXYST-1)
                  VEND=XY1+XYDEL*(NXYOUT-IXYST)
                  CALL FIG_WFILL(VSTART,VEND,.FALSE.,NXYOUT,
     :                           %VAL(CNF_PVAL(XYOPTR)))
               END IF
            ELSE
               CNAXIS=ICH_CI(NAXIS)
               CALL PAR_WRUSER('Warning: AXIS('//CNAXIS//
     :            ') data is not linear.  ISUPER will not attempt to '//
     :            'extrapolate it. No AXIS('//CNAXIS//
     :            ') output structure will be created.',STATUS)
            END IF
         END IF
C
C        Second (and last) time through the loop is for AXIS(2)
C
         NXY=NY
         IXYST=IYST
         NXYOUT=NYOUT
      END DO

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
