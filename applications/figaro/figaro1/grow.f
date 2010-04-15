C+
      SUBROUTINE GROW
C
C     G R O W
C
C     This is the main routine for the Figaro commands GROWX
C     and GROWY.  These both copy a spectrum into one or more
C     cross-sections of an image, GROWX copying into cross-
C     sections of constant AXIS(2), and GROWY copying into cross-
C     sections of constant AXIS(1).  The operation performed by GROWX
C     is the inverse of that performed by EXTRACT, and similarly
C     for GROWY and YSTRACT.
C
C     Command parameters:
C
C     SPECTRUM    (Character) The name of the input spectrum file.
C
C     IMAGE       (Character) The name of the image into which the
C                 spectrum is to be copied.  If the image file does
C                 not exist, or if the 'NEW' keyword is specified,
C                 a new file is created with all other data elements
C                 set to zero.
C
C     XSTART      (Numeric) The number of the first cross-section
C       or        into which the spectrum is to be copied.  XSTART
C     YSTART      is used by GROWY, YSTART by GROWX.
C
C     XEND        (Numeric) The number of the last cross-section
C       or        into which the sepctrum is to be copied.  XEND
C     YEND        is used by GROWY, YEND by GROWX.
C
C     XSIZE       (Numeric) If a new image has to be created, one
C       or        of its dimensions will be that of the spectrum, but
C     YSIZE       the other is unknown.  This has to be specified as
C                 XSIZE (for GROWY) or YSIZE for GROWX.
C
C     Command Keywords:
C
C     NEW         Used to force the creation of a new image, even if
C                 such an image exists already.
C
C     User variables used:  None
C
C     Error information:  Ignored.
C
C     Data quality information: Handled using flagged data values.
C
C                                             KS / CIT 19th Sept 1983
C     Modified:
C
C     16 Oct 1988  JM / RAL. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines
C     23 Aug 1990  KS / AAO. Use of flagged data values introduced.
C                  DSA_TYPESIZE used to get size of 'FLOAT'.
C     21 Feb 1991  JMS / AAO. Added PAR_ABORTS to support user requested
C                  aborts.
C     29 Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                  PAR_WRUSER rather than DSA_WRUSER.
C     09 Mar 1995  HME / UoE, Starlink.  Use write access if output
C                  file is new.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Avoid _NAMED_ routines.
C                  Input read-only.
C     26 Jul 1996  MJCL / Starlink, UCL.  Added one more PAR_ABORT check.
C     27 Jul 1996  MJCL / Starlink, UCL.  Initialise VEXIST to .FALSE.
C     1  Apr 1999  TDCA / Starlink, RAL. Fixed bug in variance mapping of
C                  an existing image.
C     2  Jun 1999  TDCA / Starlink, RAL. GROWX or GROWY set to .FALSE. if
C                  appropriate.
C     2005 June 7  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      CHARACTER COMMAND*5   ! Figaro command name
      INTEGER   DIMS(2)     ! Image dimensions
      REAL      END         ! Used to read in LIM2
      LOGICAL   EXIST       ! True if SPECT AXIS(1) exists
      LOGICAL   GROWX       ! True if command is GROWX
      LOGICAL   GROWY       ! True if command is GROWY
      INTEGER   IPTR        ! Dynamic memory pointer for image data
      INTEGER   LIM1        ! First cross-section spectrum copied into
      INTEGER   LIM2        ! Last cross-section spectrum copied into
      INTEGER   NDIM        ! Number of dimensions
      INTEGER   NELM        ! Number of elements - ignored
      LOGICAL   NEW         ! True if GROW performed into new file
      INTEGER   NSPECT      ! Dimension of SPECTRUM
      INTEGER   NX          ! First dimension of image
      INTEGER   NY          ! Second dimension of image
      LOGICAL   OLDFIL      ! True if GROW performed into old file
      REAL      SIZE        ! Size of extra axis dimension of IMAGE
      INTEGER   SLOT        ! Slot number for mapped data - ignored
      INTEGER   SPTR        ! Dynamic memory pointer for spectrum data
      REAL      START       ! Used to read in LIM1
      INTEGER   STATUS      ! Running status for DSA routines
      INTEGER   SVPTR       ! Dynamic memory pointer for spectrum variances
      LOGICAL   VEXIST      ! Is there a variance structure?
      REAL      VMAX        ! Max. size of extra axis dimension of IMAGE
      INTEGER   VPTR        ! Dynamic memory pointer for image variance
      CHARACTER XYCH*1      ! (Old Figaro) name of axis (for parameters)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      OLDFIL=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Find out which command
C
      CALL PAR_COMMAND(COMMAND)
      GROWX=COMMAND.EQ.'GROWX'
      IF (GROWX) THEN
         XYCH='Y'
         GROWY=.FALSE.
      ELSE
         XYCH='X'
         GROWY=.TRUE.
         GROWX=.FALSE.
      END IF
C
C     Get the SPECTRUM name and open it.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get the dimensions of the spectrum data and map it.  A simple copy
C     propagates flagged data values naturally, so we indicate that we
C     can handle them.
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      NSPECT=DIMS(1)
C
      CALL DSA_USE_FLAGGED_VALUES ('SPECT',STATUS)
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',SPTR,SLOT,STATUS)
C
C     Check to see if a variance structure exists and if so, map it.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('SPECT',VEXIST,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('SPECT','READ','FLOAT',SVPTR,SLOT,
     :                         STATUS)
      END IF
C
C     See if output is new or old image. A new image is opened with
C     DSA_OUTPUT, an old image with DSA_INPUT_UPDATE.
C
      CALL PAR_RDKEY('NEW',.FALSE.,NEW)
      IF (PAR_ABORT()) GOTO 500
      IF (NEW) THEN
         CALL DSA_OUTPUT('IMAGE','IMAGE','SPECT',NO_DATA,NEW_FILE,
     :                          STATUS)
         OLDFIL=.FALSE.
      ELSE

C     Possible cause for bug is that structure reference name is the same
C     as the corresponding parameter name.

         CALL DSA_INPUT_UPDATE('IMAGE','IMAGE',STATUS)
         OLDFIL=.TRUE.
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     If we have an image already, get its dimensions
C
      IF (OLDFIL) THEN
         CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
         IF(STATUS.NE.0)GOTO 500
         IF (NDIM.NE.2) THEN
            CALL PAR_WRUSER('This is not two-dimensional data',STATUS)
            GO TO 500
         END IF
         NX=DIMS(1)
         NY=DIMS(2)
C
C        Check appropriate dimensions match
C
         IF(GROWX)THEN
            CALL DSA_MATCH_DIMENSION('SPECT',1,'IMAGE',1,STATUS)
         ELSE
            CALL DSA_MATCH_DIMENSION('SPECT',1,'IMAGE',2,STATUS)
         END IF

      END IF
      IF(STATUS.NE.0)GOTO 500
C
C     Get the AXIS(1) or AXIS(2) start and end parameters.
C     If we don't have an image yet, we set ludicrous maximum
C     values for the size limits.
C
      IF (NEW) THEN
         VMAX=32767.
      ELSE
         IF (GROWX) THEN
            VMAX=NY
         ELSE
            VMAX=NX
         END IF
      END IF
      CALL PAR_RDVAL(XYCH//'START',1.,VMAX,1.,' ',START)
      IF (PAR_ABORT()) GOTO 500
      LIM1=START
      CALL PAR_RDVAL(XYCH//'END',START,VMAX,START,' ',END)
      IF (PAR_ABORT()) GOTO 500
      LIM2=END

      IF (NEW) THEN
C
C        Need to create a new image, so need to know size
C
         CALL PAR_RDVAL(XYCH//'SIZE',END,32767.,END,' ',SIZE)
         IF (PAR_ABORT()) GOTO 500
C
C        Create IMAGE data array of appropriate size.
C
         IF (GROWX) THEN
            DIMS(1)=NSPECT
            DIMS(2)=SIZE
         ELSE
            DIMS(1)=SIZE
            DIMS(2)=NSPECT
         END IF
         NX=DIMS(1)
         NY=DIMS(2)
         CALL DSA_RESHAPE_DATA('IMAGE','SPECT',2,DIMS,STATUS)
C
C        Copy SPECTRUM AXIS(1) structure (if any) to appropriate
C        structure in IMAGE.
C
         CALL DSA_SEEK_AXIS('SPECT',1,EXIST,STATUS)
         IF(EXIST)THEN
            CALL DSA_AXIS_SIZE('SPECT',1,2,NDIM,DIMS,NELM,STATUS)
            IF (GROWX) THEN
               CALL DSA_RESHAPE_AXIS('IMAGE',1,'SPECT',1,NDIM,DIMS,
     :                                STATUS)
            ELSE
               CALL DSA_RESHAPE_AXIS('IMAGE',2,'SPECT',1,NDIM,DIMS,
     :                                STATUS)
            END IF
         END IF

      END IF
C
C     Map the image data.
C
C     CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
C     CALL DSA_MAP_DATA('IMAGE','UPDATE','FLOAT',IPTR,SLOT,STATUS)
C     IF(STATUS.NE.0)GOTO 500
C     IF (NEW) CALL GEN_FILL(NX*NY*DSA_TYPESIZE('FLOAT',STATUS),
C    :                       0,%VAL(CNF_PVAL(IPTR)))
      IF (NEW) THEN
         CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
         CALL DSA_MAP_DATA('IMAGE','WRITE','FLOAT',IPTR,SLOT,STATUS)
         IF (VEXIST) THEN
           CALL DSA_MAP_VARIANCE('IMAGE','WRITE','FLOAT',VPTR,SLOT,
     :                           STATUS)
         END IF
         IF(STATUS.NE.0)GOTO 500
         CALL GEN_FILL(NX*NY*DSA_TYPESIZE('FLOAT',STATUS),
     :                 0,%VAL(CNF_PVAL(IPTR)))
      ELSE
         CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
         CALL DSA_MAP_DATA('IMAGE','UPDATE','FLOAT',IPTR,SLOT,STATUS)
         IF (VEXIST) THEN
           CALL DSA_MAP_VARIANCE('IMAGE','UPDATE','FLOAT',VPTR,SLOT,
     :                        STATUS)
         END IF
         IF(STATUS.NE.0)GOTO 500
      END IF
C
C     Now, we have all the data mapped, perform the grow.
C
      IF (GROWX) THEN
         CALL GEN_GROWX(%VAL(CNF_PVAL(SPTR)),NX,NY,LIM1,LIM2,
     :                  %VAL(CNF_PVAL(IPTR)))
         IF (VEXIST) THEN
             CALL GEN_GROWX(%VAL(CNF_PVAL(SVPTR)),NX,NY,LIM1,LIM2,
     :                      %VAL(CNF_PVAL(VPTR)))
         END IF
      END IF

      IF (GROWY) THEN
         CALL GEN_GROWY(%VAL(CNF_PVAL(SPTR)),NX,NY,LIM1,LIM2,
     :                  %VAL(CNF_PVAL(IPTR)))
         IF (VEXIST) THEN
             CALL GEN_GROWY(%VAL(CNF_PVAL(SVPTR)),NX,NY,LIM1,LIM2,
     :                      %VAL(CNF_PVAL(VPTR)))
         END IF
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
