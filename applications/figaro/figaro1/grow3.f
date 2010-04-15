C+
      SUBROUTINE GROW3
C
C     G R O W 3
C
C     This is the main routine for the Figaro commands GROWXY, GROWXT
C     and GROWYT.  These both copy an cube into one or more
C     planes of a cuboid, GROWXY copying into planes of constant T,
C     GROWYT into planes of constant X, and GROWXT into planes of
C     constant Y.  The operations performed by GROWXY, GROWXT and
C     GROWYT are the inverses of XYPLANE, XTPLANE and YTPLANE
C     respectively.
C
C     Command parameters -
C
C     IMAGE       (Character) The name of the input image file.
C                 This will actually be image.dst.
C
C     CUBE        (Character) The name of the cube into which the
C                 image is to be copied.  If the cube file does
C                 not exist, or if the 'NEW' keyword is specified,
C                 a new file is created with all other data elements
C                 set to zero.
C
C     XSTART,     (Numeric) The number of the first plane
C     YSTART or   into which the image is to be copied.  XSTART
C     TSTART      is used by GROWYT, YSTART by GROWXT, TSTART by GROWXY.
C
C     XEND,       (Numeric) The number of the last plane
C     YEND, or    into which the image is to be copied.  XEND is
C     TEND        used by GROWYT, YEND by GROWXT, TEND by GROWXY.
C
C     XSIZE,      (Numeric) If a new cube has to be created, two
C     YSIZE, or   of its dimensions will be those of the image, but
C     TSIZE       the other is unknown.  This has to be specified as
C                 XSIZE, YSIZE or TSIZE for GROWYT, GROWXT, GROWXY
C                 respectively.
C
C     Command Keywords
C
C     NEW         Used to force the creation of a new cube, even if
C                 such an cube exists already.
C
C     User variables used -  None
C
C                                             KS / AAO 15th April 1985
C     Modified:
C
C     21 Oct 1988  JM / RAL. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines
C     22 Feb 1991  JMS / AAO. Added PAR_ABORTs and STATUS checks to
C                  support user requested aborts.
C     29 Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                  PAR_WRUSER rather than DSA_WRUSER.
C     09 Mar 1995  HME / UoE, Starlink.  Use write access if output
C                  file is new.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Avoid _NAMED_ routines.
C                  Input read-only.
C     26 Jul 1996  MJCL / starlink, UCL.  PAR_ABORT check for NEW.
C     2005 June 7  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
C
C     Local variables
C
      CHARACTER COMMAND*6  ! Figaro command name
      INTEGER   CUPTR      ! Dynamic memory element for CUBE data
      INTEGER   DIMS(5)    ! Various dimensions
      REAL      END        ! Upper axis limit for extra dimension of CUBE
      LOGICAL   EXIST      ! Used to check for existence of axes structures
      LOGICAL   GROWXT     ! True if command is GROWXT
      LOGICAL   GROWXY     ! True if command is GROWXY
      LOGICAL   GROWYT     ! True if command is GROWYT
      INTEGER   IDX        ! A dimension of CUBE
      INTEGER   IDY        ! A dimension of CUBE
      INTEGER   IPTR       ! Dynamic memory pointer for IMAGE data
      INTEGER   LIM1       ! Lower axis limit for extra dimension of CUBE
      INTEGER   LIM2       ! Upper axis limit for extra dimension of CUBE
      INTEGER   NDIM       ! No. of dimensions for various structures
      INTEGER   NELM       ! Number of elements in image - ignored
      LOGICAL   NEW        ! True is user specifies a new outfile file
      INTEGER   NX         ! First dimension of CUBE
      INTEGER   NY         ! Second dimension of CUBE
      INTEGER   NT         ! Third dimension of CUBE
      LOGICAL   OLDFIL     ! True if existing CUBE file is used
      REAL      SIZE       ! Size of extra axis dimension of CUBE
      INTEGER   SLOT       ! Slot number for mapped data - ignored
      REAL      START      ! Lower axis limit for extra dimension of CUBE
      INTEGER   STATUS     ! Running status for DSA routines
      REAL      VMAX       ! Max. size of extra axis dimension of CUBE
      CHARACTER XYTCH*1    ! (Old Figaro) name of axis
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
C     Find out which command
C
      CALL PAR_COMMAND(COMMAND)
      GROWXY=COMMAND.EQ.'GROWXY'
      GROWXT=COMMAND.EQ.'GROWXT'
      GROWYT=COMMAND.EQ.'GROWYT'
      IF (GROWXY) XYTCH='T'
      IF (GROWXT) XYTCH='Y'
      IF (GROWYT) XYTCH='X'
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get the dimensions of the image data and map it
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('This is not two-dimensional data',STATUS)
         GO TO 500
      END IF
      IDX=DIMS(1)
      IDY=DIMS(2)

      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500

C
C     See if output is new or old image. A new image is opened with
C     DSA_OUTPUT, an old image with DSA_INPUT_UPDATE.
C
      CALL PAR_RDKEY('NEW',.FALSE.,NEW)
      IF (PAR_ABORT()) GOTO 500
      IF (NEW) THEN
         CALL DSA_OUTPUT('CUBE','CUBE','IMAGE',NO_DATA,NEW_FILE,
     :                   STATUS)
         OLDFIL=.FALSE.
      ELSE
         CALL DSA_INPUT_UPDATE('CUBE','CUBE',STATUS)
         OLDFIL=.TRUE.
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     If we have an cube already, get its dimensions
C
      IF (OLDFIL) THEN
         CALL DSA_DATA_SIZE('CUBE',5,NDIM,DIMS,NELM,STATUS)
         IF(STATUS.NE.0)GOTO 500
         IF (NDIM.NE.3) THEN
            CALL PAR_WRUSER('This is not three-dimensional data',
     :                      STATUS)
            GO TO 500
         END IF
         NX=DIMS(1)
         NY=DIMS(2)
         NT=DIMS(3)

         IF(GROWXT)THEN
            CALL DSA_MATCH_DIMENSION('IMAGE',1,'CUBE',1,STATUS)
            CALL DSA_MATCH_DIMENSION('IMAGE',2,'CUBE',3,STATUS)
         ELSEIF(GROWYT)THEN
            CALL DSA_MATCH_DIMENSION('IMAGE',1,'CUBE',3,STATUS)
            CALL DSA_MATCH_DIMENSION('IMAGE',2,'CUBE',2,STATUS)
         ELSEIF(GROWXY)THEN
            CALL DSA_MATCH_DIMENSION('IMAGE',1,'CUBE',1,STATUS)
            CALL DSA_MATCH_DIMENSION('IMAGE',2,'CUBE',2,STATUS)
         END IF
         IF(STATUS.NE.0)GOTO 500
      END IF
C
C     Get the appropriate axis start and end parameters.  If we don't
C     have a cube yet, we set ludicrous maximum values for the size
C     limits.
C
      IF (NEW) THEN
         VMAX=32767.
      ELSE
         IF (GROWXY) VMAX=NT
         IF (GROWYT) VMAX=NX
         IF (GROWXT) VMAX=NY
      END IF
      CALL PAR_RDVAL(XYTCH//'START',1.,VMAX,1.,' ',START)
      IF (PAR_ABORT()) GOTO 500
      LIM1=START
      CALL PAR_RDVAL(XYTCH//'END',START,VMAX,START,' ',END)
      IF (PAR_ABORT()) GOTO 500
      LIM2=END

      IF (NEW) THEN
C
C        Need to create a data structure in CUBE, so need to know size
C
         CALL PAR_RDVAL(XYTCH//'SIZE',END,32767.,END,' ',SIZE)
         IF (PAR_ABORT()) GOTO 500

         IF (GROWXY) THEN
            DIMS(1)=IDX
            DIMS(2)=IDY
            DIMS(3)=SIZE
         ELSE IF (GROWXT) THEN
            DIMS(1)=IDX
            DIMS(2)=SIZE
            DIMS(3)=IDY
         ELSE
            DIMS(1)=SIZE
            DIMS(2)=IDY
            DIMS(3)=IDX
         END IF
         NX=DIMS(1)
         NY=DIMS(2)
         NT=DIMS(3)
         CALL DSA_RESHAPE_DATA('CUBE','IMAGE',3,DIMS,STATUS)
C
C        Copy IMAGE axes structures (if any) to appropriate
C        structure in CUBE.
C
         CALL DSA_SEEK_AXIS('IMAGE',1,EXIST,STATUS)
         IF(EXIST)THEN
            CALL DSA_AXIS_SIZE('IMAGE',1,5,NDIM,DIMS,NELM,STATUS)
            IF (GROWXY.OR.GROWXT) THEN
               CALL DSA_RESHAPE_AXIS('CUBE',1,'IMAGE',1,NDIM,DIMS,
     :                               STATUS)
            ELSE
               CALL DSA_RESHAPE_AXIS('CUBE',3,'IMAGE',1,NDIM,DIMS,
     :                               STATUS)
            END IF
         END IF

         CALL DSA_SEEK_AXIS('IMAGE',2,EXIST,STATUS)
         IF(EXIST)THEN
            CALL DSA_AXIS_SIZE('IMAGE',2,5,NDIM,DIMS,NELM,STATUS)
            IF (GROWXY.OR.GROWYT) THEN
               CALL DSA_RESHAPE_AXIS('CUBE',2,'IMAGE',2,NDIM,DIMS,
     :                                STATUS)
            ELSE
               CALL DSA_RESHAPE_AXIS('CUBE',3,'IMAGE',2,NDIM,DIMS,
     :                                STATUS)
            END IF
         END IF
      END IF
C
C     Map the cube data and set it to all zeros if new.
C
C     CALL DSA_MAP_DATA('CUBE','UPDATE','FLOAT',CUPTR,SLOT,STATUS)
C     IF(STATUS.NE.0)GOTO 500
C     IF (NEW) CALL GEN_FILL(NX*NY*NT*4,0,%VAL(CNF_PVAL(CUPTR)))
      IF (NEW) THEN
         CALL DSA_MAP_DATA('CUBE','WRITE','FLOAT',CUPTR,SLOT,STATUS)
         IF(STATUS.NE.0)GOTO 500
         CALL GEN_FILL(NX*NY*NT*4,0,%VAL(CNF_PVAL(CUPTR)))
      ELSE
         CALL DSA_MAP_DATA('CUBE','UPDATE','FLOAT',CUPTR,SLOT,STATUS)
         IF(STATUS.NE.0)GOTO 500
      END IF
C
C     Now, we have all the data mapped, perform the grow.
C
      IF (GROWXY) THEN
         CALL GEN_GROWXY(%VAL(CNF_PVAL(IPTR)),NX,NY,NT,LIM1,LIM2,
     :                   %VAL(CNF_PVAL(CUPTR)))
      ELSE IF (GROWXT) THEN
         CALL GEN_GROWXT(%VAL(CNF_PVAL(IPTR)),NX,NY,NT,LIM1,LIM2,
     :                   %VAL(CNF_PVAL(CUPTR)))
      ELSE
         CALL GEN_GROWYT(%VAL(CNF_PVAL(IPTR)),NX,NY,NT,LIM1,LIM2,
     :                   %VAL(CNF_PVAL(CUPTR)))
      END IF

  500 CONTINUE
C
C     Close down files etc
C
      CALL DSA_CLOSE(STATUS)

      END
