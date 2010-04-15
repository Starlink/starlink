C+
      SUBROUTINE PLANE
C
C     Y T P L A N E ,   X T P L A N E,    X Y P L A N E
C
C     Adds a number of consecutive planes from a cube to
C     produce a 2D data object.  YTPLANE, XTPLANE and XYPLANE
C     take planes whose X,Y or T values respectively are
C     constant over the extracted plane.  (Pedantic note:
C     strictly, a cuboid is meant, rather than a cube.)
C     (Further pedantic note: the X, Y and T axes mentioned
C     in this routine should really be referred to as AXIS(1),
C     AXIS(2) and AXIS(3) respectively. However, explanations become
C     confusing if this is done, so the references to X, Y and T
C     remain. This does not mean that the data are stored in
C     .X .Y and .T structures - JM.)
C
C     Command parameters -
C
C     'CUBE'     The name of the cube from which the planes
C                are to be taken.
C
C     'XSTART'   The X,Y, or T-value of the first plane to be used.
C     'YSTART'   If CUBE has a X, Y or T  structure, the data
C     'TSTART'   from this is used.  If not, the plane numbers
C                are used, starting from 1.
C
C     'XEND'     The X, Y, or T-value of the last plane to be used.
C     'YEND'
C     'TEND'
C
C     'IMAGE'    The name of the resulting data structure.
C
C     Input data -
C
C     CUBE - 3-D data-array
C
C     Output data -
C
C     IMAGE is created with the same structure as CUBE
C     except that the data array will only have 2 dimensions, and if
C     CUBE has a X,Y or T structure, this will be omitted -
C     that is, XPLANE will omit any X structure, YPLANE any
C     Y structure and  etc.
C
C                                     KS / AAO 8th Jan 1985
C     Modified:
C
C     21st Nov 1988  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER.
C     25th Jul 1995  HME / UoE, Starlink.  Map output with write access.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      REAL      AXEND            ! Data value of end of selected axis
                                 ! range
      INTEGER   AXIS             ! Axis number
      REAL      AXST             ! Data value of start of selected axis
                                 ! range
      CHARACTER COMMAND*6        ! Command name
      INTEGER   CUPTR            ! Dynamic-memory pointer for cube data
      INTEGER   DIMS(5)          ! Various dimensions
      LOGICAL   EXIST            ! Axis data exist?
      INTEGER   IEN              ! Higher axis pixel number
      INTEGER   IPTR             ! Dynamic-memory pointer for image data
      INTEGER   IST              ! Lower axis pixel number
      INTEGER   J                ! Do loop variable
      INTEGER   NDIM             ! Number of various dimensions
      INTEGER   NELM             ! Number of elements in various arrays
      INTEGER   NT               ! Third dimension of cube
      INTEGER   NX               ! First dimension of cube
      INTEGER   NY               ! Second dimension of cube
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Initial values
C
      STATUS=0
      EXIST=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get name of CUBE file and open file
C
      CALL DSA_INPUT ('CUBE','CUBE',STATUS)
C
C     Get size of data in CUBE
C
      CALL DSA_DATA_SIZE ('CUBE',5,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF (NDIM.NE.3) THEN
         CALL PAR_WRUSER('Input data is not a cube',STATUS)
         GO TO 500
      END IF
      NT=DIMS(3)
      NY=DIMS(2)
      NX=DIMS(1)
C
C     Find which command this is, and which dimension we
C     are slicing in.
C
      CALL PAR_COMMAND(COMMAND)
      IF (COMMAND(1:2).EQ.'YT') THEN
         AXIS=1
      ELSE IF (COMMAND(1:2).EQ.'XT') THEN
         AXIS=2
      ELSE IF (COMMAND(1:2).EQ.'XY') THEN
         AXIS=3
      ELSE
         CALL PAR_WRUSER(
     :      'Internal error, unexpected command name',STATUS)
      END IF
C
C     Select axis limits
C
      CALL DSA_AXIS_RANGE('CUBE',AXIS,' ',.FALSE.,AXST,AXEND,IST,IEN,
     :                     STATUS)
C
C     The output IMAGE file, based on CUBE but without the axis
C     and data information, is now created.
C
      CALL DSA_OUTPUT('IMAGE','IMAGE','CUBE',NO_DATA,NEW_FILE,STATUS)

      IF (AXIS.EQ.1) THEN
         DIMS(1)=NT
         DIMS(2)=NY
      ELSE IF (AXIS.EQ.2) THEN
         DIMS(1)=NX
         DIMS(2)=NT
      ELSE
         DIMS(1)=NX
         DIMS(2)=NY
      END IF

C
C     The IMAGE data structure is now created. This is based on that
C     of CUBE, but has the appropriate dimensions for IMAGE.
C
      CALL DSA_RESHAPE_DATA('IMAGE','CUBE',2,DIMS,STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA('CUBE','READ','FLOAT',CUPTR,SLOT,STATUS)

      CALL DSA_MAP_DATA('IMAGE','WRITE','FLOAT',IPTR,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Perform the extraction
C
      CALL FIG_PLANE(%VAL(CNF_PVAL(CUPTR)),NX,NY,NT,AXIS,IST,IEN,
     :               DIMS(1),DIMS(2),%VAL(CNF_PVAL(IPTR)))
C
C     If the original cube had certain axis structures these
C     may need to be copied and possibly renamed.
C
      CALL DSA_SEEK_AXIS('CUBE',3,EXIST,STATUS)
      IF(EXIST.AND.(AXIS.NE.3)) THEN
         CALL DSA_AXIS_SIZE('CUBE',3,5,NDIM,DIMS,NELM,STATUS)
         CALL DSA_RESHAPE_AXIS('IMAGE',AXIS,'CUBE',3,NDIM,DIMS,STATUS)
      END IF

      DO J=1,2
         CALL DSA_SEEK_AXIS('CUBE',J,EXIST,STATUS)
         IF(EXIST.AND.(AXIS.NE.J))THEN
            CALL DSA_AXIS_SIZE('CUBE',J,5,NDIM,DIMS,NELM,STATUS)
            CALL DSA_RESHAPE_AXIS('IMAGE',J,'CUBE',J,NDIM,DIMS,STATUS)
         END IF
      END DO

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
C+
      SUBROUTINE FIG_PLANE(CUBE,NX,NY,NT,AXIS,IST,IEN,NIX,NIY,IMAGE)
C
C     F I G _ P L A N E
C
C     Adds together consecutive planes of a cube to form
C     a 2-dimensional image.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) CUBE      (Real array, CUBE(NX,NY,NT)) The cube.
C     (>) NX        (Integer) The first dimension of the cube.
C     (>) NY        (Integer) The second dimension of the cube.
C     (>) NT        (Integer) The third dimension of the cube.
C     (>) AXIS      (Integer) The axis relative to which the extracted
C                   image is constant.  Ie, if AXIS=1, the image has
C                   a constant X value.
C     (>) IST       (Integer) The number of the first of the
C                   planes to be added.
C     (>) IEN       (Integer) The number of the last of the
C                   planes to be added.
C     (>) NIX       (Integer) The first dimension of the image.
C     (>) NIY       (Integer) The second dimension of the image.
C     (<) IMAGE     (Real array IMAGE(NIX,NIY)) The resulting image.
C
C     Common variables used -  None.
C
C     Subroutines / functions used -  None.
C
C                                    KS / AAO  8th Jan 1985
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,NT,AXIS,IST,IEN,NIX,NIY
      REAL CUBE(NX,NY,NT),IMAGE(NIX,NIY)
C
C     Local variables
C
      INTEGER I1, I2, IX, IY, IT
C
C     Perform extraction
C
      IF (AXIS.EQ.1) THEN
         I1=MAX(IST,1)
         I2=MIN(IEN,NX)
         DO IT=1,NT
            DO IY=1,NY
               IMAGE(IT,IY)=0.
               DO IX=I1,I2
                  IMAGE(IT,IY)=IMAGE(IT,IY)+CUBE(IX,IY,IT)
               END DO
            END DO
         END DO
      ELSE IF (AXIS.EQ.2) THEN
         I1=MAX(IST,1)
         I2=MIN(IEN,NY)
         CALL GEN_FILL(NIX*NIY*4,0,IMAGE)
         DO IT=1,NT
            DO IY=I1,I2
               DO IX=1,NX
                  IMAGE(IX,IT)=IMAGE(IX,IT)+CUBE(IX,IY,IT)
               END DO
            END DO
         END DO
      ELSE IF (AXIS.EQ.3) THEN
         I1=MAX(IST,1)
         I2=MIN(IEN,NT)
         CALL GEN_FILL(NIX*NIY*4,0,IMAGE)
         DO IT=I1,I2
            DO IY=1,NY
               DO IX=1,NX
                  IMAGE(IX,IY)=IMAGE(IX,IY)+CUBE(IX,IY,IT)
               END DO
            END DO
         END DO
      END IF
C
      END
