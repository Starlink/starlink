C+
      SUBROUTINE ROTX
C
C     R O T X
C
C     Rotates a data array by an integer number of pixels in the
C     X direction.  The X data is not changed.
C
C     Command parameters -
C
C     IMAGE  (Character) The name of the structure containing the data
C            to be rotated.
C
C     PIXELS (Numeric) The number of pixels by which the data is to be
C            rotated.  A positive number indicates a shift towards
C            higher pixel numbers.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C                                            KS / AAO 23rd Sept 1986
C
C     Modified:
C
C     20th Jul 1987  DJA/AAO. Modifed dynamic memory handling - now
C                    uses DYN_ routines. Alse uses revised DSA_ routines
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     30th Sep 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                    changed.
C     27th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10   MJC (Starlink)  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER   BYTES            ! Number of bytes in workspace area
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      INTEGER   I                !
      INTEGER   IGNORE           ! Used to ignore status errors
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
      INTEGER   NSPECT           ! Total number of spectra in image
      INTEGER   NX               ! Size of the data's first dimension
      INTEGER   OPTR             ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER   OSLOT            ! Map slot number output data array
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      INTEGER   PIXELS           ! Number of pixels to rotate
      INTEGER   STATUS           ! Running status for DSA_ routines
      INTEGER   TPTR             ! Temporary dynamic mem pointer
      REAL      VALUE            ! Temporary real number
      INTEGER   WPTR             ! Dynamic-memory pointer to workspace
      INTEGER   WSLOT            ! Map slot number for workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NSPECT=NELM/NX
C
C     Get number of pixels to rotate.
C
      CALL PAR_RDVAL('PIXELS',-FLOAT(NX),FLOAT(NX),FLOAT(NX/2),
     :               ' ',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      PIXELS=NINT(VALUE)
      IF (FLOAT(PIXELS).NE.VALUE) THEN
         CALL PAR_WRUSER('Note that a rotation must be by an '
     :                   //'integer number of pixels.',IGNORE)
         CALL PAR_WRUSER('The value supplied will be truncated.',IGNORE)
      END IF
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     We need workspace sufficient for one cross-section of the data.
C
      BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE(BYTES,WPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Now rotate the data.
C
      PISNEW = .FALSE.
      DO I=1,NSPECT
         CALL FIG_ROTX(NX,PIXELS,%VAL(CNF_PVAL(OPTR)),
     :                 %VAL(CNF_PVAL(WPTR)))
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(OPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNEW = ISNEW
      END DO
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_ROTX (NX,PIXELS,DATA,WORK)
C
C     F I G _ R O T X
C
C     Rotates a one dimensional array a specified number of pixels.
C
C     Parameters -  (">" input, "<" output, "!" modified, "W" workspace)
C
C     (>) NX      (Integer) Number of elements in the data
C     (>) PIXELS  (Integer) Number of elements the data is to be rotated,
C                 a positive number indicating a rotation towards higher
C                 pixel numbers.
C     (!) DATA    (Real array, DATA(NX)) The data array to be rotated.
C     (W) WORK    (Real array, WORK(NX)) Workspace.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     GEN_MOVE   (GEN_ package) Fast byte transfer from array to array.
C
C                                                KS / AAO 23rd Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, PIXELS
      REAL DATA(NX), WORK(NX)
C
      IF (PIXELS.GT.0) THEN
         CALL GEN_MOVE((NX-PIXELS)*4,DATA(1),WORK(PIXELS+1))
         CALL GEN_MOVE(PIXELS*4,DATA(NX-PIXELS+1),WORK(1))
         CALL GEN_MOVE(NX*4,WORK,DATA)
      ELSE IF (PIXELS.LT.0) THEN
         CALL GEN_MOVE((NX+PIXELS)*4,DATA(1-PIXELS),WORK(1))
         CALL GEN_MOVE(-PIXELS*4,DATA(1),WORK(NX+PIXELS+1))
         CALL GEN_MOVE(NX*4,WORK,DATA)
      END IF
C
      END
