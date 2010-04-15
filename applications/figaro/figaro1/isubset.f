C+
      SUBROUTINE ISUBSET
C
C     I S U B S E T
C
C     Creates a subset of an image.
C
C     Command parameters -
C
C     IMAGE  (Char) The name of the structure containing the image.
C     YSTART (Numeric) The AXIS(2) value for the start of the subset.
C     YEND   (Numeric) The AXIS(2) value for the end of the subset.
C     XSTART (Numeric) The AXIS(1) value for the start of the subset.
C     XEND   (Numeric) The AXIS(1) value for the end of the subset.
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C                                      KS / CIT 19th June 1984
C     Modified:
C
C     24 Jan 1985  KS / AAO.  Now works properly in the case
C                  where YSTART=YEND, creating a 1D output object
C                  even if OUTPUT=IMAGE.
C     26 Jun 1986  KS / AAO.  Error handling revised so tidying
C                  up is performed properly. Re-naming of arrays
C                  to change size deferred to after unmapping.
C     21 Aug 1988  JM / AAO. Modified to use DSA_ routines
C                  Dynamic memory handling changed to use
C                  DYN_ routines. Routine now subsets error and
C                  quality arrays.
C     07 Dec 1989  KS / AAO. Now checks for flagged values and axis
C                  width arrays, and uses GEN_SUBSETB to handle the
C                  quality array.  Minor bug (IXST,IYST set to 1 if
C                  axes had 1..N values) corrected.  Handles the case
C                  where the axis data arrays are 2-dimensional,
C                  and differentiates between uncertainty and variance
C                  arrays.
C     12 Oct 1992  HME / UoE, Starlink.  INCLUDE changed.
C                  If only (part of) one row was selected from an image,
C                  this routine would still try to create a second axis.
C                  Fixed that. But this routine still cannot (properly)
C                  subset a column into a spectrum.
C     06 Oct 1993  HME / UoE, Starlink.  Mapping the output for update
C                  causes DSA_MAP_DATA to unflag whatever garbage may
C                  be in the new array.  Change access to "write".
C                  (This is for the output data only, output axis,
C                  error, variance and quality arrays are still mapped
C                  update.)  Related to this, the routine called
C                  dsa_use_* for input, but not for output.  Now calls
C                  it for both.
C     12 Feb 1995  KS/AAO. Now calls DSA_USE_FLAGGED_VALUES and
C                  DSA_USE_QUALITY for the output as well as for the
C                  input.
C     29 Nov 1995  KS/AAO. Now calls DSA_QUALITY_AND_FLAGS_OK to
C                  indicate that the program can handle both types of
C                  quality information simultaneously.
C     01 Dec 1995  HME / UoE, Starlink.  Re-did the corrections of 6
C                  October 1993. In fact, since then, all output was
C                  changed to map for write access, since there had
C                  been problems when axis data were double precision in
C                  the file.
C                  Due to an oversight then, DSA_USE_ was called twice
C                  for input instead of for input and for output. This
C                  has been fixed by KS.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Concurrent mapping. Had to swap mapping axis data and
C                  getting axis ranges.
C                  Bad pixel handling.
C     22 Jul 1996  MJC / STARLINK.  Take the appropriate subset of
C                  2-dimensional axis centres, and all axis widths.
C     24 Jul 1996  MJCL / Starlink, UCL.  Corrected type of SINGLE.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL GEN_CHKNSF
C
C     Local variables
C
      INTEGER  DIMS(2)           ! Image dimensions
      LOGICAL  EEXIST            ! Image has an error array?
      INTEGER  EPTR              ! Dynamic mem pointer for input errors
      INTEGER  EPTRO             ! Dynamic mem pointer for output errors
      LOGICAL  ERRORS            ! Error information is an uncertainty
                                 ! array?
      LOGICAL  FEXIST            ! Data contains flagged values?
      INTEGER  IPTR              ! Dynamic mem pointer for input data
      INTEGER  IPTRO             ! Dynamic mem pointer for output data
      INTEGER  ITXST             ! Temporary value of IXST used for axis
                                 ! data only
      INTEGER  ITYST             ! Temporary value of IYST used for axis
                                 ! data only
      INTEGER  IXEN              ! Index of last AXIS(1) element used
      INTEGER  IXST              ! Index of first AXIS(1) element used
      INTEGER  IYEN              ! Index of last AXIS(2) element used
      INTEGER  IYST              ! Index of first AXIS(2) element used
      INTEGER  NDIM              ! Number of image dimensions
      INTEGER  NELM              ! Number of elements in image
      LOGICAL  NONE              ! No error information available?
      INTEGER  NX                ! First dimension of image
      INTEGER  NXNEW             ! First dimension of subsetted image
      INTEGER  NY                ! Second dimension of image
      INTEGER  NYNEW             ! Second dimension of subsetted image
      LOGICAL  SINGLE            ! True if axis width is a single value
      INTEGER  SLOT              ! Slot number for mapped data -ignored
      INTEGER  STATUS            ! Running status for DSA routines
      LOGICAL  VARIANCE          ! Error information is a variance
                                 ! array?
      DOUBLE PRECISION WIDTH     ! Single axis width value - ignored
      INTEGER  XDIMS(2)          ! Dimensions of AXIS(1) data array
      INTEGER  XDIMSN(2)         ! Dimensions of new AXIS(1) data array
      REAL     XEN               ! Value of last AXIS(1) element used
      LOGICAL  XEXIST            ! Image has an AXIS(1) data array?
      INTEGER  XNDIM             ! Number of dimensions of original X
                                 ! data array
      INTEGER  XNDIMN            ! Number of dimensions of new X data
                                 ! array
      INTEGER  XPTR              ! Dynamic mem pointer for input AXIS(1)
                                 ! data
      INTEGER  XPTRO             ! Dynamic mem pointer for output
                                 ! AXIS(1) data
      REAL     XST               ! Value of first AXIS(1) element used
      LOGICAL  XWEXIST           ! AXIS(1) array also has a width array?
      INTEGER  XWPTR             ! Dynamic mem pointer for input AXIS(1)
                                 ! width data
      INTEGER  XWPTRO            ! Dynamic mem pointer for output
                                 ! AXIS(1) width data
      INTEGER  YDIMS(2)          ! Dimensions of AXIS(2) data array
      INTEGER  YDIMSN(2)         ! Dimensions of new AXIS(2) data array
      REAL     YEN               ! Value of last AXIS(2) element used
      LOGICAL  YEXIST            ! Image has an AXIS(2) data array?
      INTEGER  YNDIM             ! Number of dimensions of original Y
                                 ! data array
      INTEGER  YNDIMN            ! Number of dimensions of new Y data
                                 ! array
      INTEGER  YPTR              ! Dynamic mem pointer for input AXIS(2)
                                 ! data
      INTEGER  YPTRO             ! Dynamic mem pointer for output
                                 ! AXIS(2) data
      LOGICAL  YWEXIST           ! AXIS(2) array also has a width array?
      INTEGER  YWPTR             ! Dynamic mem pointer for input AXIS(2)
                                 ! width data
      INTEGER  YWPTRO            ! Dynamic mem pointer for output
                                 ! AXIS(2) width data
      REAL     YST               ! Value of first AXIS(2) element used
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
      IF (NDIM.EQ.1) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
      NX=DIMS(1)
C
C     Map input data
C
      CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES('IMAGE',FEXIST,STATUS)
C
C     Don't bother with AXIS(2) values if this is a 1D 'image'
C
      IF (NDIM.GT.1) THEN
C
C        Check for existence of AXIS(2) data array. If this exists, it
C        is mapped and the limits for subsetting are selected.
C
         CALL DSA_SEEK_AXIS('IMAGE',2,YEXIST,STATUS)
         CALL DSA_AXIS_RANGE('IMAGE',2,' ',.FALSE.,YST,YEN,IYST,IYEN,
     :                        STATUS)
         NYNEW=IYEN-IYST+1
         IF(YEXIST)THEN
            CALL DSA_MAP_AXIS_DATA('IMAGE',2,'READ','FLOAT',YPTR,
     :                             SLOT,STATUS)
         END IF
C
C        Check for the existence of a width array and map it if
C        it exists and is not just a single value.
C
         CALL DSA_SEEK_WIDTH('IMAGE',2,YWEXIST,SINGLE,WIDTH,STATUS)
         IF (YWEXIST.AND.SINGLE) YWEXIST=.FALSE.
         IF (YWEXIST) THEN
            CALL DSA_MAP_WIDTH('IMAGE',2,'READ','FLOAT',YWPTR,
     :                         SLOT,STATUS)
         END IF
      ELSE
C
C        1D case
C
         NYNEW=1
         IYST=1
         IYEN=1
      END IF
C
C     And ditto for X..
C
      CALL DSA_SEEK_AXIS('IMAGE',1,XEXIST,STATUS)
      CALL DSA_AXIS_RANGE('IMAGE',1,' ',.FALSE.,XST,XEN,IXST,IXEN,
     :                     STATUS)
      NXNEW=IXEN-IXST+1
      IF(XEXIST)THEN
         CALL DSA_MAP_AXIS_DATA('IMAGE',1,'READ','FLOAT',XPTR,SLOT,
     :                           STATUS)
      END IF
      CALL DSA_SEEK_WIDTH('IMAGE',1,XWEXIST,SINGLE,WIDTH,STATUS)
      IF (XWEXIST.AND.SINGLE) XWEXIST=.FALSE.
      IF (XWEXIST) THEN
         CALL DSA_MAP_WIDTH('IMAGE',1,'READ','FLOAT',XWPTR,
     :                      SLOT,STATUS)
      END IF
C
C     Get output structure name and indicate that we can handle
C     flagged values.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF (FEXIST) CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
C
C     Create data components of the appropriate size in the output
C     structure and map them.
C
      DIMS(1)=NXNEW
      DIMS(2)=NYNEW
      IF (NYNEW.GT.1) THEN
         NDIM=2
      ELSE
         NDIM=1
      END IF
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,DIMS,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',IPTRO,SLOT,STATUS)
C
      IF (XEXIST) THEN
         CALL DSA_AXIS_SIZE('IMAGE',1,2,XNDIM,XDIMS,NELM,STATUS)
         XDIMSN(1)=NXNEW
         XNDIMN=XNDIM
         IF (XNDIM.EQ.1) THEN
            XDIMS(2)=1
            XDIMSN(2)=1
         ELSE
            XDIMSN(2)=NYNEW
         END IF
         CALL DSA_RESHAPE_AXIS('OUTPUT',1,'IMAGE',1,XNDIMN,XDIMSN,
     :                                                       STATUS)
         CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE','FLOAT',XPTRO,
     :                           SLOT,STATUS)
         IF (XWEXIST) THEN
            CALL DSA_MAP_WIDTH('OUTPUT',1,'WRITE','FLOAT',XWPTRO,
     :                         SLOT,STATUS)
         END IF
      END IF
      IF (NDIM.GT.1.AND.YEXIST) THEN
         CALL DSA_AXIS_SIZE('IMAGE',2,2,YNDIM,YDIMS,NELM,STATUS)
         YDIMSN(1)=NYNEW
         YNDIMN=YNDIM
         IF (YNDIM.EQ.1) THEN
            YDIMS(2)=1
            YDIMSN(2)=1
         ELSE
            YDIMSN(2)=NXNEW
         END IF
         CALL DSA_RESHAPE_AXIS('OUTPUT',2,'IMAGE',2,YNDIMN,YDIMSN,
     :                         STATUS)
         CALL DSA_MAP_AXIS_DATA('OUTPUT',2,'WRITE','FLOAT',YPTRO,
     :                          SLOT,STATUS)
         IF (YWEXIST) THEN
            CALL DSA_MAP_WIDTH('OUTPUT',2,'WRITE','FLOAT',YWPTRO,
     :                         SLOT,STATUS)
         END IF
      END IF
C
C     Map input and output error or variance arrays if they exists.
C     Reshaping of these arrays is automatically performed by
C     DSA_RESHAPE_DATA.  The reshaping is independent of which
C     type of error data we have, but its more efficient to map the
C     actual array in the data structure than a processed one.
C
      CALL DSA_SEEK_ERRORS('IMAGE',EEXIST,STATUS)
      IF(EEXIST)THEN
         CALL DSA_ERROR_INFORMATION('IMAGE',ERRORS,VARIANCE,NONE,
     :                              STATUS)
         IF (ERRORS) THEN
            CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',EPTR,SLOT,
     :                          STATUS)
         ELSE
            CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',EPTR,
     :                            SLOT,STATUS)
         END IF
         IF (ERRORS) THEN
            CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',EPTRO,
     :                          SLOT,STATUS)
         ELSE
            CALL DSA_MAP_VARIANCE('OUTPUT','WRITE','FLOAT',EPTRO,
     :                            SLOT,STATUS)
         END IF
      END IF
C
      IF(STATUS.NE.0)GOTO 500
C
C     Perform the subsetting.  This is complex for the axis data arrays,
C     since a) they may not exist, b) if they simply enumerate the
C     element numbers, we want to do the same in the result image,
C     rather than just take the corresponding subset.
C
C     First, subset the data
C
      CALL GEN_SUBSET(%VAL(CNF_PVAL(IPTR)),NX,NY,NXNEW,NYNEW,IXST,IYST,
     :                %VAL(CNF_PVAL(IPTRO)))
C
C     Now the AXIS(1) data if any (checking for the nos 1-N).  Also
C     take the appropriate section from a 2-dimensional axis when this
C     axis does not have values 1 to NX*NY.
C
      IF(XEXIST)THEN
         IF (GEN_CHKNSF(%VAL(CNF_PVAL(XPTR)),NX)) THEN
            ITXST=1
         ELSE
            ITXST=IXST
         END IF

         IF ( XNDIM .GT. 1 ) THEN
            CALL GEN_SUBSET(%VAL(CNF_PVAL(XPTR)),NX,XDIMS(2),NXNEW,
     :                      XDIMSN(2),IXST,IYST,%VAL(CNF_PVAL(XPTRO)))
         ELSE
            CALL GEN_SUBSET(%VAL(CNF_PVAL(XPTR)),NX,XDIMS(2),NXNEW,
     :                      XDIMSN(2),ITXST,1,%VAL(CNF_PVAL(XPTRO)))
         END IF
C
C     And the AXIS(1) width, if any.  Note that although the axis
C     centres always start at one if they are the pixel indices and
C     not in a 2-dimensional array, the axis widths extracted should
C     correspond to the data-array values propagated, and therefore
C     start at the lower bound.  Previously, they had started at ITXST.
C     (MJC).
C
         IF (XWEXIST) THEN
            IF ( XNDIM .GT. 1 ) THEN
               CALL GEN_SUBSET(%VAL(CNF_PVAL(XWPTR)),NX,XDIMS(2),NXNEW,
     :                         XDIMSN(2),IXST,IYST,
     :                         %VAL(CNF_PVAL(XWPTRO)))
            ELSE
               CALL GEN_SUBSET(%VAL(CNF_PVAL(XWPTR)),NX,XDIMS(2),NXNEW,
     :                         XDIMSN(2),IXST,1,%VAL(CNF_PVAL(XWPTRO)))
            END IF
         END IF
      END IF
C
C    Ditto for AXIS(2).
C
      IF (NDIM.GT.1.AND.YEXIST) THEN
         IF (GEN_CHKNSF(%VAL(CNF_PVAL(YPTR)),NY)) THEN
            ITYST=1
         ELSE
            ITYST=IYST
         END IF

         IF ( YNDIM .GT. 1 ) THEN
            CALL GEN_SUBSET(%VAL(CNF_PVAL(YPTR)),NY,YDIMS(2),NYNEW,
     :                      YDIMSN(2),IYST,IXST,%VAL(CNF_PVAL(YPTRO)))
         ELSE
            CALL GEN_SUBSET(%VAL(CNF_PVAL(YPTR)),NY,YDIMS(2),NYNEW,
     :                      YDIMSN(2),ITYST,1,%VAL(CNF_PVAL(YPTRO)))
         END IF
C
C        And the AXIS(2) width, if any.
C
         IF (YWEXIST) THEN
            IF ( YNDIM .GT. 1 ) THEN
               CALL GEN_SUBSET(%VAL(CNF_PVAL(YWPTR)),NY,YDIMS(2),NYNEW,
     :                         YDIMSN(2),IYST,IXST,
     :                         %VAL(CNF_PVAL(YWPTRO)))
            ELSE
               CALL GEN_SUBSET(%VAL(CNF_PVAL(YWPTR)),NY,YDIMS(2),NYNEW,
     :                         YDIMSN(2),IYST,1,%VAL(CNF_PVAL(YWPTRO)))
            END IF
         END IF
      END IF
C
C     And the error (or variance) array (if any)
C
      IF(EEXIST)THEN
         CALL GEN_SUBSET(%VAL(CNF_PVAL(EPTR)),NX,NY,NXNEW,NYNEW,IXST,
     :                   IYST,%VAL(CNF_PVAL(EPTRO)))
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
