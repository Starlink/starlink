C+
C                        D S A _ F I L L _ W I D T H
C
C  Routine name:
C     DSA_FILL_WIDTH
C
C  Function:
C     Fills a newly created width array with the appropriate data values.
C
C  Description:
C     This is a utility routine designed to be called by DSA_MAP_WIDTH.
C     It takes a newly created and mapped axis width array and fills it
C     with the appropriate data values.  These will either be a constant
C     single width, the apparent widths taken by assuming that the ranges
C     of the axis data elements are continuous but not overlapping (ie the
C     range is the difference between successive axis data values), or, if
C     all else fails, the value 1.0 in all cases.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FILL_WIDTH (REF_NAME,AXIS,SINGLE,WIDTH,ADDRESS,
C                                               NDIM,DIMS,TYPE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name for the
C                       structure whose width array is being mapped.
C     (>) AXIS          (Integer,ref) The number of the axis in question.
C     (>) SINGLE        (Logical,ref) True if there is a single width
C                       value that applies to the whole array.
C     (>) WIDTH         (Double,ref) If there is such a single width,
C                       this is it.
C     (>) ADDRESS       (Integer,ref) The virtual address of the start
C                       of the mapped array.
C     (>) NDIM          (Integer,ref) The number of dimensions of the
C                       width array.
C     (>) DIMS          (Integer array,ref) The dimensions of the width
C                       array.
C     (>) TYPE          (Fixed string,descr) The type of the mapped data.
C                       This should be a DTA system primitive type.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is passed
C                       to it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     CNF_PVAL, ICH_LEN, ICH_CI, DSA_SEEK_AXIS,
C     DSA_CFILL_ARRAY, DSA_GET_WORK_ARRAY, DSA_MAP_AXIS_DATA,
C     DSA_CALC_WIDTH, DSA_UNMAP, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_WRFLUSH, DSA_FREE_WORKSPACE, VEC_DTOx
C
C  Prior requirements:
C     This is intended to be called by DSA_MAP_WIDTH.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  Subroutine / function details:
C     CNF_PVAL             Full pointer to dynamically allocated memory
C     ICH_LEN              Position of last non blank char in string
C     ICH_CI               Formats an integer into a character string
C     DSA_SEEK_AXIS        See if axis data exists
C     DSA_CFILL_ARRAY      Fill an array with a constant
C     DSA_GET_WORK_ARRAY   Get a work array of a specified type
C     DSA_MAP_AXIS_DATA    Map the data array for a specified axis
C     DSA_CALC_WIDTH       Calcualte axis widths from the axis data
C     DSA_UNMAP            Unmap a mapped array
C     DSA_WRUSER           Output a string to the user
C     DSA_WRFLUSH          Flush messages to user
C     DSA_FREE_WORKSPACE   Free workspace obtained by DSA_GET_WORK_ARRAY
C     DSA_GET_ACTUAL_NAME  Get full name of data structure from ref name
C     VEC_DTOx             Convert array from DOUBLE to correct type
C
C  History:
C     29th Aug 1988.  Original version.  KS / AAO.
C     31st Aug 1992.  Replace CNV_ call with VEC_ calls.
C                     HME / UoE, Starlink. Added use of DSA_WRFLUSH. KS/AAO
C     2005 June 3     Replace DYNAMIC_MEMORY with
C                     %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                     addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_FILL_WIDTH (REF_NAME,AXIS,SINGLE,WIDTH,ADDRESS,
     :                                         NDIM,DIMS,TYPE,STATUS)
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      LOGICAL SINGLE
      INTEGER AXIS, ADDRESS, NDIM, DIMS(NDIM), STATUS
      CHARACTER*(*) REF_NAME, TYPE
      DOUBLE PRECISION WIDTH
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*16 ICH_CI
C
C     Local variables
C
      INTEGER   AXIS_ADDR        ! Virtual address of axis data array
      INTEGER   AXIS_SLOT        ! Slot number for axis mapping
      LOGICAL   EXIST            ! True if axis data exists
      INTEGER   I                ! Loop index
      INTEGER   IGNORE           ! Dummy status argument
      INTEGER   NBAD             ! Number of bad conversions
      INTEGER   NELM             ! Number of elements in array
      INTEGER   NSECT            ! Number of 1D cross-sections in data
      CHARACTER NUMBER*16        ! Used to format numbers
      LOGICAL   SIMPLE           ! True if a  single value can be used
      CHARACTER STRUCTURE*64     ! Full name of structure
      DOUBLE PRECISION VALUE     ! Single value for width
      INTEGER   WORK_ADDR        ! Virtual address of work array
      INTEGER   WORK_SLOT        ! Slot number for work array
      INTEGER   IERR, VECSTA     ! Unused for VEC_
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Work out the elements in the array, and the number of 1D
C     cross-sections.
C
      NELM=1
      DO I=1,NDIM
         NELM=NELM*DIMS(I)
      END DO
      NSECT=NELM/DIMS(1)
C
C     See if a single value will do
C
      IF (SINGLE) THEN
         SIMPLE=.TRUE.
         VALUE=WIDTH
      ELSE
         CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
         IF (.NOT.EXIST) THEN
            SIMPLE=.TRUE.
            VALUE=1.0D0
         ELSE
            SIMPLE=.FALSE.
         END IF
      END IF
      IF (SIMPLE) THEN
         CALL DSA_CFILL_ARRAY (NELM,ADDRESS,VALUE,TYPE,STATUS)
      ELSE
C
C        A single value will not do.  We have to map the axis array
C        and use its values.  Rather than do this in the natural type
C        of the array and have to handle all the possible combinations
C        of axis type and width type, we do it all in double precision,
C        which will involve some overhead, but is safe and easier to code.
C
         IF (TYPE.EQ.'DOUBLE') THEN
            WORK_ADDR=ADDRESS
         ELSE
            CALL DSA_GET_WORK_ARRAY (NELM,'DOUBLE',WORK_ADDR,
     :                               WORK_SLOT,STATUS)
         END IF
         CALL DSA_MAP_AXIS_DATA (REF_NAME,AXIS,'READ','DOUBLE',
     :                           AXIS_ADDR,AXIS_SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500      ! Error exit
         CALL DSA_CALC_WIDTH (%VAL(CNF_PVAL(AXIS_ADDR)),DIMS(1),NSECT,
     :                        %VAL(CNF_PVAL(WORK_ADDR)),STATUS)
         CALL DSA_UNMAP (AXIS_SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500      ! Error exit
         IF (TYPE.NE.'DOUBLE') THEN
            VECSTA=0
            IF (TYPE.EQ.'BYTE') THEN
               CALL VEC_DTOB(.FALSE.,NELM,%VAL(CNF_PVAL(WORK_ADDR)),
     :            %VAL(CNF_PVAL(ADDRESS)),IERR,NBAD,VECSTA)
            ELSE IF (TYPE.EQ.'SHORT'.OR.TYPE.EQ.'WORD') THEN
               CALL VEC_DTOW(.FALSE.,NELM,%VAL(CNF_PVAL(WORK_ADDR)),
     :            %VAL(CNF_PVAL(ADDRESS)),IERR,NBAD,VECSTA)
            ELSE IF (TYPE.EQ.'INT') THEN
               CALL VEC_DTOI(.FALSE.,NELM,%VAL(CNF_PVAL(WORK_ADDR)),
     :            %VAL(CNF_PVAL(ADDRESS)),IERR,NBAD,VECSTA)
            ELSE IF (TYPE.EQ.'FLOAT'.OR.TYPE.EQ.'REAL') THEN
               CALL VEC_DTOR(.FALSE.,NELM,%VAL(CNF_PVAL(WORK_ADDR)),
     :            %VAL(CNF_PVAL(ADDRESS)),IERR,NBAD,VECSTA)
            ELSE IF (TYPE.EQ.'USHORT'.OR.TYPE.EQ.'UWORD') THEN
               CALL VEC_DTOUW(.FALSE.,NELM,%VAL(CNF_PVAL(WORK_ADDR)),
     :            %VAL(CNF_PVAL(ADDRESS)),IERR,NBAD,VECSTA)
            END IF
            IF (NBAD.NE.0) THEN
               CALL DSA_WRUSER ('Warning - ')
               NUMBER=ICH_CI(NBAD)
               CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
               CALL DSA_WRUSER(' type conversion')
               IF (NBAD.EQ.1) THEN
                  CALL DSA_WRUSER(' error')
               ELSE
                  CALL DSA_WRUSER(' errors')
               END IF
               CALL DSA_WRUSER(
     :            ' occurred while calculating a width array of type "')
               CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
               CALL DSA_WRUSER('" for the structure ')
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
            END IF
            CALL DSA_FREE_WORKSPACE(WORK_SLOT,STATUS)
         END IF
C
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
