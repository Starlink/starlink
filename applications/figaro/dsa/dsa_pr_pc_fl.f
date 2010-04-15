C+
C        D S A _ P R E _ P R O C E S S _ F L A G G E D _ V A L U E S
C
C  Routine name:
C     DSA_PRE_PROCESS_FLAGGED_VALUES
C
C  Function:
C     Performs pre-processing for applications using flagged data.
C
C  Description:
C     This routine must be called by any routine that is using flagged
C     data values to process data.  It should be called after the
C     data array for the structure in question has been mapped, but
C     before any processing is done on it.  It handles any processing
C     that may be required to deal with data arrays that are stored on
C     disk with data quality arrays rather than directly with flagged
C     elements.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_PRE_PROCESS_FLAGGED_VALUES (REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name for the
C                        structure being processed.
C     (!) STATUS         (Integer,ref) Status code.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     CNF_PVAL, ICH_FOLD, ICH_LEN, DSA_FIND_REF, DSA_GET_ACTUAL_NAME,
C     DSA_WRUSER, DSA_REFLAG_DATA
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure must have been opened using DSA_INPUT, DSA_OUTPUT or
C     an equivalent routine.  The data array must have been mapped.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 3rd February 1995
C-
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     ICH_FOLD         Convert string to upper case
C     ICH_LEN          Position of last non-blank char in string
C     DSA_FIND_REF     Look up reference name in common tables
C     DSA_GET_ACTUAL_NAME  Get full name corresponding to reference name
C     DSA_ACT_MAP_QUALITY  Map the quality array (or a dummy)
C     DSA_WRUSER       Output message to user
C     DSA_REFLAG_DATA  Flag data values using a quality array.
C
C  Common variable details:
C     (>) MAP_POINTER   (Integer array) Memory address for the array.
C     (>) MAP_TYPE      (String array) Type of actual data object.
C     (>) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (>) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C     (>) DATA_NDIM     (Integer array) Number of dimensions of main data array.
C     (>) DATA_DIMS     (Integer array) Dimensions of main data array. (2D)
C     (>) DATA_SLOT     (Integer array) Map call slot for data mapping.
C     (<) QUALITY_SLOT  (Integer array) Map call slot for quality mapping.
C     (>) WORK_POINTER  (Integer array) Memory address of workspace array.
C     (>) WORK_TYPE     (String array)  Type of array held in workspace.
C     (<) PRE_FLAG      (Logical array) Indicates pre-processing done.
C
C  History:
C     21st July 1988 Original version.  KS / AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C      3rd Feb 1995  Now supports files with both flagged values and
C                    quality arrays, and simulataneous use of both by
C                    an application.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_PRE_PROCESS_FLAGGED_VALUES (REF_NAME,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      INTEGER   DATA_ADDR                   ! Virtual address of data array
      CHARACTER DATA_TYPE*16                ! Type of mapped data array
      INTEGER   I                           ! Index through dimensions
      INTEGER   IGNORE                      ! Dummy status value
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      INTEGER   MAP_SLOT                    ! Mapping slot for data
      CHARACTER MODE*8                      ! Quality mapping mode
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   QUAL_ADDR                   ! Virtual address of quality array
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Full structure name from ref_name
      INTEGER   WORK_SLOT                   ! Workspace slot for data
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     Look up the reference name in the tables and get the data
C     array dimensions.
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     Check that data has been mapped.
C
      IF (DATA_SLOT(REF_SLOT).EQ.0) THEN
         CALL DSA_WRUSER('The main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER(
     :        ' has not been mapped. DSA_PRE_PROCESS_FLAGGED_VALUES ')
         CALL DSA_WRUSER(
     :        'has been called out of sequence.  Programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__SEQERR
         GO TO 500           ! Error exit
      END IF
C
C     Now, see if we need to do anything.  The only condition we have
C     to look for is the case where the structure has a quality array,
C     and the calling program is not handling data quality itself,
C     in which case we have to flag the appropriate elements.  Note
C     that since the data has been mapped, we can assume that QUAL_EXIST
C     is now set one way or the other, so we can use it directly.
C
C     (It doesn't matter if the structure has flagged values in the data
C     that aren't reflected in the quality array - these will be left
C     in.)
C
      IF ((QUAL_EXIST(REF_SLOT).GT.0).AND.
     :                 (.NOT.USE_QUALITY(REF_SLOT))) THEN
C
C        We have to flag the data.  First we have to hunt around
C        the common tables to actually find the mapped array.
C
         WORK_SLOT=MAP_CALL_WORK(DATA_SLOT(REF_SLOT))
         IF (WORK_SLOT.EQ.0) THEN
C
C           Data was mapped directly to its base array.
C
            MAP_SLOT=MAP_CALL_SLOT(DATA_SLOT(REF_SLOT))
            DATA_ADDR=MAP_POINTER(MAP_SLOT)
            DATA_TYPE=MAP_TYPE(MAP_SLOT)
         ELSE
C
C           Data is in a work array
C
            DATA_ADDR=WORK_POINTER(WORK_SLOT)
            DATA_TYPE=WORK_TYPE(WORK_SLOT)
         END IF
C
C        Get the number of elements in the data
C
         NELM=1
         DO I=1,DATA_NDIM(REF_SLOT)
            NELM=NELM*DATA_DIMS(I,REF_SLOT)
         END DO
C
C        We have to map the quality array.  Note the possibility that
C        the application will change the flags in the data array - this
C        means that unless the data array was mapped readonly, we map
C        the quality for update.
C
         IF (MAP_CALL_MODE(DATA_SLOT(REF_SLOT)).EQ.'R') THEN
            MODE='READ'
         ELSE
            MODE='UPDATE'
         END IF
         CALL DSA_ACT_MAP_QUALITY(REF_NAME,MODE,'BYTE',QUAL_ADDR,
     :                                 QUALITY_SLOT(REF_SLOT),STATUS)
C
C        Now process the data array
C
         CALL DSA_REFLAG_DATA(NELM,DATA_TYPE,%VAL(CNF_PVAL(DATA_ADDR)),
     :                        %VAL(CNF_PVAL(QUAL_ADDR)),STATUS)
C
      END IF
C
C     Set the pre-processing done flag
C
      PRE_FLAG(REF_SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
