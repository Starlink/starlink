C+
C       D S A _ P O S T _ P R O C E S S _ F L A G G E D _ V A L U E S
C
C  Routine name:
C     DSA_POST_PROCESS_FLAGGED_VALUES
C
C  Function:
C     Performs any necessary post-processing for flagged data values.
C
C  Description:
C     This routine must be called by any routine that has used flagged
C     data values to process data.  It reverses any operations
C     on data arrays that DSA_PRE_PROCESS_FLAGGED_VALUES may have
C     performed by in order to handle data structures where the quality
C     information is in fact held in data quality arrays.  It should be
C     called once all processing on the data array is complete.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_POST_PROCESS_FLAGGED_VALUES (REF_NAME,STATUS)
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
C     DSA_WRUSER, DSA_UNFLAG_DATA
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure must have been opened using DSA_INPUT, DSA_OUTPUT or
C     an equivalent routine.  The data array must have been mapped.
C     DSA_PRE_PROCESS_FLAGGED_VALUES must have been called.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 6th February 1995
C-
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     ICH_FOLD         Convert string to upper case
C     ICH_LEN          Position of last non-blank char in string
C     DSA_FIND_REF     Look up reference name in common tables
C     DSA_GET_ACTUAL_NAME  Get full name corresponding to reference name
C     DSA_WRUSER       Output message to user
C     DSA_GET_WORKSPACE  Get specified amount of workspace
C     DSA_FREE_WORKSPACE  Release workspace obtained by DSA_GET_WORKSPACE
C     DSA_UNFLAG_DATA  Remove flag values from data array
C
C  Common variable details:
C     (>) MAP_POINTER   (Integer array) Memory address for the array.
C     (>) MAP_TYPE      (String array)  Type of actual data object.
C     (>) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (>) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C     (>) DATA_NDIM     (Integer array) Number of dimensions of main data array.
C     (>) DATA_DIMS     (Integer array) Dimensions of main data array. (2D)
C     (>) DATA_FLAGGED  (Integer array) State of knwolege about data flagging.
C     (>) DATA_SLOT     (Integer array) Map call slot for data mapping.
C     (>) QUALITY_SLOT  (Integer array) Map call slot for quality mapping.
C     (>) WORK_POINTER  (Integer array) Memory address of workspace array.
C     (>) WORK_TYPE     (String array)  Type of array held in workspace.
C     (>) PRE_FLAG      (Logical array) Indicates quality preprocessing done.
C
C  History:
C     22nd July 1988 Original version.  KS / AAO.
C     3rd  May  1990 Flagged value count added to DSA_UNFLAG_DATA call. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C      6th Feb 1995  Modified to allow files with both flagged values and
C                    quality information, and for both to be handled at
C                    once. Call to DSA_UNFLAG_DATA changed. KS/AAO.
C     25th Jul 1996  Corrected type of LEAVE_FLAGGED.  MJCL/Starlink, UCL.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_POST_PROCESS_FLAGGED_VALUES (REF_NAME,STATUS)
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
      LOGICAL   LEAVE_FLAGGED               ! True if data to be left flagged
      INTEGER   LENGTH                      ! Object name length
      INTEGER   MAP_SLOT                    ! Mapping slot for data
      INTEGER   NELM                        ! Number of data array elements
      INTEGER   NFLAGGED                    ! Number of flagged data values
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
C     Check that DSA_PRE_PROCESS_FLAGGED_VALUES has indeed been called.
C
      IF (.NOT.PRE_FLAG(REF_SLOT)) THEN
         CALL DSA_WRUSER('DSA_POST_PROCESS_FLAGGED_VALUES has '//
     :                                     'been called to process ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. It has been called out of sequence, since ')
         CALL DSA_WRUSER(' DSA_PRE_PROCESS_FLAGGED_VALUES has not been '
     :                                   //'called for this structure.')
         CALL DSA_WRUSER(' Programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__SEQERR
         GO TO 500           ! Error exit
      END IF
C
C     Now, see if we need to do anything.  The only condition we have
C     to look for is the case where there was a data quality array,
C     which the program was not handling for itself, in which case we have
C     to set the quality array to reflect any possible changes.  Note that
C     by this stage all the common table items such as QUAL_EXIST will have
C     been set.
C
      IF ((QUAL_EXIST(REF_SLOT).GT.0).AND.
     :                 (.NOT.USE_QUALITY(REF_SLOT))) THEN
C
C        We don't need to bother updateing the quality if the data array was
C        mapped readonly, because it will not be updated when it is
C        unmapped.
C
         IF (MAP_CALL_MODE(DATA_SLOT(REF_SLOT)).NE.'R') THEN
C
C           We have to process the flags.  First we have to hunt around the
C           common tables to actually find the mapped arrays.
C
            WORK_SLOT=MAP_CALL_WORK(DATA_SLOT(REF_SLOT))
            IF (WORK_SLOT.EQ.0) THEN
C
C              Data was mapped directly to its base array.
C
               MAP_SLOT=MAP_CALL_SLOT(DATA_SLOT(REF_SLOT))
               DATA_ADDR=MAP_POINTER(MAP_SLOT)
               DATA_TYPE=MAP_TYPE(MAP_SLOT)
            ELSE
C
C              Data is in a work array
C
               DATA_ADDR=WORK_POINTER(WORK_SLOT)
               DATA_TYPE=WORK_TYPE(WORK_SLOT)
            END IF
C
C           We also have to find the quality array, which will have been
C           mapped by DSA_PRE_PROCESS_FLAGGED_VALUES.  We know it will
C           have been mapped as 'BYTE', so we don't need the type.
C
            WORK_SLOT=MAP_CALL_WORK(QUALITY_SLOT(REF_SLOT))
            IF (WORK_SLOT.EQ.0) THEN
               MAP_SLOT=MAP_CALL_SLOT(QUALITY_SLOT(REF_SLOT))
               QUAL_ADDR=MAP_POINTER(MAP_SLOT)
            ELSE
               QUAL_ADDR=WORK_POINTER(WORK_SLOT)
            END IF
C
C           Get the number of elements in the data
C
            NELM=1
            DO I=1,DATA_NDIM(REF_SLOT)
               NELM=NELM*DATA_DIMS(I,REF_SLOT)
            END DO
C
C           We are here because the program has handled the data using flags,
C           so it will probably have left elements of the data array flagged.
C           If the array was originally flagged then we will only have ended
C           up here if there was originally a quality array as well in the
C           file. We could just unflag the data and set the quality array
C           elements, but it seems better to leave them flagged but to
C           make sure the quality array reflects them. So we need to clear the
C           quality array before calling DSA_UNFLAG_DATA. If we are leaving
C           flags in the data, we should make sure we have the array flagged
C           as containing flagged data.
C
            LEAVE_FLAGGED=.TRUE.
            CALL DSA_ZFILL_ARRAY (NELM,QUAL_ADDR,'BYTE',STATUS)
            CALL DSA_UNFLAG_DATA(NELM,LEAVE_FLAGGED,DATA_TYPE,
     :                           %VAL(CNF_PVAL(DATA_ADDR)),
     :                           %VAL(CNF_PVAL(QUAL_ADDR)),
     :                           NFLAGGED,STATUS)
            IF (QUAL_EXIST(REF_SLOT).GT.0) QUAL_UPDATE(REF_SLOT)=.TRUE.
         END IF
C
C        Now unmap the quality array
C
         CALL DSA_UNMAP (QUALITY_SLOT(REF_SLOT),STATUS)
C
      END IF
C
C     If the data is to be left flagged - which will be the case unless
C     the mapping was for update only, then we must make sure the array
C     is flagged as one that contains flagged values.
C
      IF (MAP_CALL_MODE(DATA_SLOT(REF_SLOT)).NE.'R') THEN
         IF (DATA_FLAGGED(REF_SLOT).NE.1) THEN
            CALL DSA__SET_FLAGGED (REF_SLOT,.TRUE.,STATUS)
         END IF
      END IF
C
C     Clear the pre-processing flag
C
      PRE_FLAG(REF_SLOT)=.FALSE.
C
C     Exit
C
  500 CONTINUE
C
      END
