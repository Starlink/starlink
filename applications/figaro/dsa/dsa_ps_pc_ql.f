C+
C              D S A _ P O S T _ P R O C E S S _ Q U A L I T Y
C
C  Routine name:
C     DSA_POST_PROCESS_QUALITY
C
C  Function:
C     Performs any necessary post-processing for quality arrays.
C
C  Description:
C     This routine must be called by any routine that has used data
C     quality arrays to process data.  It reverses any operations
C     on data and quality arrays that may have been performed by
C     DSA_PRE_PROCESS_QUALITY in order to handle flagged data values.
C     It should be called once all processing on the data and the
C     quality arrays is complete.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_POST_PROCESS_QUALITY (REF_NAME,STATUS)
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
C     DSA_WRUSER, DSA_GET_WORKSPACE, DSA_FREE_WORKSPACE, DSA_WRFLUSH,
C     DSA_REFLAG_DATA, VEC_xTOx
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure must have been opened using DSA_INPUT, DSA_OUTPUT or
C     an equivalent routine.  Data and quality arrays must both have
C     been mapped.  DSA_PRE_PROCESS_QUALITY must have been called.
C
C  Version date: 6th february 1995.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     ICH_FOLD         Convert string to upper case
C     ICH_LEN          Position of last non-blank char in string
C     DSA_FIND_REF     Look up reference name in common tables
C     DSA_GET_ACTUAL_NAME  Get full name corresponding to reference name
C     DSA_WRUSER       Output message to user
C     DSA_WRFLUSH      Flush output messages to user
C     DSA_GET_WORKSPACE  Get specified amount of workspace
C     DSA_FREE_WORKSPACE  Release workspace obtained by DSA_GET_WORKSPACE
C     DSA_REFLAG_DATA  Reinstate flag values into data array
C     VEC_xTOx         Type conversion
C
C  Common variable details:
C     (>) MAP_POINTER   (Integer array) Memory address for the array.
C     (>) MAP_TYPE      (String array) Type of actual data object.
C     (>) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (>) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C     (>) DATA_NDIM     (Integer array) Number of dimensions of main data array.
C     (>) DATA_DIMS     (Integer array) Dimensions of main data array. (2D)
C     (>) DATA_FLAGGED  (Integer array) State of knowledge about data flagging.
C     (>) DATA_SLOT     (Integer array) Map call slot for data mapping.
C     (>) QUALITY_SLOT  (Integer array) Map call slot for quality mapping.
C     (>) WORK_POINTER  (Integer array) Memory address of workspace array.
C     (>) WORK_TYPE     (String array)  Type of array held in workspace.
C     (>) PRE_QUAL      (Logical array) Indicates quality preprocessing done.
C
C  History:
C     21st July 1988 Original version.  KS / AAO.
C     25th Aug  1992 Replace CNV_ calls with VEC_ calls. If the type
C                    used internally is not recognised, STATUS is set
C                    to DSA__INVTYP.  HME / UoE, Starlink.
C     31st Aug  1992 Introduced use of DSA_WRFLUSH. KS/AAO.
C      1st Sep  1992 Usused variable declarations removed. KS/AAO.
C      6th Feb  1995 Now allows for files with both flagged values ans
C                    data quality arrays and for both to be handled
C                    explicitly by the application. KS/AAO.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_POST_PROCESS_QUALITY (REF_NAME,STATUS)
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
      INTEGER   DATA_ADDR      ! Virtual address of data array
      CHARACTER DATA_TYPE*16   ! Type of mapped data array
      INTEGER   I              ! Index through dimensions
      INTEGER   IGNORE         ! Dummy status value
      INTEGER   INVOKE         ! Dummy function value
      INTEGER   LENGTH         ! Object name length
      INTEGER   MAP_SLOT       ! Mapping slot for data/quality
      INTEGER   NBAD           ! Bad conversions - ignored
      INTEGER   NELM           ! Number of data array elements
      CHARACTER OBJ_NAME*128   ! DTA_ name of data object
      INTEGER   QUAL_ADDR      ! Virtual address of quality array
      CHARACTER QUAL_TYPE*16   ! Type of mapped quality array
      CHARACTER REF_NAME_UC*32 ! Upper case version of REF_NAME
      INTEGER   REF_SLOT       ! Reference table slot #
      CHARACTER STRUCTURE*128  ! Full structure name from ref_name
      INTEGER   TEMP_ADDR      ! Virtual address of work array
      INTEGER   TEMP_SLOT      ! Workspace slot for work array
      INTEGER   WORK_SLOT      ! Workspace slot for data/quality
      INTEGER   IERR, VECSTA   ! Unused for VEC_ routines
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
C     Check that DSA_PRE_PROCESS_QUALITY has indeed been called.
C
      IF (.NOT.PRE_QUAL(REF_SLOT)) THEN
         CALL DSA_WRUSER(
     :         'DSA_POST_PROCESS quality has been called to process ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. It has been called out of sequence, since ')
         CALL DSA_WRUSER(' DSA_PRE_PROCESS_QUALITY has not been '//
     :                                     'called for this structure.')
         CALL DSA_WRUSER(' Programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__SEQERR
         GO TO 500           ! Error exit
      END IF
C
C     Now, see if we need to do anything. There is one condition under
C     which we have to reflag the data in the main array according to
C     the - now potentially updated - quality array. This is the case
C     where the data array may be flagged, where there is no quality
C     array associated with the data file, and where the application
C     program was not handling the flagged data itself. Note that by this
C     stage all the common table items such as DATA_FLAGGED will have
C     been set.
C
      IF ((DATA_FLAGGED(REF_SLOT).GT.0).AND.
     :            (QUAL_EXIST(REF_SLOT).LE.0).AND.
     :                    (.NOT.USE_FLAGS(REF_SLOT))) THEN
C
C        Yes, so we have to reflag it.  The main complication here is
C        just that we have to hunt around the common tables to actually
C        find the mapped arrays.
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
C        Now do the same for the quality array
C
         WORK_SLOT=MAP_CALL_WORK(QUALITY_SLOT(REF_SLOT))
         IF (WORK_SLOT.EQ.0) THEN
            MAP_SLOT=MAP_CALL_SLOT(QUALITY_SLOT(REF_SLOT))
            QUAL_ADDR=MAP_POINTER(MAP_SLOT)
            QUAL_TYPE=MAP_TYPE(MAP_SLOT)
         ELSE
            QUAL_ADDR=WORK_POINTER(WORK_SLOT)
            QUAL_TYPE=WORK_TYPE(WORK_SLOT)
         END IF
C
C        Get the number of elements in the data
C
         NELM=1
         DO I=1,DATA_NDIM(REF_SLOT)
            NELM=NELM*DATA_DIMS(I,REF_SLOT)
         END DO
C
C        It is possible that the quality array wasn't mapped as a byte
C        array (how perverse can some people be?).  If that's the case
C        - and we assume that would be unusual - we need a byte array
C        of the same size as temporary workspace.
C
         IF (QUAL_TYPE.NE.'BYTE') THEN
            CALL DSA_GET_WORKSPACE(NELM,TEMP_ADDR,TEMP_SLOT,STATUS)
            VECSTA = 0
            IF (QUAL_TYPE.EQ.'DOUBLE') THEN
               CALL VEC_DTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :                       %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'SHORT'.OR.QUAL_TYPE.EQ.'WORD') THEN
               CALL VEC_WTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :                       %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'INT') THEN
               CALL VEC_ITOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :                       %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'FLOAT'.OR.QUAL_TYPE.EQ.'REAL') THEN
               CALL VEC_RTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :                       %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'USHORT'.OR.QUAL_TYPE.EQ.'UWORD') THEN
               CALL VEC_UWTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :                       %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE
               CALL DSA_WRUSER('Unable to convert quality from type "')
               CALL DSA_WRUSER(QUAL_TYPE(:ICH_LEN(QUAL_TYPE)))
               CALL DSA_WRUSER('".  Invalid type.')
               CALL DSA_WRFLUSH
               STATUS=DSA__INVTYP
            END IF
            QUAL_ADDR = TEMP_ADDR
         END IF
C
C        Now reset the flags in the data array.
C
         CALL DSA_REFLAG_DATA(NELM,DATA_TYPE,%VAL(CNF_PVAL(DATA_ADDR)),
     :                        %VAL(CNF_PVAL(QUAL_ADDR)),STATUS)
C
C        If we needed a temporary array, get rid of it.
C
         IF (QUAL_TYPE.NE.'BYTE') THEN
            CALL DSA_FREE_WORKSPACE(TEMP_SLOT,STATUS)
         END IF
      END IF
C
C     Clear the pre-processing flag
C
      PRE_QUAL(REF_SLOT)=.FALSE.
C
C     Exit
C
  500 CONTINUE
C
      END
