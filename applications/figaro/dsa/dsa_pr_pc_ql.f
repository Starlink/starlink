C+
C                  D S A _ P R E _ P R O C E S S _ Q U A L I T Y
C
C  Routine name:
C     DSA_PRE_PROCESS_QUALITY
C
C  Function:
C     Performs any necessary pre-processing for quality arrays.
C
C  Description:
C     This routine must be called by any routine that is using data
C     quality arrays to process data.  It should be called after the
C     data array and the quality array for the structure in question
C     have been mapped, but before any processing is done on either.
C     It handles any processing that may be required to deal with
C     flagged data values in the main data array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_PRE_PROCESS_QUALITY (REF_NAME,STATUS)
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
C     DSA_WRUSER, DSA_GET_WORKSPACE, DSA_FREE_WORKSPACE,
C     DSA_UNFLAG_DATA, VEC_xTOx
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure must have been opened using DSA_INPUT, DSA_OUTPUT or
C     an equivalent routine.  Data and quality arrays must both have
C     been mapped.
C
C  Version date: 1st December 1995.
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
C     DSA_WRFLUSH      Flush message buffer to user
C     DSA_GET_WORKSPACE   Get specified amount of workspace
C     DSA_FREE_WORKSPACE  Release workspace obtained by DSA_GET_WORKSPACE
C     DSA_UNFLAG_DATA  Remove flag values from data array
C     VEC_xTOx         Type conversion
C
C  Common variable details:
C     (>) MAP_POINTER   (Integer array) Memory address for the array.
C     (>) MAP_TYPE      (String array) Type of actual data object.
C     (>) MAP_MODE      (Character array) Access type for the data.
C     (>) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (>) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C     (>) DATA_NDIM     (Integer array) Number of dimensions of main data array.
C     (>) DATA_DIMS     (Integer array) Dimensions of main data array. (2D)
C     (>) DATA_FLAGGED  (Integer array) State of knowledge about data flagging.
C     (>) DATA_SLOT     (Integer array) Map call slot for data mapping.
C     (>) QUALITY_SLOT  (Integer array) Map call slot for quality mapping.
C     (>) WORK_POINTER  (Integer array) Memory address of workspace array.
C     (>) WORK_TYPE     (String array)  Type of array held in workspace.
C     (<) PRE_QUAL      (Logical array) Flags quality pre-processing done.
C
C  History:
C     21st July 1988 Original version.  KS / AAO.
C     3rd  May  1990 Flagged value count added to DSA_UNFLAG_DATA call.
C                    KS / AAO.
C     25th Aug  1992 Replace CNV_ calls with VEC_ calls. If the type
C                    used internally is not recognised, STATUS is set
C                    to DSA__INVTYP.  HME / UoE, Starlink.
C     31st Aug  1992 Introduced use of calls to DSA_WRFLUSH. KS/AAO.
C      1st Sep  1992 Usused variable declarations removed. KS/AAO.
C      3rd Feb  1995 Now supports files that have both flagged data values
C                    and quality arrays, and the simultaneous use of both
C                    by an application. KS/AAO.
C      1st Dec  1995 No longer attempts to unflag a write-only array. KS/AAO.
C     11th Dec  1995 Use the proper check whether write-only. Before (1 Dec)
C                    the question was whether
C                    MAP_MODE(MAP_CALL_SLOT(DATA_SLOT(REF_SLOT))).NE.'W'.
C                    However, that's the access actually used, not the
C                    one requested by the application. DSA never uses 'W',
C                    but 'U' if possible and 'R' otherwise.
C                    Therefore the test must be for the requested access
C                    MAP_CALL_MODE(DATA_SLOT(REF_SLOT)).NE.'W'.
C                    HME / UoE, Starlink.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_PRE_PROCESS_QUALITY (REF_NAME,STATUS)
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
      INTEGER   NFLAGGED       ! Number of flagged values - ignored
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
C     Check that both data and quality have been mapped.
C
      IF ((DATA_SLOT(REF_SLOT).EQ.0).OR.(QUALITY_SLOT(REF_SLOT).EQ.0))
     :                                                             THEN
         IF (DATA_SLOT(REF_SLOT).NE.0) THEN
            CALL DSA_WRUSER('The data quality array in ')
         ELSE IF (QUALITY_SLOT(REF_SLOT).NE.0) THEN
            CALL DSA_WRUSER('The main data array in ')
         ELSE
            CALL DSA_WRUSER('Neither the main data array nor the '//
     :                                        'data quality array in ')
         END IF
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         IF ((DATA_SLOT(REF_SLOT).EQ.0).AND.
     :                       (QUALITY_SLOT(REF_SLOT).EQ.0)) THEN
            CALL DSA_WRUSER(' have been mapped.')
         ELSE
            CALL DSA_WRUSER(' has not been mapped.')
         END IF
         CALL DSA_WRUSER(
     :      ' DSA_PRE_PROCESS_QUALITY has been called out of sequence.')
         CALL DSA_WRUSER(' Programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__SEQERR
         GO TO 500           ! Error exit
      END IF
C
C     Now, see if we need to do anything.  The only condition we have
C     to look for is the case where the data is flagged and the calling
C     program is not handling flagged data directly, in which case
C     we have to process the flags out and into the quality array. Note
C     that by this stage all the common table items such as DATA_FLAGGED
C     will have been set. If the data is being mapped writeonly, then we
C     don't need to do anything.
C
C     (A moot point is the question of whether we really need to process
C     the flagged values into the quality array in the case where both
C     are present in the data. We ought to be able to assume that the
C     contents of the quality array reflect the flagged values in the data
C     - even though the reverse is not true, since doing that would loose
C     the original data values, and that's silly, even if they are
C     questionable. However, we don't, because someone might have produced
C     this effect with a program that handled both flagged values and
C     the quality array directly in the application - or by messing with LET!
C     Also, assuming the two forms of quality information can differ seems
C     to be in line with the NDF design philosophy.)
C
C     MAP_SLOT=MAP_CALL_SLOT(DATA_SLOT(REF_SLOT))
C     IF ((DATA_FLAGGED(REF_SLOT).GT.0).AND.(MAP_MODE(MAP_SLOT).NE.'W')
C    :                        .AND.(.NOT.USE_FLAGS(REF_SLOT))) THEN
      IF ((DATA_FLAGGED(REF_SLOT).GT.0).AND.
     :    (MAP_CALL_MODE(DATA_SLOT(REF_SLOT)).NE.'W').AND.
     :    (.NOT.USE_FLAGS(REF_SLOT))) THEN
C
C        Yes, so we have to unflag it.  The main complication here is
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
C        of the same size as temporary workspace, and we need to fill
C        it with the existing quality values. (This is a messy load of
C        code that appears in reverse almost immediately and will probably
C        never be used..)
C
         IF (QUAL_TYPE.NE.'BYTE') THEN
            CALL DSA_GET_WORKSPACE(NELM,TEMP_ADDR,TEMP_SLOT,STATUS)
            IF (QUAL_TYPE.EQ.'DOUBLE') THEN
               CALL VEC_DTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :            %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'SHORT'.OR.QUAL_TYPE.EQ.'WORD') THEN
               CALL VEC_WTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :            %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'INT') THEN
               CALL VEC_ITOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :            %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'FLOAT'.OR.QUAL_TYPE.EQ.'REAL') THEN
               CALL VEC_RTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :            %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'USHORT'.OR.QUAL_TYPE.EQ.'UWORD') THEN
               CALL VEC_UWTOB(.FALSE.,NELM,%VAL(CNF_PVAL(QUAL_ADDR)),
     :            %VAL(CNF_PVAL(TEMP_ADDR)),IERR,NBAD,VECSTA)
            ELSE
               CALL DSA_WRUSER('Unable to mapped quality array type "')
               CALL DSA_WRUSER(QUAL_TYPE(:ICH_LEN(QUAL_TYPE)))
               CALL DSA_WRUSER('".  Invalid type.')
               CALL DSA_WRFLUSH
               STATUS=DSA__INVTYP
            END IF
C
C        Now process the data array using the temporary _BYTE quality
C        or the original quality.
C
            CALL DSA_UNFLAG_DATA(NELM,.FALSE.,DATA_TYPE,
     :                           %VAL(CNF_PVAL(DATA_ADDR)),
     :                           %VAL(CNF_PVAL(TEMP_ADDR)),NFLAGGED,
     :                           STATUS)
         ELSE
            CALL DSA_UNFLAG_DATA(NELM,.FALSE.,DATA_TYPE,
     :                           %VAL(CNF_PVAL(DATA_ADDR)),
     :                           %VAL(CNF_PVAL(QUAL_ADDR)),NFLAGGED,
     :                           STATUS)
         END IF
CC
C        And convert into the correct array, should that be necessary.
C
         IF (QUAL_TYPE.NE.'BYTE') THEN
            IF (QUAL_TYPE.EQ.'DOUBLE') THEN
               CALL VEC_BTOD(.FALSE.,NELM,%VAL(CNF_PVAL(TEMP_ADDR)),
     :                       %VAL(CNF_PVAL(QUAL_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'SHORT'.OR.QUAL_TYPE.EQ.'WORD') THEN
               CALL VEC_BTOW(.FALSE.,NELM,%VAL(CNF_PVAL(TEMP_ADDR)),
     :                       %VAL(CNF_PVAL(QUAL_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'INT') THEN
               CALL VEC_BTOI(.FALSE.,NELM,%VAL(CNF_PVAL(TEMP_ADDR)),
     :                       %VAL(CNF_PVAL(QUAL_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'FLOAT'.OR.QUAL_TYPE.EQ.'REAL') THEN
               CALL VEC_BTOR(.FALSE.,NELM,%VAL(CNF_PVAL(TEMP_ADDR)),
     :                       %VAL(CNF_PVAL(QUAL_ADDR)),IERR,NBAD,VECSTA)
            ELSE IF (QUAL_TYPE.EQ.'USHORT'.OR.QUAL_TYPE.EQ.'UWORD') THEN
               CALL VEC_BTOUW(.FALSE.,NELM,%VAL(CNF_PVAL(TEMP_ADDR)),
     :                       %VAL(CNF_PVAL(QUAL_ADDR)),IERR,NBAD,VECSTA)
            ELSE
               STATUS=DSA__INVTYP
            END IF
            CALL DSA_FREE_WORKSPACE(TEMP_SLOT,STATUS)
         END IF
      END IF
C
C     Set the pre-processing done flag
C
      PRE_QUAL(REF_SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
