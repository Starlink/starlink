C+
C                     D S A _ G E T _ R A N G E
C
C  Routine name:
C     DSA_GET_RANGE
C
C  Function:
C     Determines the maximum and minimum values for data.
C
C  Description:
C     Determines the maximum and minimum values for the main data array
C     in a structure.  These may be obtained quickly from a range
C     structure associated with the data, if such a structure exists,
C     or may have to obtained by a pass through the actual data values.
C     Clearly, the latter will usually be much less efficient.  A call
C     to DSA_SEEK_RANGE will reveal whether or not a valid range
C     structure does exist.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_RANGE (REF_NAME,VMIN,VMAX,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (<) VMIN         (Real,ref) The minimum data value in the array.
C     (<) VMAX         (Real,ref) The maximum data value in the array.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     CNF_PVAL, DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA_UNMAP,
C     DSA_SET_RANGE, DSA_ARRAY_SIZE, DSA_MAP_ARRAY, DSA_SEEK_QUALITY,
C     DSA_SEEK_FLAGGED_VALUES, DSA_GET_FLAG_VALUE, DSA__RANGE_STRUCT_NAME,
C     DSA__DATA_NAME, DSA__QUAL_NAME, DSA__ARRAY, DTA_WRVARI,
C     DTA_STRUC, DTA_RDVARI, DTA_RDVARF, ICH_FOLD, ICH_LEN, GEN_RANGEF,
C     GEN_FRANGEF, GEN_QRANGEF
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 26th October 1994
C-
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Output message to user.
C     DSA_GET_ACTUAL_NAME  Get full structure name from reference name.
C     DSA_UNMAP        Cancel an mapping call.
C     DSA_SET_RANGE    Set the range structure for a data array.
C     DSA_ARRAY_SIZE   Get dimensions of a named array.
C     DSA_MAP_ARRAY    Map a named array.
C     DSA_SEEK_QUALITY See if a structure has a quality array.
C     DSA_SEEK_FLAGGED_VALUES See if a data array contains flagged values.
C     DSA_GET_FLAG_VALUE  Get the flag data value for a given type.
C     DSA__RANGE_STRUCT_NAME Get DTA name of the range sub-structure.
C     DSA__DATA_NAME   Get DTA name of the main data array.
C     DSA__ARRAY       See if a name object is a primitive or structured array.
C     DTA_RDVARI       Read an integer from a data object.
C     DTA_WRVARI       Write an integer into a data object.
C     DTA_STRUC        See if an object is a structure.
C     DTA_RDVARF       Write a real number into a data object.
C     ICH_LEN          Position of last non-blank char in string.
C     ICH_FOLD         Convert string to upper case.
C     GEN_FRANGEF      Find max and min values in a flagged real array.
C     GEN_QRANGEF      Find max and min values in a real array with quality.
C     GEN_RANGEF       Find max and min values in a real array.
C
C  History:
C     30th July 1987  Original version.  KS / AAO.
C     3rd  Feb  1989  Now allows for data quality information.  KS/AAO.
C     8th  Sept 1989  Now controls flagged value propagation.  KS/AAO.
C     1st  Mar  1990  Now uses DSA__ routines rather than assuming the
C                     original Figaro data format.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Remove unused variable declarations. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     26th Oct 1994   Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C     2005 June 3     Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                     contruct for 64-bit addressing.  MJC / Starlink
C
C  Note:
C     This routine assumes that, no matter what the file format used,
C     the data range values are held in a private structure (whose name
C     will vary from format to format), containing .VALID, .MAX, and .MIN
C+
      SUBROUTINE DSA_GET_RANGE (REF_NAME,VMIN,VMAX,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER STATUS
      REAL VMIN, VMAX
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      LOGICAL DSA__ARRAY
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ common.  Defines MAX_AXES
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DADDRS           ! Address of data array
      INTEGER   DIMS(MAX_AXES)   ! Dimensions of data array
      INTEGER   DTA_STATUS       ! DTA_ routine status codes
      CHARACTER ERROR*64         ! DTA_ error string
      REAL      FBAD             ! Floating point flag value
      LOGICAL   FLAGS            ! Indicates data is flagged
      INTEGER   I                ! Loop index
      INTEGER   IGNORE           ! Status code - ignored
      INTEGER   INVOKE           ! Dummy function value
      LOGICAL   KNOWN            ! If range found in structure
      INTEGER   LENGTH           ! Length of structure name
      CHARACTER NAME*80          ! DTA_ name for data arrays
      INTEGER   NDIM             ! Number of array dimensions
      INTEGER   NELM             ! Number of elements in array
      CHARACTER OBJ_NAME*80      ! DTA_ name for range structure
      INTEGER   QADDRS           ! Address of quality array
      LOGICAL   QUALITY          ! Indicates quality data exists
      INTEGER   QUAL_SLOT        ! Slot used for quality mapping
      CHARACTER RANGE_NAME*64    ! Name of range sub-structure
      CHARACTER REF_NAME_UC*32   ! Upper case version of REF_NAME
      INTEGER   REF_SLOT         ! Reference table slot number
      INTEGER   SLOT             ! Slot used for map call
      CHARACTER STRUCTURE*128    ! Full name of structure
      INTEGER   VALID            ! Value of validity flag
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables and get the name of
C     the range sub-structure.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
      CALL DSA__RANGE_STRUCT_NAME (REF_SLOT,RANGE_NAME,LENGTH)
C
C     Generate the name of the validity flag and see if we can
C     read it.
C
      KNOWN=.FALSE.
      NAME=RANGE_NAME(:LENGTH)//'.VALID'
      CALL DTA_RDVARI(NAME,1,VALID,DTA_STATUS)
      IF ((DTA_STATUS.EQ.0).AND.(VALID.NE.0)) THEN
         NAME=RANGE_NAME(:LENGTH)//'.MIN'
         CALL DTA_RDVARF(NAME,1,VMIN,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            NAME=RANGE_NAME(:LENGTH)//'.MAX'
            CALL DTA_RDVARF(NAME,1,VMAX,DTA_STATUS)
         END IF
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER ('Note: The structure ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(' had an inconsistent range structure. ')
            CALL DSA_WRUSER('Some previous program must be at fault.')
            CALL DSA_WRFLUSH
            NAME=OBJ_NAME(:LENGTH)//'.VALID'
            CALL DTA_WRVARI(NAME,1,0,DTA_STATUS)
         ELSE
            KNOWN=.TRUE.
         END IF
      END IF
C
C     If we could not determine the range from a range structure,
C     map the data and work it out the hard way.
C
      IF (.NOT.KNOWN) THEN
         IF (DSA__ARRAY(OBJ_NAME)) THEN
            NAME=OBJ_NAME
         ELSE
            CALL DSA__DATA_NAME (REF_SLOT,NAME,LENGTH)
         END IF
         CALL DSA_ARRAY_SIZE (NAME,MAX_AXES,NDIM,DIMS,ERROR,STATUS)
         IF (STATUS.NE.0) THEN
            CALL DSA_WRUSER ('Unable to get dimensions for ')
            CALL DSA_WRNAME (NAME)
            CALL DSA_WRUSER ('. ')
            CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER ('.')
            CALL DSA_WRFLUSH
            GO TO 500         ! Error exit
         END IF
         NELM=1
         DO I=1,NDIM
            NELM=NELM*DIMS(I)
         END DO
         CALL DSA_SEEK_QUALITY (REF_NAME,QUALITY,STATUS)
         CALL DSA_SEEK_FLAGGED_VALUES (REF_NAME,FLAGS,STATUS)
         CALL DSA_MAP_ARRAY (NAME,'READ','FLOAT',1,NELM,NELM,
     :                       .FALSE.,.FALSE.,FLAGS,DADDRS,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500         ! Error exit
C
C        Work out the range using whatever sort of quality data
C        we have - a quality array, flagged data, or none.  We don't
C        want to have to make use of quality or flag pre-processing
C        here, so we bypass the normal DSA_MAP_QUALITY (it's faster
C        as well).
C
         IF (QUALITY) THEN
            CALL DSA__QUAL_NAME (REF_SLOT,NAME,LENGTH)
            CALL DSA_MAP_ARRAY (NAME,'READ','BYTE',1,NELM,NELM,
     :              .FALSE.,.FALSE.,.FALSE.,QADDRS,QUAL_SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500     ! Error exit
            CALL GEN_QRANGEF (%VAL(CNF_PVAL(DADDRS)),
     :                        %VAL(CNF_PVAL(QADDRS)),1,NELM,VMAX,VMIN)
            CALL DSA_UNMAP (QUAL_SLOT,STATUS)
         ELSE IF (FLAGS) THEN
            CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
            CALL GEN_FRANGEF (%VAL(CNF_PVAL(DADDRS)),FBAD,1,NELM,VMAX,
     :                        VMIN)
         ELSE
            CALL GEN_RANGEF (%VAL(CNF_PVAL(DADDRS)),1,NELM,VMAX,VMIN)
         END IF
         CALL DSA_UNMAP(SLOT,STATUS)
         CALL DSA_SET_RANGE (REF_NAME,VMIN,VMAX,STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
