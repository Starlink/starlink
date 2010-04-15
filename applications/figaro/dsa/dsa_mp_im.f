C+
C                   D S A _ M A P _ I M A G I N A R Y
C
C  Routine name:
C     DSA_MAP_IMAGINARY
C
C  Function:
C     Maps the imaginary part of the main data array in a structure.
C
C  Description:
C     This routine maps the imaginary part of a complex main data array
C     in a structure, returning the address of the mapped array.  The whole
C     array is mapped.  If there is in fact no such array, then an array
C     of zeros is generated and its address is returned, unless the mapping
C     is for write or update, in which case an array of zeros is created in
C     the data structure and mapped.  Note that some data formats, in
C     particular Starlink's NDF, have requirements that mean that a new
C     imaginary array cannot be created if the corresponding real array
C     already exists and is mapped.  For this reason, if both real and
C     imaginary arrays are to be mapped, it is worth either mapping the
C     imaginary array first, or ensuring that both arrays are present in
C     the structure before mapping either.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_IMAGINARY (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) MODE         (Fixed string,descr) One of 'READ','WRITE', or
C                      'UPDATE', indicating the way the data is going to
C                      be accessed.  Only the first character is significant.
C     (>) TYPE         (Fixed string,descr) The type of data array to be
C                      mapped onto the structure array.  This can be 'BYTE',
C                      'CHAR','FLOAT','DOUBLE','SHORT','USHORT' or 'INT'.
C                      If type conversion is needed, it will be performed
C                      automatically.
C     (<) ADDRESS      (Integer,ref) The address of the mapped array.
C     (<) SLOT         (Integer,ref) A handle value associated with this
C                      mapping call, which may be used later to unmap
C                      the data explicitly.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_FOLD, DSA_FIND_REF, DSA_SEEK_IMAGINARY,
C     DSA_DATA_SIZE, DSA_MAP_ARRAY, DSA_MAP_DUMMY, DSA_NFILL_ARRAY
C     DSA_GET_ACTUAL_NAME, DSA__CREATE_IMAGINARY, DSA__IMAGINARY_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 26th October 1994
C-
C  Common variable details:
C     (>) MAX_AXES  (Integer parameter) Maximum number of axes in data.
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_DATA_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_MAP_DUMMY Map a dummy data array.
C     DSA_GET_ACTUAL_NAME   Get name of structure from ref name.
C     DSA_ZFILL_ARRAY       Fill a dummy array with zeros.
C     DSA_SEEK_IMAGINARY    See if an imaginary data array exists.
C     DSA__IMAGINARY_NAME   Get name of imaginary array in structure.
C     DSA__CREATE_IMAGINARY Create a new imaginary array in a structure.
C
C  History:
C      3rd Feb 1989  Original version.  KS / AAO.
C     24th Feb 1989  Now creates array as DOUBLE, unless mapped
C                    as FLOAT.
C      8th Sep 1989  Call to DSA_MAP_ARRAY now sets propagate flag
C                    false.
C      2nd May 1990  Modified to use DSA__ routines for structure
C                    details rather than just assuming the original
C                    Figaro structure format.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     26th Oct 1994  Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C     14th Mar 1996  Appended code of DSA_MAP_COMPLEX. HME / UoE, Starlink.
C
C+
      SUBROUTINE DSA_MAP_IMAGINARY (REF_NAME,MODE,TYPE,ADDRESS,
     :                                                     SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER ADDRESS, SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR*1                       ! First character of MODE
      CHARACTER CRTYPE*8                    ! Type used if array created
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of data array
      LOGICAL   EXIST                       ! True if imaginary array exists
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of dummy or imaginary array
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Name of structure
      CHARACTER TYPE_UC*8                   ! Upper case version of TYPE
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of TYPE
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
C
C     Look up the reference name in the tables and get the name
C     of the imaginary array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
      CALL DSA__IMAGINARY_NAME (REF_SLOT,OBJ_NAME,LENGTH)
C
C     First find the dimensions of the main data array.  Those of
C     the imaginary array should match.
C
      CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
C
C     See if there in in fact any imaginary data.
C
      CALL DSA_SEEK_IMAGINARY (REF_NAME,EXIST,STATUS)
C
C     See if the mode requires that the array exist
C
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
C
      IF (EXIST.OR.(CHR.EQ.'W').OR.(CHR.EQ.'U')) THEN
C
C        The imaginary array either is required to exist or does exist.
C        Either way, we want to map an actual data object, not a dummy
C        array.
C
C        If the array did not exist, we need to create one of the same
C        dimensions as the data array.  Just what type to use is unclear -
C        normally, Figaro uses FLOAT arrays for most things, but because
C        complex data is usually there to be fed to NAG, which needs
C        double precision, DOUBLE is probably best in this case.  The
C        compromise used here is to use DOUBLE unless the caller explicitly
C        wanted FLOAT.
C
         IF (.NOT.EXIST) THEN
            CRTYPE='DOUBLE'
            IF (TYPE_UC.EQ.'FLOAT') CRTYPE='FLOAT'
            CALL DSA__CREATE_IMAGINARY (REF_SLOT,CRTYPE,NDIM,DIMS,
     :                                                          STATUS)
            IF (STATUS.NE.0) GO TO 500             ! Error exit
         END IF
C
C        Map the imaginary array.
C
         CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,1,NELM,
     :             NELM,.FALSE.,.FALSE.,.FALSE.,ADDRESS,SLOT,STATUS)
C
C        If we just created the array, fill it with zeros.
C
         IF (.NOT.EXIST) THEN
            CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
         END IF
      ELSE
C
C        There is no imaginary array, so we need to generate a dummy one.
C        and fill it with zeros.
C
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         NAME='imaginary array in '//STRUCTURE
         CALL DSA_MAP_DUMMY (NAME,MODE,TYPE_UC,NELM,ADDRESS,SLOT,STATUS)
C
         CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
C
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END



      SUBROUTINE DSA_MAP_COMPLEX( DSAREF, MODE, TYPE,
     :   RADDR, IADDR, MSLOT, STATUS )

*  Warning: This routine returns only one map slot even though the
*  mapping operation occupied two map slots. If it is necessary to
*  unmap the arrays mapped here (DSA_UNMAP), then this routine
*  should not be used, but DSA_MAP_IMAGINARY and DSA_MAP_DATA should
*  be called directly.

      IMPLICIT NONE

      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

      INTEGER RADDR
      INTEGER IADDR
      INTEGER MSLOT

      INTEGER STATUS

      CALL DSA_MAP_IMAGINARY( DSAREF, MODE, TYPE, IADDR, MSLOT, STATUS )
      CALL DSA_MAP_DATA(      DSAREF, MODE, TYPE, RADDR, MSLOT, STATUS )

      END
