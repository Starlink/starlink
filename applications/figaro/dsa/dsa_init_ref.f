C+
C                   D S A _ I N I T _ R E F _ S L O T
C
C  Routine name:
C     DSA_INIT_REF_SLOT
C
C  Function:
C     Sets the initial values in the common tables for a new file.
C
C  Description:
C     This is an internal routine used by routines that need to
C     allocate a new reference slot for a file and initialise the
C     values in it.  It sets the various flags to their initialised
C     state.  Having all this in one place makes it easier to add
C     new entries to the reference tables.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_INIT_REF_SLOT (REF_SLOT, STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The slot number of the common
C                        table entries for the file.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:
C     This is intended to be called from routines such as DSA_NAMED_OUTPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th February 1995
C-
C  External variable details:
C     (>) MAX_AXES    (Integer parameter) Maximum number of axes in data
C     (<) REF_USED    (Logical array) Indicates reference slot in use.
C     (<) PARM_VALUE  (String array) Parameter value associated with ref name.
C     (<) AXIS_EXIST  (Integer array) Indicates knowledge of axis arrays.
C     (<) ERROR_EXIST (Integer array) Indicates knowledge of error array.
C     (<) QUAL_EXIST  (Integer array) Indicates knowledge of quality array.
C     (<) DATA_FLAGGED(Integer array) Indicates knowledge of data flagging.
C     (<) DATA_SLOT   (Integer array) Map call slot for data mapping.
C     (<) QUALITY_SLOT (Integer array) Map call slot for quality mapping.
C     (<) PRE_QUAL    (Logical array) Indicates quality pre-processing done.
C     (<) PRE_FLAG    (Logical array) Indicates flag values pre-processed.
C     (<) USE_FLAGS   (Logical array) Indicates application will use
C                     flagged data values for this structure.
C     (<) USE_QUALITY (Logical array) Indicates application will use
C                     quality information for this structure.
C     (<) QF_HANDLING (Integer array) Flags that record the use the program
C                     is making of the quality and flag information.
C     (<) FITS_OPEN   (Logical array) Indicates FITS processing begun for
C                     this structure.
C     (<) ERROR_UPDATE (Logical array) Indicates that the error array has
C                     been updated (or at least, mapped for update).
C     (<) DATA_UPDATE (Logical array) Indicates that the data array has
C                     been updated (or at least, mapped for update).
C     (<) RANGE_UPDATE (Logical array) Indicates that the data range values
C                     have been updated.
C     (<) QUAL_UPDATE (Logical array) Indicates that the quality array
C                     has been updated (or at least, mapped for update).
C     (<) AXIS_UPDATE (Logical array) Indicates that an axis data array
C                     has been updated (or at least, mapped for update).
C     (<) WIDTH_UPDATE (Logical array) Indicates that an axis width array
C                     has been updated (or at least, mapped for update).
C     (<) DATA_RESHAPE (Logical array) Indicates that the main data array
C                     was reshaped.
C     (<) AXIS_RESHAPE (Logical array) Indicates that the axis data structures
C                     were reshaped.
C
C  History:
C     28th Nov 1988    Original version.  KS / AAO.
C     11th Dec 1989    New UPDATE flags added.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     17th Feb 1995    Added QF_HANDLING. KS/AAO.
C+
      SUBROUTINE DSA_INIT_REF_SLOT (REF_SLOT, STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER I                        ! Loop index
C
C     Check for bad passed status
C
      IF (STATUS.NE.0) RETURN
C
C     Fill up the slot entries
C
      DATA_NDIM(REF_SLOT)=-1
      DO I=1,MAX_AXES
         AXIS_EXIST(I,REF_SLOT)=0
         AXIS_UPDATE(I,REF_SLOT)=.FALSE.
         WIDTH_UPDATE(I,REF_SLOT)=.FALSE.
         AXIS_RESHAPE(I,REF_SLOT)=.FALSE.
      END DO
      ERROR_EXIST(REF_SLOT)=0
      QUALITY_SLOT(REF_SLOT)=0
      DATA_SLOT(REF_SLOT)=0
      DATA_FLAGGED(REF_SLOT)=0
      DATA_RESHAPE(REF_SLOT)=.FALSE.
      QUAL_EXIST(REF_SLOT)=0
      QF_HANDLING(REF_SLOT)=0
      PRE_FLAG(REF_SLOT)=.FALSE.
      PRE_QUAL(REF_SLOT)=.FALSE.
      USE_FLAGS(REF_SLOT)=.FALSE.
      USE_QUALITY(REF_SLOT)=.FALSE.
      ERROR_UPDATE(REF_SLOT)=.FALSE.
      DATA_UPDATE(REF_SLOT)=.FALSE.
      RANGE_UPDATE(REF_SLOT)=.FALSE.
      QUAL_UPDATE(REF_SLOT)=.FALSE.
      FITS_OPEN(REF_SLOT)=.FALSE.
      PARM_VALUE(REF_SLOT)=' '
      REF_USED(REF_SLOT)=.TRUE.
C
      END
