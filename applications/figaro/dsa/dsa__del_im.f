C+
C                   D S A _ _ D E L E T E _ I M A G I N A R Y
C
C  Routine name:
C     DSA__DELETE_IMAGINARY
C
C  Function:
C     Structure-specific conversion of a complex main array into a real one.
C
C  Description:
C     This is the routine that does all the work for DSA_DELETE_IMAGINARY.
C     It knows the details about the various formats for complex arrays
C     and handles the rather messy requirements of some of these formats.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__DELETE_IMAGINARY (REF_SLOT,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The reference slot number for the
C                         structure.
C     (<) DTA_STATUS      (Integer,ref) A DTA system error code indicating
C                         the status of the deletion.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DTA_NMVAR, DTA_RNVAR, DTA_DLVAR, DTA_TYVAR, DTA_CRVAR,
C     DSA__IMAGINARY_NAME
C
C  Prior requirements:
C     REF_SLOT must refer to an open structure.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This version supports both the original Figaro data format and
C     Starlink's NDF format.
C-
C  Subroutine / function details:
C     DTA_CRVAR     Creates a named data object
C     DTA_DLVAR     Deletes a named data object
C     DTA_NMVAR     Gets name of nth data object in a structure
C     DTA_RNVAR     Renames a data object
C     DTA_TYVAR     Get type of data object - used here to check existence
C     DSA__IMAGINARY_NAME  Get name of imaginary array in a structure.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     1st  May 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__DELETE_IMAGINARY (REF_SLOT,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, DTA_STATUS
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   INLEN            ! Number of characters in INPUT
      CHARACTER INPUT*80         ! Name of main structured array
      INTEGER   IPOS             ! Component number of array structure item
      INTEGER   LENGTH           ! Length of NAME - unused
      CHARACTER NAME*80          ! Name of array structure components
      INTEGER   NM_STATUS        ! Status from DTA_NMVAR
      INTEGER   OUTLEN           ! Number of characters in OUTPUT
      CHARACTER OUTPUT*80        ! Name of temporary output array
      CHARACTER TYPE*16          ! Array type - ignored
C
C     First we get the name of the imaginary array, and see if it
C     exists.  If it doesn't, we just return good status now.
C
      CALL DSA__IMAGINARY_NAME (REF_SLOT,NAME,LENGTH)
      CALL DTA_TYVAR (NAME(:LENGTH),TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         DTA_STATUS=0
         GO TO 500          ! Doesn't exist, so exit now.
      END IF
C
C     The array exists. See what format the structure is in.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        For NDF format, things are a little messy, since if the array
C        exists, it will be in a COMPLEX_ARRAY structure, which we will
C        have to change into an ARRAY structure, copying over all bar
C        the imaginary array, which we delete.  We have to give the
C        eventual array a temporary name until we are in a position to
C        delete the current (by then empty) data array and rename the
C        new array.
C
         INPUT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.DATA_ARRAY'
         INLEN=OBJ_LEN(REF_SLOT)+11
         OUTPUT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.TEMP_ARRAY'
         OUTLEN=INLEN
         CALL DTA_CRVAR(OUTPUT,'ARRAY',DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         IPOS=1
         NM_STATUS=0
         DO WHILE (NM_STATUS.EQ.0)
C
C           Note that in the call to DTA_NMVAR to get the IPOSth item
C           in the structure, we always specify IPOS as 1 - as we go
C           through the structure we always either delete what we find
C           (the imaginary array) or rename it into the temporary structure
C           (everything else).  So each time we will get a new item
C           even though we always specify the same number.  Note that the
C           REAL array has to have its name changed to DATA.
C
            CALL DTA_NMVAR(INPUT,IPOS,NAME,NM_STATUS)
            IF (NM_STATUS.EQ.0) THEN
               IF (NAME.EQ.'IMAGINARY') THEN
                  CALL DTA_DLVAR (INPUT(:INLEN)//'.IMAGINARY',
     :                                                   DTA_STATUS)
               ELSE IF (NAME.EQ.'REAL') THEN
                  CALL DTA_RNVAR (INPUT(:INLEN)//'.'//'REAL',
     :                      OUTPUT(:OUTLEN)//'.'//'DATA',DTA_STATUS)
               ELSE
                  CALL DTA_RNVAR (INPUT(:INLEN)//'.'//NAME,
     :                        OUTPUT(:OUTLEN)//'.'//NAME,DTA_STATUS)
               END IF
               IF (DTA_STATUS.NE.0) GO TO 500      ! Error exit
            END IF
         END DO
         CALL DTA_DLVAR(INPUT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
         CALL DTA_RNVAR(OUTPUT,INPUT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500     ! Error exit
C
      ELSE
C
C        For the original Figaro format, it's easy.  It's perfectly
C        OK just to delete the array in question.
C
         CALL DTA_DLVAR(NAME(:LENGTH),DTA_STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
