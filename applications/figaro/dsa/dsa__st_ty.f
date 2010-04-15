C+
C                       D S A _ _ S E T _ F I L E _ T Y P E
C
C  Routine name:
C     DSA__SET_FILE_TYPE
C
C  Function:
C     Sets DSA common variables depending on type of file being opened.
C
C  Description:
C     This routine should be called when a new reference slot in DSA
C     common is opened, once all the other items in the common blocks
C     have been set.  It determines, one way or another, what type of
C     data file we are looking at, and sets the flags that will be used
C     by the DSA__ routines to control their operation.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SET_FILE_TYPE (REF_SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The reference slot number in
C                        common used for the structure that has just been
C                        opened.
C     (!) STATUS         (Integer,ref) Status variable.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DTA_NMVAR, DTA_TYVAR, DTA_STRUC, DSA_WRUSER, ICH_LEN
C
C  Prior requirements:
C     The other common variables for the reference slot must have already
C     been initialised.  In particular, this routine makes use of the
C     REF_FILE value, and assumes that the file common variables this
C     points to have also been set for the file in question.  Also, if
C     this is a new structure based on another structure, then the old
C     structure should have been copied into it first.  Alternatively,
C     DSA__SAME_FILE_FORMAT can be used to force the file format to match
C     the base file format.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_TYVAR        Get type of data object - used here as existence check
C     DTA_NMVAR        Get name of nth component - used here to check for empty
C     DTA_STRUC        See if data object is a structure
C     DSA_WRUSER       Output a message to the user.
C     ICH_LEN          Position of last non-blank char in string
C
C  Common variable details:
C     (>) DISK_FORMAT   (Integer) Code controlling which formats are supported.
C     (>) NDF_THEN_DST  (Integer parameter) DST and NDF supported, NDF default.
C     (>) NDF_ONLY      (Integer parameter) Only NDF format is supported.
C     (>) FILE_NAMES    (String array) Full file specification for each file.
C     (>) REF_FILE      (Integer array) File slot number of file in question.
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (<) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     15th Jan 1990.   Original version.  KS / AAO.
C     19th Jan 1990.   Name changed to DSA__ and now looks at structure
C                      contents as well as just the name.  KS/AAO.
C     21st Feb 1990.   Check on structure contents now rather more
C                      thorough, and message output if format is ambiguous.
C                      KS/AAO.
C     3rd  May 1990.   Although the presence of a .DATA_ARRAY is now regarded
C                      as overwhelming evidence for an NDF format, the structure
C                      is now still checked for typical DSA components. KS/AAO.
C     3rd  Mar 1991.   .DST files with a .MORE structure seem to be increasingly
C                      common. Don't output the warning message if this is all
C                      that seems to be amiss. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     4th  Sep 1992    Changed strings '.SDF' to '.sdf' and '.DSA' to
C                      '.dst' (no typo here). HME/UoE, Starlink.
C+
      SUBROUTINE DSA__SET_FILE_TYPE (REF_SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STATUS
C
C     Functions
C
      INTEGER ICH_LEN
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   BY_NAME          ! Format will have to be decided by name
      INTEGER   DST_CLUES        ! Indications that file has DST format
      INTEGER   DTA_STATUS       ! Status returned by DTA routines.
      INTEGER   NDF_CLUES        ! Indications that file has NDF format
      LOGICAL   NOTIFY           ! List final decision on file type.
      CHARACTER STRING*16        ! Used as scratch string
      LOGICAL   STRUCT           ! Indicates object is a structure
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Initial values
C
      NOTIFY=.FALSE.
      BY_NAME=.FALSE.
C
C     Is the structure empty?
C
      CALL DTA_NMVAR (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT)),1,STRING,
     :                                                       DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        Structure is empty, so we can't tell anything from that.  In this
C        case we will have to decide on the basis of the file name, or
C        failing that on the basis of the default type.
C
         BY_NAME=.TRUE.
C
      ELSE
C
C        Structure is not empty.  First of all we look for a .DATA_ARRAY
C        in the structure.  If there is one, this should definitely be
C        treated as an NDF file and this clue is given overwhelming weight.
C        If not, then it's a little tricky, since all we're going to have
C        are clues. We give half weight to the file type, and then award
C        marks for finding a .Z structure, a .MORE structure, and a .FITS
C        structure and a .VARIANT string.  (We only check for a .MORE if
C        there are no other clues at all.)
C
         NDF_CLUES=0
         DST_CLUES=0
         CALL DTA_TYVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                               '.DATA_ARRAY',STRING,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) NDF_CLUES=10
         CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                      '.Z',STRUCT,DTA_STATUS)
         IF ((DTA_STATUS.EQ.0).AND.STRUCT) DST_CLUES=DST_CLUES+1
         CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                   '.VARIANT',STRUCT,DTA_STATUS)
         IF ((DTA_STATUS.EQ.0).AND.(.NOT.STRUCT)) NDF_CLUES=NDF_CLUES+1
         CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                      '.FITS',STRUCT,DTA_STATUS)
         IF ((DTA_STATUS.EQ.0).AND.STRUCT) DST_CLUES=DST_CLUES+1
         IF (DST_CLUES+NDF_CLUES.EQ.0) THEN
            CALL DTA_STRUC (OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//
     :                                      '.MORE',STRUCT,DTA_STATUS)
            IF ((DTA_STATUS.EQ.0).AND.STRUCT) NDF_CLUES=NDF_CLUES+1
         END IF
         IF (DST_CLUES.EQ.NDF_CLUES) THEN
            BY_NAME=.TRUE.
         ELSE
            NDF_FORMAT(REF_SLOT)=NDF_CLUES.GT.DST_CLUES
         END IF
         IF ((NDF_CLUES.GT.0).AND.(DST_CLUES.GT.0)) THEN
            CALL DSA_WRUSER ('Note: It is not clear just what file ')
            CALL DSA_WRUSER ('format has been used to create the file ')
            CALL DSA_WRUSER (FILE_NAMES(REF_FILE(REF_SLOT))
     :                       (:ICH_LEN(FILE_NAMES(REF_FILE(REF_SLOT)))))
            CALL DSA_WRUSER ('.  It has some components typical of'//
     :              ' the original Figaro format (.DST), and some that')
            CALL DSA_WRUSER (
     :                     ' indicate it is in Starlink''s NDF format.')
            NOTIFY=.TRUE.
         END IF
      END IF
C
C     At this point, either we found something in the file to tell us
C     it's type, or we are reduced to looking at the file name. If it is
C     a file with a .SDF extension, assume NDF format.  If it has a
C     .DST format, assume Figaro format.  If it has a different extension,
C     use the system default format.
C
      IF (BY_NAME) THEN
         IF (INDEX(FILE_NAMES(REF_FILE(REF_SLOT)),'.sdf').NE.0) THEN
            NDF_FORMAT(REF_SLOT)=.TRUE.
         ELSE
            IF (INDEX(FILE_NAMES(REF_FILE(REF_SLOT)),'.dst').NE.0) THEN
               NDF_FORMAT(REF_SLOT)=.FALSE.
            ELSE
               IF ((DISK_FORMAT.EQ.NDF_ONLY).OR.
     :                              (DISK_FORMAT.EQ.NDF_THEN_DST)) THEN
                  NDF_FORMAT(REF_SLOT)=.TRUE.
               ELSE
                  NDF_FORMAT(REF_SLOT)=.FALSE.
               END IF
            END IF
         END IF
      END IF
C
      IF (NOTIFY) THEN
         CALL DSA_WRUSER (' On balance, it is being treated as ')
         IF (NDF_FORMAT(REF_SLOT)) THEN
            CALL DSA_WRUSER ('an NDF format file.')
            CALL DSA_WRFLUSH
         ELSE
            CALL DSA_WRUSER ('a DST format file.')
            CALL DSA_WRFLUSH
         END IF
      END IF
C
      END
