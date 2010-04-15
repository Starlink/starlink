C+
C                    D S A _ _ T O P _ I T E M _ T Y P E
C
C  Routine name:
C     DSA__TOP_ITEM_TYPE
C
C  Function:
C     Classifies a named item at the top of a structure.
C
C  Description:
C     This routine takes the name of an item at the top level of a data
C     structure and classifies it as either something to do with the
C     axis information, something directly connected with the main data
C     array, or something else.  This is used for routines like
C     DSA_NAMED_OUTPUT that need to know whether or not to copy a data
C     item from one structure to another.  If the structure format is
C     well-defined (eg NDF) then a warning message can be output if an
C     unexpected item is found.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__TOP_ITEM_TYPE (REF_SLOT,NAME,WARN,AXIS_TYPE,DATA_TYPE)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT         (Integer,ref) The reference slot number for
C                          the structure.
C     (>) NAME             (Fixed string,descr) The name of the item in
C                          the top level structure.  Must be in upper case.
C     (>) WARN             (Logical,ref) True if a warning message is to be
C                          output if the item is unexpected.
C     (<) AXIS_TYPE        (Logical,ref) True if the item is associated with
C                          the axis data for the structure.
C     (<) DATA_TYPE        (Logical,ref) True if the item is directly
C                          associated with the data.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DSA__NDF_TOP_NAME
C
C  Prior requirements:
C     The file in questrion should have been opened by the DSA system,
C     and the named item should exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA__NDF_TOP_NAME     Classify an NDF top level item and warn if unknown
C
C  Common variable details:
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  History:
C     26th Feb 1990.   Original version.  KS / AAO.
C     12th Mar 1990.   WARN parameter added. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__TOP_ITEM_TYPE (REF_SLOT,NAME,WARN,
     :                                           AXIS_TYPE,DATA_TYPE)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL WARN, AXIS_TYPE, DATA_TYPE
      INTEGER REF_SLOT
      CHARACTER*(*) NAME
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   I                 ! Loop variable through structure names
C
C     Names of axis and data items in original Figaro format files
C
      INTEGER DATA_NAMES, AXIS_NAMES
      PARAMETER (DATA_NAMES=1,AXIS_NAMES=6)
      CHARACTER DATA_NAME(DATA_NAMES)*1, AXIS_NAME(AXIS_NAMES)*1
      DATA DATA_NAME,AXIS_NAME /'Z','X','Y','T','U','V','W'/
C
C     What we do depends on the format of the file.
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        For NDF format we use a utility routine that includes the
C        output of warning messages for unknown items.
C
         CALL DSA__NDF_TOP_NAME (OBJ_NAMES(REF_SLOT),NAME,WARN,
     :                                          DATA_TYPE,AXIS_TYPE)
      ELSE
C
C        For original Figaro format, unknown items are OK, so all we
C        can do is test for structures we know.
C
         AXIS_TYPE=.FALSE.
         DATA_TYPE=.FALSE.
         DO I=1,AXIS_NAMES
            IF (NAME.EQ.AXIS_NAME(I)) THEN
               AXIS_TYPE=.TRUE.
               GO TO 320         ! Break I loop
            END IF
         END DO
  320    CONTINUE
         DO I=1,DATA_NAMES
            IF (NAME.EQ.DATA_NAME(I)) THEN
               DATA_TYPE=.TRUE.
               GO TO 340         ! Break I loop
            END IF
         END DO
  340    CONTINUE
C
      END IF
C
      END
