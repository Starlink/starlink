C+
C                      D S A _ _ N D F _ T O P _ N A M E
C
C  Routine name:
C     DSA__NDF_TOP_NAME
C
C  Function:
C     Checks on a named item at the top of an NDF structure.
C
C  Description:
C     The NDF structure is well-defined, but has a mixture of items at
C     its top level.  This routine is passed the name of the top-level
C     structure (which it only uses for use in error messages) and the
C     name of an item in that structure.  It outputs a warning message if
C     if doesn't recognise the item at all, and returns a flag indicating
C     if the item is one directly assoicated with the data array (something
C     that in the original Figaro structures would have been in .Z).  If
C     it doesn't recognise the item, it assumes it is associated with the
C     data.  Similarly, it returns a flag indicating whether or not the
C     item is connected with an axis sub-structure (something that would
C     have been in the .X,.Y etc sub-structures of the original Figaro
C     format)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__NDF_TOP_NAME (ENV_NAME,NAME,WARN,DATA_ITEM,AXIS_ITEM)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ENV_NAME         (Fixed string,descr) The name of the top level
C                          structure (ie the DTA name of the environment
C                          that contains the main data array)
C     (>) NAME             (Fixed string,descr) The name of the item in
C                          the top level structure.  Must be in upper case.
C     (>) WARN             (Logical,ref) True if a warning message is to be
C                          output if the item is unexpected.
C     (<) DATA_ITEM        (Logical,ref) True if the item is directly
C                          associated with the data.
C     (<) AXIS_ITEM        (Logical,ref) True if the item is associated with
C                          the axis data for the structure.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_WRNAME
C
C  Prior requirements:
C     The file in questrion should have been opened by the DSA system,
C     the named item should exist, and the structure should be in NDF format.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C
C  Common variable details:
C
C  History:
C     26th Feb 1990.   Original version.  KS / AAO.
C     12th Mar 1990.   WARN parameter added. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996    Catenations for Linux port.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA__NDF_TOP_NAME (ENV_NAME,NAME,WARN,
     :                                          DATA_ITEM,AXIS_ITEM)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL WARN, DATA_ITEM, AXIS_ITEM
      CHARACTER*(*) ENV_NAME, NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      CHARACTER CHAR1*1            ! First character of NAME
      LOGICAL   FOUND              ! Indicates item was found in NDF lists
      INTEGER   I                  ! Loop index through NDF lists
      CHARACTER STRING*80          ! Local string storage
C
C     Following contains the names of the items expected in an NDF top
C     level, with the flags indicating if they are associated with the
C     main data array or not, and similarly the axis structure flags.
C
      INTEGER NDF_ITEMS
      PARAMETER (NDF_ITEMS=11)
      CHARACTER NDF_ITEM_NAMES(NDF_ITEMS)*10
      LOGICAL   NDF_AXIS_ITEMS(NDF_ITEMS)
      LOGICAL   NDF_DATA_ITEMS(NDF_ITEMS)
      DATA NDF_ITEM_NAMES/'VARIANT   ','TITLE     ','DATA_ARRAY',
     :       'LABEL     ','UNITS     ','VARIANCE  ','BAD_PIXEL ',
     :       'QUALITY   ','AXIS      ','HISTORY   ','MORE      '/
      DATA NDF_DATA_ITEMS/.FALSE.     ,.FALSE.     ,.TRUE.      ,
     :       .TRUE.      ,.TRUE.      ,.TRUE.      ,.TRUE.      ,
     :       .TRUE.      ,.FALSE.     ,.FALSE.     ,.FALSE.     /
      DATA NDF_AXIS_ITEMS/.FALSE.     ,.FALSE.     ,.FALSE.     ,
     :       .FALSE.     ,.FALSE.     ,.FALSE.     ,.FALSE.     ,
     :       .FALSE.     ,.TRUE.      ,.FALSE.     ,.FALSE.     /
C
C     Compare with known items.  Note assumption that a single character
C     comparison is much more efficient than a string compare.
C
      FOUND=.FALSE.
      CHAR1=NAME(1:1)
      DO I=1,NDF_ITEMS
         IF (CHAR1.EQ.NDF_ITEM_NAMES(I)(1:1)) THEN
            IF (NAME.EQ.NDF_ITEM_NAMES(I)) THEN
               FOUND=.TRUE.
               DATA_ITEM=NDF_DATA_ITEMS(I)
               AXIS_ITEM=NDF_AXIS_ITEMS(I)
               GO TO 320           ! Break out of I loop
            END IF
         END IF
      END DO
  320 CONTINUE
C
      IF ((.NOT.FOUND).AND.WARN) THEN
         CALL DSA_WRUSER ('Warning: ')
         STRING=ENV_NAME(:ICH_LEN(ENV_NAME))//'.'//NAME
         CALL DSA_WRNAME(STRING)
         CALL DSA_WRUSER (
     :      ' is not something expected in an NDF format structure.')
         CALL DSA_WRFLUSH
      END IF
C
      END
