C+
      SUBROUTINE DTA_FAINIT(FILE,EXIST,BLOCKS,TYPE,LOC,NEW,STATUS)
C
C     D T A _ F A I N I T
C
C     File Access INITialise.  FAINIT performs the initial
C     intermediate level processing when a data structure
C     file is initialised.  It opens or creates the file
C     and initiate the low-level file access initialisation.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) FILE    (Character) The name of the file to be
C                 opened.  This is passed straight to HDS,
C                 so can be any form that HDS (or the underlying
C                 operating system) will recognise.
C     (>) EXIST   (Character) The existence status of the file
C                 - must be one of 'NEW', 'OLD' or 'UNKNOWN'
C                 (see comments for DTA_ASFNAM). Must be
C                 upper case.
C     (>) BLOCKS  (Integer) The initial block allocation for
C                 the file.
C     (>) TYPE    (Character) The structure type for the data
C                 -see comments for DTA_ASFNAM.
C     (<) LOC     (Character) The location pointer for the first
C                 longword in the file following the header
C                 record.
C     (<) NEW     (Logical) True if the file was created, false
C                 if it already existed.
C     (<) STATUS  (Integer) Returns a status code.
C                 0 => OK
C                 Other error codes may be passed back from the
C                 lower level routines.
C-
C     Common block variables used - None
C
C     Subroutines / functions used -
C
C     DTA_HDSERC  (DTA_ package) Convert an HDS error code to a DTA code.
C     HDS_OPEN    (HDS_    "   ) Open an existing HDS container file
C     HDS_NEW     (  "     "   ) Create a new HDS container file
C     HDS_TUNE    (  "     "   ) Set HDS parameters (here, file size)
C     EMS_ANNUL   (EMS_  "     ) Clear current EMS error status.
C
C                                  KS / CIT  6th Feb 1983
C     Modified:
C
C     10th March 1986.  KS / AAO.  Complete re-write to use HDS routines.
C                       Locators (like LOC) are now character*15.  TYPE
C                       is now passed to this routine rather than being
C                       handled by DTA_ASFNAM.
C     8th Jan 1992.     KS / AAO.  Comments modified to remove VMS references,
C                       as part of port to the SUN.
C     24th Jan 1992.    KS / AAO. Setting STATUS=0 before call to HDS replaced
C                       by explicit call to EMS_ANNUL.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) FILE,EXIST,TYPE,LOC
      LOGICAL NEW
      INTEGER STATUS,BLOCKS
C
C     Local variables
C
      LOGICAL TRYNEW
C
C     Initially, assume we will have to try to create a new file.
C     First, however, so long as we are not FORCED to do so, try
C     to use an existing one.
C
      STATUS=0
      TRYNEW=.TRUE.
C
      IF (EXIST.NE.'NEW') THEN
C
C        Try to open an exisiting file for UPDATE.  If that
C        fails (for any reason) try again for READ.
C
         CALL HDS_OPEN(FILE,'UPDATE',LOC,STATUS)
         IF (STATUS.NE.0) THEN
            CALL EMS_ANNUL(STATUS)
            CALL HDS_OPEN(FILE,'READ',LOC,STATUS)
         END IF
C
C        If we opened the file OK, we don't need to try to create
C        a new one.  If we failed, we try a new one unless we were
C        FORCED to use an exisiting one.
C
         IF (STATUS.EQ.0) THEN
            NEW=.FALSE.
            TRYNEW=.FALSE.
         ELSE
            IF (EXIST.EQ.'OLD') TRYNEW=.FALSE.
         END IF
      END IF
C
C     So, after all that, do we try a new file?
C
      IF (TRYNEW) THEN
         NEW=.TRUE.
         CALL EMS_ANNUL(STATUS)
         CALL HDS_TUNE('INALQ',BLOCKS,STATUS)
         CALL HDS_NEW(FILE,'STRUCT',TYPE,0,0,LOC,STATUS)
      END IF
C
C     Convert HDS error code to a DTA one, if possible
C
      CALL DTA_HDSERC(STATUS)
C
      END

