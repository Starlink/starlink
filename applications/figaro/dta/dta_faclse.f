C+
      SUBROUTINE DTA_FACLSE(LOC,STATUS)
C
C     D T A _ F A C L S E
C
C     Closes down the low level file access routines for
C     a given file.  The file number is contained in the
C     location code for the top level entry, which is passed
C     to this routine.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) LOC      (Character) The HDS locator for the top
C                  level entry of the data structure held
C                  in the file.
C     (<) STATUS   (Integer) Returns a status code
C                  0 => OK
C                  DTA_HDSERR => an HDS error occurred
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     DTA_HDSERC   (DTA_ package) Convert HDS error code to DTA code
C     HDS_CLOSE    (HDS_    "   ) Close an HDS container file
C
C                                     KS / CIT 6th June 1984
C     Modified:
C
C     11th March 1986.  KS / AAO.  Now uses HDS routines.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) LOC
C
      STATUS=0
      CALL HDS_CLOSE(LOC,STATUS)
      IF (STATUS.NE.0) CALL DTA_HDSERC(STATUS)
C
      END

