C+
      SUBROUTINE DTA_FILET (ENAME,NAME,FILE,STATUS)
C
C     D T A _ F I L E T
C
C     This routine adds the additional information about a top
C     level structure name needed by, for example, DTA_NMFIL
C     into the name cache common variables.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ENAME    (Character) The top level structure name as entered
C                  into the name cache.  Ie, as passed to DTA_CACHEN.
C     (>) NAME     (Character) The top level structure name as passed
C                  originally to DTA_ASFNAM.  This is not necessarily
C                  the same as ENAME because ENAME may have been
C                  modified to conform to HDS naming requirements.
C     (>) FILE     (Character) The file name associated with the top
C                  level name, as passed to DTA_ASFNAM.
C     (<) STATUS   (Integer) Receives a status code.
C                  0 => OK
C                  DTA_NOTFND => ENAME is not in the name cache.
C-
C     Functions / subroutines used -
C
C     STR$UPCASE  (VMS routine) Convert string to upper case.
C
C     Common variables used -
C
C     CACHET      (Character) Top level names as used by the system
C     TLNAME      (Character) Top level names as supplied by the user
C     FILES       (Character) File names corresponding to top level
C                 structures.
C
C                 All defined in the file DTACACHE.INC
C
C                                       KS / AAO 26th May 1986
C     Modified:
C
C     8th  Jan 1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS
      CHARACTER*(*) ENAME,NAME,FILE
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Name cache definition
C
C     NCACHT   Number of top level cache slots
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER I,INVOKE
C
      DO I=1,NCACHT
         IF (CACHET(I).EQ.ENAME) THEN
            FILES(I)=FILE
            TLNAME(I)=NAME
            INVOKE=ICH_FOLD(FILES(I))
            INVOKE=ICH_FOLD(TLNAME(I))
            STATUS=0
            GO TO 600
         END IF
      END DO
C
C     If loop falls through, there's no such structure
C
      STATUS=DTA_NOTFND
C
  600 CONTINUE
C
      END

