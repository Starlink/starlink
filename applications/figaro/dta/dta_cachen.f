C+
      SUBROUTINE DTA_CACHEN (NAME,LOC,LEV,STATUS)
C
C     D T A _ C A C H E N
C
C     Inserts a name into the name cache at a specified
C     level, together with a location pointer.
C
C     Parameters -   (">" input, "<" output, "!" modified)
C
C     (>) NAME     (Character) The name to be inserted into
C                  the cache.  No validation is performed,
C                  if the name is shorter than the cache slot
C                  it will be blank filled, if longer it will
C                  be truncated. Should be upper case.
C     (>) LOC      (Character) The HDS locator to be
C                  associated with NAME in the cache.
C     (>) LEV      (Integer) The cache level at which NAME is
C                  to be inserted.  Essentially, this is the
C                  number of component names in NAME, ie
C                  LEV=1 => top level cache
C                  LEV=2 => 2nd level cache
C                  LEV=3 => 3rd level cache
C                  LEV>3 => bottom level cache
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => inserted OK
C                  DTA_INVPAR => LEV less than 1
C                  DTA_CFULL  => LEV=1 and no slots left (ie
C                                too many files open)
C                  DTA_CNTINT => Cache not initialised
C-
C     Common block variables used -
C
C     (>) CACHEB   The bottom level cache name array
C     (>) LOCNB    The bottom level cache location table
C     (>) CACHE3   The third level cache name array
C     (>) LOCN3    The third level cache location table
C     (>) CACHE2   The second level cache name table
C     (>) LOCN2    The second level cache location table
C     (!) CACHET   The top level cache name table
C     (>) LOCNT    The top level cache location table
C     (!) CNEXTB   Bottom level next slot pointer
C     (!) CNEXT3   Third level next slot pointer
C     (!) CNEXT2   Second level next slot pointer
C     (>) CNTINT   Cache initialisation flag
C
C     All in common blocks CACHEC and CACHEN
C
C     Subroutines / functions used -
C
C     DAT_ANNUL   (HDS_ package) Cancel a locator.
C     DTA_HDSERC  (DTA_    "   ) Convert an HDS error code to a DTA code.
C
C                                       KS / CIT   15th Oct 1982
C     Modified:
C
C     10th Mar 1986.  KS / AAO.  Changed for use with HDS based
C                     version of DTA_ routines.  Locators are now
C                     character*15 instead of integer.
C     8th  Jan 1992.  KS/AAO.  Syntax of include statements changed to remove
C                     VMS logical names and to use lower case, to enable
C                     compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME,LOC
      INTEGER LEV,STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Cache parameters - ones used are
C
C     NCACHB     Number of bottom level cache entries
C     NCACH3       "    "  third   "     "      "
C     NCACH2       "    "  second  "     "      "
C     NCACHT       "    "  top     "     "      "
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
C     Local variables
C
      INTEGER I
C
C     Check for cache initialised
C
      IF (.NOT.CACHEI) THEN
         STATUS=DTA_CNTINT
      ELSE
C
C        Check level is valid
C
         IF (LEV.LT.1) THEN
            STATUS=DTA_INVPAR
         ELSEIF (LEV.EQ.1) THEN
C
C           Top level. Have to search for a free slot
C
            DO I=1,NCACHT
               IF (CACHET(I)(1:1).EQ.'.') THEN
                  STATUS=0
                  LOCNT(I)=LOC
                  CACHET(I)=NAME
                  GO TO 310
               END IF
            END DO
C
C           If loop falls through, table is full
C
            STATUS=DTA_CFULL
  310       CONTINUE
C
C           Other levels can be done as circular lists, using
C           the next free slot pointers.  If we have to remove
C           any active entries from the cache, annul the locators.
C
         ELSEIF (LEV.EQ.2) THEN
            STATUS=0
            IF (CACHE2(CNEXT2).NE.'.') THEN
               CALL DAT_ANNUL(LOCN2(CNEXT2),STATUS)
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
            END IF
            CACHE2(CNEXT2)=NAME
            LOCN2(CNEXT2)=LOC
            CNEXT2=CNEXT2-1
            IF (CNEXT2.LT.1) CNEXT2=NCACH2
         ELSEIF (LEV.EQ.3) THEN
            STATUS=0
            IF (CACHE3(CNEXT3).NE.'.') THEN
               CALL DAT_ANNUL(LOCN3(CNEXT3),STATUS)
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
            END IF
            CACHE3(CNEXT3)=NAME
            LOCN3(CNEXT3)=LOC
            CNEXT3=CNEXT3-1
            IF (CNEXT3.LT.1) CNEXT3=NCACH3
         ELSE
            STATUS=0
            IF (CACHEB(CNEXTB).NE.'.') THEN
               CALL DAT_ANNUL(LOCNB(CNEXTB),STATUS)
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
            END IF
            CACHEB(CNEXTB)=NAME
            LOCNB(CNEXTB)=LOC
            CNEXTB=CNEXTB-1
            IF (CNEXTB.LT.1) CNEXTB=NCACHB
         END IF
      END IF
C
  600 CONTINUE
C
      END

