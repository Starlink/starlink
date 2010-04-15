C+
      SUBROUTINE DTA_CACHEL (NAME,LEVELS,LASTC,LEV,LOC,STATUS)
C
C     D T A _ C A C H E L
C
C     Locates a name cache entry for a named object.  The cache
C     may contain the complete name, in which case no further
C     searches through the data structure will be needed.  More
C     likely, the cache will contain an entry for a structure
C     name one or more levels up from the name of the object.
C     At the very least, as long as the file in question has been
C     opened, the cache will contain the top level name.  If a
C     higher level name is found, this will provide a starting
C     point for a search down through the structure.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The full name of the object.  This
C                  should have been analysed earlier by DTA_SPLITN
C                  to obtain the information in LEVELS and LASTC.
C                  NAME should be in upper case.
C     (>) LEVELS   (Integer) The number of levels of name in the
C                  object name.
C     (>) LASTC    (Integer LASTC(LEVELS)) Array giving the positions
C                  in NAME of the last characters of each of the
C                  components of NAME.
C     (<) LEV      (Integer) The level at which a cache entry was
C                  found.
C     (<) LOC      (Character) The HDS locator for the object
C                  that was found in the name cache.  This will be
C                  either the object itself or a higher level
C                  structure record.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK, an entry was found.
C                  DTA_NOFILE => No entry found, file cannot be open.
C-
C     Common block variables used -
C
C     (>) CACHEB   The bottom level cache name array
C     (>) LOCNB    The bottom level cache location table
C     (>) CACHE3   The third level cache name array
C     (>) LOCN3    The third level cache location table
C     (>) CACHE2   The second level cache name table
C     (>) LOCN2    The second level cache location table
C     (>) CACHET   The top level cache name table
C     (>) LOCNT    The top level cache location table
C     (>) CACHEI   Flag to indicate cache initialised
C     (<) LOOKUPS  # Cache lookups
C     (<) FAILS    # Cache lookups that failed
C     (<) COMPS    Total number of string comparisons made
C     (<) FOUNDT   # Cache hits at top level
C     (<) FOUNDB   # Cache hits at bottom level
C     (<) FOUND2   # Cache hits at second level
C     (<) FOUND3   # Cache hits at third level
C
C     All in common blocks CACHEN and CACHEC
C
C     Subroutines / functions used - None
C
C                                         KS / CIT  19th Oct 1982
C     Modified:
C
C     10th Mar 1986.  KS / AAO.  Changed for use with HDS based
C                     version of DTA_ routines.  Locators are now
C                     character*15 instead of integer.
C     15th May 1986.  KS / AAO.  Statistics variables added.  Search
C                     of bottom level bypassed if LEVELS is too low.
C      6th Sep 1990.  SMB / ROE. If an object with more than 4 levels
C                     was searched for, this routine missed any entries
C                     in the bottom level cache which were one or more
C                     levels up from the object. The net effect was that
C                     the cache ended up containing duplicate entries
C                     with different locators.
C                     To fix this problem, the routine was modified to
C                     check for matches at all levels from LEVELS down
C                     to 4 in the bottom level cache.
C     12th Apr 1991.  KS / AAO. Recoded to try to minimise the number of
C                     long character string comparisions made.
C     8th  Jan 1992.  KS/AAO.  Syntax of include statements changed to remove
C                     VMS logical names and to use lower case, to enable
C                     compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME,LOC
      INTEGER LEVELS,LEV,STATUS
      INTEGER LASTC(LEVELS)
C
C     Error codes
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACODES'
C
C     Cache parameters - ones used are
C
C     NCACHB     Number of bottom level cache entries
C     NCACH3       "    "  third   "     "      "
C     NCACH2       "    "  second  "     "      "
C     NCACHT       "    "  top     "     "      "
C
      INCLUDE 'DTACACHE'
C
C     Local variables
C
      INTEGER I, IENP, J
      CHARACTER ENDCHR*1
C
C     If cache not initialised, no files are open, so error
C
      IF (.NOT.CACHEI) GO TO 510
C
C     Check bottom level cache entries. (Note that a single comparison
C     is generally much faster than a string comparison, so we only do
C     a full string comparison on strings where the last character
C     matches.)
C
      LOOKUPS=LOOKUPS+1
      STATUS=0
      IF (LEVELS.GT.3) THEN
         DO J=LEVELS,4,-1
            IENP=LASTC(J)
            ENDCHR=NAME(IENP:IENP)
            DO I=1,NCACHB
               COMPS=COMPS+1
               IF (ENDCHR.EQ.CACHEB(I)(IENP:IENP)) THEN
                  IF (NAME(:IENP).EQ.CACHEB(I)) THEN
                     FOUNDB=FOUNDB+1
                     LEV=J
                     LOC=LOCNB(I)
                     GO TO 600
                  END IF
               END IF
            END DO
         END DO
      END IF
C
C     Check third level cache entries
C
      IF (LEVELS.GE.3) THEN
         IENP=LASTC(3)
         ENDCHR=NAME(IENP:IENP)
         DO I=1,NCACH3
            COMPS=COMPS+1
            IF (ENDCHR.EQ.CACHE3(I)(IENP:IENP)) THEN
               IF (NAME(:IENP).EQ.CACHE3(I)) THEN
                  FOUND3=FOUND3+1
                  LEV=3
                  LOC=LOCN3(I)
                  GO TO 600
               END IF
            END IF
         END DO
      END IF
C
C     Check second level cache entries
C
      IF (LEVELS.GE.2) THEN
         IENP=LASTC(2)
         ENDCHR=NAME(IENP:IENP)
         DO I=1,NCACH2
            COMPS=COMPS+1
            IF (ENDCHR.EQ.CACHE2(I)(IENP:IENP)) THEN
               IF (NAME(:LASTC(2)).EQ.CACHE2(I)) THEN
                  FOUND2=FOUND2+1
                  LEV=2
                  LOC=LOCN2(I)
                  GO TO 600
               END IF
            END IF
         END DO
      END IF
C
C     Finally, check top level entries
C
      DO I=1,NCACHT
         IENP=LASTC(1)
         ENDCHR=NAME(IENP:IENP)
         COMPS=COMPS+1
         IF (ENDCHR.EQ.CACHET(I)(IENP:IENP)) THEN
            IF (NAME(:IENP).EQ.CACHET(I)) THEN
               FOUNDT=FOUNDT+1
               LEV=1
               LOC=LOCNT(I)
               GO TO 600
            END IF
         END IF
      END DO
C
C     Not found.  File cannot be open.
C
  510 CONTINUE
      FAILS=FAILS+1
      STATUS=DTA_NOFILE
C
  600 RETURN
      END


