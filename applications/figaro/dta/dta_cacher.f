C+
      SUBROUTINE DTA_CACHER (NAME,LASTC,STATUS)
C
C     D T A _ C A C H E R
C
C     Removes all trace of a top level name from the
C     name cache.  There should be at least a top level
C     entry, and this is set to the illegal value '.'.
C     There may also be lower level entries, and these are
C     also removed.
C
C     Parameters -   (">" input, "<" output, "!" modified)
C
C     (>) NAME    (Character) The top level name to be
C                 removed.
C     (>) LASTC   (Integer) The number of characters in
C                 the actual top level name - not necesarily
C                 the same as the length of NAME.
C     (<) STATUS  (Integer) Returns a status code
C                 0 => OK
C                 DTA_NOFILE => No entry found, file cannot
C                               be open
C                 DTA_CNTINT => Cache not initialised
C
C     Common variables used -
C
C     (!) CACHEB  The bottom level cache name array
C     (!) CACHE3   "    3rd   "      "    "    "
C     (!) CACHE2   "    2nd   "      "    "    "
C     (!) CACHET   "    top   "      "    "    "
C     (!) CACHEI  Cache initialisation flag
C
C     All in common blocks CACHEN and CACHEC
C
C     Subroutines / functions used -
C
C     DTA_HDSERC  (DTA_    "   ) Convert HDS error code to DTA code
C
C                                   KS / CIT   18th Oct 1982
C     Modified:
C
C     4th April 1986.  KS / AAO. Modified for use with the HDS based
C                      version of the DTA package.  Now closes down
C                      HDS when the last file is closed, and resets
C                      the cache initialisation flag.
C     8th  May  1986.  KS / AAO. Test for matching top level name
C                      modified to use all of name cache entry.
C     26th May  1986.  KS / AAO. Local variable FILES renamed to avoid
C                      name clash with common variable.
C     6th Oct 1987.    KS / AAO. No longer calls HDS_STOP, since this may
C                      interfere with other packages.  Now relies on the
C                      exit handler to close HDS.
C     8th  Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER LASTC,STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Cache parameters -  ones used are
C
C     NCACHB   Number of bottom level cache entries
C     NCACH3     "    "   3rd     "    "      "
C     NCACH2     "    "   2nd     "    "      "
C     NCACHT     "    "   top     "    "      "
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
C     Local variables -
C
      LOGICAL FOUND
      INTEGER I,DOT,NFILES
C
C     Check for initialisation
C
      IF (.NOT.CACHEI) THEN
         STATUS=DTA_CNTINT
         GO TO 600
      END IF
C
C     Top level entries - must be in here.
C
      NFILES=0
      FOUND=.FALSE.
      DO I=1,NCACHT
         IF (CACHET(I).NE.'.') THEN
            IF (CACHET(I).EQ.NAME) THEN
               FOUND=.TRUE.
               CACHET(I)='.'
            ELSE
               NFILES=NFILES+1
            END IF
         END IF
      END DO
C
C     Check that it was there
C
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOFILE
         GO TO 600
      END IF
C
C     Now look at all the other entries and throw out
C     all those with NAME as top level.
C
  340 CONTINUE
      DOT=LASTC+1
      DO I=1,NCACH3
         IF (CACHE3(I)(DOT:DOT).EQ.'.') THEN
            IF (CACHE3(I)(:LASTC).EQ.NAME(:LASTC)) THEN
               CACHE3(I)='.'
            END IF
         END IF
      END DO
      DO I=1,NCACH2
         IF (CACHE2(I)(DOT:DOT).EQ.'.') THEN
            IF (CACHE2(I)(:LASTC).EQ.NAME(:LASTC)) THEN
               CACHE2(I)='.'
            END IF
         END IF
      END DO
      DO I=1,NCACHB
         IF (CACHEB(I)(DOT:DOT).EQ.'.') THEN
            IF (CACHEB(I)(:LASTC).EQ.NAME(:LASTC)) THEN
               CACHEB(I)='.'
            END IF
         END IF
      END DO
      STATUS=0
C
C     If we have no open files left in the system, close
C     everything down.
C
      IF (NFILES.EQ.0) CACHEI=.FALSE.
C
  600 CONTINUE
      END

