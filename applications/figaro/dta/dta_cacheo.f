C+
      SUBROUTINE DTA_CACHEO (NAME,LEVELS,LASTC,STATUS)
C
C     D T A _ C A C H E O
C
C     Removes any references to a named object and objects
C     lower in the structure from the name cache.  There
C     may well be no such references, since the object is
C     assumed not to be a top level name (if it were, CACHER
C     would be used).
C
C     Parameters -     (">" input, "<" output, "!" modified)
C
C     (>) NAME     (Character) The name of the data object.
C                  Must be upper case.
C     (>) LEVELS   (Integer) The number of components in NAME.
C     (>) LASTC    (Integer array LASTC(LEVELS)) The positions in
C                  NAME of the last character of each component.
C                  (LEVELS and LASTC are as returned by SPLITN)
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  DTA_CNTINT => Cache not initialised.
C
C     Common variables used -
C
C     (!) CACHEB   The bottom level cache name array
C     (!) CACHE3    "   3rd     "     "    "    "
C     (!) CACHE2    "   2nd     "     "    "    "
C     (!) LOCNB    The bottom level locator array
C     (!) LOCN3     "   3rd     "     "       "
C     (!) LOCN2     "   2nd     "     "       "
C     (>) CACHEI   Cache initialisation flag
C
C     All in common blocks CACHEN and CACHEC
C
C     Subroutines / functions used -
C
C     DAT_ANNUL    (HDS_ package) Annul an HDS locator
C     EMS_BEGIN    (EMS_   "    ) Start a new EMS reporting environment
C     EMS_ANNUL    ( "     "    ) Clear current EMS status
C     EMS_END      ( "     "    ) End current EMS reporting environment
C
C                                        KS / CIT 3rd March 1983
C     Modified:
C
C     20th March 1986. KS / AAO. Modified for use with the HDS version
C                      of the DTA package.  Locators for all deleted
C                      cache entries are now annulled.
C     8th  Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     20th Jan  1992.  KS / AAO.  Add calls to EMS. DUMMY set to zero before
C                      being used as an inherited status value.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LEVELS,LASTC(LEVELS),STATUS
      CHARACTER*(*) NAME
C
C     Error codes
C
      INCLUDE 'DTACODES'
C
C     Cache parameters -  ones used are
C
C     NCACHB  Number of bottom level cache entries
C     NCACH3    "     "   3rd    "    "       "
C     NCACH2    "     "   2nd    "    "       "
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
C     Local variables
C
      INTEGER DOT,DUMMY,I,LAST
      CHARACTER CHR
C
C     Check for initialisation
C
      IF (.NOT.CACHEI) THEN
         STATUS=DTA_CNTINT
         GO TO 600
      END IF
C
C     Since we are probably removing the cache entry because the object is
C     no longer valid, the DAT_ANNUL calls may well fail (object invalid)
C     To prevent this fouling up anything, we do all this in a new EMS
C     reporting environment that we can annul and close on the way out.
C
      STATUS=0
      CALL EMS_BEGIN(STATUS)
C
C     Check all low level cache entries and throw out all
C     those that match NAME.
C
      LAST=LASTC(LEVELS)
      DOT=LAST+1
      DO I=1,NCACH3
         CHR=CACHE3(I)(DOT:DOT)
         IF ((CHR.EQ.' ').OR.(CHR.EQ.'.')) THEN
            IF (CACHE3(I)(:LAST).EQ.NAME(:LAST)) THEN
               CACHE3(I)='.'
               DUMMY=0
               CALL DAT_ANNUL(LOCN3(I),DUMMY)
            END IF
         END IF
      END DO
      DO I=1,NCACH2
         CHR=CACHE2(I)(DOT:DOT)
         IF ((CHR.EQ.' ').OR.(CHR.EQ.'.')) THEN
            IF (CACHE2(I)(:LAST).EQ.NAME(:LAST)) THEN
               CACHE2(I)='.'
               DUMMY=0
               CALL DAT_ANNUL(LOCN2(I),DUMMY)
            END IF
         END IF
      END DO
      DO I=1,NCACHB
         CHR=CACHEB(I)(DOT:DOT)
         IF ((CHR.EQ.' ').OR.(CHR.EQ.'.')) THEN
            IF (CACHEB(I)(:LAST).EQ.NAME(:LAST)) THEN
               CACHEB(I)='.'
               DUMMY=0
               CALL DAT_ANNUL(LOCNB(I),DUMMY)
            END IF
         END IF
      END DO
C
C     On the way out, close down the EMS environment.
C
      CALL EMS_ANNUL(STATUS)
      CALL EMS_END(STATUS)
C
  600 CONTINUE
      END

