C+
      SUBROUTINE DTA_CACHES
C
C     D T A _ C A C H E S
C
C     Lists the DTA_ system locator cache statistics on SYS$OUTPUT.
C     This is mainly a debugging routine.
C
C     Parameters -   None
C
C     Common variables used -
C
C     (>) LOOKUPS  # Cache lookups
C     (>) FAILS    # Cache lookups that failed
C     (>) COMPS    Total number of string comparisons made
C     (>) FOUNDT   # Cache hits at top level
C     (>) FOUNDB   # Cache hits at bottom level
C     (>) FOUND2   # Cache hits at second level
C     (>) FOUND3   # Cache hits at third level
C
C                  All defined in the DTACACHE.INC file
C
C     External routines called - None
C
C                                   KS / AAO  15th May 1986
C     Modified:
C
C     8th  Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     DTA_ system common
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
      PRINT *,' '
      PRINT *,'DTA_ System Cache Statistics - '
      PRINT *,' '
      PRINT *,'Total number of lookups:       ',LOOKUPS
      PRINT *,'Number of failed lookups:      ',FAILS
      PRINT *,'Number of comparisions made:   ',COMPS
      PRINT *,'Names located at bottom level: ',FOUNDB
      PRINT *,'Names located at third level:  ',FOUND3
      PRINT *,'Names located at second level: ',FOUND2
      PRINT *,'Names located at top level:    ',FOUNDT
      PRINT *,' '
C
      END

