C+
      BLOCK DATA DTA_BLOCK
C
C     D T A _ B L O C K
C
C     Block data initialisation routine for the DTA_ system.
C
C     Common variables used -
C
C     (<) CACHEI    (Logical) Cache initialisation flag, set false.
C     (<) LOOKUPS   (Integer) # Cache lookups
C     (<) FAILS     (Integer) # Cache lookups that failed
C     (<) COMPS     (Integer) Total number of string comparisons made
C     (<) FOUNDT    (Integer) # Cache hits at top level
C     (<) FOUNDB    (Integer) # Cache hits at bottom level
C     (<) FOUND2    (Integer) # Cache hits at second level
C     (<) FOUND3    (Integer) # Cache hits at third level
C
C                   Defined in the include file DTACACHE.INC
C
C                                   KS / AAO 4th April 1986
C     Modified:
C
C     15th May 1986.  KS / AAO.  Cache statistic variables added
C     8th  Jan  1992. KS/AAO.  Syntax of include statements changed to remove
C                     VMS logical names and to use lower case, to enable
C                     compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Cache common block
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACACHE'
C
      DATA CACHEI/.FALSE./
      DATA LOOKUPS,FAILS,COMPS,FOUNDT,FOUNDB,FOUND2,FOUND3/7*0/
C
      END

