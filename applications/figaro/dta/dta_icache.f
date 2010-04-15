C+
      SUBROUTINE DTA_ICACHE (STATUS)
C
C     D T A _ I C A C H E
C
C     Initialises the name cache if it has not already been
C     initialised.  All names are set with an initial character
C     '.' which cannot be the start of a valid name.  Also
C     initialises the HDS locator system, if necessary.
C
C     Parameters -  ("<" output)
C
C     (<) STATUS   (Integer) Returned status code.
C                  0 => OK.
C                  DTA_HDSERR => HDS_ system error.
C
C     Subroutines / functions used -
C
C     HDS_START    (HDS_ package) Startup HDS locator facility.
C     HDS_TUNE     ( "      "   ) Tune HDS parameters
C     DTA_BLOCK    (DTA_ package) Block data initialisation routine.
C     EMS_ANNUL    (EMS_  "     ) Clear current EMS error status.
C
C     Common block variables used -   ("<" output, "!" modified)
C
C     (<) CACHEB   The bottom level cache name array
C     (<) CACHE3   The third level cache name array
C     (<) CACHE2   The second level cache name table
C     (<) CACHET   The top level cache name table
C     (!) CACHEI   Flag to indicate cache initialised
C     (<) CNEXTB   Next slot pointer bottom level
C     (<) CNEXT3   Next slot pointer third level
C     (<) CNEXT2   Next slot pointer second level
C
C     All in common blocks CACHEC and CACHEN
C
C     (<) HDSTAT   HDS_ system error code.  (In common block PROBE)
C
C                                  KS / CIT  15th Oct 1982
C     Modified:
C
C     10th March 1986.  KS / AAO.  Modified for use with HDS_ system.
C                       STATUS parameter added, call to HDS_START added.
C     15th May  1986.   KS / AAO.  Call to HDS_TUNE added.
C     4th  March 1987.  KS / AAO.  Call to EXC_$LEVEL added.  This routine
C                       will now no longer work with the old Bliss version
C                       of HDS.
C     6th Oct 1987.     KS / AAO.  Call to HDS_START now tests for 'already
C                       active' status.
C     8th  Jan  1992.   KS / AAO.  Syntax of include statements changed to
C                       remove VMS logical names and to use lower case, to
C                       enable compilation on a SUN.  Call to EXC_$LEVEL
C                       removed.
C     24th Jan 1992.    KS / AAO. Explicit calls to EMS_ANNUL replace just
C                       setting STATUS to zero.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Block data initialisation routine.  Not explicitly used by this
C     routine, but referenced to force the linker to include it.
C
      EXTERNAL DTA_BLOCK
C
C     Local variables -
C
      INTEGER I
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
C     Error codes -
C
      INCLUDE 'DTACODES'
      INCLUDE 'HDSCODES'
C
C     PROBE common block
C
      INCLUDE 'DTAPROBE'
C
C     Check for already initialised
C
      IF (.NOT.CACHEI) THEN
C
C        Initialise all the names
C
         DO I=1,NCACHB
            CACHEB(I)='.'
         END DO
         DO I=1,NCACH3
            CACHE3(I)='.'
         END DO
         DO I=1,NCACH2
            CACHE2(I)='.'
         END DO
         DO I=1,NCACHT
            CACHET(I)='.'
         END DO
C
C        Set the slot pointers
C
         CNEXTB=NCACHB
         CNEXT3=NCACH3
         CNEXT2=NCACH2
C
C        Startup HDS locator system - the '100' is something of a
C        wild guess!
C
         IF (STATUS.NE.0) CALL EMS_ANNUL(STATUS)
         CALL HDS_TUNE('MAXWPL',100,STATUS)
         CALL HDS_START(STATUS)
         IF ((STATUS.NE.0).AND.(STATUS.NE.DAT__ACTIV)) THEN
            HDSTAT=STATUS
            STATUS=DTA_HDSERR
         ELSE
            CALL EMS_ANNUL(STATUS)
         END IF
C
C        Indicate Cache now initialised
C
         CACHEI=.TRUE.
C
      ELSE
C
C        Cache was already initialised
C
         STATUS=0
C
      END IF
C
      END

