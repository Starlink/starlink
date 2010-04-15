C+
      SUBROUTINE DTA_HDSERC (STATUS)
C
C     D T A _ H D S E R C
C
C     Converts an HDS error code into the corresponding DTA
C     code, should one exist.  If there is no such code, the
C     passed error code is converted into the catch-all 'HDS
C     error occured' code.  DTA_ERROR will then be able to
C     recover the appropriate HDS error message through the
C     use of the 'probe' common block.  Note that the choice
C     of what constitutes an equivalent code is not always
C     clear-cut, and this routine may not always make what
C     any particular user might consider the correct choice.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) STATUS    (Integer) Passed as an HDS error code,
C                   returned as a DTA error code.  Note that
C                   a DTA error code should NOT be passed to
C                   this routine - there is no guarantee that
C                   there is no overlap of values between the
C                   two sets of codes.
C
C     Common variables used -
C
C     (<) HDSTAT    (Integer) Latest HDS error code.
C
C                   In common block PROBE.
C
C     Subroutines / functions used - None
C
C                                            KS / AAO 11th March 1986
C     Modified:
C
C     8th  Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN. SAVE statement added
C                      for variables initialised using DATA.
C     17th Jan  1992.  KS / AAO.  Changed to only have 9 specific codes,
C                      instead of 10 starting with zero, and to test for
C                      zero explicitly first.  This is probably infinitesimaly
C                      faster, but mainly it seems to bypass a bug in the
C                      SUN compiler, which seemed to have problems with
C                      DATA statements where the first element was set to
C                      zero!
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     DTA_ package error codes
C
      INCLUDE 'DTACODES'
C
C     HDS_ package error codes
C
      INCLUDE 'HDSCODES'
C
C     PROBE common block
C
      INCLUDE 'DTAPROBE'
C
C     Codes we think have a fair equivalent.  Note that in both
C     systems, zero represents good status.
C
      INTEGER NCODES
      PARAMETER (NCODES=9)
C
      INTEGER HDS_CODES(NCODES),DTA_CODES(NCODES)
      SAVE HDS_CODES,DTA_CODES
C
C     Local variables
C
      INTEGER I
C
C     DATA statements
C
      DATA HDS_CODES/DAT__ACCON,DAT__COMEX,DAT__CONER,
     :               DAT__CONIN,DAT__FILIN,DAT__NAMIN,DAT__OBJNF,
     :               DAT__TRUNC,DAT__VERMM/
      DATA DTA_CODES/DTA_RDONLY,DTA_EXIST,DTA_BADCON,
     :               DTA_CHRCVT,DTA_BADFILE,DTA_INVNAM,DTA_NOTFND,
     :               DTA_OK,DTA_BADVER/
C
C     Check for the codes that have equivalents
C
      IF (STATUS.EQ.0) GO TO 600
      DO I=1,NCODES
         IF (STATUS.EQ.HDS_CODES(I)) THEN
            STATUS=DTA_CODES(I)
            GO TO 600
         END IF
      END DO
C
C     If none of those, use the catch-all code.
C
      HDSTAT=STATUS
      STATUS=DTA_HDSERR
C
  600 CONTINUE
C
      END

