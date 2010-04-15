C+
      SUBROUTINE DTA_NMFIL (POS,NAME,FILE,STATUS)
C
C     D T A _ N M F I L
C
C     This routine returns the name of the nth top level structure
C     open at present.  It is intended mainly for use in general
C     purpose clean-up routines that can then close structures they
C     didn't expect to find open.  Note that calls to DTA_ASFNAM or
C     DTA_FCLOSE may change the internal numbering of top level
C     structures.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) POS      (Integer) The number of the top-level structure
C                  whose name is to be obtained. Numbers start at 1.
C     (<) NAME     (Character) Returns with the top-level structure
C                  name, truncated or blank filled, as necessary,
C                  in upper case.  This is the name as passed to
C                  DTA_ASFNAM.
C     (<) FILE     (Character) Returns with the file name associated
C                  with the top level name.
C     (<) STATUS   (Integer) Receives a status code.
C                  0 => OK
C                  DTA_NOTFND => There is no POSth top level structure
C                  open at present.
C-
C     Functions / subroutines used - None
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
C     10th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     12th Apr 1993.  HME / UoE, Starlink. For DTACACHE must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER POS,STATUS
      CHARACTER*(*) FILE,NAME
C
C     Error codes -
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTACODES'
C
C     Name cache definition
C
C     NCACHT   Number of top level cache slots
C
      INCLUDE 'DTACACHE'
C
C     Local variables
C
      INTEGER I,IPOS
C
      FILE=' '
      NAME=' '
      IPOS=0
      DO I=1,NCACHT
         IF (CACHET(I).NE.'.') THEN
            IPOS=IPOS+1
            IF (IPOS.EQ.POS) THEN
               FILE=FILES(I)
               NAME=TLNAME(I)
               STATUS=0
               GO TO 600
            END IF
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

