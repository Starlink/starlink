C+
      SUBROUTINE ARC_ARDELE(IX,CHANS,WAVES,WEIGHTS,CLASS,NLID)
C
C     A R D E L E
C
C     Deletes an identified line from the line tables.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) IX       (Integer) The number of the element to be
C                  deleted.
C     (!) CHANS    (Real array CHANS(NLID)) Channel numbers for
C                  identified arc lines.
C     (!) WAVES    (Real array WAVES(NLID)) Wavelengths of the
C                  identified arc lines.
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (!) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (!) NLID     (Integer) The number of identified arc lines.
C
C                                        KS / CIT 13th Jan 1983
C     Modified:
C
C     5th Sept 1985  KS / AAO  WEIGHTS and CLASS added.
C+
      IMPLICIT NONE
C
C     Parameters  - (dimensions are 1 in case NLID=0)
C
      INTEGER IX,NLID,CLASS(1)
      REAL CHANS(1),WAVES(1),WEIGHTS(1)
C
C     Local variables
C
      INTEGER I
C
C     Check IX value is OK. If so, delete the entries.
C
      IF ((NLID.GT.0).AND.(NLID.GE.IX)) THEN
         NLID=NLID-1
         DO I=IX,NLID
            CHANS(I)=CHANS(I+1)
            WAVES(I)=WAVES(I+1)
            WEIGHTS(I)=WEIGHTS(I+1)
            CLASS(I)=CLASS(I+1)
         END DO
      END IF
C
      RETURN
      END
