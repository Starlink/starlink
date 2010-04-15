C+
      SUBROUTINE ARC_ARSORT(CHANS,WAVES,WEIGHTS,CLASS,NLID)
C
C     A R S O R T
C
C     Sorts the identified line tables into ascending order of
C     channel number.
C
C     Parameters - (">" input, "!" modified)
C
C     (!) CHANS    (Real array CHANS(NLID)) The channel numbers
C                  of the identified arc lines
C     (!) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified arc lines
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (!) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of identified lines
C
C                                      KS / CIT 9th June 1983
C     Modified:
C
C     5th Sept 1985.  KS / AAO  WEIGHTS and CLASS parameters added.
C+
      IMPLICIT NONE
C
C     Parameters (1 as dimension in case NLID=0)
C
      INTEGER NLID,CLASS(1)
      REAL CHANS(1),WAVES(1),WEIGHTS(1)
C
C     Local variables
C
      INTEGER I,J,TEMPI
      REAL TEMP
C
C     Sort arrays on CHANS values
C
      IF (NLID.GT.1) THEN
         DO I=1,NLID-1
            DO J=I+1,NLID
               IF (CHANS(I).GT.CHANS(J)) THEN
                  TEMP=CHANS(J)
                  CHANS(J)=CHANS(I)
                  CHANS(I)=TEMP
                  TEMP=WAVES(J)
                  WAVES(J)=WAVES(I)
                  WAVES(I)=TEMP
                  TEMP=WEIGHTS(J)
                  WEIGHTS(J)=WEIGHTS(I)
                  WEIGHTS(I)=TEMP
                  TEMPI=CLASS(J)
                  CLASS(J)=CLASS(I)
                  CLASS(I)=TEMPI
               END IF
            END DO
         END DO
      END IF
C
      RETURN
      END
