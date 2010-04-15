C+
      SUBROUTINE ECH_ARDELE(IORDR,NEWNL,NLMAX,ORDER,CHANS,WAVES,
     :                                       WEIGHTS,CLASS,NLID)
C
C     E C H _ A R D E L E
C
C     Searches through the array ORDER(1..NLID) looking for matches
C     to IORDR, and deletes all lines associated with IORDR from the
C     arc line data tables ORDER, CHANS, WAVES, WEIGHTS, and CLASS.
C     Returns a reduced value for NLID.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) IORDR    (Integer) The order number whose lines are
C                  to be deleted.
C     (>) NEWNL    (Integer) The expected number of such lines.
C                  May be passed as zero or negative if not known.
C     (>) NLMAX    (Integer) The dimension of the line tables.
C     (!) ORDER    (Integer array ORDER(NLMAX)) Order numbers for
C                  identified arc lines.
C     (!) CHANS    (Real array CHANS(NLMAX)) Channel numbers for
C                  identified arc lines.
C     (!) WAVES    (Real array WAVES(NLMAX)) Wavelengths of the
C                  identified arc lines.
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (!) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (!) NLID     (Integer) The number of identified arc lines.
C
C                                            JKM / CIT 12th Dec 1986
C
C     Modified:
C
C        8th Dez. 1987 - JKM / ESO.  NEWNL<=0 allowed if it is not known
C                        what number of deleted lines to expect.
C       30th June 1988 - WFL / AAO.  Correct INAROW code to cope with cases
C                        where there are disjoint sets of lines for the
C                        same order.
C
C+
      IMPLICIT NONE
C
C     Parameters  -
C
      INTEGER IORDR,NEWNL,NLMAX,NLID
      INTEGER ORDER(NLMAX),CLASS(NLMAX)
      REAL CHANS(NLMAX),WAVES(NLMAX),WEIGHTS(NLMAX)
C
C     Local variables
C
      INTEGER NORIG,I,IX,INAROW,STATUS
C
C     Set original number of lines aside for later comparison
C
      NORIG=NLID
C
C     Now start deleting NEWNL lines matching IORDR.
C
      IF (NLID.EQ.NEWNL) THEN
C
C        Entire old array was matched, so it all gets deleted
C
         NLID=0
C
      ELSE
C
C        Need to search for array elements to be deleted.
C
         DO IX=1,NLID,1
C
            IF ((ORDER(IX).EQ.IORDR).AND.(IX.LE.NLID)) THEN
C
C              Found at least one to delete ... if it's the last
C                 element of the array, delete it and quit.
C
               IF (IX.EQ.NLID) THEN
                  NLID=NLID-1
               ELSE
C
C                 Need to check for several deletions in a row
C
                  INAROW=1
                  I=IX+1
                  DO WHILE (I.LE.NLID.AND.ORDER(I).EQ.IORDR)
                     INAROW=INAROW+1
                     I=I+1
                  END DO
C
C                 Now can delete INAROW elements all at once.
C
                  IF ((NLID-IX).EQ.(INAROW-1)) THEN
C
C                    Can delete all remaining elements
C
                     NLID=IX-1
C
                  ELSE
C
C                    Some elements from end must be saved
C
                     DO I=IX,NLID-INAROW
                        ORDER(I)=ORDER(I+INAROW)
                        CHANS(I)=CHANS(I+INAROW)
                        WAVES(I)=WAVES(I+INAROW)
                        WEIGHTS(I)=WEIGHTS(I+INAROW)
                        CLASS(I)=CLASS(I+INAROW)
                     END DO
                     NLID=NLID-INAROW
                  END IF
               END IF
            END IF
         END DO
      END IF
C
C     Check to make sure the expected number of lines were found.
C
      IF ((NLID.NE.(NORIG-NEWNL)).AND.(NEWNL.GT.0)) THEN
         CALL PAR_WRUSER('***ECH_ARDELE WARNING: LINELIST PROBLEM',
     :                                                    STATUS)
      END IF
C
      RETURN
      END
