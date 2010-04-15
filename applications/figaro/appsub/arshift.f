C+
      SUBROUTINE ARSHIFT (NX,ZVALS,SIGMA,SHIFT,NLMAX,CHANS,
     :                                  WAVES,WEIGHTS,CLASS,NLID)
C
C     A R S H I F T
C
C     ARC utility.  Reanalyses the line list assuming a shift between
C     the arc data being used and the arc data from which the line
C     list was determined.  For each line in the line list, the
C     position is moved by the shift specified, and a search is made
C     for an arc line at that new position.  If no line is found, the
C     line is dropped from the list and this is noted.  This is similar
C     to what happens in ARLADD when a reanalyse is requested, but
C     provides no interactive option for keeping missing lines.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) NX       (Integer) The number of pixels in the spectrum
C     (>) ZVALS    (Real array ZVALS(NX)) The arc data.
C     (>) SIGMA    (Real) The expected width for arc lines, in pixels.
C     (>) SHIFT    (Real) The shift to be applied.
C     (>) NLMAX    (Integer) the maximum number of lines that can
C                  be held in the tables.
C     (!) CHANS    (Real array CHANS(NLMAX)) The channel numbers of
C                  the identified lines.
C     (!) WAVES    (Real array WAVES(NLMAX)) The wavelengths of the
C                  identified lines.
C     (!) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (!) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (!) NLID     (Integer) The number of lines identified.
C
C     Functions / subroutines used -
C
C     GEN_CENTROID (GEN_ package) Find arc line centroid.
C     ICH_ENCODE   (ICH_   "    ) Encode number into character string.
C     ARDELE                      Delete line form line lists.
C
C                                           KS / AAO 20th March 1991
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NLMAX,NLID,CLASS(NLMAX)
      REAL SHIFT,ZVALS(NX),SIGMA,CHANS(NLMAX),WAVES(NLMAX)
      REAL WEIGHTS(NLMAX)
C
C     Functions -
C
      INTEGER ICH_ENCODE
C
C     Local variables
C
      INTEGER I,NEXT,NLWAS,STATUS
      REAL CENEXP,CENTR,STRENGTH
      CHARACTER REPLY*64
C
C     Loop through the arc line list list trying to calculate
C     the new centroids.
C
      DO I=1,NLID
         CENTR=CHANS(I)+SHIFT
         CENEXP=CENTR
         CALL GEN_CENTROID(ZVALS,NX,SIGMA,CENTR,STRENGTH,STATUS)
         IF (STATUS.EQ.0) THEN
            CHANS(I)=CENTR
         ELSE
            REPLY='No line found at channel '
            STATUS=ICH_ENCODE(REPLY,CENEXP,26,2,NEXT)
            CALL PAR_WRUSER(REPLY,STATUS)
            CHANS(I)=0.
         END IF
      END DO
C
C     Then pass through the list backwards deleting any flagged lines
C
      NLWAS=NLID
      DO I=NLWAS,1,-1
         IF (CHANS(I).EQ.0) THEN
            CALL ARDELE(I,CHANS,WAVES,WEIGHTS,CLASS,NLID)
         END IF
      END DO
C
      END
