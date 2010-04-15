C+
      SUBROUTINE ARC_ARLADD (CENT,WAVEL,NX,ZVALS,SIGMA,NLMAX,CHANS,
     :                        WAVES,WEIGHTS,CLASS,NLID,FORGET,REANAL)
C
C     A R L A D D
C
C     ARC utility.  Given an identified line, checks the list of
C     lines already identified and complains if a line of that
C     wavelength has already been identified.  If this is the case,
C     the user has the option of forgetting the current line, or
C     forgetting the original line, or re-analysing the line centers
C     of all the already identified lines, assuming the shift indicated
C     by this new identification.  This is for when the list of lines
C     already identified in fact came from another arc - one shifted
C     relative to the one actually being fitted.  If the line is
C     not already in the table, it is added in - unless the line
C     tables are full.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) CENT     (Real) The channel number for the line.
C     (>) WAVEL    (Real) The wavelength for the line.
C     (>) NX       (Integer) The number of pixels in the spectrum
C     (>) ZVALS    (Real array ZVALS(NX)) The arc data - needed in
C                  case the centers have to be re-analysed.
C     (>) SIGMA    (Real) The expected width for arc lines, in pixels.
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
C     (<) FORGET   (Logical) True if the line was ignored - either
C                  because there were too many lines, or because
C                  it matched a previous line.
C     (<) REANAL   (Logical) True if the line lists had to be reanalysed
C                  If both FORGET and REANAL are false, the line will
C                  have been added - a previous line that matched may
C                  have been deleted.
C
C                                           KS / CIT  13th June 1983
C     Modified:
C
C     05 Sep 1985  KS / AAO.  WEIGHTS and CLASS parameters added.
C     26 Jul 1993  HME / UoE, Starlink.  Disuse PAR_Q*.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL FORGET,REANAL
      INTEGER NX,NLMAX,NLID,CLASS(NLMAX)
      REAL CENT,WAVEL,ZVALS(NX),SIGMA,CHANS(NLMAX),WAVES(NLMAX)
      REAL WEIGHTS(NLMAX)
C
C     Functions -
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE,ICH_FOLD
C
C     Local variables
C
      LOGICAL ADD,REPLYOK,LINEOK
      INTEGER I,INVOKE,MATCH,NEXT,NLWAS,STATUS
      REAL CENEXP,CENTR,SHIFT,STRENGTH
      CHARACTER REPLY*64
C
C     Initial values
C
      REANAL=.FALSE.
      FORGET=.FALSE.
C
C     Check for too many lines
C
      IF (NLID.GE.NLMAX) THEN
         CALL PAR_WRUSER('Line tables full. Cannot add line',STATUS)
         FORGET=.TRUE.
      ELSE
C
C        Check for line already in table
C
         ADD=.TRUE.
         MATCH=0
         DO I=1,NLID
            IF (ABS(WAVES(I)-WAVEL).LT.0.05) THEN
               REPLY='Line already identified at channel '
               STATUS=ICH_ENCODE(REPLY,CHANS(I),36,5,NEXT)
               CALL PAR_WRUSER(REPLY(:NEXT),STATUS)
               MATCH=I
               GO TO 320
            END IF
         END DO
  320    CONTINUE
C
         IF (MATCH.NE.0) THEN
C
C           There is a match.  Find out what to do about it.  We can
C           delete the old line - in which case ADD will stay set - , or
C           reanalyse the tables - in which case REANAL will be set - ,
C           or we can ignore the new line.
C
            REPLY='Shift: re-analyse line centers assuming a '
            SHIFT=CENT-CHANS(MATCH)
            STATUS=ICH_ENCODE(REPLY,SHIFT,43,3,NEXT)
            REPLY(NEXT:)=' channel shift.'
            CALL PAR_WRUSER('You can resolve the double ' //
     :         'identification in one of three ways:',STATUS)
            CALL PAR_WRUSER('New: ignore the previous line,',STATUS)
            CALL PAR_WRUSER('Old: ignore this line,',STATUS)
            CALL PAR_WRUSER(REPLY,STATUS)
            CALL PAR_CNPAR('RESOLVE')
            CALL PAR_RDCHAR('RESOLVE','O',REPLY)
            INVOKE=ICH_FOLD(REPLY)
            IF (PAR_ABORT()) THEN
               RETURN
            ELSE IF (REPLY(:1).EQ.'N') THEN
               CALL ARDELE(MATCH,CHANS,WAVES,WEIGHTS,CLASS,NLID)
               REPLYOK=.TRUE.
            ELSE IF (REPLY(:1).EQ.'S') THEN
               REANAL=.TRUE.
               REPLYOK=.TRUE.
               ADD=.FALSE.
            ELSE
               ADD=.FALSE.
               REPLYOK=.TRUE.
            END IF
         END IF
C
C        Are we to re-analyse the lines?
C
         IF (REANAL) THEN
C
C           Yes, so first loop through the list trying to calculate
C           the new centroids.  Clear the graphics first.
C
            CALL PGADVANCE
C
            DO I=1,NLID
               CENTR=CHANS(I)+SHIFT
               CENEXP=CENTR
               CALL GEN_CENTROID(ZVALS,NX,SIGMA,CENTR,STRENGTH,STATUS)
               IF (STATUS.EQ.0) THEN
                  CHANS(I)=CENTR
               ELSE
                  REPLY='No line found at channel '
                  STATUS=ICH_ENCODE(REPLY,CENEXP,26,5,NEXT)
                  REPLY(NEXT:)=' Use line anyway?'
                  CALL PAR_WRUSER(REPLY,STATUS)
                  CALL PAR_CNPAR('LINEOK')
                  CALL PAR_RDKEY('LINEOK',.FALSE.,LINEOK)
                  IF (PAR_ABORT()) RETURN
                  IF (LINEOK) THEN
                     CHANS(I)=CENEXP
                  ELSE
                     CHANS(I)=0.
                  END IF
               END IF
            END DO
C
C           Then pass through the list backwards deleting any flagged lines
C
            NLWAS=NLID
            DO I=NLWAS,1,-1
               IF (CHANS(I).EQ.0) THEN
                  CALL ARC_ARDELE(I,CHANS,WAVES,WEIGHTS,CLASS,NLID)
               END IF
            END DO
         END IF
C
C        Are we to add the new line?  Note that this is the usually
C        executed part of this routine - the rest is all exceptions.
C
         IF (ADD) THEN
            NLID=NLID+1
            CHANS(NLID)=CENT
            WAVES(NLID)=WAVEL
            WEIGHTS(NLID)=1.0
            CLASS(NLID)=0
         END IF
C
      END IF
C
      RETURN
      END
