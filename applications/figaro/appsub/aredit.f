C+
      SUBROUTINE AREDIT(ARC1,ARC2,ARC3,ARCS,NLARCS,CHANS,
     :                                 WAVES,WEIGHTS,CLASS,NLID)
C
C     A R E D I T
C
C     Allows a little interactive editing of the identified line
C     list.  Lines cannot be added to the list, but they can be
C     removed, or their wavelengths can be changed.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) ARC1     (Real array ARC1(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARC2     (Real array ARC2(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARC3     (Real array ARC3(NLARCS)) The list of lines
C                  for the first of the possible arcs.
C     (>) ARCS     (Character) The arc types used.  This is the
C                  ARCTYPE parameter for the main ARC routine.
C     (>) NLARCS   (Integer) The dimension of the ARCn arrays.
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
C                                         KS / CIT 9th June 1983
C     Modified:
C
C     5th Sept 1985  KS / AAO.  WEIGHTS and CLASS parameter added.
C     20th Mar 1991  KS / AAO.  ICH_CF now used instead of ICH_ENCODE
C                    for wavelength values.
C     23rd Jul 1993  HME / UoE, Starlink.  Disuse PAR_Q*, use PAR_ABORT.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NLARCS,NLID,CLASS(NLID)
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID)
      REAL ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS)
      CHARACTER*(*) ARCS
C
C     Functions -
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN
      CHARACTER ICH_CF*16
C
C     Local variables
C
      LOGICAL EDIT,FORGET
      INTEGER LINE,NEXT,NLI,STATUS
      REAL VALUE,WAVEL
      CHARACTER REPLY*64
C
      EDIT=.TRUE.
      DO WHILE (EDIT)
         CALL PAR_CNPAR('LINENO')
         CALL PAR_RDVAL('LINENO',0.,FLOAT(NLID),1.,' ',VALUE)
         IF (PAR_ABORT()) RETURN
         EDIT=(VALUE.GE.1.)
         IF (EDIT) THEN
            LINE=ABS(VALUE)
            REPLY='Current wavelength is '//ICH_CF(WAVES(LINE))
            NEXT=ICH_LEN(REPLY)+1
            CALL PAR_WRUSER(REPLY(:NEXT-1),STATUS)
            CALL ARGETW(ARC1,ARC2,ARC3,ARCS,NLARCS,0.,0.,FORGET,
     :                                                     WAVEL)
            IF (PAR_ABORT()) RETURN
            IF (FORGET) THEN
C
C              Flag for deletion (don't change line numbers yet)
C
               WAVES(LINE)=-WAVES(LINE)
            ELSE
               WAVES(LINE)=WAVEL
            END IF
         END IF
      END DO
C
C     Now actually delete all lines flagged with -ve wavelengths
C     (note NLID is changed by ARDELE, so we don't use it as limit)
C
      NLI=NLID
      DO LINE=NLI,1,-1
         IF (WAVES(LINE).LT.0.) THEN
            CALL ARDELE(LINE,CHANS,WAVES,WEIGHTS,CLASS,NLID)
         END IF
      END DO
C
      END
