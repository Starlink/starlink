C*PGSCH -- set character height
C%void cpgsch(float size);
C+
      SUBROUTINE PGSCH (SIZE)
      REAL SIZE
C
C Set the character size attribute. The size affects all text and graph
C markers drawn later in the program. The default character size is
C 1.0, corresponding to a character height about 1/40 the height of
C the view surface.  Changing the character size also scales the length
C of tick marks drawn by PGBOX and terminals drawn by PGERRX and PGERRY.
C
C Argument:
C  SIZE   (input)  : new character size (dimensionless multiple of
C                    the default size).
C--
C (1-Mar-1983)
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     XC, XCNEW, YC, XS, YS
C
      IF (PGNOTO('PGSCH')) RETURN
C
      CALL GRCHSZ(PGID, XC, YC, XS, YS)
      IF (PGXSZ(PGID)/PGXPIN(PGID) .GT.
     1    PGYSZ(PGID)/PGYPIN(PGID)) THEN
          XCNEW = SIZE*XC*PGYSZ(PGID)/YS/40.0
      ELSE
          XCNEW = SIZE*XC*(PGXSZ(PGID)*PGYPIN(PGID)/PGXPIN(PGID))
     1            /YS/40.0
      END IF
      CALL GRSETC(PGID,XCNEW)
      PGXSP(PGID) = XS*XCNEW/XC
      PGYSP(PGID) = YS*XCNEW/XC
      PGCHSZ(PGID) = SIZE
      END
