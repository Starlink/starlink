*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE VELENCODE (VFRAME, VDEF, LSRFLG)

*  Routine to encode contents of 'LSRFLG' given velocity frame
*  and velocity law used to observe data: Must be consistent with
*  longstanding usage. 
*
*  The assumption is that the LO frequency offsets were made to
*  bring a source *at velocity VRAD* in the nominated frame to the
*  center of the spectrum.

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER VFRAME*(*)
      CHARACTER VDEF*(*)
      INTEGER   LSRFLG

*     Local variables:

      INTEGER   I, J
      CHARACTER VFRAMET*4
      CHARACTER VDEFT*3
      CHARACTER VDEFS(3)*3   /'RAD',  'OPT',  'REL'/
      CHARACTER VFRAMES(6)*4 /'TOPO', 'LSR ', 'HELI',
     &                        'GEO ', 'BARY', 'TELL'/

*  Ok, go...

      VDEFT = VDEF(1:3)
      CALL UUCASE (VDEFT)
      VFRAMET = VFRAME(1:4)
      CALL UUCASE (VFRAMET)

D     TYPE *, ' -- velencode --'
D     TYPE *, '    vel frame = ', VFRAMET
D     TYPE *, '    vel law   = ', VDEFT

      I = 1
      DO WHILE (I.LE.3 .AND. VDEFT.NE.VDEFS(I))
        I = I + 1
      END DO
        
      J = 1
      DO WHILE (J.LE.6 .AND. VFRAMET.NE.VFRAMES(J))
        J = J + 1
      END DO

*     Equate BARYCENTRIC with GEOCENTRIC
      IF (J.EQ.5) J = 4
*     Equate TELLURIC with TOPOCENTRIC
      IF (J.EQ.6) J = 1

      LSRFLG = 16*(I-1) + (J-1)

D     TYPE *, '    output value of LSRFLG', LSRFLG

      RETURN
      END

*-----------------------------------------------------------------------
