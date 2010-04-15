*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*      12 Apr 2004 (timj):
*        JCMT Only guarantees first 3 characters of VFRAMES to be
*        okay for matching.
*        Trap case where we do not recognize either VDEF or VFRAME
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
      CHARACTER VDEFS(3)*3
      CHARACTER VFRAMES(6)*4

      DATA      VDEFS   /'RAD',  'OPT',  'REL'/
      DATA      VFRAMES /'TOPO', 'LSR ', 'HELI',
     &                        'GEO ', 'BARY', 'TELL'/

*  Ok, go...

      VDEFT = VDEF(1:3)
      CALL UUCASE (VDEFT)

*  Only compare first 3 characters, not 4 (historically JCMT
*  only used first 3)
      VFRAMET = VFRAME(1:3)
      CALL UUCASE (VFRAMET)

CD    PRINT *, ' -- velencode --'
CD    PRINT *, '    vel frame = ', VFRAMET
CD    PRINT *, '    vel law   = ', VDEFT

      I = 1
      DO WHILE (I.LE.3 .AND. VDEFT.NE.VDEFS(I))
        I = I + 1
      END DO

      IF ( I .GT. 3 ) THEN
         print *,'Did not understand velocity definition string: ',
     +    VDEFT
         print *,'Assuming RADIO'
         I = 1
      END IF

      J = 1
      DO WHILE (J.LE.6 .AND. VFRAMET.NE.VFRAMES(J)(1:3))
        J = J + 1
      END DO
CD    print *,'J for frame = ',J,' and I', I

*     Equate BARYCENTRIC with GEOCENTRIC
      IF (J.EQ.5) J = 4
*     Equate TELLURIC with TOPOCENTRIC
      IF (J.EQ.6) J = 1

      IF (J .GT. 6 ) THEN
         print *,'Internal error. Did not recognize velocity frame: ',
     +        VFRAMET
         print *,'Assuming LSR'
         J = 2
      END IF

      LSRFLG = 16*(I-1) + (J-1)

CD    PRINT *, '    output value of LSRFLG', LSRFLG

      RETURN
      END

*-----------------------------------------------------------------------
