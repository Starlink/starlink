*-----------------------------------------------------------------------

      SUBROUTINE VELDECODE (LSRFLG, VFRAME, VDEF)

*  Routine to decode contents of 'LSRFLG' to produce velocity frame
*  and velocity law used to observe data: Must be consistent with
*  longstanding usage.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   LSRFLG
      CHARACTER VFRAME*(*)
      CHARACTER VDEF*(*)

*     Local variables:

      INTEGER   IVDEF
      INTEGER   IVFRAME
      CHARACTER VDEFS(3)*3   /'RAD',  'OPT',  'REL'/
      CHARACTER VFRAMES(4)*4 /'TELL', 'LSR ', 'HELI', 'GEO'/

*  Ok, go...

      IVDEF   = LSRFLG/16
      IVFRAME = LSRFLG - (16.*IVDEF)

      VDEF   = VDEFS(IVDEF+1)
      VFRAME = VFRAMES(IVFRAME+1)

D     TYPE *, ' -- veldecode --'
D     TYPE *, '    input value of LSRFLG', LSRFLG
D     TYPE *, '    vel frame = ', VFRAME
D     TYPE *, '    vel law   = ', VDEF

      RETURN
      END

*-----------------------------------------------------------------------
