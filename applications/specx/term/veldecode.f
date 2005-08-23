*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
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
      CHARACTER VDEFS(3)*3
      CHARACTER VFRAMES(4)*4

      DATA      VDEFS   /'RAD',  'OPT',  'REL'/
      DATA      VFRAMES /'TELL', 'LSR ', 'HELI', 'GEO'/

*  Ok, go...

      IVDEF   = LSRFLG/16
      IVFRAME = LSRFLG - (16.*IVDEF)

      IVDEF = IVDEF + 1
      IVFRAME = IVFRAME + 1
      IF (IVDEF .LT. 1 .OR. IVDEF .GT. 3) THEN
         print *,'** veldecode: Error decoding velocity definition',
     :        ' - assuming RADIO'
         IVDEF = 1
      END IF

      IF (IVFRAME .LT. 1 .OR. IVFRAME .GT. 4) THEN
         print *,'** veldecode: Error decoding velocity frame',
     :        ' - assuming LSR'
         IVDEF = 2
      END IF

      VDEF   = VDEFS(IVDEF)
      VFRAME = VFRAMES(IVFRAME)

CD    PRINT *, ' -- veldecode --'
CD    PRINT *, '    input value of LSRFLG', LSRFLG
CD    PRINT *, '    vel frame = ', VFRAME
CD    PRINT *, '    vel law   = ', VDEF

      RETURN
      END

*-----------------------------------------------------------------------
