C*PGASK -- control new page prompting
C%void cpgask(Logical flag);
C+
      SUBROUTINE PGASK (FLAG)
      LOGICAL FLAG
C
C Change the ``prompt state'' of PGPLOT. If the prompt state is
C ON, PGPAGE will type ``Type RETURN for next page:'' and will wait
C for the user to type a carriage-return before starting a new page.
C The initial prompt state (after the device has been opened) is ON
C for interactive devices. Prompt state is always OFF for
C non-interactive devices.
C
C Arguments:
C  FLAG   (input)  : if .TRUE., and if the device is an interactive
C                    device, the prompt state will be set to ON. If
C                    .FALSE., the prompt state will be set to OFF.
C--
C-----------------------------------------------------------------------
      INCLUDE     'pgplot.inc'
      LOGICAL     PGNOTO
      CHARACTER*1 TYPE
C
      IF (PGNOTO('PGASK')) RETURN
C
      IF (FLAG) THEN
          CALL GRQTYP(TYPE,PGPRMP(PGID))
      ELSE
          PGPRMP(PGID) = .FALSE.
      END IF
      END
