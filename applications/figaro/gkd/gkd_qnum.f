C+
C                        G K D _ Q N U M
C
C  Routine name:
C     GKD_QNUM
C
C  Function:
C     Prompts for a numeric value during a graphics dialogue.
C
C  Description:
C     This routine prompts for a numeric value during an interactive
C     graphics dialogue using the alphanumeric terminal screen
C     controlled by the GKD_ package.  It is intended to be used
C     instead of PAR_QNUM during such dialogues.
C
C  Language:
C     FORTRAN
C
C  Call:
C     STATUS = GKD_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,UNITS,VALUE)
C
C  Parameters:      (">" input, "<" output)
C     (>) PROMPT    (Fixed string, descr) The prompt string for the
C                   number. This should be 'clean' - it should not have
C                   clever control characters, nor should it have
C                   the default value encoded in it.
C     (>) VMIN      (Real, ref) The minimum acceptable value.
C     (>) VMAX      (Real, ref) The maximum acceptable value.
C     (>) DEFAULT   (Real, ref) The default value - ignored if USEDEF
C                   is false.
C     (>) USEDEF    (Logical, ref) True if there is a default value -
C                   ie if a null response is to indicate that the
C                   value of DEFAULT is to be used.
C     (>) UNITS     (Fixed string, descr) The units of the value.
C     (<) VALUE     (Real, ref) The value obtained.  Will lie between
C                   VMIN and VMAX.
C
C  Returns:
C     (<) STATUS    (Logical, function value) True if there actually was a
C                   number entered, false for a null response.  Note that
C                   a null response is always accepted.
C
C  Author: Keith Shortridge, AAO
C
C  Date: 11th MArch 1988
C
C  External subroutines details:
C     PAR_QNUM   Prompts the user for a numeric value.
C-
      LOGICAL FUNCTION GKD_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,
     :                                               UNITS,VALUE)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL USEDEF
      REAL VMIN,VMAX,DEFAULT,VALUE
      CHARACTER*(*) PROMPT,UNITS
C
C     Functions
C
      LOGICAL PAR_QNUM
C
C     At the time of writing, there seems no reason not to just
C     use PAR_QNUM.  Time will tell..
C
      GKD_QNUM=PAR_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,UNITS,VALUE)
C
      END
