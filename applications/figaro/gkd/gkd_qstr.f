C+
C                        G K D _ Q S T R
C
C  Routine name:
C     PAR_QSTR
C
C  Function:
C     Prompts for a character string during a graphics dialogue.
C
C  Description:
C     This routine prompts for a character string during an interactive
C     graphics dialogue using the alphanumeric terminal screen
C     controlled by the GKD_ package.  It is intended to be used
C     instead of PAR_QSTR during such dialogues.
C
C  Language:
C     FORTRAN
C
C  Call:
C      CALL GKD_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C
C  Parameters:     (">" input, "<" output)
C     (>) PROMPT   (Fixed string, descr) See PAR_QSTR.
C     (>) DEFAULT  (Fixed string, descr) See PAR_QSTR.
C     (>) BLANKOK  (Logical, ref) See PAR_QSTR.
C     (>) FOLD     (Logical, ref) See PAR_QSTR.
C     (<) STRING   (Fixed string, descr) See PAR_QSTR.
C
C  Author: Keith Shortridge, AAO
C
C  Date: 11th MArch 1988
C
C  External subroutines / functions details:
C
C     PAR_QSTR   Get a character string from the user.
C-
      SUBROUTINE GKD_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL BLANKOK, FOLD
      CHARACTER *(*) PROMPT, STRING, DEFAULT
C
C     At the time of writing, there seems no reason not to just
C     use PAR_QSTR.  Time will tell..
C
      CALL PAR_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C
      END
