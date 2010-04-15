C+
C                        G K D _ Q U E S T
C
C  Routine name:
C     PAR_QUEST
C
C  Function:
C     Gets a YES/NO reply during an interactive graphics dialogue.
C
C  Description:
C     Gets a yes/no reply from an interactive user in response
C     to a prompt, using the alphanumeric text screen of the terminal
C     controlled by the GKD_ package.  This routine is intended as a
C     functional replacement for PAR_QUEST for use during an interactive
C     graphics dialogue.
C
C  Language:
C     FORTRAN
C
C  Call:
C     REPLY = GKD_QUEST (PROMPT,DEFAULT)
C
C  Parameters:      (">" input, "<" output)
C     (>) PROMPT    (Fixed string, descr) The prompt string.  Should
C                   be 'clean' in the sense that it should not
C                   include clever control characters, and should
C                   not have the default value formatted in it.
C                   It looks best if it ends with a '?'
C     (>) DEFAULT   (Logical, ref) The default value.  False => NO,
C                   True => YES.
C  Returns:
C     (<) REPLY     (Logical, function value) Set to true if the reply
C                   was YES, to false if it was NO.
C
C  Author: Keith Shortridge, AAO
C
C  Date: 11th March 1988
C
C  Details of subroutines / functions used -
C     PAR_QUEST   Get a yes/no answer in response to a prompt.
C-
      LOGICAL FUNCTION GKD_QUEST (PROMPT,DEFAULT)
C
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL DEFAULT
      CHARACTER*(*) PROMPT
C
C     Functions -
C
      LOGICAL PAR_QUEST
C
C     There isn't any need to be clever here.  PAR_QUEST should
C     work perfectly well, so we use it.  (But there may arise a
C     situation when it doesn't, and we'll be glad we have this
C     wrap_up.)
C
      GKD_QUEST = PAR_QUEST (PROMPT,DEFAULT)
C
      END
