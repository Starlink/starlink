      SUBROUTINE sgs_CLRFG (IFLAG)
*+
*   - - - - - -
*    C L R F G
*   - - - - - -
*
*   Set the clear screen flag (RAL GKS dependent).
*
*   This function depends on an escape inserted to the RAL GKS by
*   Starlink.
*
*   Given:
*      IFLAG    i     flag state 0 => clear screen open.  Any other
*                     value => don't clear
*
*   Written to COMMON:
*      NCLORQ    l    no clear on open requested flag
*
*   D.L.Terrett   Starlink   11 December 1991
*-

      IMPLICIT NONE

      INTEGER IFLAG

      INCLUDE 'sgscom'



*  Set no clear open requested flag
      IF (IFLAG.EQ.0) THEN
         NCLORQ = .FALSE.
      ELSE
         NCLORQ = .TRUE. 
      END IF

      END
