      SUBROUTINE sgs_CLRFG (IFLAG)
*+
*  Name:
*     CLRFG

*  Purpose:
*     Set the clear screen flag (RAL GKS dependent).

*  Language:
*     Starlink Fortran 77

*  Description:
*     This function depends on an escape inserted to the RAL GKS by
*     Starlink.

*  Arguments:
*     IFLAG = INTEGER (Given)
*         Flag state 0 => clear screen open.  Any other
*         value => don't clear

*  Authors:
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1991 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Written To Common:
*     NCLORQ    l    no clear on open requested flag

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
