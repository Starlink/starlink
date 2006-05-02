      SUBROUTINE gns_1AGIN (M,N,LOG,L)
*+
*  Name:
*     gns_1AGIN

*  Purpose:
*     Construct a AGI_n_m name string

*  Language:
*     Starlink Fortran 77

*  Description:
*     A string of the form AGI_n_m is constructed and it and its length
*     returned

*  Arguments:
*     N = INTEGER (Given)
*         Workstation type
*     M = INTEGER (Given)
*         Sequence number
*     LOG = CHAR (Returned)
*         The resulting name
*     L = INTEGER (Returned)
*         The length of the name

*  Authors:
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     2-DEC-1991 (NE):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External References:
*     none

*  Implicit Inputs:
*     none

*  Implicit Outputs:
*     none

*-
      IMPLICIT NONE

      INTEGER M, N, L
      CHARACTER *(*) LOG

      CHARACTER*10 CM, CN
      INTEGER I,J

      WRITE (UNIT=CM, FMT='(SP,I10)') M
      I = INDEX (CM,'+')
      WRITE (UNIT=CN, FMT='(SP,I10)') N
      J = INDEX (CN,'+')
      LOG = 'AGI_'//CM(I+1:)//'_'//CN(J+1:)

      L = 4 + (LEN(CM)-I) + 1 + (LEN(CN)-J)

      END
       
