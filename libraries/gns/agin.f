      SUBROUTINE gns_1AGIN (M,N,LOG,L)
*++
*   gns_1AGIN   Construct a AGI_n_m name string
*
*   Description:
*      A string of the form AGI_n_m is constructed and it and its length
*      returned
*
*   Input arguments:
*      N    i     Workstation type
*      M    i     Sequence number
*
*   Output arguments:
*      LOG  c*(*) The resulting name
*      L    i     the length of the name
*
*   Implicit inputs:
*      none
*
*   Implicit outputs:
*      none
*
*   External references:
*      none
*
*   Nick Eaton   2-DEC-1991
*++
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
       
