      SUBROUTINE sgs_ISLER (BLKER)
*+
*   - - - - - -
*    I S L E R
*   - - - - - -
*
*   Inquire whether the current workstation has a block erase capability
*
*   Returned:
*      BLKER     l      yes or no
*
*   Read from COMMON:
*      IBLCLR    i()    workstation description table - block erase
*                                                              mechanism
*      IZTW      i()    zone table - Workstation ID
*      ISZID     i      current zone ID
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      LOGICAL BLKER

      INCLUDE 'sgscom'




      BLKER = (IBLKCL(ABS(IZTW(ISZID))).GT.0)

      END
