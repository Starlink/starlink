*+  DYN_INIT - initialises DYN_ system
      SUBROUTINE DYN_INIT()
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
*    Import :
*    Import/Export :
*    Export :
*    External references :
*    Global variables :
      INCLUDE 'ASTLIB(DYN_CMN)'
*    Local Constants :
      INTEGER NULL
      PARAMETER (NULL=0)
*    Local variables :
      INTEGER N
*-
      DO N=1,DYN_NMAX
        LIST(N).PTR=NULL
        LIST(N).NITEM=NULL
        LIST(N).NPAGE=NULL
        LIST(N).NBYTE=NULL
        LIST(N).SECTION=.FALSE.
        LIST(N).LOC=' '
      ENDDO
      NPTR=NULL

      END
