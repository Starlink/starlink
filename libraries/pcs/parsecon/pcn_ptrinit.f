*+  PARSECON_PTRINIT - Initialise pointers etc. before parsing IFL
      SUBROUTINE PARSECON_PTRINIT ( STATUS )
*    Description :
*     Initialises pointers to the various storage lists 
*     and various names before the parsing of an interface file begins.
*    Invocation :
*     CALL PARSECON_PTRINIT ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Set the various pointers to zero.
*    Deficiencies :
*     Strictly, all the pointer arrays should be zeroed - eg PARDEF, 
*     PARDYN, PARASSOC, NEEDOB, NEEDCAN.....
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984:  Original (REVAD::BDK)
*     27.02.1985:  initialize PROGNAME and EXEPATH (REVAD::BDK)
*     23.08.1985:  initialize MONOLITH (REVAD::BDK)
*     16.08.1990:  initialize ACNAME and PRNAME for error reports (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      PARPTR = 0
      ACTPTR = 0
      NEEDPTR = 0
      INTPTR = 0
      REALPTR = 0
      DOUBLEPTR = 0
      CHARPTR = 0
      LOGPTR = 0
      PROGNAME = ' '
      EXEPATH = ' '
      MONOLITH = .FALSE.

*   Names in error reporting common block
      ACNAME = ' '
      PRNAME = ' '

      END
