*+  DTASK_DUMPAST - Dummy for Unix: AST routine for generating a task stack dump
      SUBROUTINE DTASK_DUMPAST ( ASTPARM )
*    Description :
*     Generate a stack dump of a task, then reenable the dumping 
*     facility.
*    Invocation :
*     An AST routine, invoked by VMS.
*    Parameters :
*     ASTPARM=INTEGER (given)
*           the AST parameter. Unused.
*    Method :
*     Call LIB$SIGNAL to generate a stack dump. Then call DTASK_SETDUMP 
*     to reenable the AST.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.08.1986:  Original (REVAD::BDK)
*     27.08.1986:  use DTASK__DUMP in the signal (REVAD::BDK)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'

*    Import :
      INTEGER ASTPARM     ! the AST parameter.

*    Local variables :
      INTEGER STATUS
*-

!      CALL LIB$SIGNAL ( %VAL(DTASK__DUMP) )

      STATUS = SAI__OK
      CALL DTASK_SETDUMP ( STATUS )

      END
