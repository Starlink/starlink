*+  HRETYPE - changes type of an HDS data object
      SUBROUTINE HRETYPE(STATUS)

*    Description :

*    Parameters :

*     INP  = CHAR  - the name of the object
*     TO   = CHAR - new type
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*     12 Jun 1989 V1.0-1 (RJV):
*        Now checks for primitive input
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     18 Jan 1996 V2.0-0 (DJA):
*        New USI routine
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRETYPE Version 2.2-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZTYP) TYPE
      INTEGER	FID
      LOGICAL STRUC
*-
      CALL MSG_PRNT(VERSION)

*    Start ASTERIX
      CALL AST_INIT()

      CALL USI_ASSOC( 'INP', '*', 'UPDATE', FID, STATUS )
      CALL ADI1_GETLOC( FID, LOC, STATUS )

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_STRUC(LOC,STRUC,STATUS)

        IF (STRUC) THEN
          CALL USI_GET0C('TO',TYPE,STATUS)
          CALL DAT_RETYP(LOC,TYPE,STATUS)
        ELSE
          CALL ERR_REP( ' ', 'Cannot retype primitive object', STATUS )
        ENDIF

      ENDIF

      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END
