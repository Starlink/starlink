*+  HDELETE - deletes HDS data object

      SUBROUTINE HDELETE(STATUS)

*    Description :
*     Deletes a named HDS data object.  Everything below the level
*     of the object specified is also deleted
*    Parameters :
*     INP = CHAR  - the name of the object

*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
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
      PARAMETER (VERSION='HDELETE Version 1.0-0')
*    Local variables :
*-

      CALL MSG_OUT('VERSION',VERSION,STATUS)

*    simple wrap-up of HDS routine
      CALL DAT_DELET('INP',STATUS)

      END
