*+  HDELETE - deletes HDS data object
      SUBROUTINE HDELETE( STATUS )

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
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HDELETE Version 1.8-0')
*-

      CALL MSG_OUT('VERSION',VERSION,STATUS)

*    Delete environment object
      CALL USI_DELET( 'INP', STATUS )

      END
