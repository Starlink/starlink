      SUBROUTINE HRETYPE(STATUS)
*+
* Name:
*    HRETYPE

* Purpose:
*    Change the type of an HDS structure object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Description :
*    Changes the type of an HDS structure object - the type of a primitive
*    object cannot be changed.

* Usage:
*    hretype inp newtype

* ADAM Parameters :
*    INP  = UNIV (Read)
*       The name of the object. <GLOBAL.HDSOBJ>
*    TYPE = CHAR (Read)
*       New type - a valid HDS non-primitive type.

* Examples:
*    % hretype cfile.structure data_array
*       Sets the type of cfile.structure to DATA_ARRAY
*
*    % hretype cfile.structure.data _REAL
*       Error - cannot change the type of a primitive object.

* Method :
*    CALLS DAT_RETYPE

* Deficiencies :

* Bugs :

* Authors :
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)

* History :
*    ??-???-???? (RJV):
*       V1.0-0  Original Version
*     12-JUN-1989 (RJV):
*        V1.0-1 Now checks for primitive input
*     24-NOV-1994 (DJA):
*        V1.8-0 Now use USI for user interface
*     18-JAN-1996 (DJA):
*        V2.0-0 New USI routine
*     13-SEP-2001 (AJC):
*        V3.0-0 Remove Asterix stuff
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'

*    Import :

*    Import-export :

*    Export :

*    Status :
      INTEGER STATUS

*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRETYPE Version 3.0-0')

*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZTYP) TYPE
      LOGICAL STRUC
*.

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Print version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

      CALL DAT_ASSOC( 'INP', 'UPDATE', LOC, STATUS )

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_STRUC(LOC,STRUC,STATUS)

        IF (STRUC) THEN
          CALL PAR_GET0C('TYPE',TYPE,STATUS)
          CALL DAT_RETYP(LOC,TYPE,STATUS)
        ELSE
          CALL ERR_REP( ' ', 'Cannot retype primitive object', STATUS )
        ENDIF

      ENDIF

      END
