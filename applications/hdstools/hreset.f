      SUBROUTINE HRESET(STATUS)
*+
* Name:
*    HRESET

* Purpose:
*    Change state of a primitive HDS object to undefined.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Description:
*    The state of the specified object is set to 'undefined'. All subsequent
*    read operations will fail until the object is written to (re-defined).
*    An attempt to reset a slice of an object will reset the whole object,
*    an attempt to reset a structure object will have no effect.

* Usage:
*   hreset inp

* ADAM Parameters:
*   INP  = UNIV (Read)
*      The name of a primitive object.

* Examples:
*   % hreset cfile.data_array.data
*      Resets the DATA component of structure DATA_ARRAY in file cfile.
*
*   % hreset 'cfile.data_array.data(1:10,1:10)'
*      Resets the whole of object cfile.data_array.data
*
*   % hreset cfile.data_array
*      No effect if DATA_ARRAY is a structure

* Method:
*   Calls HDS subroutine DAT_RESET

* Deficiencies:

* Bugs:

* Authors:
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)

* History:
*    ??-???-???? (RJV):
*       V1.0-0  Original Version
*     24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*      6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'

*    Status :
      INTEGER STATUS

*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRESET Version 3.0-0')

*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
*.

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Print version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

      CALL DAT_ASSOC('INP','UPDATE',LOC,STATUS)

      CALL DAT_RESET(LOC,STATUS)

      END
