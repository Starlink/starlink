      SUBROUTINE HDELETE( STATUS )
*+
* Name:
*    HDELETE

* Purpose:
*    Delete an HDS object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hdelete inp

* ADAM Parameters:
*    INP = UNIV (Read)
*       Name of object to be deleted.

* Description :
*    Deletes a named HDS data object.  Everything below the level
*    of the object specified is also deleted.

* Examples:
*    % hdelete file1.data
*       Deletes component 'data' from the container file's top-level
*       structure.
*
*    % hdelete file1
*       Deletes container file file1.sdf

* Authors :
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)

* History :
*    ??-???-???? (RJV):
*       Original Version
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*    06-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Improve prologue
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*    Status :
      INTEGER STATUS

*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HDELETE Version 3.0-0')
*.

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Delete environment object
      CALL DAT_DELET( 'INP', STATUS )

      END
