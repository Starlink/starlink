      SUBROUTINE HRENAME(STATUS)
*+
* Name:
*    HRENAME

* Purpose:
*    Rename an HDS data object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hrename inp to

* ADAM Parameters:
*    INP = UNIV (Read)
*       The object to be renamed.
*    TO = _CHAR (Read)
*       The new name - must be a valid HDS component name (not a pathname).

* Description:
*    The object is renamed.
*
*    Note that if INP is specified only as a container filename, the name of
*    the top-level object contained will be changed but not the name of the
*    file.
*
*    If the new name is too long, DAT__TRUNC is reported and a new value
*    requested.

* Examples:
*    % rename cfile.structure.data array
*       Component STRUCTURE.DATA becomes STRUCTURE.ARRAY
*
*    % rename cfile container
*       The top-level component of container file cfile.sdf (probably named
*       CFILE) is renamed to CONTAINER

* Method:
*    Uses subroutine DAT_RENAME.

* Deficiencies:

* Bugs:

* Authors:
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)

* History:
*    ??-???-???? (RJV):
*       V1.0-0  Original
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*     6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Improve error reporting
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
      PARAMETER (VERSION='HRENAME Version 3.0-0')

*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZNAM) NEW
*.

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Print version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

      CALL DAT_ASSOC('INP','UPDATE',LOC,STATUS)

      CALL PAR_GET0C('TO',NEW,STATUS)

      CALL DAT_RENAM(LOC,NEW,STATUS)

      END

