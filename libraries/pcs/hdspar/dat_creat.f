      SUBROUTINE DAT_CREAT ( PARAM, TYPE, NDIMS, DIMS, STATUS )
*+
*  Name:
*     DAT_CREAT

*  Purpose:
*     Create a data structure component.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_CREAT ( PARAM, TYPE, NDIMS, DIMS, STATUS )

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*           Name of program parameter
*     TYPE=CHARACTER*(*) (given)
*           Type of HDS component. This may be a primitive type or a
*           structure
*     NDIMS=INTEGER (given)
*           Number of dimensions of the component
*     DIMS(*)=INTEGER (given)
*           Dimensions of the component
*     STATUS=INTEGER (given and returned)
*        Global status

*  Description:
*     An HDS data object is created, as specified by the character
*     string associated with the parameter, and the given type and 
*     dimensionality. If the object is a component of a structure,
*     the structure must already exist.

*  Algorithm:
*     The internal identifying number for the parameter is obtained, and
*     SUBPAR_CREAT called.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1984 (BDK)
*        Original 
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM          ! Name of program parameter

      CHARACTER*(*) TYPE           ! Type of the required component.
                                   ! primitives such as '_REAL' etc.
                                   ! or some freely-defined structure
                                   ! type

      INTEGER NDIMS                ! Number of dimensions of the component

      INTEGER DIMS(*)              ! Dimensions of the component

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE             ! code number for parameter

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )

      CALL SUBPAR_CREAT ( NAMECODE, TYPE, NDIMS, DIMS, STATUS )

      END
