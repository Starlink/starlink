      LOGICAL FUNCTION GEN_EXIST( NAME )
*+
*  Name:
*     GEN_EXIST

*  Purpose:
*     See if a files exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = GEN_EXIST( NAME )

*  Description:
*     This routine checks whether the specified file exists.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The specification of the file the existence of which is to be
*        checked.

*  Returned Value:
*     GEN_EXIST = LOGICAL
*        The returned value is true if the file exists, false otherwise.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26-AUG-1992 (HME):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) NAME

*.

      INQUIRE ( FILE = NAME, EXIST = GEN_EXIST )

      END
