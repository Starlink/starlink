      BLOCK DATA IRM_BLDAT
*+
*  Name:
*     IRM_BLDAT

*  Purpose:
*     Initialises IRM_NPEN common block.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Initialises one of the IRM common blocks---IRM_NPEN---with a
*     logical value.

*  [optional_block_data_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 16 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'IRM_COM'          ! IRM_ common blocks
*        MCM_STDAT = LOGICAL (Write)
*          True means that the pen-characteristics variables have
*          already been set, otherwise not yet.

*  Global Data:
      DATA MCM_STDAT / .FALSE. /

*.

      END
