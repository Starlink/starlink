      SUBROUTINE
     : CHP_GETNCD( INPUT, CD, STATUS )
*+
*  Name:
*     CHP_GETNCD

*  Purpose:
*     Get a new catalogue descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GETNCD( INPUT, CD, STATUS )
*
*  Description:
*     Not part of the CHI interface definition. Only use CHI_GETCD if you
*     are really sure about what is going on.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     CD = INTEGER (Returned)
*        Catalogue descriptor
*     STATUS = INTEGER
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-Aug-1992 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! CHP comon area

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      INTEGER CD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICOUNT
      LOGICAL OPENFLG
      LOGICAL NOTFOUND

*.

*  Check inherited global status.
*

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*
*    Find the next free element
*
          notfound = .true.
          icount = 0
          do while (icount .le. chp__mxass .and. notfound)
            icount = icount + 1
            if (opcatnames(icount) .eq. '9999') then
              cd = icount
              notfound = .false.
              opcatnames(icount) = input
            endif
          enddo
*
      END
