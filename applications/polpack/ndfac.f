      SUBROUTINE NDFAC( NAME, ACCESS, MINVAL, MAXVAL, NNDF, STACK, 
     :                  STATUS )
*+
*  Name:
*     NDFAC

*  Purpose:
*     To access a group of NDFs using GRP/NDG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDFAC( NAME, ACCESS, MINVAL, MAXVAL, NNDF, STACK, STATUS )

*  Description:
*     The routine accesses a group of NDFs whose parameter is
*     given by 'NAME'. The NDF identifiers are written to the array
*     STACK. Between MAXVAL and MINVAL entries are made to the stack.
*     The actual number of NDFs is returned is NNDF. The NDF locators
*     are not annulled by using this routine and should be by calling
*     NDF_ANNUL (or NDF_END), before the application exits.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The parameter name.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access method used to get the NDFs. Should be 'READ' or
*        'UPDATE'.
*     MAXVAL = INTEGER (Given)
*        The maxiumum number of NDFs allowed.
*     MINVAL = INTEGER (Given)
*        The minimum number of NDFs allowed.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user (less than
*        MAXNDF).
*     STACK( MAXNDF ) = INTEGER (Returned)
*        The stack of NDF identifiers returned from user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1997 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER NAME * ( * )
      CHARACTER ACCESS * ( * )
      INTEGER MAXVAL
      INTEGER MINVAL

*  Arguments Returned:
      INTEGER STACK( MAXVAL )
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP1              ! GRP group identifier
      INTEGER INDEX              ! Current index into NDF group

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group containing the names of the object frames to be used.
      CALL RDNDF( NAME, MAXVAL, MINVAL, '  Give more image names...', 
     :            IGRP1, NNDF, STATUS )

*  Get all the NDF identifiers. 
      DO INDEX = 1, NNDF
         CALL NDG_NDFAS( IGRP1, INDEX, ACCESS, STACK( INDEX ), STATUS )
      END DO

*  Delete the group holding the NDF names. 
      CALL GRP_DELET( IGRP1, STATUS )

      END
