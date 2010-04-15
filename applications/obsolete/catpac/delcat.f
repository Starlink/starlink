      SUBROUTINE DELCAT( STATUS )
*+
*  Name:
*     DELCAT

*  Purpose:
*     Deletes a catalogue from the system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DELCAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Delete a catalogue from the system.

*  Usage:
*     DELCAT INPUT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.

*  Example:
*     DELCAT TEST

*  Notes:
*     Catalogues not created by the user may be protected and in these
*     cases the Delete Catalogue application will have no effect.

*  Authors:
*     ARW: Alan Wood (STARLINK)

*  History:
*     11-OCT-1991 (ARW):
*        Original version.

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'          ! CHP Constants
      INCLUDE 'CHP_ERR'          ! CHP Errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)

       call par_get0c('INPUT', incat, status)
*
       if (status .eq. SAI__OK) then
*
*   Make the call
*
         call chp_delcat(incat, status)
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_out('message 1','The catalogue ^catname has been
     : deleted', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 3','An unidentified error ocurred in
     : DELCAT.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
