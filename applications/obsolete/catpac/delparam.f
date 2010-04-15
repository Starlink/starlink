      SUBROUTINE DELPARAM( STATUS )
*+
*  Name:
*     DELPARAM

*  Purpose:
*     Delete a Parameter from a Catalogue

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DELPARAM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Delete a parameter from a catalogue.

*  Usage:
*     DELPARAM INPUT NAME

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of the parameter to be deleted.

*  Example:
*     DELPARAM TEST CLASS

*  Authors:
*     ARW: Alan Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'          ! CHP constants
      INCLUDE 'CHP_ERR'          ! CHP errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name.
      CHARACTER * ( CHP__SZPNAME ) PNAME ! Name of the new parameter.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', pname, status)
*
       if (status .eq. SAI__OK) then
*
*   Make the call
*
         call chp_delpar(incat, pname, status)
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_setc('parname',pname)
           call msg_out('message 1','the parameter ^parname has been deleted
     : from the ^catname catalogue', status)
       elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
       elseif (status .eq. CHP__PARNOTFND) then
           call msg_setc('parname',pname)
           call msg_setc('catname',incat)
           call err_rep('message 3','The parameter ^parname was not found in
     : the catalogue ^catname.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : DELP.', status)
         endif
       endif
*
*
       call chp_close(status)
      end
