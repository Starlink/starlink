      SUBROUTINE CATRENAME( STATUS )
*+
*  Name:
*     CATRENAME

*  Purpose:
*     Rename a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATRENAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Rename a catalogue to a new name.

*  Usage:
*     CATRENAME INPUT NEWNAME

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NEWNAME = _CHAR (Read)
*        New name of the catalogue.

*  Example:
*     CATRENAME TEST NEWTEST

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
      CHARACTER * ( CHP__SZNAME ) NEWCAT ! New catalogue name.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*
       call chp_open(status)
*
*    Get the parameters
*
       call par_get0c('INPUT', incat, status)
       call par_get0c('NEWNAME', newcat, status)
*
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chp_rename(incat, newcat, status)

         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_setc('newname',newcat)
           call msg_out('message 1','The catalogue ^catname has been '//
     : 'renamed as ^newname', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 3','An unidentified error ocurred in
     : CATRENAME.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
