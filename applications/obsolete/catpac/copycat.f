      SUBROUTINE COPYCAT( STATUS )
*+
*  Name:
*     COPYCAT

*  Purpose:
*     Copy a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COPYCAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Copy a catalogue.

*
*  Usage:
*     COPYCAT INPUT OUTPUT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        New catalogue.

*  Example:
*     COPYCAT TEST NEWTEST

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
      CHARACTER * ( CHP__SZNAME ) OUTPUT ! New catalogue name.

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
       call par_get0c('OUTPUT', output, status)
*
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chp_copcat(incat, output, status)

         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_setc('newname',output)
           call msg_out('message 1','The catalogue ^catname has been '//
     : 'copied to ^newname', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 3','An unidentified error ocurred in
     : COPYCAT.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
