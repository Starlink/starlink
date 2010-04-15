      SUBROUTINE CATSEARCH( STATUS )
*+
*  Name:
*     CATSEARCH

*  Purpose:
*     Select entries from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATSEARCH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a new catalogue that contains entries from a catalogue
*     that pass some selection criteria. The criteria should be a legal
*     CATPAC Parser expression.

*  Usage:
*     CATSEARCH INPUT OUTPUT CRITERIA

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the catalogue to contain the selected entries.
*     CRITERIA = _CHAR (Read)
*        Logical expression of the selection criteria.

*  Example:
*     CATSEARCH TEST SEARCHTEST VALUE2.GT.300.AND.VALUE2.LT.500

*  Notes:
*     REJECT and CATSEARCH complement each other.

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
      CHARACTER * ( CHP__SZNAME ) OUTCAT ! Catalogue name for results
      CHARACTER * ( CHP__SZJEXP ) SEARCHEXP ! Search criteria

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)
*
*    Get parameter information.
*

       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get0c('CRITERIA', searchexp, status)
*
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chp_search(incat, outcat, searchexp, status)

         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catin',incat)
           call msg_setc('catname',outcat)
           call msg_out('message 1','The catalogue ^catname has been
     : created and contains those entries from ^catin that passed
     : the criteria.', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         elseif (status .eq. CHP__IVLDEXP) then
           call msg_setc('express',searchexp)
           call msg_setc('catname',incat)
           call err_rep('message 3','The expression ^express is invalid
     : check spelling.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : CATSEARCH.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
