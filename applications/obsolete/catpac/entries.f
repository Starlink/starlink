      SUBROUTINE ENTRIES( STATUS )
*+
*  Name:
*     ENTRIES

*  Purpose:
*     Finds the number of entries in a catalogue.

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
*     Finds the number of entries in a catalogue.

*  Usage:
*     ENTRIES INPUT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.

*  Example:
*     ENTRIES TEST

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
      INTEGER NUMENTS ! Number of entries in the catalogue.

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
         call chp_gnents(incat, numents, status)
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_seti('numentries',numents)
           call msg_out('message 1',
     : 'There are ^numentries entries in the ^catname catalogue',
     : status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2',
     : 'The catalogue ^catname could not be found.', status)
         else
           call err_rep('message 3',
     : 'An unidentified error ocurred in ENTRIES.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
