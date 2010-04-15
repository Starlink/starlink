      SUBROUTINE PARAMS( STATUS )
*+
*  Name:
*     PARAMS

*  Purpose:
*     Find the number and names of parameters in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PARAMS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find the number of parameters and the names of those parameters in
*     a catalogue.

*  Usage:
*     PARAMS INPUT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.

*  Example:
*     PARAMS TEST

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
      CHARACTER * ( CHP__SZPNAME ) PARAMNAMES(CHP__NUMPARS) ! Parameter
                                                            ! names
      INTEGER ICOUNT
      INTEGER NUMPARAMS

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
         call chp_getpnn(incat, paramnames, numparams, status)
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_seti('nump',numparams)
           call msg_out('message 1','There are ^nump parameters in the
     : ^catname catalogue', status)
           call msg_out('message 2','The parameters are ',status)
           do icount = 1, numparams
             call msg_setc('parname',paramnames(icount))
             call msg_out('message 3','^parname',status)
           enddo
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 3','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : PARAMS.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
