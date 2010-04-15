      SUBROUTINE ADDPARAM( STATUS )
*+
*  Name:
*     ADDPARAM

*  Purpose:
*     Add a Parameter to a Catalogue

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ADDPARAM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Add a new parameter to a catalogue. The new parameter requires a
*     name, format, value and comment.

*  Usage:
*     ADDPARAM INPUT NAME FORMAT VALUE COMMENT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of the new parameter.
*     FORMAT = _CHAR (Read)
*        Format of the new parameter. A FORTRAN format detailing how the
*        parameter value should be interpreted.
*     VALUE = _CHAR (Read)
*        Value to be associated with the new parameter.
*     COMMENT = _CHAR (Read)
*        Comment to be associated with the new parameter.

*  Example:
*     ADDPARAM TEST CLASS I2 25 'Catalogue Class'

*  Notes:
*     The value of a parameter is always a character string. The format
*     determines how the value should be interpreted.

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
      INCLUDE 'CHP_PAR'          ! CHP constants
      INCLUDE 'CHP_ERR'          ! CHP errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name.
      CHARACTER * ( CHP__SZPNAME ) PNAME ! Name of the new parameter.
      CHARACTER * ( CHP__SZPFMT ) PFORMAT ! Format of the new parameter
      CHARACTER * ( CHP__SZPVAL ) PVALUE ! Value of the new parameter.
      CHARACTER * ( CHP__SZPCMT ) PCOMMENT ! Comment for the new parameter
      INTEGER PARRDIM( 7 )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', pname, status)
       call par_get0c('FORMAT', pformat, status)
       call par_get0c('VALUE', pvalue, status)
       call par_get0c('COMMENT', pcomment, status)
*
*   Make the call
*
       call chp_addp(incat,pname,pformat,'null',pcomment,.TRUE.,1,
     :  0,parrdim,pvalue,0, status)
*
       call chp_close(status)
       if (status .eq. SAI__OK) then
*
*    Display the results
*
         call msg_setc('catname',incat)
         call msg_setc('parname',pname)
         call msg_out('message 1',
     : 'The parameter ^parname has been added to the ^catname'//
     : ' catalogue', status)
       elseif (status .eq. CHP__CATNOTFND) then
         call msg_setc('catname',incat)
         call err_rep('message 2',
     : 'The catalogue ^catname could not be found.', status)
       elseif (status .eq. CHP__PAREXISTS) then
         call msg_setc('parname',pname)
         call msg_setc('catname',incat)
         call err_rep('message 3',
     : 'The parameter ^parname already exists in the catalogue' //
     : '^catname.', status)
       elseif (status .eq. CHP__IVLDPFMT) then
         call msg_setc('parfmat',pformat)
         call msg_setc('parval',pvalue)
         call err_rep('message 4',
     : 'The parameter format ^parfmat is not consistent with the '//
     :  'parameter value ^parval.', status)
       else
         call err_rep('message 5','An unidentified error ocurred in
     : ADDP.', status)
       endif
*
      end
