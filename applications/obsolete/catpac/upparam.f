      SUBROUTINE UPPARAM( STATUS )
*+
*  Name:
*     UPPARAM

*  Purpose:
*     Update a Parameter in a Catalogue

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL UPPARAM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Update a parameter in a catalogue. The parameter has a name
*     format, value and comment. Any of these can be updated
*     by setting the relevant flag

*  Usage:
*     UPPARAM INPUT NAME [NAMEFLG] [NEWNAME] [FORMFLG] [FORMAT]
*     [VALFLG] [VALUE] [COMFLG] [COMMENT]

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of the parameter.
*     NAMEFLG = _LOGICAL (Read)
*        Name flag. Do you want to update the parameter name T/F?
*        [FALSE]
*     NEWNAME = _CHAR (Read)
*        New name for the parameter.
*     FORMFLG = _LOGICAL (Read)
*        Format flag. Do you want to update the parameter format T/F?
*        [FALSE]
*     FORMAT = _CHAR (Read)
*        New format for the parameter. A FORTRAN format detailing how the
*        parameter value should be interpreted.
*     VALFLG = _LOGICAL (Read)
*        Value flag. Do you want to update the parameter value T/F?
*        [FALSE]
*     VALUE = _CHAR (Read)
*        New value to be associated with the parameter.
*     COMFLG = _LOGICAL (Read)
*        Comment flag. Do you want to update the parameter comment T/F?
*        [FALSE]
*     COMMENT = _CHAR (Read)
*        New comment to be associated with the parameter.

*  Example:
*     UPPARAM TEST CLASS FORMFLG=T FORMAT=I2

*  Notes:
*     The value of a parameter is always a character string. The format
*     determines how the value should be interpreted. An error will be reported
*     if the value and format become inconsistent.

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
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name
      CHARACTER * ( CHP__SZPNAME ) PNAME ! Parameter name
      CHARACTER * ( CHP__SZPNAME ) NEWNAME ! New Parameter name
      CHARACTER * ( CHP__SZPFMT ) PFORMAT ! Parameter format
      CHARACTER * ( CHP__SZPFMT ) NEWFORMAT ! New Parameter format
      CHARACTER * ( CHP__SZPUNIT ) PUNITS ! Parameter unit
      CHARACTER * ( CHP__SZPCMT ) PCOMMENT ! Parameter comment
      CHARACTER * ( CHP__SZPCMT ) NEWCOMMENT ! New Parameter comment
      LOGICAL PPREFDIS
      CHARACTER * ( 1 ) PPARTYPE
      INTEGER PARDES
      INTEGER PARRSHP
      INTEGER PARRDIM( 7 )
      LOGICAL PMDATAACC
      LOGICAL PDATAACC
      INTEGER PDATELM
      LOGICAL PDELIND
      LOGICAL PNSFLAG
      CHARACTER * ( CHP__SZCVAL ) CHARVAL
      CHARACTER * ( CHP__SZCVAL ) NEWCHARVAL
      INTEGER PTRVAL
      CHARACTER * ( 7 ) PREQ ! Parameter information required.
      CHARACTER * ( CHP__SZPCMT ) INFO ! Information required.
      LOGICAL NAMEFLG ! Name flag.
      LOGICAL FORMFLG ! Format flag.
      LOGICAL VALFLG ! Value flag.
      LOGICAL COMFLG ! Comment flag.
      LOGICAL UPDATEFLG ! Update flag.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)

       updateflg = .FALSE.
       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', pname, status)
       call par_get0l('NAMEFLG', nameflg, status)
       if (nameflg) then
         updateflg = .TRUE.
         call par_get0c('NEWNAME', newname, status)
       endif
       call par_get0l('FORMFLG', formflg, status)
       if (formflg) then
         updateflg = .TRUE.
         call par_get0c('FORMAT', newformat, status)
       endif
       call par_get0l('VALFLG', valflg, status)
       if (valflg) then
         updateflg = .TRUE.
         call par_get0c('VALUE', newcharval, status)
       endif
       call par_get0l('COMFLG', comflg, status)
       if (comflg) then
         updateflg = .TRUE.
         call par_get0c('COMMENT', newcomment, status)
       endif
*
*   Make the calls
*
*   Get the parameters details
*
       call chp_gonepd(incat,pname,pformat,punits,pcomment,pprefdis,
     : ppartype, pardes,parrshp,parrdim,pmdataacc,pdataacc,pdatelm,
     : pdelind,pnsflag,status)
*
*   Get the parameters value
*
       call chp_gpdata(incat,pname,pformat, pardes,parrshp,parrdim,
     : charval, ptrval, status)
*
*   Delete the parameter.
*
       call chp_delpar(incat, pname, status)
*
*   Update the details.
*
       if (nameflg) then
           pname = newname
       endif
       if (formflg) then
         pformat = newformat
       endif
       if (valflg) then
         charval = newcharval
       endif
       if (comflg) then
         pcomment = newcomment
       endif


       call chp_addp(incat,pname,pformat,'null',pcomment,.TRUE.,1,
     :  0,parrdim,charval,0, status)
*
       if (status .eq. SAI__OK) then
*
*    Display the results
*
         if ( .NOT. updateflg ) then
           call msg_setc('catname',incat)
           call msg_setc('parname',pname)
           call msg_out('message 1',
     : 'The parameter ^parname has not been updated in catalogue '//
     : '^catname because insufficient information on the changes '//
     : 'required was supplied.',
     :  status)
         else
           call msg_setc('catname',incat)
           call msg_setc('parname',pname)
           call msg_out('message 2',
     : 'The parameter ^parname has been updated in catalogue ^catname.',
     :  status)
         endif
       elseif (status .eq. CHP__CATNOTFND) then
         call msg_setc('catname',incat)
         call err_rep('message 3',
     : 'The catalogue ^catname could not be found.', status)
       elseif (status .eq. CHP__IVLDPFMT) then
         call err_rep('message 4',
     : 'The parameter format is not consistent with the '//
     :  'parameter value.', status)
       else
         call err_rep('message 5',
     : 'An unidentified error ocurred in UPPARARAM.', status)
       endif
*
       call chp_close(status)
      end
