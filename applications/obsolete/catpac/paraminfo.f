      SUBROUTINE PARAMINFO( STATUS )
*+
*  Name:
*     PARAMINFO

*  Purpose:
*     Find specific information about a parameter in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PARAMINFO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Finds the FORMAT, VALUE or COMMENT associated with a field in a
*     catalogue. For example find the VALUE associated with the
*     parameter AUTHOR in the TEST catalogue.

*  Usage:
*     PARAMINFO TEST AUTHOR VALUE

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of parameter.
*     INFREQ = _CHAR (Read)
*        Information required.(FORMAT, VALUE or COMMENT)

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
      CHARACTER * ( CHP__SZPNAME ) PNAME ! Parameter name
      CHARACTER * ( CHP__SZPFMT ) PFORMAT ! Parameter format
      CHARACTER * ( CHP__SZPUNIT ) PUNITS ! Parameter unit
      CHARACTER * ( CHP__SZPCMT ) PCOMMENT ! Parameter comment
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
      INTEGER PTRVAL
      CHARACTER * ( 7 ) PREQ ! Parameter information required.
      CHARACTER * ( CHP__SZPCMT ) INFO ! Information required.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', pname, status)
       call par_get0c('INFREQ', preq, status)
*
       if (status .eq. SAI__OK) then
*
*   Make the call
*
       call chp_gonepd(incat,pname,pformat,punits,pcomment,pprefdis,
     : ppartype, pardes,parrshp,parrdim,pmdataacc,pdataacc,pdatelm,
     : pdelind,pnsflag,status)

       call chp_gpdata(incat,pname,pformat, pardes,parrshp,parrdim,
     : charval, ptrval, status)

       if (preq .EQ. 'FORMAT') then
         info = pformat
       elseif (preq .EQ. 'VALUE') then
         info = charval
       elseif (preq .EQ. 'COMMENT') then
         info = pcomment
       else
         info = 'IVLDFREQ'
       endif
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_setc('paramname',pname)
           call msg_setc('paramreq',preq)
           call msg_setc('paraminfo',info)
           call msg_out('message 1','The information in the ^paramreq of
     : the parameter ^paramname in catalogue ^catname is ^paraminfo .'
     : , status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         elseif (status .eq. CHP__PARNOTFND) then
           call msg_setc('paramname',pname)
           call msg_setc('catname',incat)
           call err_rep('message 3','The parameter ^paramname was not found in
     : the catalogue ^catname.', status)
         elseif (status .eq. CHP__IVLDPREQ) then
           call msg_setc('paramireq',preq)
           call err_rep('message 4','The parameter information required ^paramireq
     : is not valid.', status)
           call msg_out('message 5','The valid options are FORMAT,
     : VALUE or COMMENT.', status)
         else
           call err_rep('message 5','An unidentified error ocurred in
     : PARAMINFO', status)
         endif
       endif
*
*   Cancel the parameters.
*
       call par_cancl('INPUT', status)
       call par_cancl('NAME', status)
       call par_cancl('INFREQ', status)
       call chp_close(status)
*
      end
