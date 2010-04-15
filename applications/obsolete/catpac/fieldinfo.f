      SUBROUTINE FIELDINFO( STATUS )
*+
*  Name:
*     FIELDINFO

*  Purpose:
*     Find specific information about a field in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FIELDINFO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Finds the FORMAT, UNITS, NULLVALUE or COMMENT associated with a
*     field in a catalogue. For example find the comment associated
*     with the field VALUE1 in the TEST catalogue.

*  Usage:
*     FIELDINFO INPUT NAME INFREQ

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of the field whose information is required.
*     INFREQ = _CHAR (Read)
*        Information required. (FORMAT, UNITS, NULLVALUE or COMMENT)

*  Example:
*     FIELDINFO TEST VALUE1 COMMENT

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
      CHARACTER * ( CHP__SZCNAME ) NAME ! Field name
      CHARACTER * ( 9 ) FREQ ! Field information required.
      CHARACTER * ( CHP__SZCCMT ) INFO ! Information required.
      CHARACTER * ( CHP__SZCFMT ) FORMAT
      CHARACTER * ( CHP__SZCUNIT ) UNITS
      CHARACTER * ( CHP__SZCCMT ) COMMENT
      LOGICAL PREFDIS
      CHARACTER * ( 1 ) COLTYPE
      INTEGER COLDES
      INTEGER ARRSHP
      INTEGER ARRDIM( 7 )
      LOGICAL ASSERT
      CHARACTER * ( CHP__SZEXP ) ASSEXP
      LOGICAL DOMCHK
      LOGICAL MDATAACC
      LOGICAL DATAACC
      LOGICAL VCFLAG
      INTEGER DATELM
      CHARACTER * ( CHP__SZEXP ) VCEXP
      LOGICAL DELIND
      LOGICAL NSFLAG
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', name, status)
       call par_get0c('INFREQ', freq, status)
*
       if (status .eq. SAI__OK) then
*
*   Make the call
*
       call chp_gonecd(incat,name,format,units,comment,prefdis,
     : coltype, coldes,arrshp,arrdim,assert, assexp,domchk,mdataacc,
     : dataacc,datelm,vcflag,vcexp,delind,nsflag,status)

       if (freq .EQ. 'FORMAT') then
         info = format
       elseif (freq .EQ. 'UNITS') then
         info = units
       elseif (freq .EQ. 'COMMENT') then
         info = comment
       else
         info = 'IVLDFREQ'
       endif
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_setc('fieldname',name)
           call msg_setc('fieldireq',freq)
           call msg_setc('fieldinfo',info)
           call msg_out('message 1','The value of the ^fieldireq for field
     : ^fieldname in catalogue ^catname is ^fieldinfo .', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         elseif (status .eq. CHP__COLNOTFND) then
           call msg_setc('fieldname',name)
           call msg_setc('catname',incat)
           call err_rep('message 3','The field ^fieldname was not found in
     : the catalogue ^catname.', status)
*         elseif (status .eq. CHP__IVLDFREQ) then
*           call msg_setc('fieldireq',freq)
*           call err_rep('message 4','The field information required ^fieldireq
*     : is not valid.', status)
*           call msg_out('message 5','The valid options are FORMAT, UNITS,
*     : NULLVALUE or COMMENT.', status)
         else
           call err_rep('message 5','An unidentified error ocurred in
     : GETFINFO.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
