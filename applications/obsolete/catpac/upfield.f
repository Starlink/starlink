      SUBROUTINE UPFIELD( STATUS )
*+
*  Name:
*     UPFIELD

*  Purpose:
*     Update Field information in a Catalogue

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL UPFIELD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Update the information associated with a field in a catalogue. The
*     field has a name, format, unit, null value and comment. Any of these
*     can be updated by setting the relevant flag

*  Usage:
*     UPFIELD INUPT NAME [NAMEFLG] [NEWNAME] [FORMFLG] [FORMAT]
*     [UNITFLG] [UNITS] [NULLFLG] [NULLVAL] [COMFLG] [COMMENT]

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     NAME = _CHAR (Read)
*        Name of the field.
*     NAMEFLG = _LOGICAL (Read)
*        Name flag. Do you want to update the field name T/F?
*        [FALSE]
*     NEWNAME = _CHAR (Read)
*        New name for the field.
*     FORMFLG = _LOGICAL (Read)
*        Format flag. Do you want to update the field format T/F?
*        [FALSE]
*     FORMAT = _CHAR (Read)
*        New format for the format. A FORTRAN format detailing how the
*        field value should be displayed.
*     UNITFLG = _LOGICAL (Read)
*        Unit flag. Do you want to update the field units T/F?
*        [FALSE]
*     UNITS = _CHAR (Read)
*        New units to be associated with the field.
*     NULLFLG = _LOGICAL (Read)
*        Null value flag. Do you want to update the field null value T/F?
*        [FALSE]
*     NULLVAL = _CHAR (Read)
*        New null value to be associated with the field.
*     COMFLG = _LOGICAL (Read)
*        Comment flag. Do you want to update the field comment T/F?
*        [FALSE]
*     COMMENT = _CHAR (Read)
*        New comment to be associated with the field.

*  Example:
*     UPFIELD TEST VALUE1 COMFLG=T COMMENT="Possible error + or - 2"

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
      LOGICAL NAMEFLG ! Name flag.
      LOGICAL FORMFLG ! Format flag.
      LOGICAL UNITFLG ! Unit flag.
      LOGICAL NULLFLG ! Null value flag.
      LOGICAL COMFLG ! Comment flag.
      LOGICAL UPDATEFLG ! Values to be updated.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)

       updateflg = .FALSE.
       call par_get0c('INPUT', incat, status)
       call par_get0c('NAME', name, status)
*
       call chp_gonecd(incat,name,format,units,comment,prefdis,
     : coltype, coldes,arrshp,arrdim,assert, assexp,domchk,mdataacc,
     : dataacc,datelm,vcflag,vcexp,delind,nsflag,status)

       call par_get0l('FORMFLG', formflg, status)
       if (formflg) then
         updateflg = .TRUE.
         call par_get0c('FORMAT', format, status)
       endif
       call par_get0l('UNITFLG', unitflg, status)
       if (unitflg) then
         updateflg = .TRUE.
         call par_get0c('UNITS', units, status)
       endif
       call par_get0l('COMFLG', comflg, status)
       if (comflg) then
         updateflg = .TRUE.
         call par_get0c('COMMENT', comment, status)
       endif
*
*   Make the call if there is somthing to update.
*
       if ( updateflg ) then
         call chp_repocd(incat, name, format, units, comment,
     :    prefdis, assert, assexp, domchk, status)
       endif
*
       if (status .eq. SAI__OK) then
*
*    Display the results
*
         if (.NOT. updateflg) then
           call msg_setc('catname',incat)
           call msg_setc('fldname',name)
           call msg_out('message 1',
     : 'The field ^fldname information has not been updated in '//
     : ' catalogue ^catname because insufficient data on the changes '//
     : 'required was supplied' ,
     :  status)
         else
           call msg_setc('catname',incat)
           call msg_setc('fldname',name)
           call msg_out('message 2',
     : 'The field ^fldname has been updated in catalogue ^catname',
     :  status)
         endif
       elseif (status .eq. CHP__CATNOTFND) then
         call msg_setc('catname',incat)
         call err_rep('message 3',
     : 'The catalogue ^catname could not be found.', status)
       else
         call err_rep('message 4',
     : 'An unidentified error ocurred in UPFIELD.', status)
       endif
*
       call chp_close(status)
      end
