      SUBROUTINE FIELDS( STATUS )
*+
*  Name:
*     FIELDS

*  Purpose:
*     Find the number and names of fields in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FIELDS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Find the number of fields and the names of thoses fields
*     in a catalogue.

*  Usage:
*     FIELDS INPUT

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.

*  Example:
*     FIELDS TEST

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
      INTEGER ICOUNT
      INTEGER NUMFLDS
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name
      CHARACTER * ( CHP__SZCNAME ) NAME(CHP__NUMCOLS) ! Field name
      CHARACTER * ( 9 ) FREQ ! Field information required.
      CHARACTER * ( CHP__SZCCMT ) INFO ! Information required.
      CHARACTER * ( CHP__SZCFMT ) FORMAT(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZCUNIT ) UNITS(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZCCMT ) COMMENT(CHP__NUMCOLS)
      LOGICAL PREFDIS(CHP__NUMCOLS)
      CHARACTER * ( 1 ) COLTYPE(CHP__NUMCOLS)
      INTEGER COLDES(CHP__NUMCOLS)
      INTEGER ARRSHP(CHP__NUMCOLS)
      INTEGER ARRDIM( CHP__NUMCOLS, 7 )
      LOGICAL ASSERT(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZEXP ) ASSEXP(CHP__NUMCOLS)
      LOGICAL DOMCHK(CHP__NUMCOLS)
      LOGICAL MDATAACC(CHP__NUMCOLS)
      LOGICAL DATAACC(CHP__NUMCOLS)
      LOGICAL VCFLAG(CHP__NUMCOLS)
      INTEGER DATELM(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZEXP ) VCEXP(CHP__NUMCOLS)
      LOGICAL DELIND(CHP__NUMCOLS)
      LOGICAL NSFLAG(CHP__NUMCOLS)

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
       call chp_gallcd(incat,numflds,name,format,units,comment,prefdis,
     : coltype, coldes,arrshp,arrdim,assert, assexp,domchk,mdataacc,
     : dataacc,datelm,vcflag,vcexp,delind,nsflag,status)
*
*    Display the results
*
         if (status .eq. SAI__OK) then
           call msg_setc('catname',incat)
           call msg_seti('numf',numflds)
           call msg_out('message 1','There are ^numf fields in the
     : ^catname catalogue', status)
           call msg_out('message 2','The fields are ',status)
           do icount = 1, numflds
             call msg_setc('fldname',name(icount))
             call msg_out('message 3','^fldname',status)
           enddo
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 3','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : FIELDS.', status)
         endif
       endif
*
       call chp_close(status)
      end
