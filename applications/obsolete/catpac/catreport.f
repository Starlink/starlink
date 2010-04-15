      SUBROUTINE CATREPORT( STATUS )
*+
*  Name:
*     CATREPORT

*  Purpose:
*     Produce a catalogue report.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATREPORT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Produces a catalogue report. A simple report is produced with or
*     without a header, selecting fields or reporting all fields. The report
*     is made either to the screen or to a file ($<$catalogue name$>$.REP),

*  Usage:
*     CATREPORT TEST [HEADER] [SCREEN] [ALLFLDS] [FLDNAMES]

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     HEADER = _LOGICAL (Read)
*        Add header information to the report T/F?
*        [TRUE]
*     SCREEN = _LOGICAL (Read)
*        Output to the screen (or a file) T/F?
*        [TRUE]
*     ALLFLDS = _LOGICAL (Read)
*        All fields to be reported T/F?
*        [TRUE]
*     FLDNAMES = _CHAR (Read)
*        List of field names to be reported

*  Examples:
*     REPORT TEST HEADER=F SCREEN=F ALLFLDS=N FLDNAMES=[RA,DEC,VALUE1]
*
*        Produce a report with no header, output to a file and selecting
*        fields.
*

*  Notes:
*     A report to the screen is limited to 80 characters and to file 132
*     characters. Excess fields are ignored and a warning issued.
*     Requested fields that do not appear in the catalogue are ignored.

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
      LOGICAL HEADER ! Header required flag.
      LOGICAL SCREEN ! Output to screen flag.
      LOGICAL ALL ! All fields flag
      CHARACTER * ( CHP__SZCNAME ) FNAMES(CHP__NUMCOLS) ! Field names
      INTEGER NUMFLDS !Number of fields.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0l('HEADER', header, status)
       call par_get0l('SCREEN', screen, status)
       call par_get0l('ALLFLDS', all, status)
*
       if (.not. all) then
          call par_get1c('FLDNAMES', chp__numcols, fnames, numflds,
     :                    status)
       endif
*
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chu_catrep(incat, header, screen, fnames, numflds,
     : all, status)
*
*   Dispaly the result.
*
         if (status .eq. SAI__OK) then
           if (.not. screen) then
             call msg_setc('name', incat)
             call msg_out('message 1','The file ^name .REP contains the
     : report.', status)
           endif
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : CATREPORT.', status)
         endif
       endif
*
       call chp_close(status)
      end
