      SUBROUTINE SAMPLE( STATUS )
*+
*  Name:
*     SAMPLE

*  Purpose:
*     Select every Nth entry from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SAMPLE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Sample a catalogue at frequency N creating a new catalogue to
*     contain the selected entries. By using the REJECT option a second
*     catalogue can be created that contains those entries that were not
*     selected.

*  Usage:
*     SAMPLE INPUT OUTPUT FREQUENCY [REJECT] [OUTREJECT]

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the catalogue to contain the sampled entries.
*     FREQUENCY = _INTEGER (Read)
*        Sample frequency N
*     REJECT = _LOGICAL (Read)
*        Do you require a rejects catalogue (T/F)?
*        [FALSE]
*     OUTREJECT = _CHAR (Read)
*        Name of the catalogue to contain the rejected entries.

*  Example:
*     SAMPLE TEST SAMPTEST 20

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
      CHARACTER * ( CHP__SZNAME ) REJCAT ! Catalogue name for rejects
      INTEGER FREQ ! Sample frequency
      LOGICAL REJECT ! Rejects catalogue reqiured flag.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*
       call chp_open(status)
*
*    Get the parameters
*
       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get0i('FREQUENCY', freq, status)
       call par_get0l('REJECT', reject, status)
       if (reject) then
         call par_get0c('OUTREJECT', rejcat, status)
       endif
*
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chu_sample(incat, outcat, reject, rejcat,
     :                freq, status)

         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catname',incat)
           call msg_seti('freqnum',freq)
           if (reject) then
             call msg_out('message 1','The table ^catname has been
     :  sampled at frequency ^freqnum', status)
             call msg_setc('outname',outcat)
             call msg_setc('rejname',rejcat)
             call msg_out('message 2','and the tables ^outname and
     : ^rejname were produced.', status)
             call msg_setc('outname',outcat)
             call msg_setc('rejname',rejcat)
             call msg_out('message 3','The table ^outname contains the
     : sampled rows and ^rejname the rejects', status)
           else
             call msg_out('message 4','The table ^catname has been
     :  sampled at frequency ^freqnum', status)
             call msg_setc('outname',outcat)
             call msg_out('message 5','and the table ^outname
     : was produced.', status)
           endif
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not
     : be found.', status)
         else
           call err_rep('message 3','An unidentified error ocurred in
     : CHP_SAMPLE.', status)
         endif
       endif
*
       call chp_close(status)
*
      end
