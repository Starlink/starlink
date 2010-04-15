      SUBROUTINE FK54Z( STATUS )
*+
*  Name:
*     FK54Z

*  Purpose:
*     Convert FK5 coordinates to FK4

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FK54Z( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a catalogue containing new fields for the Right Ascension
*     and Declination. The new fields are calculated using SLA_FK54Z.
*     See SUN 67
*
*     Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0 only
*     is provided.
*
*     If necessary use UPDATE to convert fields into the appropriate units.

*  Usage:
*     FK54Z INPUT OUTPUT RAFK5 DECFK5 BEPOCH RAFK4 DECFK4

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the output catalogue.
*     RAFK5 = _CHAR (Read)
*        Name of the RA field in FK5 system.
*     DECFK5 = _CHAR (Read)
*        Name of the DEC field in FK5 system.
*     BEBOCH = _REAL (Read)
*        Epoch
*     RAFK4 = _CHAR (Read)
*        Name of the RA field in FK4 system.
*     DECFK4 = _CHAR (Read)
*        Name of the DEC field in FK4 system.

*  Example:
*     FK54Z TEST TESTFK4 RA DEC BEPOCH RAB DECB

*  Notes:
*     This application creates a new catalogue that contains extra fields for
*     Right Ascension, Declination
*     The naming of these new fields can lead to confusion. Traditionally
*     the field names RA and DEC are used for the Right Ascension and
*     Declination fields in the catalogue. It is stongly suggested that the
*     field names of Right Ascension and Declination in the new system (FK5)
*     take the form RAB, DECB for RA Besslian and DEC Besslian. You may go on to
*     rename the field RA to RAJ and DEC to DECJ and RAB to RA and DECB to DEC
*     using the UPFIELD application but you must also then update the Parallax,
*     Radial velocity and proper motions and the catalogues Equinox parameter.
*
*     Care should also be taken when renaming fields. Renaming RAB to RA
*     before renaming RA to RAJ, in the above example, would result in two
*     fields named RA in the same catalogue.

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
      CHARACTER * ( CHP__SZNAME ) OUTCAT ! Name of new catalogue.
      CHARACTER * ( CHP__SZCNAME ) RAFK4 ! Name of the RA field in FK4 system.
      CHARACTER * ( CHP__SZCNAME ) DECFK4 ! Name of the DEC field in FK4 system.
      CHARACTER * ( CHP__SZCNAME ) RAFK5 ! Name of the RA field in FK4 system.
      CHARACTER * ( CHP__SZCNAME ) DECFK5 ! Name of the DEC field in FK5 system.
      REAL BEPOCH ! Bepoch required by SLALIB FK54Z

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)
*
*    Get the parameters
*
       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get0c('RAFK5', rafk5, status)
       call par_get0c('DECFK5', decfk5, status)
       call par_get0r('BEPOCH', bepoch, status)
       call par_get0c('RAFK4', rafk4, status)
       call par_get0c('DECFK4', decfk4, status)
*
*    Make the call.
*
       call chu_fk54z(incat, outcat, rafk5, decfk5, bepoch,
     :                rafk4, decfk4, status)
*
*    Inform the user of a succesful result.
*
       if (status .eq. SAI__OK) then
         call msg_setc('catname',outcat)
         call msg_out('message 1',
     :  'The catalogue ^catname has been produced', status)
       elseif (status .eq. CHP__CATNOTFND) then
         call msg_setc('catname',incat)
         call err_rep('message 2',
     : 'The catalogue ^catname could not be found.', status)
       elseif (status .eq. CHP__COLNOTFND) then
         call err_rep('message 3',
     : 'One of the fields given does not appear in the catalogue.',
     :  status)
       else
         call err_rep('message 4','An unidentified error ocurred in
     : CHP_FK54Z.', status)
       endif
*
       call chp_close(status)
*
      end
