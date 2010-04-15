      SUBROUTINE FK425( STATUS )
*+
*  Name:
*     FK425

*  Purpose:
*     Convert FK4 coordinates to FK5

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FK425( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a catalogue containing new fields for the Right Ascension,
*     Declination, Parallax, Radial velocity and proper motions after a
*     conversion has been made from the FK4 system coordinates. The
*     new fields are calculated using SLA_FK425. See SUN 67
*
*     Conversion from Besselian epoch 1950.0 to Julian epoch 2000.0 only
*     is provided. Proper motion corrections can be made using PROPERM.
*
*     Proper motions should be given in sec/yr and arcsecs/yr
*     Parallax should be given in arcseconds.
*     Radial velocity should be given in km/sec (+ve if receeding)
*
*     If necessary use UPDATE to convert fields into the appropriate units.

*  Usage:
*     FK425 INPUT OUTPUT RAFK4 DECFK4 RAPMFK4 DECPMFK4 PARLXFK4 RADVELFK4
*     RAFK5 DECFK5 RAPMFK5 DECPMFK5 PARLXFK5 RADELFK5

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the output catalogue.
*     RAFK4 = _CHAR (Read)
*        Name of the RA field in FK4 system.
*     DECFK4 = _CHAR (Read)
*        Name of the DEC field in FK4 system.
*     RAPMFK4 = _CHAR (Read)
*        Name of the RA proper motion field in FK4 system.
*     DECPMFK4 = _CHAR (Read)
*        Name of the DEC proper motion field in FK4 system.
*     PARLXFK4 = _CHAR (Read)
*        Name of the parallax field in FK4 system.
*     RADVELFK4 = _CHAR (Read)
*        Name of the radial velocity field in FK4 system.
*     RAFK5 = _CHAR (Read)
*        Name of the RA field in FK5 system.
*     DECFK5 = _CHAR (Read)
*        Name of the DEC field in FK5 system.
*     RAPMFK5 = _CHAR (Read)
*        Name of the RA proper motion field in FK5 system.
*     DECPMFK5 = _CHAR (Read)
*        Name of the DEC proper motion field in FK5 system.
*     PARLXFK5 = _CHAR (Read)
*        Name of the parallax field in FK5 system.
*     RADVELFK5 = _CHAR (Read)
*        Name of the radial velocity field in FK5 system.

*  Example:
*     FK425 TEST TESTFK5 RA DEC RA_PM DEC_PM PARALLAX RAD_VEL RAJ DECJ
*     PARALLAXJ RADVELJ

*  Notes:
*     This application creates a new catalogue that contains extra fields for
*     Right Ascension, Declination, Parallax, Radial velocity and proper
*     motions after a conversion to FK5 system.
*     The naming of these new fields can lead to confusion. Traditionally
*     the field names RA and DEC are used for the Right Ascension and
*     Declination fields in the catalogue. It is stongly suggested that the
*     field names of Right Ascension and Declination in the new system (FK5)
*     take the form RAJ, DECJ for RA Julian and DEC Julian. In the same way
*     PARLX would become PARLXJ and RADVEL RADVELJ etc. You may go on to
*     rename the field RA to RAB and DEC to DECB and RAJ to RA and DECJ to DEC
*     using the UPFIELD application but you must also then update the Parallax,
*     Radial velocity and proper motions and the catalogues Equinox parameter.
*
*     Care should also be taken when renaming fields. Renaming RAJ to RA
*     before renaming RA to RAB, in the above example, would result in two
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
      CHARACTER * ( CHP__SZCNAME ) RAPMFK4 ! Name of the RA proper motion FK4.
      CHARACTER * ( CHP__SZCNAME ) DECPMFK4 ! Name of the DEC proper motion FK4
      CHARACTER * ( CHP__SZCNAME ) PARLXFK4 ! Name of the parallax field FK4.
      CHARACTER * ( CHP__SZCNAME ) RADVELFK4 ! Name of the radial velocity FK4.
      CHARACTER * ( CHP__SZCNAME ) RAFK5 ! Name of the RA field in FK4 system.
      CHARACTER * ( CHP__SZCNAME ) DECFK5 ! Name of the DEC field in FK5 system.
      CHARACTER * ( CHP__SZCNAME ) RAPMFK5 ! Name of the RA proper motion FK5.
      CHARACTER * ( CHP__SZCNAME ) DECPMFK5 ! Name of the DEC proper motion FK5
      CHARACTER * ( CHP__SZCNAME ) PARLXFK5 ! Name of the parallax field FK5.
      CHARACTER * ( CHP__SZCNAME ) RADVELFK5 ! Name of the radial velocity FK5.
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)
*
*    Get the parameters
*
       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get0c('RAFK4', rafk4, status)
       call par_get0c('DECFK4', decfk4, status)
       call par_get0c('RAPMFK4', rapmfk4, status)
       call par_get0c('DECPMFK4', decpmfk4, status)
       call par_get0c('PARLXFK4', parlxfk4, status)
       call par_get0c('RADVELFK4', radvelfk4, status)
       call par_get0c('RAFK5', rafk5, status)
       call par_get0c('DECFK5', decfk5, status)
       call par_get0c('RAPMFK5', rapmfk5, status)
       call par_get0c('DECPMFK5', decpmfk5, status)
       call par_get0c('PARLXFK5', parlxfk5, status)
       call par_get0c('RADVELFK5', radvelfk5, status)
*
*    Make the call.
*
       call chu_fk425(incat, outcat, rafk4, decfk4, rapmfk4, decpmfk4,
     :   parlxfk4, radvelfk4, rafk5, decfk5, rapmfk5, decpmfk5,
     :   parlxfk5, radvelfk5, status)

       call chp_close(status)
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
     : CHP_FK425.', status)
       endif
*
*
      end
