      SUBROUTINE PROPERM( STATUS )
*+
*  Name:
*     PROPERM

*  Purpose:
*     Apply proper motion correction to a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROPERM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a catalogue containing new fields for the new Right Ascension and
*     Declination after the correction has been made for proper motion.
*     Calculated using SLA_PM. See SUN 67.
*
*     Proper motions should be given in secs/yr and arcsec/yr.
*     Parallax should be given in arcseconds.
*     Radial velocity should be given in km/sec (+ve if receeding)
*
*     If necessary use UPDATE to convert fields into the appropriate units.

*  Usage:
*     PROPERM INPUT OUTPUT RAEP0 DECEP0 RAPM DECPM PARLLAX RADVEL RAEP1
*     DECEP1 EP0 EP1

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the output catalogue.
*     RAEP0 = _CHAR (Read)
*        Name of the RA field at epoch 0.
*     DECEP0 = _CHAR (Read)
*        Name of the DEC field at epoch 0.
*     RAPM = _CHAR (Read)
*        Name of the RA proper motion field.
*     DECPM = _CHAR (Read)
*        Name of the DEC proper motion field.
*     PARALLAX = _CHAR (Read)
*        Name of the parallax field.
*     RADVEL = _CHAR (Read)
*        Name of the radial velocity field.
*     RAEP1 = _CHAR (Read)
*        Name of the RA field at epoch 1.
*     DECEP1 = _CHAR (Read)
*        Name of the DEC field at epoch 1.
*     EP0 = _REAL (Read)
*        Start Epoch.
*     EP1 = _REAL (Read)
*        End epoch.

*  Example:
*     PROPERM TEST TEST1970 RA DEC RA_PM DEC_PM PARALLAX RAD_VEL 1950 1970
*     RA1970 DEC1970

*  Notes:
*     This application creates a new catalogue that contains extra fields for
*     Right Ascension and Declination after a proper motion correction.
*     The naming of these new fields can lead to confusion. Traditionally
*     the field names RA and DEC are used for the Right Ascension and
*     Declination fields in the catalogue. It is stongly suggested that the
*     field names of Right Ascension and Declination at a new epoch take the
*     form RA1970, DEC1970. You may go on to rename the field RA to RA1950 and
*     DEC to DEC1950 and RA1970 to RA and DEC1970 to DEC using the UPFIELD
*     application but you must also then update the catalogues Epoch parameter
*     to 1970.
*
*     Care should also be taken when renaming fields. Renaming RA1970 to RA
*     before renaming RA to RA1950, in the above example, would result in two
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
      CHARACTER * ( CHP__SZCNAME ) RAEP0 ! Name of the RA field at epoch 0.
      CHARACTER * ( CHP__SZCNAME ) DECEP0 ! Name of the DEC field at epoch 0.
      CHARACTER * ( CHP__SZCNAME ) RAPM ! Name of the RA proper motion field.
      CHARACTER * ( CHP__SZCNAME ) DECPM ! Name of the DEC proper motion field.
      CHARACTER * ( CHP__SZCNAME ) PARALLAX ! Name of the PARALLAX field.
      CHARACTER * ( CHP__SZCNAME ) RADVEL ! Name of the RADIAL VELOCITY field.
      CHARACTER * ( CHP__SZCNAME ) RAEP1 ! Name of the RA field at epoch 1.
      CHARACTER * ( CHP__SZCNAME ) DECEP1 ! Name of the DEC field at epoch 1.
      CHARACTER * ( 4 ) TEMP ! Required for CHR routines
      REAL EP0 ! Start epoch.
      REAL EP1 ! End epoch.
      INTEGER EP1INT ! Integer end epoch
      INTEGER NCHAR ! Required for CHR routines
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)
*
*    Get the parameters
*
       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get0c('RAEP0', raep0, status)
       call par_get0c('DECEP0', decep0, status)
       call par_get0c('RAPM', rapm, status)
       call par_get0c('DECPM', decpm, status)
       call par_get0c('PARLX', parallax, status)
       call par_get0c('RADVEL', radvel, status)
       call par_get0r('EP0', ep0, status)
       call par_get0r('EP1', ep1, status)
*
*    Create suggested names for the new fields.
*
       raep1 = 'RA1'
       ep1int = ep1
       call chr_itoc(ep1int, temp, nchar)
       raep1 = 'RA'//temp
       call par_def0c('RAEP1', raep1, status)
       decep1 = 'DEC'//temp
       call par_def0c('DECEP1', decep1, status)
*
       call par_get0c('RAEP1', raep1, status)
       call par_get0c('DECEP1', decep1, status)
*
*    Make the call.
*
       call chu_pm(incat, outcat, raep0, decep0, rapm, decpm,
     :    parallax, radvel, ep0, ep1, raep1, decep1, status)
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
     : CHP_PM.', status)
       endif
*
       call chp_close(status)
*
      end
