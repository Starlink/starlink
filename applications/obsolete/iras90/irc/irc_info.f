      SUBROUTINE IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP,
     :                     OBS, STATUS )
*+
*  Name:
*     IRC_INFO

*  Purpose:
*     Obtain values for the global properties of a CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
*                    STATUS )

*  Description:
*     The values of various global parameters are returned for the CRDD
*     file identified by the given IRC identifier. Other global
*     properties (such as the size and shape of the data array) can be
*     obtained using the NDF_ routines (see SUN/33).

*  Arguments:
*     IRC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     BAND = INTEGER (Returned)
*        The IRAS band number of the data, in the range 1 to 4.
*     REFRA = DOUBLE PRECISION (Returned)
*        The Right Ascension of the reference position, given in units
*        of radians (FK4 B1950.0).
*     REFDEC = DOUBLE PRECISION (Returned)
*        The Declination of the reference position, given in units of
*        radians (FK4 B1950.0).
*     NOMSPD = REAL (Returned)
*        The nominal scan speed in arcminutes per second (the exact
*        scan speed may vary slightly from sample to sample). Positive
*        values imply that sources move in the positive focal plane Y
*        direction (i.e in the "with-survey" direction). Negative values
*        imply that sources move in the negative Y direction (i.e.
*        "anti-survey"). See ID1 Appendix E for a description of the
*        focal plane coordinate system).
*     SOP = INTEGER (Returned)
*        The SOP number from which the crdd was derived.
*     OBS  = INTEGER (Returned)
*        The Observation number within the SOP from which the data was
*        derived.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS band number (NOT wavelength).
*        CCM_REFRA( IRC__MAX ) = DOUBLE PRECISION (Read)
*           RA (B1950) of the reference point.
*        CCM_REFDE( IRC__MAX ) = DOUBLE PRECISION (Read)
*           DEC (B1950) of the reference point.
*        CCM_SOP( IRC__MAX ) = INTEGER (Read)
*           SOP number.
*        CCM_OBS( IRC__MAX ) = INTEGER (Read)
*           OBS number.
*        CCM_NOMSP( IRC__MAX ) = REAL (Read)
*           Nominal scan speed.

*  Arguments Given:
      INTEGER IDC

*  Arguments Returned:
      INTEGER BAND
      DOUBLE PRECISION REFRA
      DOUBLE PRECISION REFDEC
      REAL NOMSPD
      INTEGER SOP
      INTEGER OBS

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid.
      IF( IDC .GT. 0 .AND. IDC .LE. IRC__MAX ) THEN
         IF( CCM_VALID( IDC ) ) THEN

* Extract the values from common.
            BAND = CCM_BAND( IDC )
            REFRA = CCM_REFRA( IDC )
            REFDEC = CCM_REFDE( IDC )
            NOMSPD = CCM_NOMSP( IDC )
            SOP = CCM_SOP( IDC )
            OBS = CCM_OBS( IDC )

*  If an invalid IRC identifier was supplied, give an error report.
         ELSE
            STATUS = IRC__INVID
            CALL ERR_REP( 'IRC_INFO_ERR1',
     :                    'IRC_INFO: Invalid IRC identifier supplied',
     :                    STATUS )
         END IF

      ELSE
         STATUS = IRC__INVID
         CALL ERR_REP( 'IRC_INFO_ERR2',
     :                 'IRC_INFO: Invalid IRC identifier supplied',
     :                 STATUS )
      END IF

      END
