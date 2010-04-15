      SUBROUTINE IRC1_TRASB( IDC, ROUTNE, STATUS )
*+
*  Name:
*     IRC1_TRASB

*  Purpose:
*     Display information about a DETAILS structure from a
*     SURVEY_BSIGHT CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_TRASB( IDC, ROUTNE, STATUS )

*  Description:
*     This routine displays information from the DETAILS structure of
*     a SURVEY_BSIGHT CRDD file on the terminal.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the astrometry structure.
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_CRDDL( IRC__MAX ) = CHARACTER (Read)
*           HDS locator for CRDD_INFO component.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS waveband index (1-4).

*  Arguments Given:
      INTEGER IDC
      EXTERNAL ROUTNE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BPOSNS             ! No. of boresight samples.
      CHARACTER DLOC*(DAT__SZLOC)! Locator to DETAILS
      INTEGER IPL                ! Pointer to LAMBDA_SUN
      INTEGER IPP                ! Pointer to PSI
      INTEGER IPT                ! Pointer to THETA
      INTEGER IPU                ! Pointer to UTCS_OFFSET
      INTEGER IPDP               ! Pointer to UNC_PSI
      INTEGER IPDT               ! Pointer to UNC_THETA
      INTEGER IPGLN              ! Pointer to GEOG_LONG
      INTEGER IPGLT              ! Pointer to GEOG_LAT
      INTEGER NEL                ! No. of elements in array (=BPOSNS)
      CHARACTER SLOC*(DAT__SZLOC)! Locator to SUPPORT_INFO.
      LOGICAL SUPP               ! True if support info is present.
      DOUBLE PRECISION UTCS0     ! UTCS at data sample number 1.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the DETAILS structure. The name of the DETAILS
*  component is given in the symbolic constant IRC__DNAME.
      CALL DAT_FIND( CCM_CRDDL( IDC ), IRC__DNAME, DLOC, STATUS )

*  Get the BASE_UTCS and BORE_POSNS components from DETAILS.
      CALL CMP_GET0I( DLOC, 'BORE_POSNS', BPOSNS, STATUS )
      CALL CMP_GET0D( DLOC, 'BASE_UTCS', UTCS0, STATUS )

*  Map each of the array components within DETAILS.
      CALL CMP_MAPV( DLOC, 'UTCS_OFFSET', '_REAL', 'READ', IPU, NEL,
     :               STATUS )
      CALL CMP_MAPV( DLOC, 'PSI', '_REAL', 'READ', IPP, NEL, STATUS )
      CALL CMP_MAPV( DLOC, 'THETA', '_REAL', 'READ', IPT, NEL, STATUS )
      CALL CMP_MAPV( DLOC, 'LAMBDA_SUN', '_REAL', 'READ', IPL, NEL,
     :               STATUS )

*  See if the DETAILS structure contains a support info structure.
      CALL DAT_THERE( DLOC, 'SUPPORT_INFO', SUPP, STATUS )

*  If so, get a locator to the SUPPORT_INFO structure.
      IF( SUPP ) THEN
         CALL DAT_FIND( DLOC, 'SUPPORT_INFO', SLOC, STATUS )

*  Map each of the array components within SUPPORT_INFO.
         CALL CMP_MAPV( SLOC, 'UNC_PSI', '_REAL', 'READ', IPDP, NEL,
     :               STATUS )
         CALL CMP_MAPV( SLOC, 'UNC_THETA', '_REAL', 'READ', IPDT, NEL,
     :               STATUS )
         CALL CMP_MAPV( SLOC, 'GEOG_LAT', '_REAL', 'READ', IPGLT, NEL,
     :               STATUS )
         CALL CMP_MAPV( SLOC, 'GEOG_LONG', '_REAL', 'READ', IPGLN, NEL,
     :               STATUS )

      END IF

*  Call a lower level routine to do the work.
      CALL IRC1_TR2SB( ROUTNE, SUPP, UTCS0, BPOSNS, %VAL( IPU ),
     :                 %VAL( IPP ), %VAL( IPT ), %VAL( IPL ),
     :                 %VAL( IPDP ), %VAL( IPDT ), %VAL( IPGLT ),
     :                 %VAL( IPGLN ), CCM_BAND( IDC ), STATUS )

*  Unmap the arrays and annull the locators.
      IF( SUPP ) THEN
         CALL CMP_UNMAP( SLOC, 'UNC_PSI', STATUS )
         CALL CMP_UNMAP( SLOC, 'UNC_THETA', STATUS )
         CALL CMP_UNMAP( SLOC, 'GEOG_LAT', STATUS )
         CALL CMP_UNMAP( SLOC, 'GEOG_LONG', STATUS )
         CALL DAT_ANNUL( SLOC, STATUS )
      END IF

      CALL CMP_UNMAP( DLOC, 'UTCS_OFFSET', STATUS )
      CALL CMP_UNMAP( DLOC, 'PSI', STATUS )
      CALL CMP_UNMAP( DLOC, 'THETA', STATUS )
      CALL CMP_UNMAP( DLOC, 'LAMBDA_SUN', STATUS )
      CALL DAT_ANNUL( DLOC, STATUS )



      END
