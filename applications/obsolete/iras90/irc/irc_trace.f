       SUBROUTINE IRC_TRACE( IDC, ROUTNE, STATUS )
*+
*  Name:
*     IRC_TRACE

*  Purpose:
*     Display information about a CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_TRACE( IDC, ROUTNE, STATUS )

*  Description:
*     This routine displays information about a CRDD file, using the
*     supplied routine to display each line of text. The particular
*     information displayed will depend on the type of CRDD file.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the astrometry structure.
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine. This routine is called with a priority of MSG__NORM
*        for the more commonly needed information, and MSG__VERB for the
*        less commonly needed information.
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
      INCLUDE 'MSG_PAR'          ! MSG constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error constants.

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
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      EXTERNAL ROUTNE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ABBREV*(IRA__SZSCA)! Abbreviation of sky long/lat.
      CHARACTER ADESCR*(IRA__SZSCD + 1)! Description of sky long.
      CHARACTER ATEXT*(IRA__SZFSC)! Formatted longitude value.
      CHARACTER BDESCR*(IRA__SZSCD + 1)! Description of sky lat.
      CHARACTER BTEXT*(IRA__SZFSC)! Formatted latitude value.
      CHARACTER BUF*80           ! Buffer for displayed text.
      INTEGER BUFLEN             ! Length of text in BUFFER.
      INTEGER DIND               ! Detector index.
      INTEGER LA                 ! Used length of ABBREV.
      INTEGER LD                 ! Position of colon.
      INTEGER LDA                ! Used length of ADESCR.
      INTEGER LDB                ! Used length of BDESCR.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid.
      IF( IDC .LT. 0 .OR. IDC .GT. IRC__MAX .OR. IDC .EQ. IRC__NOID )
     :                                                              THEN
         STATUS = IRC__INVID
      ELSE
         IF( .NOT. CCM_VALID( IDC ) ) STATUS = IRC__INVID
      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_TRACE_ERR1',
     :                 'IRC_TRACE: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Display the wave band.
      CALL MSG_SETI( 'B', I90__WAVEL( CCM_BAND( IDC ) ) )
      CALL MSG_LOAD( 'IRC_TRACE_MSG1', 'Waveband          : ^B um', BUF,
     :               BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG1', BUF( : BUFLEN ),
     :             STATUS )

*  Display the SOP number.
      CALL MSG_SETI( 'S', CCM_SOP( IDC ) )
      CALL MSG_LOAD( 'IRC_TRACE_MSG2', 'SOP               : ^S', BUF,
     :               BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG2', BUF( : BUFLEN ),
     :             STATUS )

*  Display the OBS number.
      CALL MSG_SETI( 'O', CCM_OBS( IDC ) )
      CALL MSG_LOAD( 'IRC_TRACE_MSG3', 'Observation       : ^O', BUF,
     :               BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG3', BUF( : BUFLEN ),
     :             STATUS )

*  Display the nominal scan speed number.
      CALL MSG_FMTR( 'S', 'F5.2', REAL( CCM_NOMSP( IDC )*IRA__R2AM ) )
      CALL MSG_LOAD( 'IRC_TRACE_MSG4',
     :               'Nominal scan speed: ^S arc-mins per second', BUF,
     :               BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG4', BUF( : BUFLEN ),
     :             STATUS )

*  Display information about the reference position.
      CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG5',
     :               'Reference position', STATUS )

      CALL IRA_SCNAM( 'Equatorial', 1, ADESCR, LDA, ABBREV, LA, STATUS )
      CALL IRA_SCNAM( 'Equatorial', 2, BDESCR, LDB, ABBREV, LA, STATUS )
      LD = MAX( LDA, LDB )
      ADESCR( LD + 1 : LD + 1 ) = ':'
      BDESCR( LD + 1 : LD + 1 ) = ':'
      CALL IRA_DTOC1( CCM_REFRA( IDC ), 'Equatorial', 1, 2, ATEXT,
     :                 STATUS )
      CALL IRA_DTOC1( CCM_REFDE( IDC ), 'Equatorial', 2, 2, BTEXT,
     :                 STATUS )

      CALL MSG_SETC( 'AD', ADESCR )
      CALL MSG_SETC( 'A', ATEXT )
      CALL MSG_LOAD( 'IRC_TRACE_MSG6', '   ^AD ^A', BUF, BUFLEN,
     :               STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG6', BUF( : BUFLEN ),
     :             STATUS )

      CALL MSG_SETC( 'BD', BDESCR )
      CALL MSG_SETC( 'B', BTEXT )
      CALL MSG_LOAD( 'IRC_TRACE_MSG7', '   ^BD ^B', BUF, BUFLEN,
     :               STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG7', BUF( : BUFLEN ),
     :             STATUS )

*  Display information about the detector numbers.
      CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG8',
     :               'Detector numbers', STATUS )

      DO DIND = CCM_DLOW( IDC ), CCM_DHIGH( IDC )
         CALL MSG_FMTI( 'IN', 'I3', DIND )
         CALL MSG_SETI( 'NO', CCM_DETNO( DIND +
     :                                   CCM_DETOR( IDC ), IDC ) )
         CALL MSG_LOAD( 'IRC_TRACE_MSG9', '   NDF row ^IN: #^NO',
     :                  BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRC_TRACE_MSG9', BUF( : BUFLEN ),
     :                STATUS )
      END DO

*  Call a lower level routine to trace DETAILS.
      CALL IRC1_TRACI( IDC, ROUTNE, STATUS )

*  If an error has occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_TRACE_ERR2',
     :  'IRC_TRACE: Unable to display information about a CRDD file',
     :                 STATUS )
      END IF

      END
