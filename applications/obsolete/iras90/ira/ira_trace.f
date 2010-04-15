       SUBROUTINE IRA_TRACE( IDA, ROUTNE, STATUS )
*+
*  Name:
*     IRA_TRACE

*  Purpose:
*     Display astrometry information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_TRACE( IDA, ROUTNE, STATUS )

*  Description:
*     This routine displays the astrometry information identified by
*     IDA, using the supplied routine to display each line. The
*     displayed information depends on the current IRA implementation,
*     but will include at least a description of the sky coordinate
*     system and reference equinox and probably other items as well.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine. All calls to this routine are made with a priority
*        of MSG__NORM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1992 (DSB):
*        Original version.
*     12-FEB-1993 (DSB):
*        Modified to remove locators from common.
*     {enter_further_changes_here}

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

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Read)
*           The full name of the projection.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameters.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           The full name of the sky coordinate system, with optional
*           equinox specifier.

*  Arguments Given:
      INTEGER IDA
      EXTERNAL ROUTNE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :          ABBREV*(IRA__SZSCA), ! Abbreviation of sky long/lat.
     :          ADESCR*(IRA__SZSCD + 1), ! Description of sky long.
     :          ATEXT*(IRA__SZFSC),  ! Formatted longitude value.
     :          BDESCR*(IRA__SZSCD + 1), ! Description of sky lat.
     :          BJ*1,                ! (B)esselian or (J)ulian epoch.
     :          BTEXT*(IRA__SZFSC),  ! Formatted latitude value.
     :          BUF*80,              ! Buffer for displayed text.
     :          SCSNAM*(IRA__SZSCS), ! The full SCS name.
     :          TEXT*80              ! Temporary text storage.

      DOUBLE PRECISION
     :          EQU                  ! Epoch of reference equinox.

      INTEGER
     :          BUFLEN,              ! Length of text in BUFFER.
     :          LA,                  ! Used length of ABBREV.
     :          LD,                  ! Position of colon.
     :          LDA,                 ! Used length of ADESCR.
     :          LDB                  ! Used length of BDESCR.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )

*  Find the SCS name and equinox specifier.
      CALL IRA1_CHSCS( ACM_SCS( IDA ), SCSNAM, EQU, BJ, STATUS )

*  Display the name of the projection.
      IF( ACM_PROJN( IDA ) .NE. ' ' ) THEN
         TEXT = ACM_PROJN( IDA )
         CALL CHR_LCASE( TEXT( 2: ) )
      ELSE
         TEXT = '(bad projection name found)'
      END IF

      CALL MSG_SETC( 'P', TEXT )
      CALL MSG_LOAD( 'IRA_TRACE_MSG1', 'Projection            : ^P',
     :               BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG1', BUF( : BUFLEN ),
     :             STATUS )

*  Display the nominal pixel size.
      CALL MSG_SETR( 'PX', REAL( ACM_PROJP( 5, IDA )*IRA__R2AM ) )
      CALL MSG_SETR( 'PY', REAL( ACM_PROJP( 6, IDA )*IRA__R2AM ) )
      CALL MSG_LOAD( 'IRA_TRACE_MSG2',
     :               'Nominal pixel size    : ^PX x ^PY arc-mins ',
     :                BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG2', BUF( : BUFLEN ),
     :             STATUS )

*  Display the position angle of the Y axis.
      CALL MSG_SETR( 'PA', REAL( ACM_PROJP( 7, IDA )*IRA__RTOD ) )
      CALL MSG_LOAD( 'IRA_TRACE_MSG3',
     :               'Y axis position angle : ^PA degrees',
     :               BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG3', BUF( : BUFLEN ),
     :             STATUS )

*  Display the projection twist if it is not zero.
      IF( ACM_PROJP( 8, IDA ) .NE. 0.0D0 ) THEN
         CALL MSG_SETR( 'T', REAL( ACM_PROJP( 8, IDA )*IRA__RTOD ) )
         CALL MSG_LOAD( 'IRA_TRACE_MSG4',
     :                  'Projection twist angle: ^T degrees',
     :                  BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG4', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  Display Sky Coordinate System for systems which do not need an
*  equinox specifier.
      TEXT = SCSNAM
      CALL CHR_LCASE( TEXT( 2: ) )

      IF( SCSNAM .NE. 'ECLIPTIC' .AND. SCSNAM .NE. 'EQUATORIAL' ) THEN

         CALL MSG_SETC( 'SCSNAM', TEXT )
         CALL MSG_LOAD( 'IRA_TRACE_MSG5',
     :                  'Sky coordinates system: ^SCSNAM',
     :                  BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG5', BUF( : BUFLEN ),
     :                STATUS )

*  Display Sky Coordinate System which need an equinox specifier.
      ELSE

         CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )
         CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG6',
     :                   'Sky coordinates', STATUS )

         CALL MSG_SETC( 'SCSNAM', TEXT )
         CALL MSG_LOAD( 'IRA_TRACE_MSG7',
     :                  '   Sky coordinates system: ^SCSNAM',
     :                  BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG7', BUF( : BUFLEN ),
     :                STATUS )

         IF( BJ .EQ. 'B' ) THEN
            CALL MSG_SETC( 'BJ', 'Besselian' )
         ELSE
            CALL MSG_SETC( 'BJ', 'Julian' )
         END IF

         CALL MSG_SETD( 'EP', EQU )
         CALL MSG_LOAD( 'IRA_TRACE_MSG8',
     :                  '   Reference equinox     : ^EP (^BJ)',
     :                  BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG8', BUF( : BUFLEN ),
     :                STATUS )

      END IF

*  Display information about the reference position.
      CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG9',
     :               'Reference position', STATUS )

      CALL IRA1_ISCNM( ACM_SCS(IDA), 1, ADESCR, LDA, ABBREV, LA,
     :                 STATUS )
      CALL IRA1_ISCNM( ACM_SCS(IDA), 2, BDESCR, LDB, ABBREV, LA,
     :                 STATUS )
      LD = MAX( 17, MAX( LDA, LDB ) )
      ADESCR( LD + 1 : LD + 1 ) = ':'
      BDESCR( LD + 1 : LD + 1 ) = ':'
      CALL IRA1_IDTC1( ACM_PROJP( 1, IDA ), SCSNAM, 1, 2, ATEXT,
     :                 STATUS )
      CALL IRA1_IDTC1( ACM_PROJP( 2, IDA ), SCSNAM, 2, 2, BTEXT,
     :                 STATUS )

      CALL MSG_SETC( 'AD', ADESCR )
      CALL MSG_SETC( 'A', ATEXT )
      CALL MSG_LOAD( 'IRA_TRACE_MSG10', '   ^AD ^A', BUF, BUFLEN,
     :               STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG10', BUF( : BUFLEN ),
     :             STATUS )

      CALL MSG_SETC( 'BD', BDESCR )
      CALL MSG_SETC( 'B', BTEXT )
      CALL MSG_LOAD( 'IRA_TRACE_MSG11', '   ^BD ^B', BUF, BUFLEN,
     :               STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG11', BUF( : BUFLEN ),
     :             STATUS )

      TEXT = 'Pixel coordinates'
      TEXT( 18 : ) = ' '
      TEXT( LD + 1 : LD + 1 ) = ':'
      CALL MSG_SETC( 'T', TEXT )
      CALL MSG_SETR( 'X', REAL( ACM_PROJP( 3, IDA ) ) )
      CALL MSG_SETR( 'Y', REAL( ACM_PROJP( 4, IDA ) ) )
      CALL MSG_LOAD( 'IRA_TRACE_MSG12', '   ^T ( ^X, ^Y )', BUF, BUFLEN,
     :               STATUS )
      CALL ROUTNE( MSG__NORM, 'IRA_TRACE_MSG12', BUF( : BUFLEN ),
     :             STATUS )

*  If an error has occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_TRACE_ERR1',
     :  'IRA_TRACE: Unable to display information about an astrometry'//
     :  ' structure', STATUS )
      END IF

      END
