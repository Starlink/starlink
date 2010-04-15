       SUBROUTINE IRI_TRACE( LOC, ROUTNE, STATUS )
*+
*  Name:
*     IRI_TRACE

*  Purpose:
*     Display information about an IRAS image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRI_TRACE( LOC, ROUTNE, STATUS )

*  Description:
*     This routine displays the information stored in the IMAGE_INFO
*     structure using the supplied routine to display each line.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        An HDS locator to the IMAGE_INFO structure. This should have
*        been obtained using routine IRI_OLD or IRI_NEW.
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine. All calls to this routine are made with priority
*        MSG_NORM.
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
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants
      INCLUDE 'IRI_ERR'          ! IRI error constants
      INCLUDE 'IRI_PAR'          ! IRI error constants
      INCLUDE 'I90_DAT'          ! IRAS90 data

*  Arguments Given:
      CHARACTER LOC*(*)
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
      CHARACTER CVAL*80          ! Character component value.
      DOUBLE PRECISION DVAL      ! DOUBLE PRECISION component value.
      INTEGER IAT                ! Index of last non-blank character.
      INTEGER ISSAF              ! ISSA field no.
      INTEGER IVAL               ! Integer component value.
      INTEGER LA                 ! Used length of ABBREV.
      INTEGER LD                 ! Position of colon.
      INTEGER LDA                ! Used length of ADESCR.
      INTEGER LDB                ! Used length of BDESCR.
      LOGICAL LVAL               ! Logical component value.
      INTEGER MAXSOP             ! Max. SOP number.
      INTEGER MINSOP             ! Min. SOP number.
      REAL RVAL                  ! Real component value.
      LOGICAL THERE              ! True if component exists.
      CHARACTER TYPE*80          ! Image type.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the located object has type equal to IMAGE_INFO.
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. CVAL .NE. 'IMAGE_INFO' ) THEN
         STATUS = IRI__BADII
         CALL MSG_SETC( 'T', CVAL )
         CALL ERR_REP( 'IRI_TRACE_ERR1',
     : 'IRI_TRACE: Supplied structure has wrong HDS type (^T)',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the wave band index.
      CALL CMP_GET0I( LOC, 'BAND', IVAL, STATUS )

*  Display the instrument and wave band.
      CALL CMP_GET0C( LOC, 'INSTRUMENT', CVAL, STATUS )

      IF( CVAL .EQ. 'SURVEY' ) THEN
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG1',
     :                'Instrument          : Survey array', STATUS )
         CALL MSG_SETI( 'B', I90__WAVEL( IVAL ) )

      ELSE
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG2',
     :                'Instrument          : CPC', STATUS )
         CALL MSG_SETI( 'B', I90__CWVEL( IVAL ) )

      END IF

      CALL MSG_LOAD( 'IRI_TRACE_MSG3', 'Waveband            : ^B um',
     :               BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG3', BUF( : BUFLEN ),
     :             STATUS )

*  Initialise the type of image.
      CALL CMP_GET0C( LOC, 'TYPE', TYPE, STATUS )

*  CPCRAW
      CALL DAT_THERE( LOC, 'CPCRAW', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( LOC, 'CPCRAW', LVAL, STATUS )
         IF( LVAL ) THEN
            TYPE = 'Raw CPC Pointed Observation image'
         ELSE
            TYPE = 'Cleaned CPC Pointed Observation image'
         END IF
      END IF

*  GALCEN
      CALL DAT_THERE( LOC, 'GALCEN', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( LOC, 'GALCEN', LVAL, STATUS )
         IF( LVAL ) THEN
            TYPE = 'Low resolution all-sky map cented on Galactic '//
     :      'centre'
         ELSE
            TYPE = 'Low resolution all-sky map cented on Galactic '//
     :      'anti-centre'
         END IF
      END IF

*  POFLUX.
      CALL DAT_THERE( LOC, 'POFLUX', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( LOC, 'POFLUX', LVAL, STATUS )
         IF( LVAL ) THEN
            TYPE = 'Pointed Observation flux map'
         ELSE
            TYPE = 'Pointed Observation intensity map'
         END IF
      END IF

*  PONMAP
      CALL DAT_THERE( LOC, 'PONMAP', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( LOC, 'PONMAP', LVAL, STATUS )
         IF( LVAL ) THEN

            IF( INDEX( TYPE, 'Pointed Observation' ) .EQ. 1 ) THEN
               IAT = INDEX( TYPE, 'map' ) - 1
               CALL CHR_APPND( 'noise map', TYPE, IAT )
            ELSE
               TYPE = 'Pointed Observation noise map'
            END IF

         END IF
      END IF

*  SKYWEIGHT
      CALL DAT_THERE( LOC, 'SKYWEIGHT', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( LOC, 'SKYWEIGHT', LVAL, STATUS )
         IF( LVAL ) THEN
            TYPE = 'SKYFLUX weight map'
         ELSE
            TYPE = 'SKYFLUX intensity map'
         END IF
      END IF

*  ISSAFLD
      CALL DAT_THERE( LOC, 'ISSAFLD', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'ISSAFLD', ISSAF, STATUS )
         TYPE = 'IRAS Sky Survey Atlas image'
      ELSE
         ISSAF = 0
      END IF

*  YORTYPE
      CALL DAT_THERE( LOC, 'YORTYPE', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0C( LOC, 'YORTYPE', TYPE, STATUS )

         IF( TYPE .EQ. IRI__YOIMG ) THEN
            TYPE = 'HIRES surface brightness image'

         ELSE IF( TYPE .EQ. IRI__YOPHN ) THEN
            TYPE = 'HIRES photometric noise image'

         ELSE IF( TYPE .EQ. IRI__YOCVG ) THEN
            TYPE = 'HIRES coverage image'

         ELSE IF( TYPE .EQ. IRI__YOCFV ) THEN
            TYPE = 'HIRES correction factor variance image'

         ELSE IF( TYPE .EQ. IRI__YORES ) THEN
            TYPE = 'HIRES resolution image'

         END IF

      END IF

*  Display the image type.
      CALL MSG_SETC( 'C', TYPE )
      CALL MSG_LOAD( 'IRI_TRACE_MSG4',
     :               'Image type          : ^C', BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG4', BUF( : BUFLEN ),
     :             STATUS )


*  COLCOR
      CALL DAT_THERE( LOC, 'COLCOR', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0C( LOC, 'COLCOR', CVAL, STATUS )
         CALL MSG_SETC( 'C', CVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG5',
     :                'Colour correction   : ^C', BUF, BUFLEN,
     :                STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG5', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  ISSAFLD
      IF( ISSAF .NE. 0) THEN
         CALL MSG_SETI( 'I', ISSAF )
         CALL MSG_LOAD( 'IRI_TRACE_MSG6',
     :                'Field number        : ^I', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG6', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  GALMAP
      CALL DAT_THERE( LOC, 'GALMAP', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'GALMAP', IVAL, STATUS )
         CALL MSG_SETI( 'I', IVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG7',
     :                'Map number          : ^I', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG7', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  HCON
      CALL DAT_THERE( LOC, 'HCON', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'HCON', IVAL, STATUS )
         CALL MSG_SETI( 'I', IVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG8',
     :                'HCON                : ^I', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG8', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  MAXSOP, MINSOP
      CALL DAT_THERE( LOC, 'MAXSOP', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'MAXSOP', MAXSOP, STATUS )
      ELSE
         MAXSOP = 0
      END IF

      CALL DAT_THERE( LOC, 'MINSOP', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'MINSOP', MINSOP, STATUS )
      ELSE
         MINSOP = 0
      END IF

      IF( MAXSOP .NE. 0 .AND. MINSOP .NE. 0 ) THEN
         CALL MSG_SETI( 'MAX', MAXSOP )
         CALL MSG_SETI( 'MIN', MINSOP )
         CALL MSG_LOAD( 'IRI_TRACE_MSG9',
     :                'SOP range           : ^MIN to ^MAX', BUF, BUFLEN,
     :                STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG9', BUF( : BUFLEN ),
     :                STATUS )

      ELSE IF( MAX( MAXSOP, MINSOP ) .NE. 0 ) THEN
         CALL MSG_SETI( 'SOP', MAX( MAXSOP, MINSOP ) )
         CALL MSG_LOAD( 'IRI_TRACE_MSG10',
     :                'SOP                 : ^SOP', BUF, BUFLEN,
     :                STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG10', BUF( : BUFLEN ),
     :                STATUS )

      END IF

*  OBSNO.
      CALL DAT_THERE( LOC, 'OBSNO', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'OBSNO', IVAL, STATUS )
         CALL MSG_SETI( 'I', IVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG11',
     :                'Observation number  : ^I', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG11', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  PONOISE and POUNITS.
      CALL DAT_THERE( LOC, 'PONOISE', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0R( LOC, 'PONOISE', RVAL, STATUS )
         CALL CMP_GET0C( LOC, 'POUNITS', CVAL, STATUS )
         CALL MSG_SETR( 'R', RVAL )
         CALL MSG_SETC( 'C', CVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG14',
     :                'Median noise         : ^R ^C', BUF, BUFLEN,
     :                  STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG14', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  SKYFLUX
      CALL DAT_THERE( LOC, 'SKYFLUX', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0I( LOC, 'SKYFLUX', IVAL, STATUS )
         CALL MSG_SETI( 'I', IVAL )
         CALL MSG_LOAD( 'IRI_TRACE_MSG16',
     :                'SKYFLUX plate number: ^I', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG16', BUF( : BUFLEN ),
     :                STATUS )
      END IF

*  FIELDLAT, FIELDLON, FIELDSCS
      CALL DAT_THERE( LOC, 'FIELDLON', THERE, STATUS )
      IF( THERE ) THEN
         CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )

         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG17',
     :                'Reference position', STATUS )

         CALL CMP_GET0C( LOC, 'FIELDSCS', CVAL, STATUS )
         CALL IRA_SCNAM( CVAL, 1, ADESCR, LDA, ABBREV, LA, STATUS )
         CALL IRA_SCNAM( CVAL, 2, BDESCR, LDB, ABBREV, LA, STATUS )
         LD = MAX( LDA, LDB )
         ADESCR( LD + 1 : LD + 1 ) = ':'
         BDESCR( LD + 1 : LD + 1 ) = ':'

         CALL CMP_GET0D( LOC, 'FIELDLON', DVAL, STATUS )
         CALL IRA_DTOC1( DVAL, CVAL, 1, 2, ATEXT, STATUS )

         CALL CMP_GET0D( LOC, 'FIELDLAT', DVAL, STATUS )
         CALL IRA_DTOC1( DVAL, CVAL, 2, 2, BTEXT, STATUS )

         CALL MSG_SETC( 'AD', ADESCR )
         CALL MSG_SETC( 'A', ATEXT )
         CALL MSG_LOAD( 'IRI_TRACE_MSG18', '   ^AD ^A', BUF, BUFLEN,
     :                  STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG18', BUF( : BUFLEN ),
     :                STATUS )

         CALL MSG_SETC( 'BD', BDESCR )
         CALL MSG_SETC( 'B', BTEXT )
         CALL MSG_LOAD( 'IRI_TRACE_MSG19', '   ^BD ^B', BUF, BUFLEN,
     :                  STATUS )
         CALL ROUTNE( MSG__NORM, 'IRI_TRACE_MSG19', BUF( : BUFLEN ),
     :                STATUS )

      END IF

*  If an error has occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRI_TRACE_ERR2',
     :  'IRI_TRACE: Unable to display information about an IRAS image',
     :                 STATUS )
      END IF

      END
