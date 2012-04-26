*      PROGRAM EXERCISE
*+
*{a_task_prologue}
*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ public constants
      INCLUDE 'DAT_ERR'          ! DAT__ error codes
      INCLUDE 'EMS_PAR'          ! EMS_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR
      EXTERNAL APP

*  Local Constants:
      INTEGER MXLOC              ! Max. number of HDS locators
      PARAMETER ( MXLOC = 1000 )

*  Local variables:
      CHARACTER * ( 256 ) FSPEC
      CHARACTER * ( 30 ) FORMAT
      CHARACTER * ( 30 ) ORDER
      CHARACTER * ( 80 ) CMD
      CHARACTER * ( 256 ) FILE
      CHARACTER * ( 256 ) PATH
      CHARACTER * ( 200 ) REF
      CHARACTER * ( DAT__SZLOC ) LOC( 0 : MXLOC )
      CHARACTER * ( DAT__SZLOC ) LOCTMP
      CHARACTER * ( DAT__SZMOD ) MODE
      CHARACTER * ( DAT__SZNAM ) GROUP
      CHARACTER * ( DAT__SZNAM ) NAME
      CHARACTER * ( DAT__SZNAM ) PARAM
      CHARACTER * ( DAT__SZNAM ) TOPIC
      CHARACTER * ( DAT__SZTYP ) MTYPE( 0 : MXLOC )
      CHARACTER * ( DAT__SZTYP ) TYPE
      CHARACTER * ( EMS__SZMSG ) MESS
      INTEGER BLOCK
      INTEGER BYTOFF
      INTEGER DIM( DAT__MXDIM )
      INTEGER EL( 0 : MXLOC )
      INTEGER I
      INTEGER ICOMP
      INTEGER II
      INTEGER IPRMRY
      INTEGER IREFCT
      INTEGER ISET
      INTEGER ISTAT
      INTEGER ISTRUC
      INTEGER IWLD
      INTEGER J
      INTEGER LBND( DAT__MXDIM )
      INTEGER LEN
      INTEGER LEVEL1
      INTEGER LEVEL2
      INTEGER LREF
      INTEGER MCLEN( 0 : MXLOC )
      INTEGER MDIM( DAT__MXDIM, 0 : MXLOC )
      INTEGER MNDIM( 0 : MXLOC )
      INTEGER NCOMP
      INTEGER NDIM
      INTEGER NI
      INTEGER NLEV
      INTEGER PNTR( 0 : MXLOC )
      INTEGER UBND( DAT__MXDIM )
      INTEGER VALUE
      LOGICAL MAP( 0 : MXLOC )
      LOGICAL PRMRY
      LOGICAL REPLY
      LOGICAL STATE
      LOGICAL VALID
      REAL INC
      REAL START

*  Local Data:
      DATA MAP / .FALSE., MXLOC * .FALSE. /

*.

      CALL TST_INIT
      
      STATUS = SAI__OK
      
      II = 0
      NI = 0
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .TRUE. ) THEN

*  Read command specifying operation to perform.
         CALL TST_GETC( 'Operation to perform', CMD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL TST_BLANK( STATUS )

*  Remember the error message context level.
         CALL ERR_LEVEL( LEVEL1 )

*  Alter object size.
         IF ( CHR_SIMLR( CMD, 'ALTER' ) ) THEN
            CALL TST_OUT( ' ', 'Alter object size...',
     :                    STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL DAT_ALTER( LOC( II ), NDIM, DIM, STATUS )

*  Annul locator.
         ELSE IF ( CHR_SIMLR( CMD, 'ANNUL' ) ) THEN
            CALL TST_OUT( ' ', 'Annul locator...',
     :                    STATUS )
            CALL DAT_ANNUL( LOC( II ), STATUS )

*  Map primitive as basic units.
         ELSE IF ( CHR_SIMLR( CMD, 'BASIC' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as basic units...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_BASIC( LOC( II ), MODE, PNTR( II ), EL( II ),
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66

            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL machine elements mapped', STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            CALL DAT_PREC( LOC( II ), LEN, STATUS )
            MDIM( 1, II ) = MDIM( 1, II ) * LEN
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_UBYTE'

*  Copy one structure level.
         ELSE IF ( CHR_SIMLR( CMD, 'CCOPY' ) ) THEN
            CALL TST_OUT( ' ', 'Copy one structure level...',
     :                    STATUS )
            CALL TST_GETI( 'Locator no. of destination structure',
     :                     ISTRUC, STATUS )
            CALL TST_GETC( 'New component name', NAME, STATUS )
            CALL DAT_CCOPY( LOC( II ), LOC( ISTRUC ), NAME,
     :                      LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Create type string.
         ELSE IF ( CHR_SIMLR( CMD, 'CCTYP' ) ) THEN
            CALL TST_OUT( ' ', 'Create type string...',
     :                    STATUS )
            CALL TST_GETI( 'Character string length', LEN, STATUS )
            CALL DAT_CCTYP( LEN, TYPE )
            CALL MSG_SETC( 'TYPE', TYPE )
            CALL TST_OUT( ' ', 'Data type string = ''^TYPE''', STATUS )

*  Locate cell.
         ELSE IF ( CHR_SIMLR( CMD, 'CELL' ) ) THEN
            CALL TST_OUT( ' ', 'Locate cell...',
     :                    STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Subscript', DIM( I ), STATUS )
            END DO
            CALL DAT_CELL( LOC( II ), NDIM, DIM, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Inquire character string length.
         ELSE IF ( CHR_SIMLR( CMD, 'CLEN' ) ) THEN
            CALL TST_OUT( ' ', 'Inquire character string...',
     :                    STATUS )
            CALL DAT_CLEN( LOC( II ), LEN, STATUS )
            CALL MSG_SETI( 'LEN', LEN )
            CALL TST_OUT( ' ', 'Length = ^LEN', STATUS )

*  Clone locator.
         ELSE IF ( CHR_SIMLR( CMD, 'CLONE' ) ) THEN
            CALL TST_OUT( ' ', 'Clone locator...',
     :                    STATUS )
            CALL DAT_CLONE( LOC( II ), LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Coerce object shape.
         ELSE IF ( CHR_SIMLR( CMD, 'COERC' ) ) THEN
            CALL TST_OUT( ' ', 'Coerce object shape...',
     :                    STATUS )
            CALL TST_GETI( 'New number of dimensions', NDIM, STATUS )
            CALL DAT_COERC( LOC( II ), NDIM, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Inquire conversion possible.
         ELSE IF ( CHR_SIMLR( CMD, 'CONV' ) ) THEN
            CALL TST_OUT( ' ', 'Inquire conversion possible...',
     :                    STATUS )
            CALL TST_GETC( 'New type required', TYPE, STATUS )
            CALL DAT_CONV( LOC( II ), TYPE, REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Conversion possible = ^REPLY', STATUS )

*  Copy object.
         ELSE IF ( CHR_SIMLR( CMD, 'COPY' ) ) THEN
            CALL TST_OUT( ' ', 'Copy object...',
     :                    STATUS )
            CALL TST_GETI( 'Locator no. of destination structure',
     :                     ISTRUC, STATUS )
            CALL TST_GETC( 'New component name', NAME, STATUS )
            CALL DAT_COPY( LOC( II ), LOC( ISTRUC ), NAME, STATUS )
            CALL DAT_FIND( LOC( ISTRUC ), NAME, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI
            
*  Obtain data representation.
         ELSE IF ( CHR_SIMLR( CMD, 'DREP' ) ) THEN
            CALL TST_OUT( ' ', 'Obtain data representation...',
     :                    STATUS )
            CALL DAT_DREP( LOC( II ), FORMAT, ORDER, STATUS )
            CALL MSG_SETC( 'FORMAT', FORMAT )
            CALL MSG_SETC( 'ORDER', ORDER )
            CALL TST_OUT( ' ',
     :         'Format = ''^FORMAT'', order = ''^ORDER''.', STATUS )

*  Erase component.
         ELSE IF ( CHR_SIMLR( CMD, 'ERASE' ) ) THEN
            CALL TST_OUT( ' ', 'Erase component...',
     :                    STATUS )
            CALL TST_GETC( 'Name of component', NAME, STATUS )
            CALL DAT_ERASE( LOC( II ), NAME, STATUS )

*  Report error about an object.
         ELSE IF ( CHR_SIMLR( CMD, 'ERDSC' ) ) THEN
            CALL TST_OUT( ' ',
     :           'Report error about an object...',
     :                    STATUS )
            CALL TST_GETI( 'Status code', STATUS, STATUS )
            CALL DAT_ERDSC( LOC( II ), STATUS )

*  Report error about a component.
         ELSE IF ( CHR_SIMLR( CMD, 'ERDSN' ) ) THEN
            CALL TST_OUT( ' ',
     :           'Report error about a component...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL TST_GETI( 'Status code', STATUS, STATUS )
            CALL DAT_ERDSN( LOC( II ), NAME, STATUS )

*  Report textual error message.
         ELSE IF ( CHR_SIMLR( CMD, 'ERTXT' ) ) THEN
            CALL TST_OUT( ' ',
     :           'Report textual error message...',
     :                    STATUS )
            CALL TST_GETC( 'Error text', MESS, STATUS )
            CALL TST_GETI( 'Status code', STATUS, STATUS )
            CALL DAT_ERTXT( MESS, STATUS )

*  Translate status into an error message.
         ELSE IF ( CHR_SIMLR( CMD, 'ERMSG' ) ) THEN
            CALL TST_OUT( ' ',
     :           'Translate status value into an error message...',
     :                    STATUS )
            CALL TST_GETI( 'Code to translate', ISTAT, STATUS )
            CALL DAT_ERMSG( ISTAT, LEN, MESS )
            CALL MSG_SETC( 'MESSAGE', MESS )
            CALL TST_OUT( ' ', 'Message is ''^MESSAGE''', STATUS )
            CALL MSG_SETI( 'LEN', LEN )
            CALL TST_OUT( ' ', 'Length = ^LEN', STATUS )

*  Examine mapped values.
         ELSE IF ( CHR_SIMLR( CMD, 'EX' ) ) THEN
            CALL TST_OUT( ' ', 'Examine mapped values...',
     :                    STATUS )
            IF ( .NOT. MAP( II ) ) THEN
               CALL TST_OUT( ' ', 'Array not mapped', STATUS )
            ELSE
               IF ( MNDIM( II ) .EQ. 0 ) THEN
                  CALL WRN( MTYPE( II ), 1, MDIM( 1, II ),
     :                      PNTR( II ), STATUS, MCLEN( II ) )
               ELSE
                  CALL WRN( MTYPE( II ), MNDIM( II ), MDIM( 1, II ),
     :                      PNTR( II ), STATUS, MCLEN( II ) )
               END IF
            END IF

*  Find named component.
         ELSE IF ( CHR_SIMLR( CMD, 'FIND' ) ) THEN
            CALL TST_OUT( ' ', 'Find named component...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL DAT_FIND( LOC( II ), NAME, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Go to nominated locator.
         ELSE IF ( CHR_SIMLR( CMD, 'GO' ) ) THEN
            CALL TST_OUT( ' ', 'Go to nominated locator...',
     :                    STATUS )
            CALL TST_GETI( 'Locator number', II, STATUS )

*  Index into component list.
         ELSE IF ( CHR_SIMLR( CMD, 'INDEX' ) ) THEN
            CALL TST_OUT( ' ', 'Index into component list...',
     :                    STATUS )
            CALL TST_GETI( 'Component number', ICOMP, STATUS )
            CALL DAT_INDEX( LOC( II ), ICOMP, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Inquire primitive precision.
         ELSE IF ( CHR_SIMLR( CMD, 'LEN' ) ) THEN
            CALL TST_OUT( ' ', 'Inquire primitive precision...',
     :                    STATUS )
            CALL DAT_LEN( LOC( II ), LEN, STATUS )
            CALL MSG_SETI( 'LEN', LEN )
            CALL TST_OUT( ' ', 'Precision = ^LEN', STATUS )

*  List structure components.
         ELSE IF ( CHR_SIMLR( CMD, 'LS' ) ) THEN
            CALL TST_OUT( ' ', 'List structure components...',
     :                    STATUS )
            NCOMP = 0
            CALL DAT_NCOMP( LOC( II ), NCOMP, STATUS )
            DO I = 1, NCOMP
               CALL DAT_INDEX( LOC( II ), I, LOCTMP, STATUS )
               CALL DAT_NAME( LOCTMP, NAME, STATUS )
               CALL DAT_SHAPE( LOCTMP, DAT__MXDIM, DIM, NDIM, STATUS )
               CALL DAT_TYPE( LOCTMP, TYPE, STATUS )
               CALL DAT_ANNUL( LOCTMP, STATUS )
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_SETC( 'TYPE', TYPE )
               IF ( NDIM .EQ. 0 ) THEN
                  CALL TST_OUT( ' ', '   ^NAME <^TYPE>', STATUS )
               ELSE
                  DO J = 1, NDIM
                     IF ( J .NE. 1 ) CALL MSG_SETC( 'SHAPE', ',' )
                     CALL MSG_SETI( 'SHAPE', DIM( J ) )
                  END DO
                  CALL TST_OUT( ' ', '   ^NAME(^SHAPE) <^TYPE>',
     :                          STATUS )
               END IF
            END DO

*  Map primitive.
         ELSE IF ( CHR_SIMLR( CMD, 'MAP' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive...',
     :                    STATUS )
            CALL TST_GETC( 'Data type', MTYPE( II ), STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            IF ( CHR_SIMLR( MTYPE( II )( : 6 ), '_CHAR*' ) ) THEN
               CALL CHR_CTOI( MTYPE( II )( 7 : ), MCLEN( II ), STATUS )
            ELSE
               CALL DAT_CLEN( LOC( II ), MCLEN( II ), STATUS )
            END IF
            CALL ERR_MARK
            CALL DAT_MAP( LOC( II ), MTYPE( II ), MODE, MNDIM( II ),
     :                    MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :           ( STATUS .EQ. DAT__TRUNC ) ) THEN
               CALL DAT_ERMSG( STATUS, LEN, MESS )
               CALL MSG_SETC( 'MESSAGE', MESS )
               CALL ERR_REP( ' ', '^MESSAGE', STATUS )
               CALL TST_FLUSH( STATUS )
            END IF
            CALL ERR_RLSE
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            CALL MSG_SETI( 'PNTR', PNTR( II ) )
            CALL MSG_OUT( ' ', 'Pointer value is ^PNTR', STATUS )

*  Map primitive as CHARACTER.
         ELSE IF ( CHR_SIMLR( CMD, 'MAPC' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as CHARACTER...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            CALL DAT_MAPC( LOC( II ), MODE, MNDIM( II ),
     :                       MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
 
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_CHAR'
 
*  Map primitive as DOUBLE PRECISION.
         ELSE IF ( CHR_SIMLR( CMD, 'MAPD' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as DOUBLE PRECISION...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            CALL DAT_MAPD( LOC( II ), MODE, MNDIM( II ),
     :                       MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
 
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_DOUBLE'
 
*  Map primitive as INTEGER.
         ELSE IF ( CHR_SIMLR( CMD, 'MAPI' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as INTEGER...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            CALL DAT_MAPI( LOC( II ), MODE, MNDIM( II ),
     :                       MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
 
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_INTEGER'
 
*  Map primitive as LOGICAL.
         ELSE IF ( CHR_SIMLR( CMD, 'MAPL' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as LOGICAL...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            CALL DAT_MAPL( LOC( II ), MODE, MNDIM( II ),
     :                       MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
 
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_LOGICAL'
 
*  Map primitive as REAL.
         ELSE IF ( CHR_SIMLR( CMD, 'MAPR' ) ) THEN
            CALL TST_OUT( ' ', 'Map primitive as REAL...',
     :                    STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, MDIM( 1, II ),
     :                      MNDIM( II ), STATUS )
            CALL DAT_SIZE( LOC( II ), EL( II ), STATUS )
            CALL DAT_MAPR( LOC( II ), MODE, MNDIM( II ),
     :                       MDIM( 1, II ), PNTR( II ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
 
            CALL MSG_SETI( 'EL', EL( II ) )
            CALL TST_OUT( ' ', '^EL elements mapped', STATUS )
            IF ( MNDIM( II ) .EQ. 0 ) MDIM( 1, II ) = 1
            MAP( II ) = .TRUE.
            MTYPE( II ) = '_REAL'
 
*  Alter object shape.
         ELSE IF ( CHR_SIMLR( CMD, 'MOULD' ) ) THEN
            CALL TST_OUT( ' ', 'Alter object shape...',
     :                    STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL DAT_MOULD( LOC( II ), NDIM, DIM, STATUS )

*  Move object.
         ELSE IF ( CHR_SIMLR( CMD, 'MOVE' ) ) THEN
            CALL TST_OUT( ' ', 'Move object...',
     :                    STATUS )
            CALL TST_GETI( 'Locator no. of destination structure',
     :                     ISTRUC, STATUS )
            CALL TST_GETC( 'New component name', NAME, STATUS )
            CALL DAT_MOVE( LOC( II ), LOC( ISTRUC ), NAME, STATUS )

*  Assign object name to message token.
         ELSE IF ( CHR_SIMLR( CMD, 'MSG' ) ) THEN
            CALL TST_OUT( ' ', 'Assign object name to message token...',
     :                    STATUS )
            CALL DAT_MSG( 'OBJECT', LOC( II ) )
            CALL TST_OUT( ' ', 'Object is: ^OBJECT', STATUS )

*  Enquire object name.
         ELSE IF ( CHR_SIMLR( CMD, 'NAME' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object name...',
     :                    STATUS )
            CALL DAT_NAME( LOC( II ), NAME, STATUS )
            CALL MSG_SETC( 'NAME', NAME )
            CALL TST_OUT( ' ', 'Object name = ''^NAME''', STATUS )

*  Enquire number of components.
         ELSE IF ( CHR_SIMLR( CMD, 'NCOMP' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire number of components...',
     :                    STATUS )
            CALL DAT_NCOMP( LOC( II ), NCOMP, STATUS )
            CALL MSG_SETI( 'NCOMP', NCOMP )
            CALL TST_OUT( ' ', 'Number of components = ^NCOMP', STATUS )

*  Create new component.
         ELSE IF ( CHR_SIMLR( CMD, 'NEW' ) ) THEN
            CALL TST_OUT( ' ', 'Create new component...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL TST_GETC( 'Component type', TYPE, STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL DAT_NEW( LOC( II ), NAME, TYPE, NDIM, DIM, STATUS )
            CALL DAT_FIND( LOC( II ), NAME, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Create new character component.
         ELSE IF ( CHR_SIMLR( CMD, 'NEWC' ) ) THEN
            CALL TST_OUT( ' ', 'Create new character component...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL TST_GETI( 'Character string length', LEN, STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL DAT_NEWC( LOC( II ), NAME, LEN, NDIM, DIM, STATUS )
            CALL DAT_FIND( LOC( II ), NAME, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Locate parent.
         ELSE IF ( CHR_SIMLR( CMD, 'PAREN' ) ) THEN
            CALL TST_OUT( ' ', 'Locate parent...',
     :                    STATUS )
            CALL DAT_PAREN( LOC( II ), LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Enquire storage precision.
         ELSE IF ( CHR_SIMLR( CMD, 'PREC' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire storage precision...',
     :                    STATUS )
            CALL DAT_PREC( LOC( II ), LEN, STATUS )
            CALL MSG_SETI( 'LEN', LEN )
            CALL TST_OUT( ' ', 'Storage precision = ^LEN', STATUS )

*  Enquire if object primitive.
         ELSE IF ( CHR_SIMLR( CMD, 'PRIM' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire if object primitive...',
     :                    STATUS )
            CALL DAT_PRIM( LOC( II ), REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Object primitive = ^REPLY', STATUS )

*  Set or inquire primary/secondary locator status.
         ELSE IF ( CHR_SIMLR( CMD, 'PRMRY' ) ) THEN
            CALL TST_OUT( ' ', 'Set or inquire primary/secondary ' //
     :                         'locator status...', STATUS )
            CALL TST_GETI( 'Set(1) or inquire(0) locator status?',
     :                     ISET, STATUS )
            IF ( ISET .EQ. 0 ) THEN
               CALL DAT_PRMRY( .FALSE., LOC( II ), PRMRY, STATUS )
               CALL MSG_SETL( 'PRMRY', PRMRY )
               CALL TST_OUT( ' ', 'Locator primary = ^PRMRY', STATUS )
            ELSE
               CALL TST_GETI( 'Set primary(1) or secondary(0) status?',
     :                        IPRMRY, STATUS )
               CALL DAT_PRMRY( .TRUE., LOC( II ), ( IPRMRY .NE. 0 ),
     :                         STATUS )
            END IF

*  Erase a wild-carded set of HDS container files (a tidying up
*  operation, typically prior to testing wild-carding facilities).
*  Litte of the output is required in the test output file.
         ELSE IF ( CHR_SIMLR( CMD, 'PURGE' ) ) THEN
            CALL TST_OUT( ' ', 'Erase a wild-carded set of ' //
     :                         'HDS container files...', STATUS )
            CALL TST_GETC( 'File spec', FSPEC, STATUS )
            IWLD = DAT__NOWLD
 111        CONTINUE             ! Start of 'DO WHILE' loop
            CALL HDS_WILD( FSPEC, 'W', IWLD, LOCTMP, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               IF ( IWLD .EQ. DAT__NOWLD ) THEN
                  GO TO 66
               ELSE
                  GO TO 111
               END IF
            ELSE IF ( LOCTMP .NE. DAT__NOLOC ) THEN
               CALL DAT_MSG( 'OBJECT', LOCTMP )
               CALL MSG_OUT( ' ', 'Erasing ^OBJECT', STATUS )
               CALL HDS_ERASE( LOCTMP, STATUS )
               GO TO 111
            END IF
            CALL HDS_EWILD( IWLD, STATUS )

*  Quit.
         ELSE IF ( CHR_SIMLR( CMD, 'QUIT' ) ) THEN
            CALL TST_OUT( ' ', 'Quit...',
     :                    STATUS )
            GO TO 99

*  Recursive erase.
         ELSE IF ( CHR_SIMLR( CMD, 'RCERA' ) ) THEN
            CALL TST_OUT( ' ', 'Recursive erase...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL DAT_RCERA( LOC( II ), NAME, STATUS )

*  Recursive copy.
         ELSE IF ( CHR_SIMLR( CMD, 'RCOPY' ) ) THEN
            CALL TST_OUT( ' ', 'Recursive copy...',
     :                    STATUS )
            CALL TST_GETI( 'Locator no. of destination structure',
     :                     ISTRUC, STATUS )
            CALL TST_GETC( 'New component name', NAME, STATUS )
            CALL DAT_RCOPY( LOC( II ), LOC( ISTRUC ), NAME, STATUS )
            CALL DAT_FIND( LOC( ISTRUC ), NAME, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Obtain reference name.
         ELSE IF ( CHR_SIMLR( CMD, 'REF' ) ) THEN
            CALL TST_OUT( ' ', 'Obtain reference name...',
     :                    STATUS )
            CALL DAT_REF( LOC( II ), REF, LREF, STATUS )
	    CALL MSG_SETC( 'OBJECT', REF )
            CALL TST_OUT( ' ', 'Reference name is: ^OBJECT', STATUS )
            CALL MSG_SETI( 'LREF', LREF )
            CALL MSG_OUT( ' ', 'Length of name is: ^LREF',
     :                    STATUS )

*  Obtain reference count.
         ELSE IF ( CHR_SIMLR( CMD, 'REFCT' ) ) THEN
            CALL TST_OUT( ' ', 'Obtain reference count...',
     :                    STATUS )
            CALL DAT_REFCT( LOC( II ), IREFCT, STATUS )
	    CALL MSG_SETI( 'REFCT', IREFCT )
            CALL TST_OUT( ' ', 'Reference count: ^REFCT', STATUS )

*  Rename object.
         ELSE IF ( CHR_SIMLR( CMD, 'RENAM' ) ) THEN
            CALL TST_OUT( ' ', 'Rename object...',
     :                    STATUS )
            CALL TST_GETC( 'New object name', NAME, STATUS )
            CALL DAT_RENAM( LOC( II ), NAME, STATUS )

*  Reset object state.
         ELSE IF ( CHR_SIMLR( CMD, 'RESET' ) ) THEN
            CALL TST_OUT( ' ', 'Reset object state...',
     :                    STATUS )
            CALL DAT_RESET( LOC( II ), STATUS )

*  Change object type.
         ELSE IF ( CHR_SIMLR( CMD, 'RETYP' ) ) THEN
            CALL TST_OUT( ' ', 'Change object type...',
     :                    STATUS )
            CALL TST_GETC( 'New object type', TYPE, STATUS )
            CALL DAT_RETYP( LOC( II ), TYPE, STATUS )

*  Set mapped array values.
         ELSE IF ( CHR_SIMLR( CMD, 'SET' ) ) THEN
            CALL TST_OUT( ' ', 'Set mapped array values...',
     :                    STATUS )
            IF ( .NOT. MAP( II ) ) THEN
               CALL TST_OUT( ' ', 'Array not mapped', STATUS )
            ELSE
               CALL TST_GETR( 'Starting value', START, STATUS )
               CALL TST_GETR( 'Increment', INC, STATUS )
               CALL SET( MTYPE( II ), START, INC, EL( II ), PNTR( II ),
     :                   STATUS )
            END IF

*  Enquire object shape.
         ELSE IF ( CHR_SIMLR( CMD, 'SHAPE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object shape...',
     :                    STATUS )
            CALL DAT_SHAPE( LOC( II ), DAT__MXDIM, DIM, NDIM, STATUS )
            DO I = 1, NDIM
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETI( 'DIM', DIM( I ) )
               CALL TST_OUT( ' ', 'Dimension ^I = ^DIM', STATUS )
            END DO

*  Enquire object size.
         ELSE IF ( CHR_SIMLR( CMD, 'SIZE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object size...',
     :                    STATUS )
            CALL DAT_SIZE( LOC( II ), EL, STATUS )
            CALL MSG_SETI( 'EL', EL )
            CALL TST_OUT( ' ', 'Object size = ^EL', STATUS )

*  Locate slice.
         ELSE IF ( CHR_SIMLR( CMD, 'SLICE' ) ) THEN
            CALL TST_OUT( ' ', 'Locate slice...',
     :                    STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Lower bound', LBND( I ), STATUS )
               CALL TST_GETI( 'Upper bound', UBND( I ), STATUS )
            END DO
            CALL DAT_SLICE( LOC( II ), NDIM, LBND, UBND, LOC( NI + 1 ),
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Enquire object state.
         ELSE IF ( CHR_SIMLR( CMD, 'STATE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object state...',
     :                    STATUS )
            CALL DAT_STATE( LOC( II ), REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Object state = ^REPLY', STATUS )

*  Enquire object structured.
         ELSE IF ( CHR_SIMLR( CMD, 'STRUC' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object structured...',
     :                    STATUS )
            CALL DAT_STRUC( LOC( II ), REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Object structured = ^REPLY', STATUS )

*  Create temporary object.
         ELSE IF ( CHR_SIMLR( CMD, 'TEMP' ) ) THEN
            CALL TST_OUT( ' ', 'Create temporary object...',
     :                    STATUS )
            CALL TST_GETC( 'Data type', TYPE, STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL DAT_TEMP( TYPE, NDIM, DIM, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Enquire component existence.
         ELSE IF ( CHR_SIMLR( CMD, 'THERE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire component existence...',
     :                    STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
            CALL DAT_THERE( LOC( II ), NAME, REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Component exists = ^REPLY', STATUS )

*  Set tuning parameter.
         ELSE IF ( CHR_SIMLR( CMD, 'TUNE' ) ) THEN
            CALL TST_OUT( ' ', 'Set tuning parameter...',
     :                    STATUS )
            CALL TST_GETC( 'Parameter', PARAM, STATUS )
            CALL TST_GETI( 'New value', VALUE, STATUS )
            CALL DAT_TUNE( PARAM, VALUE, STATUS )

*  Enquire object type.
         ELSE IF ( CHR_SIMLR( CMD, 'TYPE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire object type...',
     :                    STATUS )
            CALL DAT_TYPE( LOC( II ), TYPE, STATUS )
            CALL MSG_SETC( 'TYPE', TYPE )
            CALL TST_OUT( ' ', 'Data type = ''^TYPE''', STATUS )

*  Unmap object.
         ELSE IF ( CHR_SIMLR( CMD, 'UNMAP' ) ) THEN
            CALL TST_OUT( ' ', 'Unmap object...',
     :                    STATUS )
            CALL DAT_UNMAP( LOC( II ), STATUS )
            MAP( II ) = .FALSE.

*  Enquire locator valid.
         ELSE IF ( CHR_SIMLR( CMD, 'VALID' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire locator valid...',
     :                    STATUS )
            CALL DAT_VALID( LOC( II ), REPLY, STATUS )
            CALL MSG_SETL( 'REPLY', REPLY )
            CALL TST_OUT( ' ', 'Locator valid = ^REPLY', STATUS )

*  Vectorise object.
         ELSE IF ( CHR_SIMLR( CMD, 'VEC' ) ) THEN
            CALL TST_OUT( ' ', 'Vectorise object...',
     :                    STATUS )
            CALL DAT_VEC( LOC( II ), LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Determine where object is stored.
         ELSE IF ( CHR_SIMLR( CMD, 'WHERE' ) ) THEN
            CALL TST_OUT( ' ', 'Determine where object is stored...',
     :                    STATUS )
            CALL DAT_WHERE( LOC( II ), BLOCK, BYTOFF, STATUS )
            CALL MSG_SETI( 'BLOCK', BLOCK )
            CALL MSG_SETI( 'BYTOFF', BYTOFF )
            CALL TST_OUT( ' ',
     :      'Object is stored in block ^BLOCK at offset ^BYTOFF',
     :                    STATUS )

*  Close container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_CLOSE' ) ) THEN
            CALL TST_OUT( ' ', 'Close container file...',
     :                    STATUS )
            CALL HDS_CLOSE( LOC( II ), STATUS )

*  Copy object to container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_COPY' ) ) THEN
            CALL TST_OUT( ' ', 'Copy object to container file...',
     :                    STATUS )
            CALL TST_GETC( 'Container file name', FILE, STATUS )
            CALL TST_GETC( 'Component name', NAME, STATUS )
c      call lib$init_timer
            CALL HDS_COPY( LOC( II ), FILE, NAME, STATUS )
c      call lib$show_timer
            CALL HDS_OPEN( FILE, 'UPDATE', LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Erase container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_ERASE' ) ) THEN
            CALL TST_OUT( ' ', 'Erase container file...',
     :                    STATUS )
            CALL HDS_ERASE( LOC( II ), STATUS )

*  End wild-card search for container files.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_EWILD' ) ) THEN
            CALL TST_OUT( ' ', 'End a wild-card search for ' //
     :      'container files...',
     :                    STATUS )
            CALL TST_GETI( 'Search context ID', IWLD, STATUS )
            CALL HDS_EWILD( IWLD, STATUS )

*  Flush locator group.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_FLUSH' ) ) THEN
            CALL TST_OUT( ' ', 'Flush locator group...',
     :                    STATUS )
            CALL TST_GETC( 'Name of group', GROUP, STATUS )
            CALL HDS_FLUSH( GROUP, STATUS )

*  Free container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_FREE' ) ) THEN
            CALL TST_OUT( ' ', 'Free container file...',
     :                    STATUS )
            CALL HDS_FREE( LOC( II ), STATUS )

*  Enquire locator group.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_GROUP' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire locator group...',
     :                    STATUS )
            CALL HDS_GROUP( LOC( II ), GROUP, STATUS )
            CALL MSG_SETC( 'GROUP', GROUP )
            CALL TST_OUT( ' ', 'Group = ''^GROUP''', STATUS )

*  Get value of tuning setting.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_GTUNE' ) ) THEN
            CALL TST_OUT( ' ', 'Get value of tuning setting...',
     :                    STATUS )
            CALL TST_GETC( 'Parameter', PARAM, STATUS )
            CALL HDS_GTUNE( PARAM, VALUE, STATUS )
            CALL MSG_SETI( 'VALUE', VALUE )
            CALL TST_OUT( ' ', 'Value of tuning setting = ^VALUE',
     :                    STATUS )

*  Link locator into group.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_LINK' ) ) THEN
            CALL TST_OUT( ' ', 'Link locator into group...',
     :                    STATUS )
            CALL TST_GETC( 'Group name', GROUP, STATUS )
            CALL HDS_LINK( LOC( II ), GROUP, STATUS )

*  Lock container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_LOCK' ) ) THEN
            CALL TST_OUT( ' ', 'Lock container file...',
     :                    STATUS )
            CALL HDS_LOCK( LOC( II ), STATUS )

*  Create new container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_NEW' ) ) THEN
            CALL TST_OUT( ' ', 'Create new container file...',
     :                    STATUS )
            CALL TST_GETC( 'Container file name', FILE, STATUS )
            CALL TST_GETC( 'Object name', NAME, STATUS )
            CALL TST_GETC( 'Object type', TYPE, STATUS )
            CALL TST_GETI( 'Number of dimensions', NDIM, STATUS )
            DO I = 1, NDIM
               CALL TST_GETI( 'Dimension', DIM( I ), STATUS )
            END DO
            CALL HDS_NEW( FILE, NAME, TYPE, NDIM, DIM, LOC( NI + 1 ),
     :                    STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Open an HDS container file.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_OPEN' ) ) THEN
            CALL TST_OUT( ' ', 'Open an HDS container file...',
     :                    STATUS )
            CALL TST_GETC( 'Container file name', FILE, STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL HDS_OPEN( FILE, MODE, LOC( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI

*  Run application subroutine.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_RUN' ) ) THEN
            CALL TST_OUT( ' ', 'Run application subroutine...',
     :                    STATUS )
            CALL HDS_RUN( APP, STATUS )

*  Show HDS statistics.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_SHOW' ) ) THEN
            CALL TST_OUT( ' ', 'Show HDS statistics...',
     :                    STATUS )
            CALL TST_GETC( 'Statistic to show', TOPIC, STATUS )
            CALL HDS_SHOW( TOPIC, STATUS )

*  Startup locator facility.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_START' ) ) THEN
            CALL TST_OUT( ' ', 'Startup locator facility...',
     :                    STATUS )
            CALL HDS_START( STATUS )

*  Enquire HDS state.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_STATE' ) ) THEN
            CALL TST_OUT( ' ', 'Enquire HDS state...',
     :                    STATUS )
            CALL HDS_STATE( STATE, STATUS )
            CALL MSG_SETL( 'STATE', STATE )
            CALL TST_OUT( ' ', 'HDS active = ^STATE', STATUS )

*  Rundown locator facility.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_STOP' ) ) THEN
            CALL TST_OUT( ' ', 'Rundown locator facility...',
     :                    STATUS )
            CALL HDS_STOP( STATUS )

*  Trace object path.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_TRACE' ) ) THEN
            CALL TST_OUT( ' ', 'Trace object path...',
     :                    STATUS )
            CALL HDS_TRACE( LOC( II ), NLEV, PATH, FILE, STATUS )
            CALL MSG_SETI( 'NLEV', NLEV )
            CALL TST_OUT( ' ', 'Nesting level = ^NLEV', STATUS )
            CALL MSG_SETC( 'PATH', PATH )
            CALL TST_OUT( ' ', 'Path name = ''^PATH''', STATUS )
            CALL MSG_SETC( 'FILE', FILE )
            CALL TST_OUT( ' ', 'File name = ''^FILE''', STATUS )

*  Tune HDS parameter.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_TUNE' ) ) THEN
            CALL TST_OUT( ' ', 'Tune HDS parameter...',
     :                    STATUS )
            CALL TST_GETC( 'Parameter', PARAM, STATUS )
            CALL TST_GETI( 'New value', VALUE, STATUS )
            CALL HDS_TUNE( PARAM, VALUE, STATUS )

*  Perform wild-card search for container files.
         ELSE IF ( CHR_SIMLR( CMD, 'HDS_WILD' ) ) THEN
            CALL TST_OUT( ' ', 'Perform wild-card search for ' //
     :      'container files...',
     :                    STATUS )
            CALL TST_GETC( 'File spec', FSPEC, STATUS )
            CALL TST_GETC( 'Access mode', MODE, STATUS )
            CALL TST_OUT( ' ', 'Container files found...', STATUS )
            IWLD = 0
            LOC( NI + 1 ) = DAT__NOLOC
 110        CONTINUE             ! Start of 'DO WHILE' loop
            CALL HDS_WILD( FSPEC, MODE, IWLD, LOCTMP, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            CALL MSG_SETI( 'IWLD', IWLD )
            CALL MSG_OUT( ' ', '   Context = ^IWLD', STATUS )
            IF ( LOCTMP .NE. DAT__NOLOC ) THEN
               CALL DAT_NAME( LOCTMP, NAME, STATUS )
               CALL MSG_SETC( 'NAME', NAME )
               CALL TST_OUT( ' ', '   Object = ^NAME', STATUS )
               CALL HDS_TRACE( LOCTMP, NLEV, PATH, FILE, STATUS )
               CALL MSG_SETC( 'FILE', FILE )
               CALL MSG_OUT( ' ', '   ...in file ^FILE', STATUS )
               IF ( LOC( NI + 1 ) .NE. DAT__NOLOC )
     :            CALL DAT_ANNUL( LOC( NI + 1 ), STATUS )
               LOC( NI + 1 ) = LOCTMP
               LOCTMP = DAT__NOLOC
               GO TO 110
            END IF
            NI = NI + 1
            II = NI
            CALL HDS_EWILD( IWLD, STATUS )
            IF ( IWLD .EQ. DAT__NOWLD ) THEN
               CALL MSG_SETC( 'IWLD', 'DAT__NOWLD' )
            ELSE
               CALL MSG_SETI( 'IWLD', IWLD )
            END IF
            CALL TST_OUT( ' ', '   Annulled context = ^IWLD', STATUS )

*  This section will never execute and the routines appearing here do
*  not currently have tests. It exists simply to ensure that all HDS
*  routines are linked as a check on their existence.
         ELSE IF ( CHR_SIMLR ( '0', '1' ) ) THEN
            CALL CMP_GET0C
            CALL CMP_GET0D
            CALL CMP_GET0I
            CALL CMP_GET0L
            CALL CMP_GET0R
            CALL CMP_GET1C
            CALL CMP_GET1D
            CALL CMP_GET1I
            CALL CMP_GET1L
            CALL CMP_GET1R
            CALL CMP_GETNC
            CALL CMP_GETND
            CALL CMP_GETNI
            CALL CMP_GETNL
            CALL CMP_GETNR
            CALL CMP_GETVC
            CALL CMP_GETVD
            CALL CMP_GETVI
            CALL CMP_GETVL
            CALL CMP_GETVR
            CALL CMP_LEN
            CALL CMP_MAPN
            CALL CMP_MAPV
            CALL CMP_MOD
            CALL CMP_MODC
            CALL CMP_PRIM
            CALL CMP_PUT0C
            CALL CMP_PUT0D
            CALL CMP_PUT0I
            CALL CMP_PUT0L
            CALL CMP_PUT0R
            CALL CMP_PUT1C
            CALL CMP_PUT1D
            CALL CMP_PUT1I
            CALL CMP_PUT1L
            CALL CMP_PUT1R
            CALL CMP_PUTNC
            CALL CMP_PUTND
            CALL CMP_PUTNI
            CALL CMP_PUTNL
            CALL CMP_PUTNR
            CALL CMP_PUTVC
            CALL CMP_PUTVD
            CALL CMP_PUTVI
            CALL CMP_PUTVL
            CALL CMP_PUTVR
            CALL CMP_SHAPE
            CALL CMP_SIZE
            CALL CMP_STRUC
            CALL CMP_TYPE
            CALL CMP_UNMAP

            CALL DAT_GET
            CALL DAT_GETC
            CALL DAT_GETD
            CALL DAT_GETI
            CALL DAT_GETL
            CALL DAT_GETR
            CALL DAT_GET0C
            CALL DAT_GET0D
            CALL DAT_GET0I
            CALL DAT_GET0L
            CALL DAT_GET0R
            CALL DAT_GET1C
            CALL DAT_GET1D
            CALL DAT_GET1I
            CALL DAT_GET1L
            CALL DAT_GET1R
            CALL DAT_GETNC
            CALL DAT_GETND
            CALL DAT_GETNI
            CALL DAT_GETNL
            CALL DAT_GETNR
            CALL DAT_GETVC
            CALL DAT_GETVD
            CALL DAT_GETVI
            CALL DAT_GETVL
            CALL DAT_GETVR
            CALL DAT_MAPC
            CALL DAT_MAPD
            CALL DAT_MAPI
            CALL DAT_MAPL
            CALL DAT_MAPR
            CALL DAT_MAPN
            CALL DAT_MAPV
            CALL DAT_NEW0C
            CALL DAT_NEW0D
            CALL DAT_NEW0I
            CALL DAT_NEW0L
            CALL DAT_NEW0R
            CALL DAT_NEW1C
            CALL DAT_NEW1D
            CALL DAT_NEW1I
            CALL DAT_NEW1L
            CALL DAT_NEW1R
            CALL DAT_NEWC
            CALL DAT_PUT
            CALL DAT_PUT0C
            CALL DAT_PUT0D
            CALL DAT_PUT0I
            CALL DAT_PUT0L
            CALL DAT_PUT0R
            CALL DAT_PUT1C
            CALL DAT_PUT1D
            CALL DAT_PUT1I
            CALL DAT_PUT1L
            CALL DAT_PUT1R
            CALL DAT_PUTNC
            CALL DAT_PUTND
            CALL DAT_PUTNI
            CALL DAT_PUTNL
            CALL DAT_PUTNR
            CALL DAT_PUTVC
            CALL DAT_PUTVD
            CALL DAT_PUTVI
            CALL DAT_PUTVL
            CALL DAT_PUTVR
         ENDIF

 66      IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ERMSG( STATUS, LEN, MESS )
            CALL MSG_SETC( 'MESSAGE', MESS )
            CALL ERR_REP( ' ', '^MESSAGE', STATUS )
            CALL TST_FLUSH( STATUS )
         END IF

         CALL TST_BLANK( STATUS )
         CALL DAT_VALID( LOC( II ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL HDS_TRACE( LOC( II ), NLEV, PATH, FILE, STATUS )
            CALL MSG_SETI( 'II', II )
            CALL MSG_SETC( 'PATH', PATH )
            CALL MSG_SETI( 'NLEV', NLEV )
            CALL TST_OUT( ' ',
     :      'Current object is no. ^II: ^PATH, nested at level ^NLEV',
     :      STATUS )

*  N.B. Don't echo machine-dependent file name to test results file.
            CALL MSG_SETC( 'FILE', FILE )
            CALL MSG_OUT( ' ',
     :      '   in the file ^FILE', STATUS )

         ELSE
            IF ( LOC( II ) .EQ. DAT__NOLOC ) THEN
               CALL MSG_SETI( 'II', II )
               CALL MSG_SETC( 'VALUE', DAT__NOLOC )
               CALL TST_OUT( ' ',
     :         'Current locator is no. ^II, value is ''^VALUE'' ' //
     :         '(not valid).', STATUS )

*  N.B. Eliminate machine-dependent locator values to make regression
*  testing easier.
            ELSE
               CALL MSG_SETI( 'II', II )
               CALL TST_OUT( ' ',
     :         'Current locator is no. ^II, value is ''<junk>'' ' //
     :         '(not valid).', STATUS )
            END IF
         END IF
         CALL TST_BLANK( STATUS )

         CALL ERR_LEVEL( LEVEL2 )
         IF ( LEVEL2 .NE. LEVEL1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'LEVEL1', LEVEL1 )
            CALL MSG_SETI( 'LEVEL2', LEVEL2 )
            CALL ERR_REP( ' ', '*** Error in message context level '//
     :                         'detected; initial level was ' //
     :                         '^LEVEL1, current level is ^LEVEL2',
     :                         STATUS )
            CALL TST_FLUSH( STATUS )
         END IF
      
         GO TO 1
      END IF

 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_SYSER( 'MESSAGE', STATUS )
         CALL TST_FLUSH( STATUS )
      END IF

      CALL TST_END

      END

      SUBROUTINE APP( STATUS )
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      LOGICAL STATE
      
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL HDS_STATE( STATE, STATUS )
      CALL MSG_SETL( 'STATE', STATE )
      CALL TST_OUT( ' ', 'HDS state in application = ^STATE', STATUS )

      END
      
      SUBROUTINE TST_GETC( PROMPT, CVALUE, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) CVALUE
      INTEGER NC
      INTEGER STATUS

      INTEGER CHR_LEN

      IF ( STATUS .NE. SAI__OK ) RETURN
      WRITE( *, '( 1X, A, '': '', $ )' ) PROMPT

      STATUS = SAI__ERROR
      READ( *, '( A )', END = 10 ) CVALUE
      STATUS = SAI__OK
 10   CONTINUE

      IF ( STATUS .EQ. SAI__OK ) THEN
         NC = CHR_LEN( CVALUE )

*  Echo input to the input log file.
         IF ( NC .EQ. 0 ) THEN
            WRITE( 12, * )
         ELSE
            WRITE( 12, '( A )' ) CVALUE( : NC )
         END IF

*  Echo what appears on the input screen to the benchmark file.
         IF ( NC .EQ. 0 ) THEN
            WRITE( 13, '( A, '': '' )' ) PROMPT
         ELSE
            WRITE( 13, '( A, '': '', A )' ) PROMPT, CVALUE( : NC )
         END IF
      ENDIF
      END

      SUBROUTINE TST_GETI( PROMPT, IVALUE, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      CHARACTER * ( * ) PROMPT
      INTEGER IVALUE
      INTEGER NC
      CHARACTER * ( 20 ) CVALUE
      INTEGER STATUS

      IF ( STATUS .NE. SAI__OK ) RETURN
      WRITE( *, '( 1X, A, '': '', $ )' ) PROMPT
      READ( *, * ) IVALUE

      CALL CHR_ITOC( IVALUE, CVALUE, NC )

*  Echo input to the input log file and what appears on the input
*  screen to the benchmark file.
      WRITE( 12, '( A )' ) CVALUE( : NC )
      WRITE( 13, '( A, '': '', A )' ) PROMPT, CVALUE( : NC )

      END

      SUBROUTINE TST_GETR( PROMPT, RVALUE, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      CHARACTER * ( * ) PROMPT
      REAL RVALUE
      INTEGER NC
      CHARACTER * ( 20 ) CVALUE
      INTEGER STATUS

      IF ( STATUS .NE. SAI__OK ) RETURN
      WRITE( *, '( 1X, A, '': '', $ )' ) PROMPT
      READ( *, * ) RVALUE

      CALL CHR_RTOC( RVALUE, CVALUE, NC )

*  Echo input to the input log file and what appears on the input
*  screen to the benchmark file.
      WRITE( 12, '( A )' ) CVALUE( : NC )
      WRITE( 13, '( A, '': '', A )' ) PROMPT, CVALUE( : NC )

      END

      SUBROUTINE TST_OUT( PARAM, TEXT, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TEXT
      INTEGER STATUS
      CHARACTER * ( MSG__SZMSG ) MSG
      INTEGER NC
      INTEGER I1, I2
      
      CALL MSG_LOAD( PARAM, TEXT, MSG, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NC .EQ. 0 ) THEN
            WRITE( *, * )
            WRITE( 13, * )
         ELSE
c            DO 1 I1 = 1, NC, 80
c               I2 = MIN( I1 + 79, NC )
c               WRITE( *, '( 1X, A )' ) MSG( I1 : I2 )
c               WRITE( 13, '( A )' ) MSG( I1 : I2 )
c 1          CONTINUE           
            WRITE( *, '( A )' ) MSG( : NC )
            WRITE( 13, '( A )' ) MSG( : NC )
         END IF
      END IF

      END
      
      SUBROUTINE TST_BLANK( STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      IF ( STATUS .EQ. SAI__OK ) THEN
         WRITE( *, * )
         WRITE( 13, * )
      END IF
      END
      
      SUBROUTINE TST_INIT
      IMPLICIT NONE
      OPEN( UNIT = 12, FILE = 'exercise.input', STATUS = 'UNKNOWN',
     : CARRIAGECONTROL ='LIST' )
      OPEN( UNIT = 13, FILE = 'exercise.result', STATUS = 'UNKNOWN',
     : CARRIAGECONTROL ='LIST' )
C  NB Workaround for EMS bug.
      CALL ERR_MARK
      CALL ERR_MARK
      END

      SUBROUTINE TST_END
C  NB Workaround for EMS bug.
      CALL ERR_RLSE
      CALL ERR_RLSE
      CLOSE( UNIT = 12 )
      CLOSE( UNIT = 13 )
      END

      SUBROUTINE TST_FLUSH( STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ERR_PAR'
      INTEGER STATUS
      CHARACTER * ( ERR__SZPAR ) MSGNAM
      INTEGER LNAM
      CHARACTER * ( ERR__SZMSG ) MSG
      INTEGER NC, I1, I2
      
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      CALL ERR_LOAD( MSGNAM, LNAM, MSG, NC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( NC .EQ. 0 ) THEN
            WRITE( *, * )
            WRITE( 13, * )
         ELSE
C            DO 2 I1 = 1, NC, 80
C               I2 = MIN( I1 + 79, NC )
C               WRITE( *, '( 1X, A )' ) MSG( I1 : I2 )
C               WRITE( 13, '( A )' ) MSG( I1 : I2 )
C 2          CONTINUE           
            WRITE( *, '( A )' ) MSG( : NC )
            WRITE( 13, '( A )' ) MSG( : NC )
         END IF
         GO TO 1
      END IF     
      END

      SUBROUTINE WRN( TYPE, NDIM, DIM, PNTR, STATUS, LENPTR )
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      CHARACTER * ( * ) TYPE
      CHARACTER * ( DAT__SZNAM ) UTYPE
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER PNTR
      INTEGER STATUS
      INTEGER LENPTR
      IF ( STATUS .NE. SAI__OK ) RETURN

      UTYPE = TYPE
      CALL CHR_UCASE( UTYPE )

      IF ( UTYPE .EQ. '_BYTE' ) THEN
         CALL WRNB( NDIM, DIM, %VAL( PNTR ), STATUS )
 
      ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
         CALL WRNUB( NDIM, DIM, %VAL( PNTR ), STATUS )
 
      ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
         CALL WRND( NDIM, DIM, %VAL( PNTR ), STATUS )
 
      ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
         CALL WRNI( NDIM, DIM, %VAL( PNTR ), STATUS )
 
      ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
         CALL WRNR( NDIM, DIM, %VAL( PNTR ), STATUS )
 
      ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
         CALL WRNW( NDIM, DIM, %VAL( PNTR ), STATUS )

      ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
         CALL WRNUW( NDIM, DIM, %VAL( PNTR ), STATUS )

      ELSE IF ( UTYPE .EQ. '_LOGICAL' ) THEN
         CALL WRNL( NDIM, DIM, %VAL( PNTR ), STATUS )

      ELSE IF ( UTYPE( : 5 ) .EQ. '_CHAR' ) THEN
         CALL WRNC( NDIM, DIM, %VAL( PNTR ), STATUS, %VAL( LENPTR ) )
      END IF

      END

      SUBROUTINE WRNB( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRB( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRNUB( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRUB( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRND( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      DOUBLE PRECISION ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRD( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRNI( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRI( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRNR( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      REAL ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRR( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRNW( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRW( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END
 
      SUBROUTINE WRNUW( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRUW( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END

      SUBROUTINE WRNL( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      LOGICAL ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRL( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END

      SUBROUTINE WRNC( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
 
      INTEGER NDIM
      INTEGER DIM( NDIM )
      CHARACTER * ( * ) ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE
 
      DO 2 I = 1, N
         CALL WRC( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         CALL TST_BLANK( STATUS )
2     CONTINUE
 
      END

      SUBROUTINE WRB( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      BYTE ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETI( 'MSG', NUM_BTOI( ARRAY( I ) ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRUB( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      BYTE ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETI( 'MSG', NUM_UBTOI( ARRAY( I ) ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRI( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      INTEGER ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETI( 'MSG', ARRAY( I ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRW( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      INTEGER*2 ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETI( 'MSG', NUM_WTOI( ARRAY( I ) ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRUW( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      INTEGER*2 ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETI( 'MSG', NUM_UWTOI( ARRAY( I ) ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRR( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      REAL ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            IF ( ARRAY( I ) .EQ. 0.0 ) THEN
               CALL MSG_SETC( 'MSG', '0' )
            ELSE
               CALL MSG_SETR( 'MSG', ARRAY( I ) )
            END IF
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRD( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            IF ( ARRAY( I ) .EQ. 0.0D0 ) THEN
               CALL MSG_SETC( 'MSG', '0' )
            ELSE
               CALL MSG_SETD( 'MSG', ARRAY( I ) )
            END IF
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END
      SUBROUTINE WRL( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      LOGICAL ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETL( 'MSG', ARRAY( I ) )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END

      SUBROUTINE WRC( N, ARRAY, STATUS )
*+
*{subroutine_prologue}
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given:
      INTEGER N
      CHARACTER * ( * ) ARRAY( * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF(N.GT.0)THEN
         DO 1 I = 1, MIN( N, MSG__SZMSG )
            IF ( I .GT. 1 ) CALL MSG_SETC( 'MSG', ' ' )
            CALL MSG_SETC( 'MSG', '''' )
            CALL MSG_SETC( 'MSG', ARRAY( I ) )
            CALL MSG_SETC( 'MSG', '''' )
 1       CONTINUE        
         CALL TST_OUT( ' ', '^MSG', STATUS )
      ENDIF
 
      END

      SUBROUTINE SET( TYPE, START, INC, EL, PNTR, STATUS )
      INCLUDE 'SAE_PAR'
      CHARACTER * ( * ) TYPE
      REAL START, INC
      INTEGER EL, PNTR, STATUS
      LOGICAL CHR_SIMLR

      IF ( STATUS .NE. SAI__OK ) RETURN
      
      IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         CALL SETB( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         CALL SETUB( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         CALL SETD( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         CALL SETI( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         CALL SETR( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         CALL SETW( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         CALL SETUW( START, INC, EL, %VAL( PNTR ), STATUS )
 
      ENDIF

      END

      SUBROUTINE SETB( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOB( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETUB( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOUB( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETD( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      DOUBLE PRECISION ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOD( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETI( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOI( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETR( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      REAL ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOR( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETW( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOW( START + INC * REAL( I -1 ) )
1     CONTINUE
 
      END
 
      SUBROUTINE SETUW( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER I

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      DO 1 I = 1, EL
         ARRAY( I ) = NUM_RTOUW( START + INC * REAL( I -1 ) )
         STATUS = SAI__OK
1     CONTINUE
 
      END
