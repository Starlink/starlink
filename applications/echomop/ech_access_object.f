      SUBROUTINE ECH_ACCESS_OBJECT(
     :           LOBJECT,
     :           OPERATION,
     :           TYPE,
     :           NELEMENTS,
     :           ADDRESS,
     :           HANDLE,
     :           LDIMS,
     :           LMAX_DIMS,
     :           LNUM_DIM,
     :           IO_STRING,
     :           STATUS
     :          )
*+
*  Name:
*     ECH_ACCESS_OBJECT

*  Purpose:
*     This routine performs low-level data object access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ECH_ACCESS_OBJECT(
*     :     LOBJECT,
*     :     OPERATION,
*     :     TYPE,
*     :     NELEMENTS,
*     :     ADDRESS,
*     :     HANDLE,
*     :     LDIMS,
*     :     LMAX_DIMS,
*     :     LNUM_DIM,
*     :     IO_STRING,
*     :     STATUS
*     :     )

*  Arguments:
*     LOBJECT = CHARACTER*( * ) (Given)
*        Name of the object to be accessed.
*     OPERATION = CHARACTER*( * ) (Given)
*        Operation to be performed on specified object.
*     TYPE = CHARACTER*( * ) (Given or Returned)
*        Type of the object accessed.
*     NELEMENTS = INTEGER ()
*
*     ADDRESS = INTEGER ()
*
*     HANDLE = INTEGER ()
*
*     LMAX_DIMS = INTEGER ()
*
*     LDIMS = INTEGER( LMAX_DIMS ) ()
*
*     LNUM_DIM = INTEGER ()
*
*     IO_STRING = CHARACTER*( * ) ()
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1991 (DMILLS):
*       Original Version.
*     13-MAR-1995 (MJC):
*       New prologue, tidy up, comment.
*     14-MAR-1995 (MJC):
*       Early loop exits, removed NDF_IMPRT calls.
*       Fixed faulty NDF_SBAD call.
*     23-JUL-1996 (MJC):
*       NDF_SQMF calls added for much faster access as ECHOMOP
*       does all quality processing internally.
*     25-JUL-1996 (MJC):
*       Moved immediate DATA for F77 compliance.
*     07-OCT-1996 (MJC):
*       Changed Access Log file OPEN to UNKNOWN status.
*     20-MAR-1997 (MJC):
*       Major changes to open files using NDF rather than HDS.
*       Implemented as part of preparation for IRAF interoperability.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRM constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_USE_DIMEN.INC'
      INCLUDE 'ECH_CONTEXT.INC'

*  Arguments:
      CHARACTER*( * ) LOBJECT
      CHARACTER*( * ) OPERATION
      CHARACTER*( * ) TYPE
      INTEGER NELEMENTS
      INTEGER ADDRESS
      INTEGER HANDLE
      INTEGER LMAX_DIMS
      INTEGER LDIMS( LMAX_DIMS )
      INTEGER LNUM_DIM
      CHARACTER*( * ) IO_STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAX_NDF_IDS
      PARAMETER ( MAX_NDF_IDS = 128 )

*  Local Variables:
      INTEGER LBOUND( MAX_DIMENSIONS )
      INTEGER HBOUND( MAX_DIMENSIONS )
      INTEGER TPTR( 2 )          ! Pointers to mapped components.
      INTEGER AUNIT
      INTEGER EL                 ! Number of elements in NDF_MAP call.
      INTEGER I
      INTEGER IDIM
      INTEGER IDOT               ! Index to dot "." in a string.
      INTEGER IEND
      INTEGER ILLEN              ! Length of Object name string.
      INTEGER INDF
      INTEGER PLACE
      INTEGER ISTAT
      INTEGER PNTR
      INTEGER THENDF
      INTEGER TYPE_CODE
      INTEGER VM_SIZE
      INTEGER XLEN

      LOGICAL SUCCESS
      LOGICAL LBAD
      LOGICAL ISACC
      LOGICAL SUPRESS_ERROR

      CHARACTER*255 XLATE
      CHARACTER*80 OBJECT
      CHARACTER*80 TMP_STRING
      CHARACTER*( DAT__SZLOC ) LOC
      CHARACTER*( NDF__SZMMD ) MODE
      CHARACTER*( NDF__SZFTP ) LTYPE

*  Local Static Variables:
      LOGICAL DIAG_ACCESS
      COMMON / DIAG_ACCESSES / DIAG_ACCESS

      INTEGER NDF_IDS( MAX_NDF_IDS )
      INTEGER ACTIVE_NDFS
      COMMON / ECHO_NDF_1 / NDF_IDS, ACTIVE_NDFS

      CHARACTER*16 NDF_PARMS( MAX_NDF_IDS )
      CHARACTER*( DAT__SZLOC ) NDF_LOCS( MAX_NDF_IDS )
      CHARACTER*80 NDF_FILES( MAX_NDF_IDS )
      COMMON / ECHO_NDF_2 / NDF_PARMS, NDF_LOCS, NDF_FILES

*  External Functions:
      INTEGER ECH_WORD_LEN   ! Word length.

*  Data statements:
      DATA LBOUND / 1, 1, 1, 1 /
      DATA HBOUND / 1, 1, 1, 1 /
      DATA ACTIVE_NDFS / 0 /
      DATA DIAG_ACCESS / .FALSE. /
*.

*  Zero-out INTEGERs.
      INDF = 0
      ISTAT = 0
      THENDF = 0

*  Zero-out LOGICALs.
      SUCCESS = .FALSE.
      LBAD = .FALSE.
      ISACC = .FALSE.
      SUPRESS_ERROR = .FALSE.

*  Look for first dot "." in Object name string.
*  This terminates the first sub-field of the Object name.
      ILLEN = ECH_WORD_LEN( LOBJECT )
      DO I = 1, ILLEN
         IF ( LOBJECT( I:I ) .EQ. '.' ) THEN
            IDOT = I
            GO TO 10
         END IF
      END DO
      IDOT = 0
 10   CONTINUE

      IF ( IDOT .GT. 0 ) THEN
         IEND = IDOT - 1

      ELSE
         IEND = ILLEN
      END IF
      DO I = 1, ACTIVE_NDFS
         IF ( LOBJECT( :IEND ).EQ.
     :        NDF_PARMS( I )( :IEND ) ) THEN
            INDF = NDF_IDS( I )
            LOC = NDF_LOCS( I )
            THENDF = I
            GO TO 50
         END IF
      END DO
 50   CONTINUE

*  Read Object Data Type
*  =====================
      IF ( OPERATION .EQ. 'READ-TYPE' ) THEN
         STATUS = SAI__OK

*     Find object name in string.
         CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT, ILLEN, IEND,
     :        IDIM )

*     Get object type.
         CALL ECH_READ_TYPE( LOBJECT( IDOT+1:IEND ), LOC,
     :        ( LOBJECT( :9 ) .EQ. 'ECH_RDUCD' ), TYPE, STATUS )
         GO TO 900
      END IF


*  Zero-out Memory for Object
*  ==========================
      IF ( OPERATION .EQ. 'ZERO' ) THEN
         CALL ECH_TYPEINFO( TYPE, TYPE_CODE, AUNIT )
         CALL ECH_ZERO_INIT( NELEMENTS, AUNIT, %VAL( ADDRESS ) )


*  Get Object Size
*  ===============
      ELSE IF ( OPERATION .EQ. 'READ-SIZE' ) THEN
         STATUS = SAI__OK
         DO I = 1, LMAX_DIMS
            LDIMS( I ) = 0
         END DO

*     If this is an image, get image dimensions.
         IF ( TYPE .EQ. 'IMAGE-DATA' ) THEN
            CALL NDF_DIM( INDF, LMAX_DIMS, LDIMS, LNUM_DIM, STATUS )

*     Get object dimensions from string.
         ELSE

*        Get object name.
            CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT, ILLEN, IEND,
     :           IDIM )
            OBJECT = LOBJECT( IDOT + 1 : IEND )

*        Handle objects in a reduction database.
            IF ( LOBJECT( :9 ) .EQ. 'ECH_RDUCD' ) THEN

*           Data array.
               IF ( OBJECT .EQ. 'DATA_ARRAY' ) THEN
                  CALL NDF_DIM( INDF, LMAX_DIMS, LDIMS, LNUM_DIM,
     :                 STATUS )
                  IF ( LDIMS( 1 ) .EQ. 1 ) THEN
                     STATUS = 12345
                  END IF

*           Variance array.
               ELSE IF ( OBJECT .EQ. 'VARIANCE' ) THEN
                  CALL NDF_DIM( INDF, LMAX_DIMS, LDIMS, LNUM_DIM,
     :                 STATUS )

*           X-axis data.
               ELSE IF ( OBJECT .EQ. 'XAXIS' ) THEN
                  CALL NDF_DIM( INDF, LMAX_DIMS, LDIMS, LNUM_DIM,
     :                 STATUS )
                  LNUM_DIM = 1

*           Y-axis data.
               ELSE IF ( OBJECT .EQ. 'YAXIS' ) THEN
                  CALL NDF_DIM( INDF, LMAX_DIMS, LDIMS, LNUM_DIM,
     :                 STATUS )
                  LDIMS( 1 ) = LDIMS( 2 )
                  LNUM_DIM = 1

*           X-axis label.
               ELSE IF ( OBJECT .EQ. 'XLABEL' ) THEN
                  CALL NDF_ACLEN( INDF, 'LABEL', 1, LDIMS, STATUS )
                  LNUM_DIM = 1

*           Y-axis label.
               ELSE IF ( OBJECT .EQ. 'YLABEL' ) THEN
                  CALL NDF_ACLEN( INDF, 'LABEL', 2, LDIMS, STATUS )
                  LNUM_DIM = 1

*           Title.
               ELSE IF ( OBJECT .EQ. 'ZLABEL' ) THEN
                  CALL NDF_CLEN( INDF, 'TITLE', LDIMS, STATUS )
                  LNUM_DIM = 1

*           X-axis units.
               ELSE IF ( OBJECT .EQ. 'XUNITS' ) THEN
                  CALL NDF_ACLEN( INDF, 'UNITS', 1, LDIMS, STATUS )
                  LNUM_DIM = 1

*           Y-axis units.
               ELSE IF ( OBJECT .EQ. 'YUNITS' ) THEN
                  CALL NDF_ACLEN( INDF, 'UNITS', 2, LDIMS, STATUS )
                  LNUM_DIM = 1

*           Handle other objects.
               ELSE
                  CALL CMP_SHAPE( LOC, OBJECT, LMAX_DIMS, LDIMS,
     :                 LNUM_DIM, STATUS )
               END IF

*        Handle unknown objects.
            ELSE
               CALL CMP_SHAPE( LOC, OBJECT, LMAX_DIMS, LDIMS, LNUM_DIM,
     :              STATUS )
            END IF
         END IF


*  Delete Object
*  =============
      ELSE IF ( OPERATION .EQ. 'DELETE' ) THEN
         IF ( TYPE .EQ. 'LUN' ) THEN
            CALL FIO_PUNIT( HANDLE, STATUS )

         ELSE
            STATUS = SAI__OK
            CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT, ILLEN,
     :           IEND, IDIM )
            CALL DAT_ERASE( LOC, LOBJECT( IDOT+1:IEND ), STATUS )
         END IF


*  Create Object
*  =============
      ELSE IF ( OPERATION .EQ. 'CREATE' ) THEN

*     Create a new container file.
         IF ( TYPE .EQ. 'REDUCTION' ) THEN
            STATUS = SAI__OK
            LOC = DAT__NOLOC
            CALL PAR_CANCL( LOBJECT, STATUS )
            XLATE = IO_STRING
            CALL ECH_PARSE_ENV( XLATE, XLEN )
            REPORT_STRING = ' Creating file: ' // XLATE( :XLEN ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL NDF_OPEN( DAT__ROOT, XLATE( :XLEN ), 'WRITE', 'NEW',
     :           INDF, PLACE, STATUS )
            CALL NDF_NEW( '_REAL', 2, LBOUND, HBOUND, PLACE, INDF,
     :           STATUS )
            CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE/ZERO',
     :           TPTR, EL, STATUS )
            CALL NDF_UNMAP( INDF, 'DATA', STATUS )
            CALL NDF_XNEW( INDF, 'ECHELLE', LOBJECT, 0, 0, LOC, STATUS )
            ACTIVE_NDFS = ACTIVE_NDFS + 1
            NDF_PARMS( ACTIVE_NDFS ) = LOBJECT
            NDF_IDS( ACTIVE_NDFS ) = INDF
            NDF_LOCS( ACTIVE_NDFS ) = LOC

*     Create extension in existing container file.
         ELSE IF ( TYPE .EQ. 'STRUCTURE' ) THEN
            STATUS = SAI__OK
            IF ( LOBJECT( :9 ) .NE. 'ECH_RDUCD' ) THEN
               DO I = 1, ACTIVE_NDFS
                  IF ( LOBJECT .EQ. NDF_PARMS( I ) ) THEN
                     CALL NDF_XNEW( NDF_IDS( I ), 'ECHELLE',
     :                    LOBJECT, 0, 0, LOC, STATUS )
                     NDF_LOCS( I ) = LOC
                     GO TO 100
                  END IF
               END DO
 100           CONTINUE
            END IF

*     Image (may have errors).
         ELSE IF ( TYPE( : 10 ) .EQ. 'IMAGE-DATA' ) THEN
            STATUS = SAI__OK
            CALL NDF_CREAT( LOBJECT, '_REAL', LNUM_DIM, LBOUND,
     :           LDIMS, INDF, STATUS )
            ACTIVE_NDFS = ACTIVE_NDFS + 1
            NDF_PARMS( ACTIVE_NDFS ) = LOBJECT
            NDF_IDS( ACTIVE_NDFS ) = INDF
            NDF_LOCS( ACTIVE_NDFS ) = DAT__NOLOC

*     Generate a new file handle.
         ELSE IF ( TYPE .EQ. 'LUN' ) THEN
            CALL FIO_GUNIT( HANDLE, STATUS )

*     Create other object.
         ELSE
            STATUS = SAI__OK
            IF ( TYPE .EQ. 'BYTE' ) THEN
               LTYPE = '_BYTE'

            ELSE IF ( TYPE .EQ. 'SHORT' ) THEN
               LTYPE = '_UWORD'

            ELSE IF ( TYPE .EQ. 'INT' ) THEN
               LTYPE = '_INTEGER'

            ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN
               LTYPE = '_REAL'

            ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
               LTYPE = '_DOUBLE'

            ELSE IF ( TYPE .EQ. 'CHAR' ) THEN
               LTYPE ='_CHAR'
            END IF

*        Get object dimensions.
            CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT,
     :           ILLEN, IEND, IDIM )
            OBJECT = LOBJECT( IDOT + 1 : IEND )

*        Deal with objects in result files.
            IF ( LOBJECT( :9 ) .EQ. 'ECH_RDUCD' ) THEN
               IF ( OBJECT .EQ. 'DATA_ARRAY' ) THEN
                  CALL NDF_SBND( IDIM, LBOUND, DIMENSIONS, INDF,
     :                 STATUS )
                  CALL NDF_MAP( INDF, 'DATA,VARIANCE', '_REAL',
     :                 'WRITE/ZERO', TPTR, EL, STATUS )
                  CALL NDF_UNMAP( INDF, 'DATA,VARIANCE', STATUS )
                  CALL NDF_XLOC( INDF, 'ECHELLE', 'WRITE', LOC, STATUS )
                  NDF_IDS( THENDF ) = INDF
                  NDF_LOCS( THENDF ) = LOC

               ELSE IF ( OBJECT .EQ. 'VARIANCE' ) THEN
                  CALL NDF_MAP( INDF, 'VARIANCE', '_REAL',
     :                 'WRITE/ZERO', TPTR, EL, STATUS )
                  CALL NDF_UNMAP( INDF, 'VARIANCE', STATUS )

               ELSE IF ( OBJECT .EQ. 'XAXIS' ) THEN
                  CONTINUE

               ELSE IF ( OBJECT .EQ. 'YAXIS' ) THEN
                  CONTINUE

               ELSE IF ( TYPE .NE. 'CHAR' ) THEN
                  CALL DAT_NEW( LOC, OBJECT, LTYPE, IDIM, DIMENSIONS,
     :                 STATUS )
                  CALL CMP_MAPV( LOC, OBJECT, LTYPE, 'WRITE', PNTR,
     :                 EL, STATUS )
                  CALL CMP_UNMAP( LOC, OBJECT, STATUS )
               END IF

*        Deal with other objects.
            ELSE
               IF ( TYPE .EQ. 'CHAR' ) THEN
                  CALL NDF_XPT0C( 'UNINITIALISED', INDF, 'ECHELLE',
     :                 OBJECT, STATUS )

               ELSE
                  CALL DAT_NEW( LOC, OBJECT, LTYPE, IDIM, DIMENSIONS,
     :                 STATUS )
                  CALL CMP_MAPV( LOC, OBJECT, LTYPE, 'WRITE', PNTR,
     :                 EL, STATUS )
                  CALL CMP_UNMAP( LOC, OBJECT, STATUS )
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ECH_SET_CONTEXT( 'PROBLEM',
     :                 'Cannot create object' )
                  STATUS = ECH__CANT_CREATE
               END IF
            END IF
         END IF


*  Unmap Object
*  ============
      ELSE IF ( OPERATION .EQ. 'UNMAP' ) THEN

*     Unmap image quality array.
         IF ( TYPE .EQ. 'IMAGE-QUALITY' ) THEN
            IF ( HANDLE .NE. 0 ) THEN
               CALL NDF_UNMAP( HANDLE, 'QUALITY', STATUS )
            END IF

*     Unmap image data array.
         ELSE IF ( TYPE .EQ. 'IMAGE-DATA' ) THEN
            STATUS = SAI__OK
            IF ( HANDLE .NE. 0 ) THEN
               CALL NDF_UNMAP( HANDLE, 'DATA', STATUS )
            END IF

*     Unmap image errors (variance).
         ELSE IF ( TYPE .EQ. 'IMAGE-ERRORS' ) THEN
            STATUS = SAI__OK
            IF ( HANDLE .NE. 0 ) THEN
               CALL NDF_UNMAP( HANDLE, 'VARIANCE', STATUS )
            END IF

*     Unmap image flags.
         ELSE IF ( TYPE .EQ. 'IMAGE-FLAGS' ) THEN
            STATUS = SAI__OK
            CALL NDF_SBAD( .TRUE., INDF, 'DATA', STATUS )

*     Unmap structure.
         ELSE IF ( TYPE .EQ. 'STRUCTURE' ) THEN
            STATUS = SAI__OK
            DO I = 1, ACTIVE_NDFS
               IF ( LOBJECT .EQ. NDF_PARMS( I ) ) THEN
                  IF ( NDF_LOCS( I ) .NE. DAT__NOLOC ) THEN
                     CALL DAT_ANNUL( NDF_LOCS( I ), STATUS )
                  END IF
                  IF ( LOBJECT( :9 ) .NE. 'ECH_RDCTN' ) THEN
                     IF ( NDF_IDS( I ) .NE. 0 ) THEN
                        CALL NDF_ANNUL( NDF_IDS( I ), STATUS )
                     END IF
                  END IF
                  NDF_IDS( I ) = 0
                  NDF_LOCS( I ) = DAT__NOLOC
                  NDF_PARMS( I ) = ' '
                  NDF_FILES( I ) = ' '
                  GO TO 150
               END IF
            END DO
 150        CONTINUE

         ELSE IF ( LOBJECT .EQ. 'WORKSPACE' ) THEN
            CALL PSX_FREE( ADDRESS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ECH_SET_CONTEXT( 'PROBLEM', 'VMem release' )
               STATUS = ECH__BAD_VMEMORY
            END IF

*     Unmap other item.
         ELSE

*        Get object name.
            CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT,
     :           ILLEN, IEND, IDIM )
            OBJECT = LOBJECT( IDOT + 1 : IEND )
            IF ( LOBJECT( :9 ) .EQ. 'ECH_RDUCD' ) THEN
               IF ( OBJECT .EQ. 'DATA_ARRAY' ) THEN
                  CALL NDF_UNMAP( INDF, 'DATA', STATUS )

               ELSE IF ( OBJECT .EQ. 'VARIANCE' ) THEN
                  CALL NDF_UNMAP( INDF, 'VARIANCE', STATUS )

               ELSE IF ( OBJECT .EQ. 'XAXIS' ) THEN
                  CALL NDF_AUNMP( INDF, 'CENTRE', 1, STATUS )

               ELSE IF ( OBJECT .EQ. 'YAXIS' ) THEN
                  CALL NDF_AUNMP( INDF, 'CENTRE', 2, STATUS )

               ELSE IF ( TYPE .NE. 'CHAR' ) THEN
                  CALL CMP_UNMAP( LOC, OBJECT, STATUS )

               ELSE
                  STATUS = SAI__OK
               END IF

            ELSE
               CALL CMP_UNMAP( LOC, OBJECT, STATUS )
            END IF
         END IF

      ELSE IF ( LOBJECT .EQ. 'WORKSPACE' ) THEN
         IF ( OPERATION .EQ. 'MAP' ) THEN
            CALL ECH_TYPEINFO( TYPE, TYPE_CODE, AUNIT )
            VM_SIZE = NELEMENTS * AUNIT
            CALL PSX_MALLOC( VM_SIZE, ADDRESS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ECH_SET_CONTEXT( 'PROBLEM', 'VMem exhausted' )
               STATUS = ECH__BAD_VMEMORY

            ELSE
               CALL ECH_ZERO_INIT( NELEMENTS, AUNIT, %VAL( ADDRESS ) )
               STATUS = SAI__OK
            END IF
         END IF

*  Switch on image quality?
      ELSE IF ( TYPE .EQ. 'IMAGE-QUALITY' )  THEN
         IF ( OPERATION .EQ. 'ENABLE' ) THEN
            STATUS = SAI__OK

*         Commented out until NDF_SBB bug gets fixed.
*            CALL NDF_SBB( 255, INDF, STATUS )
         END IF

*  Switch on image flags?
      ELSE IF ( TYPE .EQ. 'IMAGE-FLAGS' )  THEN
         IF ( OPERATION .EQ. 'ENABLE' ) THEN
            STATUS = SAI__OK
            CALL NDF_BAD( INDF, 'DATA', .TRUE., LBAD, STATUS )
         END IF

*  Read envirmonment variable.
      ELSE IF ( TYPE .EQ. 'ENVIRONMENT' ) THEN
         IF ( OPERATION .EQ. 'READ' .AND.
     :      LOBJECT .EQ. 'VARIABLE-PREFIX' ) THEN
            IO_STRING = 'ADAM_USER:GLOBALS.'
            NELEMENTS = 18

         ELSE IF ( OPERATION .EQ. 'OPEN' ) THEN
            ECH__BAD_BYTE = VAL__BADB
            ECH__BAD_INT = VAL__BADI
            ECH__BAD_REAL = VAL__BADR
            ECH__BAD_DOUBLE = VAL__BADD
            STATUS = SAI__OK
            CALL NDF_BEGIN( STATUS )

         ELSE IF ( OPERATION .EQ. 'CLOSE' ) THEN
            STATUS = SAI__OK
            CALL NDF_END( STATUS )
         END IF

*  Handle structure?
      ELSE IF ( TYPE .EQ. 'STRUCTURE' ) THEN

*     Open structure file.
         IF ( OPERATION( :4 ) .EQ. 'OPEN' ) THEN
            IF ( OPERATION .EQ. 'OPEN' .AND.
     :           STATUS .EQ. ECH__SUPRESS_ERRORS ) THEN
               SUPRESS_ERROR = .TRUE.
            END IF

*        Select mode of access.
            IF ( OPERATION .EQ. 'OPEN' ) THEN
               MODE = 'UPDATE'

            ELSE
               MODE = 'READ'
            END IF
            STATUS = SAI__OK
            LOC = DAT__NOLOC
            XLATE = IO_STRING
            CALL ECH_PARSE_ENV( XLATE, XLEN )
            CALL NDF_OPEN( DAT__ROOT, XLATE( :XLEN ), MODE, 'OLD',
     :           INDF, PLACE, STATUS )
            IF ( LOBJECT( :4 ) .EQ. 'ECH_' .OR.
     :           LOBJECT( :5 ) .EQ. 'CLONE' ) THEN
               CALL NDF_XLOC( INDF, 'ECHELLE', MODE, LOC, STATUS )
            END IF
            IF ( STATUS .EQ. SAI__OK ) THEN
               ACTIVE_NDFS = ACTIVE_NDFS + 1
               NDF_PARMS( ACTIVE_NDFS ) = LOBJECT
               NDF_IDS( ACTIVE_NDFS ) = INDF
               NDF_FILES( ACTIVE_NDFS ) = IO_STRING
               NDF_LOCS( ACTIVE_NDFS ) = LOC
            END IF

*     Read structure name.
         ELSE IF ( OPERATION .EQ. 'READ-NAME' ) THEN
            STATUS = SAI__OK
            OBJECT = LOBJECT // ' '
            DO I = 1, ACTIVE_NDFS
               TMP_STRING = NDF_PARMS( I ) // ' '
               IF ( TMP_STRING .EQ. OBJECT ) THEN
                  IO_STRING = NDF_FILES( I )
                  GO TO 200
               END IF
            END DO
 200        CONTINUE
         END IF


*  Map Image
*  =========
      ELSE IF ( OPERATION .EQ. 'MAP-IMAGE' ) THEN
         STATUS = SAI__OK
         CALL NDF_SQMF( .FALSE., INDF, STATUS )
         CALL NDF_MAP( INDF, 'DATA', '_REAL', IO_STRING, ADDRESS, EL,
     :        STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            HANDLE = INDF
         END IF


*  Map Image Quality
*  =================
      ELSE IF ( OPERATION .EQ. 'MAP-QUALITY' ) THEN
         STATUS = SAI__OK
         IF ( INDF .EQ. -1 ) THEN
            STATUS = -777

         ELSE
            CALL NDF_MAP( INDF, 'QUALITY', '_UBYTE', IO_STRING,
     :           ADDRESS, EL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__OK
               CALL NDF_MAP( INDF, 'QUALITY', '_UBYTE', 'WRITE/ZERO',
     :              ADDRESS, EL, STATUS )
               CALL NDF_UNMAP( INDF, 'QUALITY', STATUS )
               CALL NDF_MAP( INDF, 'QUALITY', '_UBYTE', IO_STRING,
     :              ADDRESS, EL, STATUS )
            END IF
         END IF
         HANDLE = 0
         IF ( STATUS .EQ. SAI__OK ) THEN
            HANDLE = INDF
         END IF


*  Map Image Errors
*  ================
      ELSE IF ( OPERATION .EQ. 'MAP-ERRORS' ) THEN
         IF ( INDF .EQ. -1 ) THEN
            STATUS = -777
            SUCCESS = .FALSE.

         ELSE
            IF ( IO_STRING( :1 ) .EQ. 'U' .OR. IO_STRING(:1) .EQ. 'W' )
     :          THEN
               SUCCESS = .TRUE.

            ELSE
               SUCCESS = .FALSE.
               CALL NDF_STATE( INDF, 'VARIANCE', SUCCESS, STATUS )
            END IF
         END IF
         IF ( SUCCESS )  THEN
            CALL NDF_SQMF( .FALSE., INDF, STATUS )
            CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', IO_STRING,
     :           ADDRESS, EL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               HANDLE = INDF
            END IF

         ELSE
            HANDLE = 0
         END IF


*  String Handlers
*  ===============
      ELSE IF ( TYPE .EQ. 'CHAR' ) THEN
         OBJECT = LOBJECT( IDOT + 1 : )

*     Read string.
         IF ( OPERATION .EQ. 'READ' ) THEN
            STATUS = SAI__OK
            CALL NDF_XGT0C( INDF, 'ECHELLE',
     :           LOBJECT( IDOT + 1: ), IO_STRING, STATUS )

*     Write string.
         ELSE IF ( OPERATION .EQ. 'WRITE' ) THEN
            IF ( OBJECT .EQ. 'XLABEL' ) THEN
               CALL NDF_ACPUT( IO_STRING, INDF, 'LABEL', 1, STATUS )

            ELSE IF ( OBJECT .EQ. 'YLABEL' ) THEN
               CALL NDF_ACPUT( IO_STRING, INDF, 'LABEL', 2, STATUS )

            ELSE IF ( OBJECT .EQ. 'ZLABEL' ) THEN
               CALL NDF_CPUT( IO_STRING, INDF, 'TITLE', STATUS )

            ELSE IF ( OBJECT .EQ. 'XUNITS' ) THEN
               CALL NDF_ACPUT( IO_STRING, INDF, 'UNITS', 1, STATUS )

            ELSE IF ( OBJECT .EQ. 'YUNITS' ) THEN
               CALL NDF_ACPUT( IO_STRING, INDF, 'UNITS', 2, STATUS )

            ELSE
               STATUS = SAI__OK
               CALL NDF_ISACC( INDF, 'WRITE', ISACC, STATUS )
               IF ( ISACC .OR. CONTEXT_MODE .EQ. CTX_FTRDB_CREATOR )
     :              THEN
                  CALL NDF_XPT0C( IO_STRING, INDF, 'ECHELLE',
     :                 OBJECT, STATUS )
               END IF
            END IF
         END IF


*  Other Handlers
*  ==============
      ELSE
         IF ( TYPE .EQ. 'BYTE' ) THEN
            LTYPE = '_BYTE'

         ELSE IF ( TYPE .EQ. 'SHORT' ) THEN
            LTYPE = '_UWORD'

         ELSE IF ( TYPE .EQ. 'INT' ) THEN
            LTYPE = '_INTEGER'

         ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN
            LTYPE = '_REAL'

         ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
            LTYPE = '_DOUBLE'

         ELSE
            GO TO 999
         END IF
         STATUS = SAI__OK

*     Get object dimensions.
         CALL ECH_GET_DIMS( LOBJECT, DIMENSIONS, IDOT, ILLEN, IEND,
     :        IDIM )
         OBJECT = LOBJECT( IDOT + 1 : IEND )

*     Read object.
         IF ( OPERATION .EQ. 'READ' ) THEN
            IF ( LOBJECT( : 18 ) .EQ. 'ADAM_USER:GLOBALS.' ) THEN
               IF ( NELEMENTS .GT. 1 ) THEN
                  CALL PAR_GET1R( LOBJECT( 7 : ), NELEMENTS,
     :                 %VAL( ADDRESS ), STATUS )

               ELSE
                  CALL ECH_GET_PARAMETER( LOBJECT( 7 : ), TYPE,
     :                 %VAL( ADDRESS ), .FALSE., ' ', 0, STATUS )
               END IF
               ISTAT = STATUS
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( ISTAT )

            ELSE
               CALL CMP_MAPV( LOC, OBJECT, LTYPE, 'READ', ADDRESS,
     :              NELEMENTS, STATUS )
            END IF

*     Write object.
         ELSE IF ( OPERATION .EQ. 'WRITE' ) THEN
            IF ( LOBJECT( : 9 ) .NE. 'ECH_RDUCD' ) THEN
               CALL CMP_UNMAP( LOC, OBJECT, STATUS )
               CALL CMP_MAPV( LOC, OBJECT, LTYPE, 'UPDATE', ADDRESS,
     :              NELEMENTS, STATUS )
            END IF

*     Read Integer object?
         ELSE IF ( OPERATION .EQ. 'READ-INT' ) THEN
            CALL NDF_XGT0I( INDF, 'ECHELLE', OBJECT, ADDRESS, STATUS )

         ELSE
            IF ( LOBJECT( : 9 ) .EQ. 'ECH_RDUCD' ) THEN
               IF ( OBJECT .EQ. 'DATA_ARRAY' ) THEN
                  CALL NDF_MAP( INDF, 'DATA', '_REAL', 'UPDATE',
     :                 ADDRESS, NELEMENTS, STATUS )

               ELSE IF ( OBJECT .EQ. 'VARIANCE' ) THEN
                  CALL NDF_MAP( INDF, 'VARIANCE', '_REAL',
     :                 'UPDATE', ADDRESS, NELEMENTS, STATUS )

               ELSE IF ( OBJECT .EQ. 'XAXIS' ) THEN
                  CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL',
     :                 'UPDATE', ADDRESS, NELEMENTS, STATUS )

               ELSE IF ( OBJECT .EQ. 'YAXIS' ) THEN
                  CALL NDF_AMAP( INDF, 'CENTRE', 2, '_INTEGER',
     :                 'UPDATE', ADDRESS, NELEMENTS, STATUS )

               ELSE
                  CALL CMP_MAPV( LOC, OBJECT, LTYPE, 'UPDATE', ADDRESS,
     :                 NELEMENTS, STATUS )
               END IF

*        Map.
            ELSE
               IF ( OPERATION .EQ. 'MAP-READ' ) THEN
                  MODE = 'READ'

               ELSE
                  MODE = 'UPDATE'
               END IF
               CALL CMP_MAPV( LOC, OBJECT, LTYPE, MODE, ADDRESS,
     :              NELEMENTS, STATUS )
            END IF
         END IF
      END IF


*  Error Checking
*  ==============
  900 IF ( STATUS .NE. SAI__OK ) THEN
         ISTAT = STATUS
         IF ( OPERATION .EQ. 'READ-SIZE' .OR.
     :        OPERATION .EQ. 'READ-TYPE' ) THEN
            CALL ERR_ANNUL( ISTAT )

         ELSE IF ( OPERATION .EQ. 'OPEN' .AND.
     :             TYPE .EQ. 'STRUCTURE' ) THEN
            IF ( SUPRESS_ERROR ) THEN
               CALL ERR_ANNUL( ISTAT )

            ELSE
               CALL ERR_FLUSH( ISTAT )
            END IF

         ELSE
            CALL ERR_FLUSH( ISTAT )
         END IF
         IF ( OPERATION .EQ. 'MAP-IMAGE' .AND.
     :        ( IO_STRING( :1 ) .EQ. 'W' .OR.
     :          IO_STRING( :1 ) .EQ. 'U' ) ) THEN
            CALL ECH_SET_CONTEXT( 'PROBLEM', 'No update access' )
            STATUS = ECH__CANT_UPDATE
         END IF
      END IF


*  Diagnostics
*  ===========
      IF ( DIAGNOSTICS_ACTIVE ) THEN
         IF ( .NOT. DIAG_ACCESS ) THEN
            DIAG_ACCESS = .TRUE.
            OPEN ( UNIT=77, FILE='echomop_access.log',
     :             STATUS='UNKNOWN', FORM='FORMATTED' )
         END IF

         WRITE ( 77, * ) ' Object: ', LOBJECT( :MIN(
     :                   ECH_WORD_LEN( LOBJECT ), 72 ) )
         WRITE ( 77, * ) ' Operation: ', OPERATION, ' Type: ', TYPE
         WRITE ( 77, * ) ' Nelements: ', NELEMENTS,' VM_Size: ', VM_SIZE
         WRITE ( 77, * ) ' Address: ', ADDRESS, ' Handle: ', HANDLE
         WRITE ( 77, * ) ' IO_string: ', IO_STRING( :MIN(
     :                    ECH_WORD_LEN( IO_STRING ), 72 ) )
         WRITE ( 77, * ) ' Status: ', STATUS
         WRITE ( 77, * ) ' '
      END IF

 999  CONTINUE

      END
