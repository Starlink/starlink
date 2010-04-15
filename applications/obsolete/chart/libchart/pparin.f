      SUBROUTINE PPARIN( STATUS )
*+
*  Name:
*     PPARIN

*  Purpose:
*     Read the parameter file into the common blocks

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PPARIN( STATUS )

*  Description:
*     Reads the parameter file and sets up suitable values in the
*     common blocks contained in MAIN and PLOTDAT.CBL

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     CHR:
*        CHR_LEN, [routine_used]...
*     [facility_or_package]...

*  [optional_subroutine_items]...
*  Authors:
*     {original_author_entry}

*  History:
*     Sometime (ANO):
*        Original version.
*     8-DEC-1988 (PMA + TNW):
*        Use GKS 7.2 instead of GKS 6.2.
*     17-JAN-1989 (TNW):
*        Check that enough pens exist, and then ask user if colour
*        required.
*     9-DEC-1991 (PMA):
*        Changed calls to CTOR to CHR_CTOR
*     18-JAN-1993 (PMA):
*        Changed to ADAM-style subroutine.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statements, with calls to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the READONLY specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*     7-APR-1993 (AJJB):
*        Removed call to LIB$SET_SYMBOL, as it's VAX-specific and
*        doesn't appear to be doing anything useful.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.dat' to filenames.
*     21-MAY-1993 (AJJB):
*        Removed the bit which changes the device name to uppercase as
*        they are case sensitive.
*     21-JUN-1993 (AJJB):
*        Put in a call to ERR_FLUSH in the error handling bits as error
*        messages generated in FILEOPEN were never flushed and STATUS
*        was never reset to OK.
*     {date} ({author_identifier}):
*        {changes}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'MAIN'             ! Main CHART common blocks
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'PLOTDAT'          ! Graphics common blocks
*        {descriptions_of_global_variables_referenced}...

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! [external_description]

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! CHART parameter values
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 50 ) TEXT    ! [local_variable_description]
      CHARACTER * ( 50 ) VAL2    ! [local_variable_description]
      CHARACTER * ( 50 ) WKSTN   ! [local_variable_description]
      CHARACTER * ( 50 ) DEVICE  ! [local_variable_description]
      DOUBLE PRECISION RA        ! [local_variable_description]
      DOUBLE PRECISION DEC       ! [local_variable_description]
      DOUBLE PRECISION RAI       ! [local_variable_description]
      DOUBLE PRECISION DECI      ! [local_variable_description]
      INTEGER WKCAT              ! [local_variable_description]
      INTEGER NCOLI              ! [local_variable_description]
      INTEGER COLA               ! [local_variable_description]
*     INTEGER NPCI               ! [local_variable_description]
      INTEGER IWS( 1 )           ! [local_variable_description]
      INTEGER NLT                ! [local_variable_description]
      INTEGER NLW                ! [local_variable_description]
      INTEGER NPPLI              ! [local_variable_description]
*     INTEGER NELS               ! [local_variable_description]
      REAL NOMLW                 ! [local_variable_description]
      REAL RLWMIN                ! [local_variable_description]
      REAL RLWMAX                ! [local_variable_description]
      INTEGER NPAR               !
      INTEGER NPOS               !
      INTEGER ILENG
      INTEGER GSTAT
      INTEGER I
      INTEGER ICON
      INTEGER PNCI
      INTEGER LT
      REAL WIDTH
      INTEGER L
      REAL DATE
      REAL EQ
      INTEGER N
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First Read in the Parameters
      CALL GETPARAMS( PARAMS, NPAR , STATUS )

*  If it is not the first run, skip the opening of GKS
      IF ( IFLD .EQ. 1 ) THEN

*  Now sort out the plotting device.
         CALL GETDEFLT( PARAMS, NPAR, 'DEVICE', VALUE, NPOS , STATUS )
         IF ( NPOS .EQ. 0 ) THEN
            PLOT = .FALSE.
         ELSE
            IWKID = 1

*  Find if an aspect ratio was requested.
            ILENG = INDEX( VALUE, '_A' )
            IF ( ILENG .GT. 1 ) THEN

*  Yes there was so extract ARATIO and FACTOR
               WKSTN = VALUE( 1:ILENG-1 )
               ASPECT = .TRUE.

*  First the aspect ratio
               CALL GETDEFLT( PARAMS, NPAR, 'RATIO', VALUE, NPOS ,
     :         STATUS )
               IF ( NPOS .GT. 0 ) THEN
                  CALL ERR_MARK
                  CALL CHR_CTOR( VALUE, ARATIO, STATUS )
                  IF ( STATUS .NE. SAI__OK .OR. ARATIO .LE. 0.0 ) THEN
                     ARATIO = 1.0
                     ASPECT = .FALSE.
                  END IF
                  CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
               ELSE
                  ARATIO = 1.0
                  ASPECT = .FALSE.
               END IF

*  then the extra scale factor
               CALL GETDEFLT( PARAMS, NPAR, 'FACTOR', VALUE, NPOS ,
     :         STATUS )
               IF ( NPOS .GT. 0 ) THEN
                  CALL ERR_MARK
                  CALL CHR_CTOR( VALUE, FACTOR, STATUS )
                  IF ( STATUS .NE. SAI__OK .OR. FACTOR .LE. 0.0 ) THEN
                     ASPECT = .FALSE.
                     FACTOR = 1.0
                     CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                  END IF
               ELSE
                  ASPECT = .FALSE.
                  FACTOR = 1.0
               END IF
            ELSE

*  Simply pick up the name of the device
               WKSTN = VALUE
               ASPECT = .FALSE.
               ARATIO = 1.0
               FACTOR = 1.0
            END IF

*  Now try to open the selected device
            CALL SGS_OPEN( WKSTN, IZONID, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               PLOT = .TRUE.

*  Inquire workstation number and connection identifier (This is more
*  foolproof than before because it allows almost any form of logical
*  name etc., but provided a device is opened then this will give thw
*  two numbers required.
               CALL GQACWK( 1, GSTAT, I, IWS )
               CALL GQWKC( IWS, GSTAT, ICON, WKTYPE )
               CALL CHR_FILL( ' ', DEVICE )
               I = 0
               CALL CHR_PUTC( 'GKS_', DEVICE, I )
               CALL CHR_PUTI( WKTYPE, DEVICE, I )
               CALL CHR_PUTC( '_', DEVICE, I )
               CALL CHR_PUTI( ICON, DEVICE, I )
            ELSE
               PLOT = .FALSE.

*  This was not done before, but it seems obvious that if no plotting
*  is to be done then there is not much point in proceeding.
*
               GOTO 999
            END IF

*  See if Workstation is a plotter
            CALL GQWKCA( WKTYPE, GSTAT, WKCAT )
            PLOTTER = WKCAT .EQ. 0

*  See if colours are required
            CALL GQCF( WKTYPE, GSTAT, NCOLI, COLA, PNCI )
            CALL GQPLF( WKTYPE, 1, GSTAT, NLT, LT, NLW, NOMLW, RLWMIN,
     :      RLWMAX, NPPLI )
            COLOUR = ( COLA .EQ. 1 ) .AND. ( NPPLI .GE. 5 )
            IF ( COLOUR ) THEN
               CALL PAR_DEF0L( 'COLOUR', COLOUR, STATUS )
               CALL PAR_GET0L( 'COLOUR', COLOUR, STATUS )
            END IF
            IF ( ICON .EQ. 0 ) THEN
* There used to be code here to Sort out if Device is Picture Oriented.
* It was not clear what this meant, and in the last revision the code
* reduced to this next line anyway.
               PIC = .NOT. PLOTTER
            ENDIF

         END IF

      ENDIF


*  Now get the area to be plotted
      CALL GETDEFLT( PARAMS, NPAR, 'PAREA', VALUE, NPOS , STATUS )
      IF ( NPOS .EQ. 0 ) THEN
         WIDTH = 2.0
      ELSE
         CALL CHR_CTOR( VALUE, WIDTH, STATUS )
      ENDIF

      SIZE = WIDTH / 2.0

*  and the Plot Scale
      CALL GETDEFLT( PARAMS, NPAR, 'SCALE', VALUE, NPOS , STATUS )
      IF ( NPOS .EQ. 0 ) THEN
         SCALE = 67.14
      ELSE
         CALL CHR_CTOR( VALUE, SCALE, STATUS )
      ENDIF

*  Now decide what symbol type is to be used
      CALL GETDEFLT( PARAMS, NPAR, 'SYMBOL', VALUE, NPOS , STATUS )
      IF ( ( NPOS .NE. 0 ) .AND. ( VALUE( 1:5 ) .EQ. 'CROSS' ) ) THEN
         SYMBOL = 'CROS'
      ELSE IF( ( NPOS .NE. 0 ) .AND. ( VALUE( 1:4 ) .EQ. 'SPOT' ) ) THEN
         SYMBOL = 'SPOT'
      ELSE
         SYMBOL = 'CIRC'
      END IF

*  Now decide if the key information is required.
      KEYSCALE = .TRUE.
      KEYNUMB = .TRUE.
      CALL GETDEFLT( PARAMS, NPAR, 'KEY', VALUE, NPOS , STATUS )
      IF ( ( NPOS .EQ. 0 ) .OR. ( INDEX( VALUE, 'NO' ) .NE. 0 ) ) THEN
         KEYSCALE = .FALSE.
         KEYNUMB = .FALSE.
      ELSE
         IF ( VALUE( 1:7 ) .EQ. 'NUMBERS' ) THEN
            KEYSCALE = .FALSE.
         ENDIF
         IF ( VALUE( 1:6 ) .EQ. 'SCALES' ) THEN
            KEYNUMB = .FALSE.
         ENDIF
      ENDIF

*  Now decide if a grid is required.
      CALL GETDEFLT( PARAMS, NPAR, 'GRID', VALUE, NPOS , STATUS )
      IF ( NPOS .EQ. 0 .OR. ( INDEX( VALUE, 'NO' ) .GT. 0 ) ) THEN
         GRID = 'NO'
      ELSE IF ( VALUE( 1:1 ) .EQ. 'M' ) THEN
         GRID = 'MI'
      ELSE
         GRID = 'YE'
      END IF

*  Now handle error boxes.
      CALL GETDEFLT( PARAMS, NPAR, 'ERRBOX', VALUE, NPOS , STATUS )
      IF ( NPOS .EQ. 0 .OR. INDEX( VALUE, 'NO' ) .NE. 0 ) THEN
         ERR = .FALSE.
      ELSE
         ERR = .TRUE.

*  now decide which type (circular or quad)
         IF ( VALUE( 1:1 ) .EQ. 'P' ) THEN
            QEBOX = .FALSE.
         ELSE IF ( VALUE( 1:1 ) .EQ. 'C' ) THEN
            QEBOX = .FALSE.

*  It was circular, so get radius
            CALL GETDEFLT( PARAMS, NPAR, 'RADIUS', VAL2, NPOS , STATUS )
            IF ( NPOS .EQ. 0 ) THEN
               CALL MSG_OUT( ' ', 'Radius assumed to be 2 minutes',
     :            STATUS )
               ERRB = 2.0
            ELSE
               CALL CHR_CTOR( VAL2, ERRB, STATUS )
            END IF
            ERRB = ERRB*60.0
         ELSE
            QEBOX = .TRUE.

*  It was quadrilateral, so find out where the co-ordinates are held
*  and read them.
            CALL GETDEFLT( PARAMS, NPAR, 'COORDS', VAL2, NPOS , STATUS )
            L = CHR_LEN( VAL2 )
            TEXT = VAL2( 1:L )

* This statement
*
*     OPEN ( UNIT=11, FILE=TEXT, STATUS='OLD', ERR=100, READONLY )
*
* has been replaced by the following call (see History) :

      CALL FILEOPEN( 11, TEXT, 'OLD', ' ', ' ', .FALSE., 0, .TRUE.,
     :              STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*  If the file opens then read the positions (but first read the
*  equinox of the positions) and precess them.
*
            READ ( 11, '(A)' ) TEXT
            CALL CHR_CTOR( TEXT, DATE, STATUS )
            DO I = 1, 4
               READ ( 11, '(A)' ) TEXT
               CALL CONVRA( TEXT, 1, 50, RAI, STATUS )
               READ ( 11, '(A)' ) TEXT
               CALL CONVDEC( TEXT, 1, 50, DECI, STATUS )
               QEBC( (I-1)*2 + 1 ) =SNGL( RAI )
               QEBC( I*2 ) = SNGL( DECI )
            END DO
            DO I = 1, 4
               CALL PRECES( DBLE( QEBC( (I-1)*2 + 1 ) ),
     :            DBLE( QEBC( I*2 ) ), RA, DEC, DATE, EQUOUT , STATUS )
               QEBC( (I-1)*2 + 1 ) = SNGL( RA )
               QEBC( I*2 ) = SNGL( DEC )
            END DO

*  Skip the error handling
            GO TO 200

*  If the file does not open, then set no-error box.
  100       CONTINUE
            CALL ERR_FLUSH( STATUS )
            CALL MSG_OUT( ' ', 'Failed to find the co-ordinates',
     :         STATUS)
            CALL MSG_OUT( ' ', 'of the error box vertices', STATUS )
            ERR = .FALSE.
  200       CONTINUE
            CLOSE ( UNIT=11 )
         END IF
      END IF

*  Finally handle the question of extra (or supplemetary) objects.
      CALL GETDEFLT( PARAMS, NPAR, 'EXTRA', VALUE, NPOS , STATUS )
      IF ( NPOS .EQ. 0 .OR. INDEX( VALUE, 'NONE' ) .NE. 0 ) THEN
         SUPP = .FALSE.
      ELSE
         SUPP = .TRUE.
         L = CHR_LEN( VALUE )
         TEXT = VALUE( 1:L )

* This statement :
*
*        OPEN ( UNIT=11, FILE=TEXT, STATUS='OLD', READONLY, ERR=400 )
*
* is replaced by the following call (see History) :

         CALL FILEOPEN( 11, TEXT, 'OLD', ' ', ' ', .FALSE., 0, .TRUE.,
     :                 STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 400

*
*  First read the equinox of the positions.
         READ ( 11, '(A)' ) VAL2
         CALL CHR_CTOR( VAL2, EQ, STATUS )

*  Maximum number of extra objects is 200
         DO N = 1, 201

*  Exit will probably be by hitting the end of the file
            READ ( 11, '(A)', END=300 ) TEXT
            CALL CONVRA( TEXT, 1, 50, OWNOBJ( 1, N ), STATUS )
            READ ( 11, '(A)' ) TEXT
            CALL CONVDEC( TEXT, 1, 50, OWNOBJ( 2, N ), STATUS )
         END DO
  300    CONTINUE
         NUMSUPP = N - 1
         IF ( ABS( EQ - EQUOUT ) .GT. 1E-6 ) THEN
            DO N = 1, NUMSUPP
               CALL PRECES( OWNOBJ( 1, N ), OWNOBJ( 2, N ),
     :            OWNOBJ( 1, N ), OWNOBJ( 2, N ), EQ, EQUOUT , STATUS )
            END DO
         END IF

*  Skip error handling.
         GO TO 500
  400    CONTINUE
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( ' ', 'Failed to find file of extra objects',
     :      STATUS )
         SUPP = .FALSE.
  500    CONTINUE
         CLOSE ( UNIT=11 )
      END IF

*  Now handle two further parameters - first the direction of the
*  RA-axis
      CALL GETDEFLT( PARAMS, NPAR, 'DIRECT', VALUE, NPOS , STATUS )
      IF ( VALUE( 1:3 ) .EQ. 'REV' ) THEN
         DIRECT = .FALSE.
      ELSE
         DIRECT = .TRUE.
      END IF

*  and then the question of a central cross.
      CALL GETDEFLT( PARAMS, NPAR, 'CROSS', VALUE, NPOS , STATUS )
      IF (VALUE( 1:1 ) .EQ. 'N' ) THEN
         CENCROS = .FALSE.
      ELSE
         CENCROS = .TRUE.
      END IF

*  That completes entry of the plotting parameters.

  999 CONTINUE
      END
