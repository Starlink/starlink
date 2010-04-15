      SUBROUTINE RDCAL2( FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDCAL2

*  Purpose:
*      Reads Calibration data from a Vn3+ UEC file.
*
*      Read the calibration part of the dataset from the given logical
*      unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDCAL2( FD, STATUS )

*  Description:
*     Assume a strict fixed order in which the items can appear.  Items
*     can be undefined, but HEADER is mandatory.  The occurence of END
*     implies that the file contains no more items. This should allow
*     the addition of new "features" as experience shows the need.
*     Later versions of this might accept the items in any order, with
*     the posibility that some may not exist at all.  Use sequential
*     formatted Fortran I/O.

*  Arguments:
*     FD = INTEGER (Given)
*        Logical unit from which to read the data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     BKM: Brian McIlwrath (RAL)
*     {enter_new_authors_here}

*  History:
*     1-MAY-82 (JEG):
*       Original version (IUEDR Vn. 1.0).
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     ??-???-?? (DMILLS):
*       IUEDR Vn. 3.0
*     16-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*       Fixed aperture data read bug for BAP
*     29-MAR-95 (MJC):
*       IUEDR Vn. 3.2
*       Support for New Dispersion format.
*     5-SEP-1996 (BKM):
*       Alter to support standard (rather than Vax form) run-time formats
*       for Linux.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER FD            ! Logical unit number.

*  Status:
      INTEGER STATUS        ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'
      INCLUDE 'CMGEOM'
      INCLUDE 'CMDISP'
      INCLUDE 'CMABS'
      INCLUDE 'CMRIP'
      INCLUDE 'CMCUT'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMECOR'
      INCLUDE 'CMVEL'
      INCLUDE 'CMHAL'
      INCLUDE 'CMITFC'
      INCLUDE 'CMTEM'

*  External References:
      LOGICAL STR_SIMLR2    ! Caseless string equality.

*  Local Variables:
      LOGICAL NOT           ! Whether no input defined.

      BYTE TEMP( 16 )       ! Type temporary.
      BYTE VERS( 16 )       ! IUEDR version identifier.
      BYTE VNAME( 16 )      ! Input name.

      INTEGER I             ! Loop index.
      INTEGER IOSTAT        ! Fortran I/O status.
      INTEGER IX            ! Loop index.
      INTEGER IY            ! Loop index.
      INTEGER J             ! Loop index.
      INTEGER LENGTH        ! String length.
      INTEGER NNN           ! Intermediate for FORMATs.

      CHARACTER*( 40 ) C_ABSID  ! CHARACTER ABSID.
      CHARACTER*( 16 ) C_APER   ! CHARACTER APER.
      CHARACTER*( 16 ) C_CAMERA ! CHARACTER CAMERA.
      CHARACTER*( 40 ) C_FIDSID ! CHARACTER FIDSID.
      CHARACTER*( 40 ) C_FIDTID ! CHARACTER FIDTID.
      CHARACTER*( 16 ) C_RESOL  ! CHARACTER RESOL.
      CHARACTER*( 40 ) C_RIPID  ! CHARACTER RIPID.
      CHARACTER*( 40 ) C_TITLE  ! CHARACTER TITLE.
      CHARACTER*( 80 ) RT_FMT   ! RUNTIME FORMAT
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set everything undefined.
      CALL CNDATA

*  Read the version identifier.
      CALL RDPART2( FD, VERS, NOT, TYPE, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         GO TO 999
      END IF

*  Read the HEADER data.
      CALL RDPART2( FD, VNAME, NOT, TYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. STR_SIMLR2( 'HEADER\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when HEADER expected\\', STATUS )

      ELSE IF ( NOT ) THEN
         CALL ERROUT( 'Error: HEADER undefined\\', STATUS )
         GO TO 999

      ELSE
         NOHEAD = .TRUE.
         READ( FD, '( 1X, A )', IOSTAT = IOSTAT ) C_TITLE
         I = 40
         DO WHILE ( C_TITLE( I : I ) .EQ. ' ' )
            I = I - 1
         END DO
         C_TITLE( I+1 : I+1 ) = '\\'
         CALL GEN_CTOS( C_TITLE, 40, TITLE, LENGTH )
         READ( FD, '( 1X, A )', IOSTAT = IOSTAT ) C_CAMERA
         I = 16
         DO WHILE ( C_CAMERA( I:I ) .EQ. ' ' )
            I = I - 1
         END DO
         C_CAMERA( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_CAMERA, 16, CAMERA, LENGTH )
         READ( FD, '( 1X, A )', IOSTAT = IOSTAT ) C_APER
         I = 16
         DO WHILE ( C_APER( I:I ) .EQ. ' ' )
            I = I - 1
         END DO
         C_APER( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_APER, 16, APER, LENGTH )
         READ( FD, '( 1X, A )', IOSTAT = IOSTAT) C_RESOL
         I = 16
         DO WHILE ( C_RESOL( I:I ) .EQ. ' ' )
            I = I-1
         END DO
         C_RESOL( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_RESOL, 16, RESOL, LENGTH )
         READ ( FD, 1000, IOSTAT = IOSTAT )
     :          IMAGE, GEOM, PHOT, THDA, YEAR, MONTH, DAY, DATE, NAPER
 1000    FORMAT ( 1X, I8, L4, L4, D10.3, 3I4, I8, I4 )
         IF ( IOSTAT .NE. SAI__OK) THEN
            CALL ERROUT( 'Error: reading dataset header\\', STATUS )
            GO TO 999
         END IF

         IF ( NAPER .GT. 0 ) THEN
            READ ( FD, 1001, IOSTAT = IOSTAT )
     :             ( ( APERS( J, I ), J = 1, 16 ),
     :             UTS( I ), TSECS( I ),
     :             FSCALE( I ), VEL( I ), I = 1, NAPER )
 1001       FORMAT ( 1X, 16I4, 4D16.5 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading aperture data\\', STATUS )
               GO TO 999
            END IF

            NOVEL = .FALSE.
         END IF

         NOHEAD = .FALSE.
      END IF

*  Read the IMAGE data.
      CALL RDPART2( FD, VNAME, NOT, DATATP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'IMAGE\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when IMAGE expected\\', STATUS )

      ELSE IF ( .NOT.NOT ) THEN
         NODATA = .TRUE.
         READ ( FD, 1002, IOSTAT = IOSTAT ) NS, NL, LMIN, LMAX
 1002    FORMAT ( 1X, 4I6 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: READing array dimensions\\', STATUS )
            GO TO 999
         END IF

         IF ( LMIN .LE. LMAX ) THEN
            NNN = LMAX - LMIN + 1
            WRITE( RT_FMT, '(''(1X,'',I4,''I4,'',I4,''I4)'')') NNN, NNN
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( SMIN( I ), I = LMIN, LMAX ),
     :             ( SMAX( I ), I = LMIN, LMAX )
C 1003       FORMAT ( 1X, <NNN>I4, <NNN>I4 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading array subset limits\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         READ (FD, 1004, IOSTAT = IOSTAT) DBLANK, DZERO, DSCALE, DLIM
 1004    FORMAT ( 1X, I8, D16.5, D16.5, 2I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading data scaling\\', STATUS )
            GO TO 999
         END IF

         NODATA = .FALSE.
      END IF

*   Read the FIDS data.
      CALL RDPART2( FD, VNAME, NOT, FIDSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'FIDS\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when FIDS expected\\', STATUS )

      ELSE IF ( .NOT. NOT ) THEN
         NOFIDS = .TRUE.
         NOFIDT = .TRUE.
         READ ( FD, '(1X, A)', IOSTAT = IOSTAT ) C_FIDSID
         I = 40
         DO WHILE ( C_FIDSID( I:I ) .EQ. ' ' )
            I = I-1
         END DO
         C_FIDSID( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_FIDSID, 40, FIDSID, LENGTH )
         READ ( FD, 1005, IOSTAT = IOSTAT ) FIDHW, FIDT0, NFIDX, NFIDY
 1005    FORMAT ( 1X, 2D16.5, 2I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading fiducial parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NFIDX.GT.1 .AND. NFIDY.GT.1 ) THEN
            WRITE( RT_FMT, '(''( 1X, '',I4,''D16.5, '', I4,''D16.5,'',
     :                      I4,''( '', I4,''D16.5 ),'',
     :                      I4,''( '', I4,''D16.5 ),'',
     :                      I4,''( '', I4,''I4 ) )'' )' ) NFIDX, NFIDY,
     :                                                    NFIDY, NFIDX,
     :                                                    NFIDY, NFIDX,
     :                                                    NFIDY, NFIDX
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( FIDX( IX ), IX = 1, NFIDX ),
     :             ( FIDY( IY ), IY = 1, NFIDY ),
     :             ( ( FIDS( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY ),
     :             ( ( FIDL( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY ),
     :             ( ( FIDQ( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY )
C 1006       FORMAT ( 1X, <NFIDX>D16.5, <NFIDY>D16.5,
C    :                   <NFIDY>( <NFIDX>D16.5 ),
C    :                   <NFIDY>( <NFIDX>D16.5 )
C    :                   <NFIDY>( <NFIDX>I4 ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading fiducial positions\\',
     :                       STATUS )
               GO TO 999
            END IF

            NOFIDS = .FALSE.

*        Read the FIDT data.
            CALL RDPART2( FD, VNAME, NOT, FIDTTP, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 999

            ELSE IF ( .NOT.STR_SIMLR2( 'FIDT\\', VNAME ) ) THEN
               CALL ERRSTR( VNAME )
               CALL ERROUT( ': found when FIDT expected\\', STATUS )

            ELSE IF ( .NOT. NOT ) THEN
               READ ( FD, '(1X, A)', IOSTAT = IOSTAT ) C_FIDTID
               I = 40
               DO WHILE ( C_FIDTID( I:I ) .EQ. ' ' )
                  I = I - 1
               END DO
               C_FIDTID( I+1:I+1 ) = '\\'
               CALL GEN_CTOS( C_FIDTID, 40, FIDTID, LENGTH )

               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: reading fiducial label\\',
     :                         STATUS )
                  GO TO 999
               END IF

               WRITE( RT_FMT, '(''( 1X, '',
     :                         I4, ''( '', I4, ''D16.5 ),'',
     :                         I4, ''( '', I4, ''D16.5 ))'')' )
     :                         NFIDY, NFIDX,
     :                         NFIDY, NFIDX
               READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( ( FIDST( IX, IY ), IX = 1, NFIDX ),
     :                                     IY = 1, NFIDY ),
     :                ( ( FIDLT( IX, IY ), IX = 1, NFIDX ),
     :                                     IY = 1, NFIDY )
C1007          FORMAT ( 1X, <NFIDY>( <NFIDX>D16.5 ),
C    :                      <NFIDY>( <NFIDX>D16.5 ) )
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: reading fiducial shifts\\',
     :                         STATUS )
                  GO TO 999
               END IF

               NOFIDT = .FALSE.
            END IF
         END IF
      END IF

*  Read the GEOMETRY data.
      CALL RDPART2( FD, VNAME, NOT, GEOMTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'GEOMETRY\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when GEOMETRY expected\\', STATUS )

      ELSE IF ( (.NOT.NOT)
     :          .AND. STR_SIMLR2( 'CHEBYSHEV\\', GEOMTP ) ) THEN
         NOGEOM = .TRUE.

         READ ( FD, 1008, IOSTAT = IOSTAT )
     :          NGTERM, GAXMIN, GAXMAX, NGCHEB
 1008    FORMAT ( 1X, 2I8, 2D16.5, 2D16.5, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading Geometry parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NGCHEB .GT. 0 ) THEN
            WRITE ( RT_FMT, '(''( '', I4, ''( 1X, 4D16.5/ ))'')')
     :             NGCHEB
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( GCHEBS( I ), GCHEBL( I ), GCHEBX( I ), GCHEBY( I ),
     :             I = 1, NGCHEB )
C1009       FORMAT ( <NGCHEB>( 1X, 4D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading Chebyshev terms\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         NOGEOM = .FALSE.

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: GEOMETRY type unknown\\', STATUS )
         GO TO 999
      END IF

*   FACEPLATE
      CALL RDPART2( FD, VNAME, NOT, FACETP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT. STR_SIMLR2( 'FACEPLATE\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when FACEPLATE expected\\', STATUS )

      ELSE IF ( (.NOT.NOT) .AND. STR_SIMLR2('IUE_FACE\\', FACETP)) THEN
         NOFACE = .TRUE.
         NOROT = .TRUE.
         READ ( FD, 1010, IOSTAT = IOSTAT)
     :          RADIUS, CENTRE, ANGLE, RLIM, PLIM
 1010    FORMAT ( 1X, I8, 2I8, D16.5, 4I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading faceplate parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         NOFACE = .FALSE.
         NOROT = .FALSE.

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: FACEPLATE type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the DISPERSION data.
      CALL RDPART2( FD, VNAME, NOT, DISPTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT. STR_SIMLR2( 'DISPERSION\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when DISPERSION expected\\', STATUS )

*  New-style dispersion data.
      ELSE IF ( (.NOT.NOT)
     :          .AND. STR_SIMLR2( 'IUE_DISPN\\', DISPTP ) ) THEN
         NODISP = .TRUE.
         READ ( FD, 2011, IOSTAT = IOSTAT )
     :          NDISP, DISPT0, DISPD0,
     :          DISPWS1, DISPWS2, DISPWS3, DISPWS4,
     :          DISPWL1, DISPWL2, DISPWL3, DISPWL4
 2011    FORMAT ( 1X, I8, 10D16.5 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NDISP .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 2D16.5/ ))'')') NDISP
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( DISPS( I ), DISPL( I ), I = 1, NDISP )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading dispersion\\', STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NDISP .GT. 2 ) THEN
            READ( FD, 1013, IOSTAT = IOSTAT ) RIPCON
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple constant\\', STATUS )
               GO TO 999
            END IF
         END IF

         NOECOR = .TRUE.
         NOWCOR = .TRUE.

         IF ( NAPER .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 4D16.5/ ))'')') NAPER
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( DISPDS( I ), DISPDL( I ), DISPSG( I ), DISPLG( I ),
     :             I = 1, NAPER )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading shifts\\', STATUS )
               GO TO 999
            END IF

            WRITE( RT_FMT, '(''( 1X, '', I4, ''D16.6 ))'')') NAPER
            IF ( NDISP .EQ. 2 ) THEN
               READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( WCOR( I ), I = 1, NAPER )

            ELSE
               READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( ECOR( I ), I = 1, NAPER )
            END IF
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading wavelength shifts\\',
     :                      STATUS )
               GO TO 999
            END IF

            IF ( NDISP .EQ. 2 ) THEN
               NOWCOR = .FALSE.

            ELSE
               NOECOR = .FALSE.
            END IF
         END IF

         NODISP = ( ( NDISP.LT.2 ) .OR. ( NAPER.LT.1 ) )

*  Old-style dispersion data.
      ELSE IF ( (.NOT.NOT)
     :          .AND. STR_SIMLR2( 'IUE_DISP\\', DISPTP ) ) THEN
         NODISP = .TRUE.
         READ ( FD, 1011, IOSTAT = IOSTAT )
     :          NDISP, DISPT0, DISPD0, DISPST, DISPLT, DISPSD, DISPLD
 1011    FORMAT ( 1X, I8, 6D16.5 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NDISP .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 2D16.5/ ))'')') NDISP
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( DISPS( I ), DISPL( I ), I = 1, NDISP )
C1012       FORMAT ( <NDISP>( 1X, 2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading dispersion\\', STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NDISP .GT. 2 ) THEN
            READ( FD, 1013, IOSTAT = IOSTAT ) RIPCON
 1013       FORMAT ( 1X, D16.5 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple constant\\', STATUS )
               GO TO 999
            END IF
         END IF

         NOECOR = .TRUE.
         NOWCOR = .TRUE.

         IF ( NAPER .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 4D16.5/ ))'')') NAPER
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( DISPDS( I ), DISPDL( I ), DISPSG( I ), DISPLG( I ),
     :             I = 1, NAPER )
C1014       FORMAT ( <NAPER>( 1X, 4D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading shifts\\', STATUS )
               GO TO 999
            END IF

            WRITE( RT_FMT, '(''( 1X, '', I4, ''D16.6 ))'')') NAPER
            IF ( NDISP .EQ. 2 ) THEN
               READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( WCOR( I ), I = 1, NAPER )
C1015          FORMAT ( 1X, <NAPER>D16.5 )

            ELSE
               READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( ECOR( I ), I = 1, NAPER )
            END IF
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading wavelength shifts\\',
     :                      STATUS )
               GO TO 999
            END IF

            IF ( NDISP .EQ. 2 ) THEN
               NOWCOR = .FALSE.

            ELSE
               NOECOR = .FALSE.
            END IF
         END IF

         NODISP = ( ( NDISP.LT.2 ) .OR. ( NAPER.LT.1 ) )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: DISPERSION type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the ABSCAL data.
      CALL RDPART2( FD, VNAME, NOT, ABSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'ABSCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when ABSCAL expected\\', STATUS )

      ELSE IF ( .NOT.NOT .AND. STR_SIMLR2('IUE_ABS\\', ABSTP) ) THEN
         NOABS = .TRUE.
         READ ( FD, '(1X, A)', IOSTAT = IOSTAT ) C_ABSID
         I = 40
         DO WHILE ( C_ABSID( I:I ) .EQ. ' ' )
            I = I - 1
         END DO
         C_ABSID( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_ABSID, 40, ABSID, LENGTH )
         READ ( FD, 1016, IOSTAT = IOSTAT ) NABS, TSEN, DSEN
 1016    FORMAT ( 1X, I4, 2D16.5, 2D16.5 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading absolute calibration\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NABS .GT. 1 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 2D16.5/ ))'')') NABS
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( XABS( I ), YCORR( I ), I = 1, NABS )
C1017       FORMAT ( <NABS>( 1X, 2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading absolute calibration\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         NOABS = ( ( NABS.LT.2 ) .OR. ( NAPER.LT.1 ) )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: ABSCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the RIPCAL data.
      CALL RDPART2( FD, VNAME, NOT, RIPTP, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'RIPCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when RIPCAL expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR2( 'IUE_RIP\\', RIPTP ) ) THEN
         NORIP = .TRUE.
         READ ( FD, '(1X, A)', IOSTAT = IOSTAT ) C_RIPID
         I = 40
         DO WHILE ( C_RIPID( I:I ) .EQ. ' ' )
            I = I - 1
         END DO
         C_RIPID( I+1:I+1 ) = '\\'
         CALL GEN_CTOS( C_RIPID, 40, RIPID, LENGTH )
         READ ( FD, 1018, IOSTAT = IOSTAT ) RIPALF, XRLIM, NRIPM, NRIPO
 1018    FORMAT ( 1X, D16.5, 2D16.5, I8, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading ripple parameters\\', STATUS )
            GO TO 999
         END IF

         IF ( NRIPM .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( 1X,'', I4, ''D16.5 )'')' ) NRIPM
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( RIPM( I ), I = 1, NRIPM )
C1019       FORMAT ( 1X, <NRIPM>D16.5 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple constants\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NRIPO .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, I8, 2D16.5, I8/ ))'')')
     :             NRIPO
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( RIPOS( I ), RIPKS( I ), RIPAS( I ), NRIPCS( I ),
     :             I = 1, NRIPO )
C1020       FORMAT ( <NRIPO>( 1X, I8, 2D16.5, I8/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple data\\', STATUS )
               GO TO 999
            END IF

            DO I = 1, NRIPO
               IF ( NRIPCS( I ) .GT. 0 ) THEN
                  NNN = NRIPCS( I )
                  WRITE( RT_FMT, '(''( 1X, '', I4, ''D16.5 )'')') NNN
                  READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :                   ( RIPCS( J, I ), J = 1, NRIPCS( I ) )
C1021              FORMAT ( 1X, <NNN>D16.5 )
                  IF ( IOSTAT .NE. 0 ) THEN
                     CALL ERROUT( 'Error: reading ripple data\\',
     :                            STATUS )
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF

         NORIP = ( NRIPM .LE. 0 )

      ELSE IF ( .NOT.NOT ) THEN
         CALL ERROUT( 'Error: RIPCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the CUTCAL data.
      CALL RDPART2( FD, VNAME, NOT, CUTTP, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'CUTCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when CUTCAL expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR2( 'IUE_CUT\\', CUTTP ) ) THEN
         NOCUT = .TRUE.
         READ ( FD, 1022, IOSTAT = IOSTAT ) NCUT
 1022    FORMAT ( 1X, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading wavelength clip data\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NCUT .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, I8, 2D16.5/ ))'')')
     :             NCUT
            READ ( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( CUTORD( I ), CUTW1( I ), CUTW2( I ), I = 1, NCUT )
C1023       FORMAT ( <NCUT>( 1X, I8, 2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading wavelength clipping\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF
         NOCUT = ( NCUT.LE.0 )

      ELSE IF ( .NOT.NOT ) THEN
         CALL ERROUT( 'Error: CUTCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the OVERLAP data.
      CALL RDPART2( FD, VNAME, NOT, HALTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'OVERLAP\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when OVERLAP expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR2( 'POWER\\', HALTP ) ) THEN
         NOHAL = .TRUE.
         READ ( FD, 1024, IOSTAT = IOSTAT ) HALC, HALWC, HALW0, HALAV
 1024    FORMAT ( 1X, 4D16.5 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading overlap data\\', STATUS )
            GO TO 999
         END IF
         NOHAL = .FALSE.

      ELSE IF ( .NOT.NOT ) THEN
         CALL ERROUT( 'Error: OVERLAP type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the ITFCOR data.
      CALL RDPART2( FD, VNAME, NOT, ITFCTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT. STR_SIMLR2( 'ITFCOR\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when ITFCOR expected\\', STATUS )

      ELSE
         NOITFC = NOT
      END IF

*  Read the TEMCAL data.
      CALL RDPART2( FD, VNAME, NOT, TEMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR2( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR2( 'TEMCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when TEMCAL expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR2('IUE_TEM\\', TEMP ) ) THEN
         NOTEM = .TRUE.
         READ ( FD, 1022, IOSTAT = IOSTAT ) NTEMO
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading template data\\', STATUS )
            GO TO 999

         ELSE IF ( NTEMO .GT. 0 ) THEN
            DO I = 1, NTEMO
               READ ( FD, 1025, IOSTAT = IOSTAT )
     :                TEMORD( I ), NNN, TEMW0( I ), TEMDW( I )
               IF ( IOSTAT .NE. 0 ) GOTO 1026
               WRITE( RT_FMT, '(''( 1X, '', I4, ''D16.5 )'')' ) NNN
               READ( FD, RT_FMT, IOSTAT = IOSTAT )
     :               (TEMCEN( J, I ), J = 1, NNN)
C1025          FORMAT ( 1X, 2I8, 2D16.5/, 1X, <NNN>D16.5 )
 1025          FORMAT ( 1X, 2I8, 2D16.5 )
               NTEMS( I ) = NNN
 1026          IF ( IOSTAT .NE. 0 ) THEN
                 CALL ERROUT( 'Error: reading template data\\',
     :                         STATUS )
                  GO TO 999
               END IF
            END DO
         END IF

         NOTEM = ( NTEMO .LE. 0 )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: TEMCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Abort.
 999  CONTINUE

      END
