      SUBROUTINE WRCAL2( FD, STATUS )
*+
*  Name:
*     WRCAL2

*  Purpose:
*     Write the calibration part of the dataset to the given logical
*     unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRCAL2( FD, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Logical unit to which to write the data.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Description:
*     Use Fortran sequential I/O to write the data in a specific order.
*     The keyword "END" implies that the file contains no more items
*     (parts) of data.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     BKM: Brian McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1982 (JEG):
*       Original version (IUEDR Vn. 1.0).
*     22-SEP-1988 (PCTR):
*       IUEDR Vn. 2.0.
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     03-APR-95 (MJC):
*       IUEDR Vn. 3.2
*     05-SEP-1996 (BKM):
*       Convert formats to standard f77 run-time form from non-standard
*       Vax form for Linux.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Declarations:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

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

*  Arguments Given:
      INTEGER FD            ! file dscriptor

*  Status:
      INTEGER STATUS        ! Returned status

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Local Variables:
      INTEGER I             ! Loop index
      INTEGER IOSTAT        ! Fortran I/O status
      INTEGER IX            ! Loop index
      INTEGER IY            ! Loop index
      INTEGER J             ! Loop index
      INTEGER LENGTH        ! String length
      INTEGER NNN           ! Intermediate for FORMATs

      CHARACTER*( 40 ) C_ABSID  ! CHARACTER ABSID
      CHARACTER*( 16 ) C_APER   ! CHARACTER APER
      CHARACTER*( 16 ) C_CAMERA ! CHARACTER CAMERA
      CHARACTER*( 40 ) C_FIDSID ! CHARACTER FIDSID
      CHARACTER*( 40 ) C_FIDTID ! CHARACTER FIDTID
      CHARACTER*( 16 ) C_RESOL  ! CHARACTER RESOL
      CHARACTER*( 40 ) C_RIPID  ! CHARACTER RIPID
      CHARACTER*( 40 ) C_TITLE  ! CHARACTER TITLE
      CHARACTER*( 120 ) RT_FMT   ! RUN RIME FORMAT BUFFER
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the version identifier.
      CALL WRPART2( FD, 'IUEDR\\', .FALSE., 'Vn 3.0\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
          GO TO 999
      END IF

*  Write the HEADER data (types IUE_RAW, IUE_GPHOT, IUE_PHOT).
      CALL WRPART2( FD, 'HEADER\\', NOHEAD, TYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOHEAD ) THEN
         CALL GEN_STOC( TITLE, 40, C_TITLE, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT=IOSTAT ) C_TITLE
         CALL GEN_STOC( CAMERA, 16, C_CAMERA, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT=IOSTAT ) C_CAMERA
         CALL GEN_STOC( APER, 16, C_APER, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT=IOSTAT ) C_APER
         CALL GEN_STOC( RESOL, 16, C_RESOL, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT=IOSTAT ) C_RESOL
         WRITE ( FD, 1000, IOSTAT=IOSTAT )
     :           IMAGE, GEOM, PHOT, THDA, YEAR, MONTH, DAY, DATE, NAPER
 1000    FORMAT ( 1X, I8, L4, L4, 1PD10.3, 3I4, I8, I4 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing dataset header\\', STATUS )
            GO TO 999
         END IF

         IF ( NAPER.GT.0 ) THEN
            WRITE( FD, 1001, IOSTAT=IOSTAT )
     :             ( ( APERS( J, I ), J = 1, 16 ),
     :             UTS( I ), TSECS( I ),
     :             FSCALE( I ), VEL( I ), I = 1, NAPER )

*  This line replaced by MJC, APERTURES=BAP bug
* 1001       FORMAT ( 1X, 16I4, 2D16.5, 2D16.5, 2D16.5, <NAPER>D16.5 )
 1001       FORMAT ( 1X, 16I4, 1P4D16.5 )

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing aperture data\\', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the IMAGE data.
      CALL WRPART2( FD, 'IMAGE\\', NODATA, DATATP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NODATA ) THEN
         WRITE ( FD, 1002, IOSTAT=IOSTAT ) NS, NL, LMIN, LMAX
 1002    FORMAT ( 1X, 4I6 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing array dimensions\\', STATUS )
            GO TO 999
         END IF

         IF ( LMIN .LE. LMAX ) THEN
            NNN = LMAX - LMIN + 1
            WRITE( RT_FMT, '(''( 1X, '', I4, ''I4, '', I4, ''I4 )'')')
     :             NNN, NNN
            WRITE ( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( SMIN( I ), I = LMIN, LMAX ),
     :             ( SMAX( I ), I = LMIN, LMAX )
C1003       FORMAT ( 1X, <NNN>I4, <NNN>I4 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing array subset limits\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         WRITE ( FD, 1004, IOSTAT=IOSTAT ) DBLANK, DZERO, DSCALE, DLIM
 1004    FORMAT ( 1X, I8, 1PD16.5, D16.5, 2I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing data scaling\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Write the FIDS data (types FID_IUEG, FID_IUER, FID_IUET).
      CALL WRPART2( FD, 'FIDS\\', NOFIDS, FIDSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOFIDS ) THEN
         CALL GEN_STOC( FIDSID, 40, C_FIDSID, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT = IOSTAT ) C_FIDSID
         WRITE( FD, 1005, IOSTAT = IOSTAT ) FIDHW, FIDT0, NFIDX, NFIDY
 1005    FORMAT ( 1X, 1P2D16.5, 2I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing fiducial parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NFIDX.GT.1 .AND. NFIDY.GT.1 ) THEN
            WRITE( RT_FMT, '(''( 1X, '',I4,
     :                         ''( 1PD16.5 ), '', I4,''D16.5,'',
     :                      I4,''( '', I4,''D16.5 ),'',
     :                      I4,''( '', I4,''D16.5 ),'',
     :                      I4,''( '', I4,''I4 ) )'' )' ) NFIDX, NFIDY,
     :                                                  NFIDY, NFIDX,
     :                                                  NFIDY, NFIDX,
     :                                                  NFIDY, NFIDX
            WRITE( FD, RT_FMT, IOSTAT = IOSTAT )
     :             ( FIDX( IX ), IX = 1, NFIDX ),
     :             ( FIDY( IY ), IY = 1, NFIDY ),
     :             ( ( FIDS( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY ),
     :             ( ( FIDL( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY ),
     :             ( ( FIDQ( IX, IY ), IX = 1, NFIDX ), IY = 1, NFIDY )
C1006       FORMAT ( 1X, <NFIDX>( 1PD16.5 ), <NFIDY>D16.5,
C    :                   <NFIDY>( <NFIDX>D16.5 ),
C    :                   <NFIDY>( <NFIDX>D16.5 ),
C    :                   <NFIDY>( <NFIDX>I4 ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing fiducial positions\\',
     :                      STATUS )
               GO TO 999
            END IF

*        Write the FIDT data.
            CALL WRPART2( FD, 'FIDT\\', NOFIDT, FIDTTP, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 999

            ELSE IF ( .NOT. NOFIDT ) THEN
               CALL GEN_STOC( FIDTID, 40, C_FIDTID, LENGTH )
               WRITE( FD, '(1X, A)' , IOSTAT = IOSTAT ) C_FIDTID
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: writing fiducial label\\',
     :                         STATUS )
                  GO TO 999
               END IF

               WRITE( RT_FMT, '(''( 1X, '',
     :                         I4, ''( '', I4, ''( 1PD16.5 ) ),'',
     :                         I4, ''( '', I4, ''D16.5 ))'')' )
     :                         NFIDY, NFIDX,
     :                         NFIDY, NFIDX
               WRITE( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( ( FIDST( IX, IY ), IX = 1, NFIDX ),
     :                                     IY = 1, NFIDY ),
     :                ( ( FIDLT( IX, IY ), IX = 1, NFIDX ),
     :                                     IY = 1, NFIDY )
C1007          FORMAT ( 1X, <NFIDY>( <NFIDX>( 1PD16.5 ) ),
C    :                      <NFIDY>( <NFIDX>D16.5 ) )
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: writing fiducial shifts\\',
     :                         STATUS )
                  GO TO 999
               END IF
            END IF
         END IF
      END IF

*  Write GEOMETRY data (types CHEBYSHEV).
      CALL WRPART2( FD, 'GEOMETRY\\', NOGEOM, GEOMTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOGEOM ) THEN
         WRITE( FD, 1008, IOSTAT = IOSTAT ) NGTERM, GAXMIN, GAXMAX,
     :          NGCHEB
 1008    FORMAT ( 1X, 2I8, 1P2D16.5, 2D16.5, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing Geometry parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NGCHEB .GT. 0 ) THEN
            WRITE ( RT_FMT, '(''( '', I4, ''( 1X, 1P4D16.5/ ))'')')
     :             NGCHEB
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( GCHEBS( I ), GCHEBL( I ), GCHEBX( I ), GCHEBY( I ),
     :             I = 1, NGCHEB )
C1009       FORMAT ( <NGCHEB>( 1X, 1P4D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing Chebyshev terms\\', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the FACEPLATE data (types IUE_FACE).
      CALL WRPART2( FD, 'FACEPLATE\\', NOFACE, FACETP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOFACE ) THEN
         WRITE( FD, 1010, IOSTAT=IOSTAT )
     :          RADIUS, CENTRE, ANGLE, RLIM, PLIM
 1010    FORMAT ( 1X, I8, 2I8, 1PD16.5, 4I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing faceplate parameters\\',
     :                   STATUS )
            GO TO 999
         END IF
      END IF

*  Write the DISPERSION data (types IUE_DISP).
      CALL WRPART2( FD, 'DISPERSION\\', NODISP, DISPTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NODISP ) THEN
         IF ( STR_SIMLR( 'IUE_DISPN\\', DISPTP ) ) THEN
            WRITE ( FD, 2011, IOSTAT = IOSTAT )
     :             NDISP, DISPT0, DISPD0,
     :             DISPWS1, DISPWS2, DISPWS3, DISPWS4,
     :             DISPWL1, DISPWL2, DISPWL3, DISPWL4
 2011       FORMAT ( 1X, I8, 1P10D16.5 )

         ELSE
            WRITE( FD, 1011, IOSTAT = IOSTAT )
     :             NDISP, DISPT0, DISPD0, DISPST, DISPLT, DISPSD, DISPLD
 1011       FORMAT ( 1X, I8, 1P6D16.5 )
         END IF
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing dispersion parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NDISP .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 1P2D16.5/ ))'')')
     :             NDISP
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( DISPS( I ), DISPL( I ), I = 1, NDISP )
C1012       FORMAT ( <NDISP>( 1X, 1P2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing dispersion\\', STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NDISP .GT. 2 ) THEN
            WRITE( FD, 1013, IOSTAT=IOSTAT ) RIPCON
 1013       FORMAT ( 1X, 1PD16.5 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing ripple constant\\', STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NAPER .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 1P4D16.5/ ))'')')
     :             NAPER
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( DISPDS( I ), DISPDL( I ), DISPSG( I ), DISPLG( I ),
     :             I = 1, NAPER )
C1014       FORMAT ( <NAPER>( 1X, 1P4D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing shifts\\', STATUS )
               GO TO 999
            END IF

            WRITE( RT_FMT, '(''( 1X,'', I4, ''( 1PD16.5 ))'')')
     :             NAPER
            IF ( NDISP .EQ. 2 ) THEN
               WRITE( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( WCOR( I ), I = 1, NAPER )
C1015          FORMAT ( 1X, <NAPER>( 1PD16.5 ) )

            ELSE
               WRITE( FD, RT_FMT, IOSTAT = IOSTAT )
     :                ( ECOR( I ), I = 1, NAPER )
            END IF
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing wavelength shifts\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the ABSCAL data (types IUE_ABS).
      CALL WRPART2( FD, 'ABSCAL\\', NOABS, ABSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOABS ) THEN
         CALL GEN_STOC( ABSID, 40, C_ABSID, LENGTH )
         WRITE( FD, '(1X, A)', IOSTAT=IOSTAT ) C_ABSID
         WRITE( FD, 1016, IOSTAT=IOSTAT ) NABS, TSEN, DSEN
*   Corrected format 03-AUG-1994
 1016    FORMAT ( 1X, I4, 1P2D16.5, 2D16.5 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing absolute calibration\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NABS .GT. 1 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''( 1X, 1P2D16.5/ ))'')')
     :             NABS
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( XABS( I ), YCORR( I ), I = 1, NABS )
C1017       FORMAT ( <NABS>( 1X, 1P2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing absolute calibration\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the RIPCAL data (types IUE_RIP).
      CALL WRPART2( FD, 'RIPCAL\\', NORIP, RIPTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NORIP ) THEN
         CALL GEN_STOC( RIPID, 40, C_RIPID, LENGTH )
         WRITE( FD, '( 1X, A )', IOSTAT = IOSTAT ) C_RIPID
         WRITE( FD, 1018, IOSTAT = IOSTAT ) RIPALF, XRLIM, NRIPM, NRIPO
 1018    FORMAT ( 1X, 1PD16.5, 2D16.5, I8, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing ripple parameters\\', STATUS )
            GO TO 999
         END IF

         IF ( NRIPM .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( 1X, '', I4, ''( 1PD16.5 ))'')' )
     :             NRIPM
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :            ( RIPM( I ), I = 1, NRIPM )
C1019       FORMAT ( 1X, <NRIPM>( 1PD16.5 ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing ripple constants\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NRIPO .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4,
     :                     ''( 1X, I8, 1P2D16.5, I8/ ))'')' ) NRIPO
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( RIPOS( I ), RIPKS( I ), RIPAS( I ), NRIPCS( I ),
     :             I = 1, NRIPO )
C1020       FORMAT ( <NRIPO>( 1X, I8, 1P2D16.5, I8/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing ripple data\\', STATUS )
               GO TO 999
            END IF

            DO I = 1, NRIPO
               IF ( NRIPCS( I ) .GT. 0 ) THEN
                  NNN = NRIPCS ( I )
                  WRITE( RT_FMT, '(''( 1X, '', I4, ''( 1PD16.5 ))'')')
     :                   NNN
                  WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :                   ( RIPCS( J, I ), J = 1, NRIPCS( I ) )
C1021             FORMAT ( 1X, <NNN>( 1PD16.5 ) )
                  IF ( IOSTAT .NE. 0 ) THEN
                     CALL ERROUT( 'Error: writing ripple data\\',
     :                            STATUS )
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF
      END IF

*  Write the CUTCAL data.
      CALL WRPART2( FD, 'CUTCAL\\', NOCUT, CUTTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOCUT ) THEN
         WRITE( FD, 1022, IOSTAT=IOSTAT ) NCUT
 1022    FORMAT ( 1X, I8 )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing wavelength clip data\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NCUT .GT. 0 ) THEN
            WRITE( RT_FMT, '(''( '', I4, ''(1X, I8, 1P2D16.5/ ))'')' )
     :             NCUT
            WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :             ( CUTORD( I ), CUTW1( I ), CUTW2( I ), I = 1, NCUT )
C1023       FORMAT ( <NCUT>( 1X, I8, 1P2D16.5/ ) )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing wavelength clipping\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the OVERLAP data.
      CALL WRPART2( FD, 'OVERLAP\\', NOHAL, HALTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOHAL ) THEN
         IF ( STR_SIMLR( 'POWER\\', HALTP ) ) THEN
            WRITE( FD, 1024, IOSTAT=IOSTAT ) HALC, HALWC, HALW0, HALAV
 1024       FORMAT ( 1X, 1P4D16.5 )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing overlap data\\', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Write the ITFCOR data.
      CALL WRPART2( FD, 'ITFCOR\\', NOITFC, ITFCTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Write the TEMCAL data.
      CALL WRPART2( FD, 'TEMCAL\\', NOTEM, 'IUE_TEM\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT. NOTEM ) THEN
         WRITE( FD, 1022, IOSTAT=IOSTAT ) NTEMO
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing template data\\', STATUS )
            GO TO 999

         ELSE IF ( NTEMO .GT. 0 ) THEN
            DO I = 1, NTEMO
               NNN = NTEMS( I )
               WRITE( RT_FMT, '(''( 1X, 2I8, 1P2D16.5/, 1X, '', I4,
     :                ''D16.5 )'')' ) NNN
               WRITE( FD, RT_FMT, IOSTAT=IOSTAT )
     :                TEMORD( I ), NTEMS( I ), TEMW0( I ), TEMDW( I ),
     :                ( TEMCEN( J, I ), J = 1, NTEMS( I ) )
C1025          FORMAT ( 1X, 2I8, 1P2D16.5/, 1X, <NNN>D16.5 )
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: writing template data\\',
     :                         STATUS )
                  GO TO 999
               END IF
            END DO
         END IF
      END IF

*  Write the terminator for file, i.e. END.
      CALL WRPART2( FD, 'END\\', .TRUE., '\\', STATUS )

 999  CONTINUE

      END
