      SUBROUTINE RDCAL( FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDCAL

*  Purpose:
*     Read the calibration part of the dataset from the given logical
*     unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDCAL( FD, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Logical unit from which to read the data.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Description:
*     Assume a strict fixed order in which the items can appear.  Items
*     can be undefined, but HEADER is mandatory.  The occurence of END
*     implies that the file contains no more items. This should allow
*     the addition of new "features" as experience shows the need.
*     Later versions of this might accept the items in any order, with
*     the posibility that some may not exist at all.  Use unformatted
*     sequential Fortran I/O.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1982 (JRG):
*       Original version (IUEDR Vn. 1.0).
*     22-SEP-1988 (PCTR):
*       IUEDR Vn. 2.0.
*     07-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
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
      INTEGER FD            ! Logical unit number.

*  Status:
      INTEGER STATUS        ! Returned status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Local Variables:
      LOGICAL NOT           ! Whether no input defined.

      BYTE TEMP( 16 )       ! Type temporary.
      BYTE VNAME( 16 )      ! Input name.

      INTEGER I             ! Loop index.
      INTEGER III
      INTEGER K
      INTEGER IOSTAT        ! Fortran I/O status.
      INTEGER IX            ! Loop index.
      INTEGER IY            ! Loop index.
      INTEGER J             ! Loop index.

*  Set of Duplicate REAL versions of REAL*8 variables read from OLD FORMAT
*  calibration files.

      REAL RXABS(27800)    ! wavelengths
      REAL RYCORR(27800)
		  ! associated sensitivity curve fn(date, THDA, ap, tr)
      REAL RTSEN(2)        ! camera temperature sensitivity
      REAL RDSEN(2)        ! dummy date sensitivity
      REAL RCUTW1(100)     ! start of order
      REAL RCUTW2(100)     ! end of order
      REAL RDZERO          ! Data zero point
      REAL RDSCALE         ! Data Scale Factor
      REAL RDISPS(7)       ! S-relations
      REAL RDISPL(7)       ! L-relations
      REAL RDISPDS(2)      ! aperture S-offsets
      REAL RDISPDL(2)      ! aperture L-offsets
      REAL RDISPSG(2)      ! global adhoc S-shift for apertures
      REAL RDISPLG(2)      ! global adhoc L-shift for apertures
      REAL RDISPT0         ! standard THDA
      REAL RDISPD0         ! standard DATE
      REAL RDISPST         ! S-dependence on THDA
      REAL RDISPLT         ! L-dependence on THDA
      REAL RDISPSD         ! S-dependence on DATE
      REAL RDISPLD         ! L-dependence on DATE
      REAL RRIPCON         ! rough ripple constant
      REAL RECOR(2)        ! wavelength correction (M.DW)
      REAL RANGLE          ! the angle of records and line dirn
      REAL RFIDHW          ! fiducial half width
      REAL RFIDT0          ! standard THDA for fiducials
      REAL RFIDX(13)       ! x-fiducial positions
      REAL RFIDY(13)       ! y-fiducial positions
      REAL RFIDS(13, 13)   ! sample positions
      REAL RFIDL(13, 13)   ! line positions
      REAL RFIDST(13, 13)  ! sample position sensitivity
      REAL RFIDLT(13, 13)  ! line position sensitivity
      REAL RGAXMIN(2)      ! axis minima
      REAL RGAXMAX(2)      ! axis maxima
      REAL RGCHEBS(36)     ! S-Chebyshev coefficients
      REAL RGCHEBL(36)     ! L-Chebyshev coefficients
      REAL RGCHEBX(36)     ! U-Chebyshev coefficients
      REAL RGCHEBY(36)     ! V-Chebyshev coefficients
      REAL RHALC           ! halation constant at standard wavelength
      REAL RHALWC          ! standard wavelength for halation
      REAL RHALW0          ! wavelength for zero halation
      REAL RHALAV          ! halation averaging (A)
      REAL RTHDA           ! Camera temperature (K)
      REAL RTSECS(2)       ! aperture exposure times
      REAL RFSCALE(2)      ! aperture scale factors
      REAL RUTS(2)         ! UT of observation relative to DAY
      REAL RRIPM(6)        ! Ripcon in terms of M
      REAL RRIPALF         ! Ripple "alpha" value
      REAL RXRLIM(2)       ! ripple "x" limits
      REAL RRIPKS(100)     ! ripple K-values
      REAL RRIPAS(100)     ! ripple A-values
      REAL RRIPCS(6, 100)  ! ripple polynomials
      REAL RTEMCEN(100, 100) ! centroid shifts about dispersion
      REAL RTEMW0(100)     ! wavelength of first point in each template
      REAL RTEMDW(100)     ! wavelength spacing in each template
      REAL RVEL(2)         ! velocity for each aperture
      REAL RWCOR(2)        ! wavelength correction for each aperture
*.

*  Set everything undefined.
      CALL CNDATA

*  Read the HEADER data.
      CALL RDPART( FD, VNAME, NOT, TYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'HEADER\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when HEADER expected\\', STATUS )

      ELSE IF ( NOT ) THEN
         CALL ERROUT( 'Error: HEADER undefined\\', STATUS )
         GO TO 999

      ELSE
         NOHEAD = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) TITLE, CAMERA, IMAGE, APER,
     :                             RESOL, GEOM, PHOT, RTHDA, YEAR,
     :                             MONTH, DAY, DATE, NAPER
         THDA = DBLE( RTHDA )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dataset header\\', STATUS )
            GO TO 999
         END IF

         IF ( NAPER.GT.0 ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( (APERS( J, I ), J = 1, 16 ),
     :                                   RUTS( I ), RTSECS( I ),
     :                                   RFSCALE( I ), RVEL( I ),
     :                                   I = 1, NAPER )
            DO iii = 1 , NAPER
              UTS(iii) = DBLE(RUTS(iii))
              TSECS(iii) = DBLE(RTSECS(iii))
              FSCALE(iii) = DBLE(RFSCALE(iii))
              VEL(iii) = DBLE(RVEL(iii))
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT('Error: reading aperture data\\', STATUS)
               GO TO 999
            END IF

            NOVEL = .FALSE.
         END IF

         NOHEAD = .FALSE.
      END IF

*  Read the IMAGE data.
      CALL RDPART( FD, VNAME, NOT, DATATP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'IMAGE\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when IMAGE expected\\', STATUS )

      ELSE IF ( .NOT.NOT ) THEN
         NODATA = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) NS, NL, LMIN, LMAX

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: READing array dimensions\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( LMIN.LE.LMAX ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( SMIN( I ), I = LMIN, LMAX ),
     :                                ( SMAX( I ), I = LMIN, LMAX )

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading array subset limits\\',
     :                     STATUS )
               GO TO 999
            END IF
         END IF

         READ( FD, IOSTAT=IOSTAT ) DBLANK, RDZERO, RDSCALE, DLIM
         DZERO = DBLE( RDZERO )
         DSCALE = DBLE( RDSCALE )

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading data scaling\\', STATUS )
            GO TO 999
         END IF

         NODATA = .FALSE.
      END IF

*   Read the FIDS data.
      CALL RDPART( FD, VNAME, NOT, FIDSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'FIDS\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when FIDS expected\\', STATUS )

      ELSE IF ( .NOT.NOT ) THEN
         NOFIDS = .TRUE.
         NOFIDT = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) FIDSID, RFIDHW, RFIDT0, NFIDX, NFIDY
         FIDHW = DBLE( RFIDHW )
         FIDT0 = DBLE( RFIDT0 )

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading fiducial parameters\\',
     :                  STATUS )
            GO TO 999
         END IF

         IF ( ( NFIDX.GT.1 )
     :        .AND. ( NFIDY.GT.1 ) ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( RFIDX( IX ), IX = 1, NFIDX ),
     :                                ( RFIDY( IY ), IY = 1, NFIDY ),
     :                                ( ( RFIDS( IX, IY ), IX = 1,
     :                                   NFIDX ), IY = 1, NFIDY ),
     :                                ( ( RFIDL( IX, IY ), IX = 1,
     :                                   NFIDX ), IY = 1, NFIDY ),
     :                                ( ( FIDQ( IX, IY ), IX = 1,
     :                                   NFIDX ), IY = 1, NFIDY )
            DO III = 1, NFIDX
               FIDX( III ) = DBLE( RFIDX( III ) )
            END DO
            DO III = 1, NFIDY
               FIDY( III ) = DBLE( RFIDY( III ) )
            END DO
            DO III = 1, NFIDX
               DO K = 1, NFIDY
                  FIDL( K, III ) = DBLE( RFIDL( K, III ) )
                  FIDS( K, III ) = DBLE( RFIDS( K, III ) )
               END DO
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading fiducial positions\\',
     :                      STATUS )
               GO TO 999
            END IF

            NOFIDS = .FALSE.

*        Read the FIDT data.
            CALL RDPART( FD, VNAME, NOT, FIDTTP, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 999
            ELSE IF ( .NOT.STR_SIMLR( 'FIDT\\', VNAME ) ) THEN
               CALL ERRSTR( VNAME )
               CALL ERROUT( ': found when FIDT expected\\', STATUS )
            ELSE IF ( .NOT.NOT ) THEN
               READ( FD, IOSTAT=IOSTAT ) FIDTID

               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: reading fiducial label\\',
     :                         STATUS )
                  GO TO 999
               END IF

               READ( FD, IOSTAT=IOSTAT ) ( ( RFIDST( IX, IY ),
     :                                         IX = 1, NFIDX ),
     :                                         IY = 1, NFIDY ),
     :                                      ( ( RFIDLT( IX, IY ),
     :                                         IX = 1, NFIDX ),
     :                                         IY = 1, NFIDY )
               DO iii = 1 , NFIDY
                 DO K = 1 , NFIDX
                   FIDLT(K, iii) = DBLE(RFIDLT(K, iii))
                   FIDST(K, iii) = DBLE(RFIDST(K, iii))
                 END DO
               END DO

               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: reading fiducial shifts\\',
     :                        STATUS )
                  GO TO 999
               END IF

               NOFIDT = .FALSE.
            END IF
         END IF
      END IF

*  Read the GEOMETRY data.
      CALL RDPART( FD, VNAME, NOT, GEOMTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999
      ELSE IF ( .NOT.STR_SIMLR( 'GEOMETRY\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when GEOMETRY expected\\', STATUS )
      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR( 'CHEBYSHEV\\', GEOMTP ) ) THEN
         NOGEOM = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) NGTERM, RGAXMIN, RGAXMAX, NGCHEB

         DO iii = 1 , 2
           GAXMIN(iii) = DBLE(RGAXMIN(iii))
           GAXMAX(iii) = DBLE(RGAXMAX(iii))
         END DO

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading Geometry parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NGCHEB.GT.0 ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( RGCHEBS( I ), RGCHEBL( I ),
     :                                  RGCHEBX( I ), RGCHEBY( I ),
     :                                  I = 1, NGCHEB )
            DO iii = 1 , NGCHEB
              GCHEBX(iii) = DBLE(RGCHEBX(iii))
              GCHEBY(iii) = DBLE(RGCHEBY(iii))
              GCHEBS(iii) = DBLE(RGCHEBS(iii))
              GCHEBL(iii) = DBLE(RGCHEBL(iii))
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading Chebyshev terms\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         NOGEOM = .FALSE.
      ELSE IF ( .NOT.NOT ) THEN
         CALL ERROUT( 'Error: GEOMETRY type unknown\\', STATUS )
         GO TO 999
      END IF

*   FACEPLATE
      CALL RDPART( FD, VNAME, NOT, FACETP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999
      ELSE IF ( .NOT.STR_SIMLR( 'FACEPLATE\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when FACEPLATE expected\\', STATUS )
      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR( 'IUE_FACE\\', FACETP ) ) THEN
         NOFACE = .TRUE.
         NOROT = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) RADIUS, CENTRE, RANGLE, RLIM, PLIM
         ANGLE = DBLE(RANGLE)

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading faceplate parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         NOFACE = .FALSE.
         NOROT = .FALSE.
      ELSE IF ( .NOT.NOT ) THEN
         CALL ERROUT( 'Error: FACEPLATE type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the DISPERSION data.
      CALL RDPART( FD, VNAME, NOT, DISPTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999
      ELSE IF ( .NOT.STR_SIMLR( 'DISPERSION\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when DISPERSION expected\\', STATUS )
      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR( 'IUE_DISP\\', DISPTP ) ) THEN
         NODISP = .TRUE.
         READ( FD, IOSTAT=IOSTAT ) NDISP, RDISPT0, RDISPD0, RDISPST,
     :                             RDISPLT, RDISPSD, RDISPLD
         DISPT0 = DBLE(RDISPT0)
         DISPD0 = DBLE(RDISPD0)
         DISPST = DBLE(RDISPST)
         DISPLT = DBLE(RDISPLT)
         DISPSD = DBLE(RDISPSD)
         DISPLD = DBLE(RDISPLD)

         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading dispersion parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NDISP.GT.0 ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( RDISPS( I ), RDISPL( I ),
     :                                  I = 1, NDISP )
            DO iii = 1 , NDISP
              DISPS(iii) = DBLE(RDISPS(iii))
              DISPL(iii) = DBLE(RDISPL(iii))
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading dispersion\\', STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NDISP.GT.2 ) THEN
            READ( FD, IOSTAT=IOSTAT ) RRIPCON
            RIPCON = DBLE(RRIPCON)
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple constant\\',
     :                     STATUS )
               GO TO 999
            END IF
         END IF

         NOECOR = .TRUE.
         NOWCOR = .TRUE.

         IF ( NAPER.GT.0 ) THEN
            READ( FD, IOSTAT=IOSTAT ) ( RDISPDS( I ), RDISPDL( I ),
     :                                  RDISPSG( I ), RDISPLG( I ),
     :                                  I = 1, NAPER )
            DO iii = 1 , NAPER
              DISPDS(iii) = DBLE(RDISPDS(iii))
              DISPDL(iii) = DBLE(RDISPDL(iii))
              DISPSG(iii) = DBLE(RDISPSG(iii))
              DISPLG(iii) = DBLE(RDISPLG(iii))
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading shifts\\', STATUS )
               GO TO 999
            END IF

            IF ( NDISP.EQ.2 ) THEN
               READ( FD, IOSTAT=IOSTAT ) ( RWCOR( I ), I = 1, NAPER )
               DO iii = 1 , NAPER
                 WCOR(iii) = DBLE(RWCOR(iii))
               END DO

            ELSE
               READ( FD, IOSTAT=IOSTAT ) ( RECOR( I ), I = 1, NAPER )
               DO iii = 1 , NAPER
                 ECOR(iii) = DBLE(RECOR(iii))
               END DO
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

         NODISP = ( ( NDISP .LT. 2 ) .OR. ( NAPER .LT. 1 ) )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: DISPERSION type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the ABSCAL data.
      CALL RDPART( FD, VNAME, NOT, ABSTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'ABSCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when ABSCAL expected\\', STATUS )

      ELSE IF (.NOT.NOT .AND. STR_SIMLR('IUE_ABS\\', ABSTP)) THEN
         NOABS = .TRUE.
         READ( FD, IOSTAT = IOSTAT ) ABSID, NABS, RTSEN, RDSEN
         DO K = 1, 2
           DSEN( K ) = DBLE( RDSEN( K ) )
           TSEN( K ) = DBLE( RTSEN( K ) )
         END DO
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading absolute calib\\', STATUS )
            GO TO 999
         END IF

         IF ( NABS. GT. 1 ) THEN
            READ( FD, IOSTAT = IOSTAT )
     :            ( RXABS( I ), RYCORR( I ), I = 1, NABS )
            DO K = 1, NABS
              XABS( K ) = DBLE( RXABS( K ) )
              YCORR( K ) = DBLE( RYCORR( K ) )
            END DO
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading absolute calib\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         NOABS = ( ( NABS .LT. 2 ) .OR. ( NAPER .LT. 1 ) )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: ABSCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the RIPCAL data.
      CALL RDPART( FD, VNAME, NOT, RIPTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'RIPCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when RIPCAL expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR( 'IUE_RIP\\', RIPTP ) ) THEN
         NORIP = .TRUE.
         READ( FD, IOSTAT = IOSTAT )
     :         RIPID, RRIPALF, RXRLIM, NRIPM, NRIPO
         RIPALF = DBLE( RRIPALF )
         XRLIM( 1 ) = DBLE( RXRLIM( 1 ) )
         XRLIM( 2 ) = DBLE( RXRLIM( 2 ) )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading ripple parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NRIPM.GT.0 ) THEN
            READ( FD, IOSTAT = IOSTAT ) ( RRIPM( I ), I = 1, NRIPM )
            DO K = 1 , NRIPM
               RIPM( K ) = DBLE( RRIPM( K ) )
            END DO
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple constants\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         IF ( NRIPO.GT.0 ) THEN
            READ( FD, IOSTAT = IOSTAT )
     :            ( RIPOS( I ), RRIPKS( I ), RRIPAS( I ), NRIPCS( I ),
     :            I = 1, NRIPO )
            DO K = 1, NRIPO
              RIPKS( K ) = DBLE( RRIPKS( K ) )
              RIPAS( K ) = DBLE( RRIPAS( K ) )
            END DO
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading ripple data\\', STATUS )
               GO TO 999
            END IF

            DO I = 1, NRIPO
               IF ( NRIPCS( I ) .GT. 0 ) THEN
                  READ( FD, IOSTAT = IOSTAT )
     :                  ( RRIPCS( J, I ), J = 1, NRIPCS( I ) )
                  DO K = 1, NRIPCS( I )
                     RIPCS( K, I ) = DBLE( RRIPCS( K, I ) )
                  END DO

                  IF ( IOSTAT .NE. 0 ) THEN
                     CALL ERROUT( 'Error: reading ripple data\\',
     :                            STATUS )
                     GO TO 999
                  END IF
               END IF
            END DO
         END IF

         NORIP = ( NRIPM .LE. 0 )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: RIPCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the CUTCAL data.
      CALL RDPART( FD, VNAME, NOT, CUTTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'CUTCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when CUTCAL expected\\', STATUS )

      ELSE IF ( ( .NOT. NOT )
     :          .AND. STR_SIMLR( 'IUE_CUT\\', CUTTP ) ) THEN
         NOCUT = .TRUE.
         READ( FD, IOSTAT = IOSTAT ) NCUT
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading wavelength clip data\\',
     :                   STATUS )
            GO TO 999
         END IF

         IF ( NCUT .GT. 0 ) THEN
            READ( FD, IOSTAT = IOSTAT )
     :            ( CUTORD( I ), RCUTW1( I ), RCUTW2( I ), I = 1, NCUT )
            DO K = 1, NCUT
               CUTW1( K ) = DBLE( RCUTW1( K ) )
               CUTW2( K ) = DBLE( RCUTW2( K ) )
            END DO

            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: reading wavelength clipping\\',
     :                      STATUS )
               GO TO 999
            END IF
         END IF

         NOCUT = ( NCUT .LE. 0 )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: CUTCAL type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the OVERLAP data.
      CALL RDPART( FD, VNAME, NOT, HALTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'OVERLAP\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when OVERLAP expected\\', STATUS )

      ELSE IF ( ( .NOT. NOT )
     :          .AND. STR_SIMLR( 'POWER\\', HALTP ) ) THEN
         NOHAL = .TRUE.
         READ( FD, IOSTAT = IOSTAT ) RHALC, RHALWC, RHALW0, RHALAV
         HALC = DBLE( RHALC )
         HALWC = DBLE( RHALWC )
         HALW0 = DBLE( RHALW0 )
         HALAV = DBLE( RHALAV )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading overlap data\\', STATUS )
            GO TO 999
         END IF

         NOHAL = .FALSE.

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: OVERLAP type unknown\\', STATUS )
         GO TO 999
      END IF

*  Read the ITFCOR data.
      CALL RDPART( FD, VNAME, NOT, ITFCTP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'ITFCOR\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when ITFCOR expected\\', STATUS )

      ELSE
         NOITFC = NOT
      END IF

*  Read the TEMCAL data.
      CALL RDPART( FD, VNAME, NOT, TEMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999

      ELSE IF ( STR_SIMLR( 'END\\', VNAME ) ) THEN
         GO TO 999

      ELSE IF ( .NOT.STR_SIMLR( 'TEMCAL\\', VNAME ) ) THEN
         CALL ERRSTR( VNAME )
         CALL ERROUT( ': found when TEMCAL expected\\', STATUS )

      ELSE IF ( ( .NOT.NOT )
     :          .AND. STR_SIMLR('IUE_TEM\\', TEMP ) ) THEN
         NOTEM = .TRUE.
         READ( FD, IOSTAT = IOSTAT ) NTEMO
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: reading template size\\', STATUS )
            GO TO 999

         ELSE IF ( NTEMO .GT. 0 ) THEN
            DO I = 1, NTEMO
               READ( FD, IOSTAT = IOSTAT )
     :               TEMORD( I ), NTEMS( I ), RTEMW0( I ), RTEMDW( I ),
     :               ( RTEMCEN( J, I ), J = 1, NTEMS( I ) )
               TEMW0( I ) = DBLE( RTEMW0( I ) )
               TEMDW( I ) = DBLE( RTEMDW( I ) )
               DO K = 1 , NTEMS( I )
                  TEMCEN( K, I ) = DBLE( RTEMCEN( K, I ) )
               END DO
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL ERROUT( 'Error: reading template data\\',
     :                         STATUS )
                  GO TO 999
               END IF
            END DO
         END IF

         NOTEM = ( NTEMO.LE.0 )

      ELSE IF ( .NOT. NOT ) THEN
         CALL ERROUT( 'Error: TEMCAL type unknown\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
