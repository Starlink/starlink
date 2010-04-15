      SUBROUTINE POINB2( MAXDET, MAXSRC, DETNUM, EXPSRC, IDET, LOGREQ,
     :                   LOGFID, SCALE, SCS, SRCN, SRCAMP, SRCANG,
     :                   SRCBAS, SRCCOR, SRCDEC, SRCINS, SRCNON,
     :                   SRCNOS, SRCRA, SRCSLP, UNITS, STATUS )
*+
*  Name:
*     POINB2

*  Purpose:
*     Report the detections to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINB2( DETNUM, EXPSRC, IDET, LOGREQ, LOGFID, SCALE, SCS,
*                  SRCAMP, SRCANG, SRCBAS, SRCCOR, SRCDEC, SRCINS,
*                  SRCRA, SRCN, SRCNON, SRCNOS, SRCSLP, UNITS,
*                  STATUS )

*  Description:
*     This subroutine is used by the POINTCRDD to report the detection
*     results to the terminal in use and write to a text file if
*     required.

*  Arguments:
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors
*     MAXSRC = INTEGER (Given)
*        Maximum number of sources
*     DETNUM( MAXDET )  = INTEGER (Given)
*        Detector number for given detector index
*     EXPSRC = LOGICAL (Given)
*        If true an expected source position was supplied.
*     IDET = INTEGER (Given)
*        Detector index for the current detector
*     LOGFID = INTEGER (Given)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Given)
*        TRUE when logging results to the logfile is required.
*     SCALE = REAL (Given)
*        Scale used to convert data to the desired units.
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system in use.
*     SRCN = INTEGER (Given)
*        The number of the sources detected.
*     SRCAMP( MAXSRC ) = REAL (Given)
*        The amplitude of the detected point sources.
*     SRCANG( MAXSRC ) = DOUBLE PRECISION (Given)
*        The scan angle at each source position.
*     SRCBAS( MAXSRC ) = REAL (Given)
*        Baseline height of the detected point sources.
*     SRCCOR( MAXSRC ) = REAL (Given)
*        Correlation coefficient for the detected point sources.
*     SRCDEC( MAXSRC ) = DOUBLE PRECISION (Given)
*        The Dec of the detected source positions.
*     SRCINS( MAXSRC ) = REAL (Given)
*        The in-scan distances of the detected sources.
*     SRCNON( MAXSRC ) = INTEGER (Given)
*        Number of samples used in calculating the local noise
*     SRCNOS( MAXSRC ) = REAL (Given)
*        Estimated local noise.
*     SRCRA( MAXSRC ) = DOUBLE PRECISION (Given)
*        The RA of the detected source positions.
*     SRCSLP( MAXSRC ) = REAL (Given)
*        The slope of the data in the region surrounding the source.
*     UNITS = CHARACTER*( * ) (Given)
*        The units used when output detecting results.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA package constants

*  Arguments Given:
      INTEGER MAXDET
      INTEGER MAXSRC
      INTEGER DETNUM( MAXDET )
      LOGICAL EXPSRC
      INTEGER IDET
      INTEGER LOGFID
      LOGICAL LOGREQ
      REAL SCALE
      CHARACTER*( * ) SCS
      INTEGER SRCN
      REAL SRCAMP( MAXSRC )
      DOUBLE PRECISION SRCANG( MAXSRC )
      REAL SRCBAS( MAXSRC )
      REAL SRCCOR( MAXSRC )
      DOUBLE PRECISION SRCDEC( MAXSRC )
      REAL SRCINS( MAXSRC )
      INTEGER SRCNON( MAXSRC )
      REAL SRCNOS( MAXSRC )
      DOUBLE PRECISION SRCRA( MAXSRC )
      REAL SRCSLP( MAXSRC )
      CHARACTER*( * ) UNITS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      REAL SLA_RANGE             ! Normalisation of angles

*  Local Variables:
      CHARACTER*( 10 ) AMPST     ! String form of source amplitude
      INTEGER AMPLN              ! Used length of AMPST
      REAL ANGLE                 ! The normalised scan angle
      CHARACTER*( 10 ) BASST     ! String form of baseline height
      INTEGER BASLN              ! Used length of BASST
      CHARACTER*( 10 ) CORRST    ! String form of the correlation coefficient
      INTEGER CORRLN             ! Used length of CORRST
      CHARACTER*( 4 ) DETST      ! String form of detector number
      INTEGER DETLN              ! Used length of DETST
      INTEGER ISRC               ! Source number do loop index
      CHARACTER*( 2 ) ISRCST     ! String form of source number
      INTEGER ISRCLN             ! Used length of ISRCST
      REAL INSCAM                ! In-scan distance in arcmin
      CHARACTER*( 10 ) INSCST    ! String form of in-scan distance
      INTEGER INSCLN             ! Used length of INSCST
      DOUBLE PRECISION LAT( 1 )  ! Lat. of source position in SCS coord system.
      CHARACTER*( IRA__SZFSC ) LATST  ! String form of source latitude
      INTEGER LATLN              ! Used length of LATST
      DOUBLE PRECISION LON( 1 )  ! Long. of source position in SCS coord system.
      CHARACTER*( IRA__SZFSC ) LONST  ! String form of source longitude
      INTEGER LONLN              ! Used length of LONST
      CHARACTER*( 10 ) NOIST     ! String form of the estimated local noise
      INTEGER NOILN              ! Used length of NOIST
      CHARACTER*( 4 ) NOSMST     ! String form of the number of samples used
				 ! in estimated local noise
      INTEGER NOSMLN             ! Used length of NOSMST
      INTEGER SCSLN              ! Used length of SCS
      CHARACTER*( 7 ) SCSST      ! Short string form of SCS
      CHARACTER*( 19 ) SRCDIR    ! String describing scan direction
      CHARACTER*( 2 ) SRCNST     ! String form of Number of sources
      INTEGER SRCNLN             ! Used length of SRCNST
      CHARACTER*( 10 ) SLPST     ! String form of slope of baseline
      INTEGER SLPLN              ! Used length of SLPST
      INTEGER UNITLN             ! Used length of UNITS
      CHARACTER*(80) TEMPSTR     ! Temporary string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* **************************************************************************
* Translate information relating to the detector and number of sources found
* **************************************************************************
*  Translate the detector number associated with the detector index number
*  to a character string
      CALL CHR_ITOC( DETNUM( IDET ), DETST, DETLN )

*  Translate the number of sources found
      CALL CHR_ITOC( SRCN, SRCNST, SRCNLN )

*  Get the used length of the UNITS.
      UNITLN = CHR_LEN( UNITS )


* **************************************************************************
* Report the number of sources found
* **************************************************************************
      CALL MSG_BLANK( STATUS )
      IF ( SRCN .EQ. 1 ) THEN
         CALL MSG_SETC('DETST',DETST(:DETLN))
         CALL MSG_OUT( 'POINB2_MSG1', ' A single '/
     :                /' point source is detected in the trace'/
     :                /' of the detector ^DETST',
     :                STATUS )
      ELSE
         CALL MSG_SETC('DETST',DETST(:DETLN))
         CALL MSG_SETC('SRCNST',SRCNST(:SRCNLN))
         CALL MSG_OUT( 'POINB2_MSG1', '^SRCNST'/
     :                /' point sources have been found in the trace'/
     :                 /' of the detector ^DETST', STATUS )
      END IF
      IF ( LOGREQ ) THEN
         CALL FIO_WRITE( LOGFID, ' ', STATUS )
         IF ( SRCN .EQ. 1 ) THEN
            CALL FIO_WRITE( LOGFID, ' A single point source '/
     :                    /'is detected in the trace of detector '/
     :                    /DETST( : DETLN ), STATUS )
         ELSE
            CALL FIO_WRITE( LOGFID, SRCNST( : SRCNLN )//' point '/
     :                    /'sources are detected in the trace '/
     :                    /'of detector '//DETST( : DETLN ), STATUS )
         END IF
      END IF


* **************************************************************************
*  Report the sources one by one.
* **************************************************************************
      DO ISRC = 1, SRCN

*  Translate source number
         CALL CHR_ITOC( ISRC, ISRCST, ISRCLN )

*  Write source number heading
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC('ISRCST',ISRCST(:ISRCLN))
         CALL MSG_OUT( 'POINB2_MSG2', '  SOURCE ^ISRCST:',STATUS)
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            CALL FIO_WRITE( LOGFID, '   SOURCE '//ISRCST( : ISRCLN )/
     :                  /':',STATUS )
         END IF

* **************************************************************************
*  Convert or calculate positional information and report it
* **************************************************************************
*  Convert the source RA and Dec to the users source coordinate system
*  and thence to character strings
         CALL IRA_CONVT( 1, SRCRA( ISRC ), SRCDEC( ISRC ),
     :                  'EQUATORIAL(B1950.0)', SCS, IRA__IRJEP,
     :                   LON, LAT, STATUS )
         CALL IRA_DTOC( LON( 1 ), LAT( 1 ), SCS, 1, LONST, LATST,
     :                  STATUS )
         LONLN = CHR_LEN( LONST )
         LATLN = CHR_LEN( LATST )
         SCSLN = CHR_LEN( SCS )

*  Report the sky coordinates of the source position, and the coordinate system
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC('LONST', LONST(:LONLN))
         CALL MSG_SETC('LATST', LATST(:LATLN))
         CALL MSG_SETC('SCS',   SCS(:SCSLN))
         CALL MSG_OUT( 'POINB2_MSG3', 'Sky position    : '/
     :                 /'^LONST ^LATSRC ^SCS', STATUS)

         IF ( LOGREQ ) THEN

*  Produce a short form of the coordinate system description
            IF      ( ( SCS .EQ. 'EQUATORIAL(B1950)        ')
     :         .OR.   ( SCS .EQ. 'EQUATORIAL(J1950)        ') ) THEN
               SCSST = 'Eq''50'
            ELSE IF ( (SCS .EQ. 'EQUATORIAL(B2000)         ')
     :         .OR.   (SCS .EQ. 'EQUATORIAL(J2000)         ') ) THEN
               SCSST = 'Eq20'''
            ELSE IF   (SCS(1:10) .EQ. 'EQUATORIAL')             THEN
               SCSST = 'Eq???'
*  Ecliptic
            ELSE IF ( ( SCS .EQ. 'ECLIPTIC(B1950)          ')
     :         .OR.   ( SCS .EQ. 'ECLIPTIC(J1950)          ') ) THEN
               SCSST = 'Ec''50'
            ELSE IF ( (SCS .EQ. 'ECLIPTIC(B2000)           ')
     :         .OR.   (SCS .EQ. 'ECLIPTIC(J2000)           ') ) THEN
               SCSST = 'Ec20'''
            ELSE IF   (SCS(1:8) .EQ. 'ECLIPTIC')               THEN
               SCSST = 'Ec???'
*  Galactic
            ELSE IF ( SCS(1:8) .EQ. 'GALACTIC' ) THEN
               SCSST = 'Galac'
            ELSE
*  Unknown
               SCSST = '?????'
            END IF

*  Find the length of the output coord system string
            SCSLN = CHR_LEN( SCSST )

*  Write output message
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            CALL FIO_WRITE( LOGFID, 'Sky position    : '/
     :                     /LONST( : LONLN )//' '//LATST( : LATLN )/
     :                     /' '//SCSST( :SCSLN ), STATUS )
         END IF

*  Report the in-scan distance of the source if an expected position was
*  supplied
         IF ( EXPSRC ) THEN
            CALL CHR_RTOC( ABS( SRCINS( ISRC ) ), INSCST, INSCLN)

            CALL MSG_SETC('INSCST', INSCST(:INSCLN))
            CALL MSG_OUT( 'POINB2_MSG4', 'Absolute value of '/
     :                    /'In-scan distance: ^INSCST (Arcmin)',
     :                    STATUS )
            IF ( LOGREQ ) THEN
               CALL FIO_WRITE( LOGFID, 'Absolute value of '/
     :                    /'In-scan distance: '//INSCST( : INSCLN)/
     :                    /' (Arcmin) ',STATUS )
            END IF
         END IF

* ****************************************************************************
*  Convert or calculate point source amplitude, etc. information and report it
* ****************************************************************************
*  Apply scaling and translate amplitude, slope and constant values
         CALL CHR_RTOC( SCALE * SRCAMP( ISRC ), AMPST, AMPLN )
         CALL CHR_RTOC( SCALE * SRCSLP( ISRC ), SLPST, SLPLN )
         CALL CHR_RTOC( SCALE * SRCBAS( ISRC ), BASST, BASLN )

*  Determine whether the slope is North to south or vice versa
         ANGLE = SLA_RANGE( REAL( SRCANG( ISRC ) ) )
         IF ( ABS( ANGLE ) .LT. REAL( IRA__PIBY2 ) ) THEN
            SRCDIR = 'from North to South'
         ELSE
            SRCDIR = 'from South to North'
         END IF

*  Write information to the screen
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC('AMPST',AMPST(:AMPLN))
         CALL MSG_SETC('UNITS',UNITS(:UNITLN))
         CALL MSG_OUT( 'POINB2_MSG4', 'Source Amplitude: '/
     :                 /'^AMPST (^UNITS)',
     :                  STATUS )
         CALL MSG_SETC('SLPST',SLPST(:SLPLN))
         CALL MSG_SETC('UNITS',UNITS(:UNITLN))
         CALL MSG_SETC('SRCDIR',SRCDIR)
         CALL MSG_OUT( 'POINB2_MSG4', 'Slope           : '/
     :                 /'^SLPST (^UNITS per arcmin ^SRCDIR)',STATUS)
         CALL MSG_SETC('BASST',BASST(:BASLN))
         CALL MSG_SETC('UNITS',UNITS(:UNITLN))
         CALL MSG_OUT( 'POINB2_MSG4', 'Baseline height : '/
     :                 /'^BASST (^UNITS)', STATUS)

*  And to the logfile if required
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            TEMPSTR = UNITS( : UNITLN )
            CALL FIO_WRITE( LOGFID, 'Source Amplitude: '/
     :                  /AMPST( : AMPLN )//' ('//TEMPSTR//')',
     :                      STATUS )
            CALL FIO_WRITE( LOGFID, 'Slope           : '/
     :                     /SLPST( : SLPLN )//' ('//TEMPSTR/
     :                     /' per arcmin '//SRCDIR//')', STATUS )
            CALL FIO_WRITE( LOGFID, 'Baseline height : '/
     :                  /BASST( : BASLN )//' ('//TEMPSTR//')',
     :                      STATUS )
         END IF

* ****************************************************************************
*  Convert noise correlation coefficient etc. and report it
* ****************************************************************************
*  Get the string form of the estimated noise, number of samples used in
*  calculating the noise and the correlation coefficient.
         CALL CHR_RTOC( SCALE * SRCNOS( ISRC ), NOIST, NOILN )
         CALL CHR_ITOC( SRCNON( ISRC ), NOSMST, NOSMLN )
         CALL CHR_RTOC( SRCCOR( ISRC ), CORRST, CORRLN )


*  Report the estimated noise.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC('NOIST',NOIST(:NOILN))
         CALL MSG_SETC('UNITS',UNITS(:UNITLN))
         CALL MSG_OUT( 'POINB2_MSG4', 'Estimated noise : '/
     :                 /'^NOIST (^UNITS)', STATUS)
         CALL MSG_SETC('NOSMST',NOSMST(:NOSMLN))
         CALL MSG_OUT( 'POINB2_MSG5', 'Calculated over  '/
     :                 /'^NOSMST samples, ',STATUS )
         CALL MSG_OUT( 'POINB2_MSG5', '    taken both '/
     :                 /'sides of the point source profile around '/
     :                 /'the source',STATUS )
         CALL MSG_SETC('CORRST', CORRST(:CORRLN))
         CALL MSG_OUT( 'POINB2_MSG6', 'Correlation coef: '/
     :                 /'^CORRST' , STATUS )

*  And to the logfile if required
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            TEMPSTR = UNITS( : UNITLN )
            CALL FIO_WRITE( LOGFID, 'Estimated noise : '/
     :                 /NOIST( : NOILN )//' ('//TEMPSTR//')',
     :                      STATUS )
            CALL FIO_WRITE( LOGFID, 'Calculated over  '/
     :                 /NOSMST( : NOSMLN )//' samples, ',STATUS )
            CALL FIO_WRITE( LOGFID, '    taken both '/
     :                 /'sides of the point source profile around '/
     :                 /'the source',STATUS )
            CALL FIO_WRITE( LOGFID, 'Correlation coef: '/
     :                 /CORRST( : CORRLN ), STATUS )
         END IF

      END DO

      END


