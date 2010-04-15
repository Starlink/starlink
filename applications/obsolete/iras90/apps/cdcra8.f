      SUBROUTINE CDCRA8( FID, GID, NCRDD, NDFID, IRCID, SCNDIR, SOP,
     :                   OBS, BAND, NCROS, CRSFLX, CRSDTX, CRSDIS,
     :                   CRSSMP, CRSFLG, STATUS )
*+
*  Name:
*     CDCRA8

*  Purpose:
*     Write the crossing information to the log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA8( FID, GID, NCRDD, NDFID, IRCID, SCNDIR, SOP,
*                  OBS, BAND, NCROS, CRSFLX, CRSDTX, CRSDIS,
*                  CRSSMP, CRSFLG, STATUS )

*  Description:
*     This subroutine is used to write crossing information: SOP number,
*     observation number, waveband number, crossing NDF name, crossing
*     detector number, X-scan crossing distance, crossing sample and
*     crossing direction about a given source to a log file.

*  Arguments:
*     FID = INTEGER (Given)
*        ID of the logging file.
*     GID = INTEGER (Given)
*        Group ID of the input CRDD NDF group.
*     NCRDD = INTEGER (Given)
*        Number of input CRDD NDF files.
*     NDFID( NCRDD ) = INTEGER (Given)
*        NDF IDs of input CRDD NDFs.
*     IRCID( NCRDD ) = INTEGER (Given)
*        IRC IDs of input CRDD NDFs.
*     CRSDIR( NCRDD ) = LOGICAL (Given)
*        Crossing direction of each CRDD scan.
*     SOP( NCRDD ) = INTEGER (Given)
*        SOP number of input CRDD NDFs.
*     OBS( NCRDD ) = INTEGER (Given)
*        Observation number on input CRDD NDFs.
*     BAND = INTEGER (Given)
*        Wave band number of the crossings.
*     NCROS = INTEGER (Given)
*        Number of crossing.
*     CRSFLX( NCROS ) = INTEGER (Given)
*        File index of each crossing.
*     CRSDTX( NCROS ) = INTEGER (Given)
*        Detector index of each crossing.
*     CRSDIR( NCROS ) = REAL (Given)
*        X-scan distance of each crossing.
*     CRSSMP( NCROS ) = REAL (Given)
*        Crossing sample of each crossing.
*     CRSFLG( NCROS ) = INTEGER (Given)
*        Crossing flag. 1 - more than half samples of this crossing
*        section are bad and should be discarded while coadding. 2 -
*        this crossing section is valid and will be used when coadding.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     23-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FID, GID
      INTEGER NCRDD
      INTEGER NDFID( NCRDD )
      INTEGER IRCID( NCRDD )
      LOGICAL SCNDIR( NCRDD )
      INTEGER SOP( NCRDD ), OBS( NCRDD )
      INTEGER BAND
      INTEGER NCROS
      INTEGER CRSFLX( NCROS ), CRSDTX( NCROS )
      REAL CRSDIS( NCROS ), CRSSMP( NCROS )
      INTEGER CRSFLG( NCROS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER IRC_DETNO          ! Detector number of an index

*  Local Variables:
      CHARACTER*( 20 ) AMODE     ! Access mode of input NDF
      INTEGER DET                ! Detector number
      CHARACTER*( 20 ) DRST      ! Scan direction string
      CHARACTER*( 2 ) DTST       ! String form of detector number
      CHARACTER*( 20 ) DEVICE    ! Name of device holding CRDD NDF
      CHARACTER*( 20 ) DIRN      ! Name of dir CRDD NDF reside in
      CHARACTER*( 120 ) FULNAM   ! Full name of input CRDD NDF file
      INTEGER I                  ! Do loop index
      CHARACTER*( 3 ) ICST       ! String form of I
      CHARACTER*( 60 ) NAME      ! Name of CRDD NDF file
      INTEGER NAMELN             ! Length of the name of a CRDD NDF
      INTEGER NCHAR              ! Number of characters in a string
      CHARACTER*( 3 ) NCROST     ! String form of number of crossing
      CHARACTER*( 10 ) OBST      ! String form of observation number
      LOGICAL OUT                ! Output NDF flag
      CHARACTER*( 20 ) SLICE     ! Slice specification of input NDF
      CHARACTER*( 5 ) SPST       ! String form of SOP number
      CHARACTER*( 10 ) XDST      ! String form of X-scan distance
      INTEGER XDSTLN             ! Used length of XDST
      CHARACTER*( 10 ) SMST      ! String form of crossing sample no.
      CHARACTER*( 6 ) WVST       ! Wavelength string
      INTEGER WVSTLN             ! Used length of WVST

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the wavelength from the band number.
      IF ( BAND .EQ. 1 ) THEN
         WVST = '12 um'
         WVSTLN = 5
      ELSE IF ( BAND .EQ. 2 ) THEN
         WVST = '25 um'
         WVSTLN = 5
      ELSE IF ( BAND .EQ. 3 ) THEN
         WVST = '60 um'
         WVSTLN = 5
      ELSE IF ( BAND .EQ. 4 ) THEN
         WVST = '100 um'
         WVSTLN = 6
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CDCRA8_ERR1', 'Invalid waveband number',
     :                  STATUS )
         GOTO 999
      END IF

*  Write total amount of crossings.
      CALL CHR_ITOC( NCROS, NCROST, NCHAR )
      CALL FIO_WRITE( FID, '   There are total '//NCROST( : NCHAR )/
     :              /' crossings from '//WVST( : WVSTLN )/
     :              /' waveband for this expected source.', STATUS )
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Report the crossings one by one.
      DO I = 1, NCROS

*  Get the file name and detector number of this crossing.
         CALL NDF_MSG( 'NDF', NDFID( CRSFLX( I ) ) )
         CALL MSG_LOAD( ' ', '^NDF', FULNAM, NAMELN, STATUS )
         CALL IRM_FILNM( FULNAM( : NAMELN ), NAME, STATUS )
         NAMELN = CHR_LEN( NAME )
         DET = IRC_DETNO( IRCID( CRSFLX( I ) ), CRSDTX( I ), STATUS )
         CALL CHR_ITOC( DET, DTST, NCHAR )

*  Get the scan direction of this crossing.
         IF ( SCNDIR( CRSFLX( I ) ) ) THEN
            DRST = 'From North to South'
         ELSE
            DRST = 'From South to North'
         END IF

*  Write an item for this crossing.
         CALL CHR_ITOC( I, ICST, NCHAR )
         CALL CHR_ITOC( SOP( CRSFLX( I ) ), SPST, NCHAR )
         CALL CHR_ITOC( OBS( CRSFLX( I ) ), OBST, NCHAR )
         CALL CHR_RTOC( CRSDIS( I ), XDST, XDSTLN )
         CALL CHR_RTOC( CRSSMP( I ), SMST, NCHAR )
         CALL FIO_WRITE( FID, '      Crossing '//ICST//':',
     :                   STATUS )
         CALL FIO_WRITE( FID, '         CRDD NDF name     : '/
     :                  /NAME( : NAMELN ), STATUS )
         CALL FIO_WRITE( FID, '         Detector Number   : '//DTST,
     :                   STATUS )
         CALL FIO_WRITE( FID, '         X-scan distance   : '/
     :                  /XDST( : XDSTLN )//' (arcmin)', STATUS )
         CALL FIO_WRITE( FID, '         Crossing at sample: '//SMST,
     :                   STATUS )
         CALL FIO_WRITE( FID, '         Crossing direction: '//DRST,
     :                   STATUS )
         CALL FIO_WRITE( FID, '         SOP number        : '//SPST,
     :                   STATUS )
         CALL FIO_WRITE( FID, '         Observation number: '//OBST,
     :                   STATUS )
         CALL FIO_WRITE( FID, ' ', STATUS )

*  If this crossing is discarded, because it contains more than half of
*  its length the bad samples, write a note to the logging file.
         IF ( CRSFLG( I ) .EQ. 1 ) THEN
            CALL FIO_WRITE( FID, '         Since more than half '/
     :                    /'of its samples are bad, this crossing is '/
     :                    /'discarded while coadding', STATUS )
            CALL FIO_WRITE( FID, ' ', STATUS )
         END IF
      END DO

 999  CONTINUE

      END
