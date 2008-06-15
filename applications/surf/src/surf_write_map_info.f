      SUBROUTINE SURF_WRITE_MAP_INFO( OUT_NDF, OUT_COORDS, OUT_TITLE,
     :     OUT_UNITS, MJD_STANDARD, NFILE, FILENAME, OUT_LONG, OUT_LAT,
     :     OUT_PIXEL, I_CENTRE, J_CENTRE, NX_OUT, NY_OUT, WAVELENGTH,
     :     FILTER, WRITE_CHOP, CHOP_CRD, CHOP_PA, CHOP_THROW, 
     :     WPLATE, ANGROT, TELESCOPE, INSTRUMENT,
     :     STATUS)
*+
*  Name:
*     SURF_WRITE_MAP_INFO

*  Purpose:
*     Calculate FITS and WCS information for rebinned image

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURF_WRITE_MAP_INFO( OUT_NDF, OUT_COORDS, OUT_TITLE,
*    :     MJD_STANDARD, NFILE, FILENAME, OUT_LONG, OUT_LAT,
*    :     OUT_PIXEL, I_CENTRE, J_CENTRE, NX_OUT, NY_OUT, WAVELENGTH
*    :     FILTER, WRITE_CHOP, CHOP_PA, CHOP_THROW, WPLATE, ANGROT, 
*    :     STATUS )

*  Description:
*     This routine writes Axis, FITS and WCS information to a rebinned
*     map file. Note that this routine does not write WCS information
*     to the FITS header - that is stored in the WCS information.
*     Keywords CRVAL, CRPIX and CTYPE are used here to generate the
*     WCS only.

 
*  Arguments:
*     OUT_NDF = INTEGER (Given)
*        NDF identifier of output file
*     OUT_COORDS = CHAR (Given)
*        Output coordinate system
*     OUT_TITLE = CHAR (Given)
*        Title of map
*     OUT_UNITS = CHAR (Given)
*        Output units
*     MJD_STANDARD = DOUBLE (Given)
*        Modified Julian date of map
*     NFILE = INTEGER (Given)
*        Number of files in input map
*     FILENAME = CHAR(NFILE) (Given)
*        Names of input files
*     OUT_PIXEL = REAL (Given)
*        Size of pixel in output map (radians)
*     OUT_LONG  = DOUBLE (Given)
*         Longitude of output map (radians)
*     OUT_LAT  = DOUBLE (Given)
*         Latitude of output map (radians)
*     I_CENTRE = INTEGER (Given)
*        Position of X reference pixel in output map
*     J_CENTRE = INTEGER (Given)
*        Position of Y reference pixel in output map
*     NX_OUT = INTEGER (Given)
*        Size of output map in X direction
*     NY_OUT = INTEGER (Given)
*        Size of output map in Y direction
*     WAVELENGTH = REAL (Given)
*        Wavelength of the map (microns)
*     FILTER = CHARACTER (Given)
*        Name of filter
*     WRITE_CHOP = LOGICAL (Given)
*        Write the CHOP information to the header
*     CHOP_CRD = CHARACTER (Given)
*        Chop coordinate frame
*     CHOP_PA = REAL (Given)
*        Chop position angle in degrees east of north
*     CHOP_THROW = REAL (Given)
*        Chop throw in arcsec
*     WPLATE = REAL (Given)
*        Wave plate angle (ignored if bad value).
*     ANGROT = REAL (Given)
*        POLPACK rotation angle between waveplate and X pixel axis
*        (ignored if bad value)
*     TELESCOPE = CHARACTER (Given)
*        Name of telescope to write to FITS header
*     INSTRUMENT = CHARACTER (Given)
*        Name of instrument to write to FITS header
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  
*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1995-2005 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 April 08 (TIMJ):
*        Extract from reds_wtfn_rebin.f
*     1997 May 12 (TIMJ):
*        Remove FITS from input arguments
*     1997 May 13 (TIMJ):
*        Add support for PL.
*     1998 April 28 (TIMJ):
*        Write chop information to FITS
*     1999 May 15 (TIMJ):
*        - Remove IRAS90 code
*        - make sure that WCS extracts FITS keywords before writing to disk.
*     1999 July 14 (TIMJ):
*        Pass TELESCOPE and INSTRUMENT in from above.
*     1999 August 3 (TIMJ):
*        Add copyyright to header.
*        Minor fixes to header style.
*     1999 August 19 (TIMJ):
*        Header tweaks to help SSN/72 creation
*     2000 June 23 (TIMJ):
*        - Add DATE and ORIGIN keywords
*        - Add HH:MM:SS to DATE-OBS field
*     2000 July 7 (TIMJ):
*        Fix typo in FITS comment
*     2003 April 2 (TIMJ):
*        - Units for FITS headers now appear at start of comment
*        - Use PSX_GMTIME rather than GMTIME
*     2004 July 14 (TIMJ):
*        No FPP so rename file to .f
*     2004 September 8 (TIMJ):
*        Add CNF_PVAL where appropriate.
*     2004 November 18 (TIMJ):
*        - Annul AST objects
*        - Forgot to pass STATUS into PSX_TIME
*     2005 March 18 (TIMJ):
*        - Warn if frequency can not be written when Wavelength is zero.
*     2008 June 14 (TIMJ):
*        - Write OBSGEO headers. Retain DATE-OBS.
*     {write_additional_history_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'SURF_PAR'         ! REDS constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER          NFILE
      REAL             ANGROT
      CHARACTER*(*)    CHOP_CRD
      REAL             CHOP_PA
      REAL             CHOP_THROW
      CHARACTER*(*)    FILENAME (NFILE)
      CHARACTER*(*)    FILTER
      CHARACTER*(*)    INSTRUMENT
      INTEGER          I_CENTRE
      INTEGER          J_CENTRE
      DOUBLE PRECISION MJD_STANDARD
      INTEGER          NX_OUT
      INTEGER          NY_OUT
      CHARACTER * (*)  OUT_COORDS
      DOUBLE PRECISION OUT_LAT
      DOUBLE PRECISION OUT_LONG
      INTEGER          OUT_NDF
      REAL             OUT_PIXEL
      CHARACTER * (*)  OUT_TITLE
      CHARACTER * (*)  OUT_UNITS
      CHARACTER*(*)    TELESCOPE
      LOGICAL          USEWCS
      REAL             WAVELENGTH
      REAL             WPLATE
      LOGICAL          WRITE_CHOP

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER          CHR_LEN
      DOUBLE PRECISION SLA_EPJ

*  Local Constants:
      DOUBLE PRECISION VLIGHT   ! Speed of light in metres per second
      PARAMETER (VLIGHT = 299792458.0D0)
      DOUBLE PRECISION MJD99    ! MJD of 1 Jan 1999
      PARAMETER ( MJD99 = 51179.0D0 )
      INTEGER NRETAIN           ! Number of FITS headers to retain
      PARAMETER ( NRETAIN = 4 )

*  Local Variables:
      CHARACTER*10     CTYPE1   ! Coordinate type of output FITS
      CHARACTER*10     CTYPE2   ! Coordinate type of output FITS
      CHARACTER*24     DATEOBS  ! Date of map obs
      DOUBLE PRECISION DJM      ! current mjd
      DOUBLE PRECISION DTEMP    ! Scratch double
      REAL             FITS_OUT_PIXEL  ! size of pixels in output map (degrees)
      CHARACTER * 80   FITS(SCUBA__MAX_FITS) ! Output FITS array
      INTEGER          FITSCHAN ! AST FITS channel
      INTEGER          FRAME    ! Base frame for WCS/AST
      DOUBLE PRECISION FREQ     ! Frequency of observation
      INTEGER          I        ! Counter
      INTEGER          ICARD    ! Counter for FITS cards
      INTEGER          ID       ! day of an input observation
      INTEGER          IM       ! month in which observation started
      INTEGER          ITEMP    ! Temporary variable
      INTEGER          IY       ! year in which input observation started
      INTEGER          NDIM     ! Number of dimensions in FITS array
      INTEGER          NTICKS   ! Current time in sedonds
      INTEGER          N_FITS   ! Number of FITS entries
      DOUBLE PRECISION OBSRA    ! RA of output map (degrees)
      DOUBLE PRECISION OBSDEC   ! Dec of output map (degrees)
      INTEGER          OUT_A_PTR ! Pointer to output axis
      DOUBLE PRECISION OUT_EPOCH ! epoch of output map
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC
                                ! locator of FITS extension in output
                                ! file
      CHARACTER*(8)    RETAINFITS(NRETAIN) ! FITS headers to retain
      REAL             RTEMP    ! Scratch real
      CHARACTER*30     SCS      ! name of sky coordinate system
      CHARACTER*80     STEMP    ! scratch string
      INTEGER          TARRAY(9) ! Array of times from GMTIME function
      CHARACTER*5      RADECSYS ! Type of coordinate system
      DOUBLE PRECISION RDEPOCH  ! Julian epoch of given MJD
      CHARACTER * (80) XLAB     ! X axis label
      CHARACTER * (80) YLAB     ! Y axis label 
      INTEGER          WCSINFO  ! WCS AST information

*  Local Data:
      DATA RETAINFITS / 'DATE-OBS', 'OBSGEO-X', 'OBSGEO-Y',
     :     'OBSGEO-Z' /

*.

*     Check status
      IF (STATUS .NE. SAI__OK) RETURN

*     Flag to indicate whether we need a world coordinate system
      IF (OUT_COORDS .NE. 'NA'.AND.OUT_COORDS.NE.'AZ' .AND.
     :     OUT_COORDS .NE. 'PL') THEN
         USEWCS = .TRUE.
      ELSE
         USEWCS = .FALSE.
      END IF

*     set up the output axes

      IF (OUT_COORDS .EQ. 'GA') THEN
         XLAB = 'Longitude offset'
         YLAB = 'Latitude offset'
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         XLAB = 'X Nasmyth offset'
         YLAB = 'Y Nasmyth offset'
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         XLAB = 'Azimuth offset'
         YLAB = 'Elevation offset'
      ELSE
         XLAB = 'R.A. offset'
         YLAB = 'Declination offset'
      END IF

      CALL NDF_AMAP (OUT_NDF, 'CENTRE', 1, '_REAL', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLR (NX_OUT, %VAL(CNF_PVAL(OUT_A_PTR)))
         CALL SCULIB_ADDCAR (NX_OUT, %VAL(CNF_PVAL(OUT_A_PTR)),
     :        REAL(-I_CENTRE), %VAL(CNF_PVAL(OUT_A_PTR)))
         CALL SCULIB_MULCAR (NX_OUT, %VAL(CNF_PVAL(OUT_A_PTR)),
     :        -OUT_PIXEL * REAL(R2AS), %VAL(CNF_PVAL(OUT_A_PTR)))
      END IF
      CALL NDF_ACPUT (XLAB, OUT_NDF, 'LABEL', 1, STATUS)
      CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 1, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 1, STATUS)

      CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLR (NY_OUT, %VAL(CNF_PVAL(OUT_A_PTR)))
         CALL SCULIB_ADDCAR (NY_OUT, %VAL(CNF_PVAL(OUT_A_PTR)),
     :        REAL(-J_CENTRE), %VAL(CNF_PVAL(OUT_A_PTR)))
         CALL SCULIB_MULCAR (NY_OUT, %VAL(CNF_PVAL(OUT_A_PTR)),
     :        OUT_PIXEL * REAL(R2AS), %VAL(CNF_PVAL(OUT_A_PTR)))
      END IF
      CALL NDF_ACPUT (YLAB, OUT_NDF, 'LABEL', 2, STATUS)
      CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 2, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)

*     and a title

      CALL NDF_CPUT(OUT_TITLE, OUT_NDF, 'Title', STATUS)
      CALL NDF_CPUT(OUT_UNITS, OUT_NDF, 'UNITS', STATUS)

*     Sort out the astrometry stuff

      IF (USEWCS) THEN

         IF (OUT_COORDS .EQ. 'RB') THEN
            SCS = 'EQUATORIAL(B1950.0)'
            OUT_EPOCH = 1950.0D0
            RADECSYS  = 'FK4'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
            SCS = 'EQUATORIAL(J2000.0)'
            OUT_EPOCH = 2000.0D0
            RADECSYS  = 'FK5'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'RD') THEN

*     Calculate current epoch
            RDEPOCH = SLA_EPJ (MJD_STANDARD) 
            SCS = 'EQUATORIAL(J'
            CALL CHR_RTOC(REAL(RDEPOCH), STEMP, ITEMP)
            CALL CHR_APPND(STEMP, SCS, CHR_LEN(SCS))
            CALL CHR_APPND(')', SCS, CHR_LEN(SCS))
            OUT_EPOCH = RDEPOCH
            RADECSYS  = 'FK5'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'EQ') THEN ! We dont use EQ...
            SCS = 'ECLIPTIC(J2000.0)'
            OUT_EPOCH = 2000.D0
            RADECSYS  = 'GAPPT'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'GA') THEN
            SCS = 'GALACTIC(J2000.0)'
            OUT_EPOCH = 2000.0D0
            CTYPE1 = 'GLON-TAN'
            CTYPE2 = 'GLAT-TAN'
         END IF

      END IF

*     PLanet is special

      IF (OUT_COORDS  .EQ. 'PL') THEN
         CTYPE1 = 'RA---TAN'
         CTYPE2 = 'DEC--TAN'
      END IF

*     and write out the same information to a .MORE.FITS section

      N_FITS = 0

*     Dont store OBJECT - NDF Title takes priority
*      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
*     :     'OBJECT', OUT_TITLE, 'name of object', STATUS)

      DO I = 1, NFILE
         STEMP = 'FILE_'
         ITEMP = 5
         CALL CHR_PUTI (I, STEMP, ITEMP)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        STEMP,FILENAME(I), 'name of input datafile', STATUS)
      END DO

      IF (OUT_COORDS.NE.'GA' .AND. OUT_COORDS.NE.'NA'
     :     .AND.OUT_COORDS.NE.'AZ' .AND. OUT_COORDS .NE.'PL') THEN
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'RADECSYS', RADECSYS, 'Frame of reference', STATUS)
      END IF

      IF (USEWCS) THEN
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'SYSTEM', SCS, 'sky coordinate system', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'LONG', OUT_LONG, '[rad] centre longitude', 
     :        STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'LAT', OUT_LAT, '[rad] centre latitude', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'EPOCH', OUT_EPOCH, 'epoch of map', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS,FITS,
     :        'EQUINOX', OUT_EPOCH, 
     :        'epoch of mean equator and equinox', STATUS)
         
      END IF

*     Position of the telescope in standard form. Hardwire location.
      IF (TELESCOPE .EQ. 'JCMT') THEN
         CALL SCULIB_PUT_FITS_D( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'OBSGEO-X', -5464587.157627D0, '[m]', STATUS)
         CALL SCULIB_PUT_FITS_D( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'OBSGEO-Y', -2493002.332712D0, '[m]', STATUS)
         CALL SCULIB_PUT_FITS_D( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'OBSGEO-Z', 2150655.061682D0, '[m]', STATUS)

      END IF


      CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'MJD-OBS', MJD_STANDARD, 'MJD of first observation', 
     :     STATUS)

      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'TELESCOP', TELESCOPE, 'name of telescope', STATUS)
      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'INSTRUME', INSTRUMENT, 'name of instrument', STATUS)

*     Store the wavelength (in metres) and frequency (in Hz)

      CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'WAVELEN', DBLE(WAVELENGTH) * 1.0D-6,
     :     '[m] Wavelength of the observation', STATUS)

      IF (WAVELENGTH .GT. 0) THEN
         FREQ = VLIGHT * 1.0D6 / DBLE(WAVELENGTH)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'FREQ', FREQ, '[Hz] Frequency of observation', STATUS)
      ELSE
         CALL MSG_OUTIF( MSG__QUIET, ' ',
     :        'WARNING: Wavelength not defined. Unable to write '//
     :        'Frequency header', STATUS)
      END IF

*     Write the FILTER name
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'FILTER', FILTER, 'SCUBA filter name',
     :     STATUS)

*     Store the polarimeter information

      IF (ANGROT .NE. VAL__BADR) THEN
         CALL SCULIB_PUT_FITS_D( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'ANGROT', DBLE(ANGROT), 
     :        '[deg] Angle between X-pixel axis and wplate 0', STATUS)
      END IF
      IF (WPLATE .NE. VAL__BADR) THEN
         CALL SCULIB_PUT_FITS_D( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'WPLATE', DBLE(WPLATE), 
     :        '[deg] Wave-plate P.A. (anti-clockwise)',
     :        STATUS)
      END IF

*     Store SCUBA projection name

      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SCUPROJ',OUT_COORDS, 'SCUBA output coordinate system', 
     :     STATUS)

*     Pixel size as chosen in REBIN (arcsec)

      RTEMP = OUT_PIXEL * REAL(R2AS) ! Generate rounding errors

      CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SCUPIXSZ', DBLE(RTEMP), 
     :     '[arcsec] Pixel size', STATUS)

*     Store the chop information

      IF (WRITE_CHOP) THEN
         CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CHOP_CRD',CHOP_CRD, 'Coordinate system of chop', 
     :        STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CHOP_PA', DBLE(CHOP_PA), 
     :        '[deg] Position angle of chop', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CHOP_THR', DBLE(CHOP_THROW), 
     :        '[arcsec] Size of chop throw', STATUS)
      END IF

*     Put in a DATE-OBS field, converting MJD to DATE

      CALL SCULIB_MJD_TO_DATEOBS( MJD_STANDARD, DATEOBS, STATUS)

      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'DATE-OBS', DATEOBS, 'Date of first observation', 
     :     STATUS)

*     Put in a DATE field (the date the file was written)
*      - Dont feel the need to call MJD_TO_DATEOBS since we know PSX_GMTIME
*        gives the Y/M/D etc and we are now always going to be writing
*        post-1999 FITS format since we cant go back in time!
      CALL PSX_TIME( NTICKS, STATUS )   ! Time in seconds
      CALL PSX_GMTIME( NTICKS, TARRAY(1), TARRAY(2), TARRAY(3),
     :     TARRAY(4), TARRAY(5), TARRAY(6), TARRAY(7), TARRAY(8), 
     :     ITEMP, STATUS )

      TARRAY(6) = TARRAY(6) + 1900 ! 4 digit year
      TARRAY(5) = TARRAY(5) + 1 ! Months start at 1 for SLA

*     Now write into the dateobs string (do not need the fraction of second
*     part)
      DATEOBS = ' ' ! reset the string
      WRITE (DATEOBS, '(I4.4,''-'',I2.2,''-'',I2.2,''T'',
     :     I2.2,'':'',I2.2,'':'',I2.2)')
     :     TARRAY(6), TARRAY(5), TARRAY(4), TARRAY(3), TARRAY(2), 
     :     TARRAY(1)

*     Write to the fits array
      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'DATE', DATEOBS, 
     :     'file creation date (YYYY-MM-DDThh:mm:ss UTC)', 
     :     STATUS)


*     Add an ORIGIN keyword
      CALL SCULIB_PUT_FITS_C( SCUBA__MAX_FITS, N_FITS, FITS,
     :     'ORIGIN',  'SCUBA User Reduction Facility (SURF)',
     :     'Origin of the FITS file', STATUS)


*     Now need to calculate the FITS Axis info
*     If this is NA then NDF2FITS will do this for us

      IF (USEWCS) THEN

         OBSRA = OUT_LONG * 180.0D0 / PI
         OBSDEC= OUT_LAT  * 180.0D0 / PI
         FITS_OUT_PIXEL = OUT_PIXEL * REAL(180.0D0 / PI)
         
         IF (OUT_COORDS.NE.'GA') THEN
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'OBSRA',OBSRA,
     :           '[deg] RA of map centre (deprecated)', STATUS)
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS,N_FITS, FITS, 
     :           'OBSDEC', OBSDEC, 
     :           '[deg] Dec. of map centre (deprecated)',
     :           STATUS)
         END IF

         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CTYPE1', CTYPE1,'TAN projection used', STATUS)
         CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CRPIX1', I_CENTRE, 'I of centre (ref) pixel', 
     :        STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CRVAL1', OBSRA, '[deg] Map centre', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CDELT1',DBLE(-FITS_OUT_PIXEL), 
     :        '[deg] increment per pixel',
     :        STATUS)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CUNIT1', 'deg','physical units of axis 1', STATUS)
         

         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CTYPE2', CTYPE2,'TAN projection used', STATUS)
         CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CRPIX2', J_CENTRE, 'J of centre (ref) pixel', 
     :        STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CRVAL2', OBSDEC, '[deg] Map centre', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CDELT2', DBLE(FITS_OUT_PIXEL), 
     :        '[deg] increment per pixel',
     :        STATUS)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CUNIT2','deg','physical units of axis 2', STATUS)
         
      ELSE IF (OUT_COORDS .EQ. 'PL') THEN
*     We should still write out tangent plane

         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CTYPE1', CTYPE1,'TAN projection used', STATUS)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CTYPE2', CTYPE2,'TAN projection used', STATUS)

      END IF

*     Create the AST extension

      IF (USEWCS) THEN
         CALL AST_BEGIN( STATUS )

*     Create a FitsChan and fill it with FITS header cards.
         FITSCHAN = AST_FITSCHAN( AST_NULL, AST_NULL, 
     :        ' ', STATUS )
         DO ICARD = 1, N_FITS
            CALL AST_PUTFITS( FITSCHAN, FITS( ICARD ), .FALSE., 
     :           STATUS )
         END DO

*     Retain some keywords in the FITS header itself
         DO I = 1, NRETAIN
            CALL AST_CLEAR( FITSCHAN, 'Card', STATUS )
            IF (AST_FINDFITS( FITSCHAN, RETAINFITS(I),STEMP,
     :           .FALSE., STATUS)) THEN
               CALL AST_RETAINFITS( FITSCHAN, STATUS )
            END IF
         END DO

         
*     Rewind the FitsChan and read WCS information from it.
         CALL AST_CLEAR( FITSCHAN, 'Card', STATUS )
         WCSINFO = AST_READ( FITSCHAN, STATUS )

*     Set the base frame to be GRID
         FRAME = AST_GETFRAME( WCSINFO, AST__BASE, STATUS )
         CALL AST_SETC( FRAME, 'Domain', 'GRID', STATUS )
         
*     Write the channel to the NDF
         CALL NDF_PTWCS( WCSINFO, OUT_NDF, STATUS )

*     Now copy the remaining FITS keywords from the FITSCHAN
*     back into the FITS array so that we can write them
*     We do this so that the information in the WCS-AST structure
*     is not duplicated by information in the FITS array
*     This would lead to confusion

*     Rewind the FITSCHAN
         CALL AST_CLEAR (FITSCHAN, 'Card', STATUS)

*     Loop over all fits cards and copy back into the FITS array
         N_FITS = 0
         DO WHILE (AST_FINDFITS( FITSCHAN, '%f', STEMP, .TRUE.,
     :        STATUS))
            N_FITS = N_FITS + 1
            FITS(N_FITS) = STEMP
         END DO

*     Shut down AST
         CALL AST_ANNUL( WCSINFO, STATUS )
         CALL AST_ANNUL( FITSCHAN, STATUS )
         CALL AST_END( STATUS )

      END IF

*     write out the FITS extension

      NDIM =1 
      CALL NDF_XNEW (OUT_NDF, 'FITS', '_CHAR*80', NDIM, N_FITS, 
     :     OUT_FITSX_LOC, STATUS)
      CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)
      CALL DAT_ANNUL (OUT_FITSX_LOC, STATUS)

      END
