      SUBROUTINE REDS_WRITE_MAP_INFO( OUT_NDF, OUT_COORDS, OUT_TITLE,
     :     MJD_STANDARD, NFILE, FILENAME, OUT_LONG, OUT_LAT,
     :     OUT_PIXEL, I_CENTRE, J_CENTRE, NX_OUT, NY_OUT, STATUS)
*+
*  Name:
*     REDS_WRITE_MAP_INFO

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL REDS_WRITE_MAP_INFO( OUT_NDF, OUT_COORDS, OUT_TITLE,
*    :     MJD_STANDARD, NFILE, FILENAME, OUT_LONG, OUT_LAT,
*    :     OUT_PIXEL, I_CENTRE, J_CENTRE, NX_OUT, NY_OUT,
*    :     STATUS )

*  Description:
*     This routine writes Axis and FITS information to a rebinned
*     map file.

 
*  Arguments:
*     OUT_NDF = INTEGER (Given)
*        NDF identifier of output file
*     OUT_COORDS = CHAR (Given)
*        Output coordinate system
*     OUT_TITLE = CHAR (Given)
*        Title of map
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
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  
*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)

*  History:
*     1997 April 08 (TIMJ)
*        Extract from reds_wtfn_rebin.f
*     1997 May 12 (TIMJ)
*        Remove FITS from input arguments

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'REDS_SYS'         ! REDS constants

*  Arguments Given:
      INTEGER          NFILE
      CHARACTER*40     FILENAME (NFILE)
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

*  Status
      INTEGER STATUS

*  External References:
      INTEGER          CHR_LEN
      DOUBLE PRECISION SLA_EPJ

*  Local Variables:
      CHARACTER*10     CTYPE1   ! Coordinate type of output FITS
      CHARACTER*10     CTYPE2   ! Coordinate type of output FITS
      CHARACTER*12     DATEOBS  ! Date of map obs
      DOUBLE PRECISION DTEMP    ! Scratch double
      REAL             FITS_OUT_PIXEL  ! size of pixels in output map (degrees)
      CHARACTER * 80   FITS(SCUBA__MAX_FITS) ! Output FITS array
      INTEGER          I        ! Counter
      INTEGER          ID       ! day of an input observation
      INTEGER          IM       ! month in which observation started
      CHARACTER*(DAT__SZLOC) IMAGE_LOC ! locator of IMAGE_INFO in output file
      CHARACTER*40     INSTRUMENT ! FITS instrument entry
      INTEGER          ITEMP    ! Temporary variable
      INTEGER          IY       ! year in which input observation started
      INTEGER          NDIM     ! Number of dimensions in FITS array
#ifdef HAS_IRAS90
      INTEGER          NP       ! size of P array in call to IRA_CREAT
#endif
      INTEGER          N_FITS   ! Number of FITS entries
      DOUBLE PRECISION OBSRA    ! RA of output map (degrees)
      DOUBLE PRECISION OBSDEC   ! Dec of output map (degrees)
      INTEGER          OUT_A_PTR ! Pointer to output axis
      DOUBLE PRECISION OUT_EPOCH ! epoch of output map
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC
                                ! locator of FITS extension in output
                                ! file
      CHARACTER*(DAT__SZLOC) OUT_XLOC ! locator of IRAS item in output file
      DOUBLE PRECISION P (8)    ! input array to IRA_CREAT
      CHARACTER*30     SCS      ! name of sky coordinate system
      CHARACTER*80     STEMP    ! scratch string
      CHARACTER*10     TELESCOPE ! FITS telescope entry
      CHARACTER*5      RADECSYS ! Type of coordinate system
      DOUBLE PRECISION RDEPOCH  ! Julian epoch of given MJD
      CHARACTER * (80) XLAB     ! X axis label
      CHARACTER * (80) YLAB     ! Y axis label 
*     .

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
     :           OUT_A_PTR, ITEMP, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_NFILLR (NX_OUT, %val(OUT_A_PTR))
               CALL SCULIB_ADDCAR (NX_OUT, %val(OUT_A_PTR), 
     :              REAL(-I_CENTRE), %val(OUT_A_PTR))
               CALL SCULIB_MULCAR (NX_OUT, %val(OUT_A_PTR), 
     :              -OUT_PIXEL * REAL(R2AS), %val(OUT_A_PTR))
            END IF
            CALL NDF_ACPUT (XLAB, OUT_NDF, 'LABEL', 1, STATUS)
            CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 1, STATUS)
            CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 1, STATUS)

            CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :           OUT_A_PTR, ITEMP, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_NFILLR (NY_OUT, %val(OUT_A_PTR))
               CALL SCULIB_ADDCAR (NY_OUT, %val(OUT_A_PTR), 
     :              REAL(-J_CENTRE), %val(OUT_A_PTR))
               CALL SCULIB_MULCAR (NY_OUT, %val(OUT_A_PTR),
     :              OUT_PIXEL * REAL(R2AS), %val(OUT_A_PTR))
            END IF
            CALL NDF_ACPUT (YLAB, OUT_NDF, 'LABEL', 2, STATUS)
            CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 2, STATUS)
            CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)

*     and a title

            CALL NDF_CPUT(OUT_TITLE, OUT_NDF, 'Title', STATUS)
            CALL NDF_CPUT('Volts', OUT_NDF, 'UNITS', STATUS)

*     create the IRAS astrometry structure

            IF (OUT_COORDS .NE. 'NA'.AND.OUT_COORDS.NE.'AZ' .AND.
     :           OUT_COORDS .NE. 'PL') THEN

#ifdef HAS_IRAS90
               CALL IRA_INIT (STATUS)
#endif

               P (1) = OUT_LONG
               P (2) = OUT_LAT
               P (3) = DBLE (I_CENTRE) - 0.5D0
               P (4) = DBLE (J_CENTRE) - 0.5D0
               P (5) = DBLE (OUT_PIXEL)
               P (6) = DBLE (OUT_PIXEL)
               P (7) = 0.0D0
               P (8) = 0.0D0

               IF (OUT_COORDS .EQ. 'RB') THEN
                  SCS = 'EQUATORIAL(1950.0)'
                  OUT_EPOCH = 1950.0D0
                  RADECSYS  = 'FK4'
                  CTYPE1 = 'RA---TAN'
                  CTYPE2 = 'DEC--TAN'
               ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
                  SCS = 'EQUATORIAL(2000.0)'
                  OUT_EPOCH = 2000.0D0
                  RADECSYS  = 'FK5'
                  CTYPE1 = 'RA---TAN'
                  CTYPE2 = 'DEC--TAN'
               ELSE IF (OUT_COORDS .EQ. 'RD') THEN

*       Calculate current epoch
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
                  SCS = 'ECLIPTIC(2000.0)'
                  OUT_EPOCH = 2000.D0
                  RADECSYS  = 'GAPPT'
                  CTYPE1 = 'RA---TAN'
                  CTYPE2 = 'DEC--TAN'
               ELSE IF (OUT_COORDS .EQ. 'GA') THEN
                  SCS = 'GALACTIC'
                  OUT_EPOCH = 2000.0D0
                  CTYPE1 = 'GLON-TAN'
                  CTYPE2 = 'GLAT-TAN'
               END IF

#ifdef HAS_IRAS90
*     Create the IRAS extension
               CALL NDF_XNEW (OUT_NDF, 'IRAS', 'IRAS_EXTENSION', 0,0,
     :              OUT_XLOC, STATUS)
               NP = 8

*     Create the ASTROMETRY extension
               CALL IRA_CREAT ('GNOMONIC', NP, P, SCS, OUT_EPOCH, 
     :              OUT_NDF, ITEMP, STATUS)

               CALL IRA_CLOSE (STATUS)
#endif

*     Create the IMAGE_INFO extension to fool IRAS90 software
               CALL DAT_NEW(OUT_XLOC, 'IMAGE_INFO', 'IMAGE_INFO',
     :              0,0,STATUS)

               CALL DAT_FIND(OUT_XLOC, 'IMAGE_INFO', IMAGE_LOC,
     :              STATUS)

               CALL DAT_NEW(IMAGE_LOC, 'INSTRUMENT', '_CHAR*6',
     :              0,0,STATUS)
               CALL CMP_PUT0C(IMAGE_LOC, 'INSTRUMENT', 'SURVEY',
     :              STATUS)

               CALL DAT_NEW(IMAGE_LOC, 'BAND', '_INTEGER',
     :              0,0,STATUS)
               CALL CMP_PUT0I(IMAGE_LOC, 'BAND', 1,
     :              STATUS)

               CALL DAT_NEW(IMAGE_LOC, 'TYPE', '_CHAR*8',
     :              0,0,STATUS)
               CALL CMP_PUT0C(IMAGE_LOC, 'TYPE', 'UNKNOWN',
     :              STATUS)

               CALL DAT_ANNUL(IMAGE_LOC, STATUS)

               CALL DAT_ANNUL(OUT_XLOC, STATUS)

            END IF

*     PLanet is special

            IF (OUT_COORDS  .EQ. 'PL') THEN
                  CTYPE1 = 'RA---TAN'
                  CTYPE2 = 'DEC--TAN'
            END IF

*     This is the SCUBA software so I know the instrument and telescope

            INSTRUMENT = 'SCUBA'
            TELESCOPE = 'JCMT-Hawaii'

*     and write out the same information to a .MORE.FITS section

            N_FITS = 0
            CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'OBJECT', OUT_TITLE, 'name of object', STATUS)

            DO I = 1, NFILE
               STEMP = 'FILE_'
               ITEMP = 5
               CALL CHR_PUTI (I, STEMP, ITEMP)
               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              STEMP,FILENAME(I), 'name of input datafile', STATUS)
            END DO

            IF (OUT_COORDS.NE.'GA' .AND. OUT_COORDS.NE.'NA'
     :           .AND.OUT_COORDS.NE.'AZ' .AND. OUT_COORDS .NE.'PL') THEN
               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'RADECSYS', RADECSYS, 'Frame of reference', STATUS)
            END IF

            IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ' .AND.
     :           OUT_COORDS .NE. 'PL') THEN
               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'SYSTEM', SCS, 'sky coordinate system', STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :              'LONG', OUT_LONG, 'centre longitude (radians)', 
     :              STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :              'LAT', OUT_LAT, 'centre latitude (radians)', STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :              'EPOCH', OUT_EPOCH, 'epoch of map', STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS,FITS,
     :              'EQUINOX', OUT_EPOCH, 
     :              'epoch of mean equator and equinox', STATUS)

            END IF

            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'MJD-OBS', MJD_STANDARD, 'MJD of first observation', 
     :           STATUS)

            CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'TELESCOP', TELESCOPE, 'name of telescope', STATUS)
            CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'INSTRUME', INSTRUMENT, 'name of instrument', STATUS)

*     Store SCUBA projection name

            CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :           'SCUPROJ',OUT_COORDS, 'SCUBA output coordinate system', 
     :           STATUS)

*     Put in a DATE-OBS field, converting MJD to DATE

            CALL SLA_DJCL(MJD_STANDARD, IY, IM, ID, DTEMP, ITEMP)

            ITEMP = 0
            CALL CHR_PUTI(IY, DATEOBS, ITEMP)
            CALL CHR_APPND('/',DATEOBS, ITEMP)
            CALL CHR_PUTI(IM, DATEOBS, ITEMP)
            CALL CHR_APPND('/',DATEOBS, ITEMP)
            CALL CHR_PUTI(ID, DATEOBS, ITEMP)

            CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'DATE-OBS', DATEOBS, 'Date of first observation', 
     :           STATUS)


*     Now need to calculate the FITS Axis info
*     If this is NA then NDF2FITS will do this for us

            IF (OUT_COORDS .NE. 'NA'.AND.OUT_COORDS.NE.'AZ') THEN

               OBSRA = OUT_LONG * 180.0D0 / PI
               OBSDEC= OUT_LAT  * 180.0D0 / PI
               FITS_OUT_PIXEL = OUT_PIXEL * REAL(180.0D0 / PI)

               IF (OUT_COORDS.NE.'GA') THEN
                  CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :                 'OBSRA',OBSRA,
     :                 'RA of map centre (degrees; deprecated)', STATUS)
                  CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS,N_FITS, FITS, 
     :                 'OBSDEC', OBSDEC, 
     :                 'Dec. of map centre (degrees; deprecated)',
     :                 STATUS)
               END IF

               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CTYPE1', CTYPE1,'TAN projection used', STATUS)
               CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CRPIX1', I_CENTRE, 'I of centre (ref) pixel', 
     :              STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CRVAL1', OBSRA, 'Map centre (degrees)', STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CDELT1',DBLE(-FITS_OUT_PIXEL), 
     :              'increment per pixel (degrees)', 
     :              STATUS)
               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CUNIT1', 'deg','physical units of axis 1', STATUS)


               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CTYPE2', CTYPE2,'TAN projection used', STATUS)
               CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CRPIX2', J_CENTRE, 'J of centre (ref) pixel', 
     :              STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CRVAL2', OBSDEC, 'Map centre (degrees)', STATUS)
               CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CDELT2', DBLE(FITS_OUT_PIXEL), 
     :              'increment per pixel (degrees)', 
     :              STATUS)
               CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'CUNIT2','deg','physical units of axis 2', STATUS)

            END IF

*     write out the FITS extension

            NDIM =1 
            CALL NDF_XNEW (OUT_NDF, 'FITS', '_CHAR*80', NDIM, N_FITS, 
     :           OUT_FITSX_LOC, STATUS)
            CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)
            CALL DAT_ANNUL (OUT_FITSX_LOC, STATUS)



      END
