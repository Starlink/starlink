      SUBROUTINE SURF_SCUBA2MEM ( STATUS )
*+
*  Name:
*     SCUBA2MEM

*  Purpose:
*     Calculate bolometer positions as tangent plane offsets

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_SCUBA2MEM ( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This routine reads in SCUBA demodulated data and writes it
*     out along with the positions of the bolometers on the sky
*     for each sample. The positions of the
*     chop beams can be requested as well as the positions of the tracking
*     centre. Returns tangent plane offsets from the map centre in arcseconds.
*     Additionally, the LST of each sample is stored as axis information.

*  Usage:
*     scuba2mem in out

*  ADAM Parameters:
*     IN = CHAR (Read)
*        The name ofthe input files to be processed. This is a demodulated
*        data file. RESTORE should not have been run on it. Multiple
*        file names can be specified (see the documentation on GRP).
*        All the input files are referenced to the same output coordinate
*        frame.
*     LAT = CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the observation in the output
*        coordinates.
*     LONG = CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the observation in the output
*        coordinates.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     NBEAMS = INTEGER (Read)
*         Number of output beams to be written to file. NBEAMS=1 just
*         writes the Middle beam, NBEAMS=2 writes the Left (negative)
*         and Right beams, NBEAMS=3 writes Middle, Left and Right beams.
*     OUT = NDF (Write)
*         This parameter specifies the name of the output file to be used
*         to store the positional information.
*         The file format is described below.
*     OUT_COORDS = CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*     SHIFT = REAL( 2 ) (Read)
*        The pointing shift [X,Y] to be applied that would bring the
*        map into the correct position. 
*        This is a shift in the output coordinate frame. CHANGE_POINTING
*        should be used to add Az/El pointing offsets.

*  Examples:
*     scuba2mem out_coords=GA o34_lon_ext o34_mem nbeams=1 \\
*        Calculate the coordinates of all bolometer positions
*        in tangent plane offsets from the GA map centre.
*     scuba2mem o34_lon_ext nbeams=3 \\
*        Calculate all chop positions for o34_lon_ext. Use RJ coordinates.

*  Notes:
*     - Can be used on JIGGLE and SCAN data.
*     - The coordinates of the selected output frame are written
*       to the output FITS extension in keywords OUT_CRDS, OUTLONG and 
*       OUTLAT. The full FITS header of the observation itself is still
*       available.

*  Format of output file:
*     SCUBA DBMEM requires the data, positions of every beam and the
*     LST for every point. This information (along with a standard FITS
*     header) is stored as a standard NDF. The data array is constructed
*     as follows:
*         3 dimensional: N_BOL * N_POS * ((N_BEAM * 2) + 1)
*         where N_BOL is the number of bolometers
*               N_POS is the number of samples for each bolometer (time axis)
*               N_BEAM is the number of beams
*
*         The 3rd dimension contains the actual data value plus positions
*         of every beam associated with the data point. Each beam has two
*         positions (X offset and Y offset)
*         Axis components store bolometer number, LST and beam weight.

*  Related Applications:
*     SURF: EXTRACT_DATA, REBIN
*     DBMEM

*  Authors:
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)
 

*  Copyright:
*     Copyright (C) 1995-2002 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.15  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.14  2003/04/02 02:58:31  timj
*     Protect bad pixels when changing type with VEC
*
*     Revision 1.13  2002/09/14 03:58:13  timj
*     Update copyright
*
*     Revision 1.12  2002/09/09 21:34:13  timj
*     Correctly initialize LST_PTR to 0
*
*     Revision 1.11  1999/08/03 20:01:41  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.10  1999/07/14 20:39:10  timj
*     Add documentation for NBEAMS for SCUBA2MEM
*
*     Revision 1.9  1999/07/14 20:13:31  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.8  1999/07/14 19:22:11  timj
*     Write output coordinate information to FITS header.
*     Tidy up documentation header.
*
*     Revision 1.7  1999/07/14 04:52:36  timj
*     Allow shift of coordinate output frame.
*     Support GRP multiple input files.
*
*     Revision 1.6  1999/07/05 02:20:08  timj
*     Fix args to surf_read_rebin_ndf
*
*     Revision 1.5  1999/05/15 01:48:42  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.4  1998/05/06 18:29:58  timj
*     Increase size of 'IN' parameter.
*
*     Revision 1.3  1998/04/28 20:50:20  timj
*     Support the return of FITS information from SURF_READ_REBIN_NDF
*
*     Revision 1.2  1998/03/18 23:32:54  timj
*     Add BOLWT
*
*     Revision 1.1  1998/01/27 02:01:33  timj
*     Initial revision
*
*     {note_history_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
* Type Definitions:
      IMPLICIT NONE

* Global constants:
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'                ! for VAL__ constants
      INCLUDE 'SAE_PAR'
      INCLUDE 'SURF_PAR'               ! SURF definitions
      INCLUDE 'MSG_PAR'
      INCLUDE 'GRP_PAR'
      INCLUDE 'CNF_PAR'

* Status:
      INTEGER STATUS

* External references:

* Local Constants:
      INTEGER     MAX_FILE      ! max number of input files
      PARAMETER (MAX_FILE = 1)
      INTEGER     NDIM          ! Number of dimensions in output file
      PARAMETER (NDIM = 3)
      CHARACTER * (9) TSKNAME   ! Name of task
      PARAMETER (TSKNAME = 'SCUBA2MEM')

* Local variables:
      INTEGER ADDED             ! Number added to group this time
      REAL             ANG_INT(MAX_FILE,SCUBA__MAX_INT, 2) 
                                       ! Angles (wplate,angrot) for each
                                       ! integration (polarimetry) 
      REAL             ANG_MEAS(MAX_FILE,SCUBA__MAX_MEAS, 2)
                                       ! Angles (wplate,angrot) for each
                                       ! measurement (polarimetry) 
      REAL    BOLWT (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! Bolometer weights
      INTEGER BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                        ! in input file
      INTEGER BOL_DEC_END               ! pointer to end of BOL_DEC_PTR scratch
                                        ! space
      INTEGER BOL_DEC_PTR               ! pointer to scratch space holding
                                        ! apparent Dec / y offset positions of
                                      ! measured points in input file (radians)
      INTEGER BOL_RA_END                ! pointer to end of BOL_RA_PTR scratch
                                        ! space
      INTEGER BOL_RA_PTR                ! pointer to scratch space holding
                                        ! apparent RA / x offset positions of
                                      ! measured points in input file (radians)
      INTEGER DATA_END                  ! Pointer to end of input data
      INTEGER DATA_PTR                  ! Pointer to input data
      CHARACTER * 128 DATA_SPEC(SCUBA__MAX_SECT) ! Array of section specs
      CHARACTER * 80  FITS ( SCUBA__MAX_FITS ) ! FITS information
      LOGICAL FLAG                      ! GRP flag
      CHARACTER * 132 FNAME             ! Input filename
      INTEGER GRP                       ! Group counter
      LOGICAL HOURS                     ! .TRUE. if the angle being read in is
                                        ! in units of hours rather than degrees
      INTEGER I                         ! Loop counter
      INTEGER IERR                      ! Position of error in VEC copy
      INTEGER IGRP                      ! Group identifier
      CHARACTER * 256 INSTRING          ! Input filename string + section
      INTEGER INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1) ! Integration pointers
      DOUBLE PRECISION IN_DEC_CEN       ! Declination of map centre
      DOUBLE PRECISION IN_RA_CEN        ! RA of map centre
      INTEGER IN_NDF                    ! NDF identified of input file
      DOUBLE PRECISION IN_UT1           ! MJD of start of observation
      INTEGER ITEMP                     ! scratch integer
      DOUBLE PRECISION LAT_OBS          ! Latitude of observatory
      INTEGER LBND(NDIM)                ! Lower bounds of output array
      INTEGER LST_PTR(2)                ! Array of pointers to LST (start/end)
      INTEGER          MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS + 1)
                                       ! pointers to start of each measurement
      INTEGER NERR                      ! Number of errors in VEC_ copy
      INTEGER NMEMBERS                  ! Number of members in group
      INTEGER NSPEC                     ! Number of SCUBA sections
      INTEGER N_BEAM                    ! Number of beams requested
      INTEGER N_BOL                     ! Number of bolometers in input file
      INTEGER N_FILE                    ! File number (always 1)
      INTEGER N_FITS                    ! Number of fits entries in input
      INTEGER N_INTS                    ! Number of integrations in file
      INTEGER N_MEAS                    ! Number of measurements in file
      INTEGER N_POS                     ! Number of samples (time axis)
      INTEGER N_PTS                     ! Number of points per beam
      CHARACTER * 40 OBJECT             ! name of object
      CHARACTER * 132 OUTFILE           ! Suggested output filename
      INTEGER OUTNDF                    ! Identifier to output NDF
      CHARACTER * 5  OUT_COORDS         ! Output coordinate system
      INTEGER OUT_A_PTR                 ! Pointer to axis
      INTEGER OUT_DATA_PTR              ! Pointer
      DOUBLE PRECISION OUT_DEC_CEN      ! Declination of output map centre
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC
                                        ! locator of FITS extension in output
                                        ! file
      DOUBLE PRECISION OUT_LAT          ! longitude of output map centre 
                                        ! (radians)
      DOUBLE PRECISION OUT_LONG         ! longitude of output map centre
                                        ! (radians)
      DOUBLE PRECISION OUT_RA_CEN       ! RA of output map centre
      DOUBLE PRECISION OUT_ROTATION     ! angle between apparent N and N of
                                        ! output coord system (radians)
      CHARACTER * 15 SUB_INSTRUMENT     ! Sub instrument name
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      INTEGER UBND(NDIM)                ! Upper bounds of output array
      LOGICAL USE_SECTION               ! Are we using the section or inverse
      CHARACTER * 15 UTDATE             ! date of observation
      CHARACTER * 15 UTSTART            ! UT of start of observation
      BYTE    QBITS                     ! BADBITS mask of input file
      LOGICAL QMF                       ! .false. = return quality array
                                        ! .true.= return masked data
                                        ! This is used in NDF_SQMF.
      INTEGER QUALITY_END               ! Pointer to end of input quality
      INTEGER QUALITY_PTR               ! Pointer to input quality
      INTEGER SECNDF                    ! Identifier to propogating section
      REAL    SHIFT(2)                  ! Tangent plane shift (X, Y)
      LOGICAL USELST                    ! TRUE if we want the LST information
      INTEGER VARIANCE_END              ! Pointer to end of input variance
      INTEGER VARIANCE_PTR              ! Pointer to input variance
      REAL    WAVELENGTH                ! Wavelength of data (microns)

* Local data:
 
      DATA SUFFIX_STRINGS /'!_mem','m','_mem'/

*.
 
      IF (STATUS .NE. SAI__OK) RETURN

*     Init pointers
      LST_PTR(1) = 0
      LST_PTR(2) = 0
 



*     start up the NDF system
      CALL NDF_BEGIN

*     Create a new input and output groups
      CALL GRP_NEW('Input files', IGRP, STATUS )

*     Turn off curly brackets so that SCUBA sections work okay
      CALL GRP_SETCC( IGRP, 'OPEN_KERNEL,CLOSE_KERNEL', '%%', STATUS )

*     Read in the group members 
      CALL GRP_GROUP('IN', GRP__NOID, IGRP, NMEMBERS, ADDED,
     :     FLAG, STATUS)

*     Read the coordinate frame of the output data
      CALL PAR_CHOIC('OUT_COORDS','RJ','AZ,NA,RB,RJ,GA,RD,PL',.TRUE.,
     :     OUT_COORDS, STATUS)

      HOURS = .TRUE.
      IF (OUT_COORDS .EQ. 'RB') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK4 B1950.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK5 J2000.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'GA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :     '^PKG: output coordinates are galactic', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'RD') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are apparent RA,Dec '//
     :        '(no date as yet)', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are nasmyth', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are Az/El offsets', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'PL') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are offsets from moving centre', 
     :        STATUS)
         HOURS = .FALSE.
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: invalid output '//
     :        'coordinate system', STATUS)
         END IF
      END IF

*     Read in the input data
*     We will do automatic quality masking 
      QMF = .TRUE.
      
*     Set the file counter to 1 since we are not trying to convert
*     all input files to a standard MJD. We are trying to convert
*     each frame to the supplied RA/Dec grid
      N_FILE = 1

*     Specify the required number of beams
*     This will be adjusted by the SURF_READ_REBIN_NDF subroutine
*     if we have selected an input data set that is incompatible
*     with the beam selection:
*        N_BEAM = 1  will return the middle-beam (standard position)
*        N_BEAM = 2 will return L and R beams for RASTER and JIGGLE
*        N_BEAM = 3 will return L, M and R beams (as M,L,R) for JIGGLE
*                   data that has been reduce_switched and for SCAN
*                   data. N_BEAM is set to 2 if a single switch of a
*                   JIGGLE map is requested.
*      Note that 3 position chopping with 2 switches is not supported.

*     Get the required number of beams
      CALL PAR_GET0I('NBEAMS', N_BEAM, STATUS)

*     Loop over all members of the group

      DO GRP = 1, NMEMBERS

         IF (STATUS .NE. SAI__OK) GO TO 10

*     Read in the next member
         CALL GRP_GET(IGRP, GRP, 1, INSTRING, STATUS)

*     Parse the filename string - GRP must be configured not to use {}
         CALL SCULIB_SPLIT_FILE_SPEC(INSTRING, SCUBA__MAX_SECT, FNAME, 
     :        NSPEC, DATA_SPEC, USE_SECTION, STATUS)

*     If we are processing multiple input files, list each one
*     as we go.
         IF (NMEMBERS .GT. 1) THEN
            CALL MSG_SETC('FILE', FNAME)
            CALL MSG_OUTIF(MSG__NORM, ' ','Processing file ^FILE',
     :           STATUS)
         END IF

*     Open the input file
         CALL NDF_FIND (DAT__ROOT, FNAME, IN_NDF, STATUS) 

*     We want the LST array
         USELST = .TRUE.

*     Calculate the beam positions and read in the data.

         CALL SURF_READ_REBIN_NDF( IN_NDF, MAX_FILE, 
     :        NSPEC, DATA_SPEC, OUT_COORDS, N_FILE, USE_SECTION,
     :        N_BOL, N_POS, N_INTS, N_MEAS, N_BEAM,
     :        IN_UT1, IN_UT1, IN_RA_CEN, 
     :        IN_DEC_CEN, FITS, N_FITS, WAVELENGTH, SUB_INSTRUMENT, 
     :        OBJECT, UTDATE, UTSTART, 
     :        BOL_ADC, BOL_CHAN, BOL_RA_PTR,
     :        BOL_RA_END, BOL_DEC_PTR,
     :        BOL_DEC_END, DATA_PTR, 
     :        DATA_END, VARIANCE_PTR,
     :        VARIANCE_END, QMF, QUALITY_PTR,
     :        QUALITY_END, QBITS, USELST, LST_PTR,
     :        ANG_INT, ANG_MEAS, INT_LIST, MEAS_LIST, BOLWT, STATUS)

*     Number of points per beam
         N_PTS = N_POS * N_BOL

*     Cancel the SHIFT parameter if this is not the first time around
         IF (GRP .GT. 1) CALL PAR_CANCL('SHIFT', STATUS)

*     Get the tangent plane shifts (in an array)
         SHIFT(1) = 0.0
         SHIFT(2) = 0.0

         CALL PAR_DEF1R('SHIFT', 2, SHIFT, STATUS)
         CALL PAR_GET1R('SHIFT', 2, SHIFT, ITEMP, STATUS)

*     Convert to radians
         SHIFT(1) = SHIFT(1) / REAL(R2AS)
         SHIFT(2) = SHIFT(2) / REAL(R2AS)

*     The NA, AZ and PL frames already have offsets so we dont need
*     to do anything with them

         IF ((OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ' 
     :        .AND. OUT_COORDS.NE.'PL')) THEN

*     Inform the 'RD' regridder the date of regrid

            IF (OUT_COORDS .EQ. 'RD') THEN
               CALL MSG_SETC ('UTDATE', UTDATE)
               CALL MSG_SETC ('UTSTART', UTSTART)
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUTIF(MSG__NORM, ' ', 
     :              '^PKG: Using output coordinates of '//
     :              'apparent RA,Dec at ^UTSTART on ^UTDATE', STATUS)
            END IF

*     Read the latitude of the observatory
            CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :           'LAT-OBS', LAT_OBS, STATUS)
            LAT_OBS = LAT_OBS * PI / 180.0D0

*     Request the new apparent ra/dec centre
            CALL SURF_REQUEST_OUTPUT_COORDS( TSKNAME, 'LONG', 'LAT',
     :           OUT_COORDS, LAT_OBS, IN_RA_CEN, IN_DEC_CEN, IN_UT1, 
     :           HOURS, OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, OUT_LONG,
     :           OUT_LAT, STATUS)

*     Convert everything to tangent plane offsets from the selected
*     map centre

            CALL SCULIB_APPARENT_2_TP (N_PTS * N_BEAM, 
     :           %VAL(CNF_PVAL(BOL_RA_PTR)), 
     :           %VAL(CNF_PVAL(BOL_DEC_PTR)),
     :           OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION,
     :           DBLE(SHIFT(1)), DBLE(SHIFT(2)), STATUS)
            
         ELSE

*     NA, AZ or PL

            OUT_RA_CEN = 0.0
            OUT_DEC_CEN = 0.0

*     Add on the tangent plane shift

            IF (STATUS .EQ. SAI__OK) THEN

               CALL SCULIB_ADDCAD(N_PTS * N_BEAM,
     :              %VAL(CNF_PVAL(BOL_RA_PTR)), DBLE(SHIFT(1)),
     :              %VAL(CNF_PVAL(BOL_RA_PTR)))
               CALL SCULIB_ADDCAD(N_PTS * N_BEAM,
     :              %VAL(CNF_PVAL(BOL_DEC_PTR)), DBLE(SHIFT(2)),
     :              %VAL(CNF_PVAL(BOL_DEC_PTR)))
            
            END IF

         END IF

*     Convert everything to arcsec

         IF (STATUS .EQ. SAI__OK) THEN

            CALL SCULIB_MULCAD(N_PTS * N_BEAM,
     :           %VAL(CNF_PVAL(BOL_RA_PTR)), R2AS,
     :           %VAL(CNF_PVAL(BOL_RA_PTR)), STATUS)
            CALL SCULIB_MULCAD(N_PTS * N_BEAM,
     :           %VAL(CNF_PVAL(BOL_DEC_PTR)), R2AS,
     :           %VAL(CNF_PVAL(BOL_DEC_PTR)), STATUS)

         END IF

*     Construct the output name
         CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :        SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     Create the output file
*     Would like to propogate the FITS header to the output 
*     plus the other extensions for information.
*     Will update the FITS header later.
*     This means that I may as well get a section from the input
*     file and propogate it to the output (without the Variance and
*     Quality arrays).

*     Get a section of the input file but make it the correct size
*     for the output file (so that we do this instead of using NDF_SBND)

         LBND(1) = 1
         LBND(2) = 1
         LBND(3) = 1
         UBND(1) = N_BOL
         UBND(2) = N_POS
         UBND(3) = (2 * N_BEAM) + 1

         CALL NDF_SECT(IN_NDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     Cancel the OUT parameter if this is not the first time around
         IF (GRP .GT. 1) CALL PAR_CANCL('OUT', STATUS)

*     Set the default output filename
         CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*     Propogate to the output NDF
         CALL NDF_PROP (SECNDF, 'Units', 'OUT', OUTNDF, STATUS)

*     Close the input file
         CALL NDF_ANNUL(SECNDF, STATUS)
         CALL NDF_ANNUL(IN_NDF, STATUS)

*     Map the output array (DOUBLE for intial test)
         CALL NDF_MAP(OUTNDF, 'DATA', '_REAL', 'WRITE/BAD',
     :        OUT_DATA_PTR, ITEMP, STATUS)

*     Now copy some data in.
*     First the actual data values

         CALL VEC_RTOR(.FALSE., N_PTS, %VAL(CNF_PVAL(DATA_PTR)),
     :        %VAL(CNF_PVAL(OUT_DATA_PTR)), IERR, NERR, STATUS)


*     Go through a beam at a time

         DO I = 1, N_BEAM

*     Now the X positions
            CALL VEC_DTOR(.TRUE., N_PTS, 
     :   %VAL(CNF_PVAL(BOL_RA_PTR) + VAL__NBD * N_PTS * (I - 1)),
     :   %VAL(CNF_PVAL(OUT_DATA_PTR) + VAL__NBR * (2 * I - 1) * N_PTS),
     :           IERR, NERR, STATUS)

*     Now Y
            CALL VEC_DTOR(.TRUE., N_PTS, 
     :   %VAL(CNF_PVAL(BOL_DEC_PTR) + VAL__NBD * N_PTS * (I - 1)),
     :   %VAL(CNF_PVAL(OUT_DATA_PTR) + VAL__NBR * (2 * I) * N_PTS),
     :           IERR, NERR, STATUS)

         END DO

*     Unmap the data array
         CALL NDF_UNMAP( OUTNDF, '*', STATUS )

*     Add some keywords to the FITS header to specify the
*     output coordinate system

         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'OUT_CRDS', OUT_COORDS, 
     :        'coordinate system of tangent plane offsets', STATUS)

*       No long and lat for NA/AZ/PL frames
         IF ( OUT_COORDS.NE.'NA' .AND.OUT_COORDS.NE.'AZ'
     :      .AND. OUT_COORDS .NE.'PL') THEN

            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'OUTLONG', OUT_LONG, 
     :           'centre longitude of output offsets (radians)', 
     :           STATUS)
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'OUTLAT', OUT_LAT, 
     :           'centre latitude of output offsets (radians)', 
     :           STATUS)

         END IF

*     write out the FITS extension (deleting the old one first)

      CALL NDF_XDEL(OUTNDF, 'FITS', STATUS)
      CALL NDF_XNEW(OUTNDF, 'FITS', '_CHAR*80', 1, N_FITS, 
     :     OUT_FITSX_LOC, STATUS)
      CALL DAT_PUT1C(OUT_FITSX_LOC, N_FITS, FITS, STATUS)
      CALL DAT_ANNUL(OUT_FITSX_LOC, STATUS)

*     Set up the axes
*     Axis 1: Bolometer number
*     Axis 2: LST
*     Axis 3: Beam
      
         CALL NDF_ACRE(OUTNDF, STATUS)

*     Deal with BOLOMETER axis
 
         CALL NDF_ASTYP('_INTEGER', OUTNDF, 'CENTRE', 1, STATUS)

         CALL NDF_AMAP(OUTNDF, 'CENTRE', 1, '_INTEGER', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLI (N_BOL, %VAL(CNF_PVAL(OUT_A_PTR)))
         END IF
         CALL NDF_ACPUT ('Bolometer', OUTNDF, 'LABEL', 1, STATUS)
         CALL NDF_AUNMP (OUTNDF, 'CENTRE', 1, STATUS)

*     Deal with LST axis

         CALL NDF_ASTYP('_DOUBLE', OUTNDF, 'CENTRE', 2, STATUS)

         CALL NDF_AMAP(OUTNDF, 'CENTRE', 2, '_DOUBLE', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

         CALL VEC_DTOD(.TRUE., N_POS, 
     :        %VAL(CNF_PVAL(LST_PTR(1))), %VAL(CNF_PVAL(OUT_A_PTR)),
     :        IERR, NERR, STATUS)

         CALL NDF_AUNMP (OUTNDF, 'CENTRE', 2, STATUS)

         CALL NDF_ACPUT('LST', OUTNDF, 'LABEL', 2, STATUS)
         CALL NDF_ACPUT('radians', OUTNDF, 'UNITS', 2, STATUS)

*     Unmap the data and close down
         CALL SCULIB_FREE ('IN_DATA', DATA_PTR,
     :        DATA_END, STATUS)
         CALL SCULIB_FREE ('IN_VARIANCE', VARIANCE_PTR,
     :        VARIANCE_END, STATUS)
         CALL SCULIB_FREE ('BOL_RA', BOL_RA_PTR,
     :        BOL_RA_END, STATUS)
         CALL SCULIB_FREE ('BOL_DEC', BOL_DEC_PTR,
     :        BOL_DEC_END, STATUS)

         CALL SCULIB_FREE('LST', LST_PTR(1), LST_PTR(2), STATUS)

*     Close the output file
         CALL NDF_ANNUL(OUTNDF, STATUS)

*     End the loop over groups
      END DO

*     Panic exit from loop
 10   CONTINUE

*     Close the GRoup
      CALL GRP_DELET(IGRP, STATUS)

*     Shut down NDF
      CALL NDF_END(STATUS)

      END
