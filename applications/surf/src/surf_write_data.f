      SUBROUTINE REDS_WRITE_DATA( FD, NPTS, IN_DATA, IN_VARIANCE,
     :     BOL_RA, BOL_DEC, STATUS)
*+
*  Name:
*     EXTRACT_DATA

*  RealName: 
*     REDS_WRITE_DATA (Use first name for help files)
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL REDS_WRITE_DATA( FD, NPTS, IN_DATA, IN_VARIANCE,
*    : BOL_RA, BOL_DEC, STATUS )
 
*  Description:
*     This routine writes the value, variance and position of each
*     data point to a ASCII file.
*     The interface is the same as that used in the REBIN task.
*     The data and variance are in volts. The positions are in radians.
*     The data are written out as columns: RA DEC DATA VAR
 
*  Arguments:
*     FD = INTEGER (Given)
*        Output file descriptor
*     NPTS = INTEGER (Given)
*        Number of points in input data
*     IN_DATA( NPTS ) = REAL (Given)
*        Data value
*     IN_VARIANCE( NPTS ) = REAL (Given)
*        Variance on data
*     BOL_RA( NPTS ) = DOUBLE (Given)
*        Bolometer position (radians offset from map centre)
*     BOL_DEC( NPTS ) = DOUBLE (Given)
*        Bolometer position (radians offset from map centre)
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  ADAM parameters:
*     FILE = FILENAME (Write)
*        The name of the ASCII file used for storing the data.
*     REF = NDF (Read)
*        The name of the first NDF to be rebinned. The name may also be the
*        name of an ASCII text file containing NDF and parameter values.
*        See the notes. REF can be a SCUBA section.
*     IN = NDF (Read)
*        The name of the input file to be rebinned. This parameter is requested
*        repeatedly until a NULL value (!) is supplied. LOOP must be TRUE.
*        IN can be a SCUBA section.
*        Like the REF parameter this parameter accepts a text file.
*     LAT_OUT = _CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the first map.
*     LONG_OUT = _CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the first map.
*     LOOP = LOGICAL (Read)
*        Task will ask for multiple input files if true. Only REF is read
*        if noloop.
*     OUT_COORDS = _CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*     SHIFT_DX = _REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line. This is a shift in the output coordinte frame.
*     SHIFT_DY = _REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line. This is a shift in the output coordinate frame.
*     USE_SECTION = _LOGICAL (Read)
*        If you wish to discard the data specified by the SCUBA Section
*        then select 'no'. If you wish to rebin a map using only
*        the specified section select 'yes'.
*     WEIGHT = _REAL (Read)
*        The relative weight that should be assigned to each dataset.

*  Notes:
*     For each file name that is entered, values for the parameters
*     SELECT_INTS, WEIGHT, SHIFT_DX and SHIFT_DY are requested.
*     - The application can read in up to 100 separate input datasets. 
*     - No data is returned if the DATA or positions are bad.
*     Data is still returned if Variance is bad.
*     - The REF and IN parameters accept ASCII text files as input. These
*     text files may contain comments (signified by a #), NDF names,
*     values for the parameters WEIGHT, SHIFT_DX, SHIFT_DY and USE_SECTION,
*     and names of other ASCII files. There is one data file per line.
*     An example entry is:
*
*         file1{b5}   1.0   0.5   0.0  YES
*         # This is file2
*         file2       0.9   0.0   0.0  # No section needed
*
*     Note that the parameters are position dependent and are not necessary.
*     Missing parameters are requested. This means it is not possible to
*     specify SHIFT_DX (position 3) without specifying the WEIGHT.
*     Also note that SCUBA sections can be specified with any input NDF.

*  Authors:
*     TIMJ: Tim Jenness (JACH)

*  History:
*     1997 April 09 (TIMJ)
*       Initial version

*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION BOL_RA(NPTS)
      DOUBLE PRECISION BOL_DEC(NPTS)
      INTEGER FD
      REAL    IN_DATA(NPTS)
      REAL    IN_VARIANCE(NPTS)
      INTEGER NPTS

*  Status
      INTEGER STATUS

*  Local Variables:
      INTEGER      I             ! Loop counter
      CHARACTER*80 LINE          ! Text buffer

*.
      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NPTS

         IF (BOL_RA(I) .NE. VAL__BADD .AND.
     :        BOL_DEC(I) .NE. VAL__BADD .AND.
     :        IN_DATA(I) .NE. VAL__BADR) THEN

            IF (IN_VARIANCE(I) .EQ. VAL__BADR) THEN
               WRITE(LINE,*) BOL_RA(I), BOL_DEC(I), IN_DATA(I),
     :              ' --BAD--'
            ELSE
               WRITE(LINE,*) BOL_RA(I), BOL_DEC(I), IN_DATA(I), 
     :              IN_VARIANCE(I)
            END IF

            CALL FIO_WRITE(FD, LINE, STATUS)
         END IF

      END DO

 10   FORMAT(E8.3, ' ', E8.3, ' ', F8.3, ' ', F8.3)

      END
