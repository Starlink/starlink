      SUBROUTINE AST2XSP( STATUS )
*+
*  Name:
*     AST2XSP

*  Purpose:
*     Converts spectra to XANADU FITS spectral format

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AST2XSP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Program converts an ASTERIX HDS spectrum file into a form
*     that can be read by XSPEC -(X-ray Spectral Analysis).
*     It outputs a pulse height analyser file (.PHA) and a
*     response file (.RMF).  It currently only handles data for the
*     Exosat ME, ROSAT XRT, and ROSAT WFC instruments.

*  Usage:
*     AST2XSP hds_file [slice] xspec_file

*  Environment Parameters:
*     INP = CHARACTER (read)
*        Name of the input HDS spectrum
*     SLICE = INTEGER (read)
*        Number of spectrum in spectral set
*     AREA = REAL (read)
*        Geometrical area of detector
*     OUT = CHARACTER (read)
*        Filename root of the output XSPEC format file

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     OGIP CAL/GEN/92-002 : The Calibration Requirements for Spectral Analysis

*  Keywords:
*     astxsp, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     PAM: Paul McGale (XRA,University of Leicester).
*     RDS: Richard Saxton (ROSAT, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*        Feb 91 (PAM):
*        Original Version.
*        Apr 91 (RDS):
*        Adamised version
*      7 Nov 91 (RMJ):
*        Allows binned up data from XRT to be handled (XMV::RMJ)
*        Jun 92 (RDS):
*        Takes a slice from a spectral_series and works on EXOSAT LE data
*        Aug 92 (RDS):
*        Fixed a bug in the slicing
*        Apr 93 (RDS,RMJ):
*        Solved a problem which occured when an energy had a
*        response in only one PH bin.
*     24 Nov 94 V1.8-0 (DJA):
*        Now use USI for user interface (DJA)
*     14 Jun 95 V1.8-1 (DJA):
*        Rewrite for XANADU FITS formats. No longer needs
*        linking against XANADU.
*      6 Aug 95 V1.8-2 (DJA):
*        Don't write AREASCAL keyword. Handle spectra with bins more than
*        one channel wide properly. Removed ignoring channels 1..7 for
*        XRT data.
*     14 Aug 95 V1.8-3 (DJA):
*        Reinstated AREASCAL and normalise response to take the factor out.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN

*  Local Constants:
      INTEGER			MAXCOL
        PARAMETER		( MAXCOL = 6 )

      INTEGER			MAXIG
        PARAMETER		( MAXIG = 2 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AST2XSP Version 1.8-3a' )

      INTEGER			XSP_OK
        PARAMETER		( XSP_OK = 0 )

      INTEGER			XSP_BAD
        PARAMETER		( XSP_BAD = 1 )

*  Local Variables:
	CHARACTER*(DAT__SZLOC)  LOCSRT, LOCTIME

      CHARACTER*40		DET			! Detector name
      CHARACTER*132  		INFILE			! Input spectrum name
      CHARACTER*40		INSTMNT			! Instrument name
      CHARACTER*132		OUTPHA			! O/p spectrum name
      CHARACTER*132		OUTRSP			! O/p response name
      CHARACTER*8		TTYPE(MAXCOL)		! Column names
      CHARACTER*8		TFORM(MAXCOL)		! Column types
      CHARACTER*20		TUNIT(MAXCOL)		! Column units

*     Relate to HDS files.
        INTEGER DIMS(4),ODIMS(4),AMIN(4),AMAX(4),ORD(4)

      REAL			EXTIME			! Exposure time
      REAL			GEOMAREA		! Geometric area

      INTEGER			DETID			! Detector configuration
      INTEGER			E_AX			! Energy axis number
      INTEGER			EAPTR			! Energy axis data
      INTEGER			ERRCOL			! Error column number
      INTEGER			I,J			! Loop variables
      INTEGER			IDPTR			! I/p data
      INTEGER			IEPTR			! I/p errors
      INTEGER			IFID			! I/p file descriptor
      INTEGER			IGSTART(MAXIG)		! Channels to ignore
      INTEGER			IGEND(MAXIG)		! Start and end points
      INTEGER			IQPTR			! I/p quality
      INTEGER			KDOT			! Character index
      INTEGER			LUN			! O/p logical unit
      INTEGER			NDIM			! I/p dimensionality
      INTEGER			NEDIM			! Not E_AX dim in 2D
      INTEGER			NFIELDS			! # o/p fields
      INTEGER			NLEV			! File trace info
      INTEGER			OLEN			! Length of OUTPHA/RSP
      INTEGER			OPHA			! Output PHA dataset
      INTEGER			ORSP			! Output RMF dataset
      INTEGER			NIGNORE			! # Regions to ignore
      INTEGER			PIXID, PRJID, SYSID	! Astrometry details
      INTEGER			QUALCOL			! Quality column number
      INTEGER			RMFID, ARFID		! I/p response objects
      INTEGER 			SLICE			! Energy slice wanted
      INTEGER			TIMID			! Timing info
      INTEGER			WPTR			! Workspace pointer

      LOGICAL			ERROK			! Errors present?
      LOGICAL			EXPCOR			! Corrected spectrum?
      LOGICAL			LVAL			! Spectral bin bad?
      LOGICAL			OK			! General validity test
      LOGICAL			QUALOK			! Quality present?
      LOGICAL			TWOD			! I/p is 2-dimensional

*     Relate to XSPEC files.
	INTEGER  INFOAR(4)
	DATA INFOAR/4*0/

*     Relate to main program.
        CHARACTER*80 PATH
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input file name
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )

*  Find name of input file
      CALL ADI_FTRACE( IFID, NLEV, PATH, INFILE, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Get rid of the file extension
      KDOT = MAX(INDEX(INFILE,'.sdf'),INDEX(INFILE,'.SDF'))
      IF ( KDOT .GT. 0 ) THEN
        CALL USI_DEF0C( 'OUT', INFILE(:KDOT-1), STATUS )
      END IF
      CALL USI_GET0C( 'OUT', OUTPHA, STATUS )
      OLEN = CHR_LEN(OUTPHA)
      OUTPHA = OUTPHA(:OLEN)//'.pha'
      OUTRSP = OUTPHA(:OLEN)//'.rmf'
      OLEN = OLEN + 4

*  Get hardware configuration, astrometry and timing data
      CALL DCI_GETID( IFID, DETID, STATUS )
      CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
      CALL TCI_GETID( IFID, TIMID, STATUS )

*  Get instrument name
      CALL ADI_CGET0C( DETID, 'Instrument', INSTMNT, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_PRNT( 'Error reading instrument name' )
        INSTMNT = 'unknown'
        CALL ADI_CPUT0C( DETID, 'Instrument', INSTMNT, STATUS )
      END IF

*  Get detector name
      CALL ADI_CGET0C( DETID, 'Detector', DET, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_PRNT( 'Error reading detector name, assumed unknown' )
        DET = 'unknown'
        CALL ADI_CPUT0C( DETID, 'Detector', INSTMNT, STATUS )
      END IF

*  Get data dimensions
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, 2, DIMS, NDIM, STATUS )
      IF (STATUS .NE. SAI__OK .OR. .NOT. OK) THEN
        CALL MSG_PRNT('Error reading data array')
        GOTO 99
      END IF

*  If a 2-d XRT file assume that the other dimension is radial bins
      IF (NDIM .EQ. 2 .AND. INDEX(INSTMNT, 'XRT') .NE. 0) THEN

*    Find which axis is the PH axis
        CALL BDI0_FNDAXC( IFID, 'E', E_AX, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set the other axis
        IF (E_AX .EQ. 1) THEN
          NEDIM = 2
        ELSE IF ( E_AX .EQ. 2 ) THEN
          NEDIM = 1
        ELSE
          CALL MSG_PRNT( '** PH axis not found **' )
          GOTO 99
        END IF

*    Ask user which slice is required
        CALL MSG_SETI( 'NSPEC', DIMS(NEDIM) )
        CALL MSG_PRNT( 'This file contains ^NSPEC spectra' )
        CALL USI_GET0I( 'SLICE', SLICE, STATUS )
        IF ( (STATUS .EQ. SAI__OK) .AND.
     :       ((SLICE.LT.1) .OR. (SLICE.GT.DIMS(NEDIM))) ) THEN
          CALL MSG_SETI( 'S', SLICE )
          CALL MSG_SETI( 'N', DIMS(NEDIM) )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Invalid slice index ^S, should be 1..^N',
     :                  STATUS )
        END IF
        IF (STATUS .NE. SAI__OK) GOTO 99

*
        DIMS(3)=1
        DIMS(4)=1

        TWOD = .TRUE.

      ELSE
        E_AX = 1
        TWOD = .FALSE.
        SLICE = 0

      END IF

* Locate response elements
      CALL ERI_GETIDS( IFID, SLICE, RMFID, ARFID, STATUS )

* Get the base and scale values of the PHA axis
C      CALL BDI_MAPR( IFID, 'E_Axis_Data', 'READ', EAPTR, STATUS )
C      IF (STATUS .NE. SAI__OK) THEN
C        CALL ERR_REP( ' ', 'Error reading spectral axis values',
C     :                STATUS )
C        GOTO 99
C      END IF

*  Check if instrument is ROSAT XRT, WFC, EXOSAT ME or LE.
      NIGNORE = 0
      GEOMAREA = -1.0
      IF ( INDEX(INSTMNT, 'XRT') .GT. 0 ) THEN

        IF ( INDEX( DET,'PSPC') .NE. 0 ) THEN
	  GEOMAREA = 1141.0

*      Standard ignores for XRT/PSPC are channels 1-7
C          NIGNORE = 1
C          IGSTART(1) = 1
C          IGEND(1) = 7

        END IF

C	   CALL DAT_FIND(LOCSRT, 'TIME', LOCTIME, STATUS)
C*
C           IF (STATUS .NE. SAI__OK) THEN
C              CALL MSG_PRNT('Error reading times from SORt box')
C              GOTO 99
C           ENDIF

*  EXOSAT ME
      ELSE IF (INDEX(INSTMNT, 'ME') .GT. 0 ) THEN
	GEOMAREA = 1676.71
        NIGNORE = 1
        IGSTART(1) = 1
        IGEND(1) = 4
C 	CALL DAT_FIND(LOCSRT, 'TIME', LOCTIME, STATUS)

*  EXOSAT LE
      ELSE IF (INDEX(INSTMNT, 'LE') .GT. 0 ) THEN
	GEOMAREA = 90.5

*  ROSAT WFC
      ELSE IF (INDEX(INSTMNT, 'WFC') .GT. 0 ) THEN
	GEOMAREA = 456.0

*  HEXE
      ELSE IF (INDEX(INSTMNT, 'HEXE') .GT. 0 ) THEN
        GEOMAREA = 574.117

*  TTM
      ELSE IF ( INDEX(INSTMNT, 'TTM') .GT. 0 ) THEN
        GEOMAREA = 200.0

      END IF

*  Unrecognised instrument so prompt for area
      IF ( GEOMAREA .LT. 0.0 ) THEN
        CALL MSG_SETC( 'INS', INSTMNT )
        CALL MSG_SETC( 'DET', DET )
        CALL MSG_PRNT( 'Cannot derive geometric area for ^INS / ^DET' )
        CALL USI_GET0R( 'AREA', GEOMAREA, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Store geometrical area in response
      CALL ADI_CPUT0R( RMFID, 'GeometricalArea', GEOMAREA, STATUS )

* Read in info from HDS file.
* Have to get locator to array cell.
C	IF (INDEX(INSTMNT, 'WFC') .GT. 0 ) THEN
C	  STRTIME = 1.0E6
C	  STPTIME = 1.1E6

*  Read in info from HDS file first.
      CALL ADI_CGET0R( TIMID, 'Exposure', EXTIME, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_PRNT( 'Unable to get exposure time from input, '/
     :                 /'assuming exposure of 1 second' )
        EXTIME = 1.0
      END IF

*  Map the data array
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )

*  Map error array if present
      CALL BDI_CHK( IFID, 'Variance', ERROK, STATUS )
      IF ( ERROK ) THEN
        CALL BDI_MAPR( IFID, 'Error', 'READ', IEPTR, STATUS )
        IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP( ' ', 'Error reading errors from input', STATUS )
          GOTO 99
        END IF
      END IF

*  Map quality array if present
      CALL BDI_CHK( IFID, 'Quality', QUALOK, STATUS )
      IF ( QUALOK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', 'Error reading quality from input',
     :                  STATUS )
          GOTO 99
        END IF
      END IF

*  Extract slice if 2-D
      IF ( TWOD ) THEN

*    Set min and max values
        DO J = 1, 4
          ORD(J) = J
          IF ( J .EQ. NEDIM ) THEN
            AMIN(J) = SLICE
            AMAX(J) = SLICE
            ODIMS(J)=1
          ELSE
            AMIN(J) = 1
            AMAX(J) = DIMS(J)
            ODIMS(J) = DIMS(J)
          END IF
        END DO
        ODIMS(E_AX) = DIMS(E_AX)

*    Copy the slice that you want into the output array
        CALL DYN_MAPR( 1, DIMS(E_AX), WPTR, STATUS )
        CALL DTA_COPYSLICER( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                 %VAL(IDPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     :                 ODIMS(3), ODIMS(4), %VAL(WPTR) )
        IDPTR = WPTR
        CALL DYN_MAPR( 1, DIMS(E_AX), WPTR, STATUS )
        CALL DTA_COPYSLICER( DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                 %VAL(IEPTR), AMIN, AMAX, ORD, ODIMS(1), ODIMS(2),
     :                 ODIMS(3), ODIMS(4), %VAL(WPTR) )
        IEPTR = WPTR

      END IF

*  Open new PHA file. Has a null primary header, into which we shove
*  most of the ancillary keywords
      CALL ADI_FCREAT( OUTPHA(:OLEN)//'%fits', ADI__NULLID, OPHA,
     :                 STATUS )

*    Write loads'a'keywords
      CALL ADI2_PKEY0C( OPHA, ' ', 'CONTENT', 'SPECTRUM',
     :             'Spectrum file contains', STATUS )
      CALL ADI2_PKEY0C( OPHA, ' ', 'CREATOR', VERSION,
     :             'Creator of this file', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'EXPOSURE', EXTIME,
     :             'Exposure time', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'AREASCAL', GEOMAREA,
     :             'Area scaling factor', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'BACKSCAL', 1.0,
     :             'Background scaling factor', STATUS )
      CALL ADI2_PKEY0R( OPHA, 'SPECTRUM', 'CORRSCAL', 1.0,
     :             'Corrections scaling factor', STATUS )

*  Number of raw detector channels
      CALL ADI2_PKEY0I( OPHA, 'SPECTRUM', 'DETCHANS', DIMS(E_AX),
     :             'Total number of raw detector channels', STATUS )

*  Is the spectrum corrected?
      CALL PRF_GET( IFID, 'CORRECTED.EXPOSURE', EXPCOR, STATUS )

*  Create table names, types and units
      TTYPE(1) = 'CHANNEL'
      TFORM(1) = 'I'
      CALL BDI_AXGET0C( IFID, E_AX, 'Units', TUNIT(1), STATUS )
      IF ( EXPCOR ) THEN
        TTYPE(2) = 'RATE'
        TFORM(2) = 'E'
      ELSE
        TTYPE(2) = 'COUNTS'
        TFORM(2) = 'I'
      END IF
      CALL BDI_GET0C( IFID, 'Units', TUNIT(2), STATUS )
      NFIELDS = 2
      IF ( ERROK ) THEN
        NFIELDS = NFIELDS + 1
        ERRCOL = NFIELDS
        TTYPE(NFIELDS) = 'STAT_ERR'
        TFORM(NFIELDS) = 'E'
        TUNIT(NFIELDS) = TUNIT(2)
      END IF
      IF ( QUALOK .OR. (NIGNORE.GT.0) ) THEN
        NFIELDS = NFIELDS + 1
        QUALCOL = NFIELDS
        TTYPE(NFIELDS) = 'QUALITY'
        TFORM(NFIELDS) = 'I'
        TUNIT(NFIELDS) = ' '
      END IF

*  Create extension for spectrum
      CALL ADI2_POGIPK( OPHA, 'SPECTRUM', 'SPECTRUM',
     :             ' ', ' ', ' ', ' ', ' ',
     :               STATUS )
      CALL ADI2_PKEY0I( OPHA, 'SPECTRUM', 'TLMIN1', 1,
     :              'Lowest legal channel number', STATUS )
      CALL ADI2_PKEY0I( OPHA, 'SPECTRUM', 'TLMAX1', DIMS(E_AX),
     :              'Highest legal channel number', STATUS )
      CALL ADI2_PKEY0C( OPHA, 'SPECTRUM', 'RESPFILE', OUTRSP(:OLEN),
     :             'Redistribution matrix file (RMF)', STATUS )
      CALL ADI2_PKEY0C( OPHA, 'SPECTRUM', 'PHAVERSN', '1992a',
     :             'OGIP classification of FITS format style', STATUS )

*  Write astrometry, detector info and timing
      CALL WCI_PUTIDS( OPHA, PIXID, PRJID, SYSID, STATUS )
      CALL DCI_PUTID( OPHA, DETID, STATUS )
      CALL TCI_PUTID( OPHA, TIMID, STATUS )
      CALL ADI2_DEFBTB( OPHA, 'SPECTRUM', DIMS(E_AX), NFIELDS, TTYPE,
     :                  TFORM, TUNIT, 0, STATUS )

*  Write the spectrum
      CALL ADI2_GETLUN( OPHA, LUN, STATUS )
      CALL DYN_MAPR( 1, DIMS(E_AX), EAPTR, STATUS )
      CALL ARR_REG1R( 1.0, 1.0, DIMS(E_AX), %VAL(EAPTR), STATUS )
      CALL FTPCLE( LUN, 1, 1, 1, DIMS(E_AX), %VAL(EAPTR), STATUS )
      CALL FTPCLE( LUN, 2, 1, 1, DIMS(E_AX), %VAL(IDPTR), STATUS )
      IF ( ERROK ) THEN
        CALL FTPCLE( LUN, ERRCOL, 1, 1, DIMS(E_AX), %VAL(IEPTR),
     :                                                  STATUS )
      END IF

*  Write quality
      IF ( QUALOK ) THEN
        DO I = 1, DIMS(E_AX)
          CALL ARR_ELEM1L( IQPTR, DIMS(E_AX), I, LVAL, STATUS )
          IF ( LVAL ) THEN
            CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_OK, STATUS )
          ELSE
            CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_BAD, STATUS )
          END IF
        END DO
      ELSE IF ( NIGNORE .GT. 0 ) THEN
        DO I = 1, DIMS(E_AX)
          CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_OK, STATUS )
        END DO
      END IF
      IF ( NIGNORE .GT. 0 ) THEN
        DO I = 1, NIGNORE
          DO J = IGSTART(I), IGEND(I)
            CALL FTPCLJ( LUN, QUALCOL, J, 1, 1, XSP_BAD, STATUS )
          END DO
        END DO
      END IF

*  Close spectrum file
      CALL ADI_FCLOSE( OPHA, STATUS )

*  Open the response file
      CALL ADI_FCREAT( OUTRSP(:OLEN)//'%fits', ADI__NULLID, ORSP,
     :                 STATUS )

      CALL ADI2_PKEY0C( ORSP, ' ', 'CREATOR', VERSION, '*', STATUS )
      CALL ADI2_PKEY0C( ORSP, ' ', 'CONTENT', 'MATRIX',
     :             'File contains response matrix', STATUS )

      CALL ERI_PUTIDS( ORSP, RMFID, ARFID, STATUS )
      CALL DCI_PUTID( ORSP, DETID, STATUS )

*  Close the response file
      CALL ADI_FCLOSE( ORSP, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
