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
*     RB: Richard Beard (ROSAT, University of Birmingham)
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
*     18 Dec 97 V rb test
*        Munge into new ADI mode
*      8 Apr 97 V2.1-0 (RB):
*        Can't make it work with ADI, use 100% pure FITSIO for a better bake
*     16 Feb 97 V2.2-1 (RB):
*        Add proper EXOSAT areas. Sort things out properly.
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
      INTEGER			STATUS, FSTAT          	! Global status

*  External References:
      INTEGER			CHR_LEN
        EXTERNAL		  CHR_LEN
      LOGICAL			STR_SUB
        EXTERNAL		  STR_SUB
      LOGICAL			STR_ABBREV
        EXTERNAL		  STR_ABBREV

*  Local Constants:
      INTEGER			MAXCOL
        PARAMETER		( MAXCOL = 6 )
      INTEGER			MAXIG
        PARAMETER		( MAXIG = 2 )
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AST2XSP Version 2.2-1' )
      INTEGER			XSP_OK
        PARAMETER		( XSP_OK = 0 )
      INTEGER			XSP_BAD
        PARAMETER		( XSP_BAD = 1 )

*  Local Variables:
      CHARACTER*40		TELESCOP		! Telescope name
      CHARACTER*40		INSTMNT			! Instrument name
      CHARACTER*40		DET			! Detector name
      CHARACTER*132  		INFILE			! Input spectrum name
      CHARACTER*132		OUTPHA			! O/p spectrum name
      CHARACTER*132		OUTRSP			! O/p response name
      CHARACTER*132		OUTANC			! O/p ancillary name
      CHARACTER*40		ANCTYP			! Type of ancillary file required
      CHARACTER*8		TTYPE(MAXCOL)		! Column names
      CHARACTER*8		TFORM(MAXCOL)		! Column types
      CHARACTER*20		TUNIT(MAXCOL)		! Column units
      CHARACTER*1		MODE			! Ancillary file mode (N,A or R)
      CHARACTER*8		MASK			! Detector settings for EXOSAT
      CHARACTER*(DAT__SZLOC)	ASTLOC			! Locator for MORE.ASTERIX
      CHARACTER*(DAT__SZLOC)	INSLOC			! Locator for MORE.ASTERIX.INSTRUMENT
      CHARACTER*(DAT__SZLOC)	MSKLOC			! Locator for MORE.ASTERIX.INSTRUMENT.ID_MASKS
      CHARACTER*(DAT__SZLOC)	LOCMSK			! Locator for MORE.ASTERIX.INSTRUMENT.ID_MASKS[1]

      COMMON /F_NAMES/		OUTPHA, OUTRSP, OUTANC	! Output filename
      COMMON /MISSION/		TELESCOP, INSTMNT, DET	! Mission strings

*     Relate to HDS files.
      INTEGER DIMS(4),ODIMS(4),AMIN(4),AMAX(4),ORD(4)

      REAL			EXTIME			! Exposure time
      REAL			GEOMAREA		! Geometric area
      REAL			EXO_AREA(8)		! Areas of the 8 EXOSAT detectors

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
      INTEGER			LUN, LUN2(2)		! O/p logical unit
      INTEGER			NDIM			! I/p dimensionality
      INTEGER			NEDIM			! Not E_AX dim in 2D
      INTEGER			NFIELDS			! # o/p fields
      INTEGER			NLEV			! File trace info
      INTEGER			OLEN			! Length of OUTPHA/RSP
      INTEGER			NIGNORE			! # Regions to ignore
      INTEGER			PIXID, PRJID, SYSID	! Astrometry details
      INTEGER			QUALCOL			! Quality column number
      INTEGER			GRPGCOL			! Grouping column number
      INTEGER			RMFID, ARFID		! I/p response objects
      INTEGER 			SLICE			! Energy slice wanted
      INTEGER			TIMID			! Timing info
      INTEGER			WPTR			! Workspace pointer
      INTEGER			BWIDTH			! Table width in bytes
      INTEGER			BLKSIZ

      LOGICAL			ERROK			! Errors present?
      LOGICAL			EXPCOR			! Corrected spectrum?
      LOGICAL			BGNDSB			! Background subtracted spectrum?
      LOGICAL			LVAL			! Spectral bin bad?
      LOGICAL			OK			! General validity test
      LOGICAL			QUALOK			! Quality present?
      LOGICAL			TWOD			! I/p is 2-dimensional
      LOGICAL			THERE			! Does a component exist

*  local data:
      DATA EXO_AREA		/212.47, 196.85, 206.32, 209.06,
     :				 202.91, 229.14, 206.96, 213.00/
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
      OUTANC = OUTPHA(:OLEN)//'.arf'
      OLEN = OLEN + 4

*  Get hardware configuration, astrometry and timing data
      CALL DCI_GETID( IFID, DETID, STATUS )
      CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
      CALL TCI_GETID( IFID, TIMID, STATUS )

*  Get misssion name
      CALL ADI_CGET0C( DETID, 'Mission', TELESCOP, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_ANNUL( STATUS )
        TELESCOP = 'unknown'
      END IF

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
c     CALL BDI_MAPR( IFID, 'E_Axis_Data', 'READ', EAPTR, STATUS )
c     IF (STATUS .NE. SAI__OK) THEN
c        CALL ERR_REP( ' ', 'Error reading spectral axis values',
c    :                 STATUS )
c       GOTO 99
c     END IF

*  Check if instrument is ROSAT XRT, WFC, EXOSAT ME or LE.
      NIGNORE = 0
      GEOMAREA = -1.0
      IF ( INDEX(INSTMNT, 'XRT') .GT. 0 ) THEN

        IF ( INDEX( DET,'PSPC') .NE. 0 ) THEN
	  GEOMAREA = 1141.0

*      Standard ignores for XRT/PSPC are channels 1-7
C         NIGNORE = 1
C         IGSTART(1) = 1
C         IGEND(1) = 7

        END IF

*  EXOSAT ME
      ELSE IF (INDEX(INSTMNT, 'ME') .GT. 0 ) THEN
         CALL ADI1_LOCAST( IFID, .FALSE., ASTLOC, STATUS )
         CALL DAT_FIND( ASTLOC, 'INSTRUMENT', INSLOC, STATUS )
         CALL DAT_THERE( INSLOC, 'ID_MASKS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND( INSLOC, 'ID_MASKS', MSKLOC, STATUS )
            CALL DAT_CELL( MSKLOC, 1, 1, LOCMSK, STATUS )
            CALL DAT_GET0C( LOCMSK, MASK, STATUS )
            GEOMAREA = 0.0
            DO I = 1, 8
               IF ( MASK(I:I) .EQ. '1' ) THEN
                  GEOMAREA = GEOMAREA + EXO_AREA(I)
               END IF
            END DO
         ELSE
            CALL MSG_PRNT( 'No ID_MASKS keyword, assuming full area' )
            GEOMAREA = 1676.71
         END IF

c        NIGNORE = 1
c        IGSTART(1) = 1
c        IGEND(1) = 4

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

*  What sort of ancillary response file is required
      OK = .FALSE.
      DO WHILE ( .NOT. OK )
        CALL USI_GET0C( 'ANC', ANCTYP, STATUS )
        CALL CHR_UCASE( ANCTYP )
        IF ( STR_ABBREV( ANCTYP, 'HELP' ) ) THEN
          WRITE (*,'(A)') ''
          WRITE (*,'(A)') 'The following options are possible:-'
          WRITE (*,'(A)') '  NONE - No ancillary file, ' //
     :                    'area contained in AREASCAL, ' //
     :                    'response matrix normalised'
          WRITE (*,'(A)') '  ABS - Ancillary file set to AREASCAL, ' //
     :                    'AREASCAL=1, respone matrix absolute'
          WRITE (*,'(A)') '  REL - Ancillary file set to 1, ' //
     :                    'AREASCAL=1, reponse matrix normalised'
          WRITE (*,'(A)') ''
          CALL USI_CANCL( 'ANC', STATUS )
        ELSE IF ( STR_ABBREV( ANCTYP, 'NONE' ) ) THEN
          OK = .TRUE.
        ELSE IF ( STR_ABBREV( ANCTYP, 'ABSOLUTE' ) ) THEN
          OK = .TRUE.
        ELSE IF ( STR_ABBREV( ANCTYP, 'RELATIVE' ) ) THEN
          OK = .TRUE.
        ELSE
          WRITE (*,'(A)') 'Must be one of N(ONE), A(BSOLUTE), ' //
     :                    'R(ELATIVE) or H(ELP)'
          CALL USI_CANCL( 'ANC', STATUS )
        END IF
      END DO
      MODE = ANCTYP(1:1)

*  Read in info from HDS file first.
      CALL ADI_CGET0R( TIMID, 'EffExposure', EXTIME, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI_CGET0R( TIMID, 'Exposure', EXTIME, STATUS )
      END IF
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
*  All FITSIO from here on in... (RB)
      CALL FTGIOU( LUN, FSTAT )
      CALL FTOPEN( LUN, OUTPHA(:OLEN), 1, BLKSIZ, FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        CALL FTDELT( LUN, FSTAT )
      END IF
      FSTAT = 0
      CALL FTINIT( LUN, OUTPHA(:OLEN), 2880, FSTAT )

*  initialise a standard FITS file and write main HDU keywords only
      CALL FTPHPR( LUN, .TRUE., 8, 0, 0, 0, 1, .TRUE., FSTAT)
      CALL FTPKYS( LUN, 'CREATOR', VERSION,
     :             'Creator of this file', FSTAT )
      CALL FTPKYS( LUN, 'CONTENT', 'SPECTRUM',
     :             'File contains spectrum', FSTAT )

*  Is the spectrum corrected (or are we dealing with /s data)?
      CALL PRF_GET( IFID, 'CORRECTED.EXPOSURE', EXPCOR, FSTAT )
      CALL BDI_AXGET0C( IFID, E_AX, 'Units', TUNIT(1), FSTAT )
      CALL BDI_GET0C( IFID, 'Units', TUNIT(2), FSTAT )
      IF ( .NOT. EXPCOR .AND. STR_SUB( '/', TUNIT(2) ) ) THEN
        EXPCOR = .TRUE.
      END IF

*  Is the spectrum background subtracted?
      CALL PRF_GET( IFID, 'BGND_SUBTRACTED', BGNDSB, STATUS )

*  Create table names, types and units
      BWIDTH = 0
      TTYPE(1) = 'CHANNEL'
      TFORM(1) = 'I'
      BWIDTH = BWIDTH + 2
      IF ( EXPCOR ) THEN
        TTYPE(2) = 'RATE'
        TFORM(2) = 'E'
        BWIDTH = BWIDTH + 4
      ELSE
        TTYPE(2) = 'COUNTS'
        TFORM(2) = 'I'
        BWIDTH = BWIDTH + 2
      END IF
      NFIELDS = 2
      IF ( ERROK ) THEN
        NFIELDS = NFIELDS + 1
        ERRCOL = NFIELDS
        TTYPE(NFIELDS) = 'STAT_ERR'
        TFORM(NFIELDS) = 'E'
        TUNIT(NFIELDS) = TUNIT(2)
        BWIDTH = BWIDTH + 4
      END IF
      IF ( QUALOK .OR. (NIGNORE.GT.0) ) THEN
        NFIELDS = NFIELDS + 1
        QUALCOL = NFIELDS
        TTYPE(NFIELDS) = 'QUALITY'
        TFORM(NFIELDS) = 'I'
        TUNIT(NFIELDS) = ' '
        BWIDTH = BWIDTH + 2
      END IF
      NFIELDS = NFIELDS + 1
      GRPGCOL = NFIELDS
      TTYPE(NFIELDS) = 'GROUPING'
      TFORM(NFIELDS) = 'I'
      TUNIT(NFIELDS) = ' '
      BWIDTH = BWIDTH + 2


*  Make the binary table
      CALL FTIBIN( LUN, DIMS(E_AX), NFIELDS, TTYPE, TFORM, TUNIT,
     :             'SPECTRUM', 0, FSTAT )

*  Create extension for spectrum (with "loads'a'keywords")
      CALL FTPKYF( LUN, 'EXPOSURE', EXTIME, 7,
     :             'Effective exposure time', FSTAT )
      IF ( MODE .EQ. 'A' ) THEN
        CALL FTPKYF( LUN, 'AREASCAL', 1.0, 7,
     :               'Area scaling factor (see .arf file)', FSTAT )
      ELSE
        CALL FTPKYF( LUN, 'AREASCAL', GEOMAREA, 7,
     :               'Area scaling factor (cm**2)', FSTAT )
      END IF
      CALL FTPKYF( LUN, 'BACKSCAL', 1.0, 7,
     :             'Background scaling factor', FSTAT )
      CALL FTPKYS( LUN, 'BACKFILE', 'none',
     :             'No backgorund file available', FSTAT )
      CALL FTPKYF( LUN, 'CORRSCAL', 1.0, 7,
     :             'Corrections scaling factor', FSTAT )
      CALL FTPKYS( LUN, 'CORRFILE', 'none',
     :             'No correction file available', FSTAT )
      CALL FTPKYS( LUN, 'RESPFILE', OUTRSP(:OLEN),
     :             'Redistribution matrix file (RMF)', FSTAT )
      IF ( MODE .EQ. 'A' .OR. MODE .EQ. 'R' ) THEN
        CALL FTPKYS( LUN, 'ANCRFILE', OUTANC(:OLEN),
     :               'Ancillary response file (ARF)', FSTAT )
      END IF
      CALL FTPKYS( LUN, 'XFLT0001', 'none',
     :             'XSPEC selection filter description', FSTAT )
      CALL FTPKYS( LUN, 'PHAVERSN', '1992a',
     :             'OGIP classification of FITS format style', FSTAT )
      CALL FTPKYS( LUN, 'CREATOR', VERSION,
     :             'Creator of this file', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLASS', 'OGIP',
     :             'Format conforms to OGIP standard', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS1', 'SPECTRUM',
     :             'Format contains spectrum', FSTAT )
      CALL FTPKYS( LUN, 'HDUVERS1', '1.1.0',
     :             'PHA version dated September 1993', FSTAT )
      IF ( BGNDSB ) THEN
        CALL FTPKYS( LUN, 'HDUCLAS2', 'NET',
     :               'A background subtracted spectrum', FSTAT )
      ELSE
        CALL FTPKYS( LUN, 'HDUCLAS2', 'TOTAL',
     :               'A gross spectrum (source + background)', FSTAT )
      END IF
      IF ( EXPCOR ) THEN
        CALL FTPKYS( LUN, 'HDUCLAS3', 'RATE',
     :               'Data stored in count/s', FSTAT )
      ELSE
        CALL FTPKYS( LUN, 'HDUCLAS3', 'COUNT',
     :               'Data stored in counts', FSTAT )
      END IF
      CALL FTPKYJ( LUN, 'TLMIN1', 1,
     :             'Lowest legal channel number', FSTAT )
      CALL FTPKYJ( LUN, 'TLMAX1', DIMS(E_AX),
     :             'Highest legal channel number', FSTAT )
      CALL FTPKYL( LUN, 'POISSERR', .FALSE.,
     :             'Poissonion errors appropriate to the data', FSTAT )
      CALL FTPKYS( LUN, 'CHANTYPE', 'PHA',
     :             'Channels assigned by detector electronics', FSTAT )
      CALL FTPKYJ( LUN, 'DETCHANS', DIMS(E_AX),
     :             'Total number of raw detector channels', FSTAT )

*  Write astrometry, detector info and timing to both extrension
      CALL A2X_PUTMIS( IFID, DETID, LUN, 2, FSTAT )
      CALL A2X_PUTPRJ( SYSID, PRJID, LUN, 2, FSTAT )
      CALL A2X_PUTTIM( TIMID, LUN, 2, FSTAT )

*  Write the spectrum
      CALL DYN_MAPR( 1, DIMS(E_AX), EAPTR, STATUS )
      CALL ARR_REG1R( 1.0, 1.0, DIMS(E_AX), %VAL(EAPTR), STATUS )
      CALL FTPCLE( LUN, 1, 1, 1, DIMS(E_AX), %VAL(EAPTR), STATUS )
      CALL FTPCLE( LUN, 2, 1, 1, DIMS(E_AX), %VAL(IDPTR), STATUS )
      IF ( ERROK ) THEN
        CALL FTPCLE( LUN, ERRCOL, 1, 1, DIMS(E_AX), %VAL(IEPTR), FSTAT )
      END IF

*  Write quality
      IF ( QUALOK ) THEN
        DO I = 1, DIMS(E_AX)
          CALL ARR_ELEM1L( IQPTR, DIMS(E_AX), I, LVAL, STATUS )
          IF ( LVAL ) THEN
            CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_OK, FSTAT )
          ELSE
            CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_BAD, FSTAT )
          END IF
        END DO
      ELSE IF ( NIGNORE .GT. 0 ) THEN
        DO I = 1, DIMS(E_AX)
          CALL FTPCLJ( LUN, QUALCOL, I, 1, 1, XSP_OK, FSTAT )
        END DO
      END IF
      IF ( NIGNORE .GT. 0 ) THEN
        DO I = 1, NIGNORE
          DO J = IGSTART(I), IGEND(I)
            CALL FTPCLJ( LUN, QUALCOL, J, 1, 1, XSP_BAD, FSTAT )
          END DO
        END DO
      END IF

*  Write grouping (all +1 at present)
      DO I = 1, DIMS(E_AX)
        CALL FTPCLJ( LUN, GRPGCOL, I, 1, 1, +1, FSTAT )
      END DO

*  Close spectrum file
      CALL FTCLOS( LUN, FSTAT )
      CALL FTFIOU( LUN )

*  Open the response file
      CALL FTGIOU( LUN2(1), FSTAT )
      CALL FTOPEN( LUN2(1), OUTRSP(:OLEN), 1, BLKSIZ, FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        CALL FTDELT( LUN2(1), FSTAT )
      ELSE
        FSTAT = 0
      END IF
      CALL FTINIT( LUN2(1), OUTRSP(:OLEN), 2880, FSTAT )

*  initialise a standard FITS file and write main HDU keywords only
      CALL FTPHPR( LUN2(1), .TRUE., 8, 0, 0, 0, 1, .TRUE., FSTAT)
      CALL FTPKYS( LUN2(1), 'CREATOR', VERSION,
     :             'Creator of this file', FSTAT )
      CALL FTPKYS( LUN2(1), 'CONTENT', 'MATRIX',
     :             'File contains response matrix', FSTAT )

*  Open the ancillary file if required
      IF ( MODE .EQ. 'A' .OR. MODE .EQ. 'R' ) THEN
        CALL FTGIOU( LUN2(2), FSTAT )
        CALL FTOPEN( LUN2(2), OUTANC(:OLEN), 1, BLKSIZ, FSTAT )
        IF ( FSTAT .EQ. 0 ) THEN
          CALL FTDELT( LUN2(2), FSTAT )
        ELSE
          FSTAT = 0
        END IF
        CALL FTINIT( LUN2(2), OUTANC(:OLEN), 2880, FSTAT )

        CALL FTPHPR( LUN2(2), .TRUE., 8, 0, 0, 0, 1, .TRUE., FSTAT)
        CALL FTPKYS( LUN2(2), 'CREATOR', VERSION,
     :               'Creator of this file', FSTAT )
        CALL FTPKYS( LUN2(2), 'CONTENT', 'MATRIX',
     :               'File contains ancillary matrix', FSTAT )
      END IF

*  write out the response matrix
      CALL A2X_WRITRMF( LUN2, RMFID, ARFID, MODE, STATUS )

*  write out the detector strings
      CALL A2X_PUTMIS( IFID, DETID, LUN2(1), 3, FSTAT )
      CALL A2X_PUTPRJ( SYSID, PRJID, LUN2(1), 3, FSTAT )
      CALL A2X_PUTTIM( TIMID, LUN2(1), 3, FSTAT )

      IF ( MODE .EQ. 'A' .OR. MODE .EQ. 'R' ) THEN
        CALL A2X_PUTMIS( IFID, DETID, LUN2(2), 2, FSTAT )
        CALL A2X_PUTPRJ( SYSID, PRJID, LUN2(2), 2, FSTAT )
        CALL A2X_PUTTIM( TIMID, LUN2(2), 2, FSTAT )
      END IF

*  Close the response file
      CALL FTCLOS( LUN2(1), STATUS )
      CALL FTFIOU( LUN2(1) )

*  Close the ancillary file if opened
      IF ( MODE .EQ. 'A' .OR. MODE .EQ. 'R' ) THEN
        CALL FTCLOS( LUN2(2), STATUS )
        CALL FTFIOU( LUN2(2) )
      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE A2X_WRITRMF( LUN2, RMFID, ARFID, MODE, STATUS )
*+
*  Name:
*     A2X_WRITRMF

*  Purpose:
*     Write ASTERIX type energy response to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL A2X_WRITRMF( LUN2, RMFID, ARFID, MODE, STATUS )

*  Description:
*     This method provides the low level mechanism of writing a energy
*     response structure to a dataset. It writes only that information
*     which is contained within the RMFID passed as the second argument.
*     Other data of interest must be written by surrounding methods
*     which have access to the dataset to which this response is
*     "attached".
*
*     The response is written in one of two formats. If the input
*     compression method is NONE, the the response is written as a simple
*     2D array in a fixed size BINTABLE extension. If the method is ASTERIX
*     or OGIP_CMP then the response is written to a variable field size
*     BINTABLE.

*  Arguments:
*     LUN2 = INTEGER (given)
*        Logical unit numbers for output
*     RMFID = INTEGER (given)
*        ADI idenifier of response matrix
*     ARFID = INTEGER (given)
*        ADI idenifier of ancillary matrix
*     MODE = CHARACTER(*) (given)
*        Type of ancillary response file reqiured (NON, ABS, or REL)
*     STATUS = INTEGER (given and returned)
*        The global status.

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

*  External Routines Used:
*     ADI:
*        ADI2_POGIPK	- Write OGIP classification keywords

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     14 Aug 1995 (DJA):
*        Divide ASTERIX response by geometrical area if available.
*      8 Apr 1997 (RB):
*        Re-write for stand-alone ast2xsp/fitsio mode
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants

*  Arguments Given:
      INTEGER			LUN2(2), RMFID, ARFID
      CHARACTER*(*)		MODE

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      REAL			AREA			! Geometrical area

      INTEGER			CIPTR			! Channel indices
      INTEGER			EBPTR			! Energy bounds ptr
      INTEGER			EIPTR			! Energy indices
      INTEGER			NCHAN			! # channel bins
      INTEGER			NENER			! # energy bins
      INTEGER			NRMF			! # response elements
      INTEGER			RPTR			! Mapped RMF
      INTEGER			WPTR1, WPTR2		!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get size of conceptual RMF
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )

*  Get size of matrix
      CALL ADI_CSIZE( RMFID, 'RMF', NRMF, STATUS )

*  Write the energy boundaries into the table
      CALL ADI_CMAPR( RMFID, 'Energy', 'READ', EBPTR, STATUS )

*  Map the matrix data
      CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )

*   Map the energy and channel index arrays
      CALL ADI_CMAPI( RMFID, 'ChannelIndices', 'READ', CIPTR, STATUS )
      CALL ADI_CMAPI( RMFID, 'EnergyIndices', 'READ', EIPTR, STATUS )

*   Map some workspace
      CALL DYN_MAPI( 1, NENER*2, WPTR1, STATUS )
      CALL DYN_MAPI( 1, NRMF*2, WPTR2, STATUS )

*   Get geometrical area
      CALL ADI_CGET0R( RMFID, 'GeometricalArea', AREA, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        AREA = 1.0
      END IF

*   Write the variable length fields
      IF ( MODE .EQ. 'N' .OR. MODE .EQ. 'A' .OR. MODE .EQ. 'R' ) THEN
        CALL A2X_RMF_RESP( LUN2(1), NENER, NCHAN, NRMF, %VAL(EBPTR),
     :                     %VAL(CIPTR), %VAL(EIPTR), %VAL(RPTR),
     :                     AREA, %VAL(WPTR1), %VAL(WPTR2), STATUS )
      END IF

*   Release workspace
      CALL DYN_UNMAP( WPTR1, STATUS )
      CALL DYN_UNMAP( WPTR2, STATUS )

*   Release index arrays
      CALL ADI_CUNMAP( RMFID, 'ChannelIndices', CIPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'EnergyIndices', EIPTR, STATUS )

*  Release the matrix data
      CALL ADI_CUNMAP( RMFID, 'Energy', EBPTR, STATUS )
      CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )

*  Write channel energy bounds extension
      CALL A2X_RMF_EBOU( LUN2(1), RMFID, STATUS )

*  Now handle the ancillary responce matrix
      IF (MODE .EQ. 'A' ) THEN
        CALL A2X_ANC_SPEC( LUN2(2), ARFID, NENER, %VAL(EBPTR),
     :                     AREA, STATUS )
      ELSE IF ( MODE .EQ. 'R' ) THEN
        CALL A2X_ANC_SPEC( LUN2(2), ARFID, NENER, %VAL(EBPTR),
     :                     1.0, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ERI2_WRITRMF_AST', STATUS )
      END IF

      END


      SUBROUTINE A2X_ANC_SPEC ( LUN, ARF, NE, EBND, AREA, STATUS )
*+
*-

* Import:
      INTEGER			LUN, ARF, NE
      REAL			EBND(*), AREA

* Status:
      INTEGER			STATUS

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			  CHR_LEN

      CHARACTER*132		OUTPHA, OUTRSP, OUTANC	! Output file names
      COMMON /F_NAMES/		OUTPHA, OUTRSP, OUTANC

* Local variables:
      CHARACTER*8		TTYPE(3)		! Column names
      CHARACTER*8		TFORM(3)		! Column types
      CHARACTER*8		TUNIT(3)		! Column units

      DATA TTYPE		/'ENERG_LO', 'ENERG_HI', 'SPECRESP'/
      DATA TFORM		/'1E',       '1E',       '1E'      /
      DATA TUNIT		/'keV',      'keV',      'cm**2'   /

      INTEGER			FSTAT
      INTEGER			I
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define the HDU data area
      CALL FTIBIN( LUN, NE, 3, TTYPE, TFORM, TUNIT, 'SPECRESP', 0,
     :             FSTAT )

*  Other mandatory keywords
      CALL FTPKYS( LUN, 'ARFVERSN', '1992a',
     :             'OGIP classification of FITS format style', FSTAT )
      CALL FTPKYS( LUN, 'PHAFILE', OUTPHA(:CHR_LEN(OUTPHA)),
     :             'Name of spectrum file (PHA)', FSTAT )

*  Write keywords for response extension
      CALL FTPKYS( LUN, 'HDUCLASS', 'OGIP',
     :             'Format conforms to OGIP standard', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS1', 'RESPONSE',
     :             'Dataset relates to spectral response', FSTAT )
      CALL FTPKYS( LUN, 'HDUVERS1', '1.0.0',
     :             'Version of family of formats', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS2', 'SPECRESP',
     :             'Type of data stored', FSTAT )
      CALL FTPKYS( LUN, 'HDUVERS2', '1.1.0',
     :             'Version of format', FSTAT )

*  Write energy lower and upper bounds
      CALL FTPCLE( LUN, 1, 1, 1, NE, EBND(1), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NE, EBND(2), FSTAT )

*  Write the response area (constant at present)
      DO I = 1, NE
        CALL FTPCLE( LUN, 3, I, 1, 1, AREA, FSTAT )
      END DO

*  Report any errors
      IF (FSTAT .NE. SAI__OK ) THEN
        WRITE(*,*) 'FITSIO error in A2X_ANC_SPEC'
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_ANC_SPEC', STATUS )
      END IF

      END



      SUBROUTINE A2X_RMF_RESP( LUN, NE, NCH, NRMF, EBND, CI,
     :                         EI, RSP, AREA, WRK1, WRK2, STATUS )
*+
*  Name:
*     A2X_RMF_RESP

*  Purpose:
*     Write ASTERIX energy response elements to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL A2X_RMF_RESP( LUN, NE, NCH, NRMF, EBND, CI, EI,
*                        RSP, AREA, WRK1, WRK2, STATUS )

*  Description:
*

*  Arguments:
*     LUN = INTEGER (given)
*        Logical unit number for output
*     NE = INTEGER (given)
*        Number of energy bins (ie number of rows in table)
*     NCH = INTEGER (given)
*        Number of channel bins
*     NRMF = INTEGER (given)
*        Number of specified response elements
*     EBND[] = REAL (given)
*        Energy bounds array
*     CI[] = INTEGER (given)
*        Channel indices of non-zero elements
*     EI[] = INTEGER (given)
*        Energy indices of non-zero elements
*     RSP[] = REAL (given)
*        Response elements
*     AREA = REAL (given)
*        Geometrical area in cm**2
*     WRK1[] = INTEGER (given)
*        Workspace as big as NE
*     WRK2[] = INTEGER (given)
*        Workspace as big as NRMF
*     STATUS = INTEGER (given and returned)
*        The global status.

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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			LUN, NE, NCH, NRMF, CI(*), EI(*)
      REAL			EBND(*),AREA,RSP(*)
      INTEGER			WRK1(NE,2), WRK2(NRMF,2)

*  Status:
      INTEGER                   STATUS                  ! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			  CHR_LEN

      CHARACTER*132		OUTPHA, OUTRSP, OUTANC	! Output file names
      COMMON /F_NAMES/		OUTPHA, OUTRSP, OUTANC

      CHARACTER*40		TELESCOP, INSTMNT, DET	! Mission strings
      COMMON /MISSION/		TELESCOP, INSTMNT, DET

*  Local Variables:
      CHARACTER*10		STR
      CHARACTER*8		TTYPE(6)		! Column names
      CHARACTER*8		TFORM(6)		! Column types
      CHARACTER*3		TUNIT(6)		! Column units

      INTEGER			ACTHEAP			! Actual heap size
      INTEGER			CS
      INTEGER			E			! Loop over energy
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			!
      INTEGER			LASTR			!
      INTEGER			LC			! Last channel bin
      INTEGER			LE			! Last energy bin
      INTEGER			LUN			! Logical unit
      INTEGER			MAX_NGRP		! Max value of N_GRP
      INTEGER			MAX_SMAT		! Max width of matrix
      INTEGER			NDIG			!
      INTEGER			NFIXED			!
      INTEGER			NRPTR			! Workspace
      INTEGER			NS			! Number of subsets
      INTEGER			R			! Loop over RMF
      INTEGER			SMAT			! Elements != 0 in row

      LOGICAL			FNVAR, MATVAR		! Use variable lengths?

*  Local data;
      DATA TUNIT		/'keV', 'keV', ' ', ' ', ' ', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0
      DO E = 1, NE
        WRK1(E,1) = 0
      END DO

*  Count number of channel subsets
      R = 1
      LE = -1
      LC = -1
      NS = 0
      DO WHILE ( R .LE. NRMF )

*    Same energy as before?
        IF ( EI(R) .EQ. LE ) THEN

*      Channel index has not advanced by one?
          IF ( CI(R) .NE. (LC+1) ) THEN

*        Increment number of subsets in this energy bin
            WRK1(LE,1) = WRK1(LE,1) + 1

*      Mark length of current subset
            IF ( NS .GT. 0 ) WRK2(NS,2) = CI(R-1) - WRK2(NS,1) + 1

*        Start new subset
            NS = NS + 1
            WRK2(NS,1) = CI(R)

          END IF

        ELSE

*      Mark length of current subset
          WRK2(NS,2) = CI(R-1) - WRK2(NS,1) + 1

*      Advance to next energy bin, start new subset
          LE = EI(R)

          NS = NS + 1
          WRK1(LE,1) = 1
          WRK1(LE,2) = R
          WRK2(NS,1) = CI(R)

        END IF

*    Next element
        LC = CI(R)
        R = R + 1

      END DO
      WRK2(NS,2) = CI(NRMF) - WRK2(NS,1) + 1

*  Find maximum size of N_GRP
      MAX_NGRP = 0
      MAX_SMAT = 0
      DO E = 1, NE
        MAX_NGRP = MAX( MAX_NGRP, WRK1(E,1) )
        IF ( (WRK1(E,1) .EQ. 0) .OR. (E.GT.LE) ) THEN
          SMAT = 0
        ELSE IF ( E .EQ. LE ) THEN
          SMAT = NRMF - WRK1(E,2)
        ELSE
          SMAT = WRK1(E+1,2) - WRK1(E,2)
        END IF
        MAX_SMAT = MAX( MAX_SMAT, SMAT )
      END DO

*  Use a variable length array for the F_CHAN and N_CHAN columns? Six is
*  factor by which a single element of F_CHAN or N_CHAN is smaller than
*  the descriptor required to store a variable length array element.
      NFIXED = MAX_NGRP * NE
      IF ( (MAX_NGRP .GT. 6) .AND. (NS .NE. NFIXED) .AND.
     :                              (NS .LE. 6*NFIXED) ) THEN
        ACTHEAP = 2*2*NS
        FNVAR = .TRUE.
      ELSE
        ACTHEAP = 0
        FNVAR = .FALSE.
      END IF

*  Use a variable length array for the matrix column? Three is the
*  factor by which a single element of the response is smaller than
*  the descriptor required to store a variable length array element.
      NFIXED = MAX_SMAT * NE
      IF ( (MAX_SMAT .GT. 3) .AND. (NRMF .NE. NFIXED) .AND.
     :                              (NRMF .le. 3* NFIXED) ) THEN
        ACTHEAP = ACTHEAP + 4*NRMF
        MATVAR = .TRUE.
      ELSE
        MATVAR = .FALSE.
      END IF

*  Construct the field descriptions for the BINTABLE
      TTYPE(1) = 'ENERG_LO'
      TFORM(1) = '1E'
      TTYPE(2) = 'ENERG_HI'
      TFORM(2) = '1E'
      TTYPE(3) = 'N_GRP'
      TFORM(3) = '1I'
      TTYPE(4) = 'F_CHAN'
      TTYPE(5) = 'N_CHAN'
      IF ( FNVAR ) THEN
        TFORM(4) = 'PI'
        TFORM(5) = 'PI'
      ELSE
        CALL CHR_ITOC( MAX_NGRP, STR, NDIG )
        TFORM(4) = STR(:NDIG)//'I'
        TFORM(5) = STR(:NDIG)//'I'
      END IF
      TTYPE(6) = 'MATRIX'
      IF ( MATVAR ) THEN
        TFORM(6) = 'PE'
      ELSE
        CALL CHR_ITOC( MAX_SMAT, STR, NDIG )
        TFORM(6) = STR(:NDIG)//'E'
      END IF

*  Workspace for normalised response
      CALL DYN_MAPR( 1, MAX_SMAT, NRPTR, STATUS )

*  Define the HDU data area
      CALL FTIBIN( LUN, NE, 6, TTYPE, TFORM, TUNIT, 'MATRIX', ACTHEAP,
     :             FSTAT )

*  Other mandatory keywords
      CALL FTPKYS( LUN, 'RMFVERSN', '1992a',
     :             'OGIP classification of FITS format style', FSTAT )
      CALL FTPKYS( LUN, 'CHANTYPE', 'PHA',
     :             'Channels assigned by detector electronics', FSTAT )
      CALL FTPKYJ( LUN, 'DETCHANS', NCH,
     :             'Total number of raw PHA channels', FSTAT )
      CALL FTPKYS( LUN, 'PHAFILE', OUTPHA(:CHR_LEN(OUTPHA)),
     :             'Name of spectrum file (PHA)', FSTAT )
      CALL FTPKYF( LUN, 'LO_THRESH', 0.0, 7,
     :             'Matrix elements below threshold not stored', FSTAT )

*  Write keywords for response extension
      CALL FTPKYS( LUN, 'HDUCLASS', 'OGIP',
     :             'Format conforms to OGIP standard', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS1', 'RESPONSE',
     :             'Dataset relates to spectral response', FSTAT )
      CALL FTPKYS( LUN, 'HDUVERS1', '1.0.0',
     :             'Version of family of formats', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS2', 'RSP_MATRIX',
     :             'Type of data stored', FSTAT )
      CALL FTPKYS( LUN, 'HDUVERS2', '1.1.0',
     :             'Version of format', FSTAT )
      CALL FTPKYS( LUN, 'HDUCLAS3', 'FULL',
     :             'All energy-dependent effects included', FSTAT )

*  Write energy lower and upper bounds
      CALL FTPCLE( LUN, 1, 1, 1, NE, EBND(1), FSTAT )
      CALL FTPCLE( LUN, 2, 1, 1, NE, EBND(2), FSTAT )

*  The N_GRP field
      CALL FTPCLJ( LUN, 3, 1, 1, NE, WRK1(1,1), FSTAT )

*  Write the table data
      R = 1
      CS = 1
      DO E = 1, NE

*    Subsets in this energy bin?
        IF ( WRK1(E,1) .GT. 0 ) THEN

*      Write the F_CHAN fields
          CALL FTPCLJ( LUN, 4, E, 1, WRK1(E,1), WRK2(CS,1), FSTAT )
          IF ( .NOT. FNVAR ) THEN
            DO I = WRK1(E,1) + 1, MAX_NGRP
              CALL FTPCLJ( LUN, 4, E, 1, I, 0, FSTAT )
            END DO
          END IF

*      Write the N_CHAN field
          CALL FTPCLJ( LUN, 5, E, 1, WRK1(E,1), WRK2(CS,2), FSTAT )
          IF ( .NOT. FNVAR ) THEN
            DO I = WRK1(E,1) + 1, MAX_NGRP
              CALL FTPCLJ( LUN, 5, E, 1, I, 0, FSTAT )
            END DO
          END IF

*      Write the channel values
          IF ( CS .LE. NS ) THEN
            IF ( CS .EQ. NS ) THEN
              LASTR = NRMF
            ELSE
              LASTR = WRK1(E+1,2) - 1
            END IF

*        Copy response values and normalise (RB)
            CALL ARR_COP1R( LASTR - R + 1, RSP(R), %VAL(NRPTR), STATUS )
            CALL ARR_MULT1R( LASTR - R + 1, %VAL(NRPTR), 1.0 / AREA,
     :                       %VAL(NRPTR), STATUS )

            CALL FTPCLE( LUN, 6, E, 1, LASTR - R + 1, %VAL(NRPTR),
     :                   FSTAT )
            IF ( .NOT. MATVAR ) THEN
              DO I = (LASTR - R + 1) + 1, MAX_SMAT
                CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
              END DO
            END IF
          ELSE
            IF ( .NOT. MATVAR ) THEN
              DO I = 1, MAX_SMAT
                CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
              END DO
            END IF
          END IF

*      Advance the element counter
          R = LASTR + 1
          CS = CS + WRK1(E,1)

*    Fill in zeroes if not vector columns
        ELSE IF ( .NOT. FNVAR ) THEN

          DO I = 1, MAX_NGRP
            CALL FTPCLJ( LUN, 4, E, 1, I, 0, FSTAT )
            CALL FTPCLJ( LUN, 5, E, 1, I, 0, FSTAT )
          END DO
          IF ( .NOT. MATVAR ) THEN
            DO I = 1, MAX_SMAT
              CALL FTPCLE( LUN, 6, E, 1, I, 0.0, FSTAT )
            END DO
          END IF

        END IF

      END DO

*   Free workspace
      CALL DYN_UNMAP( NRPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_RMF_RESP', STATUS )
      END IF

      END


      SUBROUTINE A2X_RMF_EBOU( LUN, RMFID, STATUS )
*+
*  Name:
*     A2X_RMF_EBOU

*  Purpose:
*     Write channel energy bounds to EBOUNDS extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL A2X_RMF_EBOU( LUN, RMFID, STATUS )

*  Description:
*

*  Arguments:
*     LUN = INTEGER (given)
*        Logical unit number for output
*     RMFID = INTEGER (given)
*        Response containing
*     STATUS = INTEGER (given and returned)
*        The global status.

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

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			LUN			! See above
      INTEGER			RMFID			!

*  Status:
      INTEGER                   STATUS, FSTAT           ! Global status

* External references:
      EXTERNAL			CHR_LEN
        INTEGER			  CHR_LEN

*  Local Variables:
      CHARACTER*8		TTYPE(3)		! Column names
      CHARACTER*8		TFORM(3)		! Column types
      CHARACTER*3		TUNIT(3)		! Column units

      CHARACTER*132		OUTPHA, OUTRSP, OUTANC	! Output file names
      COMMON /F_NAMES/		OUTPHA, OUTRSP, OUTANC

      INTEGER			CBPTR			! Channel energies
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			!
      INTEGER			LUN			! Logical unit
      INTEGER			NCHAN			! Number of channels
      INTEGER			BWIDTH			! Width of binary table in bytes
      INTEGER			HDUTYPE

      LOGICAL			THERE			! Bounds specified?

*  Local data:
      DATA    			TUNIT/' ', ' ', ' '/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of channels
      CALL ADI_CGET0I( RMFID, 'NCHAN', NCHAN, STATUS )

*  Does the Channels member exist?
      CALL ADI_THERE( RMFID, 'Channels', THERE, STATUS )
      IF ( THERE ) THEN

*    Construct the field descriptions for the BINTABLE
        BWIDTH = 0
        TTYPE(1) = 'CHANNEL'
        TFORM(1) = '1I'
        BWIDTH = BWIDTH + 2
        TTYPE(2) = 'E_MIN'
        TFORM(2) = '1E'
        BWIDTH = BWIDTH + 4
        TTYPE(3) = 'E_MAX'
        TFORM(3) = '1E'
        BWIDTH = BWIDTH + 4

*    Define the HDU data area
        CALL FTMAHD( LUN, 2, HDUTYPE, FSTAT )
        CALL FTIBIN( LUN, NCHAN, 3, TTYPE, TFORM, TUNIT, 'EBOUNDS', 0,
     :               FSTAT )

*    Other mandatory keywords
        CALL FTPKYS( LUN, 'RMFVERSN', '1992a',
     :               'OGIP classification of FITS format style', FSTAT )
        CALL FTPKYS( LUN, 'CHANTYPE', 'PHA',
     :               'Channels assigned by detector electronics',FSTAT )
        CALL FTPKYS( LUN, 'PHAFILE', OUTPHA(:CHR_LEN(OUTPHA)),
     :               'Name of spectrum file (PHA)', FSTAT )
        CALL FTPKYJ( LUN, 'DETCHANS', NCHAN,
     :               'Total number of raw PHA channels', FSTAT )
        CALL FTPKYS( LUN, 'HDUCLASS', 'OGIP',
     :               'Format conforms to OGIP standard', FSTAT )
        CALL FTPKYS( LUN, 'HDUCLAS1', 'RESPONSE',
     :               'Dataset relates to spectral response', FSTAT )
        CALL FTPKYS( LUN, 'HDUVERS1', '1.0.0',
     :               'Version of family of formats', FSTAT )
        CALL FTPKYS( LUN, 'HDUCLAS2', 'EBOUNDS',
     :               'Type of data stored', FSTAT )
        CALL FTPKYS( LUN, 'HDUVERS2', '1.1.0',
     :               'Version of format', FSTAT )

*    Write the channels column
        FSTAT = 0
        DO I = 1, NCHAN
          CALL FTPCLJ( LUN, 1, I, 1, 1, I, FSTAT )
        END DO

*  Map the channel bounds array
        CALL ADI_CMAPR( RMFID, 'Channels', 'READ', CBPTR, STATUS )

*  Write the bounds columns
        CALL FTPCLE( LUN, 2, 1, 1, NCHAN, %VAL(CBPTR), FSTAT )
        CALL FTPCLE( LUN, 3, 1, 1, NCHAN, %VAL(CBPTR+4), FSTAT )

*  End of Channels presence test
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_RMF_EBOU', STATUS )
      END IF

      END



      SUBROUTINE A2X_PUTMIS( IFID, DETID, LUN, N, FSTAT )
*+
*

* Import:
      INTEGER			IFID			!
      INTEGER			DETID			!
      INTEGER			LUN			!
      INTEGER			N

* Status:
      INTEGER			FSTAT

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

* Local variables:
      CHARACTER*80		STRING

      INTEGER			I
      INTEGER			HDUTYPE
      INTEGER			STATUS

      LOGICAL			OK
*.
      IF ( FSTAT .NE. SAI__OK ) RETURN
      STATUS = FSTAT

      DO I = 1, N
        CALL FTMAHD( LUN, I, HDUTYPE, FSTAT )

        CALL BDI_CHK( IFID, 'Title', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_GET0C( IFID, 'Title', STRING, STATUS )
          CALL FTPKYS( LUN, 'OBJECT', STRING,
     :                 'Name of target', FSTAT )
        END IF

        CALL ADI_THERE( DETID, 'Mission', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0C( DETID, 'Mission', STRING, STATUS )
          CALL FTPKYS( LUN, 'TELESCOP', STRING,
     :                 'Telescope (mission) name', FSTAT )
        END IF

        CALL ADI_THERE( DETID, 'Instrument', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0C( DETID, 'Instrument', STRING, STATUS )
          CALL FTPKYS( LUN, 'INSTRUME', STRING,
     :                 'Instrument name', FSTAT )
        END IF

        CALL ADI_THERE( DETID, 'Detector', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0C( DETID, 'Detector', STRING, STATUS )
          CALL FTPKYS( LUN, 'DETNAM', STRING,
     :                 'Detector name', FSTAT )
        END IF

        CALL ADI_THERE( DETID, 'Filter', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0C( DETID, 'Filter', STRING, STATUS )
          CALL FTPKYS( LUN, 'FILTER', STRING,
     :                 'Instrument filter name', FSTAT )
        END IF
      END DO

*  Report any errors
      IF (FSTAT .NE. SAI__OK ) THEN
        WRITE(*,*) 'FITSIO error in A2X_PUTMIS'
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_PUTMIS', STATUS )
      END IF

      END



      SUBROUTINE A2X_PUTPRJ( SYSID, PRJID, LUN, N, FSTAT )
*+
*

* Import:
      INTEGER			SYSID
      INTEGER			PRJID			!
      INTEGER			LUN			!
      INTEGER			N

* Status:
      INTEGER			FSTAT

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

* Local variables:
      CHARACTER*80		STRING

      DOUBLE PRECISION		SPOINT(2)
      REAL			NUMBER, DUM

      INTEGER			I
      INTEGER			HDUTYPE
      INTEGER			STATUS
*.
      IF ( FSTAT .NE. SAI__OK ) RETURN
      STATUS = FSTAT

      DO I = 1, N
        CALL FTMAHD( LUN, I, HDUTYPE, FSTAT )

        CALL ADI_CGET0C( SYSID, 'NAME', STRING, STATUS )
        CALL FTPKYS( LUN, 'RADECSYS', STRING,
     :               'World coordinate system', FSTAT )

        CALL ADI_CGET0R( SYSID, 'EQUINOX', NUMBER, STATUS )
        CALL FTPKYF( LUN, 'EQUINOX', NUMBER, 7,
     :               'Epoch of mean equator & equinox', FSTAT )

	CALL ADI_CGET1D( PRJID, 'SPOINT', 2, SPOINT, DUM, STATUS )
        CALL FTPKYF( LUN, 'RA_OBJ', REAL(SPOINT(1)), 7,
     :               'Right ascension of the object (degrees)', FSTAT )
        CALL FTPKYF( LUN, 'DEC_OBJ', REAL(SPOINT(2)), 7,
     :               'Declination of the object (degrees)', FSTAT )
      END DO

*  Report any errors
      IF (FSTAT .NE. SAI__OK ) THEN
        WRITE(*,*) 'FITSIO error in A2X_PUTPRJ'
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_PUTPRJ', STATUS )
      END IF

      END



      SUBROUTINE A2X_PUTTIM( TIMID, LUN, N, FSTAT )
*+
*

* Import:
      INTEGER			TIMID			!
      INTEGER			LUN			!
      INTEGER			N

* Status:
      INTEGER			FSTAT

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

* Local variables:
      CHARACTER*20		TSTR, DSTR

      DOUBLE PRECISION		MJD, OBLEN

      INTEGER			I
      INTEGER			HDUTYPE
      INTEGER			STATUS

      LOGICAL			OK
*.
      IF ( FSTAT .NE. SAI__OK ) RETURN
      STATUS = FSTAT

      DO I = 1, N
        CALL FTMAHD( LUN, I, HDUTYPE, FSTAT )

        CALL ADI_THERE( TIMID, 'MJDObs', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( TIMID, 'MJDObs', MJD, STATUS )
          CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
          CALL FTPKYG( LUN, 'MJD-OBS', MJD, 13,
     :                 'MJD of the data start', FSTAT )
          CALL FTPKYS( LUN, 'DATE-OBS', DSTR,
     :                 'Date of data start', FSTAT )
          CALL FTPKYS( LUN, 'TIME-OBS', TSTR,
     :                 'Time of data start', FSTAT )
          CALL ADI_THERE( TIMID, 'ObsLength', OK, STATUS )
          IF ( OK ) THEN
            CALL ADI_CGET0D( TIMID, 'ObsLength', OBLEN, STATUS )
            MJD = MJD + OBLEN / 86400.0D0
            CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
            CALL FTPKYG( LUN, 'MJD-END', MJD, 13,
     :                 'MJD of the data end', FSTAT )
            CALL FTPKYS( LUN, 'DATE-END', DSTR,
     :                 'Date of data end', FSTAT )
            CALL FTPKYS( LUN, 'TIME-END', TSTR,
     :                 'Time of data end', FSTAT )
          END IF
        END IF
        CALL ADI_THERE( TIMID, 'MJDRef', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( TIMID, 'MJDRef', MJD, STATUS )
          CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
          CALL FTPKYG( LUN, 'MJDREF', MJD, 13,
     :                 'MJD SC clock start', FSTAT )
          CALL FTPKYS( LUN, 'ZERODATE', DSTR,
     :                 'Date of data start', FSTAT )
          CALL FTPKYS( LUN, 'ZEROTIME', TSTR,
     :                 'Time of data start', FSTAT )
        END IF
      END DO

*  Report any errors
      IF (FSTAT .NE. SAI__OK ) THEN
        WRITE(*,*) 'FITSIO error in A2X_PUTTIM'
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'A2X_PUTTIM', STATUS )
      END IF

      END
