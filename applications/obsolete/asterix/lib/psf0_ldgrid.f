      SUBROUTINE PSF0_LDGRID( GRID, DIMS, NDIM, DX, XYSAM, RPTR,
     :                        EPTR, DPTR, FLAGS, STATUS )
*+
*  Name:
*     PSF0_LDGRID

*  Purpose:
*     Locate and load the data from a psf grid

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_LDGRID( GRID, DIMS, NDIM, STATUS )

*  Description:
*     Loads psf grid file into memory, and releases the file. All data can
*     be released by DYN_UNMAPing the non-zero pointers returned.

*  Arguments:
*     GRID = CHARACTER*(*) (given)
*        The grid file name. Appended to the AST_ETC environment variable
*        to generate the full file name
*     DIMS[5] = INTEGER (returned)
*        Grid dimensions. Only NDIM values are set
*     NDIM = INTEGER (returned)
*        Number of grid dimensions. Can take values 3..5 depending on
*        presence of energy axis and polar/rectangular mode
*     DX = REAL (returned)
*        Size in radians of gridded psf pixels
*     XYSAM[2,2] = REAL (returned)
*        Origin and spacing of rectangular samples if non-polar. Units
*        are radians
*     RPTR = INTEGER (returned)
*        Pointer to radial samples, if radial mode, otherwise zero. Units
*        are radians
*     EPTR = INTEGER (returned)
*        Pointer to energy bin values, zero if none
*     DPTR = INTEGER (returned)
*        Pointer to grid data
*     FLAGS = INTEGER (returned)
*        Flags denoting grid state.
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
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 May 1996 (DJA):
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
      CHARACTER*(*)		GRID

*  Arguments Returned:
      INTEGER			DIMS(5), NDIM, RPTR, EPTR, DPTR, FLAGS
      REAL			DX, XYSAM(2,2)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*80		ETCDIR			! AST_ETC directory
        SAVE			ETCDIR
      CHARACTER*132		GNAME			! Full grid name
      CHARACTER*40		UNITS			! Grid spatial units

      REAL			SPAR(2)			! Spaced array info
      REAL			TOR			! Conversion to radian

      INTEGER			APTR			! Mapped grid item
      INTEGER			EAX			! Energy axis number
      INTEGER			ELEN			! Length of ETCDIR
	SAVE			ELEN
      INTEGER			FID			! Grid file identifier
      INTEGER			IAX			! Loop over axes
      INTEGER			NELM			! Total # grid elements

      LOGICAL			FIRST			! First time through?
	SAVE 			FIRST
      LOGICAL			POLAR			! Polar grid?

*  Local Data:
      DATA			FIRST/.TRUE./
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First time through
      IF ( FIRST ) THEN

*    Get ASTERIX data directory
        CALL PSX_GETENV( 'AST_ETC', ETCDIR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          ETCDIR = '.'
          ELEN = 1
        ELSE
          ELEN = CHR_LEN ( ETCDIR )
        END IF

*    Not first any more
        FIRST = .FALSE.

      END IF

*  Construct full grid name
      GNAME = ETCDIR(:ELEN)//'/'//GRID

*  Try and load the file
      CALL ADI_FOPEN( GNAME, 'PsfGrid', 'READ', FID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Get grid dimensions
        CALL BDI_GETSHP( FID, 5, DIMS, NDIM, STATUS )

*    Energy axis present?
        CALL BDI0_FNDAXC( FID, 'E', EAX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          EAX = 0
        END IF

*    If present make copy of energy axis values
        IF ( EAX .GT. 0 ) THEN
          CALL DYN_MAPR( 1, DIMS(EAX), EPTR, STATUS )
          CALL BDI_AXMAPR( FID, EAX, 'Data', 'READ', APTR, STATUS )
          CALL ARR_COP1R( DIMS(EAX), %VAL(APTR), %VAL(EPTR), STATUS )
          CALL BDI_AXUNMAP( FID, EAX, 'Data', APTR, STATUS )
        ELSE
          EPTR = 0
        END IF

*    Get psf pixel size in radians
        CALL BDI_AXGET0C( FID, 1, 'Units', UNITS, STATUS )
        CALL CONV_UNIT2R( UNITS, TOR, STATUS )
        CALL BDI_AXGET1R( FID, 1, 'SpacedData', 2, SPAR, NELM, STATUS )
        DX = ABS(SPAR(2)) * TOR

*    Polar or rectaungular gridding?
        POLAR = .TRUE.
        IF ( (NDIM .EQ. 5) .OR. ((NDIM.EQ.4) .AND. (EAX.EQ.0)) ) THEN
          POLAR = .FALSE.
        END IF

*    Get spatial grouping of psfs
        IF ( POLAR ) THEN

*      Make copy of radii
          CALL DYN_MAPR( 1, DIMS(3), RPTR, STATUS )
          CALL BDI_AXMAPR( FID, 3, 'Data', 'READ', APTR, STATUS )
          CALL ARR_COP1R( DIMS(3), %VAL(APTR), %VAL(RPTR), STATUS )
          CALL BDI_AXUNMAP( FID, 3, 'Data', APTR, STATUS )

*      Get radian conversion factor
          CALL BDI_AXGET0C( FID, 3, 'Units', UNITS, STATUS )
          CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*      Scale the radii
          CALL ARR_MULTR( TOR, DIMS(3), %VAL(RPTR) )

*    Rectangular grid
        ELSE
          RPTR = 0

          DO IAX = 1, 2
            CALL BDI_AXGET0C( FID, IAX + 2, 'Units', UNITS, STATUS )
            CALL CONV_UNIT2R( UNITS, TOR, STATUS )
            CALL BDI_AXGET1R( FID, IAX + 2, 'SpacedData', 2,
     :                        XYSAM(1,IAX), NELM, STATUS )
            XYSAM(1,IAX) = XYSAM(1,IAX) * TOR
            XYSAM(2,IAX) = XYSAM(2,IAX) * TOR
          END DO

        END IF

*    Make a copy of the psf grid data
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
        CALL DYN_MAPR( NDIM, DIMS, DPTR, STATUS )
        CALL BDI_MAPR( FID, 'Data', 'READ', APTR, STATUS )
        CALL ARR_COP1R( NELM, %VAL(APTR), %VAL(DPTR), STATUS )
        CALL BDI_UNMAP( FID, 'Data', APTR, STATUS )

*    Set the flags
        FLAGS = 0

*    Release the file
        CALL ADI_FCLOSE( FID, STATUS )

*    Announce successful load
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETC( 'G', GRID )
          CALL MSG_SETC( 'DIR', ETCDIR )
          CALL MSG_OUT( ' ', 'Loaded psf grid ^G from ^DIR' )
        END IF

*  Report error
      ELSE
        CALL MSG_SETC( 'G', GRID )
        CALL ERR_REP( ' ', 'Unable to load psf grid ^G', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_LDGRID', STATUS )
      END IF

      END
