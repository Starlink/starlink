      SUBROUTINE BDI2_SETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_SETLNK

*  Purpose:
*     Service SetLink method for various class to FITSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SETLNK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Establishes ADI file link between high level objects Scalar, Array
*     and BinDS and the FITSfile.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			  CHR_SIMLR

*  Local Variables:
      CHARACTER*4		BCS			! String of BCOL
      CHARACTER*72		CMNT			! Key comment
      CHARACTER*20		CONTNT			! CONTENT keyword value
      CHARACTER*80		FPATH			! Sub-HDU path info
      CHARACTER*10		TYP			! Primary data type

      INTEGER			BCOL			! Binary table column #
      INTEGER			BITPIX			! Bits per pixel
      INTEGER			BLEN			! Length of BCS
      INTEGER			DHDU			! Data HDU
      INTEGER			DIMS(ADI__MXDIM)	! Dimensions
      INTEGER			HDUTYP			! FITSIO HDU type code
      INTEGER			MID			! Output model object
      INTEGER			NDIM			! Dimensionality
      INTEGER			NWRD			! # words in FPATH
      INTEGER			PHDU			! Main HDU
      INTEGER			SPOS(3), EPOS(3)	! FPATH bits
      INTEGER			UIHDU			! User specified HDU #

      LOGICAL			FPOK			! Fpath data present?
      LOGICAL			ISARY			! Array?
      LOGICAL			ISIMAG			! Image?
      LOGICAL			ISSCAL			! Scalar?
      LOGICAL			ISSPEC			! Spectrum?
      LOGICAL			ISTIME			! Time series?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Initialise
      ISARY = .FALSE.
      ISSPEC = .FALSE.
      ISIMAG = .FALSE.
      ISTIME = .FALSE.
      ISSCAL = .FALSE.

*  Did user supply an HDU number?
      CALL ADI_CGET0I( ARGS(2), 'UserHDU', UIHDU, STATUS )

*  Locate the main HDU for this input. This is the primary HDU for file
*  level input, or the user specified HDU otherwise.
      CALL ADI2_FNDHDU( ARGS(2), 'PRIMARY', .FALSE., PHDU, STATUS )
      IF ( PHDU .EQ. ADI__NULLID .OR. STATUS .EQ. SAI__ERROR ) THEN
        PHDU = ADI__NULLID
        CALL ERR_ANNUL( STATUS )
        GOTO 99
      END IF

*  If the user has specified the HDU we can't move outside it
      IF ( UIHDU .GT. 0 ) THEN

*    Did user supply sub-HDU path info?
        CALL ADI_THERE( ARGS(2), 'Fpath', FPOK, STATUS )
        IF ( FPOK ) THEN
          CALL ADI_CGET0C( ARGS(2), 'Fpath', FPATH, STATUS )
          CALL ADI2_FPSPL( FPATH, 3, SPOS, EPOS, NWRD, STATUS )
        ELSE
          NWRD = 0
        END IF

*    The only possible abstract types (for BDI) with an image extension are
*    BinDS (the whole HDU), or Scalar (a keyword). If the extension is a
*    a BINTABLE then there must be additional path details. If the first
*    path name string is a column name, and there are no additional details
*    then the returned type is Array. If the name is not a column name then
*    the returned type is Scalar. If the the path is a column, then the
*    following modifiers are allowed to specify particular keywords (.UNIT,
*    .MIN, .MAX).
        CALL ADI_CGET0I( PHDU, 'HduType', HDUTYP, STATUS )
        IF ( (HDUTYP .EQ. 1) .OR. (HDUTYP.EQ.2) ) THEN

*      If the user did not specify anything beyond the BINTABLE we have
*      an error
          IF ( NWRD .EQ. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BDI2_SETLNK_BT', 'BDI cannot access ascii'/
     :                    /'or binary table as input', STATUS )
          ELSE

*        Does the 2nd path word specify a table column?
            CALL ADI2_FNDBTC( PHDU, FPATH(SPOS(2):EPOS(2)), BCOL,
     :                        STATUS )
            IF ( BCOL .GT. 0 ) THEN

*          Convert column number to string for keyword testing
              CALL CHR_ITOC( BCOL, BCS, BLEN )

*          If no other stuff supplied we're dealing with the whole column
              IF ( NWRD .EQ. 2 ) THEN

*            In which case our link type is Array
                ISARY = .TRUE.

*            The sub-item is the column name
                CALL ADI_CPUT0C( ARGS(2), 'SubItem',
     :                           FPATH(SPOS(2):EPOS(2)), STATUS )

*            Convert type info. Only make array 2-D if column has many
*            values
                CALL ADI2_BTCTYP( PHDU, BCOL, DIMS(1), TYP, STATUS )
                IF ( DIMS(1) .EQ. 1 ) THEN
                  NDIM = 1
                ELSE
                  NDIM = 2
                END IF

*            NDIM'th dimension is the length of the table
                CALL ADI2_HGKYI( PHDU, 'NAXIS2', DIMS(NDIM), CMNT,
     :                           STATUS )

*          The TLMIN keyword for the column?
              ELSE IF ( CHR_SIMLR( FPATH(SPOS(3):EPOS(3)),
     :                             'MIN' ) ) THEN

*            Link with the keyword
                CALL BDI2_LNKKEY( ARGS(2), PHDU, 'TLMIN'//BCS(:BLEN),
     :                            .FALSE., ' ', TYP, ISSCAL, STATUS )

*          The TLMAX keyword for the column?
              ELSE IF ( CHR_SIMLR( FPATH(SPOS(3):EPOS(3)),
     :                             'MAX' ) ) THEN

*            Link with the keyword
                CALL BDI2_LNKKEY( ARGS(2), PHDU, 'TLMAX'//BCS(:BLEN),
     :                            .FALSE., ' ', TYP, ISSCAL, STATUS )

*          The TUNIT keyword for the column?
              ELSE IF ( CHR_SIMLR( FPATH(SPOS(3):EPOS(3)),
     :                             'UNIT' ) ) THEN

*            Link with the keyword
                CALL BDI2_LNKKEY( ARGS(2), PHDU, 'TUNIT'//BCS(:BLEN),
     :                            .FALSE., ' ', TYP, ISSCAL, STATUS )

              END IF

*        Otherwise must be a keyword
            ELSE

*          Link with the keyword
              CALL BDI2_LNKKEY( ARGS(2), PHDU, FPATH(SPOS(2):EPOS(2)),
     :                          (SPOS(3).GT.0), FPATH(EPOS(2)+2:),
     :                          TYP, ISSCAL, STATUS )

            END IF

          END IF

        ELSE

*      Second path item should be keyword name (1st is HDU itself)
          IF ( NWRD .GT. 1 ) THEN

*        Link with the keyword
            CALL BDI2_LNKKEY( ARGS(2), PHDU, FPATH(SPOS(2):EPOS(2)),
     :                        (SPOS(3).GT.0), FPATH(EPOS(2)+2:),
     :                        TYP, ISSCAL, STATUS )

          ELSE

*        Get shape of HDU
            CALL ADI2_IMGTSHP( PHDU, .FALSE., BITPIX, NDIM, DIMS,
     :                         STATUS )
            CALL ADI2_BP2TYP( BITPIX, TYP, STATUS )

          END IF

        END IF

*  Look for high level data forms
      ELSE

*    HDU has CONTENT keyword?
        CALL ADI2_HGKYC( PHDU, 'CONTENT', CONTNT, CMNT, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL CHR_UCASE( CONTNT )
          IF ( INDEX( CONTNT, 'SPECTRUM' ) .GT. 0 ) THEN
            ISSPEC = .TRUE.
          ELSE IF ( INDEX( CONTNT, 'IMAGE' ) .GT. 0 ) THEN
            ISIMAG = .TRUE.
          ELSE IF ( INDEX( CONTNT, 'SERIES' ) .GT. 0 ) THEN
            ISTIME = .TRUE.
          END IF
        ELSE
          CALL ERR_ANNUL( STATUS )

*      Check dimensionality
          CALL ADI2_HGKYI( PHDU, 'NAXIS', NDIM, CMNT, STATUS )

*      Look for RATE extension if no primary data
          IF ( NDIM .EQ. 0 ) THEN
            CALL ADI2_FNDHDU( ARGS(2), 'RATE', .FALSE., DHDU, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
              ISTIME = .TRUE.
              CALL ADI_ERASE( DHDU, STATUS )
            ELSE
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Unknown FITS dataset type', STATUS )
            END IF
          ELSE

*        Get shape of HDU
            CALL ADI2_IMGTSHP( PHDU, .FALSE., BITPIX, NDIM, DIMS,
     :                         STATUS )
            CALL ADI2_BP2TYP( BITPIX, TYP, STATUS )

          END IF

        END IF

      END IF

*  Switch on the supported dataset types. First images
 99   IF ( ISIMAG ) THEN

*    New model object
        CALL ADI_NEW0( 'XYimage', OARG, STATUS )

*    The number of bins is the number of rows in the table
        NDIM = 2
        CALL ADI2_HGKYI( PHDU, 'NAXIS1', DIMS(1), CMNT, STATUS )
        CALL ADI2_HGKYI( PHDU, 'NAXIS2', DIMS(2), CMNT, STATUS )

*    Convert BITPIX value to type
        CALL ADI2_HGKYI( PHDU, 'BITPIX', BITPIX, CMNT, STATUS )
        CALL ADI2_BP2TYP( BITPIX, TYP, STATUS )

*  OGIP spectra
      ELSE IF ( ISSPEC ) THEN

*    New model object
        CALL ADI_NEW0( 'Spectrum', OARG, STATUS )

*    Locate the extension containing the spectrum
        CALL ADI2_FNDHDU( ARGS(2), 'SPECTRUM', .FALSE., DHDU, STATUS )

*    The number of bins is the number of rows in the table
        NDIM = 1
        CALL ADI2_HGKYI( DHDU, 'NAXIS2', DIMS(1), CMNT, STATUS )

*    Type is REAL
        TYP = 'REAL'

*    Release the data HDU
        CALL ADI_ERASE( DHDU, STATUS )

*  OGIP time series
      ELSE IF ( ISTIME ) THEN

*    New model object
        CALL ADI_NEW0( 'TimeSeries', OARG, STATUS )

*    Locate the extension containing the time series
        CALL ADI2_FNDHDU( ARGS(2), 'RATE', .FALSE., DHDU, STATUS )

*    The number of bins is the number of rows in the table
        NDIM = 1
        CALL ADI2_HGKYI( DHDU, 'NAXIS2', DIMS(1), CMNT, STATUS )

*    Type is REAL
        TYP = 'DOUBLE'

*    Release the data HDU
        CALL ADI_ERASE( DHDU, STATUS )

*  Array?
      ELSE IF ( ISARY ) THEN

*    New model object
        CALL ADI_NEW0( 'Array', OARG, STATUS )

*  Scalar?
      ELSE IF ( ISSCAL ) THEN

*    New model object
        CALL ADI_NEW0( 'Scalar', OARG, STATUS )

*    Set dimensions
        NDIM = 0
        DIMS(1) = 0

      END IF

      IF ( OARG .EQ. ADI__NULLID ) THEN
        MID = ARGS(1)
      ELSE
        MID = OARG
      END IF

*  Store the dimensions
      CALL BDI_SETSHP( MID, NDIM, DIMS, STATUS )
      CALL BDI_SETTYP( MID, TYP, STATUS )

*  Release the main HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_SETLNK', STATUS )

*  Make link
      CALL ADI_SETLNK( MID, ARGS(2), STATUS )

      END



      SUBROUTINE BDI2_LNKKEY( FID, HDU, KEY, GSUB, SUBKEY, TYP, ISSCAL,
     :                        STATUS )
*+
*  Name:
*     BDI2_LNKKEY

*  Purpose:
*     Try to link BDI with a FITS keyword or its comment

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SETLNK_KEY( FID, HDU, KEY, GSUB, SUBKEY, TYP, ISSCAL, STATUS )

*  Description:
*     Establishes ADI file link between a Scalar and a FITS keyword

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INTEGER                   FID, HDU
      LOGICAL			GSUB
      CHARACTER*(*)		KEY, SUBKEY

*  Arguments Returned:
      LOGICAL			ISSCAL
      CHARACTER*(*)		TYP

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			  CHR_SIMLR

*  Local Variables:
      CHARACTER*10		SUBITM			! The sub file item

      INTEGER			KID			! Keyword object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search for named keyword
      CALL ADI2_HGKY( HDU, KEY, KID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    The file sub item
        SUBITM = KEY

*    More path data?
        IF ( GSUB .AND. (SUBKEY .GT. ' ') ) THEN

*      Must be COMMENT
          IF ( CHR_SIMLR( SUBKEY, 'COMMENT' ) ) THEN

*        Keyword comment exists?
            TYP = 'CHAR'
            ISSCAL = .TRUE.
            SUBITM = '&'//SUBITM

          ELSE
            CALL MSG_SETC( 'KEY', KEY )
            CALL MSG_SETC( 'ITEM', SUBKEY )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Spurious FITS path item /'/
     :                 /'^ITEM/ following valid keyword name ^KEY',
     :                  STATUS )

          END IF

*    Keyword only required
        ELSE
          ISSCAL = .TRUE.
          CALL ADI_TYPE( KID, TYP, STATUS )

        END IF

*    The file SubItem is the keyword name with a '&' prepended if
*    we're after the comment
        CALL ADI_CPUT0C( FID, 'SubItem', SUBITM, STATUS )

*    Release the keyword
        CALL ADI_ERASE( KID, STATUS )

*   Keyword not found
      ELSE
        CALL MSG_SETC( 'KEY', KEY )
        CALL ERR_REP( ' ', 'Unable to locate keyword ^KEY in'/
     :                        /' file ^FILE (HDU ^H)', STATUS )

      END IF

      END
