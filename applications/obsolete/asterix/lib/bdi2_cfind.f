      SUBROUTINE BDI2_CFIND( MID, FITID, ITEM, CREATE, DELETE, CACHEID,
     :                       CNDIM, CDIMS, STATUS )
*+
*  Name:
*     BDI2_CFIND

*  Purpose:
*     Locate FITS component for a given item, creating/deleting if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_CFIND( MID, FITID, ITEM, CREATE, DELETE, CACHEID, CNDIM, CDIMS, STATUS )

*  Description:
*     Locate FITS component in the object cache for a given item, creating if
*     required. If the object does not exist and creation is not allowed then
*     CACHEID is set to a flag value. The routine returns the shape of the
*     object, whether or not it is created, which is defined by the NDF data
*     model.

*  Arguments:
*     MID = INTEGER (given)
*        Model data object
*     FITID = INTEGER (given)
*        FITSfile data object
*     ITEM = CHARACTER*(*) (given)
*        BDI data item
*     CREATE = LOGICAL (given)
*        Create structures if they don't exist?
*     DELETE = LOGICAL (given)
*        Delete named item?
*     CACHEID = INTEGER (returned)
*        Identifier to ADI object handling the FITS data. If the item does
*        not exist then CACHEID is set to the symbolic value ADI__NULLID
*     CNDIM = INTEGER (returned)
*        The dimensionality of the object according to the data model. Note
*        that this not necessarily the dimensionality of the actual HDS
*        component
*     CDIMS[] = INTEGER (returned)
*        The dimensions of the object according to the data model. Note
*        that these are not necessarily the dimensions of the actual HDS
*        component
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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      3 Jun 1996 (DJA):
*        General purpose FITS version derived from BDI1_CFIND
*     30 Jan 1997 (RB):
*        Hunt for the axis units and alter for ROSAT images
*     11 Feb 1997 (RB):
*        Axis_n_Width and QUALITY extensions
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
      INTEGER			MID, FITID
      CHARACTER*(*)		ITEM
      LOGICAL			CREATE, DELETE

*  Arguments Returned:
      INTEGER			CACHEID, CNDIM, CDIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET
      EXTERNAL			CHR_LEN
	INTEGER			CHR_LEN
      EXTERNAL			STR_SUB
        LOGICAL			STR_SUB

*  Local Variables:
      CHARACTER*1		CAX			! Axis digit character
      CHARACTER*40		CRECOM			! Keyword comment
      CHARACTER*40		UNITS			! Axis units if need changing
      CHARACTER*15		CREOBJ			! Created object
      CHARACTER*15		RTYPE, TYPE		! Data types

      INTEGER			DIMS(ADI__MXDIM)	! Dimensions
      INTEGER			NDIM			! Dimensionality

      LOGICAL			DIDCRE			! Did we create cache
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      CACHEID = ADI__NULLID

*  Get model dimensions
      CALL ADI_THERE( MID, 'SHAPE', THERE, STATUS )
      IF ( THERE ) THEN
        CALL BDI_GETSHP( MID, ADI__MXDIM, DIMS, NDIM, STATUS )
      ELSE
        NDIM = -1
      END IF

*  Get dimensions and basic type in create mode. If no values replace
*  with nulls in the hope that we can get away with it! These nulls
*  must be trapped by BDI2_CFIND1
      IF ( CREATE ) THEN
        CALL BDI_GETTYP( MID, TYPE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          TYPE = '*unknown*'
        END IF

*    Derive the type for axes and errors. This must be floating point.
*    Use single precision normally, but double if that's the main
*    dataset type
        IF ( TYPE .EQ. 'DOUBLE' ) THEN
          RTYPE = TYPE
        ELSE
          RTYPE = 'REAL'
        END IF

      END IF

*  Top-level data array
      IF ( ITEM .EQ. 'Data' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access primary array
        CALL ADI2_CFIND( FITID, ' ', '@', ' ', CREATE, DELETE, TYPE,
     :                   NDIM, DIMS, DIDCRE, CACHEID, STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'DATA'

*  Top-level variance
      ELSE IF ( ITEM .EQ. 'Variance' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access variance
        CALL ADI2_CFIND( FITID, 'VARIANCE', '@', ' ', CREATE,
     :                   DELETE, TYPE, NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'VARIANCE'

*  Top-level quality
      ELSE IF ( ITEM .EQ. 'Quality' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access quality
        CALL ADI2_CFIND( FITID, 'QUALITY', '@', ' ', CREATE,
     :                   DELETE, 'WORD', NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'QUALITY'

*  Top-level logical quality
      ELSE IF ( ITEM .EQ. 'LogicalQuality' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access quality
        CALL ADI2_CFIND( FITID, 'QUALITY', '@', ' ', CREATE,
     :                   DELETE, 'WORD', NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'QUALITY'

*  Lower error
      ELSE IF ( ITEM .EQ. 'LoError' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access low error - ensure its a floating type
        CALL ADI2_CFIND( FITID, 'LO_ERROR', '@', ' ', CREATE,
     :                   DELETE, RTYPE, NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'LO_ERROR'

*  Upper error
      ELSE IF ( ITEM .EQ. 'HiError' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access high error - ensure its a floating type
        CALL ADI2_CFIND( FITID, 'HI_ERROR', '@', ' ', CREATE,
     :                   DELETE, RTYPE, NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'HI_ERROR'

*  Grouping
      ELSE IF ( ITEM .EQ. 'Grouping' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Access grouping
        CALL ADI2_CFIND( FITID, 'GROUPING', '@', ' ', CREATE,
     :                   DELETE, 'INTEGER', NDIM, DIMS, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the array
        IF ( DIDCRE ) CREOBJ = 'GROUPING'

*  Quality mask
      ELSE IF ( ITEM .EQ. 'QualityMask' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

*    Access keyword
        CALL ADI2_CFIND( FITID, 'QUALITY', '.QMASK', ' ', CREATE,
     :                   DELETE, 'INTEGER', 0, 0, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the keyword
*    And also assing a value of '11111111' to the mask (RB).
        IF ( DIDCRE ) THEN
          CREOBJ = 'QMASK'
          CRECOM = 'Data quality mask'
          CALL ADI_CPUT0I( CACHEID, 'Value', 255, STATUS )
        END IF

*  Top level title
      ELSE IF ( ITEM .EQ. 'Title' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

*    Access keyword
        CALL ADI2_CFIND( FITID, ' ', '.TITLE', ' ', CREATE,
     :                   DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                   STATUS )
        IF ( CACHEID .EQ. ADI__NULLID ) THEN
        CALL ADI2_CFIND( FITID, ' ', '.OBJECT', ' ', CREATE,
     :                   DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                   STATUS )
        END IF

*    Add extra info if we created the keyword
        IF ( DIDCRE ) THEN
          CREOBJ = 'TITLE'
          CRECOM = 'Dataset title'
        END IF

*  Top level units
      ELSE IF ( ITEM .EQ. 'Units' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

*    Access keyword
        CALL ADI2_CFIND( FITID, ' ', '.UNITS', ' ', CREATE,
     :                   DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the keyword
        IF ( DIDCRE ) THEN
          CREOBJ = 'TITLE'
          CRECOM = 'Data units'
        END IF

*  Top level label
      ELSE IF ( ITEM .EQ. 'Label' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

*    Access keyword
        CALL ADI2_CFIND( FITID, ' ', '.LABEL', ' ', CREATE,
     :                   DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                   STATUS )

*    Add extra info if we created the keyword
        IF ( DIDCRE ) THEN
          CREOBJ = 'TITLE'
          CRECOM = 'Data label'
        END IF

*  Axis stuff
      ELSE IF ( ITEM(1:5) .EQ. 'Axis_' ) THEN

*    Axis character
        CAX = ITEM(6:6)

*    Axis units
        IF ( ITEM(8:) .EQ. 'Units' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.AUNIT'//CAX, ' ', CREATE,
     :                     DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                     STATUS )

*      Try the next best option...
          IF ( CACHEID .EQ. ADI__NULLID ) THEN
            CALL ADI2_CFIND( FITID, ' ', '.CDELT'//CAX, ' ', CREATE,
     :                       DELETE, 'REAL', 0, 0, DIDCRE, CACHEID,
     :                       STATUS )

*        Now try to guess the units
            IF ( CACHEID .NE. ADI__NULLID ) THEN
              CALL ADI_THERE( CACHEID, 'Comment', THERE, STATUS)
              IF ( THERE ) THEN
                CALL ADI_CGET0C( CACHEID, 'Comment', UNITS, STATUS )
                IF ( STR_SUB( 'deg', UNITS ) ) THEN
                  UNITS = 'degrees'
                ELSE IF ( STR_SUB( 'arcm', UNITS ) ) THEN
                  UNITS = 'arcminutes'
                ELSE IF ( STR_SUB( 'arcs', UNITS ) ) THEN
                  UNITS = 'arcseconds'
                ELSE IF ( STR_SUB( 'pix', UNITS ) ) THEN
                  UNITS = 'pixels'
                ELSE
                  UNITS = 'degrees'
                END IF
              ELSE
                UNITS = 'degrees'
              END IF

*        Erase old value and add new character string
              CALL ADI_CERASE( CACHEID, 'Value', STATUS )
              CALL ADI_CNEWV0C( CACHEID, 'Value', UNITS, STATUS )
            ELSE

*        Try for a plate type object
              CALL ADI2_CFIND( FITID, ' ', '.PLTSCALE', ' ', CREATE,
     :                         DELETE, 'REAL', 0, 0, DIDCRE, CACHEID,
     :                         STATUS )
              IF ( CACHEID .NE. ADI__NULLID ) THEN
                CALL ADI_CERASE( CACHEID, 'Value', STATUS )
                CALL ADI_CNEWV0C( CACHEID, 'Value', 'degrees', STATUS )
              ELSE

*        Need a new cache object for default units string
                CALL ADI2_CFIND( FITID, ' ', '.BITPIX', ' ', CREATE,
     :                           DELETE, 'INTEGER', 0, 0, DIDCRE,
     :                           CACHEID, STATUS )
                CALL ADI_CERASE( CACHEID, 'Value', STATUS )
                CALL ADI_CNEWV0C( CACHEID, 'Value', 'pixels', STATUS )
              END IF
            END IF
          END IF

*      Add extra info if we created the keyword
          IF ( DIDCRE ) THEN
            CREOBJ = 'AUNIT'
            CRECOM = 'Axis '//CAX//' units'
            CALL ADI_CPUT0C( CACHEID, 'Value', ' ', STATUS )
          END IF

*    Axis label
        ELSE IF ( ITEM(8:) .EQ. 'Label' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.ALABEL'//CAX, ' ', CREATE,
     :                     DELETE, 'CHAR', 0, 0, DIDCRE, CACHEID,
     :                     STATUS )

*      Add extra info if we created the keyword
          IF ( DIDCRE ) THEN
            CREOBJ = 'ALABEL'
            CRECOM = 'Axis '//CAX//' label'
            CALL ADI_CPUT0C( CACHEID, 'Value', ' ', STATUS )
          END IF

* added for BDI_AXCHK in GDRAW (start rb)
*    Axis scalling
        ELSE IF ( ITEM(8:) .EQ. 'Data' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.CDELT'//CAX, ' ', CREATE,
     :                     DELETE, 'REAL', 0, 0, DIDCRE, CACHEID,
     :                     STATUS )
          IF ( CACHEID .EQ. ADI__NULLID ) THEN
            CALL ADI2_CFIND( FITID, ' ', '.CD'//CAX//'_'//CAX, ' ',
     :                       CREATE, DELETE, 'REAL', 0, 0, DIDCRE,
     :                       CACHEID, STATUS )
          END IF
          IF ( CACHEID .EQ. ADI__NULLID ) THEN
            CALL ADI2_CFIND( FITID, ' ', '.PLTSCALE', ' ',
     :                       CREATE, DELETE, 'REAL', 0, 0, DIDCRE,
     :                       CACHEID, STATUS )
          END IF

*      Add extra info if we created the keyword
          IF ( DIDCRE ) THEN
            CREOBJ = 'DATA'
            IF ( CAX .EQ. '1' ) THEN
              CRECOM = 'X degrees per pixel'
            ELSE IF (CAX .EQ. '2') THEN
              CRECOM = 'Y degrees per pixel'
            END IF
          END IF

*    Spaced data
        ELSE IF ( ITEM(8:) .EQ. 'SpacedData' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.CDELT'//CAX, ' ', CREATE,
     :                     DELETE, RTYPE, 0, 0, DIDCRE, CACHEID,
     :                     STATUS )

*    Axis widths
        ELSE IF ( ITEM(8:) .EQ. 'Width' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.CDELT'//CAX, ' ', CREATE,
     :                     DELETE, RTYPE, 0, 0, DIDCRE, CACHEID,
     :                     STATUS )

*      Add extra info if we created the keyword
          IF ( DIDCRE ) THEN
            CALL ADI_CPUT0I( CACHEID, 'Value', 1, STATUS )
            CALL ADI_CPUT0C( CACHEID, 'Comment',
     :                       'Axis '//CAX//' spacing', STATUS )
          END IF

*    Axis normalisation flag (end rb)
        ELSE IF ( ITEM(8:) .EQ. 'Normalised' ) THEN

*      Access keyword
          CALL ADI2_CFIND( FITID, ' ', '.ANORM'//CAX, ' ', CREATE,
     :                     DELETE, 'LOGICAL', 0, 0, DIDCRE, CACHEID,
     :                     STATUS )

*      Add extra info if we created the keyword
          IF ( DIDCRE ) THEN
            CREOBJ = 'ANORM'
            CRECOM = 'Axis '//CAX//' normalised?'
            CALL ADI_CPUT0L( CACHEID, 'Value', .FALSE., STATUS )
          END IF

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'IT', ITEM )
          CALL ERR_REP( 'BDI2_CFIND', 'Unrecognised BDI axis data '/
     :                  /'item ^IT', STATUS )
        END IF

*  Otherwise an error
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'IT', ITEM )
        CALL ERR_REP( 'BDI2_CFIND', 'Unrecognised BDI data item '/
     :                /'^IT', STATUS )

      END IF

*  Add extra info for created objects?
      IF ( DIDCRE .AND. (STATUS.EQ.SAI__OK) ) THEN

*    VARIANCE extension
        IF ( CREOBJ .EQ. 'VARIANCE' ) THEN
          CALL ADI2_FPKYC( FITID, 'VARIANCE', 'EXTNAME', 'VARIANCE',
     :                     'Contains ASTERIX pixel variances', STATUS )

*    QUALITY extension
        ELSE IF ( CREOBJ .EQ. 'QUALITY' ) THEN
          CALL ADI2_FPKYC( FITID, 'QUALITY', 'EXTNAME', 'QUALITY',
     :                     'Contains ASTERIX pixel quality', STATUS )

*    GROUPING extension
        ELSE IF ( CREOBJ .EQ. 'GROUPING' ) THEN
          CALL ADI2_FPKYC( FITID, 'GROUPING', 'EXTNAME', 'VARIANCE',
     :             'Contains ASTERIX pixel group membership', STATUS )

*    LO_ERROR extension
        ELSE IF ( CREOBJ .EQ. 'LO_ERROR' ) THEN
          CALL ADI2_FPKYC( FITID, 'LO_ERROR', 'EXTNAME', 'LO_ERROR',
     :             'Contains ASTERIX pixel lower errors', STATUS )

*    HI_ERROR extension
        ELSE IF ( CREOBJ .EQ. 'HI_ERROR' ) THEN
          CALL ADI2_FPKYC( FITID, 'HI_ERROR', 'EXTNAME', 'HI_ERROR',
     :             'Contains ASTERIX pixel upper errors', STATUS )

*    Keyword creation? Write comment
        ELSE IF ( CHR_INSET( 'TITLE,UNITS,LABEL,ANORM,'/
     :                   /'AUNIT,ALABEL,QMASK', CREOBJ ) ) THEN
          CALL ADI_CPUT0C( CACHEID, 'Comment',
     :                     CRECOM(:CHR_LEN(CRECOM)), STATUS )

        END IF

      END IF

*  If bad status nullify identifier
      IF ( STATUS .NE. SAI__OK ) CACHEID = ADI__NULLID

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_CFIND', STATUS )

      END
