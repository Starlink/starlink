      SUBROUTINE EDI2_SETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI2_SETLNK

*  Purpose:
*     Service SetLink method for EventDS to FITSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI2_SETLNK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Establishes ADI file link between high level objects EventDS and
*     its derivatives, and FITSfile.

*  Arguments:
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

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
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*6		TSTRING
        PARAMETER               ( TSTRING = 'BIJEDC' )

*  Local Variables:
      CHARACTER*6		ETABLE			! Extension for events
      CHARACTER*40		NAME			! Column name
      CHARACTER*20		TYPE			! Column type
      CHARACTER*40		UNITS			! Column name

      DOUBLE PRECISION		DMAX, DMIN		! Field extrema

      INTEGER			DIMS(ADI__MXDIM)	! List dimensions
      INTEGER			EVHDU			! EVENTS hdu
      INTEGER			IC			! Character ptr
      INTEGER			ILIST			! Loop over lists
      INTEGER			L			! Use length of NAME
      INTEGER			LID			! Lists object id
      INTEGER			MAXID, MINID		! Extrema values
      INTEGER			NDIM			! List dimensionality
      INTEGER			NEVENT			! Number of records
      INTEGER			NLIST			! Number of lists
      INTEGER			TPOS			! Type code index

      LOGICAL			GOTMAX, GOTMIN		! Got extrema?
      LOGICAL			RDF			! RDF data?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID
      RDF = .FALSE.

*  Try to locate the extension containing events. If no HDU has been
*  supplied search for EVENTS or STDEVT, otherwise use main HDU
      CALL ADI_CGET0I( ARGS(2), 'UserHDU', UIHDU, STATUS )
      IF ( UIHDU .GT. 0 ) THEN
        CALL ADI2_FNDHDU( ARGS(2), ' ', EVHDU, STATUS )
        ETABLE = '      '
      ELSE
        CALL ADI2_FNDHDU( ARGS(2), 'EVENTS', EVHDU, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL ADI2_FNDHDU( ARGS(2), 'STDEVT', EVHDU, STATUS )
          ETABLE = 'STDEVT'
        ELSE
          ETABLE = 'EVENTS'
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Write the event extension name as property
      CALL ADI_CPUT0C( ARGS(2), '.Etable', ETABLE, STATUS )

*  Read the keywords defining the number of events and columns
      CALL ADI2_HGKYI( EVHDU, 'TFIELDS', NLIST, STATUS )
      CALL ADI2_HGKYI( EVHDU, 'NAXIS2', NEVENT, STATUS )

*  Get number of top level components
      DO ILIST = 1, NLIST

*    Read keyword containing name and description of field
        CALL ADI2_HGKYIC( EVHDU, 'TTYPE', ILIST, NAME, STATUS )
        L = CHR_LEN(NAME)

*    Read table item describing the type of the data stored
        CALL ADI2_HGKYIC( EVHDU, 'TFORM', ILIST, TYPE, STATUS )

*    Translate type string to dimensions and ADI type. The format is
*    [<dims>]<tcode> where <dims> is some number and <tcode> is one of
*    TSTRING
        NDIM = 0
        TPOS = INDEX(TSTRING,TYPE(1:1))
        IF ( TPOS .EQ. 0 ) THEN
          IC = 2
          DO WHILE ( TPOS .EQ. 0 )
            TPOS = INDEX(TSTRING,TYPE(IC:IC))
            IF ( TPOS .EQ. 0 ) IC = IC + 1
          END DO
          CALL CHR_CTOI( TYPE(1:IC-1), DIMS(1), STATUS )
          IF ( DIMS(1) .GT. 1 ) NDIM = 1
        END IF
        IF ( TPOS .EQ. 1 ) THEN
          TYPE = 'BYTE'
        ELSE IF ( TPOS .EQ. 2 ) THEN
          TYPE = 'WORD'
        ELSE IF ( TPOS .EQ. 3 ) THEN
          TYPE = 'INTEGER'
        ELSE IF ( TPOS .EQ. 4 ) THEN
          TYPE = 'REAL'
        ELSE IF ( TPOS .EQ. 5 ) THEN
          TYPE = 'DOUBLE'
        ELSE IF ( TPOS .EQ. 6 ) THEN
          TYPE = 'CHAR'
        END IF

*    Now the optional items
        CALL ADI2_HGKYIC( EVHDU, 'TUNIT', ILIST, UNITS, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          UNITS = ' '
        END IF

        CALL ADI2_HGKYID( EVHDU, 'TLMIN', ILIST, DMIN, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          GOTMIN = .FALSE.
        ELSE
          GOTMIN = .TRUE.
        END IF
        CALL ADI2_HGKYID( EVHDU, 'TLMAX', ILIST, DMAX, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          GOTMAX = .FALSE.
        ELSE
          GOTMAX = .TRUE.
        END IF

*    Increment list counter
        NLIST = NLIST + 1

*    Create new list descriptor
        CALL ADI_NEW0( 'EventList', LID, STATUS )

*    Write list name
        CALL ADI_CPUT0C( LID, 'Name', NAME(:L), STATUS )

*    Write shape data
        IF ( NDIM .GT. 1 ) THEN
          CALL ADI_CPUT1I( LID, 'SHAPE', NDIM-1, DIMS, STATUS )
        END IF

*    Write the type
        CALL ADI_CPUT0C( LID, 'TYPE', TYPE(:CHR_LEN(TYPE)), STATUS )

*    Write extrema
        IF ( GOTMIN ) THEN
          CALL ADI_NEW0( TYPE, MINID, STATUS )
          CALL ADI_PUT0D( MINID, DMIN, STATUS )
          CALL ADI_CPUTID( LID, 'Min', MINID, STATUS )
        END IF
        IF ( GOTMAX ) THEN
          CALL ADI_NEW0( TYPE, MAXID, STATUS )
          CALL ADI_PUT0D( MAXID, DMAX, STATUS )
          CALL ADI_CPUTID( LID, 'Max', MAXID, STATUS )
        END IF

*    Write units
        IF ( UNITS .GT. ' ' ) THEN
          CALL ADI_CPUT0C( LID, 'Units', UNITS(:CHR_LEN(UNITS)),
     :                     STATUS )
        END IF

*    Update list description
        CALL EDI0_UPDLD( ARGS(1), LID, STATUS )

*  Next component
      END DO

*  Write class members
      CALL ADI_CPUT0I( ARGS(1), 'NEVENT', NEVENT, STATUS )

*  Release HDU
      CALL ADI_ERASE( EVHDU, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI2_SETLNK', STATUS )

*  Invoke base method to perform linkage
      CALL ADI_SETLNK( ARGS(1), ARGS(2), STATUS )

      END
