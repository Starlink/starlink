      SUBROUTINE Q2BAD( STATUS )
*+
*  Name:
*     Q2BAD

*  Purpose:
*     Converts an NDF's quality into bad values.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL Q2BAD( STATUS )

*  Description:
*     The routine converts an NDF's quality information into bad values.
*     There is no QUALITY structure in the output.
*     This is a temporary measure required whilst Figaro cannot handle
*     NDFs with both a QUALITY structure and flagged values.

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure.
*     OUT = NDF (Write)
*        Output NDF data structure.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-May-1991 (JM):
*        Original version.
*     02-JUL-1991 (HME):
*        Get quality as logical array.
*     13-OCT-1992 (HME):
*        Map quality before data. Rename NDFBAD -> Q2BAD.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP )
     :   ITYPE                   ! Data type for processing
      INTEGER EL                 ! Number of mapped elements
      INTEGER NDF1               ! Identifier for 1st NDF (input)
      INTEGER NDF2               ! Identifier for 2nd NDF (output)
      INTEGER PNTR1( 2 )         ! Pointer to 1st NDF mapped arrays
      INTEGER PNTR2( 1 )         ! Pointer to 2nd NDF mapped array
      BYTE BADXST                ! False if quality indic. no bad data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDF1, STATUS )

*  Create a new output NDF based on the input NDF. Propagate the axis,
*  quality, units and variance components.
      CALL NDF_PROP( NDF1, 'Axis,Units,Variance', 'OUT', NDF2, STATUS )

*  Map the quality information as a logical array.
*  .TRUE. good, .FALSE. bad.
*  If BADXST is returned .FALSE. all array elements are .TRUE. (good).
*  This mapping must be done before the corresponding data are mapped.
      CALL NDF_MAPQL( NDF1, PNTR1(2), EL, BADXST, STATUS )

*  Determine which data type to use to process the input data array.
*  Any non-DOUBLE reverts to REAL.
      CALL NDF_TYPE( NDF1, 'Data', ITYPE, STATUS )
      IF ( ITYPE .NE. '_DOUBLE' ) ITYPE = '_REAL'

*  Map the input and output data arrays. Output initialised as bad.
      CALL NDF_MAP( NDF1, 'Data', ITYPE, 'READ', PNTR1(1), EL, STATUS )
      CALL NDF_MAP( NDF2, 'Data', ITYPE, 'WRITE/BAD', PNTR2(1), EL,
     :   STATUS )

*  Do the action.
      IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL FIG_QLBADD( EL, BADXST, %VAL(CNF_PVAL(PNTR1(2))),
     :                    %VAL(CNF_PVAL(PNTR1(1))),
     :                    %VAL(CNF_PVAL(PNTR2(1))), STATUS )
      ELSE
         CALL FIG_QLBADR( EL, BADXST, %VAL(CNF_PVAL(PNTR1(2))),
     :                    %VAL(CNF_PVAL(PNTR1(1))),
     :                    %VAL(CNF_PVAL(PNTR2(1))), STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'Q2BAD_ERR',
     :   'Q2BAD: Error creating modified NDF data structure.',
     :   STATUS )
      END IF

      END
