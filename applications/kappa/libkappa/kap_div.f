      SUBROUTINE KAP_DIV( STATUS )
*+
*  Name:
*     DIV

*  Purpose:
*     Divides one NDF data structure by another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DIV( STATUS )

*  Description:
*     The routine divides one NDF data structure by another
*     pixel-by-pixel to produce a new NDF.

*  Usage:
*     div in1 in2 out

*  ADAM Parameters:
*     IN1 = NDF (Read)
*        First NDF, to be divided by the second NDF.
*     IN2 = NDF (Read)
*        Second NDF, to be divided into the first NDF.
*     OUT = NDF (Write)
*        Output NDF to contain the ratio of the two input NDFs.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN1 to be used
*        instead. [!]

*  Examples:
*     div a b c
*        This divides the NDF called a by the NDF called b, to make the
*        NDF called c.  NDF c inherits its title from a.
*     div out=c in1=a in2=b title="Normalised data"
*        This divides the NDF called a by the NDF called b, to make the
*        NDF called c.  NDF c has the title "Normalised data".

*  Notes:
*     If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being divided.  An error will
*     result if they have no pixels in common.

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CMULT, CSUB, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, HISTORY, WCS and VARIANCE components of an NDF data
*     structure and propagates all extensions.
*     -  Units processing is not supported at present and therefore the
*     UNITS component is not propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     Calculations will be performed using either real or double
*     precision arithmetic, whichever is more appropriate.  If the
*     input NDF structures contain values with other data types, then
*     conversion will be performed as necessary.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-1990 (RFWS):
*        Original version.
*     1992 January 15 (MJC):
*        Added Usage and Examples items.
*     1995 September 12 (MJC):
*        Title inherited by default.  Usage and examples to lowercase.
*        Added Related Applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     23-JUN-1998 (DSB):
*        Used KPG1_MAP instead of NDF_MAP, so that NaN and Inf values
*        are converted to Starlink BAD values before being used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 13 ) COMP    ! NDF component list
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for output components
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER EL                 ! Number of mapped elements
      INTEGER NDF1               ! Identifier for 1st NDF (input)
      INTEGER NDF2               ! Identifier for 2nd NDF (input)
      INTEGER NDF3               ! Identifier for 3rd NDF (output)
      INTEGER NERR               ! Number of errors
      INTEGER PNTR1( 2 )         ! Pointers to 1st NDF mapped arrays
      INTEGER PNTR2( 2 )         ! Pointers to 2nd NDF mapped arrays
      INTEGER PNTR3( 2 )         ! Pointers to 3rd NDF mapped arrays
      LOGICAL BAD                ! Need to check for bad pixels?
      LOGICAL VAR                ! Process variance?
      LOGICAL VAR1               ! Variance component in 1st input NDF?
      LOGICAL VAR2               ! Variance component in 2nd input NDF?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifiers for the two input NDFs.
      CALL NDF_ASSOC( 'IN1', 'READ', NDF1, STATUS )
      CALL NDF_ASSOC( 'IN2', 'READ', NDF2, STATUS )

*  Trim the input pixel-index bounds to match.
      CALL NDF_MBND( 'TRIM', NDF1, NDF2, STATUS )

*  Create a new output NDF based on the first input NDF. Propagate the
*  WCS, axis and quality components.
      CALL NDF_PROP( NDF1, 'WCS,Axis,Quality', 'OUT', NDF3, STATUS )

*  See whether a variance component is defined in both the input NDFs
*  and set the list of components to be processed accordingly.
      CALL NDF_STATE( NDF1, 'Variance', VAR1, STATUS )
      CALL NDF_STATE( NDF2, 'Variance', VAR2, STATUS )
      VAR = VAR1 .AND. VAR2
      IF ( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine which data type to use to process the input arrays and set
*  an appropriate data type in the output NDF.
      CALL NDF_MTYPE('_REAL,_DOUBLE',
     :                NDF1, NDF2, COMP, ITYPE, DTYPE, STATUS )
      CALL NDF_STYPE( DTYPE, NDF3, COMP, STATUS )

*  Map the input and output arrays.
      CALL KPG1_MAP( NDF1, COMP, ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDF2, COMP, ITYPE, 'READ', PNTR2, EL, STATUS )
      CALL KPG1_MAP( NDF3, COMP, ITYPE, 'WRITE', PNTR3, EL, STATUS )

*  Merge the bad pixel flag values for the input arrays to see if
*  checks for bad pixels are needed.
      CALL NDF_MBAD( .TRUE., NDF1, NDF2, COMP, .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  divide the arrays.

*  Real data...
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_DIVR( BAD, VAR, EL,
     :                   %VAL( PNTR1( 1 ) ), %VAL( PNTR1( 2 ) ),
     :                   %VAL( PNTR2( 1 ) ), %VAL( PNTR2( 2 ) ),
     :                   %VAL( PNTR3( 1 ) ), %VAL( PNTR3( 2 ) ),
     :                   NERR, STATUS )

*  Double precision data...
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_DIVD( BAD, VAR, EL,
     :                   %VAL( PNTR1( 1 ) ), %VAL( PNTR1( 2 ) ),
     :                   %VAL( PNTR2( 1 ) ), %VAL( PNTR2( 2 ) ),
     :                   %VAL( PNTR3( 1 ) ), %VAL( PNTR3( 2 ) ),
     :                   NERR, STATUS )
      END IF

*  See if there may be bad pixels in the output arrays and set the
*  output bad pixel flag value accordingly unless the output NDF is
*  primitive. (Assume variance is in the same form as the data array.)
      BAD = BAD .OR. ( NERR .NE. 0 )
      CALL NDF_FORM( NDF3, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, NDF3, 'Data', STATUS )
      END IF

*  Obtain a title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF3, 'Title', STATUS )
      
*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DIV_ERR',
     :   'DIV: Error dividing two NDF data structures.', STATUS )
      END IF

      END
