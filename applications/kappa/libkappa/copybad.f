      SUBROUTINE COPYBAD( STATUS )
*+
*  Name:
*     COPYBAD 

*  Purpose:
*     Copies bad pixels from one NDF file to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COPYBAD( STATUS )

*  Description:
*     This application copies bad pixels from one NDF file to another. It
*     takes in two NDFs (parameters IN and REF), and creates a third
*     (parameter OUT) which is a copy of IN, except that any pixel which
*     is set bad in the DATA array of REF, is also set bad in the DATA 
*     and VARIANCE (if available) arrays in OUT.

*  Usage:
*     copybad in ref out title

*  ADAM Parameters:
*     IN = NDF (Read)
*        NDF containing the data to be copied to OUT.
*     REF = NDF (Read)
*        NDF containing the bad pixels to be copied to OUT.
*     OUT = NDF (Write)
*        The output NDF.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     copybad in=a ref=b out=c title="New image"
*        This creates a NDF called c, which is a copy of the NDF called a. 
*        Any bad pixels present in the NDF called b are copied into the 
*        corresponding positions in c (non-bad pixels in b are ignored). 
*        The title of c is "New image".

*  Notes:
*     - If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being processed.  An error 
*     will result if they have no pixels in common.

*  Related Applications:
*     KAPPA: SUBSTITUTE, NOMAGIC, FILLBAD, PASTE, GLITCH

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, HISTORY, and VARIANCE components of an NDF data
*     structure and propagates all extensions.
*     -  All non-complex numeric data types can be handled.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*    TDCA: Tim D.C. Ash(STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1998 (TDCA):
*        Original version, partially based on the KAPPA routine ADD
*     13-OCT-1998 (DSB):
*        - Tidied up the prologue and code a touch. 
*        - Corrected distinction between the data type in which values are 
*          processed, and the data type in which values are stored in the 
*          output NDF.
*        - Use ERR_REP in place of MSG_OUT to report an error if an
*          unrecognised data type is obtained. Also reformat the error
*          report to include the unrecognised data type, and a friendly
*          message to explain to the user that the problem is not of their
*          doing.
*        - Changes the KPS routine names to use a consistent root
*          ("kps1_cpb").
*        - Replaced the message which reports the number of bad pixels
*          copied, with a message reporting the number of bad pixels in
*          the output NDF. This is easier for a user to understand.
*        - Removed un-required initialisation of NBAD.
*        - Make the grammar of the NBAD message dependant on the value of
*          NBAD, and include the name of the output NDF.
*        - Sort local variable declarations into alphabetical order.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'NDF_PAR'                 ! NDF_ public constants

*  Status:
      INTEGER STATUS                    ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) TY_IN  ! Data type for processing
      CHARACTER * ( NDF__SZTYP ) TY_OUT ! Data type for output NDF
      INTEGER NEL                       ! Number of mapped elements
      INTEGER IN                        ! Identifier for IN (input)
      INTEGER NBAD                      ! Number of bad pixels in output
      INTEGER OUT                       ! Identifier for OUT (output)
      INTEGER P_OUT                     ! Pointer to OUT's data array
      INTEGER P_OUTV                    ! Pointer to OUT's variance array
      INTEGER P_REF                     ! Pointer to REF's data array
      INTEGER REF                       ! Identifier for REF (input)
      LOGICAL BAD                       ! Bad pixels present ?
      LOGICAL VAR                       ! Varience array present in IN ?

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifiers for the two input NDFs.
      CALL NDG_ASSOCL( 'IN', 'READ', IN, STATUS )
      CALL NDG_ASSOCL( 'REF', 'READ', REF, STATUS )

*  Trim the input pixel-index bounds to match.
      CALL NDF_MBND( 'TRIM', IN, REF, STATUS )

*  Create a new output NDF based on the first input NDF. Propagate the
*  data, WCS, and axis components. (Use if neither variance or quality 
*  arrays are present.)
      CALL NDG_PROPL( IN, 'Data,WCS,Axis,Variance,Quality', 'OUT', OUT, 
     :               STATUS )

*  Determine which data type to use to process the input data/variance
*  arrays. Also find and set an appropriate data type for these components 
*  in the output NDF.
      CALL NDF_MTYPE( '_BYTE,_WORD,_UBYTE,_UWORD,_INTEGER, _REAL,'//
     :                '_DOUBLE', IN, REF, 'Data,Variance', TY_IN, 
     :                TY_OUT, STATUS )
      CALL NDF_STYPE( TY_OUT, OUT, 'Data,Variance', STATUS )

* Map the NDF DATA arrays.
      CALL NDF_MAP( REF, 'Data', TY_IN, 'READ', P_REF, NEL, STATUS )
      CALL NDF_MAP( OUT, 'Data', TY_IN, 'WRITE', P_OUT, NEL, STATUS )
      
*  If required, map the VARIANCE array.
      CALL NDF_STATE( IN, 'Variance', VAR, STATUS )
      IF( VAR ) CALL NDF_MAP( OUT, 'Variance', TY_IN, 'WRITE', P_OUTV, 
     :                        NEL, STATUS )

*  Select the appropriate routine to copy the bad pixels.
      IF ( TY_IN .EQ. '_INTEGER' ) THEN
         CALL KPS1_CPBI( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
         CALL KPS1_CPBR( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
         CALL KPS1_CPBD( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_BYTE' ) THEN
         CALL KPS1_CPBB( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_UBYTE' ) THEN
         CALL KPS1_CPBUB( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_WORD' ) THEN
         CALL KPS1_CPBW( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_UWORD' ) THEN
         CALL KPS1_CPBUW( NEL, VAR, %VAL( P_REF ), %VAL( P_OUT ), 
     :                   %VAL( P_OUTV ), NBAD, STATUS )

      ELSE 
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TY', TY_IN )
         CALL ERR_REP( 'COPYBAD_UTYP', 'Unsupported data type '//
     :                 '''^TY'' (programming error).', STATUS )
      END IF

*  Display a blank line to highlight the following message.
      CALL MSG_BLANK( STATUS )

*  Report the number of pixels in the output NDF.
      CALL NDF_MSG( 'NDF', OUT )
      IF( NBAD .EQ. 0 ) THEN
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There are no bad pixels in '//
     :                 'the output NDF ''^NDF''.', STATUS)

      ELSE IF( NBAD .EQ. 1 ) THEN
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There is 1 bad pixel in the'//
     :                 ' output NDF ''^NDF''.', STATUS)

      ELSE
         CALL MSG_SETI( 'NBAD', NBAD )
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There are ^NBAD bad pixels '//
     :                 'in the output NDF ''^NDF''.', STATUS)
      END IF

*  Display a blank line to highlight the previous message.
      CALL MSG_BLANK( STATUS )

*  Obtain the output title and insert it into the output NDF.
      CALL NDF_CINP( 'TITLE', OUT, 'Title', STATUS )
  
*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COPYBAD_ERR', 'COPYBAD: Error copying bad '//
     :                 'pixels.', STATUS )
      END IF

      END 
