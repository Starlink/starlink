      SUBROUTINE WCSATTRIB( STATUS )
*+
*  Name:
*     WCSATTRIB

*  Purpose:
*     Manages attribute values associated with the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSATTRIB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application can be used to manage the values of attributes 
*     associated with the current co-ordinate Frame of an NDF (title, axis 
*     labels, axis units, etc).

*     Each attribute has a name, a value, and a state. This application 
*     accesses all attribute values as character strings, converting to 
*     and from other data types as necessary. The attribute state is a 
*     Boolean flag (i.e. TRUE or FALSE) indicating whether or not a value 
*     has been assigned to the attribute. If no value has been assigned to 
*     an attribute, then it adopts a default value until an explicit value 
*     is assigned to it. An attribute value can be cleared, causing the
*     attribute to revert to its default value.
*
*     The operation performed by this application is controlled by 
*     parameter MODE, and can be:
*
*     -  display the value of an attribute.
*     -  set a new value for an attribute.
*     -  clear an attribute value.
*     -  test the state of an attribute.

*  Usage:
*     wcsattrib ndf mode name newval

*  ADAM Parameters:
*     NAME = LITERAL (Read)
*        The attribute name
*     NDF = NDF (Read and Write)
*        The NDF to be modified.
*     MODE = LITERAL (Read)
*        The operation to be performed on the attribute specified by
*        parameter NAME: It can be one of:
*
*        - "Get" -- The current value of the attribute is displayed on the
*        screen and written to output parameter VALUE. If the attribute
*        has not yet been assigned a value (or has been cleared), then the
*        default value will be displayed.
*
*        - "Set" -- Assigns a new value, given by parameter NEWVAL, to the 
*        attribute.
*
*        - "Test" -- Displays "TRUE" if the attribute has been assigned a
*        value, and "FALSE" otherwise (in which case the attribute will
*        adopt its default value). This flag is written to the output
*        parameter STATE.
*
*        - "Clear" -- Clears the current attribute value, causing it to
*        revert to its default value.
*
*     NEWVAL = LITERAL (Read)
*        The new value to assign to the attribute.
*     STATE = LOGICAL (Write)
*        On exit, this holds the state of the attribute on entry to this 
*        application.
*     VALUE = LITERAL (Write)
*        On exit, this holds the value of the attribute on entry to this
*        application.

*  Examples:
*     wcsattrib ngc5128 set title "Polarization map of Centaurus-A"
*        This sets the Title attribute of the current co-ordinate Frame in 
*        the NDF called ngc5128 to the string "Polarization map of Centaurus-A".
*     wcsattrib my_data set domain saved_pixel
*        This sets the Domain attribute of the current co-ordinate Frame in 
*        the NDF called my_data to the string SAVED_PIXEL. 
*     wcsattrib my_data set format(1) "%10.5G"
*        This sets the Format attribute for axis 1 in the current co-ordinate 
*        Frame in the NDF called my_data, so that axis values are
*        formated as floating point values using a minimum field width of
*        10 characters, and displaying 5 significant figures. An exponent is
*        used if necessary.
*     wcsattrib ngc5128 set format(2) bdms.2
*        This sets the Format attribute for axis 2 in the current co-ordinate 
*        Frame in the NDF called ngc5128, so that axis values are formated as
*        separate degrees, minutes and seconds field, separated by blanks.
*        The seconds field has 2 decimal places. This assumes the current 
*        co-ordinate Frame in the NDF is a celestial co-ordinate Frame.
*     wcsattrib my_data get label(1)
*        This displays the label associated with the first axis of the 
*        current co-ordinate Frame in the NDF called my_data. A default
*        label is displayed if no value has been set for this attribute.
*     wcsattrib my_data test label(1)
*        This displays "TRUE" if a value has been set for the Label 
*        attribute for the first axis of the current co-ordinate Frame in 
*        the NDF called my_data, and "FALSE" otherwise.
*     wcsattrib my_data clear label(1)
*        This clears the Label attribute for the first axis of the current 
*        co-ordinate Frame in the NDF called my_data. It reverts to its
*        default value.

*  Notes:
*     -  An error is reported if an attempt is made to set or clear the 
*     Base Frame in the WCS component.

*  Related Applications:
*     KAPPA: NDFTRACE, WCSFRAME, WCSREMOVE, WCSCOPY, WCSADD

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants 
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER MODE*5           ! Required operation
      CHARACTER NAME*30          ! Attribute name
      CHARACTER NEWVAL*255       ! New attribute value
      CHARACTER VALUE*255        ! Value of the attribute on entry
      INTEGER INDF               ! NDF identifier for NDF being modified
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER LVAL               ! Length of original value
      LOGICAL STATE              ! State of the attribute on entry
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Get the WCS FrameSet associated with the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the operation to perform.
      CALL PAR_CHOIC( 'MODE', 'Get', 'Get,Set,Test,Clear', .FALSE., 
     :                MODE, STATUS )

*  Get the attribute name.
      CALL PAR_GET0C( 'NAME', NAME, STATUS )
      CALL CHR_RMBLK( NAME )
      CALL CHR_UCASE( NAME )

*  Get the current state of the attribute and assign it to the output
*  parameter STATE.
      STATE = AST_TEST( IWCS, NAME, STATUS )
      CALL PAR_PUT0L( 'STATE', STATE, STATUS )

*  Get the current value of the attribute and assign it to the output
*  parameter VALUE.
      VALUE = AST_GETC( IWCS, NAME, STATUS )
      LVAL = MAX( 1, CHR_LEN( VALUE ) )
      CALL PAR_PUT0C( 'VALUE', VALUE( : LVAL ), STATUS )

*  Branch for the four operations...

*  Get.
      IF( MODE .EQ. 'GET' ) THEN
         CALL MSG_SETC( 'VAL', VALUE )
         CALL MSG_OUT( 'WCSATTRIB_MSG1', '^VAL', STATUS )

*  Set.
      ELSE IF( MODE .EQ. 'SET' ) THEN
         IF( NAME .EQ. 'BASE' .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSATTRIB_ERR1', 'Cannot change the Base '//
     :                    'attribute of the WCS FrameSet.', STATUS )
         ELSE  
            CALL PAR_DEF0C( 'NEWVAL', VALUE( : LVAL ), STATUS )
            CALL PAR_GET0C( 'NEWVAL', NEWVAL, STATUS )
            CALL AST_SETC( IWCS, NAME, 
     :                     NEWVAL( : MAX( 1, CHR_LEN( NEWVAL ) ) ), 
     :                     STATUS )
         END IF

*  Test.
      ELSE IF( MODE .EQ. 'TEST' ) THEN
         CALL MSG_SETL( 'VAL', STATE )
         CALL MSG_OUT( 'WCSATTRIB_MSG2', '^VAL', STATUS )

*  Clear.
      ELSE
         IF( NAME .EQ. 'BASE' .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSATTRIB_ERR2', 'Cannot clear the Base '//
     :                    'attribute of the WCS FrameSet.', STATUS )
         ELSE  
            CALL AST_CLEAR( IWCS, NAME, STATUS )
         END IF

      END IF

*  Save the FrameSet in the NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSATTRIB_ERR2', 'WCSATTRIB: Failed to get, '//
     :                 'set, test or clear an attribute of an NDF '//
     :                 'WCS component.', STATUS )
      END IF

      END
