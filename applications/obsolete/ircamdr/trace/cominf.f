*+ COMINF - Write out information concerning a component of a structure.
      SUBROUTINE COMINF( COMLOC, INDENT, NAME, PRIM, SIZE, NDIM, DIMS,
     :  STATUS )
*    Description :
*     Obtains information on the structure component with locator COMLOC
*     and formats it into a message indented by INDENT speces. The
*     information obtained is returned to the calling routine.
*    Invocation :
*     CALL COMINF( COMLOC, INDENT, NAME, PRIM, SIZE, NDIM, DIMS, STATUS )
*    Parameters :
*     COMLOC = CHARACTER*(DAT__SZLOC)( READ )
*           Locator to the component.
*     INDENT = INTEGER( READ )
*           Indentation level for the message output.
*     NAME = CHARACTER*(DAT__SZNAM)( WRITE )
*           Name of the component.
*     PRIM = LOGICAL( WRITE )
*           Set to .TRUE. if component is primitive.
*     SIZE = INTEGER( WRITE )
*           Number of elements for component if treated as a vector.
*     NDIM = INTEGER( WRITE )
*           Dimensionality of the component.
*     DIMS( DAT__MXDIM ) = INTEGER( WRITE )
*           Dimensions of the component.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if an error occurs during the execution
*           of this routine then STATUS will be set to the appropriate error
*           value.
*    Method :
*     Get all the information on this component.
*     If no error then
*        Initialise the message line with indentation.
*        Append the component type.
*        Append the component name.
*        Append the component dimensions.
*        Pad out until 40 characters beyond initial indent.
*        Append values for primitive types, informational message for
*        structures.
*        Output line as the only token in a message.
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/03/1984 : Original version                      (ROE::ASOC5)
*     07/04/1984 : Revised to use LOCINF, PUTDIM, PUTVAL (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC)
     :  COMLOC ! Locator to a component.
      INTEGER
     :  INDENT ! Indentation for output informatio message string
*    Export :
      CHARACTER*(DAT__SZNAM)
     :  NAME ! Component name
      LOGICAL
     :  PRIM ! returned .TRUE. if component is primitive
      INTEGER
     :  SIZE, ! number of elements for the component if treated as a vector
     :  NDIM, ! dimensionality of the component
     :  DIMS(DAT__MXDIM) ! dimensions of the component
*    Status return :
      INTEGER STATUS     ! the global status
*    External references :
      INTEGER CHR_LEN    ! String length
*    Local constants :
      INTEGER LNSIZE     ! Line size
      PARAMETER( LNSIZE=78 )
*    Local variables :
      CHARACTER*(DAT__SZTYP)
     :  TYPE ! Component type
      CHARACTER*(LNSIZE)
     :  LINE ! Message line string
      INTEGER
     :  LENG, ! current position in the line string
     :  I     ! character index
*-

*    get all the info on this component
      CALL LOCINF( COMLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise line ( with indentation )
         LINE = ' '
         LENG = INDENT

*       append the component type
         I = LENG
         CALL CHR_PUTC( TYPE, LINE, I )
         LENG = MAX( I, LENG + DAT__SZTYP )
         CALL CHR_PUTC( ' ', LINE, LENG )

*       append the component name
         I = CHR_LEN( NAME )
         CALL CHR_PUTC( NAME(1:I), LINE, LENG )

*       append the component dimensions
         IF( NDIM .GT. 0 ) THEN
            CALL PUTDIM( NDIM, DIMS, LINE, LENG )
         ENDIF

*       pad out until 40 characters beyond initial indent
          LENG = MAX( LENG, INDENT+40 )

         IF( PRIM ) THEN

*          append values for primitive types
            CALL PUTVAL( COMLOC, TYPE, NDIM, DIMS, SIZE, LINE, LENG )
         ELSE
            IF( SIZE .EQ. 1 ) THEN

*             append "<structure>" for scalar structures
               CALL CHR_PUTC( '<structure>', LINE, LENG )
            ELSE

*             append "<array of structures>" for arrays of structures
               CALL CHR_PUTC( '<array of structures>', LINE, LENG )
            ENDIF
         ENDIF

*       Output line as the only token in a message
         CALL MSG_SETC( 'LINE', LINE(1:LENG) )
         CALL MSG_OUT( 'COMINF_MES', '^LINE', STATUS )
      ENDIF

      END
