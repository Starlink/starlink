
*+  TRACE - Trace through the contents of a data system object.
      SUBROUTINE TRACE( STATUS )
*    Description :
*     The given data system object, %OBJECT, is examined. There are three cases
*     which are handled :
*     1) %OBJECT is a primitive object. The value or first few values of
*        %OBJECT are listed.
*     2) %OBJECT is a structure. The contents of the structure are listed. If a
*        component is encountered which is itself a structure then its contents
*        are listed down to a level of 6 nested structures. If a component
*        which is an array of structures is found then either :
*         i) all elements will be listed - %FULL set to TRUE
*        ii) only the first element will be listed - %FULL set to FALSE
*     3) %OBJECT is an array of structures. The above options i) and ii) apply.
*        arrays of structures nested 6 deep can be listed.
*    Parameters :
*     OBJECT = UNIV( READ )
*           Data system object to be examined.
*     FULL = LOGICAL( READ )
*           Flag to indicate if complete trace of arrays of structures wanted
*    Method :
*     Get a locator to the HDS object
*     If no error then
*        Get the full trace switch.
*        If no error then
*           Get the necessary information for the HDS object.
*           Put out the TRACE header message.
*           Deal with the HDS object depending on whether it is of PRIMITIVE
*           type, a STRUCTURE or an ARRAY of STRUCTURES.
*           If it is of primitive type then put out the information and
*           values associated with this PRIMITIVE HDS object.
*           Elseif it is a scalar STRUCTURE then
*              Examine the HDS object using TRSTR1.
*           Else must have an ARRAY of STRUCTURES
*              Output the name and dimensions for the array of structures.
*              Trace down this array of structures.
*           Endif
*        Endif
*        Put out the TRACE termination message.
*        Tidy up the HDS object.
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/03/1984 : Original version - based on LS     (ROE::ASOC5)
*     07/04/1984 : Modified to remove replicated code (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE      ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR'  ! enviroment constants
      INCLUDE 'DAT_PAR'  ! Necessary for non-VMS
*    Status :
      INTEGER STATUS     ! the global status
*    Local Constants :
      INTEGER INDENT
      PARAMETER ( INDENT = 3 ) ! indentaion level for scalar objects
*    Local variables :
      CHARACTER*(DAT__SZLOC)
     :  OBJLOC ! locator to object
      CHARACTER*(DAT__SZTYP)
     :  TYPE ! type of the object
      CHARACTER*(DAT__SZNAM)
     :  NAME ! name of the object
      LOGICAL
     :  FULL, ! inicates if full trace of arrays of structures required
     :  PRIM  ! set to .TRUE. if object is primitive
      INTEGER
     :  SIZE, ! size of object if vectorised
     :  NDIM, ! dimensionality of the object
     :  DIMS( DAT__MXDIM ) ! dimensions of the object
*-

*    get a locator to the desired object
      CALL DAT_ASSOC( 'OBJECT', 'READ', OBJLOC, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       get full trace switch
         CALL PAR_GET0L( 'FULL', FULL, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          get all necessary information on this object
            CALL LOCINF( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
     :        STATUS )

*          put out the header
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_SETC( 'TYPE', TYPE )
            CALL MSG_SETC( 'PAR', 'OBJECT' )
            CALL MSG_OUT( 'TRACE_START', '^TYPE  $^PAR', STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          deal with the object depending on whether it is PRIMITIVE,
*          a STRUCTURE or an ARRAY of STRUCTURES
            IF( PRIM ) THEN

*             put out information and values associated with PRIMITIVE object
               CALL PRIMIT( OBJLOC, NAME, TYPE, SIZE, NDIM, DIMS,
     :           INDENT, STATUS )
            ELSE

*             check for scalar STRUCTURE object
               IF( NDIM .EQ. 0 ) THEN

*                examine the scalar structure using TRSTR1
                  CALL TRSTR1( OBJLOC, INDENT, FULL, STATUS )
               ELSE

*                must have an ARRAY of STRUCTURES
*                output the name and dimensions of the array of structures
                  CALL ARRSTR( NAME, NDIM, DIMS, INDENT, STATUS )

*                trace down this array of structures
                  CALL TRARR1( OBJLOC, NAME, SIZE, NDIM, DIMS, INDENT,
     :              FULL, STATUS )
               ENDIF
            ENDIF

*          put out the termination message
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_OUT( 'TRACE_END', 'End of Trace.', STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          annul the locator
            CALL DAT_ANNUL( OBJLOC, STATUS )
         ENDIF
      ENDIF

      END
