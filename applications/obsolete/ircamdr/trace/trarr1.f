*+  TRARR1 - Trace down the elements of an array of structures.
      SUBROUTINE TRARR1( OBJLOC, NAME, SIZE, NDIM, DIMS, INDENT, FULL,
     :  STATUS )
*    Description :
*     If FULL is set to .TRUE. then all the elements of the array of
*     structures with locator OBJLOC will be examined, if FULL is .FALSE.
*     then only the first element of the array will be examined.
*    Invocation :
*     CALL TRARR1( OBJLOC, NAME, SIZE, NDIM, DIMS, INDENT, FULL, STATUS )
*    Parameters :
*     OBJLOC = CHARACTER*(DAT__SZLOC)
*           Locator to the array of structures.
*     NAME = CHARACTER*(DAT__SZNAM)
*           Name of the array of structures.
*     SIZE = INTEGER( READ )
*           Number of elements of the array if treated as a vector.
*     NDIM = INTEGER( READ )
*           Dimensionality of the array of structures.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Dimensions of the array of structures.
*     INDENT = INTEGER( READ )
*           Indentation level for contents message and contents.
*     FULL = LOGICAL( READ )
*           If .TRUE. then all elements of the array will be traced.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if it has an error value on entry
*           then an immediate return will occur. If an error occurs during
*           the execution of the routine then STATUS will be returned
*           containing the appropriate error message.
*    Method :
*     If no error on entry then
*        Set up the maximum number of elements of the array
*        of structures to be traced.
*        If full trace requested then
*           Maximum number of elements to be traced is set equal
*           to the total number of elements in the array.
*        Else
*           Maximum number of elements is set to 1.
*        Endif
*        For all elements up to maximum number of elements to be traced
*           Get the dimension indices for this element.
*           Write out "Contents of" message.
*           Get a locator to this element of the array of structures.
*           If no error then
*              Examine this structure using TRSTR1.
*              Tidy up this element.
*           Endif
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/03/1984 : Original version (ROE::ASOC5)
*     07/06/1984 : Revised version  (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC)
     :  OBJLOC ! locator to array of structures
      CHARACTER*(DAT__SZNAM)
     :  NAME ! name of the array of structures.
      INTEGER
     :  SIZE, ! number of elements for array if treated as a vector
     :  NDIM, ! dimensionality of the array of structures
     :  DIMS( DAT__MXDIM), ! dimensions of the array of structures
     :  INDENT ! indentation level for text output
      LOGICAL
     :  FULL ! flag indicating if full trace to be performed
*    Status :
      INTEGER STATUS  ! the global status
*    Local variables :
      CHARACTER*(DAT__SZLOC)
     :  CELLOC ! locator to first element of an array of structures
      INTEGER
     :  MAXEL, ! maximum number of elements of array to be traced
     :  INDEX, ! index to elements of the array of structures
     :  CDIMS( DAT__MXDIM ) ! array of dimension indices for DAT_CELL
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       set up maximum number of elements of array to be traced
         IF( FULL ) THEN

*          full trace requested so MAXEL set equal to SIZE
            MAXEL = SIZE
         ELSE

*          only want the first element so MAXEL set to 1
            MAXEL = 1
         ENDIF

*       loop round for all elements up to maximum element set
         DO INDEX = 1, MAXEL

*          get the dimension indices for this element
            CALL ARELEM( INDEX, NDIM, DIMS, CDIMS, STATUS )

*          write out "Contents of" message
            CALL TRACON( NAME, NDIM, CDIMS, INDENT, STATUS )

*          get a locator to this element of the array of structures
            CALL DAT_CELL( OBJLOC, NDIM, CDIMS, CELLOC, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             examine this structure using TRSTR1
               CALL TRSTR1( CELLOC, INDENT, FULL, STATUS )

*             tidy up the cell
               CALL DAT_ANNUL( CELLOC, STATUS )
            ENDIF
         ENDDO
      ENDIF

      END
