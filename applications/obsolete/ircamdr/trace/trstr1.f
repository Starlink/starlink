*+ TRSTR1 - List the contents of a structure.
      SUBROUTINE TRSTR1( STRLOC, INDENT, FULL, STATUS )
*    Description :
*     Traces down the components of a structure, if a component is primitive
*     then information is listed about it, if a component is either a structure
*     or an array of structures then these structures are examined.
*    Invocation :
*     CALL TRSTR1( STRLOC, INDENT, FULL, STATUS )
*    Method :
*     Get the number of components for this structure.
*     If an error has occured then
*        Report it.
*     Elseif the structure is empty then
*        Output a message to this effect.
*     Else
*        For each component in turn
*           Get a locator to the INDEX'th component.
*           Get the component information and write it out.
*           If the component is not primitive then
*              If it is a scalar structure then
*                 Call the second level structure listing routine.
*              Else
*                Call the second level array tracing routine.
*              Endif
*           Endif
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     14/03/1984 : Based on LS     (ROE::ASOC5)
*     07/04/1984 : Revised version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE      ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR'  ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*( DAT__SZLOC )
     :  STRLOC ! locator to the structure
      INTEGER
     :  INDENT ! indentation for output
      LOGICAL
     :  FULL ! switch for full tracing of arrays of structures
*    Status return :
      INTEGER STATUS ! the global status
*    External references :
      INTEGER CHR_LEN ! string length ignoring trailing blanks
*    Local constants :
      INTEGER LNSIZE
      PARAMETER( LNSIZE = 78 ) ! line string length
*    Local variables :
      CHARACTER*( DAT__SZLOC )
     :  LOC ! component locator
      CHARACTER*( DAT__SZNAM )
     :  NAME ! component name
      CHARACTER*( LNSIZE )
     :  LINE ! character string for message line
      INTEGER
     :  SIZE, ! size as if vector
     :  NDIM, ! number of component dimensions
     :  DIMS( DAT__MXDIM ), ! component dimensions
     :  INDEX, ! component index
     :  NCOMP, ! number of components
     :  LENG,  ! index into message line
     :  LEN    ! character string lenghth
      LOGICAL
     :  PRIM ! whether object is primitive
*-

*    get number of structure components
      CALL DAT_NCOMP( STRLOC, NCOMP, STATUS )

*    check for error
      IF( STATUS .NE. SAI__OK ) THEN

         CALL DAT_ERDSC( STRLOC, STATUS )
      ELSEIF( NCOMP .LE. 0 ) THEN

*       structure is empty so output message
         LINE = ' '
         LENG = INDENT
         CALL CHR_PUTC( '< structure is empty >', LINE, LENG )
         LEN = CHR_LEN( LINE )
         CALL MSG_SETC( 'EMPTY', LINE )
         CALL MSG_OUT( 'STR_EMPTY', '^EMPTY', STATUS )
      ELSE

*       go through each component in turn
         DO INDEX = 1, NCOMP

*          get locator to INDEX'th component
            CALL DAT_INDEX( STRLOC, INDEX, LOC, STATUS )

*          get component information and write it out
            CALL COMINF( LOC, INDENT, NAME, PRIM, SIZE, NDIM, DIMS,
     :        STATUS )

*          here is the tracing section
            IF( .NOT. PRIM ) THEN

*             check for scalar structure
               IF( NDIM .EQ. 0 ) THEN

*                call the second level structure listing routine
                  CALL TRSTR2( LOC, INDENT+3, FULL, STATUS )
               ELSE

*                must have an array of structures so call second level
*                array tracing routine
                  CALL TRARR2( LOC, NAME, SIZE, NDIM, DIMS, INDENT+3,
     :              FULL, STATUS )
               ENDIF
            ENDIF

            CALL DAT_ANNUL( LOC, STATUS )
         ENDDO
      ENDIF

      END
