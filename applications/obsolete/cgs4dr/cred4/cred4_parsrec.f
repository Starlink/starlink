*+  CRED4_PARSREC - Read data from an ASCII configuration file
      SUBROUTINE CRED4_PARSREC( RECORD, ITEM, VALUE, TYPE, STATUS )
*    Description :
*     This routine reads data from a configuration file and
*     returns an item name, a character encoded value and a data-type.
*     Similarly, if it cannot determine
*     the structure of the input file (i.e. not NBS or ASCII) it returns
*     an error. In both cases, the routine puts a single space (' ') into
*     ITEM, VALUE and TYPE.
*    Invocation :
*     CALL CRED4_PARSREC( RECORD, ITEM, VALUE, TYPE, STATUS )
*    Parameters :
*     RECORD    = CHARACTER( READ )
*           The logical unit from which the data is read.
*     ITEM      = CHARACTER( WRITE )
*           The name of the item read from the file.
*     VALUE     = CHARACTER( WRITE )
*           The value of the item as an encoded character string.
*     TYPE      = CHARACTER( WRITE )
*           The data-type of the item read.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Method :
*     The ASCII file should have been written with the IO_WRITE0x routines
*     and will, therefore, contain lines such as:
*        PUTNBS NBS_CHAR "HELLO"
*        PUTNBS NBS_LOG  TRUE
*        PUTNBS NBS_REAL 1.0000
*        PUTNBS NBS_INT  12345
*     or
*        NBS_CHAR = "HELLO"
*        NBS_LOG  = TRUE
*        NBS_REAL = 1.0000
*        NBS_INT  = 12345
*     This routine separates the item (NBS_something) from its value. It
*     then parses the value string to determine the data-type of the value
*     according to the rules:
*        if it contains TRUE or FALSE it is LOGICAL
*        if it contains a double or single quote it is a CHARACTER value
*        if it contains a '.' it is REAL
*        if none of the above, it is either integer or real
*           if it contains E or D is is REAL else INTEGER
*     The item, character encoded value and data-type are returned to the
*     user who can then decode them using CHR_CTOx routines.
*    Deficiencies :
*     Does not recognise double precision, byte etc data-types.
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*     13-Jan-1993: Original version (PND)
*    endhistory
*    Type Definitions :
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import-Export :
      CHARACTER*(*)
     :  RECORD,                  ! The record
     :  ITEM,                    ! The name of the item
     :  VALUE,                   ! The value of the item
     :  TYPE                     ! The data-type of the item
*    Status :
      INTEGER
     :  STATUS                   ! Global status
*    External references :
      INTEGER
     :  CHR_LEN                  ! Length of string routine
*    Global variables :
*    Local constants :
*    Local variables :
      INTEGER
     :  I,                       ! A counter
     :  POS,                     ! String position
     :  RECLEN                   ! Length of a record
      LOGICAL
     : NBS_OK,                   ! T if we are reading an NBS file
     : ASC_OK                    ! T if we are reading an ASCII file
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Remove leading blanks from the string
      CALL CHR_LDBLK( RECORD )
      RECLEN = CHR_LEN( RECORD )

*   Read the item and value for a NBS file
      POS = 0
      POS = INDEX( RECORD, 'PUTNBS' )
      IF ( POS .GT. 0 ) THEN

         RECORD = RECORD( POS+6:RECLEN )
         CALL CHR_LDBLK( RECORD )
         RECLEN = CHR_LEN( RECORD )

         POS = 0
         POS = INDEX( RECORD, ' ')
         IF ( POS .GT. 0) THEN

            ITEM  = RECORD( 1:POS-1 )
            VALUE = RECORD( POS+1:RECLEN )
            NBS_OK = .TRUE.
         END IF
      ELSE
         NBS_OK = .FALSE.
      END IF

*   Read the item and value for an ICL file
      POS = 0
      POS = INDEX( RECORD, '=' )
      IF ( POS .GT. 0 ) THEN

         ITEM  = RECORD( 1:POS-1 )
         VALUE = RECORD( POS+1:RECLEN )
         ASC_OK = .TRUE.
      ELSE
         ASC_OK = .FALSE.
      END IF

*   Error if we have not read the correct file type
      IF ( ( .NOT. NBS_OK ) .AND. ( .NOT. ASC_OK ) ) THEN
         ITEM  = ' '
         VALUE = ' '
         TYPE  = ' '
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ',
     :     'Error reading configuration file (unknown format)', STATUS )
      ELSE

*      Remove all blanks from the item name
         CALL CHR_RMBLK( ITEM )

*      If VALUE contains a quote it is has CHARACTER data-type
         IF ( ( INDEX( VALUE, '"' ) .GT. 0  )   .OR.
     :        ( INDEX( VALUE, '''' ) .GT. 0 ) ) THEN

            TYPE = 'CHARACTER'

*         Strip off all quote marks
            DO I = 1, CHR_LEN( VALUE ), 1
               IF ( VALUE( I:I ) .EQ. '"' ) VALUE( I:I ) = ' '
               IF ( VALUE( I:I ) .EQ. '''' ) VALUE( I:I ) = ' '
            END DO
            CALL CHR_RMBLK( VALUE )

*      If VALUE contains a 'TRUE'  or 'FALSE' it has LOGICAL data-type else REAL
         ELSE IF ( ( INDEX( VALUE, 'TRUE' ) .GT. 0 )   .OR.
     :           ( INDEX( VALUE, 'true' ) .GT. 0 )  .OR.
     :           ( INDEX( VALUE, 'FALSE' ) .GT. 0 )   .OR.
     :           ( INDEX( VALUE, 'false' ) .GT. 0 ) ) THEN

               TYPE = 'LOGICAL'

*      Else If VALUE contains a '.' it is REAL
         ELSE IF ( INDEX( VALUE, '.' ) .GT. 0 ) THEN

            TYPE = 'REAL'

*      Else value is integer or real; REAL if it contains E or D else INTEGER
         ELSE

            CALL CHR_UCASE( VALUE )
            IF ( ( INDEX( VALUE, 'E' ) .GT. 0 )   .OR.
     :           ( INDEX( VALUE, 'D' ) .GT. 0 ) ) THEN

               TYPE = 'REAL'
            ELSE

               TYPE = 'INTEGER'
            END IF
         END IF

*      Set the correct lengths
         ITEM = ITEM( 1:CHR_LEN( ITEM ) )
         VALUE = VALUE( 1:CHR_LEN( VALUE ) )
         TYPE = TYPE( 1:CHR_LEN( TYPE ) )
      END IF

*   Exit the subroutine
      END
