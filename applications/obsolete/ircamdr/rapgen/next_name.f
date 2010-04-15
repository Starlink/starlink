
*+  NEXT_NAME - increments Fnnnn type filename by one

      SUBROUTINE NEXT_NAME ( CURRNAME, NEXTNAME, STATUS )

*    Description :
*
*     This routine takes the input string CURRNAME which should end with an
*     IRCAM type filename Fnnnn (where nnnn is four integers), and returns
*     the string NEXTNAME containing the next filename in numerical sequence,
*     i.e. it adds one to the integer nnnn.
*
*    Invocation :
*
*     CALL NEXT_NAME( CURRNAME; NEXTNAME, STATUS )
*
*    Parameters :
*
*     CURRNAME  =  CHAR*(*)
*         Current filename of IRCAM type ending in Fnnnn
*     NEXTNAME  =  CHAR*(*)
*         Next filename in numerical sequence
*     STATUS    =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get length of current filename ignoring trailing blanks
*     Cut off last four characters of current filename assuming them to be nnnn
*     Convert the string holding nnnn into an integer
*     Add one to the old value to get the new value
*     If error occurred or new value is too large then
*        Set status to error
*        Set next filename equal to current filename
*        Return
*     Endif
*     Convert new number back into a string
*     Splice required leading 0's onto new string
*     Form next filename from current one with new number spliced on end
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     02-06-1986 : First implementation (REVA::MJM)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      CHARACTER*(*)
     :    CURRNAME                ! current filename ending in Fnnnn

*    Export :

      CHARACTER*(*)
     :    NEXTNAME                ! next filename in numerical sequence

*    Status :

      INTEGER  STATUS             ! global status parameter

*    External references :

      INTEGER
     :    CHR_LEN                 ! string length omitting trailing blanks

*    Local variables :

      INTEGER
     :    OLDNUM,                 ! number corresponding to current filename
     :    NEWNUM,                 !    "         "        " next        "
     :    I,                      ! length of current filename string
     :    NCHAR                   ! number of valid digits in next filename

      CHARACTER*4
     :    OLDSTR,                 ! four digits at end of current filename
     :    NEWSTR                  !   "     "    "  "   " next        "

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by getting the length of the current filename string, ignoring
*    trailing blanks
      I  =  CHR_LEN( CURRNAME )

*    assume that the current filename ends with four integers, cut them off
*    and put them into a new string
      OLDSTR  =  CURRNAME( I-3 : I )

*    convert this into an actual integer
      CALL CHR_CTOI( OLDSTR, OLDNUM, STATUS )

*    add one to the returned integer
      NEWNUM  =  OLDNUM + 1

*    check for error or new number too large
      IF ( STATUS .NE. SAI__OK .OR. NEWNUM .GT. 9999 ) THEN

*       set status to error and return old filename string
         STATUS    =  SAI__ERROR
         NEXTNAME  =  CURRNAME
         RETURN

      END IF

*    convert acceptable new number back into a string
      CALL CHR_ITOC( NEWNUM, NEWSTR, NCHAR )

*    now check how many digits the output string contains and act
*    accordingly
      IF ( NCHAR .EQ. 1 ) THEN

*       output number lies between 0 and 9 - append three 0's
         NEWSTR  =  '000'//NEWSTR( 1 : 1 )

      ELSE IF ( NCHAR .EQ. 2 ) THEN

*       output number lies between 10 and 99 - append two 0's
         NEWSTR  =  '00'//NEWSTR( 1 : 2 )

      ELSE IF ( NCHAR .EQ. 3 ) THEN

*       output number lies between 100 and 999 - append one 0
         NEWSTR  =  '0'//NEWSTR( 1 : 3 )

*    other case is four valid digits - no 0's needed
      END IF

*    now form the returned next filename
      NEXTNAME  =  CURRNAME( 1 : I-4 )//NEWSTR


*    return and end
      END
