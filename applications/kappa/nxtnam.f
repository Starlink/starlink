*+  NXTNAM - increments Fnnnn type filename by one

      SUBROUTINE NXTNAM ( CURNAM, NEXTNM, STATUS )
*
*    Description :
*
*     This routine takes the input string CURNAM which should end with
*     an IRCAM type filename Fnnnn (where nnnn is four integers), and
*     returns the string NEXTNM containing the next filename in
*     numerical sequence, i.e. it adds one to the integer nnnn.
*
*    Invocation :
*
*     CALL NXTNAM( CURNAM, NEXTNM, STATUS )
*
*    Arguments :
*
*     CURNAM  =  CHAR*(*)
*         Current filename of IRCAM type ending in Fnnnn
*     NEXTNM  =  CHAR*(*)
*         Next filename in numerical sequence
*     STATUS  =  INTEGER( READ,WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get length of current filename ignoring trailing blanks
*     Cut off last four characters of current filename assuming them to
*       be nnnn
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
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     02-06-1986 : First implementation (REVA::MJM)
*     1986 Aug 12: Renamed from NEXT_NAME, and nearly conformed to 
*                  Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 : Renamed parameter section arguments and tidied
*                  (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      CHARACTER*(*)
     :    CURNAM

*    Export :

      CHARACTER*(*)
     :    NEXTNM

*    Status :

      INTEGER  STATUS

*    External references :

      INTEGER
     :    CHR_LEN             ! string length omitting trailing blanks

*    Local variables :

      INTEGER
     :    OLDNUM,             ! number corresponding to current filename
     :    NEWNUM,             !    "         "        " next        "
     :    I,                  ! length of current filename string
     :    NCHAR               ! number of valid digits in next filename

      CHARACTER*4
     :    DUMMY,              ! dummy for concatenation
     :    OLDSTR,             ! four digits at end of current filename
     :    NEWSTR              !   "     "    "  "   " next        "

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    start by getting the length of the current filename string,
*    ignoring trailing blanks

      I  =  CHR_LEN( CURNAM )

*    assume that the current filename ends with four integers, cut them 
*    off and put them into a new string

      OLDSTR  =  CURNAM( I-3 : I )

*    convert this into an actual integer

      CALL CHR_CTOI( OLDSTR, OLDNUM, STATUS )

*    add one to the returned integer

      NEWNUM  =  OLDNUM + 1

*    check for error or new number too large

      IF ( STATUS .NE. SAI__OK .OR. NEWNUM .GT. 9999 ) THEN

*       set status to error and return old filename string

         STATUS    =  SAI__ERROR
         NEXTNM  =  CURNAM
         GOTO 999
      END IF

*    convert acceptable new number back into a string

      CALL CHR_ITOC( NEWNUM, NEWSTR, NCHAR )
      DUMMY = NEWSTR

*    now check how many digits the output string contains and act
*    accordingly

      IF ( NCHAR .EQ. 1 ) THEN

*       output number lies between 0 and 9 - append three 0's

         NEWSTR  =  '000'//DUMMY( 1 : 1 )

      ELSE IF ( NCHAR .EQ. 2 ) THEN

*       output number lies between 10 and 99 - append two 0's

         NEWSTR  =  '00'//DUMMY( 1 : 2 )

      ELSE IF ( NCHAR .EQ. 3 ) THEN
 
*       output number lies between 100 and 999 - append one 0

         NEWSTR  =  '0'//DUMMY( 1 : 3 )

*    other case is four valid digits - no 0's needed

      END IF

*    now form the returned next filename

      NEXTNM  =  CURNAM( 1 : I-4 )//NEWSTR
            
 999  CONTINUE

*    return and end

      END
