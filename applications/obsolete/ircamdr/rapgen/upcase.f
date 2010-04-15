
*+  UPCASE - converts strings to upper case

      SUBROUTINE UPCASE ( INSTRING, OUTSTRING, STATUS )

*    Description :
*
*     Just converts all lower case characters in INSTRING to upper
*     case characters in OUTSTRING
*
*    Invocation :
*
*     CALL UPCASE ( INSTRING, OUTSTRING, STATUS )
*
*    Parameters :
*
*     INSTRING   =  CHARACTER( READ )
*                String to be converted
*     OUTSTRING  =  CHARACTER( WRITE )
*                Output upper case string
*
*    Method :
*
*     Just uses Vax/VMS Run-Time Library routine STR$UPCASE
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean ( REVA::MJM )
*
*    History :
*
*     23-08-1985 : First implementation as wrap-around routine for
*                : VMS routine to alter input order of strings.
*                : ( REVA::MJM )
*     25-MAY-1994  Changed STR$ call to CHR_ (SKL@JACH)
*
*    Type Definitions :
*
      IMPLICIT  NONE             ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'         ! SSE global definitions
      INCLUDE 'CHR_ERR'

*    Import :

      CHARACTER*(*)
     :         INSTRING          ! Input string to be converted

*    Export :

      CHARACTER*(*)
     :         OUTSTRING         ! Output upper case string

*    Status :

      INTEGER  STATUS            ! global status parameter

*-
*    error check on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    Just call CHR_ library routine to do conversion

      OUTSTRING = INSTRING
      CALL CHR_UCASE( OUTSTRING )

*    That's it.

      END
