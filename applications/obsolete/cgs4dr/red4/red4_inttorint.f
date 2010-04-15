*+  RED4_INTTORINT - Convert integration file name to reduced integration
      SUBROUTINE RED4_INTTORINT( INT_FILE, RINT_FILE, STATUS )
*    Description :
*     This routine converts an integration file name of the form
*     Iyymmdd_oooo_iiii or IDIR:Iyymmdd_oooo_iiii into a reduced
*     integration file name of the form RIDIR:RIyymmdd_oooo_iiii.
*
*     The routine was produced as an attempt to rationalise the
*     conversion between one form of file name and another, so that
*     making a change to the naming convention would not be such a
*     major task in the future, and to allow error reports to be
*     generated if an invalid file name is given.
*    Invocation :
*     CALL RED4_INTTORINT( INT_FILE, RINT_FILE, STATUS )
*    Parameters :
*     INT_FILE  = CHARACTER*(*)( READ )
*         The integration file name (Iyymmdd_oooo_iiii or
*         IDIR:Iyymmdd_oooo_iiii)
*     RINT_FILE = CHARACTER*(*)( WRITE )
*         The reduced integration file name (RIDIR:RIyymmdd_oooo_iiii)
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     23-Jul-1990: Original version.                       (SMB)
*     30-Jul-1990: Documentation change: "yy" allowed to
*                  be "yyyy".                              (SMB)
*     31-Jul-1990: "yyyy" changed back to "yy" !!!!!!!!    (SMB)
*     22-Feb-1993: Conform to error strategy               (PND)
*      7-Nov-1994: Make vaguely portable                   (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  INT_FILE              ! Integration file name
*    Export:
      CHARACTER*(*)
     :  RINT_FILE             ! Reduced integration file name
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      INTEGER
     :  COLON_POS             ! Position of colon in character string
      CHARACTER*20 LPREFIX    ! prefix to add to file
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN


*    See if the file name already has a prefix. Look for a ":" or  "/"
*    in the file name.
      COLON_POS = INDEX( INT_FILE, ':' )
      IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( INT_FILE, '/' )

      CALL RED4_GET_PREFIX ('RI', LPREFIX, STATUS)

      IF ( COLON_POS .EQ. 0 ) THEN

*      There seems to be no prefix so assume the name is Iyymmdd_oooo_iiii.
*      add the new prefix, along with an "R".
         RINT_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'r' // INT_FILE

      ELSE

*      There is a prefix. Convert it to the correct form by replacing it
*      with the new one and an "R".
         RINT_FILE = LPREFIX(:CHR_LEN(LPREFIX)) // 'r' /
     :    / INT_FILE( COLON_POS+1:)

      END IF

      END


