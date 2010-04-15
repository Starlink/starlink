*+  RED4_FILE_STANDARD - File observation in index file as STANDARD
      SUBROUTINE RED4_FILE_STANDARD( STATUS )
*    Description :
*     This routine converts the given observation into a STANDARD
*     and writes its parameters to the index file.
*    Invocation :
*     CALL RED4_FILE_STANDARD( STATUS )
*    Parameters :
*     STATUS         = INTEGER( UPDATE )
*           Global status.
*    Method :
*    Deficiencies :
*    Bugs :
*     Note that there is a bug or "feature" of the interaction
*     between DSA and the ADAM parameter system which means that
*     the reference name given to DSA_NAMED_INPUT must be different
*     from the parameter name given to PAR_GET0C.
*     Bug reported to Starlink on 23-Nov-1990.
*    Authors :
*     S.M.Beard  (REVAD::SMB)
*     P.N.Daly   (JACH::PND)
*    History :
*     13-Dec-1990: Original version, based on RED4_FILE_CALIBRATION. (SMB)
*     18-Dec-1990: Typing mistakes fixed.                            (SMB)
*     19-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Status :
      INTEGER STATUS               ! Global status
*    External references:
*    Local Constants :
*    Local variables :
      CHARACTER*80
     :  GRPFILE,                   ! Name of group file
     :  STDFILE                    ! Name of STANDARD file
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the relevant parameters and convert the group into a standard.
      CALL RED4_MAKE_STANDARD( STATUS )

*   Find out the name of the group file, which was obtained by a
*   parameter system call in the above routine, and convert this
*   to the name of the STANDARD file.
      CALL PAR_GET0C( 'GRPFILE', GRPFILE, STATUS )
      CALL RED4_GRPTOSTD( GRPFILE, STDFILE, STATUS )

*   If all the above has worked, call the RED4 routine which
*   will file this STANDARD in the index file.
      CALL RED4_FILE_GROUP_2( STDFILE, STATUS )

      END
