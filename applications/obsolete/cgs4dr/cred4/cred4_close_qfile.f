*+  CRED4_CLOSE_QFILE - Close data reduction queue file
      SUBROUTINE CRED4_CLOSE_QFILE( STATUS )
*    Description :
*     Closes the data reduction queue file.
*    Invocation :
*     CALL CRED4_CLOSE_QFILE( STATUS )
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989 : Original version.                           (JFL)
*     10-Jan-1990: Didn't work. "CALL CLOSE" replaced by "CLOSE"!! (SMB)
*     15-Jan-1990: Documentation improved.                         (SMB)
*     14-May-1990: Bug fix. This routine did not free the logical
*                  unit number allocated in CRED4_OPEN_FILE.       (SMB)
*      5-Jun-1990: Name made more consistent.                      (SMB)
*     11-Jun-1990: Setting of status to ACT__END removed.
*                  Now done in CRED4_ENDACTION.                    (SMB)
*     11-Jun-1990: "Data reduction queue" terminology used.        (SMB)
*     18-Jun-1990: Renamed from CRED4_CLOSE_FILE to
*                  CRED4_CLOSE_QFILE (so the naming becomes
*                  consistent with ENG4 task).                     (SMB)
*     11-Feb-1993: Conform to error strategy                       (PND)
*     18-Feb-1993: Remove LIB$ calls                               (PND)
*     28-Jul-1994: Port to Unix                                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CRED4COM.INC'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
      QMAN_OK = .FALSE.
      END
