*+  CRED4_LIST0C - List the value of a scalar CHARACTER*(*) parameter
      SUBROUTINE CRED4_LIST0C( NAME, VALUE, STATUS )
*    Description :
*     This routine displays the current value of a scalar parameter of
*     type CHARACTER*(*).
*     This is a generic routine which can generate all types.
*    Invocation :
*     CALL CRED4_LIST0C( NAME, VALUE, STATUS )
*    Parameters :
*     NAME      = CHARACTER*(*)( READ )
*           Name of parameter.
*     VALUE     = CHARACTER*(*)( READ )
*           Value of parameter.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     22-Jun-1990: Original version.           (SMB)
*     11-Feb-1993:  Conform to error strategy  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      CHARACTER*(*) VALUE
*    Status :
      INTEGER STATUS       ! Global status
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the name and value into tokens.
      CALL MSG_SETC( 'NAME', NAME )
      CALL MSG_SETC( 'VALUE', VALUE )

*   Display the name and the value.
      CALL MSG_OUT( ' ', '   ^NAME = ^VALUE', STATUS )

      END
*+  CRED4_LIST0D - List the value of a scalar DOUBLE PRECISION parameter
      SUBROUTINE CRED4_LIST0D( NAME, VALUE, STATUS )
*    Description :
*     This routine displays the current value of a scalar parameter of
*     type DOUBLE PRECISION.
*     This is a generic routine which can generate all types.
*    Invocation :
*     CALL CRED4_LIST0D( NAME, VALUE, STATUS )
*    Parameters :
*     NAME      = CHARACTER*(*)( READ )
*           Name of parameter.
*     VALUE     = DOUBLE PRECISION( READ )
*           Value of parameter.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     22-Jun-1990: Original version.           (SMB)
*     11-Feb-1993:  Conform to error strategy  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      DOUBLE PRECISION VALUE
*    Status :
      INTEGER STATUS       ! Global status
*    External references :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the name and value into tokens.
      CALL MSG_SETC( 'NAME', NAME )
      CALL MSG_SETD( 'VALUE', VALUE )

*   Display the name and the value.
      CALL MSG_OUT( ' ', '   ^NAME = ^VALUE', STATUS )

      END
*+  CRED4_LIST0I - List the value of a scalar INTEGER parameter
      SUBROUTINE CRED4_LIST0I( NAME, VALUE, STATUS )
*    Description :
*     This routine displays the current value of a scalar parameter of
*     type INTEGER.
*     This is a generic routine which can generate all types.
*    Invocation :
*     CALL CRED4_LIST0I( NAME, VALUE, STATUS )
*    Parameters :
*     NAME      = CHARACTER*(*)( READ )
*           Name of parameter.
*     VALUE     = INTEGER( READ )
*           Value of parameter.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     22-Jun-1990: Original version.           (SMB)
*     11-Feb-1993:  Conform to error strategy  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      INTEGER VALUE
*    Status :
      INTEGER STATUS       ! Global status
*    External references :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the name and value into tokens.
      CALL MSG_SETC( 'NAME', NAME )
      CALL MSG_SETI( 'VALUE', VALUE )

*   Display the name and the value.
      CALL MSG_OUT( ' ', '   ^NAME = ^VALUE', STATUS )

      END
*+  CRED4_LIST0L - List the value of a scalar LOGICAL parameter
      SUBROUTINE CRED4_LIST0L( NAME, VALUE, STATUS )
*    Description :
*     This routine displays the current value of a scalar parameter of
*     type LOGICAL.
*     This is a generic routine which can generate all types.
*    Invocation :
*     CALL CRED4_LIST0L( NAME, VALUE, STATUS )
*    Parameters :
*     NAME      = CHARACTER*(*)( READ )
*           Name of parameter.
*     VALUE     = LOGICAL( READ )
*           Value of parameter.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     22-Jun-1990: Original version.           (SMB)
*     11-Feb-1993:  Conform to error strategy  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      LOGICAL VALUE
*    Status :
      INTEGER STATUS       ! Global status
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the name and value into tokens.
      CALL MSG_SETC( 'NAME', NAME )
      CALL MSG_SETL( 'VALUE', VALUE )

*   Display the name and the value.
      CALL MSG_OUT( ' ', '   ^NAME = ^VALUE', STATUS )

      END
*+  CRED4_LIST0R - List the value of a scalar REAL parameter
      SUBROUTINE CRED4_LIST0R( NAME, VALUE, STATUS )
*    Description :
*     This routine displays the current value of a scalar parameter of
*     type REAL.
*     This is a generic routine which can generate all types.
*    Invocation :
*     CALL CRED4_LIST0R( NAME, VALUE, STATUS )
*    Parameters :
*     NAME      = CHARACTER*(*)( READ )
*           Name of parameter.
*     VALUE     = REAL( READ )
*           Value of parameter.
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be SAI__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be SAI__OK on exit. Any other value indicates
*           an error.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     22-Jun-1990: Original version.           (SMB)
*     11-Feb-1993:  Conform to error strategy  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
      REAL VALUE
*    Status :
      INTEGER STATUS       ! Global status
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the name and value into tokens.
      CALL MSG_SETC( 'NAME', NAME )
      CALL MSG_SETR( 'VALUE', VALUE )

*   Display the name and the value.
      CALL MSG_OUT( ' ', '   ^NAME = ^VALUE', STATUS )

      END
