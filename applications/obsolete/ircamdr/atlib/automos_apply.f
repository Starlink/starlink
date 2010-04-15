*+  AUTOMOS_APPLY - applies DC correction to a set of images

      SUBROUTINE AUTOMOS_APPLY ( DIMSAX, DIMSAY, ARRAYA, DIMSBX, DIMSBY,
     :	                         ARRAYB, DCOFF, STATUS )

*    Description :
*
*    Invocation :
*
*     CALL AUTOMOS_APPLY ( DIMSAX, DIMSAY, ARRAYA, DIMSBX, DIMSBY,
*                          ARRAYB, DCOFF, STATUS )
*
*    Parameters :
*
*     DIMSAX  =  INTEGER( READ )
*     DIMSAY  =  INTEGER( READ )
*         Dimensions of first input image
*     ARRAYA( DIMSAX, DIMSAY )  =  REAL( READ )
*         input image
*     DIMSBX  =  INTEGER( READ )
*     DIMSBY  =  INTEGER( READ )
*         Dimensions of output image
*     ARRAYB( DIMSBX, DIMSBY )  =  REAL( READ )
*         output image
*     DCOFF  =  REAL( WRITE )
*         Calculated d.c. sky offset
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
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
*     07-01-1988 : First implementation (UKTH::MJM)
*     02-01-1990 : This version adapted from MOFFDCSUB (JACH::CAA)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMSAX,             ! dimensions of first input image
     :    DIMSAY,             ! dimensions of first input image
     :    DIMSBX,             !      "      " second  "     "
     :    DIMSBY              !      "      " second  "     "

      REAL
     :    ARRAYA( DIMSAX, DIMSAY ),   ! first input image
     :    ARRAYB( DIMSBX, DIMSBY )    ! second  "     "

*    Export :

      REAL
     :    DCOFF                   ! calculated d.c. sky offset

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :	J,
     :	K

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      DO J = 1, MIN( DIMSAY, DIMSBY)

        DO K = 1, MIN( DIMSAX, DIMSBX)

	  ARRAYB( K, J) = ARRAYA( K, J) + DCOFF

        END DO

      END DO

      END
