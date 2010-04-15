*+  AIF_GET1C - Obtain exact number of CHARACTER*(*) values within given range
      SUBROUTINE AIF_GET1C( PARNAM, MIN, MAX, NVALS, VALUES, STATUS )
*    Description :
*     This routine obtains an exact number of CHARACTER*(*) values, within a given
*     desired range, from the parameter system. If in any one request less
*     than the required number of values are obtained this routine will keep
*     requesting until it gets the required number of values (or until an
*     error occurs). The routine will keep prompting until numbers are given
*     within the requested range.
*    Invocation :
*      CALL AIF_GET1C( PARNAM, MIN, MAX, NVALS, VALUES, STATUS )
*    Parameters :
*     PARNAM         = CHARACTER*(*)( READ )
*           Name of the parameter associated with the requested values
*     MIN            = CHARACTER*(*)( READ )
*           Minimum allowed value for elements of VALUES.
*     MAX            = CHARACTER*(*)( READ )
*           Maximum allowed value for elements of VALUES.
*     NVALS          = INTEGER( READ )
*           The exact number of values required
*     VALUES( NVALS )= CHARACTER*(*)( WRITE )
*           The values obtained from the parameter system.
*     STATUS         = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (ROE::SMB)
*     Dave Baines (ROE::ASOC5)
*    History :
*     12/04/1984: original version (based on Dave's APPG0I)  (ROE::SMB)
*     10/06/1984: modified to generic AIF_ version           (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE         ! must declare all variables
*    Global constants :
      INCLUDE 'SAE_PAR'     ! SSE global variables
*    Import :
      CHARACTER*(*)
     :  PARNAM          ! contains parameter name associated with values
      INTEGER
     :  NVALS           ! number of values to obtain
      CHARACTER*(*)
     :  MIN,            ! minimum acceptable value for elements of VALUES
     :  MAX             ! maximum acceptable value for elements of VALUES
*    Export :
      CHARACTER*(*)
     :  VALUES (NVALS) ! contains the values obtained from parameter system
*    Status :
      INTEGER
     :  STATUS         ! global status
*    Local variables :
      CHARACTER*200
     :  VRANGE (2)     ! minimum and maximum of values given
      LOGICAL
     :  NOTOK          ! true while no acceptable value obtained
*-

*   check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       loop will keep going as long correct values not obtained and no error
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*         obtain exactly NVALS values
            CALL AIF_EXACC( PARNAM, NVALS, VALUES, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             obtain the range of the values given
               CALL AIF_MMVC( NVALS, VALUES, VRANGE, STATUS )

*             check if values are within specified range
               IF( ( VRANGE(1) .LT. MIN ) .OR.
     :             ( VRANGE(2) .GT. MAX ) ) THEN

*                report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'MIN', MIN )
                  CALL MSG_SETC( 'MAX', MAX )
                  CALL ERR_REP( 'ERR_APPG0I',
     :              'Values given are outside allowed range.'/
     :              /' Give values in range ^MIN to ^MAX please.',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARNAM, STATUS )
               ELSE

*                value must be o.k. so terminate the loop
                  NOTOK = .FALSE.
               ENDIF
            ENDIF
         ENDDO

      ENDIF

      END
