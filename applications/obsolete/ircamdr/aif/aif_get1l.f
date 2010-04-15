*+  AIF_GET1L - Obtain exact number of LOGICAL values within given range
      SUBROUTINE AIF_GET1L( PARNAM, MIN, MAX, NVALS, VALUES, STATUS )
*    Description :
*     This routine obtains an exact number of LOGICAL values, within a given
*     desired range, from the parameter system. If in any one request less
*     than the required number of values are obtained this routine will keep
*     requesting until it gets the required number of values (or until an
*     error occurs). The routine will keep prompting until numbers are given
*     within the requested range.
*    Invocation :
*      CALL AIF_GET1L( PARNAM, MIN, MAX, NVALS, VALUES, STATUS )
*    Parameters :
*     PARNAM         = CHARACTER*(*)( READ )
*           Name of the parameter associated with the requested values
*     MIN            = LOGICAL( READ )
*           Minimum allowed value for elements of VALUES.
*     MAX            = LOGICAL( READ )
*           Maximum allowed value for elements of VALUES.
*     NVALS          = INTEGER( READ )
*           The exact number of values required
*     VALUES( NVALS )= LOGICAL( WRITE )
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
      LOGICAL
     :  MIN,            ! minimum acceptable value for elements of VALUES
     :  MAX             ! maximum acceptable value for elements of VALUES
*    Export :
      LOGICAL
     :  VALUES (NVALS) ! contains the values obtained from parameter system
*    Status :
      INTEGER
     :  STATUS         ! global status
*    Local variables :
      LOGICAL
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
            CALL AIF_EXACL( PARNAM, NVALS, VALUES, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             obtain the range of the values given
               CALL AIF_MMVL( NVALS, VALUES, VRANGE, STATUS )

*             check if values are within specified range
*             See notes in aif_get0l.f
               IF( ( VRANGE(1) .EQV. MIN ) .OR.
     :             ( VRANGE(2) .EQV. MAX ) ) THEN

*                report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETL( 'MIN', MIN )
                  CALL MSG_SETL( 'MAX', MAX )
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
