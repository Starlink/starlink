*+  APPG0R - to obtain a REAL scalar between given limits
      SUBROUTINE APPG0R( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Description :
*     A value is obtained for the real variable VALUE, associated with the
*     parameter name PARNAM, which is in the range MIN <= VALUE <= MAX. The
*     value given as DEFAUL is suggested to the data system as the default
*     value for VALUE, if a defualt has already been suggested for this
*     parameter this attempt will fail and the old value will be kept.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL APPG0R( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHAR*(*)( READ )
*           Parameter name associated with the value to be returned in VALUE.
*     DEFAUL = REAL( READ )
*           Default value which will be suggested to the parameter system if no
*           value has already been suggested as a default.
*     MIN    = REAL( READ )
*           Minimum acceptable value for VALUE.
*     MAX    = REAL( READ )
*           Maximum acceptable value for VALUE.
*     VALUE  = REAL( UPDATE )
*           Value returned will only be valid if STATUS has not been set to an
*           error value.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Suggest the default value to the data system, if the status is
*          returned with a value corresponding to the parameter being in
*          an invalid state then just annul the error and continue.
*        Do while no acceptable value obtained and no error on read
*           Get a value.
*           If no error then
*              If value is outside acceptable range then
*                 Report as an error.
*              Else
*                 Set the acceptable value flag.
*              Endif
*           Endif
*        Enddo
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     30/11/1983 : Modified for release 0.5             (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*)
     :  PARNAM ! parameter name to be associated with variable VALUE
      REAL
     :  DEFAUL, ! application defined default value for VALUE
     :  MIN,    ! minimum acceptable value for VALUE
     :  MAX     ! maximum      "       "    "   "
*    Import-Export :
      REAL
     :  VALUE ! contains value to be returned if acceptable value obtained
*    Status :
      INTEGER STATUS
*    Local variables :
      LOGICAL
     :  NOTOK ! true while no acceptable value obtained
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       suggest default value to the data system
         CALL PAR_DEF0R( PARNAM, DEFAUL, STATUS )

*       check for a dynamic default having already been set
         IF( STATUS .EQ. PAR__INVST ) THEN

*           clear the error and continue
             CALL ERR_ANNUL( STATUS )
         ENDIF

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       loop will keep going as long as dont have o.k. value and no error
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*          get parameter value
            CALL PAR_GET0R( PARNAM, VALUE, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             check for value is within specified range
               IF( ( VALUE .LT. MIN ) .OR. ( VALUE .GT. MAX ) ) THEN

*                report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETR( 'VALUE', VALUE )
                  CALL MSG_SETR( 'MIN', MIN )
                  CALL MSG_SETR( 'MAX', MAX )
                  CALL ERR_REP( 'ERR_APPG0R',
     :              '^VALUE is outside of allowed range.'/
     :              /' Give value in range ^MIN to ^MAX please.',
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
