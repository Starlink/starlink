*+  AIF_GODDN - Get an odd number within a given range.
      SUBROUTINE AIF_GODDN( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Description :
*     An odd integer value, VALUE, associated with parameter name PARNAM is
*     obtained from the enviroment. The value in DEFAUL is suggested to the
*     enviroment as a default, if a default value has already been suggested
*     for this parameter then this attempt will fail and the previous value
*     will be kept. The value to be returned is tested for being in the range
*     MIN to MAX, the user being reprompted until a suitable value is obtained.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL AIF_GODDN( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHARACTER*(*)( READ )
*           Parameter name associated with returned value VALUE.
*     DEFAUL = INTEGER( READ )
*           This value will be suggested to the parameter system as a default
*           if no default has already been set.
*     MIN    = INTEGER( READ )
*           The minimum acceptable value for VALUE.
*     MAX    = INTEGER( READ )
*           The maximum acceptable value for VALUE.
*     VALUE  = INTEGER( WRITE )
*           Will only be a valid odd integer within the range MIN to MAX if
*           the STATUS has not been set to an error value.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Suggest DEFAUL to the enviroment as default value, if status is
*        returned with an error value indicating parameter in an invalid
*        state then a default must already have been given for this parameter
*        so just annul the error and continue.
*        Do while no acceptable value given and no error occurs
*           Get integer value, VALUE, associated with parameter name PARNAM
*           If no error then
*              If value is even or outside range MIN to MAX then
*                 Report as an error and cancel parameter value
*              Endif
*           Endif
*        Enddo
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     01/12/1983 : Original version     (ROE::ASOC5)
*     03/06/1984 : AIF_ version created (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE ! switch off default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'PAR_ERR' ! parameter system error constants
*    Import :
      CHARACTER*(*)
     :  PARNAM ! parameter name associated with the value to be returned
      INTEGER
     :  DEFAUL, ! default value to be suggested to the enviroment
     :  MIN,    ! minimum allowed value
     :  MAX     ! maximum allowed value
*    Export :
      INTEGER
     :  VALUE ! value to be obtained from the enviroment, an odd integer
*    Status :
      INTEGER STATUS ! the global status
*    Local variables :
      LOGICAL
     :  NOTOK ! will be true while getting an acceptable value for VALUE
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       suggest the default value to the parameter system
         CALL PAR_DEF0I( PARNAM, DEFAUL, STATUS )

*       check for default already set, that is the status is returned
*       with the "parameter in invalid state" value
         IF( STATUS .EQ. PAR__INVST ) THEN

*          just annul the error and continue
            CALL ERR_ANNUL( STATUS )
         ENDIF

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       keep looping until a good value is obtained or an error occurs
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*          get an integer from the parameter system
            CALL PAR_GET0I( PARNAM, VALUE, STATUS )

*          if no error then check for number being odd and within range
            IF( STATUS .EQ. SAI__OK ) THEN
               IF( ( VALUE .LT. MIN ) .OR. ( VALUE .GT. MAX ) .OR.
     :           ( MOD( VALUE, 2 ) .EQ. 0 ) ) THEN

*                number is out of range or is even, report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'VALUE', VALUE )
                  CALL MSG_SETI(   'MIN',   MIN )
                  CALL MSG_SETI(   'MAX',   MAX )
                  CALL ERR_REP( 'ERR_GODDN',
     :              '^VALUE not allowed, give an odd number in the'/
     :              /' range ^MIN to ^MAX please.', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARNAM, STATUS )
               ELSE

*                number must be odd so terminate the loop
                  NOTOK = .FALSE.
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      END
