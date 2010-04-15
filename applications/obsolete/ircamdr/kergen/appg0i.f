*+  APPG0I - to obtain an integer scalar between certain limits
      SUBROUTINE APPG0I( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Description :
*     A value is obtained for the integer variable VALUE, associated with
*     parameter name PARNAM, which is in the range MIN <= VALUE <= MAX. The
*     value given as DEFAUL is suggested to the data system as the default
*     value for VALUE, if a defualt has already been suggested for this
*     parameter this attempt will fail and the old value will be kept.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL APPG0I( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHAR*(*)( READ )
*           Parameter name associated with VALUE.
*     DEFAUL = INTEGER( READ )
*           Default value to be suggested to the parameter system if no default
*           value has already been set.
*     MIN    = INTEGER( READ )
*           Minimum allowed value for VALUE.
*     MAX    = INTEGER( READ )
*           Maximum allowed value for VALUE.
*     VALUE  = INTEGER( UPDATE )
*           Value returned will only be valid if STATUS is not set to an error
*           value.
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
*           Get a value from the parameter system.
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
      INTEGER
     :  DEFAUL, ! application suggested default value for VALUE
     :  MIN,    ! minimum acceptable value for VALUE
     :  MAX     ! maximum      "       "    "   "
*    Export :
      INTEGER
     :  VALUE ! variable to contain value to be returned
*    Status :
      INTEGER STATUS
*    Local variables :
      LOGICAL
     :  NOTOK ! true while no acceptable value obtained
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       suggest value in DEFAUL to data system as dynamic default
         CALL PAR_DEF0I( PARNAM, DEFAUL, STATUS )

*       check for a dynamic default having already been set
         IF( STATUS .EQ. PAR__INVST ) THEN

*           clear the error and continue
             CALL ERR_ANNUL( STATUS )
         ENDIF

*       loop will keep going as long as dont have o.k. value and no error
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*          get parameter value
            CALL PAR_GET0I( PARNAM, VALUE, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             check for value is within specified range
               IF( ( VALUE .LT. MIN ) .OR. ( VALUE .GT. MAX ) ) THEN

*                report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'VALUE', VALUE )
                  CALL MSG_SETI( 'MIN', MIN )
                  CALL MSG_SETI( 'MAX', MAX )
                  CALL ERR_REP( 'ERR_APPG0I',
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
