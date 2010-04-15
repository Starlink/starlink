*+  AIF_GET0L - Returns a scalar of type LOGICAL in a given range.
      SUBROUTINE AIF_GET0L( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Description :
*     A value is obtained for the LOGICAL variable VALUE, associated with
*     the parameter name PARNAM, which is in the range MIN <= VALUE <= MAX.
*     The value given as DEFAUL is suggested to the data system as the
*     default value for VALUE, if a defualt has already been suggested for
*     this parameter this attempt will fail and the old value will be kept.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL AIF_GET0L( PARNAM, DEFAUL, MIN, MAX, VALUE, STATUS )
*    Parameters :
*     PARNAM = CHARACTER*(*)( READ )
*           Parameter name associated with the value to be obtained.
*     DEFAUL = LOGICAL( READ )
*           Default value to be suggested to the parameter system if no
*           default value has already been set.
*     MIN    = LOGICAL( READ )
*           Minimum allowed value for value to be obtained.
*     MAX    = LOGICAL( READ )
*           Maximum allowed value for value to be obtained.
*     VALUE  = LOGICAL( WRITE )
*           Value returned will only be valid if STATUS is not set to an
*           error value.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this has an error value on
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
*     30/11/1983 : Modified for release 0.5                 (ROE::ASOC5)
*     02/06/1984 : Modified to become generic AIF_GET0 routines (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE ! switch off default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'PAR_ERR' ! parameter system error constants
*    Import :
      CHARACTER*(*)
     :  PARNAM ! parameter name associated with value to be obtained
      LOGICAL
     :  DEFAUL, ! suggested default value for value to be obtained
     :  MIN,    ! minimum acceptable value for value to be obtained
     :  MAX     ! maximum      "       "    "    "    "  "     "
*    Export :
      LOGICAL
     :  VALUE ! value only valid if STATUS does not have an error value
*    Status :
      INTEGER STATUS ! global status
*    Local variables :
      LOGICAL
     :  NOTOK ! true while no acceptable value obtained
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise NOTOK to start off the loop
         NOTOK = .TRUE.

*       suggest value in DEFAUL to data system as dynamic default
         CALL PAR_DEF0L( PARNAM, DEFAUL, STATUS )

*       check for a dynamic default having already been set
         IF( STATUS .EQ. PAR__INVST ) THEN

*           clear the error and continue
             CALL ERR_ANNUL( STATUS )
         ENDIF

*       loop will keep going as long as dont have o.k. value and no error
         DO WHILE( NOTOK .AND. ( STATUS .EQ. SAI__OK ) )

*          get parameter value
            CALL PAR_GET0L( PARNAM, VALUE, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             check for value is within specified range
*             Note that a logical can only have two values
*             Note also that this is generic-generated and
*             is not used so should not be generated if we
*             every try to regenerate from generic source
               IF( ( VALUE .EQV. MIN ) .OR. ( VALUE .EQV. MAX ) ) THEN

*                report as an error
                  STATUS = SAI__ERROR
                  CALL MSG_SETL( 'VALUE', VALUE )
                  CALL MSG_SETL( 'MIN', MIN )
                  CALL MSG_SETL( 'MAX', MAX )
                  CALL ERR_REP( 'ERR_AIFG0L',
     :              '^VALUE is outside allowed range.'/
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
