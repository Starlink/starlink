*+  AIF_EXACC - Obtain exact number of CHARACTER*(*) values from parameter system
      SUBROUTINE AIF_EXACC( PARNAM, NVALS, VALUES, STATUS )
*    Description :
*     This routine obtains an exact number of CHARACTER*(*) values from the parameter
*     system. If in any one request less than the required number of values
*     are obtained the routine will keep requesting until it gets the required
*     number of values (or until an error occurs). Up to 100 values can
*     be obtained using this routine.
*    Invocation :
*      CALL AIF_EXACC( PARNAM, NVALS, VALUES, STATUS )
*    Parameters :
*     PARNAM         = CHARACTER*(*)( READ )
*           Name of the parameter associated with the requested string
*     NVALS          = INTEGER( READ )
*           The exact number of values needed. Values will be requested
*           until this quantity (no more and no less) have been obtained.
*     VALUES( NVALS )= CHARACTER*(*)( WRITE )
*           The values obtained from the parameter system.
*     STATUS         = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*    Method :
*     If no error on entry then
*        If the number of values needed does not exceed the maximum then
*            initialise the number of values obtained to zero
*            initialise number of values needed to the number of values requested
*            Loop until no more values are needed or until an error occurs
*               If values are still needed then
*                  obtain values associated with parameter
*                  If no error has occurred then
*                     put the numbers obtained into the VALUES array
*                     increment number of values obtained altogether
*                     decrement number of values needed
*                     If values are still required then
*                        give a message indicating the number still needed
*                        cancel the parameter
*                     Endif
*                  Else
*                     terminate the loop and exit with the error status
*                  Endif
*               Endif
*           Endloop
*           supply the complete VALUES array back to the parameter system
*        Else
*           report an error
*        Endif
*     Endif
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (ROE::SMB)
*    History :
*     13/03/1984: original version             (ROE::SMB)
*     10/06/1984: generic AIF_ version         (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE         ! must declare all variables
*    Global constants :
      INCLUDE 'SAE_PAR'     ! SSE global variables
*    Import :
      CHARACTER*(*)
     :  PARNAM         ! contains parameter name associated with string
      INTEGER
     :  NVALS          ! number of values to obtain
*    Export :
      CHARACTER*(*)
     :  VALUES (NVALS) ! contains the values obtained from parameter system
*    Status :
      INTEGER
     :  STATUS         ! global status
*    Local constants :
      INTEGER MAXVAL   ! maximum number of values which can be obtained
      PARAMETER ( MAXVAL = 100 )
*    Local variables :
      INTEGER
     :  NOBT,          ! number of values obtained in one request
     :  NGOT,          ! number of values obtained so far
     :  NEEDED,        ! number of values still needed
     :  IVAL           ! loop counter
      CHARACTER*200
     :  WORK (MAXVAL)  ! work array in which to obtain values
*-

*   check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*      ensure the number of values needed does not exceed the maximum
         IF ( NVALS .LE. MAXVAL ) THEN

*         initialise the number of values obtained to zero and the
*         number of values needed to the number of values requested

            NGOT   = 0
            NEEDED = NVALS

*         loop until no more values are needed
*         or until an error occurs

            DO WHILE ( ( NEEDED .GT. 0 ) .AND. ( STATUS .EQ. SAI__OK ) )

*            check if values are needed
               IF ( NEEDED .GT. 0 ) THEN

*               obtain values associated with parameter
                  CALL PAR_GETVC( PARNAM, NEEDED, WORK, NOBT, STATUS )

*               check the status return

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                  put the numbers obtained into the VALUES array

                     DO IVAL = 1,NOBT
                        VALUES (NGOT+IVAL) = WORK (IVAL)
                     ENDDO

*                  add the number of values obtained here to the
*                  number of values obtained altogether,
*                  and subtract the number of values obtained
*                  from the number of values needed

                     NGOT   = NGOT   + NOBT
                     NEEDED = NEEDED - NOBT

*                  if values are still required then indicate this
*                  and cancel the parameter

                     IF ( NEEDED .GT. 0 ) THEN

                        CALL MSG_SETI( 'NEEDED', NEEDED )
                        IF ( NEEDED .EQ. 1 ) THEN
                           CALL MSG_SETC( 'WORDS', 'value is' )
                        ELSE
                           CALL MSG_SETC( 'WORDS', 'values are' )
                        ENDIF
                        CALL MSG_OUT( 'AIF_EXAC_NEEDED', '^NEEDED '/
     :                    /'more ^WORDS still needed.', STATUS )

                        CALL PAR_CANCL( PARNAM, STATUS )

                     ENDIF

                  ELSE

*                  some miscellaneous error has occurred
*                  terminate the loop and exit with the error status

                     NEEDED = 0

                  ENDIF

               ENDIF

            ENDDO

*            supply the complete VALUES array back to the parameter system
               CALL PAR_PUTVC( PARNAM, NVALS, VALUES, STATUS )

         ELSE

*      number of values requested exeeds the maximum
*      report the error

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NVALS', NVALS )
         CALL MSG_SETI( 'MAXVAL', MAXVAL )
         CALL ERR_REP( 'AIF_EXAC_EXMX', 'AIF_EXACC: Request for '/
     :     /'^NVALS exceeds maximum of ^MAXVAL.', STATUS )

         ENDIF

      ENDIF

      END
