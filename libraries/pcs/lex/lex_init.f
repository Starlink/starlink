*+  name - LEX_INIT
*    Description :
      SUBROUTINE LEX_INIT(NSTATE,TABLE)
*     Initialize a state table for the LEX parser
*    Invocation :
*     CALL LEX_INIT(NSTATE,TABLE)
*    Parameters :
*     NSTATE = INTEGER (given)
*           The number of states in the state table
*     TABLE(4,0:127,NSTATE) = BYTE (returned)
*           The state table
*    Method :
*     The state table is filled with entries which cause the
*     parser to signal an error. Valid state transitions will
*     be subsequently overwritten by calls to the LEX_SET
*     routine.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Jeremy Bailey (AAOEPP::JAB) 8 Jan 1987
*    History :
*     date:  changes (institution::username)
*     
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*     <any INCLUDE files containing global constant definitions>
*    Import :
      INTEGER NSTATE
*    Export :
      BYTE TABLE(4,0:127,NSTATE)
*    Local variables :
      INTEGER I,C
*-

      DO I=1,NSTATE
         DO C=0,127
            TABLE(1,C,I)=0
            TABLE(2,C,I)=-1
            TABLE(3,C,I)=0
            TABLE(4,C,I)=0
         ENDDO
      ENDDO

      END

