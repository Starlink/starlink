*+  PARSECON_SETDEF - Insert default parameter value
      SUBROUTINE PARSECON_SETDEF ( ENTRY, STATUS )
*    Description :
*     Enters a static default value for the most recently declared program 
*     parameter
*    Invocation :
*     CALL PARSECON_SETDEF ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           data value
*     STATUS=INTEGER
*    Method :
*     This routine adds a value into the space for static defaults 
*     associated with a program parameter. The common variable PARPTR 
*     has been previously set to point to the relevant parameter. The 
*     type of this parameter is extracted from storage, and compared 
*     with the type deduced from the syntax of the value string in ENTRY. 
*     Any valid type conversions are performed, and the value is stored
*     in the storage lists. The pointers to the parameter's default 
*     values are set up.
*    Deficiencies :
*     Only handles scalars and 1-D arrays. In the case of array 
*     elements, these arrive one at a time and are added to the current 
*     list. The SSE syntax of a 1-D array is (value,value...). The 
*     brackets are delivered to this routine, so in due course it should 
*     be possible to generalise it to handle the n-D syntax, which is
*     (((value,..),(value,..)...),((value,..)..),..).
*     For the time being the incoming brackets are simply ignored.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     20.09.1984:  Original (REVAD::BDK)
*     27.02.1985:  ignore '(' and ')' (REVAD::BDK)
*     06.05.1987:  handle NULL default (REVAD::BDK)
*     16.10.1990:  remove unused declarations (RLVAD::AJC)
*     25.02.1991:  Report errors 
*                  Mark and release error context - annul on '(', ')'
*                  and '!' (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'SUBPAR_PAR'

*    Import :
      CHARACTER*(*) ENTRY

*    Status :
      INTEGER STATUS

*    External references :
*     None

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    Local variables :
      INTEGER DECTYPE                          ! code for type of 
                                               ! declared program 
                                               ! parameter

      LOGICAL STRUCTURE                        ! .TRUE. => name of a 
                                               ! data structure

      REAL VALUE_REAL                          ! temporary storage for 
      INTEGER VALUE_INTEGER                    ! the value
      DOUBLE PRECISION VALUE_DOUBLE
      LOGICAL VALUE_LOGICAL
      CHARACTER*132 VALUE_CHAR

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set an error context
      CALL EMS_MARK

*   Get the type declared for the current parameter
      DECTYPE = PARTYPE(PARPTR)

*   determine what type information can be deduced from the syntax of 
*   the entry to be added, and do any syntax-dependent string 
*   processing. - eg remove single quotes from character constant.
*   Convert the string to the required data type.
      CALL PARSECON_CONVERT ( ENTRY, DECTYPE, VALUE_REAL, VALUE_CHAR, 
     :  VALUE_DOUBLE, VALUE_INTEGER, VALUE_LOGICAL, STRUCTURE, STATUS )

*   Check for acceptable type conversions between the given entry value 
*   and the declared type of the associated action.
*   CONVERT will have generated an error in the case of the ENTRY being 
*   a bracket.
      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( STRUCTURE ) THEN

*         Modify the type to indicate storage in an HDS structure, and copy 
*         the structure-name into character list storage.
            IF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN
               CHARPTR = CHARPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = CHARPTR
               PARDEF(2,PARPTR) = CHARPTR
               PARDEF(3,PARPTR) = 20 + MOD ( DECTYPE, 10 )
               CHARLIST(CHARPTR) = ENTRY
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF1',
     :         'PARSECON: Exceeded storage for STRUCTURE defaults',
     :          STRUCTURE )
            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__CHAR ) THEN

*         String constant. VALUE_CHAR will contain the string with the 
*         delimiting single quotes removed, and double (ie escaped) quotes 
*         contracted.
            IF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN
               CHARPTR = CHARPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = CHARPTR
               PARDEF(2,PARPTR) = CHARPTR
               PARDEF(3,PARPTR) = DECTYPE
               CHARLIST(CHARPTR) = VALUE_CHAR
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF2',
     :         'PARSECON: Exceeded storage for CHARACTER defaults',
     :          STRUCTURE )
            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__REAL ) THEN

            IF ( REALPTR .LT. SUBPAR__MAXLIMS ) THEN
               REALPTR = REALPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = REALPTR
               PARDEF(2,PARPTR) = REALPTR
               PARDEF(3,PARPTR) = DECTYPE
               REALLIST(REALPTR) = VALUE_REAL
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF3',
     :         'PARSECON: Exceeded storage for REAL defaults',
     :          STRUCTURE )
            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__DOUBLE ) THEN

            IF ( DOUBLEPTR .LT. SUBPAR__MAXLIMS ) THEN
               DOUBLEPTR = DOUBLEPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = DOUBLEPTR
               PARDEF(2,PARPTR) = DOUBLEPTR
               PARDEF(3,PARPTR) = DECTYPE
               DOUBLELIST(DOUBLEPTR) = VALUE_DOUBLE
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF4',
     :         'PARSECON: Exceeded storage for DOUBLE defaults',
     :          STRUCTURE )
            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__INTEGER ) THEN

            IF ( INTPTR .LT. SUBPAR__MAXLIMS ) THEN
               INTPTR = INTPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = INTPTR
               PARDEF(2,PARPTR) = INTPTR
               PARDEF(3,PARPTR) = DECTYPE
               INTLIST(INTPTR) = VALUE_INTEGER
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF5',
     :         'PARSECON: Exceeded storage for INTEGER defaults',
     :          STRUCTURE )
            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__LOGICAL ) THEN

            IF ( LOGPTR .LT. SUBPAR__MAXLIMS ) THEN
               LOGPTR = LOGPTR + 1
               IF ( PARDEF(1,PARPTR) .LE. 0 )
     :           PARDEF(1,PARPTR) = LOGPTR
               PARDEF(2,PARPTR) = LOGPTR
               PARDEF(3,PARPTR) = DECTYPE
               LOGLIST(LOGPTR) = VALUE_LOGICAL
            ELSE
               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETDEF6',
     :         'PARSECON: Exceeded storage for LOGICAL defaults',
     :          STRUCTURE )
            ENDIF

         ENDIF

      ELSE IF ( ( ENTRY .EQ. '(' ) .OR. ( ENTRY .EQ. ')' ) ) THEN

*      Should handle multiply dimensioned arrays, but for now just carry on
         CALL EMS_ANNUL ( STATUS )

      ELSE IF ( ENTRY .EQ. '!' ) THEN

*      Default is to put the parameter into the NULL state.
         PARDEF(3,PARPTR) = SUBPAR__NULLTYPE
         CALL EMS_ANNUL ( STATUS )

      ENDIF

*   Release error context
      CALL EMS_RLSE
      END
