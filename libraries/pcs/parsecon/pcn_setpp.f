*+  PARSECON_SETPP - Sets-up parameter prompt search-path
      SUBROUTINE PARSECON_SETPP ( ENTRY, STATUS )
*    Description :
*     Interprets the provided string as a PPATH specification, 
*     and adds it into the PPATH store for the most recently declared 
*     program parameter.
*    Invocation :
*     CALL PARSECON_SETPP ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           PPATH specifier
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, and the 
*     result is interpreted as a set of path specifiers which are encoded 
*     into the array holding PPATH.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     05.05.1987:  Original (REVAD::BDK)
*     16.10.1990:  Use CHR for upper case (RLVAD::AJC)
*     25.06.1991:  STRING_ARRCHAR changed to PARSECON_* (RLVAD::AJC)
*     25.02.1991:  Report errors (RLVAD::AJC)
*     26.02.1992:  _ARRCHAR no longer capitalizes (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) ENTRY             ! the PPATH string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    External references :
*     None

*    Local variables :
      CHARACTER*80 VALUE              ! PPATH string with quotes removed

      INTEGER POS                     ! loop counter for PPATH step

      CHARACTER*15 STEPS(5)           ! split PPATH string

      INTEGER LENSTEPS(5)             ! length of STEPS strings

      INTEGER COUNT                   ! number of steps
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Remove the quotes from ENTRY and force to uppercase.
      CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
      CALL CHR_UCASE( VALUE )

*   Split the string up into the set of path specifiers
      CALL PARSECON_ARRCHAR ( VALUE, 5, COUNT, STEPS, LENSTEPS,
     :  STATUS )

*   Blank the search path for the latest parameter
      DO POS = 1, 5

         PARPPATH(POS,PARPTR) = SUBPAR__NOPATH

      ENDDO

*   Load encoded version of the PPATH
      DO POS = 1, COUNT
         CALL CHR_UCASE ( STEPS(POS) )
         IF ( STEPS(POS) .EQ. 'CURRENT' ) THEN
            PARPPATH(POS,PARPTR) = SUBPAR__CURRENT
         ELSE IF ( STEPS(POS) .EQ. 'DEFAULT' ) THEN
            PARPPATH(POS,PARPTR) = SUBPAR__DEFAULT
         ELSE IF ( STEPS(POS) .EQ. 'DYNAMIC' ) THEN
            PARPPATH(POS,PARPTR) = SUBPAR__DYNAMIC
         ELSE IF ( STEPS(POS) .EQ. 'GLOBAL' ) THEN
            PARPPATH(POS,PARPTR) = SUBPAR__GLOBAL
         ELSE
            STATUS = PARSE__IVPPATH
            CALL EMS_REP ( 'PCN_SETPP1',
     :      'PARSECON: Illegal item in PPATH', STATUS )
         ENDIF

      ENDDO

      END
