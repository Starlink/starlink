      SUBROUTINE KPG1_ASSTY( SETTNG, NAME, VALUE, STATUS )
*+
*  Name:
*     KPG1_ASSTY

*  Purpose:
*     Check for synonyms and colour names in AST attribute settings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSTY( SETTNG, NAME, VALUE, STATUS )

*  Description:
*     This routine splits the supplied AST attribute setting string up
*     into a name and a value, replacing synonyms for AST attribute names
*     or qualifiers with the corresponding AST names and qualifiers, and
*     replacing colour names within the attribute value with corresponding 
*     PGPLOT colour indices. Synonyms for AST attribute names or
*     qualifiers are set up using KPG1_ASPSY.
*
*     Matching of attribute names and attribute qualifiers are performed 
*     separately. The names are matched first. An attempt to match any 
*     supplied attribute qualifier against a synonym is only made if the 
*     attribute names match, or if the synonym does not contain an 
*     attribute name. Synonyms may specify minimum abbreviations for 
*     attribute qualifiers by including an asterisk within the qualifier
*     string. The asterisk marks the end of the minimum abbreviation.

*  Arguments:
*     SETTNG = CHARACTER * ( * ) (Given)
*        The text to be checked, potetially containing synonyms and
*        colour names.
*     NAME = CHARACTER * ( * ) (Returned)
*        The corresponding AST attribute name (with a possible qualifier).
*     VALUE = CHARACTER * ( * ) (Returned)
*        The corresponding AST attribute value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! HDS constants (needed by KPG_AST)
      INCLUDE 'CTM_PAR'          ! CTM_ Colour-Table Management constants

*  Arguments Given:
      CHARACTER SETTNG*(*)

*  Arguments Returned:
      CHARACTER NAME*(*)
      CHARACTER VALUE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTING = INTEGER (Read)
*           GRP identifier for group holding synonyms.
*        ASTNPS = INTEGER (Read)
*           Number of defined synonyms.
*        ASTOUG = INTEGER (Read)
*           GRP identifier for group holding corresponding AST attribute names.

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL KPG1_SHORT         ! Is string an allowed abbreviation?
      LOGICAL CHR_SIMLR          ! Are strings equal apart from case?

*  Local Variables:
      CHARACTER ANAME*(GRP__SZNAM)! Attribute name to return
      CHARACTER AQUAL*(GRP__SZNAM)! Attribute qualifier to return
      CHARACTER SY*(GRP__SZNAM)   ! synonym
      CHARACTER TRAN*(GRP__SZNAM) ! Translation
      INTEGER CL                 ! Index of closing parenthesis in synonym
      INTEGER CL1                ! Index of closing parenthesis in translation
      INTEGER CL2                ! Index of closing parenthesis in text
      INTEGER COLIND             ! Colour index
      INTEGER DOWN               ! The lowest colour index available
      INTEGER EQUALS             ! Index of equals sign
      INTEGER IAT                ! Significant length of text2
      INTEGER ID                 ! PGPLOT device identifier (zero if none)
      INTEGER J                  ! Synonym index
      INTEGER NCH                ! No. of characters used to format COLIND
      INTEGER NPLEN              ! Length of attribute name in synonym
      INTEGER NRLEN              ! Length of attribute name in translation
      INTEGER NTLEN              ! Length of attribute name in supplied text
      INTEGER OP                 ! Index of opening parenthesis in synonym
      INTEGER OP1                ! Index of opening parenthesis in translation
      INTEGER OP2                ! Index of opening parenthesis in text
      INTEGER UP                 ! The highest colour index available
      LOGICAL MATCH              ! Does synonym match text?
      LOGICAL VALID1             ! Is synonym group identifier valid?
      LOGICAL VALID2             ! Is attribute name group identifier valid?
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Extract the name/qualifier, and value from the supplied setting string.
*  =======================================================================

*  See if there is an equals in the supplied string.
      EQUALS = INDEX( SETTNG, '=' )

*  If no equals sign is found return a balnk value.
      IF( EQUALS .EQ. 0 ) THEN
         NAME = SETTNG
         VALUE = ' '
         GO TO 999
      END IF

*  Store everything before the equals (except spaces) as the attribute name
*  and (maybe) qualifier. Return if it is blank.
      IF( EQUALS .EQ. 1 .OR. SETTNG( : EQUALS - 1 ) .EQ. ' ' ) THEN
         NAME = ' '
         VALUE = SETTNG
         GO TO 999

      ELSE
         NAME = SETTNG( : EQUALS - 1 )
         CALL CHR_RMBLK( NAME )
         CALL CHR_UCASE( NAME )
      END IF

*  Store everything after the equals as the attribute value.
      IF( EQUALS .LT. LEN( SETTNG ) ) THEN
         VALUE = SETTNG( EQUALS + 1 : )
      ELSE
         VALUE = ' '
      END IF

*  Replace synonyms in the attribute name/qualifier.
*  =================================================

*  See if any synonymns are available. This is the case if both group 
*  identifiers supplied in common (KPG_AST) are valid.
      CALL GRP_VALID( ASTING, VALID1, STATUS )
      CALL GRP_VALID( ASTOUG, VALID2, STATUS )
      IF( VALID1 .AND. VALID2 ) THEN

*  Check for each synonym in turn. 
         DO J = 1, ASTNPS

*  Get the synonym and its corresponding attribute name.
            CALL GRP_GET( ASTING, J, 1, SY, STATUS )
            CALL GRP_GET( ASTOUG, J, 1, TRAN, STATUS )

*  See if the synonym contains a qualifier (i.e. a string in parenthesise).
            CALL KPG1_PRNTH( SY, OP, CL, STATUS )

*  Get the length of the attribute name (i.e. the string in front of
*  any qualifier) in the synonym.
            IF( OP .EQ. 0 ) THEN  
               NPLEN = CHR_LEN( SY )
            ELSE
               NPLEN = OP - 1
            END IF

*  See if the synonym translation contains a qualifier.
            CALL KPG1_PRNTH( TRAN, OP1, CL1, STATUS )

*  Get the length of the attribute name in the translation.
            IF( OP1 .EQ. 0 ) THEN  
               NRLEN = CHR_LEN( TRAN )
            ELSE
               NRLEN = OP1 - 1
            END IF

*  See if the supplied name/qualifier contains a qualifier.
            CALL KPG1_PRNTH( NAME, OP2, CL2, STATUS )

*  Get the length of the attribute name in the supplied text.
            IF( OP2 .EQ. 0 ) THEN  
               NTLEN = CHR_LEN( NAME )
            ELSE
               NTLEN = OP2 - 1
            END IF

*  Indicate that as yet we have no attribute name or qualifier to return.
            ANAME = ' '
            AQUAL = ' '

*  If the synonym contains an attribute name, see if it matches the
*  supplied attribute name.
            IF( NPLEN .GT. 0 ) THEN
               IF( NTLEN .GT. 0 ) THEN
                  IF( SY( : NPLEN ) .EQ. NAME( : NTLEN ) ) THEN
                     MATCH = .TRUE.
                  ELSE
                     MATCH = .FALSE.
                  END IF
               ELSE
                  MATCH = .FALSE.
               END IF

*  If it does, and if the translation contains an attribute name, return the 
*  attribute name from the translation. Otherwise, use the supplied name.
               IF( MATCH .AND. NRLEN .GT. 0 ) THEN
                  ANAME = TRAN( : NRLEN )

               ELSE IF( NTLEN .GT. 0 ) THEN
                  ANAME = NAME( : NTLEN )

               END IF

*  If the synonym does not contain an attribute name, use the attribute
*  name from the supplied text (if any). A blank synonym name matches
*  all supplied names, so set MATCH = .TRUE..
            ELSE IF( NTLEN .GT. 0 ) THEN
               ANAME = NAME( : NTLEN )
               MATCH = .TRUE.
            END IF

*  If the synonym name did not match the supplied name, then use the
*  supplied qualifier (if any).
            IF( .NOT. MATCH ) THEN
               IF( CL2 .GT. 0 ) AQUAL = NAME( OP2 : CL2 )
                 
*  Otherwise, if the supplied text contains a qualifier, we need to 
*  compare the qualifier with any qualifier included in the synonym.
            ELSE IF( CL2 .GT. 0 ) THEN

*  If the synonym does not contain a qualifier, or if the supplied
*  qualifier is blank, then use the qualifier as supplied.
               IF( CL .LE. OP + 1 .OR. CL2 .EQ. OP2 + 1 ) THEN
                  AQUAL = NAME( OP2 : CL2 )

*  If the synonym contains a qualifier, see if it matches the supplied
*  qualifier, allowing minimum abbreviations indicated by an asterisk in 
*  the synonym. The comparison is case insensitive. If it does, use the 
*  qualifier from the synonym translation (if any).
               ELSE IF( KPG1_SHORT( SY( OP + 1 : CL - 1 ),
     :                              NAME( OP2 + 1 : CL2 - 1 ),
     :                              '*', .FALSE., STATUS ) ) THEN
                  IF( OP1 .GT. 0 ) AQUAL = TRAN( OP1 : CL1 )

*  If the qualifiers from the synonym and supplied text are different,
*  use the qualifier form the supplied text.
               ELSE
                  AQUAL = NAME( OP2 : CL2 )
               END IF

            END IF

*  Construct a new attribute name + qualifier string.
            NAME = ' '
            IAT = 0
            CALL CHR_APPND( ANAME, NAME, IAT )
            CALL CHR_APPND( AQUAL, NAME, IAT )

         END DO

      END IF

*  Replace colour names in the attribute value
*  ===========================================

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999    

*  This setting sets a colour attribute if the name/qualifier contains 
*  either of the strings COLOR or COLOUR.
      IF( INDEX( NAME, 'COLOR' ) .NE. 0 .OR.
     :    INDEX( NAME, 'COLOUR') .NE. 0 )  THEN

*  See if PGPLOT is currently active. Assume a colour index of zero if not.
         CALL PGQID( ID )
         IF( ID .LE. 0 ) THEN
            COLIND = 0

*  Otherwise, get the number of colour indices available on the current 
*  device, and find the colour index corresponding to the attribute value. 
*  An error will be reported if the attribute value cannot be interpreted 
*  as a colour specification.
         ELSE
            CALL PGQCOL( DOWN, UP )
            CALL KPG1_PGCOL( VALUE, MIN( CTM__RSVPN, UP ), UP, COLIND,
     :                       STATUS )
         END IF

*  If the colour was not known, annul the error. The calling routine will then
*  continue in an attempt to use the colour as an AST attribute value and
*  will fail. An error will then be reported including the whole
*  attribute string.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
   
*  If the colour was found, replace its name with the colour index.
         ELSE
            VALUE = ' '
            CALL CHR_ITOC( COLIND, VALUE, NCH )
         END IF
   
      END IF

 999  CONTINUE

      END
