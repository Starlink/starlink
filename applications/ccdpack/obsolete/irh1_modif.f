      SUBROUTINE IRH1_MODIF( IDH1, SIZE1, IDH2, ELEM, DEPTH, FILE,
     :                       STATUS )
*+
*  Name:
*     IRH1_MODIF

*  Purpose:
*     Expand a modification element and append the names to a given
*     group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_MODIF( IDH1, SIZE1, IDH2, ELEM, DEPTH, FILE, STATUS )

*  Description:
*     A modification element must contain the IRH_ control character
*     specified by IRH__MODNM. This character is a token representing
*     the names from the input group. The element may also contain
*     three occurences of the control character specified by
*     IRH__MODSP. This character is used to seperate the old and new
*     substitution strings. Up to four strings are thus extracted from
*     the given element:
*
*     1) Any non-blank string lying before the name token character.
*        This is used as a prefix for the names contained in the group
*        identified by IDH1.
*
*     2) Any non-blank string lying after the name token character but
*        before the first seperator character. This is used as a suffix
*        for the names contained in the group identified by IDH1.
*
*     3) The string contained between the first and second seperator
*        characters. The names in the input group are searched (before
*        addition of suffix and prefix) for this string. All occurences
*        of it are replaced by the 4th string. If no seperator
*        characters are contained in the given element, no substitution
*        is performed.
*
*     4) The string contained between the second and third seperator
*        characters. This string replaces all occurences of the 3rd
*        string.
*
*     All these strings are converted to upper case before being used.
*     Once these strings have been found, each name is obtained from
*     the input group identified by IDH1. All occurences of the 3rd
*     string are replaced by the 4th string, and the name is extended
*     by adding the prefix and suffix.  The resulting name is appended
*     to the output group.
*
*  Arguments:
*     IDH1 = INTEGER (Given)
*        An IRH identifier for the group to be used as the basis for
*        the output group. A modified form of each name in this group
*        is appended to the group identified by IDH2.
*     SIZE1 = INTEGER (Given)
*        The maximum index to use from the group identified by IDH1. Any
*        names with indices higher than SIZE1 will not give rise to
*        corresponding names in the output group.
*     IDH2 = INTEGER (Given)
*        An IRH identifier for the output group.
*     ELEM = CHARACTER (Given)
*        The text of the modification element.
*     DEPTH = INTEGER (Given)
*        The indirection depth at which the modification element was
*        given.
*     FILE = CHARACTER (Given)
*        The indirection file in which the modification element was
*        given.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (DSB):
*        Original version.
*     7-NOV-1991 (DSB):
*        Conversion of character strings to upper case included.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference. Added DAT_PAR.
*     28-FEB-1992 (PDRAPER):
*        Added conditional case sensitivity.
*     13-MAR-1992 (PDRAPER):
*        Changed addition of suffixes and prefixes to avoid overlapping
*        string elements.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      include 'IRH_COM'          ! IRH common blocks.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH1
      INTEGER SIZE1
      INTEGER IDH2
      CHARACTER ELEM*(*)
      INTEGER DEPTH
      CHARACTER FILE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a string.


*  Local Variables:
      INTEGER   DEP1             ! Indirection depth of name from IDH1.
      CHARACTER FILE1*(IRH__SZNAM)! Indirection file of name from IDH1.
      INTEGER   I                ! Loop count.
      INTEGER   LPREF            ! Length of the prefix to be added to
                                 ! the input names beore substitution.
      INTEGER   LSUFF            ! Length of the suffix to be added to
                                 ! the input names beore substitution.
      INTEGER   MODGP1           ! Modified group of name from IDH1.
      INTEGER   MODIN1           ! Modified index of name from IDH1.
      CHARACTER NAME*(IRH__SZNAM)! Input name.
      CHARACTER NEW*(IRH__SZNAM) ! The text to be substituted for the
                                 ! text held in OLD.
      INTEGER   NEWLEN           ! Used length of NEW.
      CHARACTER NEWNAM*(IRH__SZNAM)! Name after substitution of NEW for
                                 ! OLD.
      INTEGER   NLEN             ! Used length of NEWNAM.
      INTEGER   NSUB             ! No. of substitutions made.
      CHARACTER OLD*(IRH__SZNAM) ! The string to be replaced by NEW.
      INTEGER   OLDLEN           ! Used length of OLD.
      CHARACTER PREF*(IRH__SZNAM)! The prefix to be added to the input
                                 ! names beore substitution.
      INTEGER   SEP1             ! The position of the first seperator
                                 ! character within the given element.
      INTEGER   SEP2             ! The position of the second seperator
                                 ! character within the given element.
      INTEGER   SEP3             ! The position of the third seperator
                                 ! character within the given element.
      CHARACTER SUFF*(IRH__SZNAM)! The suffix to be added to the input
                                 ! names beore substitution.
      INTEGER   TOKEN            ! Position of the name token character
                                 ! within the given element.
      LOGICAL   WARNED           ! True if the user has been warned
                                 ! about possible truncation of  names.
      INTEGER   IAT              ! Position of string insertion.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the control character used as a token for the input names.
      TOKEN = INDEX( ELEM, IRH__MODNM ) 
      IF( TOKEN .EQ. 0 ) THEN
         STATUS = IRH__BADME
         GO TO 999
      END IF

*  Identify any input name prefix included in the modification element.
*  Change to upper case if enabled for case insensitive comparisons.
      IF( TOKEN .GT. 1 ) THEN      
         PREF = ELEM( : TOKEN - 1 )
         IF ( IRH__UCASE ) CALL CHR_UCASE( PREF )
         LPREF = TOKEN - 1
      ELSE
         LPREF = 0
      ENDIF

*  See if there is a substitution to be made. If so, get the string to
*  be replaced, and the string with which to replace replace it.      
      SEP1 = INDEX( ELEM, IRH__MODSP )
      IF( SEP1 .GT. 0 ) THEN

         IF( SEP1 .LT. TOKEN ) THEN
            STATUS = IRH__BADME
            GO TO 999
         END IF            

         SEP2 = INDEX( ELEM( SEP1 + 1 : ), IRH__MODSP ) + SEP1
         IF( SEP2 .LE. SEP1 + 1 ) THEN
            STATUS = IRH__BADME
            GO TO 999
         END IF            

         SEP3 = INDEX( ELEM( SEP2 + 1 : ), IRH__MODSP ) + SEP2
         IF( SEP3 .LE. SEP2 + 1 ) THEN
            STATUS = IRH__BADME
            GO TO 999
         END IF            

         OLD = ELEM( SEP1 + 1 : SEP2 - 1 )
         IF ( IRH__UCASE ) CALL CHR_UCASE( OLD )
         OLDLEN = SEP2 - SEP1 - 1
         NEW = ELEM( SEP2 + 1 : SEP3 - 1 )
         IF ( IRH__UCASE ) CALL CHR_UCASE( NEW )
         NEWLEN = SEP3 - SEP2 - 1

      END IF

*  Identify any input name suffix included in the modification element.
      IF( SEP1 .EQ. 0 ) THEN
         LSUFF = CHR_LEN( ELEM ) - TOKEN
      ELSE
         LSUFF = SEP1  - TOKEN - 1 
      END IF

      IF( LSUFF .GT. 0 ) THEN
         SUFF = ELEM( TOKEN + 1 : TOKEN + LSUFF )
         IF ( IRH__UCASE ) CALL CHR_UCASE( SUFF )
      END IF

*  Indicate that no warning mesage has yet been given about truncation
*  of names.
      WARNED = .FALSE.

*  Add a modified version of each name in the input group with index
*  less than or equal to SIZE1, to the end of the output group.
      DO I = 1, SIZE1

*  Get the next name from the input group.
         CALL IRH1_GTELM( IDH1, I, NAME, DEP1, FILE1, MODGP1,
     :                    MODIN1, STATUS )

*  ...make the substitution if one was specified.
         IF( SEP1 .GT. 0 ) THEN
            CALL IRM_SUBST( NAME, OLD(:OLDLEN), NEW(:NEWLEN), .TRUE.,
     :                         NEWNAM, NSUB, STATUS )

         ELSE

*  Name remains the same.
            NEWNAM = NAME
         END IF
      
*  Add the prefix if one was given.
         IF( LPREF .GT. 0 ) THEN
            IAT = 0
            CALL CHR_APPND( PREF( : LPREF ), NAME, IAT )
            CALL CHR_APPND( NEWNAM, NAME, IAT )
            NEWNAM = NAME
         END IF

*  Add the suffix if one was given.
         IF( LSUFF .GT. 0 ) THEN
            IAT = 0
            CALL CHR_APPND( NEWNAM, NAME, IAT )
            CALL CHR_APPND( SUFF( : LSUFF ), NAME, IAT )
            NEWNAM = NAME
         END IF

*  Get the used length of the new name.
         NLEN = CHR_LEN( NEWNAM )

*  Give a warning message if the expanded name may potentially be
*  truncated.
         IF( NLEN .EQ. IRH__SZNAM ) THEN

            IF( .NOT.WARNED ) THEN
               WARNED = .TRUE.
               CALL MSG_BLANK( STATUS )
               CALL MSG_SETC( 'ELEM', ELEM )
               CALL MSG_OUT( 'IRH1_MODIF_MSG1',
     : 'The following names (specified by the modification element '//
     : '"^ELEM") may have suffered truncation:', STATUS )
            END IF                  

            CALL MSG_SETC( 'NAME', NEWNAM )
            CALL MSG_OUT( 'IRH1_MODIF_MSG2', '  ^NAME', STATUS )

         END IF

*  Append the new name to the end of the output group.
         CALL IRH1_PTELM( IDH2, 0, NEWNAM(:NLEN), DEPTH, FILE, IDH1,
     :                    I, STATUS )

*  Do the next name from the input group.
      END DO

*  If a warning was given, output a blank line.
 999  CONTINUE
 
      IF( WARNED ) CALL MSG_BLANK( STATUS )

*  If an error occured, give a report.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL ERR_REP( 'IRH1_MODIF_ERR1',
     :        'IRH1_MODIF: Unable to expand modification element ^ELEM',
     :        STATUS )
      END IF

      END
* $Id$
