      SUBROUTINE COF_NDXT( FUNIT, NHDU, MATCH, IEXT, ICMP, MORE, STATUS)
*+
*  Name:
*     COF_NDXT

*  Purpose:
*     See if this extension is needed

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NDXT( FUNIT, NHDU, MATCH, IEXT, ICMP, MORE, STATUS)

*  Description:
*     The routine checks throgh the EXTABLE lists saved in COMMON for a
*     specifier which matches the HDU on the given Fits unit FUNIT. If a
*     match is found, argument MATCH returns TRUE; otherwise it returns FALSE.
*     If the extension is needed, IEXT and ICMP are returned indexing the
*     extension-set and component within the EXTABLE which matched. LOOP is
*     returned TRUE if any unmatched extension specifiers remain in the table,
*     FALSE otherwise.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Fits unit number
*     NHDU = INTEGER (Given)
*        The current HDU number
*     MATCH = LOGICAL (Returned)
*        Whether the current header matched a specifier in the table
*     IEXT = INTEGER (Returned)
*        The matching extension set number
*     ICMP = INTEGER (Returned)
*        The matching component number
*     MORE = LOGICAL (Returned)
*        Whether there are any unused entries in the EXTABLE
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  Prior Requirements:
*     -  The Fits unit must be opened
*     -  The EXTABLE entries must be saved in COMMON
*     [routine_prior_requirements]...

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...
*     [facility_or_package]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) {year} Central Laboratory of the Research Councils

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     21-AUG-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS standard constants
      
*  Global Variables:
      INCLUDE 'F2NDF1_CMN'      ! EXTABLE lists
*        NCMP = INTEGER    (Read)
*           Number of component lines in EXTABLE
*        NEXTS = INTEGER   (Read)
*           Number of extension sets in EXTABLE
*        EXTNS(MAXCMP,MAXEXT) = CHARACTER*32 (Read) 
*           Extension table from EXTABLE
*        DONE(MAXCMP,MAXEXT) = LOGICAL (Read and Write)
*           If extension specifier matched
      
*  Arguments Given:
      INTEGER FUNIT
      INTEGER NHDU
      
*  Arguments Returned:
      LOGICAL MATCH
      INTEGER IEXT
      INTEGER ICMP
      LOGICAL MORE
      
*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      
*  Local Constants:
      
*  Local Variables:
      INTEGER I       ! Loop counter
      INTEGER J       ! Loop counter
      INTEGER UNUSED  ! Unused extension counter
      INTEGER EXTN    ! Numeric extension specifier
      
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise return value to FALSE (no match)
      MATCH = .FALSE.      
*  Initialise UNUSED counter
      UNUSED = 0

*  Loop through the components of each extension set in turn
*  We continue looping unless both MATCH and UNUSED.GT.0 are TRUE - this
*  ensures that MORE reflects unused extension specifiers following any match.
      DO J = 1, NEXTS
         DO I = 1, NCMP
            IF ( .NOT. DONE(I,J) ) THEN
*  Count requested extensions still to find
               UNUSED = UNUSED + 1

*  If we already have a match, exit the loop
               IF ( MATCH ) GOTO 99

*  No match yet - see if this is a match
*  First see if it's a number extension specifier
               CALL CHR_CTOI( EXTNS(I,J), EXTN, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
*  It is a number
                  IF( EXTN .EQ. NHDU ) THEN
*  It is the right number
                     MATCH = .TRUE.
                     DONE(I,J) = .TRUE.
                     ICMP = I
                     IEXT = J
*  Discount this unused specifier
                     UNUSED = UNUSED - 1
                     IF ( UNUSED .GT. 0 ) GOTO 99
                  END IF  ! the right number

               ELSE
*  Not a number - assume KEYWORD=VALUE
                  CALL ERR_ANNUL( STATUS )
                  CALL COF_CHKXT( FUNIT, EXTNS(I,J), MATCH, STATUS )

*  If the specifier matches the header, get out of loop
                  IF ( ( STATUS .EQ. SAI__OK ) .AND. MATCH ) THEN
                     DONE(I,J) = .TRUE.
                     ICMP = I
                     IEXT = J
                     MATCH = .TRUE.
*  Discount this unused specifier
                     UNUSED = UNUSED - 1
                     IF ( UNUSED .GT. 0 ) GOTO 99
                  END IF
               END IF  ! assumed keyword form
            END IF  ! not done
         END DO  ! each component
      END DO  ! each extension set
              
99    CONTINUE
      MORE = UNUSED .GT. 0

      END
