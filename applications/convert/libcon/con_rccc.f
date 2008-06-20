      SUBROUTINE CON_RCCC (INLOC, CUBID, STATUS)
*+
*  Name:
*     CON_RCCC
*  Purpose:
*     Copy the character components for an Asterix cube.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_RSCC (INLOC, CUBID; STATUS)
*  Description:
*     Copy the character components from an Asterix data cube to
*     an output NDF.
*  Arguments:
*     INLOC  =  CHARACTER*(*) (Given)
*        Locator to the input cube.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each character component:-
*       Attempt to get a locator
*       If ok then
*         Attempt to get the value of the component.
*       else
*         Set the value to 'Unknown'
*         If the failure was due to not being able to find the component
*           Reset the status.
*         end if
*       end if
*     end for
*     Set the character components for the output NDF.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     3/9/97. (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard Starlink constants
      INCLUDE 'DAT_PAR'       ! HDS constants
      INCLUDE 'DAT_ERR'       ! HDS error codes
*  Arguments Given:
      CHARACTER
     :  INLOC*(*)
      INTEGER
     :  CUBID
*  Status:
      INTEGER STATUS          ! Global status
*  External References:
      INTEGER CHR_LEN         ! Length of a string less trailing blanks
*  Local Variables:
      INTEGER NC              ! Used length of string
      CHARACTER
     :  TTLLOC*(DAT__SZLOC),  ! Locator to the input title component
     :  LABLOC*(DAT__SZLOC),  !    "    "   "    "   label     "
     :  UNTLOC*(DAT__SZLOC),  !    "    "   "    "   units     "
     :  TITLE*80,             ! Value of the title component
     :  LABEL*80,             !   "   "   "  label     " 
     :  UNITS*80              !   "   "   "  units     "
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       For each component, attempt to get a locator in the input
*       Asterix structure.  If the attempt succeeds then attempt
*       to get the value.  However, if a locator is not obtained then
*       set the component value to 'Unknown' and reset the status
*       if (and only if) the failure was because the component was
*       not found.
*
*       ... the title component.

         CALL DAT_FIND (INLOC, 'TITLE', TTLLOC, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL DAT_GET0C (TTLLOC, TITLE, STATUS)

         ELSE
            TITLE = 'Unknown'

            IF (STATUS .EQ. DAT__OBJNF) THEN
               CALL ERR_ANNUL (STATUS)
            END IF

         END IF

*       ... the label component.

         CALL DAT_FIND (INLOC, 'LABEL', LABLOC, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL DAT_GET0C (LABLOC, LABEL, STATUS)

         ELSE
            LABEL = 'Unknown'

            IF (STATUS .EQ. DAT__OBJNF) THEN
               CALL ERR_ANNUL (STATUS)
            END IF

         END IF

*       ... the units component.

         CALL DAT_FIND (INLOC, 'UNITS', UNTLOC, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL DAT_GET0C (UNTLOC, UNITS, STATUS)

         ELSE
            UNITS = 'Unknown'

            IF (STATUS .EQ. DAT__OBJNF) THEN
               CALL ERR_ANNUL (STATUS)
            END IF

         END IF

*
*       Set the character components in the output NDF.

         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT (TITLE, CUBID, 'TITLE', STATUS )

         NC = CHR_LEN( LABEL )
         CALL NDF_CPUT (LABEL, CUBID, 'LABEL', STATUS )
         
         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT (UNITS, CUBID, 'UNITS', STATUS )

      END IF

      END
