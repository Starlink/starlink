      SUBROUTINE SPD_FAAB( A_MNDF, STATUS )
*+
*  Name:
*     SPD_FAAB

*  Purpose:
*     Release a result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FAAB( MNDF, STATUS )

*  Description:
*     This routine releases the result structure associated with the
*     given main NDF and accessed with SPD_FAAA.
*
*     If the given NDF is NDF__NOID, then all currently accessed result
*     structures will be released. An error report to that effect is
*     made.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the main NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to do its job even if the
*        given status is bad.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Feb 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Arguments Given:
      INTEGER A_MNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number

*.

*  Begin error context.
      CALL ERR_BEGIN( STATUS )

*  Check the given main NDF.
      IF ( A_MNDF .EQ. NDF__NOID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FAAB_E01', 'SPD_FAAB: Error releasing a ' //
     :      'result structure: The given main NDF is invalid. ' //
     :      'Probable programming error. All result structures ' //
     :      'currently accessed will be released now.', STATUS )

*     Same action as below, but for all slots.
         DO 3 SLOT = 1, SPD__FMXR
            DO 1 I = 1, XC9NC
               CALL DAT_ANNUL( CLOC(I,SLOT), STATUS )
 1          CONTINUE
            DO 2 I = 1, XC9NP
               CALL DAT_ANNUL( PLOC(I,SLOT), STATUS )
 2          CONTINUE
            CALL NDF_ANNUL( RNDF(SLOT), STATUS )
            CALL DAT_ANNUL( XLOC(SLOT), STATUS )
            MNDF( SLOT ) = NDF__NOID
 3       CONTINUE

*     Bail out.
         GO TO 500
      END IF

*  Find the slot.
      SLOT = 0
      DO 4 I = 1, SPD__FMXR
         IF ( MNDF(I) .EQ. A_MNDF ) SLOT = I
 4    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FAAB_E02', 'SPD_FAAB: Error releasing a ' //
     :      'result structure: Not accessed for that main NDF.',
     :      STATUS )
         GO TO 500
      END IF

*  Annul all component- and parameter-related vector locators.
      DO 5 I = 1, XC9NC
         CALL DAT_ANNUL( CLOC(I,SLOT), STATUS )
 5    CONTINUE
      DO 6 I = 1, XC9NP
         CALL DAT_ANNUL( PLOC(I,SLOT), STATUS )
 6    CONTINUE

*  Annul result RNDF.
      CALL NDF_ANNUL( RNDF(SLOT), STATUS )

*  Annul Extension locator.
      CALL DAT_ANNUL( XLOC(SLOT), STATUS )

*  Invalidate slot. Since MNDF is just a Fortran copy and not a clone,
*  just reset it.
      MNDF( SLOT ) = NDF__NOID

*  End the error context.
 500  CONTINUE
      CALL ERR_END( STATUS )

*  Return.
      END
