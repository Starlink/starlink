      SUBROUTINE DSA_MATCH_AXES( DSARE1, DSARE2, STATUS )
*+
*  Name:
*     DSA_MATCH_AXES

*  Purpose:
*     Check that the axis information for two NDFs matches.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MATCH_AXES( DSARE1, DSARE2, STATUS )

*  Description:
*     This routine checks that the axis information for two NDFs
*     matches. It checks
*        a) that the two NDFs have the same dimensionality,
*        b) that the units are the same for each axis,
*        c) that the centre arrays are the same for each axis.
*     If there are any discrepancies, an error message is put out, and a
*     bad status is returned.
*
*     Contrary to earlier implementations, the axis centre arrays should
*     not be mapped while this routine is called.

*  Arguments:
*     DSARE1 = CHARACTER * ( * ) (Given)
*        The reference name associated with the first NDF.
*     DSARE2 = CHARACTER * ( * ) (Given)
*        The reference name associated with the second NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Jul 1987 (ks):
*        Original version.
*     15 Feb 1991 (ks):
*        Missing \N in DSA_WRUSER call fixed.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSARE1
      CHARACTER * ( * ) DSARE2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER SLO1               ! The first reference slot
      INTEGER SLO2               ! The second reference slot
      INTEGER NDIM, NDIM1, NDIM2 ! NDF dimensionalities
      INTEGER DIM1( NDF__MXDIM ) ! NDF dimensions
      INTEGER DIM2( NDF__MXDIM ) ! NDF dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up both references.
      CALL DSA1_RFND( DSARE1, SLO1, STATUS )
      CALL DSA1_RFND( DSARE2, SLO2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Look up the dimensions of both NDFs (we're not interested in lower
*  bounds!)
      CALL NDF_DIM( DSA__REFID1(SLO1), NDF__MXDIM, DIM1, NDIM1, STATUS )
      CALL NDF_DIM( DSA__REFID1(SLO2), NDF__MXDIM, DIM2, NDIM2, STATUS )
      NDIM = NDIM1
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Reduce dimensionalities to actual dimensionalities.
 1    CONTINUE
      IF ( NDIM1 .GT. 1 .AND. DIM1(NDIM1) .EQ. 1 ) THEN
         NDIM1 = NDIM1 - 1
         GO TO 1
      END IF
 2    CONTINUE
      IF ( NDIM2 .GT. 1 .AND. DIM2(NDIM2) .EQ. 1 ) THEN
         NDIM2 = NDIM2 - 1
         GO TO 2
      END IF

*  If dimensionalities differ, set status and give message.
      IF ( NDIM1 .NE. NDIM2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLO1) )
         CALL MSG_SETC( 'FDA_T007', DSA__REFNAM(SLO2) )
         CALL ERR_REP( 'FDA_E025', 'DSA_MATCH_AXES: The two ' //
     :      'references ^FDA_T001 and ^FDA_T007 refer to ' //
     :      'data structures of different shape.', STATUS )
         GO TO 500
      END IF

*  Now check each axis in turn.
*  Here, we use again the formal dimensionality of the first NDF.
      DO I = 1, NDIM1
         CALL DSA1_MATAX( SLO1, I, SLO2, I, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
      END DO

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
