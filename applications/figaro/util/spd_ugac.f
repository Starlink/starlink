      SUBROUTINE SPD_UGAC( ZONID, STATUS )
*+
*  Name:
*     SPD_UGAC

*  Purpose:
*     Clear plot for PGPLOT in an SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UGAC( ZONID, STATUS )

*  Description:
*     The routine quadruplet SPD_UGA{ABCD} is used by
*     Specdre instead of AGP_ASSOC, AGP_DEASS, PGPAGE, AGP_SVIEW to
*     overcome the problem that normally the view surface is the base
*     picture and PGPAGE will clear more than the AGI picture to be
*     used.
*
*     This routine closes PGPLOT, makes the given SGS zone the current
*     one, clears that SGS zone and reopens PGPLOT in that SGS zone.
*     The only graphics that should be done between calls to
*     SPAEB and SPAED are PGPLOT calls (but not PGBEG or PGEND) and
*     calls to this routine. Calls to PGPAGE should not be made, instead
*     this routine should be called. The work station identifier given
*     to this routine should be the one returned by SPD_UGAA.
*
*     This routine is, however, not an exact replacement for PGPAGE. All
*     plot attributes will be reset, like the view port, window, pen
*     colour.

*  Arguments:
*     ZONID = INTEGER (Given)
*        The identifier for the SGS zone which is cleared and in which
*        PGPLOT is reopened. This should be the identifier returned by
*        the previous call to SPD_UGAA. It will be made the current SGS
*        zone.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Jun 1993 (hme):
*        Original version.
*     19 May 1994 (hme):
*        Renamed from SPAEC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ZONID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PGSTAT             ! Status from PGBEG
      INTEGER IWKID              ! Workstation ID for current SGS zone
      CHARACTER * ( 10 ) CWKID   ! Workstation ID for current SGS zone

*  Internal References:
      INTEGER PGBEG              ! Open PGPLOT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Close PGPLOT.
      CALL PGEND

*  Make the given SGS zone the current one.
      CALL SGS_SELZ( ZONID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Clear the current zone.
      CALL SGS_CLRZ

*  Enquire SGS workstation identifier, integer, returned.
      CALL SGS_ICURW( IWKID )

*  Convert integer workstation identifier to character.
      WRITE( CWKID, '(I10)' ) IWKID

*  Try to start PGPLOT, using the character workstation identifier. This
*  will open PGPLOT in the current SGS zone, which is the one identified
*  as ZONID.
      PGSTAT = PGBEG( 0, CWKID, 1, 1 )
      IF ( PGSTAT .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_UGAC_E01', 'SPD_UGAC: Error reopening ' //
     :      'PGPLOT after clearing SGS zone.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
