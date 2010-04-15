      SUBROUTINE RENOBJ( STATUS )
*+
*  Name:
*     RENOBJ

*  Purpose:
*     Change the name or location of an object within an HDS file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL RENOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine either renames an HDS object in place or moves it to
*     a different place in the structure hierarchy of the same file. It
*     is not possible to reshape objects, i.e. to change their
*     dimensions or dimensionality.

*  Usage:
*     renobj source=? destin=?

*  ADAM Parameters:
*     SOURCE = HDSOBJECT (Read)
*        The existing HDS object to be renamed. Specify beginning with
*        directory and file name in the syntax of the operating system,
*        followed by the dot-separated structure hierarchy. Elements of
*        structure arrays are specified in ordinary brackets (). An
*        array element cannot be renamed.
*     DESTIN = HDSOBJECT (Read)
*        The new name for the HDS object. Specify beginning with
*        directory and file name in the syntax of the operating system,
*        followed by the dot-separated structure hierarchy. Elements of
*        structure arrays are specified in ordinary brackets (). The
*        destination cannot be an array element. The destination object
*        must not exist beforehand.

*  Examples:
*     renobj source=file.MORE.FIGARO.OBS.TIME destin=file.MORE.FIGARO.TIME
*        This moves the time specification from .MORE.FIGARO.OBS one
*        level up into .MORE.FIGARO.
*     renobj source=file.ERRORS destin=file.VARIANCE
*        This renames the object ERRORS into VARIANCE, but the location
*        remains the same. (The contents remain the same anyway.)

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Aug 1986 (KS):
*        Original version.
*     25 Mar 1991 (KS):
*        Now allows for more than one possible default extension (eg
*        .DST, .SDF).
*     01 Oct 1992 (HME):
*        Rewritten in terms of HDS
*     11 Mar 1993 (HME):
*        Oddly, DAT_MOVE does not work when (A)PAR is involved and the
*        destination is below the top level. In that case DAT_COPY,
*        DAT_ANNUL, and DAT_DELET.
*        Also review the error reporting strategy: report immediately.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DAT_ERR'          ! DAT error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      CHARACTER * ( DAT__SZLOC ) LOC( 4 ) ! HDS locators
      CHARACTER * ( DAT__SZNAM ) DESTIN ! HDS component name
      CHARACTER * ( 132 ) FILE( 2 ) ! HDS file names
      CHARACTER * ( 132 ) PATH( 2 ) ! HDS hierarchy paths

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate source by association with an ADAM parameter.
*  Locate its parent.
      CALL DAT_ASSOC( 'SOURCE', 'UPDATE', LOC(3), STATUS )
      CALL DAT_PAREN( LOC(3), LOC(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Same for destination. Problem is we have to create the destination in
*  order to get a locator.
*  Also want to know the component name for later use.
      CALL DAT_CREAT( 'DESTIN', 'STRUCT', 0, 0, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RENOBJ_ERR', 'RENOBJ: Destination already ' //
     :      'exists. File unchanged.', STATUS )
         GO TO 500
      END IF
      CALL DAT_ASSOC( 'DESTIN', 'UPDATE', LOC(4), STATUS )
      CALL DAT_NAME(  LOC(4), DESTIN, STATUS )
      CALL DAT_PAREN( LOC(4), LOC(2), STATUS )

*  Trace the parents.
      CALL HDS_TRACE( LOC(1), I, PATH(1), FILE(1), STATUS )
      CALL HDS_TRACE( LOC(2), I, PATH(2), FILE(2), STATUS )

*  Delete the new structure again.
*  Strange things happen when we delete the thing created by the
*  parameter association. Must work our way down the hierarchy to the
*  parent of the destination again. That's the raison d'etre for the HDS
*  digging routine below.
      CALL DAT_ANNUL( LOC(2), STATUS )
      CALL DAT_ANNUL( LOC(4), STATUS )
      CALL DAT_DELET( 'DESTIN', STATUS )

*  Source and destination (or their parents) must be in the same file.
      IF ( FILE(1) .NE. FILE(2) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RENOBJ_ERR', 'RENOBJ: Destination is in ' //
     :      'different file. Files unchanged.', STATUS )
         GO TO 500
      END IF

*  If same parent, rename the source.
      IF ( PATH(1) .EQ. PATH(2) ) THEN
         CALL DAT_RENAM( LOC(3), DESTIN, STATUS )
         CALL DAT_ANNUL( LOC(3), STATUS )
         CALL DAT_ANNUL( LOC(1), STATUS )

*  Else (different parents), move the source. This will annul the source
*  locator.
*  For some obscure reason DAT_MOVE does not work if LOC(2) is the top
*  level. In that case, must copy and delete.
*  Equally obscure is the fact that LOC(1) is being annulled when SOURCE
*  is deleted.
      ELSE
         CALL FIG_HDSDIG( FILE(2), PATH(2), 'UPDATE', LOC(2), STATUS )
         IF ( INDEX(PATH(2),'.') .EQ. 0 ) THEN
            CALL DAT_COPY( LOC(3), LOC(2), DESTIN, STATUS )
            CALL DAT_ANNUL( LOC(3), STATUS )
            CALL DAT_ANNUL( LOC(2), STATUS )
            CALL DAT_DELET( 'SOURCE', STATUS )
         ELSE
            CALL DAT_MOVE( LOC(3), LOC(2), DESTIN, STATUS )
            CALL DAT_ANNUL( LOC(2), STATUS )
            CALL DAT_ANNUL( LOC(1), STATUS )
         END IF
      END IF

*  Annul the remaining locators and cancel the parameters.
      CALL DAT_CANCL( 'DESTIN', STATUS )
      CALL DAT_CANCL( 'SOURCE', STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 1 I = 4, 1, -1
            CALL DAT_ANNUL( LOC(I), STATUS )
 1       CONTINUE
         CALL DAT_CANCL( 'DESTIN', STATUS )
         CALL DAT_CANCL( 'SOURCE', STATUS )
      END IF

      END
