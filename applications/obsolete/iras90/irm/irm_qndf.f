      SUBROUTINE IRM_QNDF( INDF, QEXP, CONT, HELD, ONDF, STATUS )
*+
*  Name:
*     IRM_QNDF

*  Purpose:
*     Set NDF DATA elements bad on the basis of quality.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL IRM_QNDF( INDF, QEXP, CONT, HELD, ONDF, STATUS )

*  Description:
*     An attempt is made to compile the supplied quality expression
*     using the quality name information stored in the supplied NDF. If
*     successful, a copy of the NDF is created in which DATA elements
*     are set BAD if they hold (or do not hold, depending on argument
*     HELD) a quality which satisfies the supplied quality expression.
*     An identifier for this copy of the input NDF is returned. The BAD
*     pixel flag for the returned NDF is set appropriately.
*
*     If the supplied quality expression references any quality names
*     which are not defined within the supplied NDF, then an error
*     status may or may not be returned depending on the supplied value
*     of the argument CONT. If CONT is .FALSE. then an error status is
*     returned, but if CONT is .TRUE. then no error status is returned.
*     In this case the returned NDF identifier is a cloned copy of the
*     supplied NDF. A warning message is reported if this happens, so
*     long as the current conditional message filter level is not equal
*     to MSG__QUIET.
*
*     If the supplied quality expression is equal to "ANY" then the
*     returned NDF identifier is just a cloned copy of the input
*     identifier.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the input NDF.
*     QEXP = CHARACTER * ( * ) (Given)
*        A quality expression, this should be upper case and have no
*        leading blanks.
*     CONT = LOGICAL (Given)
*        If TRUE then the application can continue even if the quality
*        expression cannot be evaluated because of undefined quality
*        names. In this case the DATA array of the NDF is left as it is
*        (i.e. no elements are set BAD). If CONT is FALSE, then the
*        application cannot continue without evaluating the quality
*        expression, and an error is therefore reported if any of the
*        quality names referenced in the quality expression are not
*        defined within the supplied NDF.
*     HELD = LOGICAL (Given)
*        If TRUE then the elements of the DATA array which have
*        associated QUALITY values which satisfy the supplied quality
*        expression are set BAD (other elements are unaffected).
*        Otherwise, the elements of the DATA array which have
*        associated QUALITY values which do not satisfy the supplied
*        quality expression are set BAD (other elements are
*        unaffected).
*     ONDF = INTEGER (Returned)
*        An NDF identifier of a copy (or clone) of the input NDF, in
*        which selected elements of the DATA array are set BAD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      INTEGER INDF
      CHARACTER QEXP*(*)
      LOGICAL CONT
      LOGICAL HELD

*  Arguments Returned:
      INTEGER ONDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if strings are equal apart from
                                 ! case.

*  Local Variables:
      LOGICAL ALLBAD             ! True if all elements are bad.
      INTEGER ERRPNT             ! Pointer to syntax error.
      INTEGER IDQ                ! IRQ identifier for compiled quality
                                 ! expression.
      INTEGER ILEVEL             ! Conditional message filter level.
      LOGICAL INFO               ! True if undefined quality names are
                                 ! to be listed by IRQ_COMP.
      INTEGER IPT                ! Pointer to mapped DATA array.
      CHARACTER LOCS( 5 )*(DAT__SZLOC) ! Locators to quality name info.
      INTEGER NEL                ! No. of elements in mapped DATA array.
      LOGICAL NOBAD              ! True if no elements are bad.
      INTEGER NUNDEF             ! No. of undefined quality names.
      INTEGER PLACE              ! Place holder for temporary NDF.
      CHARACTER UNDEF( IRQ__QNREF )*(IRQ__SZQNM)! Array of undefined
                                 ! quality names.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension containg quality name
                                 ! info.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the quality expression is "ANY", the returned NDF is a clone of
*  the supplied NDF.
      IF( CHR_SIMLR( QEXP, 'ANY' ) ) THEN
         CALL NDF_CLONE( INDF, ONDF, STATUS )

*  Otherwise, attempt to find quality name information within the
*  supplied NDF.
      ELSE
         CALL IRQ_FIND( INDF, LOCS, XNAME, STATUS )

*  If no quality names information was found...
         IF( STATUS .EQ. IRQ__NOQNI ) THEN

*  ...if the application can continue without the information, then
*  annul the error condition, issue a warning message and return a
*  cloned copy of the input NDF.
            IF( CONT ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_OUTIF( MSG__NORM, 'IRM_QNDF_MSG1',
     : 'WARNING: No quality names are defined within ^NDF. All data '//
     : 'will be used', STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL NDF_CLONE( INDF, ONDF, STATUS )

            END IF

*  If the quality names information was found...
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  If the application cannot continue if any referenced quality names
*  are undefined, ensure that IRQ_COMP identifies any undefined quality
*  names.
            IF( .NOT. CONT ) THEN
               INFO = .TRUE.

*  Otherwise, use the current conditional message filter level to set
*  up the INFO argument for IRQ_COMP.
            ELSE

               CALL MSG_IFLEV( ILEVEL )
               IF( ILEVEL .EQ. MSG__QUIET ) THEN
                  INFO = .FALSE.
               ELSE
                  INFO = .TRUE.
               END IF

            END IF

*  Attempt to compile the quality expression.
            CALL IRQ_COMP( LOCS, IRQ__QNREF, INFO, QEXP, UNDEF,
     :                     NUNDEF, ERRPNT, IDQ, STATUS )

*  If any undefined quality names were referenced in the quality
*  expression...
            IF( STATUS .EQ. IRQ__NOQNM ) THEN

*  ...if the application can continue, then annul the error condition,
*  issue a warning message and return a cloned copy of the supplied NDF.
               IF( CONT ) THEN

                  CALL ERR_ANNUL( STATUS )
                  CALL NDF_MSG( 'NDF', INDF )
                  CALL MSG_OUTIF( MSG__NORM, 'IRM_QNDF_MSG1',
     :                       'WARNING: All data from ^NDF will be used',
     :                            STATUS )
                  CALL MSG_BLANKIF( MSG__NORM, STATUS )
                  CALL NDF_CLONE( INDF, ONDF, STATUS )

               END IF

*  If the quality expression was compiled sucessfully ...
            ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Produce a temporary copy of the supplied NDF.
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_COPY( INDF, PLACE, ONDF, STATUS )

*  Map the DATA array of the temporary NDF.
               CALL NDF_MAP( ONDF, 'DATA', '_REAL', 'UPDATE', IPT, NEL,
     :                       STATUS )

*  Set the appropriate pixels bad in the DATA array of the temporary
*  NDF.
               CALL IRQ_SBAD( IDQ, HELD, NEL, %VAL( IPT ), ALLBAD,
     :                        NOBAD, STATUS )

*  Unmap the DATA array of the temporary NDF.
               CALL NDF_UNMAP( ONDF, 'DATA', STATUS )

*  Set the bad pixel flag for the temporary NDF.
               IF( NOBAD ) THEN
                  CALL NDF_SBAD( .FALSE., ONDF, 'DATA', STATUS )
               ELSE
                  CALL NDF_SBAD( .TRUE., ONDF, 'DATA', STATUS )
               END IF

*  Annul the compiled quality expression.
               CALL IRQ_ANNUL( IDQ, STATUS )

            END IF

*  Release the locators to the quality name information.
            CALL IRQ_RLSE( LOCS, STATUS )

         END IF

      END IF

      END
