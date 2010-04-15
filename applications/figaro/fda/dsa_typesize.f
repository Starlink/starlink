      INTEGER FUNCTION DSA_TYPESIZE( TYPE, STATUS )
*+
*  Name:
*     DSA_TYPESIZE

*  Purpose:
*     Return the size in bytes of an element of a given type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = DSA_TYPESIZE( TYPE, STATUS )

*  Description:
*     Given one of the types recognised by the DSA_ routines ('FLOAT',
*     'INT', etc), this routine returns the number of bytes required
*     by a single element of that type. If the given type is
*     unrecognised a type size of zero is returned.
*
*     If there is a danger that this routine has to make an error
*     report, then the calling routine should not have it coded into
*     a WRITE statement, and perhaps not into a subroutine or function
*     call either. An error report is made if and only if the given
*     type specification is not recognised.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     DSA_TYPESIZE = INTEGER
*        The number of bytes occupied by a variable of the given type.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     29 Jul 1987 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     23 Aug 1992 (ks):
*        Ensure function value is defined before allowing a
*        RETURN (the DecStation compiler spotted this one).
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case. KS/AAO
*     26 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     30 Jul 1996 (mjcl):
*        Moved DSA_TYPES as it contains DATA statements.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * 7 TYPEUC       ! Given type in upper case

*  Includes with DATA statements:
      INCLUDE 'DSA_TYPES'        ! DSA type constants
*.

*  Safe value (well, not exactly safe).
      DSA_TYPESIZE = 0

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Fold given type to upper case.
      TYPEUC = TYPE
      CALL CHR_UCASE( TYPEUC )

*  Check each type, first match bails out.
      DO 1 I = 1, MAX_TYPES
         IF ( TYPEUC .EQ. TYPE_NAMES(I) ) THEN
            DSA_TYPESIZE = TYPE_SIZE(I)
            GO TO 2
         END IF
 1    CONTINUE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T004', TYPEUC )
         CALL ERR_REP( 'FDA_E049', 'DSA_TYPESIZE: ' //
     :      'Do not know type ^FDA_T004.', STATUS )
 2    CONTINUE

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
