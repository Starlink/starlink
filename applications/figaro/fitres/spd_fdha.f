      SUBROUTINE SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
*+
*  Name:
*     SPD_FDHA

*  Purpose:
*     Find shape of and check result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )

*  Description:
*     This routine enquires an existing result structure in the Specdre
*     Extensions, i.e. it returns the number of components and the total
*     number of parameters catered for. It will also check the bounds of
*     the result NDF and its extension HDS vectors against the total
*     number of parameters and against the bounds of the main's base
*     NDF. Finally it checks the consistency of the NPARA vector with
*     the total number of parameters. If something is wrong with the
*     result structure, then an error report is made and the status
*     variable is set. Otherwise this routine remains silent and returns
*     status OK.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     NCOMP = INTEGER (Returned)
*        The number of components the result structure caters for.
*     TNPAR = INTEGER (Returned)
*        The total number of parameters the result structure caters for.
*     TYPE( 3 ) = CHARACTER * ( * ) (Returned)
*        The numeric types used for storage. These are for (1) data and
*        variance, (2) the LABFREQ extension, (3) the MASKL and MASKR
*        extensions. Each type can be '_REAL', '_DOUBLE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This may be set for a number of reasons when
*        an error in the result structure is detected.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1992 (hme):
*        Original version.
*     25 Feb 1994 (hme):
*        Rename from SPEHA.
*     24 Nov 1994 (hme):
*        Redirect call to SPABBI to library's own routine.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC

*  Arguments Returned:
      INTEGER NCOMP
      INTEGER TNPAR
      CHARACTER * ( * ) TYPE( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! True if structure exists
      LOGICAL FAULT              ! True if NDF shape inconsitency
      INTEGER I, J               ! Loop index
      INTEGER TNDF               ! Temporary NDF identifier
      INTEGER BNDF               ! Base NDF identifier
      INTEGER NDIM1              ! Dimensionality
      INTEGER NDIM2              ! Dimensionality
      INTEGER LBND1( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND1( NDF__MXDIM ) ! NDF upper bounds
      INTEGER LBND2( NDF__MXDIM ) ! NDF lower bounds
      INTEGER UBND2( NDF__MXDIM ) ! NDF upper bounds
      INTEGER PNTR               ! Array pointer
      INTEGER SIZE               ! Size of vector
      INTEGER SPAXIS             ! Number of spectroscopic axis
      CHARACTER * ( DAT__SZLOC ) TLOC  ! Temporary locator
      CHARACTER * ( NDF__SZTYP ) RTYP  ! Stored type
      CHARACTER * ( 8 ) CNAME( XC9NC ) ! Component related vector names
      CHARACTER * ( 8 ) PNAME( XC9NP ) ! Parameter related vector names
      CHARACTER * ( 8 ) CTYPE( XC9NC ) ! Component related vector types
      CHARACTER * ( 8 ) PTYPE( XC9NP ) ! Parameter related vector types

*  Internal References:
      INTEGER SPD_FDABI          ! Get array element

*  Local Data:
      DATA CNAME / XC9C1, XC9C2, XC9C3, XC9C4, XC9C5, XC9C6 /
      DATA PNAME / XC9P1 /
      DATA CTYPE / XT9C1, XT9C2, XT9C3, XT9C4, XT9C5, XT9C6 /
      DATA PTYPE / XT9P1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if structure is there.
      CALL DAT_THERE( XLOC, XCMP9, THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOEXST', 'SPD_FDHA: Error: ' //
     :      'The result structure does not exist.', STATUS )
         GO TO 500
      END IF

*  Access the structure, should be an NDF.
      CALL NDF_FIND( XLOC, XCMP9, TNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPE_INVNDF', 'SPD_FDHA: Error accessing ' //
     :      'result structure. It is not a valid NDF.', STATUS )
         GO TO 500
      END IF

*  Find out the type needed for data and variance.
      CALL NDF_TYPE( TNDF, 'DATA,VARIANCE', TYPE(1), STATUS )
      IF ( TYPE(1) .NE. '_REAL' .AND. TYPE(1) .NE. '_DOUBLE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: ' //
     :      'The result NDF type is invalid.', STATUS )
         GO TO 500
      END IF

*  Loop through list of component related vectors.
      DO 2 I = 1, XC9NC

*     Locate the vector in the extension.
         CALL NDF_XLOC( TNDF, CNAME(I), 'READ', TLOC, STATUS )
         IF ( .NOT. THERE ) THEN
            CALL MSG_SETC( 'CHAR', CNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_NOEXST',
     :         'SPD_FDHA: Error accessing the result extension ' //
     :         '^CHAR. Structure is missing', STATUS )
            GO TO 500
         END IF

*     Find out length.
         CALL DAT_SIZE( TLOC, SIZE, STATUS )
         IF ( I .EQ. 1 ) THEN
            NCOMP = SIZE
         ELSE IF ( SIZE .NE. NCOMP ) THEN
            CALL MSG_SETC( 'CHAR', CNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHA: Error: ' //
     :         'The size of the result extension ^CHAR does not ' //
     :         'match the number of components.', STATUS )
            GO TO 500
         END IF

*     Find out type.
         CALL DAT_TYPE( TLOC, RTYP, STATUS )
         IF ( I .EQ. 2 ) THEN
            TYPE(2) = RTYP
            IF ( RTYP .NE. '_REAL' .AND. RTYP .NE. '_DOUBLE' ) THEN
               CALL MSG_SETC( 'CHAR', CNAME(I) )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: The ' //
     :            'type of the result extension ^CHAR is invalid.',
     :            STATUS )
               GO TO 500
            END IF
         ELSE IF ( I .EQ. 5 ) THEN
            TYPE(3) = RTYP
            IF ( RTYP .NE. '_REAL' .AND. RTYP .NE. '_DOUBLE' ) THEN
               CALL MSG_SETC( 'CHAR', CNAME(I) )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: The ' //
     :            'type of the result extension ^CHAR is invalid.',
     :            STATUS )
               GO TO 500
            END IF
         ELSE IF ( I .EQ. 6 ) THEN
            IF ( RTYP .NE. TYPE(3) ) THEN
               CALL MSG_SETC( 'CHAR', CNAME(I) )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: The ' //
     :            'type of the result extension ^CHAR is inconsistent.',
     :            STATUS )
               GO TO 500
            END IF
         ELSE IF ( RTYP .NE. CTYPE(I) ) THEN
            CALL MSG_SETC( 'CHAR', CNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: The type ' //
     :         'of the result extension ^CHAR is invalid.', STATUS )
            GO TO 500
         END IF

*     Find out minimum TNPAR.
         IF ( I .EQ. 4 ) THEN
            CALL DAT_MAP( TLOC, '_INTEGER', 'READ', 1, SIZE,
     :         PNTR, STATUS )
            TNPAR = 0
            DO 1 J = 1, SIZE
               TNPAR = TNPAR + SPD_FDABI( %VAL( CNF_PVAL(PNTR) ), J,
     :                                    STATUS )
 1          CONTINUE
         END IF

*     Annul the locator.
         CALL DAT_ANNUL( TLOC, STATUS )
 2    CONTINUE

*  Loop through list of parameter related vectors.
      DO 3 I = 1, XC9NP

*     Locate the vector in the extension.
         CALL NDF_XLOC( TNDF, PNAME(I), 'READ', TLOC, STATUS )
         IF ( .NOT. THERE ) THEN
            CALL MSG_SETC( 'CHAR', PNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_NOEXST',
     :         'SPD_FDHA: Error accessing the result extension ' //
     :         '^CHAR. Structure is missing', STATUS )
            GO TO 500
         END IF

*     Find out length.
         CALL DAT_SIZE( TLOC, SIZE, STATUS )
         IF ( I .EQ. 1 .AND. SIZE .GT. 0 .AND. SIZE .GE. TNPAR ) THEN
            TNPAR = SIZE
         ELSE IF ( I .EQ. 1 .OR. SIZE .NE. TNPAR ) THEN
            CALL MSG_SETC( 'CHAR', PNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHA: Error: ' //
     :         'The size of the result extension ^CHAR does not ' //
     :         'match the total number of parameters.', STATUS )
            GO TO 500
         END IF

*     Find out type.
         CALL DAT_TYPE( TLOC, RTYP, STATUS )
         IF ( RTYP .NE. PTYPE(I) ) THEN
            CALL MSG_SETC( 'CHAR', PNAME(I) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHA: Error: The type ' //
     :         'of the result extension ^CHAR is invalid.', STATUS )
            GO TO 500
         END IF

*     Annul the locator.
         CALL DAT_ANNUL( TLOC, STATUS )
 3    CONTINUE

*  Find out spectroscopic axis.
*  Get bounds of main's base NDF and of result NDF.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )
      CALL NDF_BASE( NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )
      CALL NDF_BOUND( TNDF, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )

*  Check all bounds of the result NDF.
      FAULT = .FALSE.
      IF ( NDIM2 .NE. NDIM1 + 1 ) FAULT = .TRUE.
      IF ( LBND2(1) .NE. 1 .OR. UBND2(1) .NE. TNPAR ) FAULT = .TRUE.
      IF ( LBND2(2) .NE. 1 .OR. UBND2(2) .NE. 1 ) FAULT = .TRUE.
      J = 2
      DO 4 I = 1, NDIM1
         IF ( I .NE. SPAXIS ) THEN
            J = J + 1
            IF ( LBND2(J) .NE. LBND1(I) .OR. UBND2(J) .NE. UBND1(I) )
     :         FAULT = .TRUE.
         END IF
 4    CONTINUE
      IF ( J .NE. NDIM2 ) FAULT = .TRUE.
      IF ( FAULT ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHA: Error: The shape of ' //
     :      'the result NDF is incompatible with the shape of the ' //
     :      'corresponding base main NDF or with the total number ' //
     :      'of parameters.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL DAT_ANNUL( TLOC, STATUS )
      CALL NDF_ANNUL( TNDF, STATUS )
      END
