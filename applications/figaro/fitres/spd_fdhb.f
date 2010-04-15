      SUBROUTINE SPD_FDHB( NDF, XLOC, NCOMP, TNPAR, STATUS )
*+
*  Name:
*     SPD_FDHB

*  Purpose:
*     Reshape existing result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHB( NDF, XLOC, NCOMP, TNPAR, STATUS )

*  Description:
*     This routine reshapes the result structure in the Specdre
*     Extension according to new values for the numbers of spectral
*     components and of result parameters.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     NCOMP = INTEGER (Given)
*        The number of spectral components to be provided for.
*     TNPAR = INTEGER (Given)
*        The number of result parameters to be provided for.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the given NCOMP or TNPAR are
*        less than or equal to zero.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-MAR-1992 (HME):
*        Original version (SPAAZ).
*     02-APR-1992 (HME):
*        Check that NCOMP and TNPAR are positive (SPAAZ).
*     06-JUL-1992 (HME):
*        Adapt according to SPE-routine convention.
*     24 Nov 1994 (hme):
*        Rename from SPEHB.
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
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      INTEGER NCOMP
      INTEGER TNPAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CSIZE              ! Length of a standard string
      PARAMETER ( CSIZE = 32 )

*  Local Variables:
      INTEGER OLDNCO             ! Previous number of components
      INTEGER OLDTNP             ! Previous number of parameters
      INTEGER I, J               ! Temporary integers
      INTEGER COMP( 2 )          ! Needed for SPEHE
      INTEGER NELM( 3 )          ! Returned by SPEHE
      INTEGER XNDF               ! Result NDF identifier
      INTEGER XPNTR( 2 )         ! Pointers to result data and variance
      INTEGER CPNTR( XC9NC )     ! Pointers to result vectors
      INTEGER PPNTR( XC9NP )     ! Pointers to result vectors
      INTEGER NDIM               ! Result NDF dimensionality
      INTEGER LBND( NDF__MXDIM ) ! Result NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! Result NDF upper bounds
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Types of structures
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) CLOC( XC9NC ) ! Vector locators
      CHARACTER * ( DAT__SZLOC ) PLOC( XC9NP ) ! Vector locators
      CHARACTER * ( 8 ) CNAME( XC9NC ) ! Extension names
      CHARACTER * ( 8 ) PNAME( XC9NP ) ! Extension names

*  Internal References:
      INTEGER SPD_FDABI          ! Get an integer array element

*  Local Data:
      DATA CNAME / XC9C1, XC9C2, XC9C3, XC9C4, XC9C5, XC9C6 /
      DATA PNAME / XC9P1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of spectral components and result parameters.
*  Neither must be 0 or less.
      IF ( NCOMP .LE. 0 .OR. TNPAR .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHB: Error reshaping ' //
     :      'result structure: The requested number of components ' //
     :      'or parameters is zero.', STATUS )
         GO TO 500
      END IF

*  Find out old shape.
      CALL SPD_FDHA( NDF, XLOC, OLDNCO, OLDTNP, TYPE, STATUS )

*  Access the result NDF.
      CALL NDF_FIND( XLOC, XCMP9, XNDF, STATUS )

*  Shape vectors that are indexed by spectral component.
      IF ( NCOMP .NE. OLDNCO ) THEN
         DO 1 I = 1, XC9NC
            CALL NDF_XLOC( XNDF, CNAME(I), 'UPDATE', TLOC, STATUS )
            CALL DAT_ALTER( TLOC, 1, NCOMP, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )
 1       CONTINUE
      END IF

*  Shape according to total number of result parameters.
      IF ( TNPAR .NE. OLDTNP ) THEN

*     Shape result NDF.
         CALL NDF_BOUND( XNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         LBND(1) = 1
         UBND(1) = TNPAR
         CALL NDF_SBND( NDIM, LBND, UBND, XNDF, STATUS )

*     Shape vectors that are indexed by result parameters.
         DO 2 I = 1, XC9NP
            CALL NDF_XLOC( XNDF, PNAME(I), 'UPDATE', TLOC, STATUS )
            CALL DAT_ALTER( TLOC, 1, TNPAR, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )
 2       CONTINUE
      END IF

*  If any vectors are longer now, map all arrays, so that their tails
*  can be filled with default values.
      IF ( NCOMP .GT. OLDNCO .OR. TNPAR .GT. OLDTNP ) THEN
         CALL NDF_ANNUL( XNDF, STATUS )
         COMP(1) = 1
         COMP(2) = NCOMP
         CALL SPD_FDHE( NDF, XLOC, 'UPDATE', TYPE, COMP,
     :      XNDF, CLOC, PLOC, XPNTR, CPNTR, PPNTR, NELM, STATUS )
      END IF

*  If spectral component indexed vectors are longer now, fill new
*  elements.
      IF ( NCOMP .GT. OLDNCO ) THEN

*     Line names.
         CALL SPD_FDAAC( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(1)) ),
     :                   'unidentified component', STATUS,
     :                   %VAL(CNF_CVAL(CSIZE)) )

*     Laboratory frequencies.
         IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
            CALL SPD_FDAAD( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(2)) ),
     :                      VAL__BADD, STATUS )
         ELSE
            CALL SPD_FDAAR( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(2)) ),
     :                      VAL__BADR, STATUS )
         END IF

*     Component types.
         CALL SPD_FDAAC( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(3)) ),
     :                   'unknown function', STATUS,
     :                   %VAL(CNF_CVAL(CSIZE)) )

*     Number of result parameters for each spectral component.
         I = INT( (TNPAR-OLDTNP) / (NCOMP-OLDNCO) )
         I = MAX( I, 0 )
         CALL SPD_FDAAI( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(4)) ),
     :                   I, STATUS )

*     Masks.
         IF ( TYPE(3) .EQ. '_DOUBLE' ) THEN
            CALL SPD_FDAAD( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(5)) ),
     :                      VAL__BADD, STATUS )
            CALL SPD_FDAAD( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(6)) ),
     :                      VAL__BADD, STATUS )
         ELSE
            CALL SPD_FDAAR( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(5)) ),
     :                      VAL__BADR, STATUS )
            CALL SPD_FDAAR( OLDNCO+1, NCOMP, %VAL( CNF_PVAL(CPNTR(6)) ),
     :                      VAL__BADR, STATUS )
         END IF
      END IF

*  If result parameter indexed vectors are longer now, fill new
*  elements.
      IF ( TNPAR .GT. OLDTNP ) THEN
         CALL SPD_FDAAC( OLDTNP+1, TNPAR, %VAL( CNF_PVAL(PPNTR(1)) ),
     :                   'unknown parameter', STATUS,
     :                   %VAL(CNF_CVAL(CSIZE)) )

*  If parameter indexed vectors are shorter now, some retained spectral
*  components may have been deprived of parameters they used to have.
      ELSE IF ( TNPAR .LT. OLDTNP ) THEN

*     While total number of parameters not exceeded and last component
*     not checked ...
*     J counts parameters, I counts components.
*     There are two possible outcomes of the WHILE loop.
*     (i) J is still less than or equal to TNPAR: All component's
*     previous needs are satisfied also by the new smaller TNPAR.
*     (ii) J is greater than TNPAR and I tells the first component that
*     was deprived of some of its parameters.
         J = 0
         I = 0
 3       CONTINUE             ! Start of 'DO WHILE' loop
         IF ( J .LE. TNPAR .AND. I .LT. NCOMP ) THEN

*        Next component.
            I = I + 1

*        Find out what used to be its last parameter.
            J = J + SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4)) ), I, STATUS )

*        Check the WHILE condition.
            GO TO 3
         END IF                  ! End of 'DO WHILE' loop

*     If components I ... NCOMP lost some or all of their parameters set
*     their number of parameters to zero.
         IF ( J .GT. TNPAR ) THEN
            CALL SPD_FDAAI( I, NCOMP, %VAL( CNF_PVAL(CPNTR(4)) ), 0,
     :                      STATUS )
         END IF
      END IF

*  Tidy up.
 500  CONTINUE
      DO 4 I = 1, XC9NC
         CALL DAT_ANNUL( CLOC(I), STATUS )
 4    CONTINUE
      DO 5 I = 1, XC9NP
         CALL DAT_ANNUL( PLOC(I), STATUS )
 5    CONTINUE
      CALL NDF_ANNUL( XNDF, STATUS )

*  Return.
      END
