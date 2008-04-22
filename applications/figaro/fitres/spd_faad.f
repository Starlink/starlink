      SUBROUTINE SPD_FAAD( A_MNDF, A_NCOMP, A_TNPAR, STATUS )
*+
*  Name:
*     SPD_FAAD

*  Purpose:
*     Reshape a result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FAAD( MNDF, NCOMP, TNPAR, STATUS )

*  Description:
*     This routine reshapes the result structure in the Specdre
*     Extension according to new values for the numbers of spectral
*     components and of result parameters.
*
*     This is different from a very similar older routine. That old
*     routine relied on the result structure not being accessed, but did
*     use a given locator to the Specdre Extension. This routine assumes
*     that the result structure has been accessed and takes all
*     necessary steps to unmap and re-map vectors and arrays.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     NCOMP = INTEGER (Given)
*        The number of spectral components to be provided for.
*     TNPAR = INTEGER (Given)
*        The number of result parameters to be provided for.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1992 (hme):
*        Original version (SPAAZ).
*     02 Apr 1992 (hme):
*        Check that NCOMP and TNPAR are positive (SPAAZ).
*     06 Jul 1992 (hme):
*        Adapt according to SPE-routine convention.
*     03 Mar 1994 (hme):
*        Adapt to use the FITRES common blocks.
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

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Arguments Given:
      INTEGER A_MNDF
      INTEGER A_NCOMP
      INTEGER A_TNPAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CSIZE              ! Length of a standard string
      PARAMETER ( CSIZE = 32 )

*  Local Variables:
      INTEGER I, J               ! Temporary integers
      INTEGER SLOT               ! Slot number
      CHARACTER * ( 8 ) CTYPE( XC9NC ) ! Extension data types
      CHARACTER * ( 8 ) PTYPE( XC9NP ) ! Extension data types

*  Internal References:
      INTEGER SPD_FDABI          ! Get an integer array element

*  Local Data:
      DATA CTYPE / XT9C1, XT9C2, XT9C3, XT9C4, XT9C5, XT9C6 / 
      DATA PTYPE / XT9P1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of spectral components and result parameters.
*  Neither must be 0 or less.
      IF ( A_NCOMP .LE. 0 .OR. A_TNPAR .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_FAAD_E01', 'SPD_FAAD: Error reshaping ' //
     :      'result structure: The requested number of components ' //
     :      'or parameters is zero.', STATUS )
         GO TO 500
      END IF

*  Find the slot.
      SLOT = 0
      DO 1 I = 1, SPD__FMXR
         IF ( MNDF(I) .EQ. A_MNDF ) SLOT = I
 1    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FAAD_E02', 'SPD_FAAD: Error: ' //
     :      'No result structure accessed for that main NDF.', STATUS )
         GO TO 500
      END IF

*  Beyond this point the result structure may be corrupted if something
*  goes wrong.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Shape vectors that are indexed by spectral component.
*  They have to be unmapped and re-mapped.
      IF ( A_NCOMP .NE. NCOMP(SLOT) ) THEN
         DO 2 I = 1, XC9NC
            CALL DAT_UNMAP( CLOC(I,SLOT), STATUS )
            CALL DAT_ALTER( CLOC(I,SLOT), 1, A_NCOMP, STATUS )
            IF ( I .EQ. 2 ) THEN
               CALL DAT_MAP( CLOC(I,SLOT), TYPE(2,SLOT), ACCESS(SLOT),
     :            1, A_NCOMP, CPNTR(I,SLOT), STATUS )
            ELSE IF ( I .EQ. 5 .OR. I .EQ. 6 ) THEN
               CALL DAT_MAP( CLOC(I,SLOT), TYPE(3,SLOT), ACCESS(SLOT),
     :            1, A_NCOMP, CPNTR(I,SLOT), STATUS )
            ELSE
               CALL DAT_MAP( CLOC(I,SLOT), CTYPE(I), ACCESS(SLOT),
     :            1, A_NCOMP, CPNTR(I,SLOT), STATUS )
            END IF
 2       CONTINUE
         IF ( STATUS .NE. SAI__OK ) GO TO 400
      END IF

*  Shape according to total number of result parameters.
*  Arrays and vectors have to be unmapped and re-mapped.
      IF ( A_TNPAR .NE. TNPAR(SLOT) ) THEN

*     Shape result NDF.
         CALL NDF_UNMAP( RNDF(SLOT), 'DATA,VARIANCE', STATUS )
         RUBND(1,SLOT) = A_TNPAR
         CALL NDF_SBND( RNDIM(SLOT), RLBND(1,SLOT), RUBND(1,SLOT),
     :      RNDF(SLOT), STATUS )
         CALL NDF_MAP( RNDF(SLOT), 'DATA,VARIANCE', TYPE(1,SLOT),
     :      ACCESS(SLOT), DPNTR(1,SLOT), DNELM(SLOT), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 400

*     Shape vectors that are indexed by result parameters.
         DO 3 I = 1, XC9NP
            CALL DAT_UNMAP( PLOC(I,SLOT), STATUS )
            CALL DAT_ALTER( PLOC(I,SLOT), 1, A_TNPAR, STATUS )
            CALL DAT_MAP(   PLOC(I,SLOT), PTYPE(I), ACCESS(SLOT),
     :         1, A_TNPAR, PPNTR(I,SLOT), STATUS )
 3       CONTINUE
         IF ( STATUS .NE. SAI__OK ) GO TO 400
      END IF

*  If spectral component indexed vectors are longer now, fill new
*  elements.
      IF ( A_NCOMP .GT. NCOMP(SLOT) ) THEN

*     Line names.
         CALL SPD_FDAAC( NCOMP(SLOT)+1, A_NCOMP,
     :                   %VAL( CNF_PVAL(CPNTR(1,SLOT)) ),
     :                   'unidentified component', STATUS,
     :                   %VAL(CNF_CVAL(CSIZE)) )

*     Laboratory frequencies.
         IF ( TYPE(2,SLOT) .EQ. '_DOUBLE' ) THEN
            CALL SPD_FDAAD( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(2,SLOT)) ),
     :                      VAL__BADD, STATUS )
         ELSE
            CALL SPD_FDAAR( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(2,SLOT)) ),
     :                      VAL__BADR, STATUS )
         END IF

*     Component types.
         CALL SPD_FDAAC( NCOMP(SLOT)+1, A_NCOMP,
     :                   %VAL( CNF_PVAL(CPNTR(3,SLOT)) ),
     :                   'unknown function', STATUS,
     :                   %VAL(CNF_CVAL(CSIZE)) )

*     Number of result parameters for each spectral component.
         I = INT( (A_TNPAR-TNPAR(SLOT)) / (A_NCOMP-NCOMP(SLOT)) )
         I = MAX( I, 0 )
         CALL SPD_FDAAI( NCOMP(SLOT)+1, A_NCOMP,
     :                   %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                   I, STATUS )

*     Masks.
         IF ( TYPE(3,SLOT) .EQ. '_DOUBLE' ) THEN
            CALL SPD_FDAAD( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(5,SLOT)) ),
     :                      VAL__BADD, STATUS )
            CALL SPD_FDAAD( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(6,SLOT)) ),
     :                      VAL__BADD, STATUS )
         ELSE
            CALL SPD_FDAAR( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(5,SLOT)) ),
     :                      VAL__BADR, STATUS )
            CALL SPD_FDAAR( NCOMP(SLOT)+1, A_NCOMP,
     :                      %VAL( CNF_PVAL(CPNTR(6,SLOT)) ),
     :                      VAL__BADR, STATUS )
         END IF
      END IF

*  If result parameter indexed vectors are longer now, fill new
*  elements.
      IF ( A_TNPAR .GT. TNPAR(SLOT) ) THEN
         CALL SPD_FDAAC( TNPAR(SLOT)+1, A_TNPAR,
     :                   %VAL( CNF_PVAL(PPNTR(1,SLOT)) ),
     :                   'unknown parameter', STATUS,
     :                    %VAL(CNF_CVAL(CSIZE)) )

*  If parameter indexed vectors are shorter now, some retained spectral
*  components may have been deprived of parameters they used to have.
      ELSE IF ( A_TNPAR .LT. TNPAR(SLOT) ) THEN

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
 4       CONTINUE             ! Start of 'DO WHILE' loop
         IF ( J .LE. A_TNPAR .AND. I .LT. A_NCOMP ) THEN

*        Next component.
            I = I + 1

*        Find out what used to be its last parameter.
            J = J + SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4,SLOT)) ), I,
     :                         STATUS )

*        Check the WHILE condition.
            GO TO 4
         END IF                  ! End of 'DO WHILE' loop

*     If components I ... NCOMP lost some or all of their parameters set
*     their number of parameters to zero.
         IF ( J .GT. A_TNPAR ) THEN
            CALL SPD_FDAAI( I, A_NCOMP, %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                      0, STATUS )
         END IF
      END IF

*  Now finally update the shape in the common block.
      NCOMP(SLOT) = A_NCOMP
      TNPAR(SLOT) = A_TNPAR

*  If the result structure is left in an undefined state, delete it.
*  (If this routine aborts before the result structure is harmed, it
*  diverts from above to label 500.)
 400  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN

*     See SPD_FAAB for an explanation of how to release the structure.
*     The only difference here is that RNDF(SLOT) is not annulled, but
*     deleted. That should get rid of it in the container file as well.
         CALL ERR_REP( 'SPD_FAAD_E03', 'SPD_FAAD: Error reshaping ' //
     :      'a result structure, the structure is corrupted and ' //
     :      'will now be deleted.', STATUS )
         DO 5 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC(I,SLOT), STATUS )
 5       CONTINUE
         DO 6 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC(I,SLOT), STATUS )
 6       CONTINUE
         CALL NDF_DELET( RNDF(SLOT), STATUS )
         CALL DAT_ANNUL( XLOC(SLOT), STATUS )
         MNDF( SLOT ) = NDF__NOID
      END IF

*  Return.
 500  CONTINUE
      END
