      SUBROUTINE SPD_FABBR( A_MNDF, COMP, LINNAM, LABFRQ, CMPTYP,
     :   NPARA, PARTYP, STATUS )
*+
*  Name:
*     SPD_FABB{DR}

*  Purpose:
*     Set information for a component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FABBR( MNDF, COMP, LINNAM, LABFRQ, CMPTYP, NPARA,
*        PARTYP, STATUS )

*  Description:
*     This routine changes the decsription of a component in the result
*     structure of the given main NDF. A change of NPARA from its
*     current value implies re-shaping the result structure.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The main NDF identifier. This is used to identify the accessed
*        result structure in question.
*     COMP = INTEGER (Given)
*        The component number. This must specify an existing component.
*        Especially it must be positive.
*     LINNAM = CHARACTER * ( * ) (Given)
*        The line name.
*     LABFRQ = REAL (Given)
*        The laboratory frequency. Its numeric type must correspond
*        to the information in TYPE. Its value should be positive or
*        bad.
*     CMPTYP = CHARACTER * ( * ) (Given)
*        The component type.
*     NPARA = INTEGER (Given)
*        The number of parameters in the component.
*     PARTYP( NPARA ) = CHARACTER * ( * ) (Given)
*        The parameter types.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1994 (hme):
*        Original version. Adapted from SPEHC.
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
      INTEGER COMP
      CHARACTER * ( * ) LINNAM
      REAL LABFRQ
      CHARACTER * ( * ) CMPTYP
      INTEGER NPARA
      CHARACTER * ( * ) PARTYP( NPARA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XCLEN              ! Standard length of strings
      PARAMETER ( XCLEN = 32 )
      CHARACTER * ( NDF__SZTYP ) TYPER
      PARAMETER ( TYPER = '_REAL' )
      CHARACTER * ( NDF__SZTYP ) TYPED
      PARAMETER ( TYPED = '_DOUBLE' )

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number
      INTEGER FPARA              ! First parameter of this component
      INTEGER OLDVAL             ! Previous NPARA
      INTEGER NEWTNP             ! New TNPAR
      INTEGER SHIFT0, SHIFT1     ! Parameters to shift within arrays
      INTEGER DIM2               ! Second dimension for shift
      CHARACTER * ( 8 ) CTYPE( XC9NC ) ! Extension data types
      CHARACTER * ( 8 ) PTYPE( XC9NP ) ! Extension data types

*  Internal References:
      INTEGER SPD_FDABI          ! Get an array element

*  Local Data:
      DATA CTYPE / XT9C1, XT9C2, XT9C3, XT9C4, XT9C5, XT9C6 /
      DATA PTYPE / XT9P1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that NPARA is not negative.
      IF ( NPARA .LT. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FABB_E01', 'SPD_FABBR: Error: ' //
     :      'Number of parameters is invalid (negative).', STATUS )
         GO TO 500
      END IF

*  Find the slot.
      SLOT = 0
      DO 1 I = 1, SPD__FMXR
         IF ( MNDF(I) .EQ. A_MNDF ) SLOT = I
 1    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FABB_E02', 'SPD_FABBR: Error: ' //
     :      'No result structure accessed for that main NDF.', STATUS )
         GO TO 500
      END IF

*  Check that the type is correct.
      IF ( TYPE(2,SLOT) .NE. TYPER ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FABB_E03', 'SPD_FABBR: Error: ' //
     :      'Data type mismatch for laboratory frequency.', STATUS )
         GO TO 500
      END IF

*  Set up the line name, lab frequency, component type.
      CALL SPD_FDAAC( COMP, COMP, %VAL( CNF_PVAL(CPNTR(1,SLOT)) ),
     :                LINNAM, STATUS, %VAL(CNF_CVAL(XCLEN)) )
      CALL SPD_FDAAC( COMP, COMP, %VAL( CNF_PVAL(CPNTR(3,SLOT)) ),
     :                CMPTYP, STATUS, %VAL(CNF_CVAL(XCLEN)) )
      CALL SPD_FDAAR( COMP, COMP, %VAL( CNF_PVAL(CPNTR(2,SLOT)) ),
     :                LABFRQ, STATUS )

*  Beyond this point the result structure may be corrupted if something
*  goes wrong.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  The number of parameters needs more care. If it actually changes,
*  then the NDF and the parameter-related vectors must be reshaped and
*  parts of them shifted to make space where it is needed.
*  ===================================================================

*  Get the previous number of parameters for component in question.
      OLDVAL = SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4,SLOT)) ), COMP,
     :                    STATUS )

*  It gets difficult only if the new NPARA is different.
      IF ( NPARA .NE. OLDVAL ) THEN

*     Let's see how many parameters we must provide for in future.
*     While we're at it, also find out the shift parameters for later use.
*     Later we have to shift within arrays along the parameter axis. The
*     elements 1 to SHIFT0 remain unaffected and the rest is shifted by
*     SHIFT1 to the right (SHIFT1 may be negative).
         NEWTNP = 0
         DO 2 I = 1, NCOMP(SLOT)
            NEWTNP = NEWTNP + SPD_FDABI(%VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                                  I, STATUS )
            IF ( I .EQ. COMP ) SHIFT0 = NEWTNP
 2       CONTINUE
         SHIFT1 = NPARA - OLDVAL
         IF ( SHIFT1 .LT. 0 ) SHIFT0 = SHIFT0 + SHIFT1
         NEWTNP = NEWTNP + SHIFT1
         NEWTNP = MAX( NEWTNP, TNPAR(SLOT) )

*     If we need more space for parameters, get it.
         IF ( NEWTNP .GT. TNPAR(SLOT) ) THEN

*        Shape result NDF.
            CALL NDF_UNMAP( RNDF(SLOT), 'DATA,VARIANCE', STATUS )
            RUBND(1,SLOT) = NEWTNP
            CALL NDF_SBND( RNDIM(SLOT), RLBND(1,SLOT), RUBND(1,SLOT),
     :         RNDF(SLOT), STATUS )
            CALL NDF_MAP( RNDF(SLOT), 'DATA,VARIANCE', TYPE(1,SLOT),
     :         ACCESS(SLOT), DPNTR(1,SLOT), DNELM(SLOT), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 400

*        Shape vectors that are indexed by result parameters.
            DO 3 I = 1, XC9NP
               CALL DAT_UNMAP( PLOC(I,SLOT), STATUS )
               CALL DAT_ALTER( PLOC(I,SLOT), 1, NEWTNP, STATUS )
               CALL DAT_MAP(   PLOC(I,SLOT), PTYPE(I), ACCESS(SLOT),
     :            1, NEWTNP, PPNTR(I,SLOT), STATUS )
 3          CONTINUE
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 400

*     Whatever the dimensionality of the NDF, we have to distinguish only
*     the first from the others. So we treat it as 2-D.
*     The second dimension here is the product of all dimensions
*     except the first. That is good enough for the shift process.
         DIM2 = DNELM(SLOT) / NEWTNP

*     Do the shift.
*     This should be one call for each array, with a GEN routine.
*     Currently it assumes there is only one parameter-related array.
         IF ( TYPE(1,SLOT) .EQ. '_DOUBLE' ) THEN
            CALL SPD_FDACD( VAL__BADD, NEWTNP, DIM2, SHIFT0, SHIFT1,
     :                      %VAL( CNF_PVAL(DPNTR(1,SLOT)) ), STATUS )
            CALL SPD_FDACD( VAL__BADD, NEWTNP, DIM2, SHIFT0, SHIFT1,
     :                      %VAL( CNF_PVAL(DPNTR(2,SLOT)) ), STATUS )
         ELSE
            CALL SPD_FDACR( VAL__BADR, NEWTNP, DIM2, SHIFT0, SHIFT1,
     :                      %VAL( CNF_PVAL(DPNTR(1,SLOT)) ), STATUS )
            CALL SPD_FDACR( VAL__BADR, NEWTNP, DIM2, SHIFT0, SHIFT1,
     :                      %VAL( CNF_PVAL(DPNTR(2,SLOT)) ), STATUS )
         END IF
         CALL SPD_FDACC( 'uknown parameter', NEWTNP, 1, SHIFT0, SHIFT1,
     :                   %VAL( CNF_PVAL(PPNTR(1,SLOT)) ), STATUS,
     :                   %VAL(CNF_CVAL(XCLEN)) )

*     Store the new value into its place.
         TNPAR(SLOT) = NEWTNP
         CALL SPD_FDAAI( COMP, COMP, %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                   NPARA, STATUS )
      END IF

*  The danger of corrupting the result structure is over, this is the
*  last diversion to deleting the result structure.
      IF ( STATUS .NE. SAI__OK ) GO TO 400

*  Set the parameter types.
*  If something goes wrong here, just leave the routine, but don't
*  delete the result structure.
      FPARA = 1
      DO 4 I = 1, COMP - 1
         FPARA = FPARA + SPD_FDABI(%VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                             I, STATUS )
 4    CONTINUE
      DO 5 I = 0, NPARA - 1
         CALL SPD_FDAAC( FPARA+I, FPARA+I,
     :                   %VAL( CNF_PVAL(PPNTR(1,SLOT)) ),
     :                   PARTYP(I+1), STATUS, %VAL(CNF_CVAL(XCLEN)) )
 5    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If the result structure is left in an undefined state, delete it.
*  (If this routine aborts before the result structure is harmed, it
*  diverts from above to label 500.)
 400  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN

*     See SPD_FAAB for an explanation of how to release the structure.
*     The only difference here is that RNDF(SLOT) is not annulled, but
*     deleted. That should get rid of it in the container file as well.
         CALL ERR_REP( 'SPD_FABB_E04', 'SPD_FABBR: Error ' //
     :      'reshaping a result structure, the structure is ' //
     :      'corrupted and will now be deleted.', STATUS )
         DO 6 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC(I,SLOT), STATUS )
 6       CONTINUE
         DO 7 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC(I,SLOT), STATUS )
 7       CONTINUE
         CALL NDF_DELET( RNDF(SLOT), STATUS )
         CALL DAT_ANNUL( XLOC(SLOT), STATUS )
         MNDF( SLOT ) = NDF__NOID
      END IF

*  Return.
 500  CONTINUE
      END
