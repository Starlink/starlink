      SUBROUTINE SPD_FDHC( XLOC, COMP, NPARAI, STATUS )
*+
*  Name:
*     SPD_FDHC

*  Purpose:
*     Change number of parameters for a result component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHC( XLOC, COMP, NPARAI, STATUS )

*  Description:
*     This routine sets the number of parameter for a component in the
*     result structure. If there are not sufficient unused parameters
*     available in the existing result structure, then it will be
*     reshaped to accommodate the increased needs. This routine will
*     also shift the information in the result NDF data and variance
*     structures and in the result parameter type vector. The result
*     structure must already exist. So must the component addressed.

*  Arguments:
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the Specdre Extension, of which the result
*        NDF is an HDS component.
*     COMP = INTEGER (Given)
*        The spectral component for which the number of parameters is to
*        be changed.
*     NPARAI = INTEGER (Given)
*        The new number of parameters to be allocated to the spectral
*        component.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the structure does not exist,
*        -  if the result NDF has an invalid storage type,
*        -  if the addressed component does not exist already.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-MAR-1992 (HME):
*        Original version (SPABD).
*     22-JUN-1992 (HME):
*        Make it an SPE-routine.
*     24 Nov 1994 (hme):
*        Rename from SPEHC.
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
      CHARACTER * ( * ) XLOC
      INTEGER COMP
      INTEGER NPARAI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
*  There is an assumption here that PARATYPE is the only parameter
*  related extension of the result NDF.
      CHARACTER * ( 5 ) CNAME    ! Spectral component indexed vector
      PARAMETER ( CNAME = 'NPARA' )
      CHARACTER * ( 8 ) PNAME    ! Result parameter indexed vector
      PARAMETER ( PNAME = 'PARATYPE' )
      INTEGER CSIZE              ! Length of strings
      PARAMETER ( CSIZE = 32 )

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER I                  ! Loop index
      INTEGER SHIFT0             ! The origin of the shift within arrays
      INTEGER SHIFT1             ! The rightward shift amount
      INTEGER TNPAR              ! Total number of parameters
      INTEGER NCOMP              ! Number of components
      INTEGER NPARA              ! Pointer to NPARA vector
      INTEGER OLDVAL             ! Previous value of NPARA(COMP)
      INTEGER NEWTNP             ! Total number of parameters necessary
      INTEGER XNDF               ! Result NDF identifier
      INTEGER NDIM               ! Result NDF dimensionality
      INTEGER NDFSIZ             ! Total number of pixels
      INTEGER DIM2               ! NDFSIZ/NEWTNP
      INTEGER LBND( NDF__MXDIM ) ! Result NDF lower bounds
      INTEGER UBND( NDF__MXDIM ) ! Result NDF upper bounds
      INTEGER PNTR( 3 )          ! Pointers to data, variance, paratype
      CHARACTER * ( NDF__SZTYP ) TYPE ! Type of NDF arrays
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to NPARA vector
      CHARACTER * ( DAT__SZLOC ) PLOC ! Locator to PARATYPE vector

*  Internal References:
      INTEGER SPD_FDABI          ! Get an integer array element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the result NDF and check its type.
      CALL DAT_THERE( XLOC, XCMP9, EXIST, STATUS )
      IF ( .NOT. EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOEXST', 'SPD_FDHC: Error accessing ' //
     :      'result structure. Structure does not exist.', STATUS )
         GO TO 500
      END IF
      CALL NDF_FIND( XLOC, XCMP9, XNDF, STATUS )
      CALL NDF_TYPE( XNDF, 'DATA,VARIANCE', TYPE, STATUS )
      IF ( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVTYP', 'SPD_FDHC: Error: Existing ' //
     :      'result structure has invalid type.', STATUS )
         GO TO 500
      END IF

*  Locate the result NDF's NPARA extension.
*  The length of this vector is the number of spectral components.
      CALL NDF_XLOC( XNDF, CNAME, 'UPDATE', CLOC, STATUS )
      CALL DAT_SIZE( CLOC, NCOMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( COMP .LT. 0 .OR. COMP .GT. NCOMP ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHC: Error: The ' //
     :      'addressed component does not exist.', STATUS )
         GO TO 500
      END IF

*  Map the NPARA vector.
      CALL DAT_MAPI( CLOC, 'UPDATE', 1, NCOMP, NPARA, STATUS )

*  How many parameters did the COMP-th component use to have.
      OLDVAL = SPD_FDABI( %VAL( CNF_PVAL(NPARA) ), COMP, STATUS )

*  If some fool called this routine to change the value to what it was
*  anyway, then return in an orderly fashion, but without action.
      IF ( NPARAI .EQ. OLDVAL ) GO TO 600

*  If we got here, we really have to work.
*  So locate the PARATYPE vector.
*  Find out its current size, the old total number of parameters.
      CALL NDF_XLOC( XNDF, PNAME, 'UPDATE', PLOC, STATUS )
      CALL DAT_SIZE( PLOC, TNPAR, STATUS )

*  Now let's see how many parameters we must provide for in future.
*  While we're at it, also find out the shift parameters for later use.
*  Later we have to shift within arrays along the parameter axis. The
*  elements 1 to SHIFT0 remain unaffected and the rest is shifted by
*  SHIFT1 to the right (SHIFT1 may be negative).
      NEWTNP = 0
      DO 1 I = 1, NCOMP
         NEWTNP = NEWTNP + SPD_FDABI( %VAL( CNF_PVAL(NPARA) ), I,
     :                                STATUS )
         IF ( I .EQ. COMP ) SHIFT0 = NEWTNP
 1    CONTINUE
      SHIFT1 = NPARAI  - OLDVAL
      IF ( SHIFT1 .LT. 0 ) SHIFT0 = SHIFT0 + SHIFT1
      NEWTNP = NEWTNP + SHIFT1
      NEWTNP = MAX( NEWTNP, TNPAR )

*  If we need more space for parameters, get it.
      IF ( NEWTNP .GT. TNPAR ) THEN
         CALL NDF_BOUND( XNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         LBND(1) = 1
         UBND(1) = NEWTNP
         CALL NDF_SBND( NDIM, LBND, UBND, XNDF, STATUS )
         CALL DAT_ALTER( PLOC, 1, NEWTNP, STATUS )
      END IF

*  Map the arrays within which we have to shift data. These are the
*  NDF's data and variance structures and its PARATYPE extension.
      CALL NDF_MAP(  XNDF, 'DATA,VARIANCE', TYPE, 'UPDATE', PNTR(2),
     :   NDFSIZ, STATUS )
      CALL DAT_MAPC( PLOC, 'UPDATE', 1, NEWTNP, PNTR(1), STATUS )

*  Whatever the dimensionality of the NDF, we have to distinguish only
*  the first from the others. So we treat it as 2-D.
*  The second dimension here is the product of all dimensions
*  except the first. That is good enough for the shift process.
      DIM2 = NDFSIZ / NEWTNP

*  Do the shift.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_FDACD( VAL__BADD, NEWTNP, DIM2, SHIFT0,
     :                   SHIFT1, %VAL( CNF_PVAL(PNTR(2)) ), STATUS )
         CALL SPD_FDACD( VAL__BADD, NEWTNP, DIM2, SHIFT0,
     :                   SHIFT1, %VAL( CNF_PVAL(PNTR(3)) ), STATUS )
      ELSE
         CALL SPD_FDACR( VAL__BADR, NEWTNP, DIM2, SHIFT0,
     :                   SHIFT1, %VAL( CNF_PVAL(PNTR(2)) ), STATUS )
         CALL SPD_FDACR( VAL__BADR, NEWTNP, DIM2, SHIFT0,
     :                   SHIFT1, %VAL( CNF_PVAL(PNTR(3)) ), STATUS )
      END IF
      CALL SPD_FDACC( 'unknown parameter', NEWTNP, DIM2, SHIFT0,
     :   SHIFT1, %VAL( CNF_PVAL(PNTR(1) )), STATUS,
     :   %VAL(CNF_CVAL(CSIZE)) )

*  Finally, store the new value into its place.
      CALL SPD_FDAAI( COMP, COMP, %VAL( CNF_PVAL(NPARA) ), NPARAI,
     :                STATUS )

*  Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( PLOC, STATUS )
 600  CONTINUE
      CALL DAT_ANNUL( CLOC, STATUS )
      CALL NDF_ANNUL( XNDF, STATUS )

*  Return.
      END
