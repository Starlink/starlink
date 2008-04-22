      SUBROUTINE SPD_CZWG( FILE, FU, NDF, XLOC, STATUS )
*+
*  Name:
*     SPD_CZWG

*  Purpose:
*     List result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWG( FILE, FU, NDF, XLOC, STATUS )

*  Description:
*     This routine lists the contents of the result structure in a
*     Specdre Extension either to an ASCII file or to the default output
*     device (user's screen). The list comprises contents of extension
*     vectors to the result structure, but not the contents of the NDF.

*  Arguments:
*     FILE = LOGICAL (Given)
*        True if an ASCII file is to be used, has been opened
*        successfully and is identified by the Fortran unit number FU.
*     FU = INTEGER (Given)
*        The Fortran unit number of the output ASCII file. This is
*        unused if FILE is false.
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the Extension. This should be an extension
*        of the main NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-MAR-1992 (HME):
*        Original version.
*     22-JUN-1992 (HME):
*        Use SPE-routines and include file SPEPAR.
*     24 Nov 1994 (hme):
*        Renamed from SPAAQ.
*     2005 May 31 (MJC):
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
      LOGICAL FILE
      INTEGER FU
      INTEGER NDF
      CHARACTER * ( * ) XLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER XNDF               ! NDF identifier
      INTEGER DPNTR( 2 )         ! Pointers to result NDF arrays
      INTEGER CPNTR( XC9NC )     ! Pointers to result vectors
      INTEGER PPNTR( XC9NP )     ! Pointers to result vectors
      INTEGER TNPAR              ! Total number of result parameters
      INTEGER NCOMP              ! Number of spectral components
      INTEGER COMP( 2 )          ! First and last component accessed
      INTEGER NELM( 3 )          ! Array sizes
      CHARACTER * ( 78 ) SHORT   ! Short string to write to screen
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Numeric types found
      CHARACTER * ( DAT__SZLOC ) CLOC( XC9NC ) ! Locators to vectors
      CHARACTER * ( DAT__SZLOC ) PLOC( XC9NP ) ! Locators to vectors

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats.
 101  FORMAT ( ' Result structure provides for ', I4,
     :   ' parameters in ', I3, ' components.' )
 102  FORMAT ( A )

*  Find out about and check the result structure.
*  Then access and map the structure.
*  Masks are mapped _REAL, regardless of how they are
*  actually stored. This is required by SPD_CZWH, SPD_CZWJ. The NDF arrays
*  (data and variance) are not actually used. Since they are mapped with
*  the same type as stored in the file, mapping them should not require
*  excessive resources.
      CALL SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
      TYPE(3) = '_REAL'
      COMP(1) = 1
      COMP(2) = NCOMP
      CALL SPD_FDHD( NDF, XLOC, 'READ', TYPE, COMP, XNDF,
     :               CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )

*  If list goes to ASCII file.
      IF ( FILE ) THEN
         WRITE( FU, 101 ) TNPAR, NCOMP
         WRITE( FU, 102 ) ' '

*     List the components.
         IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
            CALL SPD_CZWH( FILE, FU, NCOMP,
     :                     %VAL( CNF_PVAL( CPNTR(1) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(2) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(3) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(4) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(5) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(6) ) ),
     :                     STATUS, %VAL( CNF_CVAL( 32 ) ), 
     :                     %VAL( CNF_CVAL( 32 ) ) )
         ELSE IF ( TYPE(2) .EQ. '_REAL' ) THEN
            CALL SPD_CZWJ( FILE, FU, NCOMP,
     :                     %VAL( CNF_PVAL( CPNTR(1) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(2) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(3) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(4) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(5) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(6) ) ),
     :                     STATUS, %VAL( CNF_CVAL( 32 ) ), 
     :                     %VAL( CNF_CVAL( 32 ) ) )
         ELSE
            WRITE( FU, 102 )
     :         ' Cannot map laboratory frequencies. ' //
     :         ' They are neither _REAL nor _DOUBLE.'
         END IF

*     List the parameters.
         WRITE( FU, 102 ) ' '
         CALL SPD_CZWK( FILE, FU, TNPAR, %VAL( CNF_PVAL( PPNTR(1) ) ),
     :                  STATUS, %VAL( CNF_CVAL( 32 ) ) )

*  Else (list goes to screen etc.)
      ELSE
         WRITE( SHORT, 101 ) TNPAR, NCOMP
         CALL MSG_OUT( 'SPD_CZWG_LIST', SHORT, STATUS )
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWG_LIST', SHORT, STATUS )

*     List the components.
         IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
            CALL SPD_CZWH( FILE, FU, NCOMP,
     :                     %VAL( CNF_PVAL( CPNTR(1) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(2) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(3) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(4) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(5) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(6) ) ),
     :                     STATUS, %VAL( CNF_CVAL( 32 ) ), 
     :                     %VAL( CNF_CVAL( 32 ) ) )
         ELSE IF ( TYPE(2) .EQ. '_REAL' ) THEN
            SHORT = ' '
            CALL MSG_OUT( 'SPD_CZWG_LIST', SHORT, STATUS )
            CALL SPD_CZWJ( FILE, FU, NCOMP,
     :                     %VAL( CNF_PVAL( CPNTR(1) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(2) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(3) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(4) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(5) ) ),
     :                     %VAL( CNF_PVAL( CPNTR(6) ) ),
     :                     STATUS, %VAL( CNF_CVAL( 32 ) ), 
     :                     %VAL( CNF_CVAL( 32 ) ) )
         ELSE
            CALL MSG_OUT( 'SPD_CZWG_LIST',
     :         ' Cannot map laboratory frequencies. ' //
     :         ' They are neither _REAL nor _DOUBLE.' , STATUS )
         END IF

*     List the parameters.
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWG_LIST', SHORT, STATUS )
         CALL SPD_CZWK( FILE, FU, TNPAR, %VAL( CNF_PVAL( PPNTR(1) ) ),
     :                  STATUS, %VAL( CNF_CVAL( 32 ) ) )
      END IF

*  Unmap and release locators.
      DO 1 I = 1, XC9NC
         CALL DAT_ANNUL( CLOC(I), STATUS )
 1    CONTINUE
      DO 2 I = 1, XC9NP
         CALL DAT_ANNUL( PLOC(I), STATUS )
 2    CONTINUE

*  Release result NDF.
      CALL NDF_ANNUL( XNDF, STATUS )
      END
