      SUBROUTINE DSA_RESHAPE_DATA( DSAREF, MODREF, NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_RESHAPE_DATA

*  Purpose:
*     Create data arrays with modified dimensions in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_RESHAPE_DATA( DSAREF, MODREF, NDIM, DIMS, STATUS )

*  Description:
*     This routine combines the given shape for arrays of data and
*     possibly quality and/or variance with a model data set. The model
*     determines which components are created and what types and values
*     they have.
*
*     If both the model and the target are not yet valid NDFs (i.e. if
*     both are created via DSA_OUTPUT or DSA_NAMED_OUTPUT without
*     copying from a model), then the target will be given only a data
*     component of type FLOAT, lower bounds 1 and the given shape.
*
*     If the target is a valid NDF, but the model is only a placeholder,
*     then the target NDF is simply given new bounds according to the
*     old lower bounds and the given shape.
*
*     If the model is a valid NDF, but the target is only a placeholder,
*     then the model is first propagated to the target and then given
*     new bounds according to the model lower bounds and the given
*     shape. In this process, scalar NDF components are copied, array
*     components first copied then reshaped. The axes are not copied
*     from the model.  If the target NDF has smaller array components
*     than the model, the copying process is via a temporary NDF; this
*     trims unused space from the target NDF.
*
*     Otherwise (the normal case!), all target components are reset,
*     the array types are changed to those in the model, all model
*     components (not the axes) are copied, and the copied arrays are
*     reshaped according to the model lower bounds and the given shape.
*
*     In all cases array data will be re-ordered according to the old
*     and new bounds. New areas are given the bad value. This is
*     contrary to earlier implementations where data were not
*     re-ordered. As in earlier implementations, if the model
*     shape and the new target shape are the same, then the data are
*     in effect simply copied from the model to the target.
*
*     When this routine reshapes an NDF, any one-dimensional axis
*     components will be reshaped implicitly, but N-dimensional centre
*     or width arrays will remain unchanged. Those can be reshaped with
*     DSA_RESHAPE_AXIS after the call to this routine.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MODREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the structure whose axis
*        data is to serve as model for the reshaped structure.
*     NDIM = INTEGER (Given)
*        The number of dimensions for the reshaped data.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the reshaped data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28 Jun 1988 (ks):
*        Original version.
*     11 Dec 1989 (ks):
*        Reshape flag now set.
*     26 Feb 1990 (ks):
*        Modified to use DSA__ routines to get structure details,
*        instead of assuming the original Figaro data structure.
*     05 Mar 1990 (ks):
*        Bug where a non-existent array was being copied fixed.
*     26 Feb 1991 (ks):
*        Test for NDIM=0 added. DIMS now dimensioned as '*' instead of
*        using MAX.
*     30 Mar 1991 (ks):
*        Tests for mismatch of file types, although all it does is
*        signal an error instead of handling the case properly.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     12 Feb 1995 (ks):
*        Now handles NDF quality structures properly.
*     20 Jul 1995 (hme):
*        Remove duplicate declarations.
*     05 Feb 1996 (hme):
*        FDA library.
*     12 Feb 1996 (hme):
*        Mark reference as data-reshaped.
*     19 Feb 1996 (hme):
*        Use a cloned NDF identifier for mapping model arrays, to avoid
*        conflicts with other read accesses.
*        Translate between application-side status and Starlink status.
*     01 Jul 1996 (mjcl):
*        Removed CHR_LEN-based string truncation from NDF_CPUT calls.
*     1996 November 4 (MJC):
*        Use a temporary NDF when there is a valid model NDF, the input
*        NDF is a placeholder, and the array size is reduced.  This
*        extra copy trims the physical size of new NDF.  Some reordering
*        of the variable/function declarations.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) MODREF
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CHANGE             ! Indicates change in dimensions
      INTEGER ELMOD              ! Number of elements in model data
      INTEGER ELNEW              ! Number of elements in new data
      INTEGER I                  ! Loop index
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      LOGICAL LTEMP              ! Ignored
      INTEGER MDIM               ! Number of dimensions in model data
      INTEGER MDIMS( NDF__MXDIM ) ! Dimensions of model data
      INTEGER MODSLT             ! The model slot
      INTEGER MODNDF             ! Cloned model NDF identifier
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! Data type
      INTEGER NELM1, NELM2       ! Array sizes
      INTEGER ONE( NDF__MXDIM )  ! An NDF lower bound
      INTEGER PNTR1, PNTR2       ! Array pointers
      LOGICAL QTHERE             ! Whether QUALITY structure exists
      LOGICAL SAME               ! Indicates data structures are same
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 256 ) STRING ! Any string for copying
      LOGICAL THERE              ! Whether NDF components exist
      INTEGER TNDF               ! NDF identifier for temporary NDF
      INTEGER TPLACE             ! Placeholder for temporary NDF
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*  Local Data:
      DATA ONE / NDF__MXDIM * 1 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference names.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_RFND( MODREF, MODSLT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Model and target are only placeholders.
*  =======================================
*
*  Create target with given shape and type _REAL.
      IF ( DSA__REFID1(SLOT)   .EQ. NDF__NOID .AND.
     :     DSA__REFID1(MODSLT) .EQ. NDF__NOID ) THEN
         CALL NDF_NEW( '_REAL', NDIM, ONE, DIMS, DSA__REFPLC(SLOT),
     :      DSA__REFID1(SLOT), STATUS )
         DSA__REFRSD(SLOT) = .TRUE.
         GO TO 500
      END IF

*  Model is only a placeholder.
*  ============================
*
*  Reshape target to given shape, types remain as they are.
      IF ( DSA__REFID1(MODSLT) .EQ. NDF__NOID ) THEN
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :      LBND, UBND, I, STATUS )
         DO 1 I = 1, NDIM
            UBND(I) = LBND(I) - 1 + DIMS(I)
 1       CONTINUE
         CALL NDF_SBND( NDIM, LBND, UBND, DSA__REFID1(SLOT), STATUS )
         DSA__REFRSD(SLOT) = .TRUE.
         GO TO 500
      END IF

*  Target is only placeholder.
*  ===========================
*
*  Propagate all Data, Quality, Variance, Units, and default propagated
*  components for the model to the target, then reshape the target.
*  This, however, can generate a target dataset which is approximately
*  the same size as the model, even if the array components are much
*  smaller.  So a copy has to be made to reclaim the unused space.
      IF ( DSA__REFID1(SLOT) .EQ. NDF__NOID ) THEN
         CALL NDF_SIZE( DSA__REFID1(MODSLT), ELMOD, STATUS )
         CALL NDF_BOUND( DSA__REFID1(MODSLT), NDF__MXDIM,
     :      LBND, UBND, I, STATUS )
         ELNEW = 1
         DO 2 I = 1, NDIM
            ELNEW = ELNEW * DIMS(I)
            UBND(I) = LBND(I) - 1 + DIMS(I)
 2       CONTINUE

*  Test for a smaller target than model.  This ignores any file space
*  vacated by the extensions and history so perhaps there should be a
*  factor applied to ELMOD, say 0.9.
         IF ( ELNEW .LT. ELMOD ) THEN

*  Get a placeholder to a temporary NDF.
            CALL NDF_TEMP( TPLACE, STATUS )

*  Copy from the model NDF to the temporary NDF.  create the target
*  NDF.  Adjust the temporary NDF's bounds to the desired limits.
            CALL NDF_SCOPY( DSA__REFID1(MODSLT),
     :         'DATA,QUAL,VAR,UNITS,NOHISTORY,NOEXTENSION()',
     :         TPLACE, TNDF, STATUS )

            CALL NDF_SBND( NDIM, LBND, UBND, TNDF, STATUS )

*  Copy from the temporary to the target NDF via the target placeholder.
            CALL NDF_COPY( TNDF, DSA__REFPLC(SLOT), DSA__REFID1(SLOT),
     :         STATUS )

*  Delete the temporary NDF.
            CALL NDF_ANNUL( TNDF, STATUS )

*  Just use the target's placeholder to copy from the model NDF to
*  create the target NDF.  Adjust its bounds to the desired limits.
         ELSE
            CALL NDF_SCOPY( DSA__REFID1(MODSLT),
     :         'DATA,QUAL,VAR,UNITS,NOHISTORY,NOEXTENSION()',
     :         DSA__REFPLC(SLOT), DSA__REFID1(SLOT), STATUS )

            CALL NDF_SBND( NDIM, LBND, UBND, DSA__REFID1(SLOT), STATUS )
         END IF
         DSA__REFRSD(SLOT) = .TRUE.
         GO TO 500
      END IF

*  Both model and target are valid NDFs.
*  =====================================

*  Establish whether model and target are the same NDF.
      CALL NDF_SAME( DSA__REFID1(SLOT), DSA__REFID1(MODSLT),
     :   SAME, LTEMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Establish whether model has the same shape as given.
      CALL NDF_DIM( DSA__REFID1(MODSLT), NDF__MXDIM,
     :   MDIMS, MDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CHANGE = MDIM .NE. NDIM
      IF ( .NOT. CHANGE ) THEN
         DO 3 I = 1, NDIM
            IF ( MDIMS(I) .NE. DIMS(I) ) CHANGE = .TRUE.
 3       CONTINUE
      END IF

*  If same and no change in shape, return.
      IF ( SAME .AND. .NOT. CHANGE ) GO TO 500


*  Got here, so both are valid NDFs and the copy is more work on our
*  part. It is also not as easy as having the same NDF without change in
*  shape.
*  If the two are not the same, then we first have to:
*     - reset the target
*     - give target arrays same type and shape as model (!)
*     - re-type and copy arrays
*       (model and target have same size at this point)
*     - re-type and copy scalars
*  Then (same or not) we give the target the given shape.
*  -----------------------------------------------------------------

*  If not same.
      IF ( .NOT. SAME ) THEN

*     Reset all but data and axis.
         CALL NDF_RESET( DSA__REFID1(SLOT),
     :      'VAR,TITLE,LABEL,UNITS,HISTORY,EXTENSION', STATUS )

*     Reshape target to same shape as model.
         CALL NDF_BOUND( DSA__REFID1(MODSLT), NDF__MXDIM,
     :      LBND, UBND, I, STATUS )
         CALL NDF_SBND( NDIM, LBND, UBND, DSA__REFID1(SLOT), STATUS )

*     Copy quality, data, variance by mapping model and target.
*     Due to the interaction between quality and the other two, we map
*     quality first and unmap it last.
         CALL NDF_CLONE( DSA__REFID1(MODSLT), MODNDF, STATUS )
         CALL NDF_STATE( MODNDF, 'QUAL', QTHERE, STATUS )
         IF ( QTHERE ) THEN
            CALL NDF_MAP( MODNDF,'QUAL', '_UBYTE', 'READ',
     :                    PNTR1, NELM1, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT),  'QUAL', '_UBYTE', 'WRITE',
     :                    PNTR2, NELM2, STATUS )
            CALL DSA1_CPDAT( '_UBYTE', '_UBYTE', NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )
         END IF

         CALL NDF_TYPE(  MODNDF, 'DATA', NDFTYP, STATUS )
         CALL NDF_STYPE( NDFTYP, DSA__REFID1(SLOT),   'DATA', STATUS )
         CALL NDF_MAP(   MODNDF, 'DATA', NDFTYP, 'READ',
     :      PNTR1, NELM1, STATUS )
         CALL NDF_MAP( DSA__REFID1(SLOT),     'DATA', NDFTYP, 'WRITE',
     :      PNTR2, NELM2, STATUS )
         CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                    %VAL( CNF_PVAL(PNTR1) ),
     :                    %VAL( CNF_PVAL(PNTR2) ), STATUS )
         CALL NDF_UNMAP( MODNDF, 'DATA', STATUS )
         CALL NDF_UNMAP( DSA__REFID1(SLOT),   'DATA', STATUS )

         CALL NDF_STATE( MODNDF, 'VAR', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_TYPE(  MODNDF, 'VAR', NDFTYP, STATUS )
            CALL NDF_STYPE( NDFTYP, DSA__REFID1(SLOT),  'VAR',  STATUS )
            CALL NDF_MAP(   MODNDF, 'VAR', NDFTYP, 'READ',
     :         PNTR1, NELM1, STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT),     'VAR', NDFTYP, 'WRITE',
     :         PNTR2, NELM2, STATUS )
            CALL DSA1_CPDAT( NDFTYP, NDFTYP, NELM1,
     :                       %VAL( CNF_PVAL(PNTR1) ),
     :                       %VAL( CNF_PVAL(PNTR2) ), STATUS )
            CALL NDF_UNMAP( MODNDF, 'VAR', STATUS )
            CALL NDF_UNMAP( DSA__REFID1(SLOT),   'VAR', STATUS )
         END IF
         IF ( QTHERE ) THEN
            CALL NDF_UNMAP( MODNDF, 'QUAL', STATUS )
            CALL NDF_UNMAP( DSA__REFID1(SLOT),   'QUAL', STATUS )
         END IF
         CALL NDF_ANNUL( MODNDF, STATUS )

*     Copy title, label, unit.
         CALL NDF_STATE( DSA__REFID1(MODSLT), 'TITLE', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_CGET( DSA__REFID1(MODSLT), 'TITLE',
     :         STRING, STATUS )
            CALL NDF_CPUT( STRING, DSA__REFID1(SLOT),
     :         'TITLE', STATUS )
         END IF
         CALL NDF_STATE( DSA__REFID1(MODSLT), 'LABEL', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_CGET( DSA__REFID1(MODSLT), 'LABEL',
     :         STRING, STATUS )
            CALL NDF_CPUT( STRING, DSA__REFID1(SLOT),
     :         'LABEL', STATUS )
         END IF
         CALL NDF_STATE( DSA__REFID1(MODSLT), 'UNITS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_CGET( DSA__REFID1(MODSLT), 'UNITS',
     :         STRING, STATUS )
            CALL NDF_CPUT( STRING, DSA__REFID1(SLOT),
     :         'UNITS', STATUS )
         END IF

      END IF

*  Find current target bounds, work out new bounds, set bounds.
      IF ( CHANGE ) THEN
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :      LBND, UBND, I, STATUS )
         DO 4 I = 1, NDIM
            UBND(I) = LBND(I) - 1 + DIMS(I)
 4       CONTINUE
         CALL NDF_SBND( NDIM, LBND, UBND, DSA__REFID1(SLOT), STATUS )
         DSA__REFRSD(SLOT) = .TRUE.
      END IF

*  Tidy up.
 500  CONTINUE

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
