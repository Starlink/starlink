      SUBROUTINE SPD_FAAA( A_MNDF, A_ACCESS,
     :   A_NCOMP, A_TNPAR, A_TYPE, STATUS )
*+
*  Name:
*     SPD_FAAA

*  Purpose:
*     Access a result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FAAA( MNDF, ACCESS, NCOMP, TNPAR, TYPE, STATUS )

*  Description:
*     This routine accesses the result structure associated with the
*     given main NDF. If the result structure does not exist but the
*     access is write or update then it will be created with some
*     default shape. The Specdre Extension (the structure containing the
*     result structure) is created if necessary.
*
*     Any call to this routine should be matched by a later call to
*     SPD_FAAB to release the result structure. That should happen
*     before the main NDF is annulled. When the main NDF is annulled
*     then the result structure will be released as well and becomes
*     inaccessible.
*
*     Although this routine will map the contents of the result
*     structure, it will not grant direct access by the calling routine.
*     Other SPD_F* routines must be called to get and set contents.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'. For read and
*        update access the result structure must exist. For write
*        access any existing result structure is deleted and a new one
*        created.
*     NCOMP = INTEGER (Given)
*        For write access, the number of components. Otherwise unused.
*     TNPAR = INTEGER (Given)
*        For write access, the total number of parameters. Otherwise
*        unused.
*     TYPE( 3 ) = CHARACTER * ( * ) (Given and Returned)
*        The numeric types for mapping. These are for (1) data and
*        variance, (2) the LABFREQ extension, (3) the MASKL and MASKR
*        extensions. Each type can be '_REAL', '_DOUBLE' or blank. If
*        given blank and if the array is stored '_DOUBLE', then it is
*        mapped in double precision. If given blank and if the array is
*        not stored '_DOUBLE', then it its mapped '_REAL'. In effect,
*        usually a blank type specification causes the array to be
*        mapped with the stored type. On return, TYPE is '_REAL' or
*        '_DOUBLE' and tells the type actually used for mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Feb 1994 (hme):
*        Original version.
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Arguments Given:
      INTEGER A_MNDF
      CHARACTER * ( * ) A_ACCESS
      INTEGER A_NCOMP
      INTEGER A_TNPAR

*  Arguments Given and Returned:
      CHARACTER * ( * ) A_TYPE( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SPD_FBLK          ! Block data routine

*  Local Variables:
      LOGICAL XTHERE             ! True if Extension exists
      LOGICAL RTHERE             ! True if result structure exists
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number
      INTEGER CMPRNG( 2 )        ! Component range
      INTEGER NELM( 3 )          ! Array sizes

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find a free slot.
      SLOT = 0
      DO 1 I = SPD__FMXR, 1, -1
         IF ( MNDF(I) .EQ. NDF__NOID ) SLOT = I
 1    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FAAA_E01', 'SPD_FAAA: Error accessing a ' //
     :      'result structure: No free access slots left.', STATUS )
         GO TO 500
      END IF

*  Enter access mode into slot.
      ACCESS(SLOT) = A_ACCESS
      CALL CHR_UCASE( ACCESS(SLOT) )

*  If write access.
      IF ( ACCESS(SLOT) .EQ. 'WRITE' ) THEN

*     Access (possibly create) Specdre Extension.
         CALL SPD_EAAA( A_MNDF, 'UPDATE', XTHERE, XLOC(SLOT), STATUS )

*     Look for an existing result structure and delete it.
         CALL DAT_THERE( XLOC(SLOT), XCMP9, RTHERE, STATUS )
         IF ( RTHERE ) CALL DAT_ERASE( XLOC(SLOT), XCMP9, STATUS )

*     Create a new result structure.
         NCOMP(SLOT) = A_NCOMP
         TNPAR(SLOT) = A_TNPAR
         DO 2 I = 1, 3
            TYPE(I,SLOT) = A_TYPE(I)
            IF ( TYPE(I,SLOT) .EQ. ' ' ) TYPE(I,SLOT) = XT9D
 2       CONTINUE
         CALL SPD_FDHF( A_MNDF, XLOC(SLOT), NCOMP(SLOT), TNPAR(SLOT),
     :      TYPE(1,SLOT), STATUS )

*     Access result structure just created. Use UPDATE access to
*     preserve settings made by SPD_FDHF.
         CMPRNG(1) = 1
         CMPRNG(2) = NCOMP(SLOT)
         CALL SPD_FDHE( A_MNDF,
     :      XLOC(SLOT), 'UPDATE', TYPE(1,SLOT), CMPRNG,
     :      RNDF(SLOT), CLOC(1,SLOT), PLOC(1,SLOT),
     :      DPNTR(1,SLOT), CPNTR(1,SLOT), PPNTR(1,SLOT), NELM, STATUS )
         DNELM(SLOT) = NELM(1)

*  Else (read or update access).
      ELSE

*     Access Specdre Extension, absence is error.
         CALL SPD_EAAA( A_MNDF, 'READ', XTHERE, XLOC(SLOT), STATUS )
         IF ( .NOT. XTHERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_FAAA_E02', 'SPD_FAAA: Error ' //
     :         'accessing a result structure: The structure does ' //
     :         'not exist.', STATUS )
            GO TO 500
         END IF
         CALL DAT_ANNUL( XLOC(SLOT), STATUS )
         CALL SPD_EAAA( A_MNDF, 'UPDATE', XTHERE, XLOC(SLOT), STATUS )

*     Enquire about result structure, but revert to given data types.
         CALL SPD_FDHA( A_MNDF, XLOC(SLOT), NCOMP(SLOT), TNPAR(SLOT),
     :      TYPE(1,SLOT), STATUS )
         DO 3 I = 1, 3
            TYPE(I,SLOT) = A_TYPE(I)
 3       CONTINUE

*     Access existing result structure.
         CMPRNG(1) = 1
         CMPRNG(2) = NCOMP(SLOT)
         CALL SPD_FDHE( A_MNDF,
     :      XLOC(SLOT), ACCESS(SLOT), TYPE(1,SLOT), CMPRNG,
     :      RNDF(SLOT), CLOC(1,SLOT), PLOC(1,SLOT),
     :      DPNTR(1,SLOT), CPNTR(1,SLOT), PPNTR(1,SLOT), NELM, STATUS )
         DNELM(SLOT) = NELM(1)
      END IF

*  Fill in the slot: Bounds of main and result NDF.
      CALL NDF_BOUND( A_MNDF, NDF__MXDIM, MLBND(1,SLOT), MUBND(1,SLOT),
     :   MNDIM(SLOT), STATUS )
      CALL NDF_BOUND(   RNDF, NDF__MXDIM, RLBND(1,SLOT), RUBND(1,SLOT),
     :   RNDIM(SLOT), STATUS )

*  Validate the slot. This is just a Fortran copy, not an NDF clone.
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      MNDF(SLOT) = A_MNDF

*  Return.
 500  CONTINUE
      END
