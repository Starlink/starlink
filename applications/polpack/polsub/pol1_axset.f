      SUBROUTINE POL1_AXSET( GOTZ, MAP, TR, NX, NY, NZ, CENX, CENY,
     :                       CENZ, STATUS )
*+
*  Name:
*     POL1_AXSET

*  Purpose:
*     Store linear 2 or 3D axis centre values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_AXSET( GOTZ, MAP, TR, NX, NY, NZ, CENX, CENY, CENZ, STATUS )

*  Description:
*     This routine stores linearly increasing values values in 3 or 3
*     1D arrays.

*  Arguments:
*     GOTZ = LOGICAL (Given)
*        If .TRUE. then the data is 3D. Otherwise it is 2D.
*     MAP = INTEGER (Given and Returned)
*        If not AST__NULL, this should be a Mapping from the Frame spanned
*        by the (CEMX,CENY,CENZ) axes to the AXIS Frame, in which case TR is
*        ignored.
*     TR( 6 ) = REAL (Given)
*        The coefficients of the transformation which converts cell indices
*        into (X,Y) values to be stored in the catalogue (if required).
*           X = TR( 1 ) + TR( 2 )*REAL( IPIX )  ( IPIX = 1, NX )
*           Y = TR( 3 ) + TR( 4 )*REAL( IROW )  ( IROW = 1, NY )
*           Z = TR( 5 ) + TR( 6 )*REAL( IZ )    ( IZ = 1, NZ )
*        This is only used if MAP is supplied as AST__NULL.
*     NX = INTEGER (Given)
*        The number of points in CENX.
*     NY = INTEGER (Given)
*        The number of points in CENY.
*     NZ = INTEGER (Given)
*        The number of points in CENZ.
*     CENX( NX ) = REAL (Given and Returned)
*        The X axis centre values. If MAP is not AST__NULL, this array
*        should be supplied holding the X pixel coordinate at each point.
*     CENY( NY ) = REAL (Given and Returned)
*        The Y axis centre values. If MAP is not AST__NULL, this array
*        should be supplied holding the Y pixel coordinate at each point.
*     CENZ( NZ ) = REAL (Given and Returned)
*        The Z axis centre values. If MAP is not AST__NULL, this array
*        should be supplied holding the Z pixel coordinate at each point.
*        Only accessed if GOTZ is .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-APR-1999 (DSB):
*        Original version.
*     7-FEB-2001 (DSB):
*        Modified to support 3D data.
*     7-APR-2003 (DSB):
*        Added MAP argument.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL GOTZ
      INTEGER MAP
      REAL TR( 6 )
      INTEGER NX
      INTEGER NY
      INTEGER NZ

*  Arguments Returned:
      REAL CENX( NX )
      REAL CENY( NY )
      REAL CENZ( NZ )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! loop index
      INTEGER NIN                !
      INTEGER NOUT               !
      INTEGER NEL                !
      INTEGER IPW1               !
      INTEGER IPW2               !
      INTEGER PERM( NDF__MXDIM ) !
      INTEGER PMAP1              !
      INTEGER PMAP2              !
      INTEGER CM1                !
      INTEGER CM2                !
      INTEGER NPOINT             !
      INTEGER IERR               !
      INTEGER NERR               !
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* If no Mapping was supplied, use the linear transformation coefficients.
      IF( MAP .EQ. AST__NULL ) THEN

         DO I = 1, NX
            CENX( I ) = TR( 1 ) + TR( 2 )*REAL( I )
         END DO

         DO I = 1, NY
            CENY( I ) = TR( 3 ) + TR( 4 )*REAL( I )
         END DO

         IF( GOTZ ) THEN
            DO I = 1, NZ
               CENZ( I ) = TR( 5 ) + TR( 6 )*REAL( I )
            END DO
         END IF

* If a Mapping was supplied, use it.
      ELSE

*  Use an AST context so we do not need to annull every AST object
*  explicitly.
         CALL AST_BEGIN( STATUS )

*  Get the no. of NDF pixel axes (NIN) and the number of AXIS axes
*  inherited from the catalogue (NOUT). NOUT may be greater than NIN
*  because the catalogue may have a STOKES axes (for instance). We
*  assume the NIN NDF pixel axes are associated with the lowest NIN
*  catalogue AXIS axes.
         NIN = AST_GETI( MAP, 'NIN', STATUS )
         NOUT = AST_GETI( MAP, 'NOUT', STATUS )

*  Create arrays to hold axis values. Make them big enough to hold
*  any of the pixel axes.
         NEL = MAX( NX, NY )
         IF( GOTZ ) NEL = MAX( NEL, NZ )

         CALL PSX_CALLOC( NEL, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NEL, '_DOUBLE', IPW2, STATUS )

*  Initialise the axis permutation array (zero indicates "assign BAD
*  values to this axis").
         DO I = 1, NDF__MXDIM
            PERM( I ) = 0.0
         END DO

*  Set up values for each of the supplied AXIS arrays.
         DO I = 1, NIN

*  Create PermMaps which selects the current input and output.
            PERM( I ) = 1

            PMAP1 = AST_PERMMAP( 1, I, NIN, PERM, 0.0D0, ' ', STATUS )
            PMAP2 = AST_PERMMAP( NOUT, PERM, 1, I, 0.0D0, ' ', STATUS )

*  Concatentate the Mappings to produce a 1-input 1-output CmpMap.
            CM1 = AST_CMPMAP( PMAP1, MAP, .TRUE., ' ', STATUS )
            CM2 = AST_CMPMAP( CM1, PMAP2, .TRUE., ' ', STATUS )

*  Copy the supplied PIXEL axis values into the first work array.
            IF( I .EQ. 1 ) THEN
               NPOINT = NX
               CALL VEC_RTOD( .TRUE., NX, CENX,
     :                        %VAL( CNF_PVAL( IPW1 ) ), IERR,
     :                        NERR, STATUS )

            ELSE IF( I .EQ. 2 ) THEN
               NPOINT = NY
               CALL VEC_RTOD( .TRUE., NY, CENY,
     :                        %VAL( CNF_PVAL( IPW1 ) ), IERR,
     :                        NERR, STATUS )

            ELSE
               NPOINT = NZ
               CALL VEC_RTOD( .TRUE., NZ, CENZ,
     :                        %VAL( CNF_PVAL( IPW1 ) ), IERR,
     :                        NERR, STATUS )
            END IF

*  Transform them into the AXIS Frame.
            CALL AST_TRAN1( CM2, NPOINT, %VAL( CNF_PVAL( IPW1 ) ),
     :                      .TRUE.,
     :                      %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Copy them back into the supplied arrays.
            IF( I .EQ. 1 ) THEN
               CALL VEC_DTOR( .TRUE., NX, %VAL( CNF_PVAL( IPW2 ) ),
     :                        CENX, IERR,
     :                        NERR, STATUS )

            ELSE IF( I .EQ. 2 ) THEN
               CALL VEC_DTOR( .TRUE., NY, %VAL( CNF_PVAL( IPW2 ) ),
     :                        CENY, IERR,
     :                        NERR, STATUS )

            ELSE
               CALL VEC_DTOR( .TRUE., NZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                        CENZ, IERR,
     :                        NERR, STATUS )

            END IF

*  Reset the permutation for this axis.
            PERM( I ) = 0
         END DO

*  Free workspace.
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPW2, STATUS )

*  End the AST context, thus annulling all AST objects created above.
         CALL AST_END( STATUS )

      END IF

      END
