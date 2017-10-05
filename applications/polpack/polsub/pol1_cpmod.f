      SUBROUTINE POL1_CPMOD( CI, TWCS, XID, YID, QID, UID, ANGID, DQID,
     :                       DUID, DANGID, NROW, TABLE, STATUS )
*+
*  Name:
*     POL1_CPMOD

*  Purpose:
*     Modify the WCS and reference direction of a supplied catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPMOD( CI, TWCS, XID, YID, QID, UID, ANGID, DQID,
*                      DUID, DANGID, NROW, TABLE, STATUS )

*  Description:
*     The polpack column values that are affected by WCS and reference
*     direction are copied from the supplied catalogue, modified to refer
*     to a new WCS and reference direction, and stored in the supplied
*     table.

*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the input catalogue.
*     TWCS =  INTEGER (Given)
*        The WCS FrameSet that defines the required WCS and reference
*        direction.
*     XID =  INTEGER (Given)
*        Identifier for the X column within the input catalogue.
*     YID =  INTEGER (Given)
*        Identifier for the Y column within the input catalogue.
*     QID =  INTEGER (Given)
*        Identifier for the Q column within the input catalogue.
*     UID =  INTEGER (Given)
*        Identifier for the U column within the input catalogue.
*     ANGID =  INTEGER (Given)
*        Identifier for the ANG column within the input catalogue.
*     DQID =  INTEGER (Given)
*        Identifier for the DQ column within the input catalogue.
*     DUID =  INTEGER (Given)
*        Identifier for the DU column within the input catalogue.
*     DANGID =  INTEGER (Given)
*        Identifier for the DANG column within the input catalogue.
*     NROW =  INTEGER (Given)
*        The number of rows in the input catalogue.
*     TABLE( NROW, 8 ) =  REAL (Returned)
*        The table returned holding the modified values. Each column
*        contains the values from a single catalogue row, in the order X,
*        Y, Q, DQ, U, DU, ANG, DANG (thus each row in the returned table
*        corresponds to a column of the catalogue).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The error column identifiers (DQID, DUID and DANGID) may be
*     supplied as CAT__NOID if no errors are available, in which case the
*     corresponding columns in TABLE will be returned holding VAL__BADR.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2017 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing



*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants
      INCLUDE 'AST_PAR'     ! AST symbolic constants
      INCLUDE 'CNF_PAR'     ! CNF functions
      INCLUDE 'PRM_PAR'     ! VAL__ constants

*  External References:
      INTEGER KPG1_CEIL

*  Arguments Given:
      INTEGER CI
      INTEGER TWCS
      INTEGER XID
      INTEGER YID
      INTEGER QID
      INTEGER UID
      INTEGER ANGID
      INTEGER DQID
      INTEGER DUID
      INTEGER DANGID
      INTEGER NROW

*  Arguments Returned:
      REAL TABLE( NROW, 8 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION SHIFT( 2 )
      INTEGER FRM
      INTEGER GI( 8 )
      INTEGER ICUR
      INTEGER IPQ
      INTEGER IPQV
      INTEGER IPU
      INTEGER IPUV
      INTEGER IPXIN
      INTEGER IPXOUT
      INTEGER IPYIN
      INTEGER IPYOUT
      INTEGER IWCS
      INTEGER IXHI
      INTEGER IXLO
      INTEGER IYHI
      INTEGER IYLO
      INTEGER J
      INTEGER MAP
      INTEGER MAXPOS
      INTEGER MINPOS
      INTEGER NBAD
      INTEGER NX
      INTEGER NY
      INTEGER PMAP
      INTEGER TMAP
      INTEGER TWCSC
      LOGICAL VAR
      REAL ANG
      REAL IP2
      REAL Q
      REAL RTOD
      REAL SXHI
      REAL SXLO
      REAL SYHI
      REAL SYLO
      REAL U
      REAL VANG
      REAL VQ
      REAL VU
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Set up the conversion factor from radians to degrees.
      RTOD = 180.0 / ACOS( -1.0 )

*  Are variances being processed?
      VAR = ( DQID .NE. CAT__NOID .AND.
     :        DUID .NE. CAT__NOID .AND.
     :        DANGID .NE. CAT__NOID )

*  Get the WCS FrameSet from the input catalogue. BASE Frame will be
*  PIXEL coords, and it will contain a POLANAL Frame. No GRID Frame will
*  be present.
      CALL POL1_GTCTW( CI, IWCS, STATUS )
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'POL1_CPMOD: No WCS information found in '//
     :                 'input catalogue (programming error)".', STATUS )
      END IF

*  Read the required columns from the catalogue into the supplied array.
      GI( 1 ) = XID
      GI( 2 ) = YID
      GI( 3 ) = QID
      GI( 4 ) = DQID
      GI( 5 ) = UID
      GI( 6 ) = DUID
      GI( 7 ) = ANGID
      GI( 8 ) = DANGID
      CALL POL1_CTCLM( CI, NROW, 8, GI, TABLE, STATUS )

*  Find the upper and lower bounds on the X and Y columns (pixel
*  coordinates).
      CALL KPG1_MXMNR( .TRUE., NROW, TABLE( 1, 1 ), NBAD, SXHI,
     :                 SXLO, MAXPOS, MINPOS, STATUS )
      CALL KPG1_MXMNR( .TRUE., NROW, TABLE( 1, 2 ), NBAD, SYHI,
     :                 SYLO, MAXPOS, MINPOS, STATUS )

*  Use the edges of the corresponding pixel as the upper and lower
*  bounds, and convert to integer pixel indices.
      IXHI = KPG1_CEIL( SXHI )
      IXLO = KPG1_CEIL( SXLO ) - 1
      IYHI = KPG1_CEIL( SYHI )
      IYLO = KPG1_CEIL( SYLO ) - 1

*  Find the number of pixels along each axis.
      NX = IXHI - IXLO + 1
      NY = IYHI - IYLO + 1

*  Allocate arrays to hold maps of the required quantities, and copy
*  the catalogue values from the table into these maps, squaring the
*  sigma values to get variances.
      CALL PSX_CALLOC( NX*NY, '_DOUBLE', IPQ, STATUS )
      CALL PSX_CALLOC( NX*NY, '_DOUBLE', IPU, STATUS )
      CALL POL1_CPMD1( NROW, TABLE, 3, IXLO, IYLO, NX, NY, .FALSE.,
     :                 %VAL( CNF_PVAL( IPQ ) ), STATUS )
      CALL POL1_CPMD1( NROW, TABLE, 5, IXLO, IYLO, NX, NY, .FALSE.,
     :                 %VAL( CNF_PVAL( IPU ) ), STATUS )

      IF( VAR ) THEN
         CALL PSX_CALLOC( NX*NY, '_DOUBLE', IPQV, STATUS )
         CALL PSX_CALLOC( NX*NY, '_DOUBLE', IPUV, STATUS )
         CALL POL1_CPMD1( NROW, TABLE, 4, IXLO, IYLO, NX, NY, .TRUE.,
     :                    %VAL( CNF_PVAL( IPQV ) ), STATUS )
         CALL POL1_CPMD1( NROW, TABLE, 6, IXLO, IYLO, NX, NY, .TRUE.,
     :                    %VAL( CNF_PVAL( IPUV ) ), STATUS )
      ELSE
         IPQV = IPQ
         IPUV = IPU
      END IF

*  Modify the IWCS FrameSet so that it includes a GRID Frame describing
*  these maps. Make this new Frame the Base Frame, retaining the
*  original Current Frame.
      ICUR = AST_GETI( IWCS, 'Current', STATUS )
      SHIFT( 1 ) = IXLO - 1.0D0
      SHIFT( 2 ) = IYLO - 1.0D0
      PMAP = AST_SHIFTMAP( 2, SHIFT, ' ', STATUS )
      FRM = AST_FRAME( 2, 'Domain=GRID', STATUS )
      CALL AST_ADDFRAME( IWCS, AST__BASE, PMAP, FRM, STATUS )
      CALL AST_SETI( IWCS, 'Base', AST_GETI( IWCS, 'Current', STATUS ),
     :               STATUS )
      CALL AST_SETI( IWCS, 'Current', ICUR, STATUS )

*  Take a deep copy of TWCS so that the original does not get modified by
*  POL1_ROTRF.
      TWCSC = AST_COPY( TWCS, STATUS )

*  Modify the Q and U maps (in-situ) so that they refer to the new
*  reference direction. This also returns the Mapping from the base Frame
*  of IWCS (GRID) to the base Frame of TWCSC (PIXEL).
      CALL POL1_ROTRF( NY, NX, IWCS, TWCSC, 0, 0, VAR,
     :                 %VAL( CNF_PVAL( IPQ ) ),
     :                 %VAL( CNF_PVAL( IPU ) ),
     :                 %VAL( CNF_PVAL( IPQ ) ),
     :                 %VAL( CNF_PVAL( IPU ) ),
     :                 %VAL( CNF_PVAL( IPQV ) ),
     :                 %VAL( CNF_PVAL( IPUV ) ),
     :                 %VAL( CNF_PVAL( IPQV ) ),
     :                 %VAL( CNF_PVAL( IPUV ) ), MAP,
     :                 STATUS )

*  Copy the modified values back to the returned table.
      CALL POL1_CPMD2( IXLO, IYLO, NX, NY, %VAL( CNF_PVAL( IPQ ) ),
     :                 .FALSE., NROW, 3, TABLE, STATUS )
      CALL POL1_CPMD2( IXLO, IYLO, NX, NY, %VAL( CNF_PVAL( IPU ) ),
     :                 .FALSE., NROW, 5, TABLE, STATUS )

      IF( VAR ) THEN
         CALL POL1_CPMD2( IXLO, IYLO, NX, NY, %VAL( CNF_PVAL( IPQV ) ),
     :                    .TRUE., NROW, 4, TABLE, STATUS )
         CALL POL1_CPMD2( IXLO, IYLO, NX, NY, %VAL( CNF_PVAL( IPUV ) ),
     :                    .TRUE., NROW, 6, TABLE, STATUS )
      END IF

*  Calculate new ANG and DANG values, based on the modified Q and U values.
      DO J = 1, NROW
         Q = TABLE( J, 3 )
         U = TABLE( J, 5 )

         IF( Q .NE. VAL__BADR .AND. U .NE. VAL__BADR ) THEN
            TABLE( J, 7 ) = RTOD * 0.5 * ATAN2( U, Q )

            IF( VAR ) THEN
               VQ = TABLE( J, 4 )
               VU = TABLE( J, 6 )
               IF( VQ .NE. VAL__BADR .AND. VU .NE. VAL__BADR ) THEN
                  VQ = VQ*VQ
                  VU = VU*VU
                  IP2 = Q*Q + U*U
                  VANG = RTOD*RTOD*( Q*Q*VU + U*U*VQ )/( 4.0*IP2*IP2 )
                  TABLE( J, 8 ) = SQRT( VANG )
               ELSE
                  TABLE( J, 8 ) = VAL__BADR
               END IF
            ELSE
               TABLE( J, 8 ) = VAL__BADR
            END IF

         ELSE
            TABLE( J, 7 ) = VAL__BADR
            TABLE( J, 8 ) = VAL__BADR
         END IF

      END DO

*  Free resources.
      IF( IPQV .NE. IPQ ) CALL PSX_FREE( IPQV, STATUS )
      IF( IPUV .NE. IPU ) CALL PSX_FREE( IPUV, STATUS )
      CALL PSX_FREE( IPQ, STATUS )
      CALL PSX_FREE( IPU, STATUS )

*  Copy the (X,Y) values into double precision workspace so that they can
*  be transformed using AST.
      CALL PSX_CALLOC( NROW, '_DOUBLE', IPXIN, STATUS )
      CALL PSX_CALLOC( NROW, '_DOUBLE', IPYIN, STATUS )
      CALL PSX_CALLOC( NROW, '_DOUBLE', IPXOUT, STATUS )
      CALL PSX_CALLOC( NROW, '_DOUBLE', IPYOUT, STATUS )

      CALL POL1_CPMD3( NROW, TABLE, 1, %VAL( CNF_PVAL(IPXIN) ), STATUS )
      CALL POL1_CPMD3( NROW, TABLE, 2, %VAL( CNF_PVAL(IPYIN) ), STATUS )

*  Modify the X and Y positions (PIXEL coords) so that they refer to the
*  PIXEL Frame in TWCS. First get the Mapping from PIXEL in the input
*  catalogue (IWCS) to PIXEL in the output catalogue (TWCS). Then use the
*  Mapping to transform the (X,Y) values.
      TMAP = AST_CMPMAP( PMAP, MAP, .TRUE., ' ', STATUS )
      CALL AST_TRAN2( TMAP, NROW, %VAL( CNF_PVAL( IPXIN ) ),
     :                %VAL( CNF_PVAL( IPYIN ) ), .TRUE.,
     :                %VAL( CNF_PVAL( IPXOUT ) ),
     :                %VAL( CNF_PVAL( IPYOUT ) ), STATUS )

*  Copy the modified values back to the returned table.
      CALL POL1_CPMD4( NROW, %VAL( CNF_PVAL(IPXIN) ), TABLE, 1, STATUS )
      CALL POL1_CPMD4( NROW, %VAL( CNF_PVAL(IPYIN) ), TABLE, 2, STATUS )

*  Free resources.
      CALL PSX_FREE( IPXIN, STATUS )
      CALL PSX_FREE( IPYIN, STATUS )
      CALL PSX_FREE( IPXOUT, STATUS )
      CALL PSX_FREE( IPYOUT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END















