      SUBROUTINE NDF1_RDWCS( IACB, IWCS, STATUS )
*+
* Name:
*    NDF1_RDWCS

*  Purpose:
*     Read WCS information from an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_RDWCS( IACB, IWCS, STATUS )

*  Description:
*     The routine returns a pointer to an AST_ FrameSet which contains
*     WCS information for an NDF with an entry in the ACB. If the NDF's
*     WCS component is undefined, default WCS information is provided.
*
*     Account is taken of NDF sections and appropriate adjustments are
*     made to the WCS information returned.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index of the NDF entry in the ACB.
*     IWCS = INTEGER (Returned)
*        Pointer to a new AST_ FrameSet containing the WCS information.
*        This FrameSet will be derived from a deep copy of the
*        internally-stored information, so may be modified without
*        affecting the subsequent behaviour of the NDF_ library.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    - A value of AST__NULL will be returned for the IWCS argument if
*    this routine is called with STATUS set, or if it should fail for
*    any reason.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1997 (RFWS):
*        Original version.
*     14-JAN-1998 (RFWS):
*        Sorted out how to handle pixel-index shifts when the number of
*        dimensions changes.
*     4-AUG-2009 (DSB):
*        Add FRACTION Frame.
*     21-MAY-2015 (DSB):
*        Set LutEpsilon attribute in the AXIS Frame LutMaps.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Read)
*           Pointer to DCB world coordinate system information.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_ADMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped axis data array.
*        ACB_ADMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric type used to map axis data arrays.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      INTEGER IWCS

*  Local Constants:
      INTEGER NSTD               ! No. standard NDF coordinate systems
      PARAMETER ( NSTD = 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) AXTYPE ! Axis data array numeric type
      DOUBLE PRECISION ACA( NDF__MXDIM ) ! Axis coordinate, 1st point
      DOUBLE PRECISION ACB( NDF__MXDIM ) ! Axis coordinate, 2nd point
      DOUBLE PRECISION CONST( NDF__MXDIM ) ! Constants array for PermMap
      DOUBLE PRECISION EPS       ! Value for LutEpsilon attribute
      DOUBLE PRECISION IAA( NDF__MXDIM ) ! Index in ACB array, 1st point
      DOUBLE PRECISION IAB( NDF__MXDIM ) ! Index in ACB array, 2nd point
      DOUBLE PRECISION IDA( NDF__MXDIM ) ! Index in DCB array, 1st point
      DOUBLE PRECISION IDB( NDF__MXDIM ) ! Index in DCB array, 2nd point
      DOUBLE PRECISION MAXV      ! Max value in AXIS data array
      DOUBLE PRECISION MEAN      ! Mean value in AXIS data array
      DOUBLE PRECISION MINV      ! Min value in AXIS data array
      DOUBLE PRECISION NPCA( NDF__MXDIM ) ! Normalised Pix. coord., 1st point
      DOUBLE PRECISION NPCB( NDF__MXDIM ) ! Normalised Pix. coord., 2nd point
      DOUBLE PRECISION PCA( NDF__MXDIM ) ! Pixel coordinate, 1st point
      DOUBLE PRECISION PCB( NDF__MXDIM ) ! Pixel coordinate, 2nd point
      DOUBLE PRECISION RMS       ! RMS value in AXIS data array
      DOUBLE PRECISION SIGMA     ! Standard deviation of values in AXIS data array
      INTEGER AXMAP              ! Mapping pointer for NDF axis
      INTEGER CMPMAP             ! Base GRID to section FRACTION Mapping
      INTEGER EL                 ! Number of mapped values
      INTEGER FRAME              ! Pointer to Frame
      INTEGER I                  ! Loop index
      INTEGER IACBT              ! Index of temporary ACB entry
      INTEGER IARY               ! ID of temporary array
      INTEGER IBASE              ! Index of base Frame
      INTEGER ICURR              ! Index of current Frame
      INTEGER IDCB               ! Index of NDF entry in the DCB
      INTEGER IDIM               ! Loop counter for NDF dimensions
      INTEGER IERR               ! Index of conversion error (junk)
      INTEGER IFRAME             ! Loop counter for Frame indices
      INTEGER LBNDA( NDF__MXDIM ) ! Lower pixel index bound (ACB entry)
      INTEGER LBNDD( NDF__MXDIM ) ! Lower pixel index bound (DCB entry)
      INTEGER MAP                ! Pointer to Mapping
      INTEGER MAP0               ! Pointer to base->section GRID Mapping
      INTEGER NCONST             ! Number of PermMap constants
      INTEGER NDIMA              ! No. NDF dimensions (ACB entry)
      INTEGER NDIMD              ! No. NDF dimensions (DCB entry)
      INTEGER NERR               ! No. data conversion errors (junk)
      INTEGER NEW                ! Pointer to new FrameSet
      INTEGER NGOOD              ! Number of good values in AXIS data array
      INTEGER PERMA( NDF__MXDIM ) ! Permutation array for ACB axes
      INTEGER PERMD( NDF__MXDIM ) ! Permutation array for DCB axes
      INTEGER PLACE              ! ARY_ placeholder
      INTEGER PNTR               ! Pointer to mapped values
      INTEGER SHIFT( NDF__MXDIM ) ! NDF pixel-index shifts
      INTEGER TMPMAP             ! Pointer to temporary Mapping
      INTEGER UBNDA( NDF__MXDIM ) ! Upper pixel index bound (ACB entry)
      INTEGER UBNDD( NDF__MXDIM ) ! Upper pixel index bound (DCB entry)
      INTEGER UNIT               ! Pointer to UnitMap
      LOGICAL AXSTAT             ! NDF axis component present?
      LOGICAL DCE                ! Data conversion errors?
      LOGICAL MAPPED             ! Axis data mapped?

*.

*  Initialise the returned AST_ pointer.
      IWCS = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the index of the data object in the DCB and ensure that AXIS
*  and WCS information is available for it.
      IDCB = ACB_IDCB( IACB )
      CALL NDF1_DA( IDCB, STATUS )
      CALL NDF1_DW( IDCB, STATUS )

*  Obtain the pixel-index bounds of the NDF entry in the ACB and of the
*  data object in the DCB.
      CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBNDA, UBNDA, NDIMA,
     :                STATUS )
      CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBNDD, UBNDD, NDIMD,
     :                STATUS )

*  Obtain the offsets between the pixel indices in these two NDFs.
      CALL ARY_OFFS( DCB_DID( IDCB ), ACB_DID( IACB ), NDF__MXDIM,
     :               SHIFT, STATUS )

*  Determine if the NDF's AXIS component is in a defined state.
      IF ( STATUS .EQ. SAI__OK ) THEN
         AXSTAT = DCB_ALOC( 1, IDCB ) .NE. DAT__NOLOC

*  Obtain raw WCS data.
*  --------------------
*  If WCS information is available in the DCB, then make a copy of the
*  data object's WCS FrameSet.
         IF ( DCB_IWCS( IDCB ) .NE. AST__NULL ) THEN
            IWCS = AST_COPY( DCB_IWCS( IDCB ), STATUS )

*  Otherwise, we must create a default FrameSet to represent the
*  standard NDF coordinate systems. We start by simply adding the
*  necessary Frames to a dummy FrameSet, inter-relating the Frames with
*  null Mappings (UnitMaps).
         ELSE

*  Start by adding a Frame to represent the data grid coordinate
*  system, in which the first NDF pixel is centred at (1,1). */
            FRAME = AST_FRAME( NDIMD, 'Domain=GRID', STATUS )
            IWCS = AST_FRAMESET( FRAME, ' ', STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Add a second Frame to represent pixel coordinates, which take
*  account of the NDF's lower pixel-index bounds. For now, use a
*  UnitMap to relate this to the first Frame.
            UNIT = AST_UNITMAP( NDIMD, ' ', STATUS )
            FRAME = AST_FRAME( NDIMD, 'Domain=PIXEL', STATUS )
            CALL AST_ADDFRAME( IWCS, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Add a third Frame to represent axis coordinates, which take account
*  of data in the NDF's AXIS component. Again, use a UnitMap.
            FRAME = AST_FRAME( NDIMD, 'Domain=AXIS', STATUS )
            CALL AST_ADDFRAME( IWCS, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Add a fourth Frame to represent normalised pixel coordinates (each
*  pixel axis spans a range 0.0 to 1.0 in this Frame). Again, use a
*  UnitMap.
            FRAME = AST_FRAME( NDIMD, 'Domain=FRACTION', STATUS )
            CALL AST_ADDFRAME( IWCS, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Annul the UnitMap pointer.
            CALL AST_ANNUL( UNIT, STATUS )

*  The FRACTION Frame will now be current in our dummy FrameSet. If the
*  NDF's AXIS component is defined, change this to make the AXIS
*  coordinate Frame current instead. Otherwise, change it to make the
*  pixel coordinate Frame current.
            IF ( .NOT. AXSTAT ) THEN
               CALL AST_SETI( IWCS, 'Current', 2, STATUS )
            ELSE
               CALL AST_SETI( IWCS, 'Current', 3, STATUS )
            END IF
         END IF

*  Change dimensionality.
*  ----------------------
*  If the ACB entry's dimensionality differs from that of the data
*  object in the DCB, then we must modify the FrameSet to allow for
*  this.
         IF ( NDIMA .NE. NDIMD ) THEN

*  Create a new FrameSet containing just the new base Frame (with the
*  new number of dimensions).
            FRAME = AST_FRAME( NDIMA, 'Domain=GRID', STATUS )
            NEW = AST_FRAMESET( FRAME, ' ', STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Add a Frame to represent pixel coordinates, related to the base Frame
*  by a UnitMap.
            UNIT = AST_UNITMAP( NDIMA, ' ', STATUS )
            FRAME = AST_FRAME( NDIMA, 'Domain=PIXEL', STATUS )
            CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Similarly, add a Frame to represent axis coordinates.
            FRAME = AST_FRAME( NDIMA, 'Domain=AXIS', STATUS )
            CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Similarly, add a Frame to represent normalised pixel coordinates.
            FRAME = AST_FRAME( NDIMA, 'Domain=FRACTION', STATUS )
            CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Annul the UnitMap pointer.
            CALL AST_ANNUL( UNIT, STATUS )

*  We will now set up a PermMap which relates DCB coordinates to ACB
*  coordinates. Loop through each relevant dimension to set up the
*  permutation arrays required.
            NCONST = 0
            DO 1 IDIM = 1, MAX( NDIMD, NDIMA )

*  Identify DCB dimensions with corresponding dimensions in the ACB, if
*  they exist.
               IF ( IDIM .LE. NDIMD ) THEN
                  IF ( IDIM .LE. NDIMA ) THEN
                     PERMD( IDIM ) = IDIM

*  Otherwise, flag the dimension so it receives a constant coordinate
*  value. Set this value equal to 1, less any pixel-index shift which
*  may have been applied in this dimension (this corresponds with the
*  standard process of extending bounds by adding new dimensions with
*  the value 1, to match dimensionalities).
                  ELSE
                     NCONST = NCONST + 1
                     CONST( NCONST ) = DBLE( 1 - SHIFT( IDIM ) )
                     PERMD( IDIM ) = -NCONST
                  END IF
               END IF

*  Repeat the process to identify ACB dimensions with corresponding DCB
*  dimensions. In this case, the constant coordinate value used is
*  always 1, because any additional pixel-index shifts are accommodated
*  by the "ACB offset" and "pixel coordinate system" Mappings set up
*  later (see below).
               IF ( IDIM .LE. NDIMA ) THEN
                  IF ( IDIM .LE. NDIMD ) THEN
                     PERMA( IDIM ) = IDIM
                  ELSE
                     NCONST = NCONST + 1
                     CONST( NCONST ) = DBLE( 1 )
                     PERMA( IDIM ) = -NCONST
                  END IF
               END IF
 1          CONTINUE

*  Create a PermMap to convert between the two dimensionalities.
            MAP = AST_PERMMAP( NDIMA, PERMA, NDIMD, PERMD, CONST,
     :                         ' ', STATUS )

*  Obtain the indices of the base and current Frames in the original
*  FrameSet.
            IBASE = AST_GETI( IWCS, 'Base', STATUS )
            ICURR = AST_GETI( IWCS, 'Current', STATUS )

*  Make the base Frame current and add the original FrameSet to the new
*  one we have just created. Their base Frames (i.e. data grid indices)
*  are inter-related by the PermMap created above.
            CALL AST_SETI( IWCS, 'Current', IBASE, STATUS )
            CALL AST_ADDFRAME( NEW, AST__BASE, MAP, IWCS, STATUS )

*  Annul the PermMap pointer and the original FrameSet pointer. Replace
*  the latter with a pointer to the new FrameSet.
            CALL AST_ANNUL( MAP, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
            IWCS = NEW

*  Remove the standard Frames from the original IWCS FrameSet (allowing
*  for their new indices in the new FrameSet), as these have now been
*  replaced.
            DO I = 1, NSTD
               CALL AST_REMOVEFRAME( IWCS, IBASE + NSTD, STATUS )
            END DO

*  Re-select the original current Frame index (which may now correspond
*  to one of the new Frames).
            CALL AST_SETI( IWCS, 'Current', ICURR, STATUS )
         END IF

*  Correct for ACB offsets.
*  ------------------------
*  The base Frame now corresponds to the data grid indices in the base
*  NDF (from whose DCB entry we obtained the WCS information). However,
*  the set of pixels accessed via the ACB entry may be offset from
*  this, so might have a different data grid coordinate system. To
*  correct for this, we remap the base Frame.

*  Set up a Mapping which converts from grid indices in the base NDF
*  (first pixel at 1,1) to grid indices in the NDF section (first pixel
*  also at 1,1, but corresponding to a different base NDF pixel). In
*  doing this, allow for any pixel-index shifts that may have been
*  applied, as these will cause the section's pixel index bounds to be
*  offset.
         DO 2 IDIM = 1, NDIMA
            IDA( IDIM ) = 0.5D0
            IDB( IDIM ) = 1.5D0
            IAA( IDIM ) = DBLE( LBNDD( IDIM ) - LBNDA( IDIM ) +
     :                          SHIFT( IDIM ) ) + 0.5D0
            IAB( IDIM ) = DBLE( LBNDD( IDIM ) - LBNDA( IDIM ) +
     :                          SHIFT( IDIM ) ) + 1.5D0
 2       CONTINUE
         MAP0 = AST_WINMAP( NDIMA, IDA, IDB, IAA, IAB, ' ', STATUS )

*  Remap the base Frame to reflect the change of data grid origin (so
*  that all other coordinate systems described by the FrameSet remain
*  attached to the same actual data pixels).
         CALL AST_REMAPFRAME( IWCS, AST__BASE, MAP0, STATUS )

*  Set up pixel coordinate system.
*  -------------------------------
*  Set up a Mapping which converts from grid indices in the base NDF
*  (the coordinate system to which the pixel coordinate Frame is still
*  attached) and pixel coordinates corresponding with the ACB entry.
         DO 3 IDIM = 1, NDIMA
            IDA( IDIM ) = 0.5D0
            IDB( IDIM ) = 1.5D0
            PCA( IDIM ) = DBLE( LBNDD( IDIM ) + SHIFT( IDIM ) ) - 1.0D0
            PCB( IDIM ) = DBLE( LBNDD( IDIM ) + SHIFT( IDIM ) )
 3       CONTINUE

*  Use this Mapping to remap the second Frame to define the pixel
*  coordinate system.
         MAP = AST_WINMAP( NDIMA, IDA, IDB, PCA, PCB, ' ', STATUS )
         CALL AST_REMAPFRAME( IWCS, 2, MAP, STATUS )

*  Set up axis coordinate system.
*  ------------------------------
*  If the NDF's AXIS component is in an undefined state, then remap the
*  Frame representing the axis coordinate system using the same Mapping
*  as above. This makes the default axis coordinate system identical to
*  the pixel coordinate system.
         IF ( .NOT. AXSTAT ) THEN
            CALL AST_REMAPFRAME( IWCS, 3, MAP, STATUS )
         END IF

*  Annul the Mapping used.
         CALL AST_ANNUL( MAP, STATUS )

*  If the AXIS component is defined, then we must obtain access to the
*  axis data arrays in order to set up this coordinate system.
         IF ( AXSTAT ) THEN

*  If the ACB entry describes an NDF section, create a temporary ACB
*  entry to describe the base NDF, through which we will access its
*  axis data. This is necessary to prevent any truncation or
*  extrapolation of the axis data occurring.
            IF ( ACB_CUT( IACB ) ) THEN
               CALL NDF1_CRNBN( IDCB, IACBT, STATUS )
            END IF

*  Loop to access the axis centre array for each ACB axis.
            DO 4 IDIM = 1, NDIMA

*  If the NDF is a section, map the required axis data array for
*  reading as double precision values via the temporary ACB entry.
               IF ( ACB_CUT( IACB ) ) THEN
                  CALL NDF1_ADMAP( IDIM, IACBT, '_DOUBLE', 'READ',
     :                             PNTR, EL, STATUS )

*  If it is not a section, check if the required axis data array is
*  already mapped for access. If so, then the currently mapped values
*  will be used, but a double precision copy of them must be made.
               ELSE IF ( ACB_ADMAP( IDIM, IACB ) ) THEN
                  MAPPED = .TRUE.

*  Create and map a temporary ARY_ array to provide workspace for the
*  copy.
                  CALL ARY_TEMP( PLACE, STATUS )
                  CALL ARY_NEW( '_DOUBLE', 1, LBNDD( IDIM ),
     :                          UBNDD( IDIM ), PLACE, IARY, STATUS )
                  CALL ARY_MAP( IARY, '_DOUBLE', 'WRITE', PNTR, EL,
     :                          STATUS )

*  Convert the mapped values to double precision.
                  CALL NDF1_CVTD( .TRUE., EL, ACB_ADMTP( IDIM, IACB ),
     :                            ACB_ADMPT( IDIM, IACB ),
     :                            %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                            STATUS )

*  If the axis data array is not already mapped, then note this fact
*  and map it in the required manner.
               ELSE
                  MAPPED = .FALSE.
                  CALL NDF1_ADMAP( IDIM, IACB, '_DOUBLE', 'READ',
     :                             PNTR, EL, STATUS )
               END IF

*  If 2 or more axis centre values have been mapped, create a LutMap
*  containing these values as lookup table entries. This LutMap
*  converts from the base NDF's data grid coordinate system (to which
*  the AXIS coordinate Frame is still attached) to the axis coordinate
*  system along the current axis.
               IF ( EL .GT. 1 ) THEN
                  AXMAP = AST_LUTMAP( EL, %VAL( CNF_PVAL( PNTR ) ),
     :                                1.0D0, 1.0D0, ' ', STATUS )

*  Set an appropriate value for the LutEpsilon attribute (the relative
*  error of the values in the table), based on the data type of the AXIS
*  structure.
                  CALL NDF1_ADTYP( IDIM, IACB, AXTYPE, STATUS )
                  IF( AXTYPE .EQ. '_DOUBLE' ) THEN
                     EPS = VAL__EPSD
                  ELSE IF( AXTYPE .EQ. '_REAL' ) THEN
                     EPS = VAL__EPSR

*  For integer type data, calculate a relative error using an absolute
*  error of 1.0 and the RMS data value in the array.
                  ELSE
                     CALL NDF1_STATS( EL, %VAL( CNF_PVAL( PNTR ) ),
     :                                MAXV, MINV, MEAN, SIGMA, RMS,
     :                                NGOOD, STATUS )
                     IF( RMS .GT. 0.0D0 .AND. RMS .NE. VAL__BADD ) THEN
                        EPS = 1.0/RMS
                     ELSE
                        EPS = 1.0D0
                     END IF
                  END IF

*  Set the relative error of the LutMap.
                  CALL AST_SETD( AXMAP, 'LutEpsilon', EPS, STATUS )

*  If only one value is available (the size of this NDF dimension is
*  only 1 pixel), then copy the mapped value to a double precision
*  array so that its value can be accessed.
               ELSE
                  CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( PNTR ) ),
     :                           ACA, IERR, NERR, STATUS )

*  Use this value to set up a linear Mapping (for this dimension only)
*  that converts from the base NDF's grid index (1) to the required
*  axis centre value, with an increment of unity when extrapolating
*  outside the NDF along this dimension.
                  ACB( 1 ) = ACA( 1 ) + 1.0D0
                  IDA( 1 ) = 1.0D0
                  IDB( 1 ) = 2.0D0
                  AXMAP = AST_WINMAP( 1, IDA, IDB, ACA, ACB, ' ',
     :                                STATUS )
               END IF

*  Now relinquish access to the axis data array. If it was mapped via
*  the temporary ACB entry, then unmap it.
               IF ( ACB_CUT( IACB ) ) THEN
                  CALL NDF1_ADUMP( IDIM, IACBT, STATUS )

*  If access was to a temporary copy of the array, then annul the
*  identifier for the temporary copy. Otherwise, simply unmap the array
*  via the current ACB entry.
               ELSE IF ( MAPPED ) THEN
                  CALL ARY_ANNUL( IARY, STATUS )
               ELSE
                  CALL NDF1_ADUMP( IDIM, IACB, STATUS )
               END IF

*  For the first NDF dimension, use the Mapping produced above directly,
*  by cloning its pointer.
               IF ( IDIM .EQ. 1 ) THEN
                  MAP = AST_CLONE( AXMAP, STATUS )

*  For subsequent dimensions, accumulate the Mappings by combining them
*  in parallel in a CmpMap. Annul the previous accumulated Mapping
*  pointer on each occasion and replace it with the new one.
               ELSE
                  TMPMAP = AST_CMPMAP( MAP, AXMAP, .FALSE., ' ',
     :                                 STATUS )
                  CALL AST_ANNUL( MAP, STATUS )
                  MAP = TMPMAP
               END IF

*  Annul the Mapping pointer for the current NDF dimension.
               CALL AST_ANNUL( AXMAP, STATUS )
 4          CONTINUE

*  If a temporary ACB entry was created, then annul it.
            IF ( ACB_CUT( IACB ) ) THEN
               CALL NDF1_ANL( IACBT, STATUS )
            END IF

*  Remap the Frame representing the axis coordinate system using the
*  Mapping generated above. Then annul the Mapping pointer.
            CALL AST_REMAPFRAME( IWCS, 3, MAP, STATUS )
            CALL AST_ANNUL( MAP, STATUS )
         END IF

*  Set up normalised pixel coordinate system.
*  ------------------------------------------
*  Set up a Mapping which converts from grid indices in the section (note
*  section, not base) NDF and pixel coordinates corresponding with the ACB
*  entry.
         DO 5 IDIM = 1, NDIMA
            IAA( IDIM ) = 0.5D0
            IAB( IDIM ) = DBLE( UBNDA( IDIM ) - LBNDA( IDIM ) ) + 1.5D0
            NPCA( IDIM ) = 0.0D0
            NPCB( IDIM ) = 1.0D0
 5       CONTINUE
         MAP = AST_WINMAP( NDIMA, IAA, IAB, NPCA, NPCB, ' ', STATUS )

*  In order to remap the FRACTION frame, we need the mapping from the
*  base NDF (base, not section) to the FRACTION Frame. This Mapping is
*  the formed by applying the Mapping from base to section (MAP0),
*  followed by the Mapping from section to FRACTION (MAP).
         CMPMAP = AST_CMPMAP( MAP0, MAP, .TRUE., ' ', STATUS )

*  Use this Mapping to remap the fourth Frame to define the normalised
*  pixel coordinate system.
         CALL AST_REMAPFRAME( IWCS, 4, CMPMAP, STATUS )

*  Free remaining resource.
         CALL AST_ANNUL( MAP, STATUS )
         CALL AST_ANNUL( CMPMAP, STATUS )
         CALL AST_ANNUL( MAP0, STATUS )

      END IF

*  Set up Frame attributes.
*  ------------------------
*  Save the current Frame index and loop through the standard Frames in
*  the Frameset, making each current in turn. Initialise each of these
*  Frames (this sets up its title, axis labels, etc.). Restore the
*  original current Frame afterwards.
      ICURR = AST_GETI( IWCS, 'Current', STATUS )
      DO 6 IFRAME = 1, NSTD
         CALL AST_SETI( IWCS, 'Current', IFRAME, STATUS )
         CALL NDF1_INIFR( IACB, IWCS, STATUS )
 6    CONTINUE
      CALL AST_SETI( IWCS, 'Current', ICURR, STATUS )

*  Simplify the resulting FrameSet.
      NEW = AST_SIMPLIFY( IWCS, STATUS )
      CALL AST_ANNUL( IWCS, STATUS )
      IWCS = NEW

*  If an error occurred, annul the returned FrameSet pointer.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IWCS, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_RDWCS', STATUS )

      END
