      SUBROUTINE KPS1_LPLLM( EL, ILO, IHI, DAT, USEVAR, GOTSIG, CUTNEG,
     :                       VAR, NSIGMA, MAP1, MAP2, IAX, CEN, BAR, HI,
     :                       LO, MONO, BAD, STATUS )

*+
*  Name:
*     KPS1_LPLLM

*  Purpose:
*     Find mapped error bar limits for LINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LPLLM( EL, ILO, IHI, DAT, USEVAR, GOTSIG, CUTNEG, VAR,
*                      NSIGMA, MAP1, MAP2, IAX, CEN, BAR, HI, LO, MONO,
*                      BAD, STATUS )

*  Description:
*     This routine finds the ends of a set of error bars, maps them into
*     another co-ordinate system using 2 supplied AST Mappings, and finds
*     the extreme data values spanned by a given range of the resulting error
*     bars. The central values are also mapped, returned, and included in the
*     returned extreme data values.
*
*     The mapping from supplied to returned values is specified by 1 or 2
*     supplied mappings, which are applied in series.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of data values.
*     ILO = INTEGER (Given)
*        The lowest index to include in the returned limits.
*     IHI = INTEGER (Given)
*        The highest index to include in the returned limits.
*     DAT( EL ) = DOUBLE PRECISION (Given)
*        The supplied central data values.
*     USEVAR = LOGICAL (Given)
*        Are variance values to be used?
*     GOTSIG = LOGICAL (Given)
*        If .TRUE., the values supplied in VAR( EL ) are standard
*        deviations instead of variances.
*     CUTNEG = LOGICAL (Given)
*        If .TRUE., then central data values less than or equal to zero
*        are ignored (they are treated like bad values).
*     VAR( EL ) = DOUBLE PRECISION (Given)
*        The supplied variance values. Only accessed if USEVAR is .TRUE.
*     NSIGMA = REAL (Given)
*        The number of standard deviation in each error bar on each side of
*        the central value. Only accessed if USEVAR is .TRUE.
*     MAP1 = INTEGER (Given)
*        A pointer to a mapping to be applied to the supplied data values.
*        This should have a single input and a single output. The forward
*        direction is used. A UnitMap is used if this is AST__NULL.
*     MAP2 = INTEGER (Given)
*        A pointer to a mapping to be applied to the results of MAP1.
*        This should have equal number of inputs and outputs. The forward
*        direction is used. All axes in this mapping must be independant.
*     IAX = INTEGER (Given)
*        The axis of MAP2 to use.
*     CEN( EL ) = DOUBLE PRECISION (Returned)
*        The central values after mapping with MAP1 and MAP2.
*     BAR( EL, 2 ) = DOUBLE PRECISION (Returned)
*        Row 1 contains the lower limit and row 2 contains the upper limit
*        for each error bar after mapping with MAP1 and MAP2.Only accessed
*        if USEVAR is .TRUE.
*     HI = DOUBLE PRECISION (Returned)
*        The maximum upper limit of any data value after mapping
*        (including error bars).
*     LO = DOUBLE PRECISION (Returned)
*        The minimum lower limit of any data value after mapping
*        (including error bars).
*     MONO = INTEGER (Returned)
*        Returned equal to +1 if the mapped central value increases
*        monotonically from element 1 to element EL. Returned equal to
*        -1 if the mapped central value decreases monotonically from
*        element 1 to element EL. Returned equal to zero if the mapped
*        central value is not monotonic.
*     BAD = LOGICAL (Returned)
*        Were any bad mapped values found?
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: DAVID S. BERRY (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     29-OCT-2008 (DSB):
*        Added CUTNEG argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      INTEGER ILO
      INTEGER IHI
      DOUBLE PRECISION DAT( EL )
      LOGICAL USEVAR
      LOGICAL GOTSIG
      LOGICAL CUTNEG
      DOUBLE PRECISION VAR( EL )
      REAL NSIGMA
      INTEGER MAP1
      INTEGER MAP2
      INTEGER IAX

*  Arguments Returned:
      DOUBLE PRECISION CEN( EL )
      DOUBLE PRECISION BAR( EL, 2 )
      DOUBLE PRECISION HI
      DOUBLE PRECISION LO
      INTEGER MONO
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION LCEN      ! Previous good value
      DOUBLE PRECISION LENBAR    ! Length of half the error bar
      INTEGER I                  ! Loop count
      INTEGER MAP                ! Pointer to total mapping
      INTEGER NIN2               ! No of axes for MAP2
      INTEGER PERM( NDF__MXDIM ) ! Axis permutation array
      INTEGER PMAP1              ! PermMap pointer
      INTEGER PMAP2              ! PermMap pointer
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Create a compound Mapping from MAP1 and MAP2.
*  =============================================
*  Get the number of input axes for MAP2.
      NIN2 = AST_GETI( MAP2, 'NIN', STATUS )

*  Create a PermMap with a single input which is fed by the output from
*  MAP1, and NIN2 outputs which feed the NIN2 inputs of MAP2. The output
*  from MAP1 is routed to the selected axis of MAP2. Other PermMap output
*  axes are assigned AST__BAD by the PermMaps forward transformation.
      DO I = 1, NIN2
         PERM( I ) = 0
      END DO
      PERM( IAX ) = 1

      PMAP1 = AST_PERMMAP( 1, IAX, NIN2, PERM, 0.0D0, ' ', STATUS )

*  Create a PermMap with a NIN2 inputs which are fed by the NIN2 outputs from
*  MAP2, and a single output which produces the required mapped values. The
*  output from the selected axis of MAP2 is fed to the single PermMap
*  output. Other PermMap inputs are assigned AST__BAD by the PermMaps
*  inverse transformation.
      PMAP2 = AST_PERMMAP( NIN2, PERM, 1, IAX, 0.0D0, ' ', STATUS )

*  Compound all the Mappings in series.
      IF( MAP1 .NE. AST__NULL ) THEN
         MAP = AST_CMPMAP( MAP1, PMAP1, .TRUE., ' ', STATUS )
      ELSE
         MAP = AST_CLONE( PMAP1, STATUS )
      END IF

      MAP = AST_CMPMAP( MAP, MAP2, .TRUE., ' ', STATUS )
      MAP = AST_CMPMAP( MAP, PMAP2, .TRUE., ' ', STATUS )

*  Map the central values.
*  =======================
*  Map them.
      CALL AST_TRAN1( MAP, EL, DAT, .TRUE., CEN, STATUS )

*  Initialise the limits.
      HI = VAL__MIND
      LO = VAL__MAXD

*  No bad values found yet.
      BAD = .FALSE.

*  Set MONO to 2 to indicate that we do not yet know whether the mapped
*  central values are monotonic.
      MONO = 2

*  No previous value available yet.
      LCEN = AST__BAD

*  Do each point.
      DO I = 1, EL

*  Set the central value bad if the data value is not positive and we
*  are cutting such values.
         IF( DAT( I ) .LE. 0.0 .AND. CUTNEG ) THEN
            CEN( I ) = AST__BAD
            BAD = .TRUE.

*  Only include good positions.
         ELSE IF( CEN( I ) .NE. AST__BAD ) THEN

*  If this element is within the specified bounds, find the mapped limits.
            IF( I .GE. ILO .AND. I .LE. IHI ) THEN
               HI = MAX( HI, CEN( I ) )
               LO = MIN( LO, CEN( I ) )
            END IF

*  If the previous data was monotonic increasing, reset MONO to zero
*  if this value is less than or equal to the last good value.
            IF( MONO .EQ. 1 ) THEN
               IF( CEN( I ) .LE. LCEN ) MONO = 0

*  If the previous data was monotonic decreasing, reset MONO to zero
*  if this value is greater than or equal to the last good value.
            ELSE IF( MONO .EQ. -1 ) THEN
               IF( CEN( I ) .GE. LCEN ) MONO = 0

*  If we do not yet know whether the data is increasing or decreasing,
*  decide now if we have a previous good value.
            ELSE IF( MONO .EQ. 2 .AND. LCEN .NE. AST__BAD ) THEN

               IF( CEN( I ) .GT. LCEN ) THEN
                  MONO = 1
               ELSE IF( CEN( I ) .LT. LCEN ) THEN
                  MONO = -1
               ELSE
                  MONO = 0
               END IF

            END IF

*  Store this good value.
            LCEN = CEN( I )

*  Return a TRUE flag if a bad value was found.
         ELSE
            BAD = .TRUE.
         END IF

      END DO

*  Map the error bars if supplied.
*  ===============================
      IF( USEVAR ) THEN

*  Store the unmapped limits in BAR.
         DO I = 1, EL

*  Ignore this data value if it is bad, or if it is not positive and
*  CUTNEG is true.
            IF( ( DAT( I ) .NE. AST__BAD .AND.
     :          ( .NOT. CUTNEG .OR. DAT( I ) .GT. 0.0 ) ) .AND.
     :          VAR( I ) .NE. AST__BAD ) THEN

               IF( GOTSIG ) THEN
                  LENBAR = NSIGMA*MAX( 0.0D0, VAR( I ) )
               ELSE
                  LENBAR = NSIGMA*SQRT( MAX( 0.0D0, VAR( I ) ) )
               END IF

               BAR( I, 1 ) = DAT( I ) - LENBAR
               BAR( I, 2 ) = DAT( I ) + LENBAR
            ELSE
               BAR( I, 1 ) = AST__BAD
               BAR( I, 2 ) = AST__BAD
            END IF
         END DO

*  Map them.
         CALL AST_TRAN1( MAP, 2*EL, BAR, .TRUE., BAR, STATUS )

*  Find the mapped limits.
         DO I = ILO, IHI

            IF( BAR( I, 1 ) .NE. AST__BAD ) THEN
               HI = MAX( HI, BAR( I, 1 ) )
               LO = MIN( LO, BAR( I, 1 ) )
            ELSE
               BAD = .TRUE.
            END IF

            IF( BAR( I, 2 ) .NE. AST__BAD ) THEN
               HI = MAX( HI, BAR( I, 2 ) )
               LO = MIN( LO, BAR( I, 2 ) )
            ELSE
               BAD = .TRUE.
            END IF

         END DO

      END IF

*  Return bad limits if required.
      IF( HI .EQ. VAL__MIND .OR. LO .EQ. VAL__MAXD ) THEN
         HI = AST__BAD
         LO = AST__BAD
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
