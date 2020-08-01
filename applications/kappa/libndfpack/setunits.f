      SUBROUTINE SETUNITS( STATUS )
*+
*  Name:
*     SETUNITS

*  Purpose:
*     Sets a new units value for an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETUNITS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets a new value for the UNITS component of an
*     existing NDF data structure. The NDF is accessed in update mode
*     and any pre-existing UNITS component is over-written with a new
*     value.  Alternatively, if a `null' value (!) is given for the
*     UNITS parameter, then the NDF's UNITS component will be erased.
*
*     There is also an option to modify the pixel values within the NDF
*     to reflect the change in units (see Parameter MODIFY).

*  Usage:
*     setunits ndf units

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF data structure whose UNITS component is to be
*        modified.
*     MODIFY = _LOGICAL (Read)
*        If a TRUE value is supplied, then the pixel values in the DATA and
*        VARIANCE components of the NDF will be modified to reflect the
*        change in units. For this to be possible, both the original
*        Units value in the NDF and the new Units value must both correspond
*        to the format for units strings described in the FITS WCS standard
*        (see "Representations of world coordinates in FITS", Greisen &
*        Calabretta, 2002, A&A---available at http://www.aoc.nrao.edu/~egreisen/wcs_AA.ps.gz)
*        If either of the two units strings are not of this form, or if it is
*        not possible to find a transformation between them (for instance,
*        because they represent different quantities), an error is
*        reported. [FALSE]
*     UNITS = LITERAL (Read)
*        The value to be assigned to the NDF's UNITS component (e.g.
*        "J/(m**2*Angstrom*s)" or "count/s").  This value may later be used
*        by other applications for labelling graphs and other forms of
*        display where the NDF's data values are shown.  The suggested
*        default is the current value.

*  Examples:
*     setunits ngc1342 "count/s"
*        Sets the UNITS component of the NDF structure ngc1342 to have
*        the value "count/s". The pixel values are not changed.
*     setunits ndf=spect units="J/(m**2*Angstrom*s)"
*        Sets the UNITS component of the NDF structure spect to have
*        the value "J/(m**2*Angstrom*s)". The pixel values are not changed.
*     setunits datafile units=!
*        By specifying a null value (!), this example erases any
*        previous value of the UNITS component in the NDF structure
*        datafile. The pixel values are not changed.
*     setunits ndf=spect units="MJy" modify
*        Sets the UNITS component of the NDF structure spect to have
*        the value "MJy". If possible, the pixel values are changed from
*        their old units to the new units. For instance, if the UNITS
*        component of the NDF was original "J/(m**2*s*GHz)", the DATA
*        values will be multiplied by 1.0E11, and the variance values by
*        1.0E22.  However, if the original units component was (say) "K"
*        (Kelvin) then an error would be reported since there is no
*        direct conversion from Kelvin to MegaJansky.

*  Related Applications:
*     KAPPA: AXUNITS, SETLABEL, SETTITLE.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1995, 2003-2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1990 (RFWS):
*        Original version.
*     1995 April 21 (MJC):
*        Made usage and examples lowercase.  Added closing error
*        report and Related Applications.
*     30-APR-2003 (DSB):
*        Added MODIFY parameter.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NEWUN*100        ! New Unit value
      CHARACTER OLDUN*100        ! Original Unit value
      INTEGER EL                 ! Number of elements in array
      INTEGER FSET               ! FrameSet connecting old and new Units
      INTEGER IERR               ! Index of first numerical error
      INTEGER INDF               ! NDF identifier
      INTEGER IPDAT              ! Pointer to NDF DATA array
      INTEGER IPVAR              ! Pointer to NDF VARIANCE array
      INTEGER IPW1               ! Pointer to work space
      INTEGER IPW2               ! Pointer to work space
      INTEGER NERR               ! Number of numerical errors
      INTEGER NEWFRM             ! A 1D Frame with the new units
      INTEGER OLDFRM             ! A 1D Frame with the old units
      LOGICAL MODIFY             ! Modify DATA and VARIANCE values?
      LOGICAL VAR                ! Is the VARIANCE array defined?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Save the original Unit value.
      OLDUN = ' '
      CALL NDF_CGET( INDF, 'UNIT', OLDUN, STATUS )

*  Reset any existing units component.
      CALL NDF_RESET( INDF, 'Units', STATUS )

*  Obtain a new value for the units component.
      CALL NDF_CINP( 'UNITS', INDF, 'Units', STATUS )

*  Get the new Unit value.
      NEWUN = ' '
      CALL NDF_CGET( INDF, 'UNIT', NEWUN, STATUS )

*  See if the array components are to be modified accordingly.
      CALL PAR_GET0L( 'MODIFY', MODIFY, STATUS )
      IF( MODIFY ) THEN

*  If so, attempt to get an AST Mapping from the old Units to the
*  new Units. We do this by creating a pair of 1D Frames with the
*  appropriate units and then attempting to find a conversion between
*  them. We set the ActiveUnit flag for both Frames so that the Units
*  will be taken into accoutn when deriving the inter-Frame Mapping.
         OLDFRM = AST_FRAME( 1, ' ', STATUS )
         CALL AST_SETC( OLDFRM, 'Unit(1)', OLDUN, STATUS )
         CALL AST_SETACTIVEUNIT( OLDFRM, .TRUE., STATUS )

         NEWFRM = AST_FRAME( 1, ' ', STATUS )
         CALL AST_SETC( NEWFRM, 'Unit(1)', NEWUN, STATUS )
         CALL AST_SETACTIVEUNIT( NEWFRM, .TRUE., STATUS )

         FSET = AST_CONVERT( OLDFRM, NEWFRM, ' ', STATUS )

*  Re-instate the original Unit, and report an error if no conversion was
*  possible.
         IF( FSET .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
            CALL NDF_CPUT( OLDUN, INDF, 'UNIT', STATUS )
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_SETC( 'OLD', OLDUN )
            CALL MSG_SETC( 'NEW', NEWUN )
            CALL ERR_REP( 'SETUNIT_ERR1', 'Cannot convert ^NDF from '//
     :                    'units of ''^OLD'' to ''^NEW''.', STATUS )
            CALL ERR_REP( 'SETUNIT_ERR2', 'Unable to determine the '//
     :                    'transformation between units.', STATUS )

*  If a conversion was possible...
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Map the DATA array for update. We are constrained to use _DOUBLE since
*  AST_TRAN1 only handles double precision values.
            CALL NDF_MAP( INDF, 'Data', '_DOUBLE', 'UPDATE', IPDAT, EL,
     :                    STATUS )

*  Make a copy of it.
            CALL PSX_CALLOC( EL, '_DOUBLE', IPW1, STATUS )
            CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( IPDAT ) ),
     :                     %VAL( CNF_PVAL( IPW1 ) ),
     :                     IERR, NERR, STATUS )

*  Transform the data values.
            CALL AST_TRAN1( FSET, EL, %VAL( CNF_PVAL( IPW1 ) ), .TRUE.,
     :                      %VAL( CNF_PVAL( IPDAT ) ), STATUS )

*  If the VARIANCE array is defined, we need to transform it. We do this
*  by perturbing each data value by an amount equal to the corresponding
*  error estimate.
            CALL NDF_STATE( INDF, 'Variance', VAR, STATUS )
            IF( VAR ) THEN

*  Map the VARIANCE array for update.
               CALL NDF_MAP( INDF, 'Variance', '_DOUBLE', 'UPDATE',
     :                       IPVAR, EL, STATUS )

*  Find the square root of the Variance values (i.e. error estimates),
*  putting them in new work space.
               CALL PSX_CALLOC( EL, '_DOUBLE', IPW2, STATUS )
               CALL VEC_SQRTD( .TRUE., EL, %VAL( CNF_PVAL( IPVAR ) ),
     :                         %VAL( CNF_PVAL( IPW2 ) ),
     :                         IERR, NERR, STATUS )

*  Perturb the original DATA values by an amount equal to the error
*  estimate. Put the result back in the IPW1 workspace.
               CALL VEC_ADDD( .TRUE., EL, %VAL( CNF_PVAL( IPW1 ) ),
     :                        %VAL( CNF_PVAL( IPW2 ) ),
     :                        %VAL( CNF_PVAL( IPW1 ) ),
     :                        IERR, NERR, STATUS )

*  Transform these perturbed data values using the inter-unit Mapping,
*  putting the result back in the IPW2 array.
               CALL AST_TRAN1( FSET, EL, %VAL( CNF_PVAL( IPW1 ) ),
     :                         .TRUE.,
     :                         %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Find the difference between the tranformed original and transformed
*  perturbed data values, putting the result in IPW1.
               CALL VEC_SUBD( .TRUE., EL, %VAL( CNF_PVAL( IPDAT ) ),
     :                        %VAL( CNF_PVAL( IPW2 ) ),
     :                        %VAL( CNF_PVAL( IPW1 ) ),
     :                        IERR, NERR, STATUS )

*  Square these differences, putting the result in the output variance
*  array.
               CALL VEC_MULD( .TRUE., EL, %VAL( CNF_PVAL( IPW1 ) ),
     :                        %VAL( CNF_PVAL( IPW1 ) ),
     :                        %VAL( CNF_PVAL( IPVAR ) ),
     :                        IERR, NERR, STATUS )

*  Release work space.
               CALL PSX_FREE( IPW2, STATUS )

            END IF
            CALL PSX_FREE( IPW1, STATUS )

         END IF
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Write the closing error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETUNITS_ERR', 'SETUNITS: Error modifying '//
     :                 'the units of an NDF.', STATUS )
      END IF

      END
