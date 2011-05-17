      SUBROUTINE KPS1_WALA8( INDF1,  NDIMR, IWCSR, LBND, UBND, NDIMRT,
     :                       IWCSRT, LBNDT, UBNDT, STATUS )
*+
*  Name:
*     KPS1_WALA8

*  Purpose:
*     Modify reference to have same number of pixel axes as input NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA8( INDF1,  NDIMR, IWCSR, LBND, UBND, NDIMRT,
*                      IWCSRT, LBNDT, UBNDT, STATUS )

*  Description:
*     If the input NDF (INDF1) has more pixel axes than the reference NDF
*     (NDIMR), this routine modifies the supplied information describing the
*     reference NDF so that it describes the same number of pixel axes as
*     the input NDF. The extra axes are copied from the input NDF
*     unchanged.
*
*     The overall strategy is: 1) use AST_CONVERT and AST_MAPSPLIT to
*     identify the reference WCS axes within the current Frame of the
*     input NDF, 2) thus identify the axes within the current Frame of the
*     input NDF that are NOT present in the reference Frame, 3) use
*     AST_MAPSPLIT to split ithe input NDF WCS FrameSet and so get the
*     Mapping from these "extra" input WCS axes to the corresponding input
*     grid axes, 4) Construct the modified reference FrameSet by adding
*     the extra WCS axes into the current Frame, and using the Mapping
*     from step 3) to connect these new WCS axes to corresponding new
*     grid axes.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     NDIMR = INTEGER (Given)
*        No. of pixel axes in reference NDF.
*     IWCSR = INTEGER (Given)
*        AST pointer for the WCS FrameSet from the reference NDF.
*     LBND( NDF__MXDIM ) = INTEGER (Given)
*        Lower pixel bounds of each output NDF. VAL__BADI if not used.
*     UBND( NDF__MXDIM ) = INTEGER (Given)
*        Upper pixel bounds of each output NDF. VAL__BADI if not used.
*     NDIMRT = INTEGER (Returned)
*        Modified no. of reference pixel axes.
*     IWCSRT = INTEGER (Returned)
*        AST pointer for the modified reference WCS FrameSet.
*     LBNDT( NDF__MXDIM ) = INTEGER (Returned)
*        Modified lower pixel bounds of each output NDF. VAL__BADI if not used.
*     UBNDT( NDF__MXDIM ) = INTEGER (Returned)
*        Modified upper pixel bounds of each output NDF. VAL__BADI if not used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-FEB-2011 (DSB):
*        Original version
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER NDIMR
      INTEGER IWCSR
      INTEGER LBND( NDF__MXDIM )
      INTEGER UBND( NDF__MXDIM )

*  Arguments Returned:
      INTEGER NDIMRT
      INTEGER IWCSRT
      INTEGER LBNDT( NDF__MXDIM )
      INTEGER UBNDT( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Axis index
      INTEGER IWCS1
      INTEGER CFRM1
      INTEGER CFRMR
      INTEGER FS
      INTEGER NDIM1
      INTEGER INAX( NDF__MXDIM )
      INTEGER OUTAX( NDF__MXDIM )
      INTEGER LBND1( NDF__MXDIM )
      INTEGER UBND1( NDF__MXDIM )
      INTEGER NIN
      INTEGER NOUT
      INTEGER MAP
      INTEGER J
      INTEGER JUNK
      LOGICAL PRESENT
      INTEGER NEXTRA
      INTEGER EXTRA( NDF__MXDIM )
      INTEGER FEXTRA
*.

*  Check inherited global status.
      IWCSRT = AST__NULL
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise returned values to be copies of the supplied values.
      NDIMRT = NDIMR
      IWCSRT = AST_CLONE( IWCSR, STATUS )
      DO I = 1, NDF__MXDIM
         LBNDT( I ) = LBND( I )
         UBNDT( I ) = UBND( I )
      END DO

*  Get the bounds of the input NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )

*  Do nothing more unless the input NDF has more pixel axes than the
*  reference NDF.
      IF( NDIM1 .GT. NDIMR ) THEN

*  Get the WCS FrameSet from the input NDF.
         CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )

*  See if a Mapping can be found from the current Frame in the reference
*  NDF to the current Frame in the input NDF.
         CFRM1 = AST_GETFRAME( IWCS1, AST__CURRENT, STATUS )
         CFRMR = AST_GETFRAME( IWCSR, AST__CURRENT, STATUS )
         FS = AST_CONVERT( CFRMR, CFRM1, ' ', STATUS )
         IF( FS .NE. AST__NULL ) THEN


*  Attempt to split the Mapping from reference to input to identify the
*  input axes that are fed by the reference axes. First select all the
*  reference axes (the reference axes act as inputs to the Mapping).
            NIN = AST_GETI( FS, 'Nin', STATUS )
            DO I = 1, NIN
               INAX( I ) = I
            END DO

*  Now split off the Mapping that describes just these axes.
            CALL AST_MAPSPLIT( FS, NIN, INAX, OUTAX, MAP, STATUS )

*  Check the Mappign could be split, and that the resulting Mapping has
*  an output for each of the reference axes.
            IF( MAP .NE. AST__NULL .AND.
     :          AST_GETI( MAP, 'Nout', STATUS ) .EQ. NIN ) THEN

*  Get a list of the input frame axes that are not present in the reference
*  Frame.
               NOUT = AST_GETI( FS, 'Nout', STATUS )
               NEXTRA = 0
               DO I = 1, NOUT
                  PRESENT = .FALSE.
                  DO J = 1, NIN
                     IF( OUTAX( J ) .EQ. I ) PRESENT = .TRUE.
                  END DO
                  IF( .NOT. PRESENT ) THEN
                     NEXTRA = NEXTRA + 1
                     EXTRA( NEXTRA ) = I
                  END IF
               END DO

*  Attempt to get a Mapping from these extra axes in the input frame, to
*  the corresponding input grid axes.
               CALL AST_INVERT( IWCS1, STATUS )
               CALL AST_MAPSPLIT( IWCS1, NEXTRA, EXTRA, OUTAX, MAP,
     :                            STATUS )
               IF( MAP .NE. AST__NULL .AND.
     :             AST_GETI( MAP, 'Nout', STATUS ) .EQ.
     :             NDIM1 - NDIMR ) THEN

*  Invert the split Mapping so that it goes from grid axes to WCS axes.
                  CALL AST_INVERT( MAP, STATUS )

*  Append the bounds of the new pixel axes to the returned arrays.
                  DO I = 1, NEXTRA
                     LBNDT( NDIMR + I ) = LBND1( OUTAX( I ) )
                     UBNDT( NDIMR + I ) = UBND1( OUTAX( I ) )
                  END DO

*  Get a Frame holding the new WCS axes to add to the reference Frame.
*  These are the extra WCS axes from the input that were not originally
*  present in the reference frame.
                  FEXTRA = AST_PICKAXES( CFRM1, NEXTRA, EXTRA, JUNK,
     :                                   STATUS )

*  The returned (i.e. modified) reference FrameSet is initially just a
*  copy of the supplied reference FrameSet.
                  IWCSRT = AST_COPY( IWCSR, STATUS )

*  Now modify it by adding the new WCS axes to all WCS Frames, adding
*  corresponding pixel axes to the GRID and PIXEL Frames, etc.
                  CALL ATL_ADDWCSAXIS( IWCSRT, MAP, FEXTRA,
     :                                 LBNDT( NDIMR + 1 ),
     :                                 UBNDT( NDIMR + 1 ), STATUS )

*  The number of pixel axes in the modified reference is equal to the
*  number of pixel axes in the input.
                  NDIMRT = NDIM1

               END IF
            END IF
         ENDIF
      END IF

*  Export the FrameSet from the AST context into the parent context.
      CALL AST_EXPORT( IWCSRT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
