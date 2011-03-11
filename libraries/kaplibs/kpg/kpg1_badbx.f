      SUBROUTINE KPG1_BADBX( INDF1, OPER, INDF2, NGOOD, STATUS )
*+
*  Name:
*     KPG1_BADBX

*  Purpose:
*     Obtains an NDF section containing all good data in the supplied
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_BADBX( NDF, OPER, INDF2, NGOOD, STATUS )

*  Description:
*     This routine finds the pixel bounding box that encloses all good
*     data values in the DATA array of supplied NDF. It then either
*     creates and returns an NDF section corresponding to this bounding
*     box, or sets the pixel bounds of the supplied NDF to match the
*     bounding box.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The input NDF identifier.
*     OPER = INTEGER (Given)
*        If OPER is 1, the bounds of the supplied NDF will be modified to
*        match the bounding box enclosing the good data. Otherwise, INDF2
*        will be returned holding an NDF identifier for a section of the
*        supplied NDF matching the bounding box.
*     INDF2 = INTEGER (Returned)
*        An identifier for the smallest NDF section that contains all
*        good DATA values in the the input NDF. Returned equal to
*        NDF__NOID if OPER is 1, or if an error occurs.
*     NGOOD = INTEGER (Returned)
*        The number of good DATA values in the supplied NDF. Returned
*        equal to zero if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009-2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S Berry (JACH)
*     {enter_new_authors_here}

*  History:
*     9-MAR-2009 (DSB):
*        Original version.
*     10-MAR-2011 (DSB):
*        Added argument OPER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAI__ constants
      INCLUDE 'CNF_PAR'          ! CNF_ functions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER OPER

*  Arguments Returned:
      INTEGER INDF2
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE*( NDF__SZTYP )! Numeric type for processing
      INTEGER BLBND( NDF__MXDIM )! Lower bounds of bounding box
      INTEGER BUBND( NDF__MXDIM )! Upper bounds of bounding box
      INTEGER EL                 ! Number of elements in the NDF
      INTEGER IPDATA             ! Pointer to Data array
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of supplied NDF
      INTEGER NDIM               ! Total number of dimensions in the NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of supplied NDF
*.

*  Initialise returned values
      NGOOD = 0
      INDF2 = NDF__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the bounds of the supplied NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Find the data type of the supplied NDF, and map the Data array with
*  the same type.
      CALL NDF_TYPE( INDF1, 'Data', TYPE, STATUS )
      CALL KPG1_MAP( INDF1, 'Data', TYPE, 'READ', IPDATA, EL, STATUS )

*  Find the pixel index bounds that enclose all non-BAD values in the
*  data array.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_BBOXB( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADB, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_BBOXUB( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADUB, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_BBOXD( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADD, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_BBOXI( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADI, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_BBOXR( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADR, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_BBOXW( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADW, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_BBOXUW( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADUW, .FALSE., BLBND, BUBND, NGOOD,
     :                    STATUS )

      END IF

*  Report an error if no good data values were found in the input NDF.
      IF( NGOOD .EQ. 0 ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( ' ','No good data values found in ''^NDF''.',
     :                    STATUS )
         END IF

*  Otherwise, either create aN NDF section matching the bounding box, or
*  set the bounds of the supplied NDF
      ELSE IF( OPER .EQ. 1 ) THEN
         CALL NDF_SBND( NDIM, BLBND, BUBND, INDF1, STATUS )
      ELSE
         CALL NDF_SECT( INDF1, NDIM, BLBND, BUBND, INDF2, STATUS )
      END IF

      END
