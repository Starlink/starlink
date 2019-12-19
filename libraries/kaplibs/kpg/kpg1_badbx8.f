      SUBROUTINE KPG1_BADBX8( INDF1, OPER, INDF2, NGOOD, STATUS )
*+
*  Name:
*     KPG1_BADBX8

*  Purpose:
*     Obtains an NDF section containing all good data in the supplied
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_BADBX8( INDF1, OPER, INDF2, NGOOD, STATUS )

*  Description:
*     This routine finds the pixel bounding box that encloses all good
*     data values in the DATA array of supplied NDF. It then either
*     creates and returns an NDF section corresponding to this bounding
*     box, or sets the pixel bounds of the supplied NDF to match the
*     bounding box.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The input NDF identifier. Note, if OPER is 1 or 2 then any mapped
*        access to the NDF will be unmapped on exit. In addition, if
*        OPER is 2 then any mapped access to NDFs that are located within
*        an extension of the supplied NDF will be unmapped on exit. Also,
*        for OPER 1 and 2, "UPDATE" access is required to the NDF, and
*        (for OPER 2) any extension NDFs.
*     OPER = INTEGER (Given)
*        Indicates how the box should be used.
*
*        1 - the bounds of the supplied NDF will be modified to match the
*        bounding box enclosing the good data.
*
*        2 - the bounds of the supplied NDF will be modified to match the
*        bounding box enclosing the good data. In addition, any NDFs
*        found within the MORE component of the supplied NDF, which
*        have bounds equal to those of the supplied NDF, are changed to
*        match the bounds of the bounding box.
*
*        If any other value is supplied for OPER, INDF2 will be returned
*        holding an NDF identifier for a section of the supplied NDF
*        matching the bounding box.
*     INDF2 = INTEGER (Returned)
*        An identifier for the smallest NDF section that contains all
*        good DATA values in the the input NDF. Returned equal to
*        NDF__NOID if OPER is 1, or if an error occurs.
*     NGOOD = INTEGER*8 (Returned)
*        The number of good DATA values in the supplied NDF. Returned
*        equal to zero if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009-2012 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (JACH)
*     MJC: Malcolm J. Currie (MJC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-MAR-2009 (DSB):
*        Original version.
*     10-MAR-2011 (DSB):
*        Added argument OPER.
*     9-MAY-2011 (DSB/MJC):
*        Set mandatory STATUS bad before issuing an ERR_REP.
*     2012-05-09 (TIMJ):
*        Add _INT64
*     19-DEC-2019 (DSB):
*        Support huge files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAI__ constants
      INCLUDE 'CNF_PAR'          ! CNF_ functions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'GRP_PAR'          ! GRP__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER OPER

*  Arguments Returned:
      INTEGER INDF2
      INTEGER*8 NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE*( NDF__SZTYP )! Numeric type for processing
      INTEGER*8 EL                 ! Number of elements in the NDF
      INTEGER I                    ! NDF index within group
      INTEGER IGRP                 ! Group holding extension NDFs
      INTEGER INDFB                ! Identifier for base NDF
      INTEGER INDFX                ! Identifier for extension NDF
      INTEGER IPDATA               ! Pointer to Data array
      INTEGER J                    ! Pixel axis index
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower bounds of supplied NDF
      INTEGER*8 LBNDB( NDF__MXDIM )! Lower bounds of base NDF
      INTEGER*8 LBNDX( NDF__MXDIM )! Lower bounds of extension NDF
      INTEGER NDIM                 ! Total no. of dims in the NDF
      INTEGER NDIMB                ! Total no. of dims in base NDF
      INTEGER NDIMX                ! Total no. of dims in extension NDF
      INTEGER*8 OLBND( NDF__MXDIM )! Lower bounds of bounding box
      INTEGER*8 OUBND( NDF__MXDIM )! Upper bounds of bounding box
      INTEGER SIZE                 ! Number of extension NDFs
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper bounds of supplied NDF
      INTEGER*8 UBNDB( NDF__MXDIM )! Upper bounds of base NDF
      INTEGER*8 UBNDX( NDF__MXDIM )! Upper bounds of extension NDF
      LOGICAL SAME                 ! Base and extension NDFs match?
*.

*  Initialise returned values
      NGOOD = 0
      INDF2 = NDF__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the bounds of the supplied NDF.
      CALL NDF_BOUND8( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Find the data type of the supplied NDF, and map the Data array with
*  the same type.
      CALL NDF_TYPE( INDF1, 'Data', TYPE, STATUS )
      CALL KPG1_MAP8( INDF1, 'Data', TYPE, 'READ', IPDATA, EL, STATUS )

*  Find the pixel index bounds that enclose all non-BAD values in the
*  data array.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_BBOX8B( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADB, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_BBOX8UB( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA )),
     :                    VAL__BADUB, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_BBOX8D( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADD, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_BBOX8I( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADI, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_BBOX8R( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADR, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_BBOX8W( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADW, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_BBOX8UW( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA )),
     :                    VAL__BADUW, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_BBOX8K( NDIM, LBND, UBND, %VAL( CNF_PVAL( IPDATA ) ),
     :                    VAL__BADK, .FALSE., OLBND, OUBND, NGOOD,
     :                    STATUS )

      END IF

*  Report an error if no good data values were found in the input NDF.
      IF( NGOOD .EQ. 0 ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( ' ','No good data values found in ''^NDF''.',
     :                    STATUS )
         END IF

*  Otherwise, if required, set the bounds of the supplied NDF and
*  optionally any extension NDFs.
      ELSE IF( OPER .EQ. 1 .OR. OPER .EQ. 2 ) THEN

*  First unmap the NDF.
         CALL NDF_UNMAP( INDF1, '*', STATUS )

*  If required, just set the bounds of the supplied NDFs.
         IF( OPER .EQ. 1 ) THEN
            CALL NDF_SBND8( NDIM, OLBND, OUBND, INDF1, STATUS )

*  If required, set the bounds of supplied NDF and any matching extension
*  NDFs.
         ELSE

*  First note the original bounds of the base NDF associated with the
*  supplied NDF identifer.
            CALL NDF_BASE( INDF1, INDFB, STATUS )
            CALL NDF_BOUND8( INDFB, NDF__MXDIM, LBNDB, UBNDB, NDIMB,
     :                       STATUS )

*  Now set the new bounds in the supplied NDF.
            CALL NDF_SBND8( NDIM, OLBND, OUBND, INDF1, STATUS )

*  Get a GRP group containing paths to any NDFs contained within extensions
*  of the supplied NDF.
            IGRP = GRP__NOID
            CALL NDG_MOREG( INDF1, IGRP, SIZE, STATUS )

*  Loop round each extension NDF.
            DO I = 1, SIZE
               CALL NDG_NDFAS( IGRP, I, 'UPDATE', INDFX, STATUS )

*  Get its bounds.
               CALL NDF_BOUND8( INDFX, NDF__MXDIM, LBNDX, UBNDX, NDIMX,
     :                          STATUS )

*  See if this NDF has the same shape as the base NDF on the pixel axes
*  that they share in common...
               SAME = .TRUE.
               DO J = 1, MIN( NDIMX, NDIMB )
                  IF( UBNDX( J ) .NE. UBNDB( J ) .OR.
     :                LBNDX( J ) .NE. LBNDB( J ) ) SAME = .FALSE.
               END DO

*  If so, ensure that OLBND/OUBND arrays inherit bounds from the input
*  extension NDF if the extension NDF has more axes than the main NDF.
               IF( SAME ) THEN
                  IF( NDIMX .GT. NDIM ) THEN
                     DO J = NDIM + 1, NDIMX
                        OLBND( J ) = LBNDX( J )
                        OUBND( J ) = UBNDX( J )
                     END DO
                  END IF

*  Ensure the NDF is unmapped.
                  CALL NDF_UNMAP( INDFX, '*', STATUS )

*  Set the bounds of the extension NDF.
                  CALL NDF_SBND8( NDIMX, OLBND, OUBND, INDFX, STATUS )

*  Anull the extension NDF identifier.
                  CALL NDF_ANNUL( INDFX, STATUS )

               END IF
            END DO

*  Free resources.
            CALL GRP_DELET( IGRP, STATUS )
            CALL NDF_ANNUL( INDFB, STATUS )

         END IF

*  Otherwise, create an NDF section matching the bounding box.
      ELSE
         CALL NDF_SECT8( INDF1, NDIM, OLBND, OUBND, INDF2, STATUS )
      END IF

      END
