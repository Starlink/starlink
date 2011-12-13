      SUBROUTINE CCD1_DCON( ITYPE, ARRAY, NCOL, NLINE, BAD, THRES,
     :                      TOUCH, XLIST, YLIST, ILIST, LABEL, NGROUP,
     :                      NTHRES, STATUS )
*+
*  Name:
*     CCD1_DCON

*  Purpose:
*     Determines the connectivity of pixels above a threshold.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DCON( ITYPE, ARRAY, NCOL, NLINE, BAD, THRES, TOUCH,
*                     XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
*                     STATUS )

*  Description:
*     This routine calls the appropriate version of CCG1_DCN to
*     determine the connectivitity of pixel groups using an 8-connected
*     pixel scan technique. The results of this process are a list of
*     the X and Y positions of the pixels above the threshold, their
*     value (intensity) and a list of labels which indicate which group
*     of connected pixels the pixel belongs to.
*
*     The basic algorithm is to find all non-bad pixels above a given
*     threshold and look at its neighbours at positions
*     x-1,y; x-1,y-1; x,y-1; x+1, y-1. If these are all below the
*     threshold then a new pixel group is begun, if any of these are
*     above the threshold then their group numbers are determined. The
*     current pixel is assigned to the group of one of these pixels and
*     all the groups numbers of these pixels are aliased to the same
*     group.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the input array.
*     ARRAY = INTEGER (Given)
*        The pointer to the input array.
*     NCOL = INTEGER (Given)
*        First dimension of the pixel array.
*     NLINE = INTEGER (Given)
*        Second dimension of the pixel array.
*     BAD = LOGICAL (Given)
*        Whether BAD pixels are present in the input data or nor.
*     THRES = DOUBLE PRECISION (Given)
*        The threshold value. The connectivity of pixels above this
*        value is determined.
*     TOUCH = LOGICAL (Given)
*        Whether pixel groups may touch the edges of the data array or
*        not.
*     XLIST( * ) = INTEGER (Returned)
*        A list of the X positions of the pixels above the threshold
*        value. This array should be large enough to accomdate all
*        pixels above the threshold.
*     YLIST( * ) = INTEGER (Returned)
*        A list of the Y positions of the pixels above the threshold
*        value. This array should be large enough to accomdate all
*        pixels above the threshold.
*     ILIST( * ) = DOUBLE PRECISION (Returned)
*        A list of the values of the thresholded pixels.
*     LABEL( * ) = INTEGER (Returned)
*        A list of the group to which a given pixel belongs to.
*        Groups are specified by having the same label.
*     NTHRES = INTEGER (Returned)
*        The number of pixel positions located.
*     NGROUP = INTEGER (Returned)
*        The number of pixel groups located. Note this may not be the
*        actual number of groups returned, but may be used as an upper
*        limit on the this number. Anti-aliasing of initially
*        fragmented groups reduces the number of groups returned from
*        this value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-1992 (PDRAPER):
*        Original version.
*     10-NOV-1992 (PDRAPER):
*        Added the touch option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      INTEGER NCOL
      INTEGER NLINE
      INTEGER ARRAY
      LOGICAL BAD
      LOGICAL TOUCH
      DOUBLE PRECISION THRES

*  Arguments Returned:
      INTEGER XLIST( * )
      INTEGER YLIST( * )
      DOUBLE PRECISION ILIST( * )
      INTEGER LABEL( * )
      INTEGER NGROUP
      INTEGER NTHRES

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate version of CCG1_DCN to count the values.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_DCNB( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_DCNUB( %VAL( CNF_PVAL( ARRAY ) ),
     :                    NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_DCNW( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_DCNUW( %VAL( CNF_PVAL( ARRAY ) ),
     :                    NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_DCNI( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_DCNR( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_DCND( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NCOL, NLINE, BAD, THRES, TOUCH,
     :                   XLIST, YLIST, ILIST, LABEL, NGROUP, NTHRES,
     :                   STATUS )
      ELSE

*  Unsupported data type.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_DCON',
     :   '  CCD1_DCON: Unsupported data type (^TYPE).', STATUS )

      END IF

      END
* $Id$
