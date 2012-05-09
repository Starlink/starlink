      SUBROUTINE CCD1_CENT( TYPE, IPDATA, NCOL, NLINE, LBND, IDIN,
     :                      XIN, YIN, NIN, ISIZE, SIGN, MAXSHF, MAXIT,
     :                      TOLER, IDOUT, XOUT, YOUT, NOUT, STATUS )
*+
*  Name:
*     CCD1_CENT

*  Purpose:
*     Finds the centroid of features in a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CENT( TYPE, IPDATA, NCOL, NLINE, LBND, IDIN, XIN, YIN,
*                     NIN, ISIZE, SIGN, MAXSHF, MAXIT, TOLER, IDOUT,
*                     XOUT, YOUT, NOUT, STATUS )

*  Description:
*     This routine locates the centroids of objects whose positions are
*     specified in the XIN and YIN arrays. The input data is pointed to
*     by IPDATA and has size NCOL by NLINE with X and Y origins given
*     by LBND. The input data may be of any non-complex numeric data
*     type.  If no centroid is located at a position then a error
*     message is issued to the user and no corresponding output
*     position is given. The correspondence of the input positions with
*     the output positions is determined by the identifiers which are
*     given only those positions which successfully centroid have their
*     identifiers returned.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPDATA. This must be one of
*        the non-complex HDS numeric types.
*     IPDATA = INTEGER (Given)
*        Pointer to an array of data of type TYPE which contains the
*        image features to be centroided.
*     NCOL = INTEGER (Given)
*        First dimension of the data pointed to by IPDATA.
*     NLINES = INTEGER (Given)
*        Second dimension of the data pointed to by IPDATA.
*     LBND( 2 ) = INTEGER (Given)
*        The origins of the coordinate system of the data array
*        IPDATA. These values are those returned by the NDF system
*        from which IPDATA was mapped.
*     IDIN( NIN ) = INTEGER (Given)
*        Identifiers of the XIN and YIN data. The correspondence of
*        these values to XIN and YIN are maintained when the XIN and YIN
*        positions are centroided.
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        X positions of features whose centroid is to be determined.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Y positions of features whose centroid is to be determined.
*     ISIZE = INTEGER (Given)
*        The size of the search square side.
*     SIGN = LOGICAL (Given)
*        True of the image features have positive data values. False
*        if they have negative values.
*     MAXSHF = DOUBLE PRECISION (Given)
*        Maximum allowed shift in image centre.
*     MAXIT = INTEGER (Given)
*        Maximum number of refining iterations.
*     TOLER = DOUBLE PRECISION (Given)
*        Accuracy with which the centroid should be located.
*     IDOUT( * ) = INTEGER (Returned)
*        Identifiers of the XOUT and YOUT data. These correspond to
*        the identifiers of the input positions which are successfully
*        centroided.
*     XOUT( * ) = DOUBLE PRECISION (Returned)
*        X positions of features whose centroid has been determined.
*     YOUT( * ) = DOUBLE PRECISION (Given)
*        X positions of features whose centroid has been determined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (PDRAPER):
*        Original version.
*     8-APR-2001 (MBT):
*        Modified so that finding no centroids is no longer an error.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER IPDATA
      INTEGER NCOL
      INTEGER NLINE
      INTEGER LBND( 2 )
      INTEGER NIN
      INTEGER IDIN( NIN )
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      INTEGER ISIZE
      LOGICAL SIGN
      DOUBLE PRECISION MAXSHF
      INTEGER MAXIT
      DOUBLE PRECISION TOLER

*  Arguments Returned:
      INTEGER IDOUT( * )
      DOUBLE PRECISION XOUT( * )
      DOUBLE PRECISION YOUT( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XACC      ! Centroid position
      DOUBLE PRECISION XPOS      ! Initial position
      DOUBLE PRECISION YACC      ! Centroid position
      DOUBLE PRECISION YPOS      ! Initial position
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up an error context to trap all error messages. These are
*  informational messages from the centroiding routine.
      CALL ERR_MARK

*  Try to determine the centroid for all positions.
      NOUT = 0
      DO 1 I = 1, NIN

*  Transform this position to pixel indices.
         XPOS = XIN( I ) - DBLE( LBND( 1 ) ) + 1.5D0
         YPOS = YIN( I ) - DBLE( LBND( 2 ) ) + 1.5D0

*  Centroid this position.
         IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_CENUB( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                       NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_CENB( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_CENUW( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                       NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL CCG1_CENW( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_CENI( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL CCG1_CENR( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_CEND( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL CCG1_CENK( XPOS, YPOS, %VAL( CNF_PVAL( IPDATA ) ),
     :                      NCOL, NLINE,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE

*  Unknown data type.
            STATUS  = SAI__ERROR
            CALL ERR_REP( 'CCD1_CENT_UNKN',
     :                    '  CCD1_CENT: Unknown data type'  , STATUS )
            GO TO 99
         END IF

*  Transform this position back to input coordinates.
         XACC = XACC + DBLE( LBND( 1 ) ) - 1.5D0
         YACC = YACC + DBLE( LBND( 2 ) ) - 1.5D0

*  Check status. No values for this position - just annul the error and
*  continue.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Increment output count.
            NOUT = NOUT + 1

*  Set the output position.
            XOUT( NOUT ) = XACC
            YOUT( NOUT ) = YACC
            IDOUT( NOUT ) = IDIN( I )
         END IF
 1    CONTINUE
      CALL ERR_RLSE

 99   CONTINUE
      END
* $Id$
