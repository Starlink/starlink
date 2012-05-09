      SUBROUTINE CCD1_CEN2( TYPE1, IPDAT1, NCOL1, NLINE1, LBND1,
     :                      TYPE2, IPDAT2, NCOL2, NLINE2, LBND2,
     :                      XIN1, YIN1, XIN2, YIN2, NIN,
     :                      ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                      XOUT1, YOUT1, XOUT2, YOUT2, NOUT, STATUS )
*+
*  Name:
*     CCD1_CEN2

*  Purpose:
*     Finds the centroid of features in two data arrays, maintaining the
*     correspondence of input positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CEN2( TYPE1, IPDAT1, NCOL1, NLINE1, LBND1,
*                     TYPE2, IPDAT1, NCOL1, NLINE1, LBND2,
*                     XIN1, YIN1, XIN2, YIN2, NIN,
*                     ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
*                     XOUT1, YOUT1, XOUT2, YOUT2, NOUT, STATUS )

*  Description:
*     This routine locates the centroids of objects in the data arrays
*     pointed to by IPDAT1 and IPDAT2. The positions to be centroided
*     are specified in the XIN1, YIN and XIN2, YIN2 arrays,
*     respectively, these must be in array coordinates. On output
*     only positions which have been successfully centroided in both
*     arrays are returned (in corresponding positions in the output
*     position arrays). Origin information given in LBND1 and LBND2
*     are used to modify the positions before output, to give pixel
*     indices if required.

*  Arguments:
*     TYPE1 = CHARACTER * ( * ) (Given)
*        The HDS data pointed to by IPDAT1.
*     IPDAT1 = INTEGER (Given)
*        Pointer to an array of data of type TYPE1 which contains
*        image features to be centroided.
*     NCOL1 = INTEGER (Given)
*        First dimension of the data pointed to by IPDAT1.
*     NLINE1 = INTEGER (Given)
*        Second dimension of the data pointed to by IPDAT1.
*     LBND1( 2 ) = INTEGER (Given)
*        The lower bounds of the 1st input array coordinates (i.e. NDF
*        origins).
*     TYPE2 = CHARACTER * ( * ) (Given)
*        The HDS data pointed to by IPDAT2.
*     IPDAT2 = INTEGER (Given)
*        Pointer to an array of data of type TYPE2 which contains
*        image features to be centroided.
*     NCOL2 = INTEGER (Given)
*        First dimension of the data pointed to by IPDAT2.
*     NLINE1 = INTEGER (Given)
*        Second dimension of the data pointed to by IPDAT2.
*     LBND2( 2 ) = INTEGER (Given)
*        The lower bounds of the 2nd input array coordinates (i.e. NDF
*        origins).
*     XIN1( NIN ) = DOUBLE PRECISION (Given)
*        X ARRAY positions of features whose centroid is to be
*        determined in the data pointed to by IPDAT1.
*     YIN1( NIN ) = DOUBLE PRECISION (Given)
*        Y ARRAY positions of features whose centroid is to be
*        determined in the data pointed to by IPDAT1.
*     XIN2( NIN ) = DOUBLE PRECISION (Given)
*        X ARRAY positions of features whose centroid is to be
*        determined in the data pointed to by IPDAT2.
*     YIN2( NIN ) = DOUBLE PRECISION (Given)
*        Y ARRAY positions of features whose centroid is to be
*        determined in the data pointed to by IPDAT2.
*     NIN = INTEGER (Given)
*        The number of positions in the input arrays.
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
*     XOUT1( * ) = DOUBLE PRECISION (Returned)
*        X positions of features whose centroid has been determined
*        in IPDAT1 data and which have had a corresponding position
*        sucessfully centroid in the data pointed to by IPDAT2.
*        These positions are corrected for lower bounds (i.e. may be
*        pixel indices).
*     YOUT1( * ) = DOUBLE PRECISION (Given)
*        X positions of features whose centroid has been determined.
*        in IPDAT1 data and which have had a corresponding position
*        sucessfully centroid in the data pointed to by IPDAT2.
*        These positions are corrected for lower bounds (i.e. may be
*        pixel indices).
*     XOUT2( * ) = DOUBLE PRECISION (Returned)
*        X positions of features whose centroid has been determined.
*        in IPDAT2 data and which have had a corresponding position
*        sucessfully centroid in the data pointed to by IPDAT1.
*        These positions are corrected for lower bounds (i.e. may be
*        pixel indices).
*     YOUT2( * ) = DOUBLE PRECISION (Given)
*        X positions of features whose centroid has been determined.
*        in IPDAT2 data and which have had a corresponding position
*        sucessfully centroid in the data pointed to by IPDAT1.
*        These positions are corrected for lower bounds (i.e. may be
*        pixel indices).
*     NOUT = INTEGER (Given)
*        The number of suucessful centroid pairs returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (PDRAPER):
*        Original version.
*     3-MAR-1993 (PDRAPER):
*        Changed to process pairs of corresponding positions.
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
      CHARACTER * ( * ) TYPE1
      INTEGER IPDAT1
      INTEGER NCOL1
      INTEGER NLINE1
      INTEGER LBND1( 2 )
      CHARACTER * ( * ) TYPE2
      INTEGER IPDAT2
      INTEGER NCOL2
      INTEGER NLINE2
      INTEGER LBND2( 2 )
      INTEGER NIN
      DOUBLE PRECISION XIN1( NIN )
      DOUBLE PRECISION YIN1( NIN )
      DOUBLE PRECISION XIN2( NIN )
      DOUBLE PRECISION YIN2( NIN )
      INTEGER ISIZE
      LOGICAL SIGN
      DOUBLE PRECISION MAXSHF
      INTEGER MAXIT
      DOUBLE PRECISION TOLER

*  Arguments Returned:
      DOUBLE PRECISION XOUT1( * )
      DOUBLE PRECISION YOUT1( * )
      DOUBLE PRECISION XOUT2( * )
      DOUBLE PRECISION YOUT2( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XACC1     ! Centroid position
      DOUBLE PRECISION XACC2     ! Centroid position
      DOUBLE PRECISION XPOS1     ! Initial position
      DOUBLE PRECISION XPOS2     ! Initial position
      DOUBLE PRECISION YACC1     ! Centroid position
      DOUBLE PRECISION YACC2     ! Centroid position
      DOUBLE PRECISION YPOS1     ! Initial position
      DOUBLE PRECISION YPOS2     ! Initial position
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the input data types are known.
      IF ( TYPE1 .NE. '_UBYTE' .AND.
     :     TYPE1 .NE. '_BYTE' .AND.
     :     TYPE1 .NE. '_WORD' .AND.
     :     TYPE1 .NE. '_UWORD' .AND.
     :     TYPE1 .NE. '_INTEGER' .AND.
     :     TYPE1 .NE. '_INT64' .AND.
     :     TYPE1 .NE. '_REAL' .AND.
     :     TYPE1 .NE. '_DOUBLE' ) THEN
         STATUS  = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE1 )
         CALL ERR_REP( 'CCD1_CENT_UNKN',
     :   '  CCD1_CENT: Unknown data type ^TYPE'  , STATUS )
         GO TO 99
      END IF
      IF ( TYPE2 .NE. '_UBYTE' .AND.
     :     TYPE2 .NE. '_BYTE' .AND.
     :     TYPE2 .NE. '_WORD' .AND.
     :     TYPE2 .NE. '_UWORD' .AND.
     :     TYPE2 .NE. '_INTEGER' .AND.
     :     TYPE2 .NE. '_INT64' .AND.
     :     TYPE2 .NE. '_REAL' .AND.
     :     TYPE2 .NE. '_DOUBLE' ) THEN
         STATUS  = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE2 )
         CALL ERR_REP( 'CCD1_CENT_UNKN',
     :   '  CCD1_CENT: Unknown data type ^TYPE'  , STATUS )
         GO TO 99
      END IF

*  Set up an error context to trap all error messages. These are
*  informational messages from the centroiding routine.
      CALL ERR_MARK

*  Try to determine the centroid for all positions, one pair at a time.
      NOUT = 0
      DO 1 I = 1, NIN

*  Set the initial positions
         XPOS1 = XIN1( I )
         YPOS1 = YIN1( I )
         XPOS2 = XIN2( I )
         YPOS2 = YIN2( I )

*  Centroid this pair of positions.
         IF ( TYPE1 .EQ. '_UBYTE' ) THEN
            CALL CCG1_CENUB( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                       NCOL1, NLINE1, ISIZE, SIGN, MAXSHF,
     :                       MAXIT, TOLER, XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_BYTE' ) THEN
            CALL CCG1_CENB( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_UWORD' ) THEN
            CALL CCG1_CENUW( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                       NCOL1, NLINE1, ISIZE, SIGN, MAXSHF,
     :                       MAXIT, TOLER, XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_WORD' ) THEN
            CALL CCG1_CENW( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_INTEGER' ) THEN
            CALL CCG1_CENI( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_REAL' ) THEN
            CALL CCG1_CENR( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         ELSE IF ( TYPE1 .EQ. '_INT64' ) THEN
            CALL CCG1_CENK( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         ELSE
            CALL CCG1_CEND( XPOS1, YPOS1, %VAL( CNF_PVAL( IPDAT1 ) ),
     :                      NCOL1, NLINE1,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC1, YACC1, STATUS )
         END IF

*  Now for second set.
         IF ( TYPE2 .EQ. '_UBYTE' ) THEN
            CALL CCG1_CENUB( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                       NCOL2, NLINE2, ISIZE, SIGN, MAXSHF,
     :                       MAXIT, TOLER, XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_BYTE' ) THEN
            CALL CCG1_CENB( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_UWORD' ) THEN
            CALL CCG1_CENUW( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                       NCOL2, NLINE2, ISIZE, SIGN, MAXSHF,
     :                       MAXIT, TOLER, XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_WORD' ) THEN
            CALL CCG1_CENW( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_INTEGER' ) THEN
            CALL CCG1_CENI( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_REAL' ) THEN
            CALL CCG1_CENR( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         ELSE IF ( TYPE2 .EQ. '_INT64' ) THEN
            CALL CCG1_CENK( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         ELSE
            CALL CCG1_CEND( XPOS2, YPOS2, %VAL( CNF_PVAL( IPDAT2 ) ),
     :                      NCOL2, NLINE2,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC2, YACC2, STATUS )
         END IF

*  Check status. If this is set then no values have been found for this
*  position pair- just annul the error and continue.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Both positions centroided successfully increment output count.
            NOUT = NOUT + 1

*  Set the output positions, correcting for origin information
            XOUT1( NOUT ) = XACC1 + LBND1( 1 ) - 1
            YOUT1( NOUT ) = YACC1 + LBND1( 2 ) - 1
            XOUT2( NOUT ) = XACC2 + LBND2( 1 ) - 1
            YOUT2( NOUT ) = YACC2 + LBND2( 2 ) - 1
         END IF
 1    CONTINUE
      CALL ERR_RLSE

*  Check that some centroids have been found.
      IF ( NOUT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_CEN2_NONE',
     : '  No image feature centroids have been located', STATUS )
      END IF

 99   CONTINUE
      END
* $Id$
