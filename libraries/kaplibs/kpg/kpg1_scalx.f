      SUBROUTINE KPG1_SCALX( SCALE, ZERO, BAD, EL, ITYPE, IP1, OTYPE,
     :                       IP2, BADOUT, NBAD, STATUS )
*+
*  Name:
*     KPG1_SCALX

*  Purpose:
*     Copies array values into another array, scaling them in the process.

*  Language:
*     Starlink Fortran 77

*  Invocation
*     CALL KPG1_SCALX( SCALE, ZERO, BAD, EL, ITYPE, IP1, OTYPE,
*                      IP2, BADOUT, NBAD, STATUS )

*  Description:
*     This routine copies values from one array to another, applying a
*     linear scaling in the process. The input and output arrays can be
*     of different data types.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero offset.
*     BAD = LOGICAL (Given)
*        If true there may be bad pixels present in the input array. If
*        false it is safe not to check for bad values.
*     EL = INTEGER (Given)
*        The size of the input array.
*     ITYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the input array. All primitive HDS data types
*        are supported.
*     IP1 = INTEGER (Given)
*        Pointer to the input array.
*     OTYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the output array. Must be an integer type
*        (i.e. one of _INTEGER, _WORD, _UWORD, _BYTE  or _UBYTE ).
*     IP2 = INTEGER (Given)
*        Pointer to the output array.
*     BADOUT = LOGICAL (Returned)
*        True if there are any bad pixels in the output array.
*     NBAD = INTEGER (Returned)
*        The number of good input pixels that could not be accomodated
*        within the dynamic range of the output data type, and were
*        consequently set bad in the output array.
*     STATUS = INTEGER  (Given and Returned)
*        Global status value

*  Notes:
*     -  This function simply wraps up the generic KPG1_SCL<T><T> routines.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE

*  Global Constants:
      INCLUDE  'SAE_PAR'
      INCLUDE  'CNF_PAR'

*  Arguments Given:
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL BAD
      INTEGER EL
      CHARACTER ITYPE*(*)
      INTEGER IP1
      CHARACTER OTYPE*(*)
      INTEGER IP2

*  Arguments Returned:
      LOGICAL BADOUT
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION VAL_RTOD
      DOUBLE PRECISION VAL_ITOD
      DOUBLE PRECISION VAL_WTOD
      DOUBLE PRECISION VAL_UWTOD
      DOUBLE PRECISION VAL_BTOD
      DOUBLE PRECISION VAL_UBTOD

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deal with cases where we are creating an _INTEGER output array.
      IF( OTYPE .EQ. '_INTEGER' ) THEN

*  Go through each type of input array.
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SCLID( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SCLIR( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_SCLII( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_SCLIW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_SCLIUW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_SCLIB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_SCLIUB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                    'input data type ''^T'' supplied.', STATUS )
         END IF


*  Deal with cases where we are creating an _WORD output array.
      ELSE IF( OTYPE .EQ. '_WORD' ) THEN

*  Go through each type of input array.
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SCLWD( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SCLWR( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_SCLWI( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_SCLWW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_SCLWUW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_SCLWB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_SCLWUB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                    'input data type ''^T'' supplied.', STATUS )
         END IF


*  Deal with cases where we are creating an _UWORD output array.
      ELSE IF( OTYPE .EQ. '_UWORD' ) THEN

*  Go through each type of input array.
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SCLUWD( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SCLUWR( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_SCLUWI( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_SCLUWW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_SCLUWUW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_SCLUWB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_SCLUWUB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                    'input data type ''^T'' supplied.', STATUS )
         END IF


*  Deal with cases where we are creating a _BYTE output array.
      ELSE IF( OTYPE .EQ. '_BYTE' ) THEN

*  Go through each type of input array.
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SCLBD( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SCLBR( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_SCLBI( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_SCLBW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_SCLBUW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_SCLBB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_SCLBUB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                    'input data type ''^T'' supplied.', STATUS )
         END IF


*  Deal with cases where we are creating a _UBYTE output array.
      ELSE IF( OTYPE .EQ. '_UBYTE' ) THEN

*  Go through each type of input array.
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_SCLUBD( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_SCLUBR( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_SCLUBI( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_SCLUBW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_SCLUBUW( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_SCLUBB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_SCLUBUB( SCALE, ZERO, BAD, EL,
     :                       %VAL( CNF_PVAL( IP1 ) ),
     :                       %VAL( CNF_PVAL( IP2 ) ), BADOUT, NBAD,
     :                       STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                    'input data type ''^T'' supplied.', STATUS )
         END IF


*  Report an error if the output data type is not supported.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', OTYPE )
         CALL ERR_REP( 'KPG1_SCALX_ERR', 'KPG1_SCALX: Unsupported '//
     :                 'output data type ''^T'' supplied.', STATUS )
      END IF

      END
