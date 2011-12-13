      SUBROUTINE KPG1_MXMNX( TYPE, BAD, EL, IPNTR, NINVAL, MAXMUM,
     :                       MINMUM, MAXPOS, MINPOS, STATUS )
*+
*  Name:
*     KPG1_MXMNX

*  Purpose:
*     Returns the maximum and minimum values of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation
*      CALL KPG1_MXMNX( TYPE, BAD, EL, IPNTR, NINVAL, MAXMUM, MINMUM,
*                       MAXPOS, MINPOS, STATUS )

*  Description:
*     This routine returns the maximum and minimum values of an input
*     array, where it found the maximum and minimum, and the number of
*     good and bad pixels in the array.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the array.
*     BAD = LOGICAL (Given)
*        If true there may be bad pixels present in the array.  If false
*        it is safe not to check for bad values.
*     EL = INTEGER (Given)
*        The dimension of the input array.
*     IPNTR = INTEGER (Given)
*        A pointer to the data array.
*     NINVAL = INTEGER (Returned)
*        Number of bad pixels in the array.
*     MAXMUM = DOUBLE PRECISION (Returned)
*        Maximum value found in the array.
*     MINMUM = DOUBLE PRECISION (Returned)
*        Minimum value found in the array.
*     MAXPOS = INTEGER (Returned)
*        Index of the pixel where the maximum value is (first) found.
*     MINPOS = INTEGER (Returned)
*        Index of the pixel where the minimum value is (first) found.
*     STATUS = INTEGER  (Given)
*        Global status value

*  Notes:
*     -  This function simply wraps up the generic KPG1_MXMN<T> routines.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     11-JUL-2006 (DSB):
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
      CHARACTER TYPE*(*)
      LOGICAL BAD
      INTEGER EL
      INTEGER IPNTR

*  Arguments Returned:
      INTEGER NINVAL
      DOUBLE PRECISION MAXMUM
      DOUBLE PRECISION MINMUM
      INTEGER MAXPOS
      INTEGER MINPOS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION VAL_RTOD
      DOUBLE PRECISION VAL_ITOD
      DOUBLE PRECISION VAL_WTOD
      DOUBLE PRECISION VAL_UWTOD
      DOUBLE PRECISION VAL_BTOD
      DOUBLE PRECISION VAL_UBTOD

*  Local Variables:
      BYTE BMN                   ! Min value
      BYTE BMX                   ! Max value
      INTEGER IMN                ! Min value
      INTEGER IMX                ! Max value
      INTEGER*2 WMN              ! Min value
      INTEGER*2 WMX              ! Max value
      REAL RMN                   ! Min value
      REAL RMX                   ! Max value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke the correct version of the generic KPG1_MXMN<T> routine.
      IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_MXMND( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    MAXMUM, MINMUM, MAXPOS, MINPOS, STATUS )

      ELSE IF( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_MXMNR( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    RMX, RMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_RTOD( .TRUE., RMX, STATUS )
         MINMUM = VAL_RTOD( .TRUE., RMN, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_MXMNI( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    IMX, IMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_ITOD( .TRUE., IMX, STATUS )
         MINMUM = VAL_ITOD( .TRUE., IMN, STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_MXMNW( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    WMX, WMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_WTOD( .TRUE., WMX, STATUS )
         MINMUM = VAL_WTOD( .TRUE., WMN, STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_MXMNUW( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                     WMX, WMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_UWTOD( .TRUE., WMX, STATUS )
         MINMUM = VAL_UWTOD( .TRUE., WMN, STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_MXMNB( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    BMX, BMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_BTOD( .TRUE., BMX, STATUS )
         MINMUM = VAL_BTOD( .TRUE., BMN, STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_MXMNUB( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ), NINVAL,
     :                    BMX, BMN, MAXPOS, MINPOS, STATUS )
         MAXMUM = VAL_UBTOD( .TRUE., BMX, STATUS )
         MINMUM = VAL_UBTOD( .TRUE., BMN, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'KPG1_MXMNX_ERR', 'KPG1_MXMNX: Unsupported '//
     :                 'data type ''^T'' supplied.', STATUS )
      END IF

      END
