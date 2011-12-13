      SUBROUTINE TRN1_TRDTD( BAD, ND1, NVIN, DATA, NDAT, CTTI, SLOT,
     :                         NR1, NVOUT, RESULT, STATUS )
*+
*  Name:
*     TRN1_TRDTD

*  Purpose:
*     transform DOUBLE PRECISION data.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_TRDTD( BAD, ND1, NVIN, DATA, NDAT, CTTI, SLOT,
*                        NR1, NVOUT, RESULT, STATUS )

*  Description:
*     The routine applies a compiled transformation to a coordinate list
*     for a set of DOUBLE PRECISION data points by splitting the list into a
*     sequence of batches.  It also attempts to optimise the use of
*     workspace, obtaining extra workspace if it cannot function
*     efficiently within that allocated at startup.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     1-MAR-1988:  Added extra workspace allocation and batch handling
*        (DUVAD::RFWS)
*     12-MAY-1988:  Converted to generic code and implemented precision
*        handling (DUVAD::RFWS)
*     17-AUG-1988:  Added "bad value" handling (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Arguments Given:
      LOGICAL BAD               ! Whether input DATA values may be "bad"
      INTEGER ND1               ! First dimension of the DATA array
      INTEGER NVIN              ! Number of variables (coordinates) for
                                ! each input data point
      DOUBLE PRECISION DATA( ND1, NVIN )  ! Array containing sets of coordinate
                                ! values for each input data point
      INTEGER NDAT              ! Number of data points to transform
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
                                ! Compiled transformation table (CTT)
                                ! integer array
      INTEGER SLOT              ! CTT slot for compiled transformation
      INTEGER NR1               ! First dimension of the RESULT array
      INTEGER NVOUT             ! Number of variables (coordinates) for
                                ! each output data point


*  Arguments Given and Returned:
      DOUBLE PRECISION RESULT( NR1, NVOUT )
                                ! Array to contain the transformed
                                ! coordinates for each output data point


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL EXTRA             ! Whether extra workspace was allocated
      INTEGER MXDAT             ! Maximum number of data points which
                                ! can be processed in one batch
      INTEGER NWRK              ! Amount of workspace preferred
      INTEGER PWRKI             ! Pointer to mapped INTEGER workspace
                                ! array
      INTEGER PWRKR             ! Pointer to mapped REAL workspace
                                ! array
      INTEGER PWRKD             ! Pointer to mapped DOUBLE PRECISION
                                ! workspace array
      INTEGER ISTART            ! First data point in a batch
      INTEGER IEND              ! Last data point in a batch
      INTEGER NPROC             ! Number of data points in a batch
      CHARACTER * ( DAT__SZLOC ) LOCWRK
                                ! Locator to temporary structure
                                ! containing mapped workspace arrays


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Calculate the maximum number of data points which can be transformed
*   at once using the workspace allocated at startup.  The routine tries
*   to process the data in batches which fit into this workspace to
*   avoid the overhead of allocating space for every set of data points
*   transformed.
      MXDAT = TRN__NWRK / MAX( CTTI( TRN_CT_MXWRK, SLOT ), 1 )


*   If the maximum batch size is too small for efficiency and is less
*   the the number of data points being transformed, then it is
*   worth allocating extra workspace.  Find the preferred number of
*   data points to process in each batch.
      EXTRA = .FALSE.
      IF( MXDAT .LT. MIN( NDAT, TRN__MNBAT ) ) THEN
        MXDAT = MIN( NDAT, TRN__MNBAT )


*   Calculate the workspace requirement and allocate it, acquiring
*   pointers to mapped workspace arrays for each data type.
        NWRK = MXDAT * CTTI( TRN_CT_MXWRK, SLOT )
        CALL TRN1_ALWRK( NWRK, PWRKI, PWRKR, PWRKD, LOCWRK, STATUS )


*   Note if extra workspace has been obtained.
        EXTRA = ( STATUS .EQ. SAI__OK )


*   If no extra workspace is required, use the normal allocation and
*   obtain pointers to it from the common block.
      ELSE
        PWRKI = TRN_PWRKI
        PWRKR = TRN_PWRKR
        PWRKD = TRN_PWRKD
      ENDIF


*   Loop to process the data in batches.
      ISTART = 1
      DO WHILE( ( ISTART .LE. NDAT) .AND. ( STATUS .EQ. SAI__OK ) )


*   Find the range and total number of data points in each batch.
        IEND = MIN( ISTART + MXDAT - 1, NDAT )
        NPROC = IEND - ISTART + 1


*   Pass the components of the compiled module list to TRN1_TRBTD
*   which evaluates the transformation for this batch of data points.
        CALL TRN1_TRBTD( BAD, ND1, NVIN, DATA, ISTART, NPROC,
     :                   CTTI( TRN_CT_NMOD, SLOT ),
     :                   %VAL( CNF_PVAL( CTTI( TRN_CT_PPIND, SLOT ) ) ),
     :                   %VAL( CNF_PVAL( CTTI( TRN_CT_PNVAR, SLOT ) ) ),
     :                   %VAL( CNF_PVAL( CTTI( TRN_CT_PIPRC, SLOT ) ) ),
     :                   CTTI( TRN_CT_MXWRK, SLOT ),
     :                   NR1, NVOUT, RESULT,
     :                   %VAL( CNF_PVAL( PWRKI ) ),
     :                   %VAL( CNF_PVAL( PWRKR ) ),
     :                   %VAL( CNF_PVAL( PWRKD ) ), STATUS )

*   Increment pointer to the start of the next batch.
        ISTART = IEND + 1


*   End of "loop to process the data in batches" loop.
      ENDDO


*   If extra workspace was allocated, release it.
      IF( EXTRA ) CALL TRN1_RELTS( 1, LOCWRK, STATUS )


*   Exit routine.
      END


