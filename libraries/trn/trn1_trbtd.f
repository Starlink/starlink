      SUBROUTINE TRN1_TRBTD( BAD, ND1, NVIN, DATA, ISTART, NPROC,
     :                         NMOD, PINDEX, NVAR, IPRC, MXWRK, NR1,
     :                         NVOUT, RESULT, WRKI, WRKR, WRKD, STATUS )
*+
*  Name:
*     TRN1_TRBTD

*  Purpose:
*     apply a transformation to a batch of DOUBLE PRECISION data.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_TRBTD( BAD, ND1, NVIN, DATA, ISTART, NPROC,
*                        NMOD, PINDEX, NVAR, IPRC, MXWRK, NR1,
*                        NVOUT, RESULT, WRKI, WRKR, WRKD, STATUS )

*  Description:
*     The routine evaluates the effect of a compiled transformation
*     (which may contain more than one module) in transforming a
*     coordinate list for a subset (batch) of DOUBLE PRECISION data points.
*     Processing the data in batches with an adjustable size in this
*     way allows flexible use of a fixed amount of workspace.

*  Implementation Deficiencies:
*     - Doesn't keep track of numerical errors (pointer, count & status
*       values).
*     - Doesn't yet implement implicit data type conversion fully - the
*       present arrangement causes an entire transformation to be
*       evaluated with one arithmetic precision (like the SCAR package).
*       This should eventually change to resemble the way the ADAM ICL
*       command language handles data types and arithmetic precision.

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
*     1-MAR-1988: Renamed and moved batch size calculation and
*        workspace allocation to calling routine (DUVAD::RFWS)
*     9-MAY-1988:  Added precision handling and converted to generic
*        code.  Changed name again. (DUVAD::RFWS)
*     17-AUG-1988:  Added "bad value" handling (DUVAD::RFWS)
*     2-DEC-1988:  Fixed bug in precision code calculation (DUVAD::RFWS)
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
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      LOGICAL BAD               ! Whether input DATA values may be "bad"
      INTEGER ND1               ! First dimension of DATA array
      INTEGER NVIN              ! Number of variables (coordinates) for
                                ! each input data point
      DOUBLE PRECISION DATA( ND1, NVIN )  ! Array containing coordinate values for
                                ! each input data point
      INTEGER ISTART            ! Location of the first set of
                                ! coordinates to be processed in the
                                ! DATA array
      INTEGER NPROC             ! Number of sets of data coordinates to
                                ! process
      INTEGER NMOD              ! Number of modules in the compiled
                                ! transformation
      INTEGER PINDEX( NMOD )    ! List of pointers to compiled module
                                ! mapped index arrays
      INTEGER NVAR( NMOD + 1 )  ! Number of variables in the data
                                ! stream between each pair of modules
      INTEGER IPRC( NMOD )      ! Precision code for each compiled
                                ! module
      INTEGER MXWRK             ! Workspace requirement for the whole
                                ! transformation per data point
      INTEGER NR1               ! First dimension of the RESULT array
      INTEGER NVOUT             ! Number of variables (coordinates) for
                                ! each output data point


*  Arguments Given and Returned:
      DOUBLE PRECISION RESULT( NR1, NVOUT )
                                ! Array to receive transformed
                                ! coordinates for the data points which
                                ! are being processed - other elements
                                ! of this array are unchanged


*  Arguments Returned:
      INTEGER WRKI( NPROC, MXWRK )
                                ! Workspace array for INTEGER data
      REAL WRKR( NPROC, MXWRK ) ! Workspace array for REAL data
      DOUBLE PRECISION WRKD( NPROC, MXWRK )
                                ! Workspace array for DOUBLE PRECISION
                                ! data


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL BADL              ! Local bad data flag
      INTEGER IVAR              ! Loop counter for variables
      INTEGER IMOD              ! Loop counter for modules
      INTEGER PRICOD            ! Data precision code for the current
                                ! module
      INTEGER PRLAST            ! Data precision code for the previous
                                ! module
      INTEGER IERRL             ! Local numerical error pointer
      INTEGER NERRL             ! Local numerical error count
      INTEGER NSTATL            ! Local numerical error status variable


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise the local bad data flag.
      BADL = BAD


*   Check that the numbers of data input and output variables match
*   the corresponding numbers of variables for the compiled
*   transformation.  Report an error if necessary.
      IF( ( NVIN .NE. NVAR( 1 ) ) .OR.
     :    ( NVOUT .NE. NVAR( NMOD + 1 ) ) ) THEN
        STATUS = TRN__NDCMM     ! number of data coordinates mis-matched
        CALL TRN1_ERROR( 'TRN1_TRBTD', ' ', STATUS )


*   If there is no error, find the data precision for the first module.
*   This is generated by combining the data type being processed and the
*   precision specified for the module itself.
      ELSE
        PRICOD = MAX( TRN_PR_ED, IPRC( 1 ) )
        IF( PRICOD .LT. TRN_PR_I ) PRICOD = PRICOD +
     :                                      ( TRN_PR_I - TRN_PR_EI )


*   Initialise the local numerical error status variable and copy the
*   input data for the batch into the upper NVIN vectors of the
*   workspace array whose type matches the data precision to be used
*   for the first module.  Perform type conversion if necessary.
          NSTATL = SAI__OK

*   ...convert to integer:
        IF( PRICOD .EQ. TRN_PR_I ) THEN
          DO IVAR = 1, NVIN
            CALL VEC_DTOI( BADL, NPROC, DATA( ISTART, IVAR ),
     :                       WRKI( 1, MXWRK - NVIN + IVAR ),
     :                       IERRL, NERRL, NSTATL )
          ENDDO

*   ...convert to real:
        ELSE IF( PRICOD .EQ. TRN_PR_R ) THEN
          DO IVAR = 1, NVIN
            CALL VEC_DTOR( BADL, NPROC, DATA( ISTART, IVAR ),
     :                       WRKR( 1, MXWRK - NVIN + IVAR ),
     :                       IERRL, NERRL, NSTATL )
          ENDDO

*   ...convert to double precision:
        ELSE IF( PRICOD .EQ. TRN_PR_D ) THEN
          DO IVAR = 1, NVIN
            CALL VEC_DTOD( BADL, NPROC, DATA( ISTART, IVAR ),
     :                       WRKD( 1, MXWRK - NVIN + IVAR ),
     :                       IERRL, NERRL, NSTATL )
          ENDDO
        ENDIF


*   If a numerical error occurred, ensure the local bad data flag is
*   set.
        IF( NSTATL .NE. SAI__OK ) BADL = .TRUE.


*   Loop to evaluate the transformation for each module.
        IMOD = 0
        DO WHILE( ( IMOD .LT. NMOD ) .AND. ( STATUS .EQ. SAI__OK ) )
          IMOD = IMOD + 1


*   Except on the first loop, calculate a new data precision for the
*   current module.  This is generated by combining the precision used
*   for the previous module with that specified for the current one.
          IF( IMOD .NE. 1 ) THEN
            PRICOD = MAX( PRLAST - ( TRN_PR_I - TRN_PR_EI ),
     :                    IPRC( IMOD ) )
            IF( PRICOD .LT. TRN_PR_I ) PRICOD = PRICOD +
     :                                          ( TRN_PR_I - TRN_PR_EI )


*   Re-initialise the local numerical error status variable and move
*   the results from evaluation of the previous module [in the lower
*   NVAR( IMOD ) vectors of the workspace array] into the upper part.
*   The data are moved between workspace arrays with type conversion if
*   the precision changes between modules.
            NSTATL = SAI__OK

*   ...convert from integer:
            IF( PRLAST .EQ. TRN_PR_I ) THEN

*      ...to integer:
              IF( PRICOD .EQ. TRN_PR_I ) THEN
                CALL VEC_ITOI( BADL, NPROC * NVAR( IMOD ), WRKI,
     :                         WRKI( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to real:
              ELSE IF( PRICOD .EQ. TRN_PR_R ) THEN
                CALL VEC_ITOR( BADL, NPROC * NVAR( IMOD ), WRKI,
     :                         WRKR( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to double precision:
              ELSE IF( PRICOD .EQ. TRN_PR_D ) THEN
                CALL VEC_ITOD( BADL, NPROC * NVAR( IMOD ), WRKI,
     :                         WRKD( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )
              ENDIF

*   ...convert from real:
            ELSE IF( PRLAST .EQ. TRN_PR_R ) THEN

*      ...to integer:
              IF( PRICOD .EQ. TRN_PR_I ) THEN
                CALL VEC_RTOI( BADL, NPROC * NVAR( IMOD ), WRKR,
     :                         WRKI( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to real:
              ELSE IF( PRICOD .EQ. TRN_PR_R ) THEN
                CALL VEC_RTOR( BADL, NPROC * NVAR( IMOD ), WRKR,
     :                         WRKR( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to double precision:
              ELSE IF( PRICOD .EQ. TRN_PR_D ) THEN
                CALL VEC_RTOD( BADL, NPROC * NVAR( IMOD ), WRKR,
     :                         WRKD( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )
              ENDIF

*   ...convert from double precision:
            ELSE IF( PRLAST .EQ. TRN_PR_D ) THEN

*      ...to integer:
              IF( PRICOD .EQ. TRN_PR_I ) THEN
                CALL VEC_DTOI( BADL, NPROC * NVAR( IMOD ), WRKD,
     :                         WRKI( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to real:
              ELSE IF( PRICOD .EQ. TRN_PR_R ) THEN
                CALL VEC_DTOR( BADL, NPROC * NVAR( IMOD ), WRKD,
     :                         WRKR( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )

*      ...to double precision:
              ELSE IF( PRICOD .EQ. TRN_PR_D ) THEN
                CALL VEC_DTOD( BADL, NPROC * NVAR( IMOD ), WRKD,
     :                         WRKD( 1, MXWRK - NVAR( IMOD ) + 1 ),
     :                         IERRL, NERRL, NSTATL )
              ENDIF
            ENDIF


*   If a numerical error occurred, ensure the local bad data flag is
*   set.
            IF( NSTATL .NE. SAI__OK ) BADL = .TRUE.


*   End of "this is not the first loop" condition.
          ENDIF


*   Perform the transformation for the next transformation module on the
*   data in the upper part of the WRKx arrays.  The results are returned
*   to the lower part - the region between is used as workspace.  Call
*   a different routine for each precision.

*   ...integer:
          IF( PRICOD .EQ. TRN_PR_I ) THEN
            CALL TRN1_EVTMI( BADL, NPROC, NVAR( IMOD ),
     :                       WRKI( 1, MXWRK - NVAR( IMOD ) + 1 ), NPROC,
     :                       %VAL( CNF_PVAL( PINDEX( IMOD ) ) ),
     :                       NVAR( IMOD + 1 ),
     :                       NPROC, MXWRK - NVAR( IMOD ), WRKI,
     :                       IERRL, NERRL, NSTATL, STATUS )

*   ...real:
          ELSE IF( PRICOD .EQ. TRN_PR_R ) THEN
            CALL TRN1_EVTMR( BADL, NPROC, NVAR( IMOD ),
     :                       WRKR( 1, MXWRK - NVAR( IMOD ) + 1 ), NPROC,
     :                       %VAL( CNF_PVAL( PINDEX( IMOD ) ) ),
     :                       NVAR( IMOD + 1 ),
     :                       NPROC, MXWRK - NVAR( IMOD ), WRKR,
     :                       IERRL, NERRL, NSTATL, STATUS )

*   ...double precision:
          ELSE IF( PRICOD .EQ. TRN_PR_D ) THEN
            CALL TRN1_EVTMD( BADL, NPROC, NVAR( IMOD ),
     :                       WRKD( 1, MXWRK - NVAR( IMOD ) + 1 ), NPROC,
     :                       %VAL( CNF_PVAL( PINDEX( IMOD ) ) ),
     :                       NVAR( IMOD + 1 ),
     :                       NPROC, MXWRK - NVAR( IMOD ), WRKD,
     :                       IERRL, NERRL, NSTATL, STATUS )
          ENDIF


*   If a numerical error occurred, ensure the local bad data flag is
*   set.
          IF( NSTATL .NE. SAI__OK ) BADL = .TRUE.


*   Remember the data precision used in this module.
          PRLAST = PRICOD


*   End of "loop to evaluate the transformation for each module" loop.
        ENDDO


*   If there is no error, move the results (in the lower NVOUT vectors
*   of the WRKx array) into the appropriate locations in the output
*   RESULT array.  Perform type conversion if required.
        IF( STATUS .EQ. SAI__OK ) THEN

*   ...convert to integer:
          IF( PRLAST .EQ. TRN_PR_I ) THEN
            DO IVAR = 1, NVOUT
              NSTATL = SAI__OK
              CALL VEC_ITOD( BADL, NPROC, WRKI( 1, IVAR ),
     :                         RESULT( ISTART, IVAR ),
     :                         IERRL, NERRL, NSTATL )
            ENDDO

*   ...convert to real:
          ELSE IF( PRLAST .EQ. TRN_PR_R ) THEN
            DO IVAR = 1, NVOUT
              NSTATL = SAI__OK
              CALL VEC_RTOD( BADL, NPROC, WRKR( 1, IVAR ),
     :                         RESULT( ISTART, IVAR ),
     :                         IERRL, NERRL, NSTATL )
            ENDDO

*   ...covert to double precision:
          ELSE IF( PRLAST .EQ. TRN_PR_D ) THEN
            DO IVAR = 1, NVOUT
              NSTATL = SAI__OK
              CALL VEC_DTOD( BADL, NPROC, WRKD( 1, IVAR ),
     :                         RESULT( ISTART, IVAR ),
     :                         IERRL, NERRL, NSTATL )
            ENDDO
          ENDIF
        ENDIF


*   End of "the numbers of data variables match" condition.
      ENDIF


*   Exit routine.
      END


