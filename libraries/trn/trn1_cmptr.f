      SUBROUTINE TRN1_CMPTR( LOCTR, FORWD, SZCML, IFIRST, NMOD, LOCCM,
     :                       PINDX, NVAR, IPRC, MXWRK, CLASS, STATUS )








*+
*  Name:
*     TRN1_CMPTR

*  Purpose:
*     compile a transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CMPTR( LOCTR, FORWD, SZCML, IFIRST, NMOD, LOCCM,
*                      PINDX, NVAR, IPRC, MXWRK, CLASS, STATUS )

*  Description:
*     The routine compiles a transformation passed by HDS locator and
*     containing a sequence of transformation modules.  The compiled
*     modules in this transformation are added to the end of an
*     existing compiled module list (CML), or are used to start a new
*     one.

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
*     4-FEB-1988: Original version called TRN1_ADDTR (DUVAD::RFWS)
*     5-FEB-1988: Added FLAGS handling (DUVAD::RFWS)
*     9-FEB-1988: Renamed as TRN1_CMPTR (DUVAD::RFWS)
*     12-FEB-1988:  Added error handling (DUVAD::RFWS)
*     9-MAY-1988:  Added classification array handling (FLAGS handling
*        removed) (DUVAD::RFWS)
*     9-MAY-1988:  Added precision handling (DUVAD::RFWS)
*     18-MAY-1988:  Changed argument order and added SZCML (DUVAD::RFWS)
*     2-JUN-1988:  Added protection against undefined IOUT value under
*        error conditions (DUVAD::RFWS)
*     28-DEC-1988:  Fixed bug causing modules to compile in wrong order
*        for inverse mapping (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure
      LOGICAL FORWD             ! Select forward/inverse transformation
      INTEGER SZCML             ! Current size of the compiled module
                                ! list arrays
      INTEGER IFIRST            ! First element in the compiled module
                                ! list arrays to use for output
      INTEGER NMOD              ! Number of modules to compile


*  Arguments Given and Returned:
      CHARACTER * ( * ) LOCCM( SZCML )
                                ! Locators to compiled module temporary
                                ! structures
      INTEGER PINDX( SZCML )    ! Pointers to compiled module mapped
                                ! index arrays
      INTEGER NVAR( SZCML + 1 )
                                ! Number of variables in the data
                                ! stream between modules
      INTEGER IPRC( SZCML )
                                ! Compiled module precision codes
      INTEGER MXWRK             ! Workspace requirement per data point
                                ! for the whole transformation
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array for the whole
                                ! transformation


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL TRCLS( TRN__MXCLS )
                                ! Classification array for the new
                                ! transformation
      LOGICAL CBCLS( TRN__MXCLS )
                                ! Classification array for the new
                                ! transformation combined with any
                                ! existing transformation in the CML
      INTEGER IMOD              ! Loop counter for new modules
      INTEGER MCELL             ! Number of the MODULE_ARRAY cell
                                ! containing the next module
      INTEGER IOUT              ! Next output compiled module list
                                ! element to use
      INTEGER NVIN              ! Number of input variables for a module
      INTEGER NVOUT             ! Number of output variables for a
                                ! module
      INTEGER NWRK              ! Module workspace requirements
      INTEGER ICLS              ! Loop counter for indexing
                                ! classification arrays
      INTEGER DFOR              ! Forward definition status for the
                                ! transformation
      INTEGER DINV              ! Inverse definition status for the
                                ! transformation
      INTEGER TSTAT             ! Temporary status variable
      CHARACTER * ( DAT__SZLOC ) LOCMA
                                ! Locator to MODULE_ARRAY component
      CHARACTER * ( DAT__SZLOC ) LOCTM
                                ! Locator to cell of MODULE_ARRAY
                                ! component


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Obtain the definition status information.  If there is no error,
*   check that the required mapping is defined.  Report an error if it
*   is not.
      CALL TRN1_RDDST( LOCTR, DFOR, DINV, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( ( FORWD .AND. ( DFOR .NE. TRN_DS_DEF ) ) .OR.
     :      ( ( .NOT. FORWD ) .AND. ( DINV .NE. TRN_DS_DEF ) ) ) THEN
          STATUS = TRN__MAPUD   ! mapping undefined
          CALL TRN1_ERRL( 'TRN1_CMPTR', LOCTR, STATUS )
        ENDIF
      ENDIF


*   Locate the MODULE_ARRAY component.
      CALL DAT_FIND( LOCTR, 'MODULE_ARRAY', LOCMA, STATUS )


*   Loop to compile each module in turn, unless an error is detected.
      IMOD = 0
      DO WHILE ( ( IMOD .LT. NMOD ) .AND. ( STATUS .EQ. SAI__OK ) )
        IMOD = IMOD + 1


*   Calculate which MODULE_ARRAY cell to compile next, working forwards
*   through the array if the forward mapping is being compiled and
*   backwards if the inverse mapping is being compiled.
        IF( FORWD ) THEN
          MCELL = IMOD
        ELSE
          MCELL = NMOD - IMOD + 1
        ENDIF


*   Obtain a locator to the required cell of the MODULE_ARRAY component
*   and compile the transformation module therein.  Put the results
*   obtained into the appropriate element of the output arrays.
        CALL DAT_CELL( LOCMA, 1, MCELL, LOCTM, STATUS )
        IOUT = IFIRST + IMOD - 1
        CALL TRN1_CMPTM( LOCTM, FORWD, LOCCM( IOUT ), PINDX( IOUT ),
     :                   NVIN, NVOUT, IPRC( IOUT ), NWRK, STATUS )


*   If there is no error, then unless this is the first module to be
*   compiled, check the number of input variables matches the number of
*   output variables from the previous module.  Report an error if it
*   does not.
        IF( STATUS .EQ. SAI__OK ) THEN
          IF( IOUT .GT. 1 ) THEN
            IF( NVIN .NE. NVAR( IOUT ) ) THEN
              STATUS = TRN__NMVMM       ! number of module variables
                                        ! mis-matched
              CALL TRN1_ERRL( 'TRN1_CMPTR', LOCTM, STATUS )
            ENDIF
          ENDIF
        ENDIF


*   If there is no error, store the numbers of variables information in
*   the appropriate elements of the output array.
        IF( STATUS .EQ. SAI__OK ) THEN
          NVAR( IOUT ) = NVIN
          NVAR( IOUT + 1 ) = NVOUT


*   Calculate the maximum workspace requirement of all the compiled
*   modules so far.
          MXWRK = MAX( MXWRK, NWRK )
        ENDIF


*   Annul the module locator.
        CALL DAT_ANNUL( LOCTM, STATUS )


*   End of "loop to compile each module in turn" loop.
      ENDDO


*   Obtain the transformation classification array.
      CALL TRN1_RDCLS( LOCTR, TRCLS, STATUS )


*   If there is no error, obtain the number of input/output variables
*   for the transformation from the information produced during
*   compilation.
      IF( STATUS .EQ. SAI__OK ) THEN
        NVIN = NVAR( IFIRST )
        NVOUT = NVAR( IOUT + 1 )


*   Validate the classification information.  If there is an error,
*   make a new error report citing the offending data structure.
        CALL ERR_MARK
        CALL TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, TRCLS, STATUS )
        IF( STATUS .NE. SAI__OK ) THEN
          TSTAT = STATUS
          CALL ERR_ANNUL( TSTAT )
          CALL TRN1_ERRC( 'TRN1_CMPTR', LOCTR, 'CLASSIFICATION',
     :                    STATUS )
        ENDIF
        CALL ERR_RLSE
      ENDIF


*   Combine the classification with the number of input/output variables
*   and then with the classification information for any transformation
*   which already exists in the CML.
      CALL TRN1_NVCLS( NVAR( IFIRST ), NVAR( IOUT + 1 ), TRCLS, STATUS )
      CALL TRN1_CBCLS( CLASS, TRCLS, CBCLS, STATUS )


*   If there is no error, return the new classification information.
      IF( STATUS .EQ. SAI__OK ) THEN
        DO ICLS = 1, TRN__MXCLS
          CLASS( ICLS ) = CBCLS( ICLS )
        ENDDO


*   If an error was detected, release the resources associated with
*   any compiled transformation modules which were generated prior to
*   the error.
      ELSE
        IF( IMOD .GT. 0 )
     :    CALL TRN1_RELTS( IOUT - IFIRST + 1, LOCCM( IFIRST ), STATUS )
      ENDIF


*   Exit routine.
      END
