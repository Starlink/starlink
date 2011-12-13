      SUBROUTINE SETMAGIC ( STATUS )
*+
*  Name:
*     SETMAGIC

*  Purpose:
*     Replaces all occurrences of a given value in an NDF array with
*     the bad value.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETMAGIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application flags all pixels that have a defined value in an
*     NDF with the standard bad (`magic') value.  Other values are
*     unchanged.  The number of replacements is reported.  SETMAGIC's
*     applications include the import of data from software that has a
*     different magic value.

*  Usage:
*     setmagic in out repval [comp]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The components whose values are to be flagged as bad.  It may
*        be "Data", "Error", "Variance", or "All".  The last of the
*        options forces substitution of bad pixels in both the data and
*        variance arrays.  This parameter is ignored if the data array
*        is the only array component within the NDF.  ["Data"]
*     IN = NDF  (Read)
*        Input NDF structure containing the data and/or variance array
*        to have some of its elements flagged with the magic-value.
*     OUT = NDF (Write)
*        Output NDF structure containing the data and/or variance array
*        that is a copy of the input array, but with bad values flagging
*        the replacement value.
*     REPVAL = _DOUBLE (Read)
*        The element value to be substituted with the bad value.  The
*        same value is replaced in both the data and variance arrays
*        when COMP="All".  It must lie within the minimum and maximum
*        values of the data type of the array with higher precision.
*        The replacement value is converted to data type of the array
*        being converted before the search begins.  The suggested
*        default is the current value.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     setmagic irasmap aitoff repval=-2000000
*        This copies the NDF called irasmap to the NDF aitoff, except
*        that any pixels with the IPAC blank value of -2000000 are
*        flagged with the standard bad value in aitoff.
*     setmagic saturn saturnb 9999.0 comp=All
*        This copies the NDF called saturn to the NDF saturnb, except
*        that any elements in the data and variance arrays that have
*        value 9999.0 are flagged with the standard bad value.

*  Notes:
*     -  The comparison for floating-point values tests that the
*     difference between the replacement value and the element value is
*     less than their mean times the precision of the data type.

*  Algorithm:
*     -  Find which components to process.
*     -  Associate the input NDF.  If variance is requested check that
*     it is indeed present.  Abort if not.
*     -  Create the output NDF, propagating the unmodified array.
*     Match the data types of the component arrays.  If both are
*     required to be processed set the output data type of the NDF to
*     be the matched type.
*     -  Loop for each component.  Map the input and output arrays.
*     For each data type define the default, maximum and minimum
*     permitted replacement values.  Obtain the value to be replaced
*     in double precision and convert it to the desired data type.
*     Perform the replacement.  Unmap the arrays.  Report the number of
*     replacements.  If there were replacements set the bad-pixel flag.
*     -  Write final error meesage and tidy the NDF system.

*  Related Applications:
*     KAPPA: CHPIX, FILLBAD, GLITCH, NOMAGIC, SEGMENT, SUBSTITUTE,
*     ZAPLIN; SPECDRE: GOODVAR.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 November 6 (MJC):
*        Original NDF_ version.
*     1994 September 26 (MJC):
*        Replaced AIF calls, and used a modern style of commenting.
*        Made messages conditional.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     01-OCT-2004 (PWD):
*        Moved CNF_PAR into declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Primdat parameter definitions
      INCLUDE 'MSG_PAR'        ! Message constants
      INCLUDE 'CNF_PAR'        ! CNF functions

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  VAL_DTOI

      REAL
     :  VAL_DTOR

      INTEGER * 2
     :  VAL_DTOUW,
     :  VAL_DTOW

      BYTE
     :  VAL_DTOUB,
     :  VAL_DTOB

*  Local Variables:
      BYTE BSUVAL              ! Replacement value
      CHARACTER * ( 28 ) COMLIS ! List of available array components
      INTEGER COMLN            ! Length of component list
      CHARACTER * 8 COMP( 2 )  ! Name of each component to be modified
      CHARACTER * 8 COMPS      ! Components to be modified
      DOUBLE PRECISION DEFREP  ! Default replacement value
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Type of the image after
                               ! processing (not used)
      INTEGER EL               ! Number of elements in an array
      INTEGER I                ! Loop counter
      INTEGER ISUVAL           ! Replacement value
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Processing type of the image
      LOGICAL LCOMP( 2 )       ! Data, variance components are requested
                               ! to be processed or not
      DOUBLE PRECISION MAXREP  ! Maximum replacement value
      DOUBLE PRECISION MINREP  ! Minimum replacement value
      INTEGER NDFI             ! Identifier for input NDF
      INTEGER NDFO             ! Identifier for output NDF
      INTEGER NREP             ! Number of replacements made
      INTEGER PNTRI( 1 )       ! Pointer to input array component
      INTEGER PNTRO( 1 )       ! Pointer to output array component
      DOUBLE PRECISION REPVAL  ! Replacement value
      REAL RSUVAL              ! Replacement value
      INTEGER*2 WSUVAL         ! Replacement value

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'    ! Definitions of conversion routines

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Associate the input NDF.
*  ========================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find which component(s) to alter.
*  =================================
*  Inquire which arrays are available and form a comma-separated list
*  of them.
      CALL KPG1_ARCOL( NDFI, 'Data,Error,Variance', COMLIS,
     :                 COMLN, STATUS )

*  Find which component to plot, including the all option.  No need to
*  inquire when the value can only be 'Data'.
      IF ( COMLIS .EQ. 'Data' ) THEN
         COMPS = 'DATA'
      ELSE
         CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN )//',All',
     :                   .FALSE., COMPS, STATUS )
      END IF

*  Set up logical flags to indicate which components to process.  Also
*  create tokens for possible-error message, and individual component
*  arrays.  Note unused one must be initialised to prevent persistence
*  between invocations.
      IF ( COMPS .EQ. 'ALL' ) THEN
         LCOMP( 1 ) = .TRUE.
         LCOMP( 2 ) = .TRUE.
         COMP( 1 ) = 'Data'
         COMP( 2 ) = 'Variance'
         CALL MSG_SETC( 'ECOMP', 'Data and Variance components have' )

      ELSE IF ( COMPS .EQ. 'DATA' ) THEN
         LCOMP( 1 ) = .TRUE.
         LCOMP( 2 ) = .FALSE.
         COMP( 1 ) = 'Data'
         COMP( 2 ) = ' '
         CALL MSG_SETC( 'ECOMP', 'Data component has' )

      ELSE IF ( COMPS .EQ. 'VARIANCE' ) THEN
         LCOMP( 1 ) = .FALSE.
         LCOMP( 2 ) = .TRUE.
         COMP( 1 ) = ' '
         COMP( 2 ) = 'Variance'
         CALL MSG_SETC( 'ECOMP', 'Variance component has' )

      ELSE IF ( COMPS .EQ. 'ERROR' ) THEN
         LCOMP( 1 ) = .FALSE.
         LCOMP( 2 ) = .TRUE.
         COMP( 1 ) = ' '
         COMP( 2 ) = 'Error'
         CALL MSG_SETC( 'ECOMP', 'Error component has' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the output NDF.
*  ======================

*  Propagate the QUALITY, UNITS, WCS and AXIS components from the input
*  NDF to the output, and the arrays not to be processed.
      IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'Quality,Units,Axis,WCS', 'OUT', NDFO,
     :                  STATUS )

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                   /'_DOUBLE', NDFI, NDFI, 'Data,Variance', ITYPE,
     :                   DTYPE, STATUS )

*  Set an appropriate data type of the component arrays in the output
*  NDF.
         CALL NDF_STYPE( DTYPE, NDFO, 'Data,Variance', STATUS )

      ELSE IF ( LCOMP( 1 ) ) THEN
         CALL LPG_PROP( NDFI, 'Variance,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                   /'_DOUBLE', NDFI, NDFI, 'Data', ITYPE,
     :                   DTYPE, STATUS )

      ELSE IF ( LCOMP( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'Data,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

*  For this purpose Error and Variance structures are synonymous.
         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                   /'_DOUBLE', NDFI, NDFI, 'Variance', ITYPE,
     :                   DTYPE, STATUS )

      END IF

*  Obtain a title and assign it title to the output NDF.
*  =====================================================
*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Processing of the data and/or variance components.
*  ==================================================
      DO I = 1, 2
         IF ( LCOMP( I ) ) THEN

*  Map the data arrays.
            CALL KPG1_MAP( NDFI, COMP( I ), ITYPE, 'READ', PNTRI, EL,
     :                    STATUS )
            CALL KPG1_MAP( NDFO, COMP( I ), ITYPE, 'WRITE', PNTRO, EL,
     :                    STATUS )

*  Process each data type directly.
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_RTOD( VAL__MINR )
               MAXREP = NUM_RTOD( VAL__MAXR )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
               RSUVAL = VAL_DTOR( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAR( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          RSUVAL,
     :                          VAL__BADR,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_BTOD( VAL__MINB )
               MAXREP = NUM_BTOD( VAL__MAXB )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
               BSUVAL = VAL_DTOB( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          BSUVAL,
     :                          VAL__BADB,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = VAL__MIND
               MAXREP = VAL__MAXD
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAD( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          REPVAL,
     :                          VAL__BADD,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_ITOD( VAL__MINI )
               MAXREP = NUM_ITOD( VAL__MAXI )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
               ISUVAL = VAL_DTOI( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAI( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ISUVAL,
     :                          VAL__BADI,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_UBTOD( VAL__MINUB )
               MAXREP = NUM_UBTOD( VAL__MAXUB )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
               BSUVAL = VAL_DTOUB( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAUB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           BSUVAL,
     :                           VAL__BADUB,
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_UWTOD( VAL__MINUW )
               MAXREP = NUM_UWTOD( VAL__MAXUW )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
               WSUVAL = VAL_DTOUW( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAUW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           WSUVAL,
     :                           VAL__BADUW,
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
               MINREP = NUM_WTOD( VAL__MINW )
               MAXREP = NUM_WTOD( VAL__MAXW )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
               WSUVAL = VAL_DTOW( .FALSE., REPVAL, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
               CALL KPG1_CHVAW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          WSUVAL,
     :                          VAL__BADW,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                          STATUS )
            END IF

*  Unmap the arrays.  An error array must be unmapped as variance
*  due to a limitation in the NDF_ library.
            IF ( COMP( I ) .EQ. 'Error' ) THEN
               CALL NDF_UNMAP( NDFI, 'Variance', STATUS )
               CALL NDF_UNMAP( NDFO, 'Variance', STATUS )
            ELSE
               CALL NDF_UNMAP( NDFI, COMP( I ), STATUS )
               CALL NDF_UNMAP( NDFO, COMP( I ), STATUS )
            END IF

*  Report the number of replacements in the array.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'COMPS', COMP( I ) )
               CALL MSG_SETI( 'NREP', NREP )
               IF ( NREP .NE. 1 ) THEN
                  CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :              'There were ^NREP elements changed in the ^COMPS '/
     :              /'array.', STATUS )
               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :              'There was ^NREP element changed in the ^COMPS '/
     :              /'array.', STATUS )
               END IF
            END IF

*  If there were values replaced set the bad-pixel flag.
            IF ( NREP .GT. 0 ) THEN
               IF ( COMP( I ) .EQ. 'Error' ) THEN
                  CALL NDF_SBAD( .TRUE., NDFO, 'Variance', STATUS )
               ELSE
                  CALL NDF_SBAD( .TRUE., NDFO, COMP( I ), STATUS )
               END IF
            END IF
         END IF

*  End of loop for the arrays.
      END DO

*  Write an error report if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETMAGIC_ERR',
     :     'SETMAGIC: Unable to modify the bad-pixel values.', STATUS )
      END IF

*  Come here if a specific error occurred early in the application.
  999 CONTINUE

*  Tidy the workspace.
      CALL NDF_END( STATUS )

*  End
      END
