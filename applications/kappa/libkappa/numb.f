      SUBROUTINE NUMB( STATUS )
*+
*  Name:
*     NUMB

*  Purpose:
*     Counts the number of elements of an NDF with values or absolute
*     values above or below a threshold.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NUMB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine counts and reports the number of elements of an
*     array within an input NDF structure that have a value or absolute
*     value greater or less than a specified threshold.  This statistic
*     is also shown as a percentage of the total number of array
*     elements.

*  Usage:
*     numb in value [comp]

*  ADAM Parameters:
*     ABS = _LOGICAL (Read)
*        If ABS is TRUE, the criterion is a comparison of the absolute
*        value with the threshold; if FALSE, the criterion is a
*        comparison of the actual value with the threshold.  The
*        current value is the suggested default. [FALSE]
*     ABOVE = _LOGICAL (Read)
*        If ABOVE is TRUE the criterion tests whether values are
*        greater than the threshold; if FALSE the criterion tests
*        whether values are less than the threshold.  The current value
*        of ABOVE is the suggested default. [TRUE]
*     COMP = LITERAL (Read)
*        The components whose flagged values are to be substituted.
*        It may be "Data", "Error", "Variance", or "Quality".  If
*        "Quality" is specified, then the quality values are treated as
*        numerical values in the range 0 to 255. ["Data"]
*     IN  = NDF (Read)
*        Input NDF structure containing the array to be tested.
*     NUMBER = _INTEGER (Write)
*        The number of elements that satisfied the criterion.
*     VALUE  = _DOUBLE (Read)
*        Threshold against which the values of the array elements will
*        be tested.  It must lie in within the minimum and maximum
*        values of the data type of the array being processed, unless
*        ABS = TRUE or the component is the variance or quality
*        array, in which case the minimum is zero.  The suggested
*        default is the current value.

*  Examples:
*     numb image 100
*        This counts the number of elements in the data array of the NDF
*        called image that exceed 100.
*     numb spectrum 100 noabove
*        This counts the number of elements in the data array of the NDF
*        called spectrum that are less than 100.
*     numb cube 100 abs
*        This counts the number of elements in the data array of the NDF
*        called cube whose absolute values exceed 100.
*     numb image -100 number=(count)
*        This counts the number of elements in the data array of the NDF
*        called image that exceed -100 and write the number to ICL
*        variable COUNT.
*     numb image 200 v
*        This counts the number of elements in the variance array of
*        the NDF called image that exceed 200.

*  Algorithm:
*     -  Find which criterion to use and which component to process.
*     -  Associate the input NDF.  Test that the chosen array component
*     is present.  Inquire whether the component may have bad pixels,
*     and the data type of the array.
*     -  For the actual data type obtain the replacement value in the
*     same type (via double precision)and count the number of elements
*     above the threshold. Write a token for the report using the
*     appropriate type.
*     -  Report the NDF name, title and component being tested.
*     Report the result and write it to an output parameter.

*  Implementation Status:
*     -  This routine correctly processes the DATA, QUALITY,
*     TITLE, and VARIANCE components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.
*     -  Huge NDFs are supported.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 2021 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     1991 November 11 (MJC):
*        Original NDF version.
*     1995 January 16 (MJC):
*        Replaced AIF by PAR.  Added ERROR option to parameter COMP.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2004 October 1 (PWD):
*        Moved CNF_PAR into declarations.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     2-JUN-2021 (DSB):
*        Add support for huge arrays.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global environment parameters
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'PRM_PAR'        ! Magic-value pixel definitions
      INCLUDE 'CNF_PAR'        ! CNF functions

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      BYTE
     :  VAL_DTOB,              ! Double to Byte conversion
     :  VAL_DTOUB              ! Double to Unsigned Byte conversion

      INTEGER
     :  VAL_DTOI               ! Double to Integer conversion

      INTEGER * 2
     :  VAL_DTOUW,             ! Double to Unsigned Word conversion
     :  VAL_DTOW               ! Double to Word conversion

      INTEGER * 8
     :  VAL_DTOK               ! Double to 64-bit integer conversion

      REAL
     :  VAL_DTOR               ! Double to Real conversion

*  Local Variables:
      BYTE
     :  BVALUE                 ! Threshold value to be tested against

      CHARACTER
     :  BUF * ( 132 ),         ! Text buffer
     :  COMP * ( 8 ),          ! Component array to be modified
     :  CVALUE * 30,           ! Threshold value
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  MCOMP * ( 8 ),         ! Component array to be mapped
     :  PCENT * ( 6 )          ! Buffer for formatting percentages

      DOUBLE PRECISION
     :  DVALUE,                ! Threshold value to be tested against
     :  DVMIN,                 ! Minimum threshold value
     :  DVMAX                  ! Maximum threshold value

      INTEGER
     :  IVALUE,                ! Threshold value to be tested against
     :  NC,                    ! Number of characters in text buffer
     :  NCV,                   ! Number of characters in value
     :  NDFI,                  ! Identifier for input NDF
     :  PNTRI( 1 )             ! Pointer to the input array

      INTEGER * 2
     :  WVALUE                 ! Threshold value to be tested against

      INTEGER * 8
     :  EL,                    ! Number of elements in an array
     :  KVALUE,                ! Threshold value to be tested against
     :  NUMBER                 ! Number of points above threshold

      LOGICAL                  ! True if:
     :  ABOVE,                 ! Criterion uses greater than threshold
     :  BAD,                   ! Either of the replacement values were
                               ! bad
     :  THERE,                 ! The variance or quality array has been
                               ! requested and it is present in the
                               ! input NDF
     :  VABS                   ! Compare absolute array values against
                               ! the threshold

      REAL
     :  RVALUE                 ! Threshold value to be tested against

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'      ! Definitions of conversion routines

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find which criterion to use.
*  ============================
      CALL PAR_GTD0L( 'ABS', .FALSE., .TRUE., VABS, STATUS )
      CALL PAR_GTD0L( 'ABOVE', .TRUE., .TRUE., ABOVE, STATUS )

*  Find which component(s) to alter.
*  =================================
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Variance,Quality',
     :                .FALSE., COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Associate the input NDF.
*  ========================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  There must be a data array, but for variance check that it is
*  present.
      IF ( COMP( 1:4 ) .NE. 'DATA' ) THEN
         CALL NDF_STATE( NDFI, COMP, THERE, STATUS )

*  The component is not present or not defined.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDFI )
            CALL MSG_SETC( 'COMPS', COMP )
            CALL ERR_REP( 'NUMB_NOCOMP',
     :        'NUMB: ^COMPS component is not defined in NDF ^NDF.',
     :        STATUS )
            GO TO 999
         END IF
      END IF

*  Inquire whether or not the NDF may have bad values.
      CALL NDF_BAD( NDFI, COMP, .FALSE., BAD, STATUS )

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
      CALL NDF_TYPE( NDFI, COMP, ITYPE, STATUS )

*  Map the array.
*  ==============
      CALL KPG1_MAP8( NDFI, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*  Process the array using the appropriate implementation data type.
*  =================================================================
      IF ( ITYPE .EQ. '_REAL' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = NUM_RTOD( VAL__MINR )
         END IF
         DVMAX = NUM_RTOD( VAL__MAXR )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         RVALUE = VAL_DTOR( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8R( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    RVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_RTOC( RVALUE, CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = NUM_BTOD( VAL__MINB )
         END IF
         DVMAX = NUM_BTOD( VAL__MAXB )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         BVALUE = VAL_DTOB( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8B( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    BVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_ITOC( NUM_BTOI( BVALUE ), CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = VAL__MIND
         END IF

         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, VAL__MAXD, .FALSE.,
     :                   DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8D( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    DVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_DTOC( DVALUE, CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = NUM_ITOD( VAL__MINI )
         END IF
         DVMAX = NUM_ITOD( VAL__MAXI )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         IVALUE = VAL_DTOI( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8I( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    IVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_ITOC( IVALUE, CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = NUM_KTOD( VAL__MINK )
         END IF
         DVMAX = NUM_KTOD( VAL__MAXK )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         KVALUE = VAL_DTOK( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8K( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    KVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_KTOC( KVALUE, CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*  Obtain the threshold in the appropriate data type.  The minimum
*  permitted must not be negative.
         DVMIN = 0.0
         DVMAX = NUM_UBTOD( VAL__MAXUB )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         BVALUE = VAL_DTOUB( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8UB( BAD, VABS, ABOVE, EL,
     :                     %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     BVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_ITOC( NUM_UBTOI( BVALUE ), CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

*  Obtain the threshold in the appropriate data type.  The minimum
*  permitted must not be negative.
         DVMIN = 0.0
         DVMAX = NUM_UWTOD( VAL__MAXUW )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         WVALUE = VAL_DTOUW( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8UW( BAD, VABS, ABOVE, EL,
     :                     %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     WVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_ITOC( NUM_UWTOI( WVALUE ), CVALUE, NCV )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Obtain the threshold in the appropriate data type.  Watch for the
*  cases where it must not be negative.
         IF ( VABS .OR. COMP .EQ. 'VARIANCE' .OR.
     :        COMP .EQ. 'QUALITY' ) THEN
            DVMIN = 0.0
         ELSE
            DVMIN = NUM_WTOD( VAL__MINW )
         END IF
         DVMAX = NUM_WTOD( VAL__MAXW )
         CALL PAR_GDR0D( 'VALUE', 0.0D0, DVMIN, DVMAX, .FALSE., DVALUE,
     :                   STATUS )

*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
         WVALUE = VAL_DTOW( .FALSE., DVALUE, STATUS )

*  Call routine to count up the values.
         CALL KPG1_NUMB8W( BAD, VABS, ABOVE, EL,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    WVALUE, NUMBER, STATUS )

*  Make a common character version later be passed to a token for
*  reporting the result.
         CALL CHR_ITOC( NUM_WTOI( WVALUE ), CVALUE, NCV )
      END IF

*  Write the statistic to the output parameter.
      CALL PAR_PUT0K( 'NUMBER', NUMBER, STATUS )

*  Report the result.
*  ==================

*  Display the NDF name, also sending it to the logfile if necessary.
      CALL MSG_BLANK( STATUS )
      CALL NDF_MSG( 'NDF', NDFI )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Pixel statistics for the NDF structure ^NDF',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( :NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
      END IF

*  Display (and log) the NDF's title.
      CALL MSG_BLANK( STATUS )
      CALL NDF_CMSG( 'TITLE', NDFI, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( :NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
      END IF

*  Display the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( :NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
      END IF

*  Display the number found and the percentage.
*  ============================================

*  Find the percentage of the total number of array elements.
      WRITE( PCENT, '(F6.2)' ) 100.0 * REAL( NUMBER ) / REAL( EL )
      CALL CHR_LDBLK( PCENT )
      CALL MSG_SETC( 'PCENT', PCENT )

*  Make a token for the sense of the criterion, the value and the
*  number counted.
      CALL MSG_SETC( 'VALUE', CVALUE )
      CALL MSG_SETK( 'NUMBER', NUMBER )
      IF ( ABOVE ) THEN
         CALL MSG_SETC( 'SIGN', '>' )
      ELSE
         CALL MSG_SETC( 'SIGN', '<' )
      END IF

*  Actually write the report.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( VABS ) THEN
            CALL MSG_OUT( 'NUM_VALUE', '      Number of elements with '/
     :        /'absolute values ^SIGN ^VALUE : ^NUMBER (^PCENT%%)',
     :        STATUS )
         ELSE
            CALL MSG_OUT( 'NUM_VALUE', '      Number of elements with '/
     :        /'values ^SIGN ^VALUE : ^NUMBER (^PCENT%%)', STATUS )
         END IF
         CALL MSG_BLANK( STATUS )
      END IF

*  Arrive here if an error occurs.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NUMB_ERR',
     :     'NUMB: Error counting the number of values above or below '/
     :     /'a threshold.', STATUS )
      END IF

      END
