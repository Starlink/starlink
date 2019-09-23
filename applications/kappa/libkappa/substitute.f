      SUBROUTINE SUBSTITUTE( STATUS )
*+
*  Name:
*     SUBSTITUTE

*  Purpose:
*     Replaces all occurrences of a given value in an NDF array with
*     another value.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SUBSTITUTE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application changes all pixels that have a defined value in
*     an NDF with an alternate value. The number of replacements is
*     reported. Two modes are available.
*
*     - A single pair of old and new values can be supplied (see
*     Parameters OLDVAL and NEWVAL). All occurrences of OLDVAL are
*     replaced with NEWVAL. Other values are unchanged.
*
*     - A look-up table containing corresponding old and new values can
*     be supplied. By default, each input pixel that equals an old value
*     is replaced by the corresponding new value. Alternatively, all
*     input pixels can be changed to a new value by interpolation between
*     the input values (see Parameters LUT and INTERP).

*  Usage:
*     substitute in out oldval newval [comp]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The components whose values are to be substituted.  It may
*        be "Data", "Error", "Variance", or "All".  The last of the
*        options forces substitution in both the data and variance
*        arrays.  This parameter is ignored if the data array is the
*        only array component within the NDF.  ["Data"]
*     INTERP = LITERAL (Read)
*        Determines how the values in the file specified by parameter
*        LUT are used, from the following options.
*
*        - "None" -- Pixel values that equal an input value are replaced
*        by the corresponding output value. Other values are left
*        unchanged.
*
*        - "Nearest" -- Every pixel value is replaced by the output value
*        corresponding to the nearest input value.
*
*        - "Linear" -- Every pixel value is replaced by an output value
*        determined using linear interpolation between the input values.
*
*        If "Nearest" or "Linear" is used, pixel values that are outside
*        the range of input value covered by the look-up table are set
*        bad in the output. Additionally, an error is reported if the
*        old data values are not montonic increasing. ["None"]
*     IN = NDF  (Read)
*        Input NDF structure containing the data and/or variance array
*        to have some of its elements substituted.
*     LUT = FILENAME (Read)
*        The name of a text file containing a look-up table of old and
*        new data values. If null (!) is supplied for this parameter the
*        old and new data values will instead be obtained using Parameters
*        OLDVAL and NEWVAL. Lines starting with a hash (#) or exclamation
*        mark (!)  are ignored. Other lines should contain an old data
*        value followed by the corresponding new data value. The way in
*        which the values in this table are used is determined by
*        Parameter INTERP. [!]
*     NEWVAL = _DOUBLE (Read)
*        The value to replace occurrences of OLDVAL.  It must lie
*        within the minimum and maximum values of the data type of the
*        array with higher precision.  The new value is converted to
*        data type of the array being converted before the search
*        begins.  The suggested default is the current value. This
*        parameter is only accessed if a null (!) value is supplied for
*        Parameter LUT.
*     OLDVAL = _DOUBLE (Read)
*        The element value to be replaced.  The same value is
*        substituted in both the data and variance arrays when
*        COMP="All".  It must lie within the minimum and maximum values
*        of the data type of the array with higher precision.  The
*        replacement value is converted to data type of the array being
*        converted before the search begins.  The suggested default is
*        the current value. This parameter is only accessed if a null
*        (!) value is supplied for Parameter LUT.
*     OUT = NDF (Write)
*        Output NDF structure containing the data and/or variance array
*        that is a copy of the input array, but with replacement values
*        substituted.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     TYPE = LITERAL (Read)
*        The numeric type for the output NDF. The value given should be
*        one of the following: _DOUBLE, _REAL, _INTEGER, _INT64, _WORD,
*        _UWORD, _BYTE or _UBYTE (note the leading underscore).  If a
*        null (!) value is supplied, the output data type equals the
*        input data type. [!]

*  Examples:
*     substitute aa bb 1 0
*        This copies the NDF called aa to the NDF bb, except
*        that any pixels with value 1 in aa are altered to have value
*        0 in bb.
*     substitute aa bb oldval=1 newval=0 comp=v
*        As above except the substitution occurs to the variance
*        values.
*     substitute in=saturn out=saturn5 oldval=2.5 newval=5 comp=All
*        This copies the NDF called saturn to the NDF saturn5, except
*        that any elements in the data and variance arrays that have
*        value 2.5 are altered to have value 5 in saturn5.

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
*     Do likewise for the new value.  Perform the replacement.  Unmap
*     the arrays.  Report the number of replacements.
*     -  Write final error message and tidy the NDF system.

*  Related Applications:
*     KAPPA: CHPIX, FILLBAD, GLITCH, NOMAGIC, SEGMENT, SETMAGIC, ZAPLIN;
*     SPECDRE: GOODVAR.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1997-1998, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 2019 East Asian Observatory.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 May 23 (MJC):
*        Original version.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     01-OCT-2004 (PWD):
*        Moved CNF_PAR into declarations.
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     4-SEP-2019 (DSB):
*        Added Parameters LUT, INTERP and TYPE.
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
      INTEGER VAL_DTOI

      REAL VAL_DTOR

      INTEGER * 2 VAL_DTOUW
      INTEGER * 2 VAL_DTOW

      INTEGER * 8 VAL_DTOK

      BYTE VAL_DTOUB
      BYTE VAL_DTOB

*  Local Variables:
      BYTE BNUVAL              ! New value
      BYTE BSUVAL              ! Replacement value
      CHARACTER * ( 28 ) COMLIS ! List of available array components
      INTEGER COMLN            ! Length of component list
      CHARACTER * 8 COMP( 2 )  ! Name of each component to be modified
      CHARACTER * 8 COMPS      ! Components to be modified
      DOUBLE PRECISION DEFREP  ! Default replacement value
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Type of the image after
                               ! processing (not used)
      INTEGER EL               ! Number of elements in an array
      DOUBLE PRECISION EPSIN   ! Input precision
      INTEGER I                ! Loop counter
      INTEGER IFLEV            ! Original MSG information level
      CHARACTER * 7 INTERP     ! LUT interpolation method
      INTEGER INUVAL           ! New value
      INTEGER IPLUT            ! Pointer to LUT data
      INTEGER ISUVAL           ! Replacement value
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Processing type of the image
      INTEGER*8 KNUVAL         ! New value
      INTEGER*8 KSUVAL         ! Replacement value
      DOUBLE PRECISION LBND( 2 )! Lower limits of data value in LUT
      LOGICAL LCOMP( 2 )       ! Data, variance components are requested
                               ! to be processed or not
      DOUBLE PRECISION MAXREP  ! Maximum replacement value
      DOUBLE PRECISION MINREP  ! Minimum replacement value
      INTEGER NDFI             ! Identifier for input NDF
      INTEGER NDFO             ! Identifier for output NDF
      CHARACTER NEWTYP * ( NDF__SZTYP ) ! New output data type
      DOUBLE PRECISION NEWVAL  ! New value
      INTEGER NLUT             ! No. of entries in LUT
      INTEGER NREP             ! Number of replacements made
      INTEGER PNTRI( 1 )       ! Pointer to input array component
      INTEGER PNTRO( 1 )       ! Pointer to output array component
      INTEGER POSCOD( 2 )      ! Indices of LUT columns to use
      DOUBLE PRECISION REPVAL  ! Replacement value
      REAL RNUVAL              ! New value
      REAL RSUVAL              ! Replacement value
      DOUBLE PRECISION UBND( 2 )! Upper limits of data value in LUT
      INTEGER*2 WNUVAL         ! New value
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

*  See if multiple values are to be substituted.
*  =============================================
*  Set MSG level to suppress the inappropriate message issued by KPG1_FLCOD
      CALL MSG_IFLEV( IFLEV, ' ', STATUS )
      CALL MSG_IFSET( MSG__QUIET, STATUS )
      POSCOD( 1 ) = 1
      POSCOD( 2 ) = 2
      IF( STATUS .NE. SAI__OK ) GOTO 999
      CALL KPG1_FLCOD( 'LUT', 2, POSCOD, NLUT, IPLUT, LBND, UBND,
     :                 STATUS )
      CALL MSG_IFSET( IFLEV, STATUS )

*  Annul the error if no LUT was supplied. Indicate that Parameters
*  OLDVAL and NEWVAL should be used by setting NLUT to zero.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NLUT = 0

*  If a LUT was supplied, get the interpolation method to use.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_CHOIC( 'INTERP', 'None', 'None,Linear,Nearest',
     :                   .TRUE., INTERP, STATUS )
      END IF

*  Find which component(s) to alter.
*  =================================
*  Inquire which arrays are available and form a comma-separated list
*  of them.
      CALL KPG1_ARCOL( NDFI, 'Data,Error,Variance', COMLIS,
     :                 COMLN, STATUS )

*  Find which component to change, including the all option.  No need to
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

*  Propagate the QUALITY, UNITS and AXIS components from the input
*  NDF to the output, and the arrays not to be processed.
      IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'WCS,Quality,Units,Axis', 'OUT', NDFO,
     :                  STATUS )

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :                   /'_REAL,_DOUBLE', NDFI, NDFI, 'Data,Variance',
     :                   ITYPE, DTYPE, STATUS )

*  Set an appropriate data type of the component arrays in the output
*  NDF.
         CALL NDF_STYPE( DTYPE, NDFO, 'Data,Variance', STATUS )

      ELSE IF ( LCOMP( 1 ) ) THEN
         CALL LPG_PROP( NDFI, 'Variance,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :                   /'_REAL,_DOUBLE', NDFI, NDFI, 'Data', ITYPE,
     :                   DTYPE, STATUS )

      ELSE IF ( LCOMP( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'Data,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

*  For this purpose Error and Variance structures are synonymous.
         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :                   /'_REAL,_DOUBLE', NDFI, NDFI, 'Variance',
     :                   ITYPE, DTYPE, STATUS )

      END IF

* Get the machine precision for the input data type.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         EPSIN = VAL__EPSB
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         EPSIN = VAL__EPSUB
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         EPSIN = VAL__EPSW
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         EPSIN = VAL__EPSUW
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         EPSIN = VAL__EPSI
      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         EPSIN = VAL__EPSK
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         EPSIN = VAL__EPSR
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         EPSIN = VAL__EPSD
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', ITYPE )
         CALL ERR_REP( ' ', 'Unsupported data type ''^T'' encountered.',
     :                STATUS )
      END IF

*  Allow the user to select a different data type for the output NDF.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL PAR_CHOIC( 'TYPE', ITYPE, '_BYTE,_UBYTE,_WORD,_UWORD,'//
     :                '_INTEGER,_INT64,_REAL,_DOUBLE', .FALSE.,
     :                NEWTYP, STATUS )

*  If null (!) was supplied for Parameter TYPE, just annul the error and
*  continue with the output NDF unchanged.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, change the data type of the output NDF.
      ELSE
         ITYPE = NEWTYP
         CALL NDF_STYPE( ITYPE, NDFO, 'Data,Variance', STATUS )
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

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAR( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_RTOD( VAL__MINR ) )
                  ELSE
                     MINREP = NUM_RTOD( VAL__MINR )
                  END IF
                  MAXREP = NUM_RTOD( VAL__MAXR )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  RSUVAL = VAL_DTOR( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  RNUVAL = VAL_DTOR( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAR( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             RSUVAL, RNUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_BTOD( VAL__MINB ) )
                  ELSE
                     MINREP = NUM_BTOD( VAL__MINB )
                  END IF
                  MAXREP = NUM_BTOD( VAL__MAXB )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  BSUVAL = VAL_DTOB( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  BNUVAL = VAL_DTOB( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             BSUVAL, BNUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAD( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, VAL__MIND )
                  ELSE
                     MINREP = VAL__MIND
                  END IF
                  MAXREP = VAL__MAXD
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAD( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             REPVAL, NEWVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAI( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_ITOD( VAL__MINI ) )
                  ELSE
                     MINREP = NUM_ITOD( VAL__MINI )
                  END IF
                  MAXREP = NUM_ITOD( VAL__MAXI )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  ISUVAL = VAL_DTOI( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  INUVAL = VAL_DTOI( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAI( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ISUVAL, INUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAK( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_KTOD( VAL__MINK ) )
                  ELSE
                     MINREP = NUM_KTOD( VAL__MINK )
                  END IF
                  MAXREP = NUM_KTOD( VAL__MAXK )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  KSUVAL = VAL_DTOK( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  KNUVAL = VAL_DTOK( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAK( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             KSUVAL, KNUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAUB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_UBTOD( VAL__MINUB ) )
                  ELSE
                     MINREP = NUM_UBTOD( VAL__MINUB )
                  END IF
                  MAXREP = NUM_UBTOD( VAL__MAXUB )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  BSUVAL = VAL_DTOUB( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  BNUVAL = VAL_DTOUB( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAUB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              BSUVAL, BNUVAL,
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAUW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_UWTOD( VAL__MINUW ) )
                  ELSE
                     MINREP = NUM_UWTOD( VAL__MINUW )
                  END IF
                  MAXREP = NUM_UWTOD( VAL__MAXUW )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  WSUVAL = VAL_DTOUW( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  WNUVAL = VAL_DTOUW( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAUW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              WSUVAL, WNUVAL,
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              NREP, STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  If a LUT was provided, apply it.
               IF( NLUT .GT. 0 ) THEN
                  CALL KPG1_LTVAW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             INTERP, LBND( 1 ), UBND( 1 ), NLUT,
     :                             %VAL( CNF_PVAL( IPLUT ) ), EPSIN,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )

*  Otherwise, use the OLDVAL and NEWVAL parameters.
               ELSE

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
                  IF ( LCOMP( 2 ) ) THEN
                     MINREP = MAX( 0.0D0, NUM_WTOD( VAL__MINW ) )
                  ELSE
                     MINREP = NUM_WTOD( VAL__MINW )
                  END IF
                  MAXREP = NUM_WTOD( VAL__MAXW )
                  DEFREP = 0.0D0

*  Get the replacement value.
                  CALL PAR_GDR0D( 'OLDVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., REPVAL, STATUS )

*  Convert the replacement value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
                  WSUVAL = VAL_DTOW( .FALSE., REPVAL, STATUS )

*  Get the new value.
                  CALL PAR_GDR0D( 'NEWVAL', DEFREP, MINREP, MAXREP,
     :                            .FALSE., NEWVAL, STATUS )

*  Convert the new value to the desired type.  Use VAL_ to protect
*  against potentionally harmful values when there is a bad status.
                  WNUVAL = VAL_DTOW( .FALSE., NEWVAL, STATUS )

*  Replace the values in the output array, otherwise copy from the
*  input to the output NDF.
                  CALL KPG1_CHVAW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             WSUVAL, WNUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             NREP, STATUS )
               END IF

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
         CALL ERR_REP( 'SUBSTITUTE_ERR',
     :     'SUBSTITUTE: Unable to modify the values.', STATUS )
      END IF

*  Come here if a specific error occurred early in the application.
  999 CONTINUE

*  Free the LUT.
      IF( NLUT .GT. 0 ) CALL PSX_FREE( IPLUT, STATUS )

*  Tidy the workspace.
      CALL NDF_END( STATUS )

*  End
      END
