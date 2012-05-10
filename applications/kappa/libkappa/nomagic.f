      SUBROUTINE NOMAGIC ( STATUS )
*+
*  Name:
*     NOMAGIC

*  Purpose:
*     Replaces all occurrences of magic value pixels in an NDF array
*     with a new value.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NOMAGIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This function replaces the standard `magic value' assigned to bad
*     pixels in an NDF with an alternative value, or with random
*     samples taken from a Normal distribution.  Input pixels which do
*     not have the magic value are left unchanged.  The number of
*     replacements is reported.  NOMAGIC's applications include the
*     export of data to software that has different magic values or
*     does not support bad values.
*
*     If a constant value is used to replace magic values (which will
*     be the case if parameter SIGMA is given the value zero), then the
*     same replacement value is used for both the data and variance
*     arrays when COMP="All".  If the variance is being processed, the
*     replacement value is constrained to be non-negative.
*
*     Magic values are replaced by random values if the parameter SIGMA
*     is given a non-zero value.  If both DATA and VARIANCE components
*     are being processed, then the random values are only stored in
*     the DATA component; a constant value equal to SIGMA squared is
*     used to replace all magic values in the VARIANCE component.  If
*     only a single component is being processed (whether it be DATA,
*     VARIANCE, or Error), then the random values are used to replace
*     the magic values.  If random values are generated which will not
*     fit into the allowed numeric range of the output NDF, then they
*     are discarded and new random values are obtained instead.  This
*     continues until a useable value is obtained.  This could introduce
*     some statistical bias if many such re-tries are performed.  For
*     this reason SIGMA is restricted so that there are at least 4
*     standard deviations between the mean (given by REPVAL) and the
*     nearest limit.  NOMAGIC notifies of any re-tries that are
*     required.

*  Usage:
*     nomagic in out repval sigma [comp]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The components whose flagged values are to be substituted. It
*        may be "Data", "Error", "Variance", or "All".  The last of the
*        options forces substitution of bad pixels in both the data and
*        variance arrays.  This parameter is ignored if the data array
*        is the only array component within the NDF.  ["Data"]
*     IN = NDF  (Read)
*        Input NDF structure containing the data and/or variance array
*        to have its elements flagged with the magic value replaced by
*        another value.
*     OUT = NDF (Write)
*        Output NDF structure containing the data and/or variance array
*        without any elements flagged with the magic value.
*     REPVAL = _DOUBLE (Read)
*        The constant value to substitute for the magic values, or (if
*        parameter SIGMA is given a non-zero value) the mean of the
*        distribution from which replacement values are obtained.  It
*        must lie within the minimum and maximum values of the data
*        type of the array with higher precision, except when variance
*        is being processed, in which case the minimum is constrained
*        to be non-negative.  The replacement value is converted to the
*        data type of the array being converted.  The suggested default
*        is the current value.
*     SIGMA = _DOUBLE (Read)
*        The standard deviation of the random values used to replace
*        magic values in the input NDF.  If this is zero (or if a null
*        value is given), then a constant replacement value is
*        used.  The supplied value must be positive and must be small
*        enough to allow at least 4 standard deviations between the
*        mean value (given by REPVAL) and the closest limit. [!]
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     nomagic aitoff irasmap repval=-2000000
*        This copies the NDF called aitoff to the NDF irasmap, except
*        that any bad values in the data array are replaced with the
*        IPAC blank value, -2000000, in the NDF called irasmap.
*     nomagic saturnb saturn 9999.0 comp=all
*        This copies the NDF called saturnb to the NDF saturn, except
*        that any bad values in the data and variance arrays are
*        replaced with 9999 in the NDF called saturn.
*     nomagic in=cleaned out=filled  repval=0 sigma=10 comp=all
*        This copies the NDF called cleaned to the NDF filled, except
*        that any bad values in the data array are replaced by random
*        samples taken from a Normal distribition of mean zero and
*        standard deviation 10.  Bad values in the variance array are
*        replaced by the constant value 100.

*  Notes:
*     -  If the NDF arrays have no bad pixels the application will
*     abort.
*     -  Use GLITCH if a neighbourhood context is required to remove
*     the bad values.

*  Algorithm:
*     -  Find which components to process.
*     -  Associate the input NDF.  If variance is requested check that
*     it is indeed present.  Abort if not.
*     -  Check whether bad pixels may be present.  Define whether
*     processing is actually required for each array.  If neither is
*     required, abort.
*     -  Create the output NDF, propagating the unmodified array.
*     Match the data types of the component arrays.  If both are
*     required to be processed set the output data type of the NDF to
*     be the matched type.
*     -  Loop for each component.  Map the input and output arrays.
*     For each data type define the default, maximum and minimum
*     permitted replacement values.  Obtain the replacement value in
*     double precision and convert it to the desired data type.  Do the
*     same for the standard deviation value.  Perform the replacement.
*     Unmap the arrays.  Report the number of replacements.
*     -  Write final error message and tidy the NDF system.

*  Related Applications:
*     KAPPA: CHPIX, FILLBAD, GLITCH, SEGMENT, SETMAGIC, SUBSTITUTE,
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
*     Copyright (C) 1998-1999, 2002, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 November 4 (MJC):
*        Original NDF_ version.
*     1994 September 26 (MJC):
*        Replaced AIF calls, and used a modern style of commenting.
*        Made messages conditional.
*     1994 December 15 (DSB):
*        Added SIGMA parameter and associated code.  Unmap '*'
*        components rather than COMP( I ) component to avoid
*        possibility of NDF_UNMAP being asked to unmap the 'Error'
*        component.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     4-MAY-1999 (TDCA):
*        Added code to set badbits mask to zero.
*     3-SEP-2002 (DSB):
*        Do not report an error if the input has no bad pixels.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     01-OCT-2004 (PWD):
*        Moved CNF_PAR into declarations.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'MSG_PAR'        ! Message constants
      INCLUDE 'PRM_PAR'        ! Primdat parameter definitions
      INCLUDE 'CNF_PAR'        ! CNF functions

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER VAL_DTOI

      REAL VAL_DTOR

      INTEGER*2 VAL_DTOUW
      INTEGER*2 VAL_DTOW

      INTEGER*8 VAL_DTOK

      BYTE VAL_DTOUB
      BYTE VAL_DTOB

*  Local Constants:
      DOUBLE PRECISION NSIGMA  ! The no. of standard deviations which must
      PARAMETER ( NSIGMA = 4.0 )! separate REPVAL from upper or lower limit
      BYTE BADBIT              ! The value the bad-bits mask of the output
      PARAMETER ( BADBIT = 0 ) ! NDF is set to after processing.

*  Local Variables:
      LOGICAL BAD( 2 )         ! Data, variance components may have bad
                               ! pixels
      BYTE BSUVAL              ! Replacement value
      BYTE BSDVAL              ! Sigma value
      CHARACTER * ( 28 ) COMLIS ! List of available array components
      INTEGER COMLN            ! Length of component list
      CHARACTER * 8 COMP( 2 )  ! Name of each component to be modified
      CHARACTER * 8 COMPS      ! Components to be modified
      DOUBLE PRECISION DEFREP  ! Default replacement value
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Type of the image after
                               ! processing (not used)
      INTEGER EL               ! Number of elements in an array
      LOGICAL FIRST            ! Is this the first component to be done?
      INTEGER I                ! Loop counter
      INTEGER ISUVAL           ! Replacement value
      INTEGER ISDVAL           ! Sigma value
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Processing type of the image
      INTEGER*8 KSUVAL         ! Replacement value
      INTEGER*8 KSDVAL         ! Sigma value
      LOGICAL LCOMP( 2 )       ! Data, variance components are requested
                               ! to be processed
      DOUBLE PRECISION MAXREP  ! Maximum replacement value
      DOUBLE PRECISION MAXSIG  ! Maximum sigma value
      DOUBLE PRECISION MINREP  ! Minimum replacement value
      INTEGER NDFI             ! Identifier for input NDF
      INTEGER NDFO             ! Identifier for output NDF
      INTEGER NREP             ! Number of replacements made
      INTEGER PNTRI( 1 )       ! Pointer to input array component
      INTEGER PNTRO( 1 )       ! Pointer to output array component
      LOGICAL PROCES( 2 )      ! Data, variance components are to be
                               ! processed (i.e. present plus may have
                               ! bad pixels)
      LOGICAL QUAL             ! Quality array present in output NDF ?
      DOUBLE PRECISION REPVAL  ! Replacement value
      REAL RSUVAL              ! Replacement value
      REAL RSDVAL              ! sigma value
      DOUBLE PRECISION SIGMA   ! Sigma value
      INTEGER*2 WSUVAL         ! Replacement value
      INTEGER*2 WSDVAL         ! Sigma value

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

*  Check whether or not bad pixels may be present.
*  ===============================================
      BAD( 1 ) = .FALSE.
      BAD( 2 ) = .FALSE.

      IF ( LCOMP( 1 ) )
     :  CALL NDF_BAD( NDFI, 'Data', .FALSE., BAD( 1 ), STATUS )

*  For this purpose Error and Variance structures are synonymous.
      IF ( LCOMP( 2 ) )
     :  CALL NDF_BAD( NDFI, 'Variance', .FALSE., BAD( 2 ), STATUS )

*  Define two logicals to indicate whether each component is to be
*  processed.
      PROCES( 1 ) = BAD( 1 ) .AND. LCOMP( 1 )
      PROCES( 2 ) = BAD( 2 ) .AND. LCOMP( 2 )

*  Create the output NDF.
*  ======================

*  Propagate the QUALITY, UNITS and AXIS components from the input NDF
*  to the output, and the arrays not to be processed.
      IF ( PROCES( 1 ) .AND. PROCES( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'Quality,Units,Axis,WCS', 'OUT', NDFO,
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

      ELSE IF ( PROCES( 1 ) ) THEN
         CALL LPG_PROP( NDFI, 'Variance,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :                   /'_REAL,_DOUBLE', NDFI, NDFI, 'Data', ITYPE,
     :                   DTYPE, STATUS )

      ELSE IF ( PROCES( 2 ) ) THEN
         CALL LPG_PROP( NDFI, 'Data,Quality,Units,Axis,WCS', 'OUT',
     :                  NDFO, STATUS )

         CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :                   /'_REAL,_DOUBLE', NDFI, NDFI, 'Variance',
     :                   ITYPE, DTYPE, STATUS )

      ELSE
         CALL LPG_PROP( NDFI, 'Data,Variance,Quality,Units,Axis,WCS',
     :                  'OUT', NDFO, STATUS )

      END IF

*  Obtain a title and assign it title to the output NDF.
*  =====================================================
*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

* If QUALITY array present output NDF, set bad-bits mask to zero.
      CALL NDF_STATE( NDFO, 'QUAL', QUAL, STATUS )
      IF ( QUAL ) THEN
         CALL NDF_SBB( BADBIT, NDFO, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999
      END IF

*  Processing of the data and/or variance components.
*  ==================================================
      FIRST = .TRUE.

      DO I = 1, 2
         IF ( PROCES( I ) ) THEN

*  Map the arrays.
            CALL KPG1_MAP( NDFI, COMP( I ), ITYPE, 'READ', PNTRI, EL,
     :                    STATUS )
            CALL KPG1_MAP( NDFO, COMP( I ), ITYPE, 'WRITE', PNTRO, EL,
     :                    STATUS )

*  Process each data type directly.
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_RTOD( VAL__MINR ) )
               ELSE
                  MINREP = NUM_RTOD( VAL__MINR )
               END IF
               MAXREP = NUM_RTOD( VAL__MAXR )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               RSUVAL = VAL_DTOR( .FALSE., REPVAL, STATUS )
               RSDVAL = VAL_DTOR( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAR( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADR,
     :                             RSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.

                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAR( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADR, RSDVAL**2,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAR( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADR, RSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1R( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADR,
     :                               REPVAL, SIGMA, MINREP, MAXREP,
     :                               %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                               NREP, STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_BTOD( VAL__MINB ) )
               ELSE
                  MINREP = NUM_BTOD( VAL__MINB )
               END IF
               MAXREP = NUM_BTOD( VAL__MAXB )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               BSUVAL = VAL_DTOB( .FALSE., REPVAL, STATUS )
               BSDVAL = VAL_DTOB( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADB,
     :                             BSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.

                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAB( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADB, BSDVAL*BSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAB( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADB, BSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1B( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADB,
     :                               REPVAL, SIGMA, MINREP, MAXREP,
     :                               %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                               NREP, STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, VAL__MIND )
               ELSE
                  MINREP = VAL__MIND
               END IF
               MAXREP = VAL__MAXD
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAD( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADD,
     :                             REPVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAD( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADD, SIGMA*SIGMA,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAD( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADD, SIGMA,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1D( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADD,
     :                               REPVAL, SIGMA, MINREP, MAXREP,
     :                               %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                               NREP, STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_ITOD( VAL__MINI ) )
               ELSE
                  MINREP = NUM_ITOD( VAL__MINI )
               END IF
               MAXREP = NUM_ITOD( VAL__MAXI )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               ISUVAL = VAL_DTOI( .FALSE., REPVAL, STATUS )
               ISDVAL = VAL_DTOI( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAI( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADI,
     :                             ISUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAI( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADI, ISDVAL*ISDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAI( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADI, ISDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1I( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADI,
     :                               REPVAL, SIGMA, MINREP, MAXREP,
     :                               %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                               NREP, STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_KTOD( VAL__MINK ) )
               ELSE
                  MINREP = NUM_KTOD( VAL__MINK )
               END IF
               MAXREP = NUM_KTOD( VAL__MAXK )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               KSUVAL = VAL_DTOK( .FALSE., REPVAL, STATUS )
               KSDVAL = VAL_DTOK( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAK( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADK, KSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAK( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADK, KSDVAL * KSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAK( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADK, KSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1K( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADK,
     :                                REPVAL, SIGMA, MINREP, MAXREP,
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                NREP, STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_UBTOD( VAL__MINUB ) )
               ELSE
                  MINREP = NUM_UBTOD( VAL__MINUB )
               END IF
               MAXREP = NUM_UBTOD( VAL__MAXUB )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               BSUVAL = VAL_DTOUB( .FALSE., REPVAL, STATUS )
               BSDVAL = VAL_DTOUB( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAUB( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              VAL__BADUB,
     :                             BSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAUB( EL,
     :   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADUB, BSDVAL*BSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAUB( EL,
     :   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADUB, BSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1UB( EL,
     :                                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                 VAL__BADUB, REPVAL, SIGMA,
     :                                 MINREP, MAXREP,
     :                                 %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                 NREP,
     :                                 STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

* Define the acceptable range of values and the suggested default
* (which is not actually used due to the ppath).  If variance is being
* processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_UWTOD( VAL__MINUW ) )
               ELSE
                  MINREP = NUM_UWTOD( VAL__MINUW )
               END IF
               MAXREP = NUM_UWTOD( VAL__MAXUW )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               WSUVAL = VAL_DTOUW( .FALSE., REPVAL, STATUS )
               WSDVAL = VAL_DTOUW( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAUW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              VAL__BADUW,
     :                             WSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAUW( EL,
     :   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADUW, WSDVAL*WSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAUW( EL,
     :   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADUW, WSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1UW( EL,
     :                                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                 VAL__BADUW, REPVAL, SIGMA,
     :                                 MINREP, MAXREP,
     :                                 %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                 NREP,
     :                                 STATUS )
                  END IF

               END IF

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).  If variance is being
*  processed the minimum value cannot be negative.
               IF ( PROCES( 2 ) ) THEN
                  MINREP = MAX( 0.0D0, NUM_WTOD( VAL__MINW ) )
               ELSE
                  MINREP = NUM_WTOD( VAL__MINW )
               END IF
               MAXREP = NUM_WTOD( VAL__MAXW )
               DEFREP = 0.0D0

*  Get the replacement value.
               CALL PAR_GDR0D( 'REPVAL', DEFREP, MINREP, MAXREP,
     :                         .FALSE., REPVAL, STATUS )

*  Abort if an error has occurred. This clears the way for the check
*  on STATUS following the attempt to get a value for parameter SIGMA.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the standard deviation to use if replacing bad pixels with
*  random values (REPVAL will be used as the mean of the distribution).
*  SIGMA is restricted so that there is at least NSIGMA*SIGMA between
*  REPVAL and the nearest limit. A null value is treated as a request
*  for constant values.
               MAXSIG = MIN( REPVAL - MINREP, MAXREP - REPVAL )/NSIGMA

               CALL PAR_GDR0D( 'SIGMA', 0.0D0, 0.0D0, MAXSIG, .FALSE.,
     :                         SIGMA, STATUS )

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SIGMA = 0.0D0
               END IF

*  Convert the replacement value and sigma to the desired type.  Use
*  VAL_ to protect against potentionally harmful values when there is a
*  bad status.
               WSUVAL = VAL_DTOW( .FALSE., REPVAL, STATUS )
               WSDVAL = VAL_DTOW( .FALSE., SIGMA, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF. First deal with cases where bad values
*  are to be replaced by a constant value.
               IF ( SIGMA .EQ. 0.0D0 ) THEN
                  CALL KPG1_CHVAW( EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             VAL__BADW,
     :                             WSUVAL,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ), NREP,
     :                             STATUS )

*  Now deal with cases where random values are to be used.
               ELSE

*  If both VARIANCE and DATA components were specified, replace bad
*  VARIANCE values with a constant equal to SIGMA**2, and ERROR values
*  with a constant equal to SIGMA.
                  IF ( LCOMP( 1 ) .AND. LCOMP( 2 ) .AND.
     :                 COMP( I ) .NE. 'Data' ) THEN

                     IF ( COMP( I ) .EQ. 'Variance' ) THEN
                        CALL KPG1_CHVAW( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADW, WSDVAL*WSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )

                     ELSE IF ( COMP( I ) .EQ. 'Error' ) THEN
                        CALL KPG1_CHVAW( EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   VAL__BADW, WSDVAL,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   NREP, STATUS )
                     END IF

*  Otherwise, replace bad values with random samples taken from a
*  normal distribution.
                  ELSE
                     CALL KPS1_NOM1W( EL,
     :                                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                VAL__BADW, REPVAL, SIGMA,
     :                                MINREP, MAXREP,
     :                                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                NREP,
     :                                STATUS )
                  END IF

               END IF

            END IF

*  Unmap the arrays. Note, COMP( I ) cannot be used to identify the
*  component to be unmapped because NDF_UNMAP does not accept 'Error'
*  as a component name.
            CALL NDF_UNMAP( NDFI, '*', STATUS )
            CALL NDF_UNMAP( NDFO, '*', STATUS )

*  Report the number of replacements in the array.
            IF ( STATUS .EQ. SAI__OK ) THEN

               IF ( FIRST ) CALL MSG_BLANK( STATUS )
               CALL MSG_SETC( 'COMPS', COMP( I ) )

               IF ( NREP .NE. 1 ) THEN
                  CALL MSG_SETI( 'NREP', NREP )
                  CALL MSG_OUTIF( MSG__NORM, 'REPLACE', '  ^NREP bad '//
     :                            'values were replaced in the ^COMPS'//
     :                            ' array.', STATUS )

               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'REPLACE', '  1 bad '//
     :                            'value was replaced in the ^COMPS '//
     :                            'array.', STATUS )
               END IF

            END IF

            FIRST = .FALSE.

         END IF

*  End of loop for the arrays.
      END DO

      CALL MSG_BLANK( STATUS )

*  Come here if a specific error occurred early in the application.
  999 CONTINUE

*  Write an error report if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NOMAGIC_ERR',
     :     'NOMAGIC: Unable to modify the bad-pixel values.', STATUS )
      END IF

*  Close the NDF and free NDF resources.
      CALL NDF_END( STATUS )

*  End
      END
