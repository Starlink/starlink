      SUBROUTINE THRESH ( STATUS )
*+
*  Name:
*     THRESH

*  Purpose:
*     Edits an NDF to replace values between or outside given limits
*     with specified constant values.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL THRESH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates an output NDF by copying values from an
*     input NDF, replacing all values within given data ranges by
*     a user-specified constant or by the bad value. Upper and lower
*     thresholds are supplied using parameters THRLO and THRHI.
*
*     If THRLO is less than or equal to THRHI, values between and
*     including the two thresholds are copied from the input to output
*     array.  Any values in the input array greater than the upper
*     threshold will be set to the value of parameter NEWHI, and anything
*     less than the lower threshold will be set to the value of parameter
*     NEWLO, in the output data array. Thus the output NDF is constrained
*     to lie between the two bounds.
*
*     If THRLO is greater than THRHI, values greater than or equal to
*     THRLO are copied from the input to output array, together with
*     values less than or equal to THRHI.  Any values between THRLO and
*     THRHI will be set to the value of parameter NEWLO in the output NDF.
*
*     Each replacement value may be the bad-pixel value for masking.

*  Usage:
*     thresh in out thrlo thrhi newlo newhi [comp]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The components whose values are to be constrained between
*        thresholds.  The options are limited to the arrays within the
*        supplied NDF.  In general the value may be "Data", "Quality",
*        "Error", or "Variance".  If "Quality" is specified, then the
*        quality values are treated as numerical values in the range 0
*        to 255.  ["Data"]
*     IN = NDF  (Read)
*        Input NDF structure containing the array to have thresholds
*        applied.
*     NEWHI = LITERAL (Read)
*        This gives the value to which all input array-element values
*        greater than the upper threshold are set.  If this is set to
*        "Bad", the bad value is substituted.  Numerical values of
*        NEWHI must lie in within the minimum and maximum values of the
*        data type of the array being processed.  The suggested default
*        is the upper threshold. This parameter is ignored if THRLO is
*        greater than THRHI.
*     NEWLO = LITERAL (Read)
*        This gives the value to which all input array-element values
*        less than the lower threshold are set.  If this is set to
*        "Bad", the bad value is substituted.  Numerical values of
*        NEWLO must lie in within the minimum and maximum values of the
*        data type of the array being processed.  The suggested default
*        is the lower threshold.
*     NUMHI = _INTEGER (Write)
*        The number of pixels whose values were thresholded as being
*        greater than the THRHI threshold.
*     NUMLO = _INTEGER (Write)
*        The number of pixels whose values were thresholded as being
*        less than the THRLO threshold.
*     NUMRANGE = _INTEGER (Write)
*        The number of pixels whose values were thresholded as being
*        between the THRLO and THRHI thresholds, if THRLO is greater
*        than THRHI.
*     NUMSAME = _INTEGER (Write)
*        The number of unchanged pixels.
*     OUT = NDF (Write)
*        Output NDF structure containing the thresholded version of
*        the array.
*     THRHI = _DOUBLE (Read)
*        The upper threshold value within the input array.  It must lie
*        in within the minimum and maximum values of the data type of
*        the array being processed.  The suggested default is the
*        current value.
*     THRLO = _DOUBLE (Read)
*        The lower threshold value within the input array.  It must lie
*        within the minimum and maximum values of the data type of
*        the array being processed.  The suggested default is the
*        current value.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     thresh zzcam zzcam2 100 500 0 0
*        This copies the data array in the NDF called zzcam to the NDF
*        called zzcam2.  Any data value less than 100 or greater than
*        500 in zzcam is set to 0 in zzcam2.
*     thresh zzcam zzcam2 500 100 0
*        This copies the data array in the NDF called zzcam to the NDF
*        called zzcam2.  Any data value less than 500 and greater than
*        100 in zzcam is set to 0 in zzcam2.
*     thresh zzcam zzcam2 100 500 0 0 comp=Variance
*        As above except that the data array is copied unchanged and the
*        thresholds apply to the variance array.
*     thresh n253 n253cl thrlo=-0.5 thrhi=10.1 \
*        This copies the data array in the NDF called n253 to the NDF
*        called n253cl.  Any data value less than -0.5 in n253 is set
*        to -0.5 in n253cl, and any value greater than 10.1 in n253
*        becomes 10.1 in n253cl.
*     thresh pavo pavosky -0.02 0.02 bad bad
*        All data values outside the range -0.02 to 0.02 in the NDF
*        called pavo become bad in the NDF called pavosky.  All values
*        within this range are copied from pavo to pavosky.

*  Related Applications:
*     KAPPA: HISTEQ, MATHS; Figaro: CLIP, IDIFF, RESCALE.

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
*     Copyright (C) 1996, 1998, 2000-2001, 2004 Central Laboratory of
*     the Research Councils. All Rights Reserved.

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
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 November 6 (MJC):
*        Original NDF_ version.
*     1994 September 26 (MJC):
*        TITLE propagated from input NDF by default.  Lowercase examples
*        and usage.
*     1996 April 11 (MJC):
*        Use the bad-pixel flag.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     6-DEC-2000 (DSB):
*        Allow THRLO to be greater than THRHI in order to exclude a range
*        of data values.
*     6-JUL-2001 (DSB):
*        Correct setting of bad pixel flag in output.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     11-MAY-2009 (BRADC):
*        Return number of changed and unchanged pixels via parameters.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Magic-value constants
      INCLUDE 'MSG_PAR'        ! Message constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL BAD              ! Either of the replacement values were
                               ! bad
      BYTE BNEWHI              ! New value for pixels above THRHI
      BYTE BNEWLO              ! New value for pixels below THRLO
      LOGICAL BPFLAG           ! Bad-pixel flag
      BYTE BTHRHI              ! Upper threshold value
      BYTE BTHRLO              ! Lower threshold value
      CHARACTER * ( 28 ) COMLIS ! List of available array components
      INTEGER COMLN            ! Length of component list
      CHARACTER * ( 8 ) COMP   ! Component array to be modified
      INTEGER EL               ! Number of elements in an array
      DOUBLE PRECISION DNEWHI  ! New value for pixels above THRHI
      DOUBLE PRECISION DNEWLO  ! New value for pixels below THRLO
      DOUBLE PRECISION DTHRHI  ! Upper threshold value
      DOUBLE PRECISION DTHRLO  ! Lower threshold value
      INTEGER INEWHI           ! New value for pixels above THRHI
      INTEGER INEWLO           ! New value for pixels below THRLO
      INTEGER ITHRHI           ! Upper threshold value
      INTEGER ITHRLO           ! Lower threshold value
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the image
      CHARACTER * ( 8 ) MCOMP  ! Component array to be mapped
      INTEGER NDFI             ! Identifier for input NDF
      INTEGER NDFO             ! Identifier for output NDF
      INTEGER NREPHI           ! Number of replacements made above the
                               ! threshold
      INTEGER NREPLO           ! Number of replacements made below the
                               ! threshold
      INTEGER NUMSAME          ! Number of unchanged pixels
      INTEGER PNTRI( 1 )       ! Pointer to input array component
      INTEGER PNTRO( 1 )       ! Pointer to output array component
      REAL RNEWHI              ! New value for pixels above THRHI
      REAL RNEWLO              ! New value for pixels below THRLO
      REAL RTHRHI              ! Upper threshold value
      REAL RTHRLO              ! Lower threshold value
      INTEGER * 2 WNEWLO       ! New value for pixels below THRLO
      INTEGER * 2 WNEWHI       ! New value for pixels above THRHI
      INTEGER * 2 WTHRLO       ! Lower threshold value
      INTEGER * 2 WTHRHI       ! Upper threshold value

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
      CALL KPG1_ARCOL( NDFI, 'Data,Quality,Error,Variance', COMLIS,
     :                 COMLN, STATUS )

*  Find which component to plot.  No need to inquire the value, if the
*  only array component is Data.  Note the mixed-case returned in the
*  list is for attractive error reports.  See below why there is a
*  MCOMP.
      IF ( COMLIS .EQ. 'Data' ) THEN
         COMP = 'DATA'
         MCOMP = COMP
      ELSE
         CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ), .FALSE.,
     :                   COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP.
         MCOMP = COMP
         IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

      END IF

*  Create the output NDF.
*  ======================

*  Propagate the LABEL, WCS, HISTORY, UNITS and AXIS components from the
*  input NDF to the output, and the arrays not to be processed.
      IF ( COMP( 1:4 ) .EQ. 'DATA' ) THEN
         CALL LPG_PROP( NDFI, 'WCS,Variance,Quality,Units,Axis', 'OUT',
     :                  NDFO, STATUS )

      ELSE IF ( COMP .EQ. 'VARIANCE' ) THEN
         CALL LPG_PROP( NDFI, 'WCS,Data,Quality,Units,Axis', 'OUT',
     :                  NDFO, STATUS )

      ELSE IF ( COMP( 1:7 ) .EQ. 'QUALITY' ) THEN
         CALL LPG_PROP( NDFI, 'WCS,Data,Variance,Units,Axis', 'OUT',
     :                  NDFO, STATUS )

      END IF

*  Obtain a title and assign it to the output NDF.
*  ===============================================
*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Map the data arrays.
*  ====================
*
*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
      CALL NDF_TYPE( NDFI, COMP, ITYPE, STATUS )

*  Map the array components.
      CALL KPG1_MAP( NDFI, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL KPG1_MAP( NDFO, MCOMP, ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Find whether there may be bad pixels present.  There is no explicit
*  check.  It just relies on the current value.
      CALL NDF_BAD( NDFI, COMP, .FALSE., BPFLAG, STATUS )

*  Process the array using the appropriate implementation data type.
*  =================================================================
      IF ( ITYPE .EQ. '_REAL' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTR( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', RTHRLO,
     :                    RTHRHI, RNEWLO, RNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSR( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    RTHRLO,
     :                    RTHRHI, RNEWLO, RNEWHI,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTB( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', BTHRLO,
     :                    BTHRHI, BNEWLO, BNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSB( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    BTHRLO,
     :                    BTHRHI, BNEWLO, BNEWHI,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTD( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', DTHRLO,
     :                    DTHRHI, DNEWLO, DNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSD( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    DTHRLO,
     :                    DTHRHI, DNEWLO, DNEWHI,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTI( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', ITHRLO,
     :                    ITHRHI, INEWLO, INEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSI( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    ITHRLO,
     :                    ITHRHI, INEWLO, INEWHI,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTUB( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', BTHRLO,
     :                     BTHRHI, BNEWLO, BNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSUB( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     BTHRLO,
     :                     BTHRHI, BNEWLO, BNEWHI,
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTUW( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', WTHRLO,
     :                     WTHRHI, WNEWLO, WNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSUW( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     WTHRLO,
     :                     WTHRHI, WNEWLO, WNEWHI,
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     NREPLO, NREPHI, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Obtain the threshold parameters in the appropriate data type.
         CALL KPS1_THGTW( 'THRLO', 'THRHI', 'NEWLO', 'NEWHI', WTHRLO,
     :                    WTHRHI, WNEWLO, WNEWHI, BAD, STATUS )

*  Replace the values in the output array outside the range with the
*  new values, otherwise copy from the input to the output NDF.
         CALL KPG1_THRSW( BPFLAG, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    WTHRLO,
     :                    WTHRHI, WNEWLO, WNEWHI,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    NREPLO, NREPHI, STATUS )

      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  First report results when replacing data outside a given range.
         IF ( NREPHI .GE. 0 ) THEN

*  Report the number of replacements in the array that were below the
*  threshold.
            CALL MSG_SETC( 'COMPS', MCOMP )
            CALL MSG_SETI( 'NREP', NREPLO )
            IF ( NREPLO .NE. 1 ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           'There were ^NREP elements changed in the ^COMPS '/
     :           /'array below the threshold.', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           'There was ^NREP element changed in the ^COMPS '/
     :           /'array below the threshold.', STATUS )
            END IF

*  Write the number of replacements to the output parameter.
            CALL PAR_PUT0I( 'NUMLO', NREPLO, STATUS )

*  Report the number of replacements in the array that were above the
*  threshold.
            CALL MSG_SETC( 'COMPS', MCOMP )
            CALL MSG_SETI( 'NREP', NREPHI )
            IF ( NREPHI .NE. 1 ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           'There were ^NREP elements changed in the ^COMPS '/
     :           /'array above the threshold.', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           'There was ^NREP element changed in the ^COMPS '/
     :           /'array above the threshold.', STATUS )
            END IF

*  Write the number of replacements to the output parameter.
            CALL PAR_PUT0I( 'NUMHI', NREPHI, STATUS )

*  Calculate and write the number of unchanged pixels.
            NUMSAME = EL - NREPHI - NREPLO
            CALL PAR_PUT0I( 'NUMSAME', NUMSAME, STATUS )

*  Now report results when replacing data inside a given range.
         ELSE

*  Report the number of replacements in the array.
            CALL MSG_SETC( 'COMPS', MCOMP )
            CALL MSG_SETI( 'NREP', NREPLO )
            IF ( NREPLO .NE. 1 ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           '^NREP elements were changed in the ^COMPS '/
     :           /'array between the thresholds.', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'REPLACE',
     :           'One element was changed in the ^COMPS '/
     :           /'array between the thresholds.', STATUS )
            END IF

*  Write the number of replacements to the output parameter.
            CALL PAR_PUT0I( 'NUMRANGE', NREPLO, STATUS )

*  Calculate and write the number of unchanged pixels.
            NUMSAME = EL - NREPLO
            CALL PAR_PUT0I( 'NUMSAME', NUMSAME, STATUS )

         END IF

      END IF

*  If there may have been bad values substituted, set the bad-pixel
*  flag.
      IF ( COMP .NE. 'QUALITY' )
     :  CALL NDF_SBAD( BAD .OR. BPFLAG, NDFO, COMP, STATUS )

*  Write an error report if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'THRESH_ERR',
     :     'THRESH: Unable to create an NDF whose array values are '/
     :     /'constrained between thresholds.', STATUS )
      END IF

*  Come here if a specific error occurred early in the application.
  999 CONTINUE

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'THRESH_ERR',
     :     'THRESH: Error setting the values beyond thresholds in an '/
     :     /'NDF.', STATUS )
      END IF

      END
