      SUBROUTINE NDFCOMPRESS( STATUS )
*+
*  Name:
*     NDFCOMPRESS

*  Purpose:
*     Compresses an NDF so that it occupies less disk space.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFCOMPRESS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a copy of an NDF that occupies less disk
*     space. This compression does not affect the data values seen by
*     subsequent application, since all applications will automatically
*     uncompress the data.

*     Currently the only compression method available is to scale the
*     data values using a linear transformation so that they fit into a
*     smaller data type.  A description of the scaling uses is stored
*     with the output NDF so that later application can reconstruct the
*     original unscaled values.

*  Usage:
*     ndfcompress in out method

*  ADAM Parameters:
*     DSCALE = _DOUBLE (Read)
*        The scale factor to use for the DATA component, when
*        compressing with METHOD set to "SCALE".  If a null (!) value is
*        supplied for DSCALE or DZERO, default values will be used for
*        both that cause the scaled data values to occupy 96% of the
*        available range of the data type selected using parameter
*        SCALEDTYPE.  [!]
*     DZERO = _DOUBLE (Read)
*        The zero offset to use for the DATA component, when compressing
*        with METHOD set to SCALE.  If a null (!) value is supplied for
*        DSCALE or DZERO, default values will be used for both that
*        cause the scaled data values to occupy 96% of the available
*        range of the data type selected using parameter SCALEDTYPE.  [!]
*     IN = NDF (Read)
*        The input NDF.
*     METHOD = LITERAL (Read)
*        The compression method to use.  Currently the only supported
*        value is "SCALE".
*     OUT = NDF (Write)
*        The output NDF.
*     SCALEDTYPE = LITERAL (Read)
*        The data type to use for the scaled data values.  It is only
*        used if METHOD is "SCALED".  It can be one of the following
*        options.
*
*        - "_INTEGER" -- 4 byte signed integers
*
*        - "_WORD" -- 2 byte signed integers
*
*        - "_UWORD" -- 2 byte unsigned integers
*
*        - "_BYTE" -- 1 byte signed integers
*
*        - "_UBYTE" -- 1 byte unsigned integers
*
*        The same data type is used for both DATA and (if required)
*        VARIANCE components of the output NDF.  The initial default
*        value is "_WORD".   [current value]
*     VSCALE = _DOUBLE (Read)
*        The scale factor to use for the VARIANCE component, when
*        compressing with METHOD set to SCALE. If a null (!) value is
*        supplied for VSCALE or VZERO, default values will be used for
*        both that cause the scaled variance values to occupy 96% of
*        the available range of the data type selected using parameter
*        SCALEDTYPE.  [!]
*     VZERO = _DOUBLE (Read)
*        The zero factor to use for the VARIANCE component, when
*        compressing with METHOD set to SCALE.  If a null (!) value is
*        supplied for VSCALE or VZERO, default values will be used for
*        both that cause the scaled variance values to occupy 96% of
*        the available range of the data type selected using parameter
*        SCALEDTYPE.  [!]

*  Examples:
*     ndfcompress infile outfile scale scaledtype=_uword
*        Copies the contents of the NDF structure infile to the new
*        structure outfile, scaling the values so that they fit into
*        unsigned 2-byte integers. The scale and zero values used are
*        chosen automatically.

*  Related Applications:
*     KAPPA: NDFCOPY.

*  Implementation Status:
*     The TITLE, LABEL, UNITS, DATA, VARIANCE, QUALITY, AXIS, WCS, and
*     HISTORY components are copied by this routine, together with all
*     extensions.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.
*     12-OCT-2010 (DSB):
*        Change docs to use correct parameter name SCALEDTYPE, rather
*        than SCALETYPE.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION VAL_ITOD
      DOUBLE PRECISION VAL_WTOD
      DOUBLE PRECISION VAL_UWTOD
      DOUBLE PRECISION VAL_BTOD
      DOUBLE PRECISION VAL_UBTOD
      REAL VAL_DTOR
      INTEGER VAL_DTOI

*  Local Variables:
      CHARACTER COMP(2)*8        ! NDF array component names
      CHARACTER METHOD*5         ! Compression method
      CHARACTER SCLPAR(2)*6      ! Parameters for getting scale values
      CHARACTER STYPE*(DAT__SZTYP)! Numerical type of scaled array
      CHARACTER TYPE*(DAT__SZTYP)! Numerical type of input array
      CHARACTER ZERPAR(2)*6      ! Parameters for getting zero values
      DOUBLE PRECISION MAXVAL    ! Max value in array
      DOUBLE PRECISION MINVAL    ! Min value in array
      DOUBLE PRECISION SCALE     ! Scale factor
      DOUBLE PRECISION VMN       ! Min value for scaled data type
      DOUBLE PRECISION VMX       ! Max value for scaled data type
      DOUBLE PRECISION ZERO      ! Zero offset
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER I                  ! Loop index
      INTEGER INDF1              ! Input NDF identifier
      INTEGER INDF2              ! Template NDF identifier
      INTEGER IP1                ! Pointer to mapped input array
      INTEGER IP2                ! Pointer to mapped output array
      INTEGER MAXPOS             ! Index of elements with max value
      INTEGER MINPOS             ! Index of elements with min value
      INTEGER NINVAL             ! Number of bad values found
      INTEGER NBAD               ! Number of unaccomodated input pixels
      LOGICAL BAD                ! Bad values in input array component?
      LOGICAL BADOUT             ! Bad values in output array component?
      LOGICAL THERE              ! Does object exists?

      DATA COMP   / 'DATA',   'VARIANCE' /
      DATA SCLPAR / 'DSCALE', 'VSCALE' /
      DATA ZERPAR / 'DZERO',  'VZERO' /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Obtain the compression method.
      CALL PAR_CHOIC( 'METHOD', METHOD, 'SCALE', .TRUE., METHOD,
     :                STATUS )

*  First deal with SCALE compression.
      IF( METHOD .EQ. 'SCALE' ) THEN

*  Create the output NDF by propagating everything except the Data and
*  Variance arrays.
         CALL LPG_PROP( INDF1, 'Title,Label,Units,Quality,Axis,' //
     :                  'History,WCS', 'OUT', INDF2, STATUS )

*  Get the data type for the scaled values in the output NDF.
         CALL PAR_CHOIC( 'SCALEDTYPE', STYPE, '_INTEGER,_WORD,_UWORD,'//
     :                   '_BYTE,_UBYTE', .TRUE., STYPE, STATUS )

*  Set the output NDF to have the selected data type.
         CALL NDF_STYPE( STYPE, INDF2, 'Data,Variance', STATUS )

*  Loop round the Data and Variance arrays.
         CALL MSG_BLANK( STATUS )
         DO I = 1, 2

*  Pass on if the component is not defined in the input NDF.
            CALL NDF_STATE( INDF1, COMP( I ), THERE, STATUS )
            IF( THERE ) THEN

*  Get the data type of the input NDF array component.
               CALL NDF_TYPE( INDF1, COMP( I ), TYPE, STATUS )

*  Map the input array, and see if there may be any bad values in the
*  mapped array.
               CALL NDF_MAP( INDF1, COMP( I ), TYPE, 'READ', IP1, EL,
     :                       STATUS )
               CALL NDF_BAD( INDF1, COMP( I ), .FALSE., BAD, STATUS )

*  Check that no error has occurred.  This is so that we can be sure
*  that  any PAR__NULL error following the next two PAR_GET0D calls was
*  generated  by one of the two PAR calls.
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the scale and zero values from the user.
               CALL PAR_GET0D( SCLPAR( I ), SCALE, STATUS )
               CALL PAR_GET0D( ZERPAR( I ), ZERO, STATUS )

*  If a null value was supplied for either, annul the error and
*  calculate default values.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )

*  Get the max and min data values in the input array.
                  CALL KPG1_MXMNX( TYPE, BAD, EL, IP1, NINVAL, MAXVAL,
     :                             MINVAL, MAXPOS, MINPOS, STATUS )

*  Get the maximum and minimum data values representable by the selected
*  scaled data type.
                  IF( STYPE .EQ. '_INTEGER' ) THEN
                     VMX = VAL_ITOD( .FALSE., VAL__MAXI, STATUS )
                     VMN = VAL_ITOD( .FALSE., VAL__MINI, STATUS )

                  ELSE IF( STYPE .EQ. '_WORD' ) THEN
                     VMX = VAL_WTOD( .FALSE., VAL__MAXW, STATUS )
                     VMN = VAL_WTOD( .FALSE., VAL__MINW, STATUS )

                  ELSE IF( STYPE .EQ. '_UWORD' ) THEN
                     VMX = VAL_UWTOD( .FALSE., VAL__MAXUW, STATUS )
                     VMN = VAL_UWTOD( .FALSE., VAL__MINUW, STATUS )

                  ELSE IF( STYPE .EQ. '_BYTE' ) THEN
                     VMX = VAL_BTOD( .FALSE., VAL__MAXB, STATUS )
                     VMN = VAL_BTOD( .FALSE., VAL__MINB, STATUS )

                  ELSE IF( STYPE .EQ. '_UBYTE' ) THEN
                     VMX = VAL_UBTOD( .FALSE., VAL__MAXUB, STATUS )
                     VMN = VAL_UBTOD( .FALSE., VAL__MINUB, STATUS )

                  ELSE IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'T', TYPE )
                     CALL ERR_REP( 'NDFCOMP_ERR1', 'NDFCOMPRESS: '//
     :                             'Unsupported data type ''^T'' '//
     :                             '(programming error).', STATUS )
                  END IF

*  Calculate the default scale and zero values so that the scaled values
*  cover 96% of the available scaled data range.
                  IF( MAXVAL .NE. MINVAL ) THEN
                     SCALE = ( MAXVAL - MINVAL )/( 0.96*( VMX - VMN ) )
                  ELSE
                     SCALE = 1.0D0
                  END IF

                  ZERO = MAXVAL - SCALE*( VMX*0.98 + VMN*0.02 )

               END IF

*  Convert the scale and zero to the data type of the output NDF, and
*  tell the user what values are being used.
               IF( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL MSG_SETD( 'S', SCALE )
                  CALL MSG_SETD( 'Z', ZERO )

               ELSE IF( TYPE .EQ. '_REAL' ) THEN
                  SCALE = REAL( SCALE )
                  ZERO = REAL( ZERO )
                  CALL MSG_SETR( 'S', REAL( SCALE ) )
                  CALL MSG_SETR( 'Z', REAL( ZERO ) )

               ELSE
                  SCALE = NINT( SCALE )
                  ZERO = NINT( ZERO )
                  CALL MSG_SETI( 'S', NINT( SCALE ) )
                  CALL MSG_SETI( 'Z', NINT( ZERO ) )

               END IF

               CALL MSG_SETC( 'C', COMP( I ) )
               CALL MSG_OUT( 'NDFCOMP_MSG1', '  ^C component: '//
     :                          'using Scale=^S   Zero=^z', STATUS )

*  Map the output array into which will be put the scaled values.
               CALL NDF_MAP( INDF2, COMP( I ), STYPE, 'WRITE', IP2, EL,
     :                       STATUS )

*  Copy the input array to the output array, scaling them in the
*  process.
               CALL KPG1_SCALX( SCALE, ZERO, BAD, EL, TYPE, IP1,
     :                          STYPE, IP2, BADOUT, NBAD, STATUS )

*  Unmap the output array, since the scale and zero values cannot be
*  set if the array is mapped.
               CALL NDF_UNMAP( INDF2, COMP( I ), STATUS )

*  Set the output bad-pixel flag.
               CALL NDF_SBAD( BADOUT, INDF2, COMP( I ), STATUS )

*  Give a warning if any input pixel values did not fit into the output
*  data range.
               IF( NBAD .EQ. 1 ) THEN
                  CALL MSG_OUT( 'NDFCOMP_MSG2', '  One pixel did not '//
     :                          'fit into the scaled data range.',
     :                          STATUS )
                  CALL MSG_BLANK( STATUS )

               ELSE IF( NBAD .GT. 1 ) THEN
                  CALL MSG_SETI( 'N', NBAD )
                  CALL MSG_OUT( 'NDFCOMP_MSG3', '  ^N pixels did not '//
     :                          'fit into the scaled data range.',
     :                          STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF

*  Store the scale and zero terms in the output NDF. This will convert
*  its storage form from simple to scaled, and change the data type of
*  the NDF to the data type of the scale and zero terms.
               IF( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL NDF_PTSZD( SCALE, ZERO, INDF2, COMP( I ),
     :                            STATUS )

               ELSE IF( TYPE .EQ. '_REAL' ) THEN
                  CALL NDF_PTSZR( VAL_DTOR( .FALSE., SCALE, STATUS ),
     :                           VAL_DTOR( .FALSE., ZERO, STATUS ),
     :                           INDF2, COMP( I ), STATUS )

               ELSE
                  CALL NDF_PTSZI( VAL_DTOI( .FALSE., SCALE, STATUS ),
     :                           VAL_DTOI( .FALSE., ZERO, STATUS ),
     :                           INDF2, COMP( I ), STATUS )
               END IF

            END IF
         END DO
         CALL MSG_BLANK( STATUS )

*  Report an error if the compression method is not recognised.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'M', METHOD )
         CALL ERR_REP( 'NDFCOMP_ERR3', 'The ^M method has not yet '//
     :                 'been implemented.', STATUS )
      END IF

*  Tidy up
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFCOMP_ERR', 'NDFCOMPRESS: Error compressing'//
     :                 ' an NDF.', STATUS )
      END IF

      END
