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

*     Two compression methods are available: SCALE or DELTA (see
*     Parameter METHOD).

*  Usage:
*     ndfcompress in out method

*  ADAM Parameters:
*     DSCALE = _DOUBLE (Read)
*        The scale factor to use for the DATA component, when
*        compressing with METHOD set to "SCALE".  If a null (!) value is
*        supplied for DSCALE or DZERO, default values will be used for
*        both that cause the scaled data values to occupy 96% of the
*        available range of the data type selected using Parameter
*        SCALEDTYPE.  [!]
*     DZERO = _DOUBLE (Read)
*        The zero offset to use for the DATA component, when compressing
*        with METHOD set to SCALE.  If a null (!) value is supplied for
*        DSCALE or DZERO, default values will be used for both that
*        cause the scaled data values to occupy 96% of the available
*        range of the data type selected using Parameter SCALEDTYPE.  [!]
*     IN = NDF (Read)
*        The input NDF.
*     METHOD = LITERAL (Read)
*        The compression method to use.  The options are as follows.
*
*        - "BOTH" -- A lossy compression scheme for all data types.
*        It first creates an intermediate NDF from the supplied NDF using
*        "SCALED" compression and then creates the final ouput NDF by
*        applying "DELTA" compression to the intermediate NDF. The
*        intermediate NDF is then deleted.
*
*        - "SCALED" -- A lossy compression scheme for all data types.
*        See "Scaled Compression" below, and Parameters DSCALE, DZERO,
*        VSCALE, VZERO, and SCALEDTYPE.
*
*        - "DELTA" -- A lossless compression scheme for integer data
*        types. See "Delta Compression" below, and Parameters ZAXIS,
*        ZMINRATIO, and ZTYPE.
*
*        The current value is the default, which is initially "DELTA".
*        []
*     OUT = NDF (Write)
*        The output NDF.
*     SCALEDTYPE = LITERAL (Read)
*        The data type to use for the scaled data values.  It is only
*        used if METHOD is "SCALED".  It can be one of the following
*        options.
*
*        - "_INTEGER" -- four-byte signed integers
*
*        - "_WORD" -- two-byte signed integers
*
*        - "_UWORD" -- two-byte unsigned integers
*
*        - "_BYTE" -- one-byte signed integers
*
*        - "_UBYTE" -- one-byte unsigned integers
*
*        The same data type is used for both DATA and (if required)
*        VARIANCE components of the output NDF.  The initial default
*        value is "_WORD".   [current value]
*     VSCALE = _DOUBLE (Read)
*        The scale factor to use for the VARIANCE component, when
*        compressing with METHOD set to SCALE. If a null (!) value is
*        supplied for VSCALE or VZERO, default values will be used for
*        both that cause the scaled variance values to occupy 96% of
*        the available range of the data type selected using Parameter
*        SCALEDTYPE.  [!]
*     VZERO = _DOUBLE (Read)
*        The zero factor to use for the VARIANCE component, when
*        compressing with METHOD set to SCALE.  If a null (!) value is
*        supplied for VSCALE or VZERO, default values will be used for
*        both that cause the scaled variance values to occupy 96% of
*        the available range of the data type selected using Parameter
*        SCALEDTYPE.  [!]
*     ZAXIS = _INTEGER (Read)
*        The index of the pixel axis along which differences are to be
*        taken, when compressing with METHOD set to "DELTA". If this is
*        zero, a default value will be selected that gives the greatest
*        compression.  [0]
*     ZMINRATIO = _REAL (Read)
*        The minimum allowed compression ratio for an array (the ratio
*        of the supplied array size to the compressed array size), when
*        compressing with METHOD set to "DELTA". If compressing an array
*        results in a compression ratio smaller than or equal to
*        ZMINRATIO, then the array is left uncompressed in the new NDF.
*        If the supplied value is zero or negative, then each array will
*        be compressed regardless of the compression ratio.  [1.0]
*     ZTYPE = LITERAL (Read)
*        The data type to use for storing differences between adjacent
*        uncompressed data values, when compressing with METHOD set to
*        "DELTA".  Must be one of _INTEGER, _WORD, _BYTE or blank. If a null
*        (!) value or blank value is supplied, the data type that gives
*        the best compression is determined and used.  [!]

*  Scaled Compression:
*     The SCALE compression method scales the supplied data values using a
*     linear transformation so that they fit into a smaller (integer) data
*     type. A description of the scaling uses is stored with the output NDF
*     so that later application can reconstruct the original unscaled values.
*     This method is not lossless, due to the truncation involved in
*     converting floatign point values to integers.

*  Delta Compression:
*     DELTA compression is lossless, but can only be used on integer
*     values. It assumes that adjacent integer values in the input tend to
*     be close in value, and so differences between adjacent values can be
*     represented in fewer bits than the absolute values themselves. The
*     differences are taken along a nominated pixel axis within the
*     supplied array (specified by Parameter ZAXIS). Any input value that
*     differs from its earlier neighbour by more than the data range of
*     the selected data type is stored explicitly using the data type of
*     the input array.
*
*     Further compression is achieved by replacing runs of equal input
*     values by a single occurrence of the value with a corresponding
*     repetition count.
*
*     It should be noted that the degree of compression achieved is
*     dependent on the nature of the data, and it is possible for a
*     compressed array to occupy more space than the uncompressed array.
*     The mean compression factor actually achieved is displayed (the
*     ratio of the supplied NDF size to the compressed NDF size).
*
*     It is possible to delta compress an NDF that has already been
*     scale compressed. This provides a means of further compressing
*     floating-point arrays. However, note that the default values
*     supplied for DSCALE, DZERO, VSCALE, and VZERO may not be
*     appropriate as they are chosen to maximise the spread of the
*     scaled integer values in order to minimise the integer truncation
*     error, but delta compression works best on arrays of integers in
*     which the spread of values is small.
*
*     If the input NDF is already DELTA compressed, it will be
*     uncompressed and then recompressed using the supplied parameter values.
*
*     More details of delta compression can be found in SUN/11 ("ARY - A
*     Subroutine Library for Accessing ARRAY Data Structures"), subsection
*     "Delta Compressed Array Form".

*  Examples:
*     ndfcompress infile outfile scale scaledtype=_uword
*        Copies the contents of the NDF structure infile to the new
*        structure outfile, scaling the values so that they fit into
*        unsigned two-byte integers. The scale and zero values used are
*        chosen automatically.

*  Related Applications:
*     KAPPA: NDFCOPY.

*  Implementation Status:
*     The TITLE, LABEL, UNITS, DATA, VARIANCE, QUALITY, AXIS, WCS, and
*     HISTORY components are copied by this routine, together with all
*     extensions.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.
*     12-OCT-2010 (DSB):
*        Change docs to use correct parameter name SCALEDTYPE, rather
*        than SCALETYPE.
*     26-OCT-2010 (DSB):
*        If integer trunction would result in a scale factor of zero being
*        used, use unity instead.
*     3-NOV-2010 (DSB):
*        Include support for delta compressed arrays.
*     21-MAY-2021 (DSB):
*        Added method "BOTH".
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
      CHARACTER COMP(3)*8        ! NDF array component names
      CHARACTER COMPS*30         ! List of NDF components to compress
      CHARACTER FORM*(NDF__SZFRM)! Array storage form
      CHARACTER CMPRS( 2 )*5     ! Explicit compression method
      CHARACTER METHOD*5         ! Compression method
      CHARACTER SCLPAR(2)*6      ! Parameters for getting scale values
      CHARACTER STYPE*(DAT__SZTYP)! Numerical type of scaled array
      CHARACTER TYPE*(DAT__SZTYP)! Numerical type of input array
      CHARACTER ZERPAR(2)*6      ! Parameters for getting zero values
      CHARACTER ZTYPE*(DAT__SZTYP)! Numerical type for storing differences
      DOUBLE PRECISION MAXVAL    ! Max value in array
      DOUBLE PRECISION MINVAL    ! Min value in array
      DOUBLE PRECISION SCALE     ! Scale factor
      DOUBLE PRECISION VMN       ! Min value for scaled data type
      DOUBLE PRECISION VMX       ! Max value for scaled data type
      DOUBLE PRECISION ZERO      ! Zero offset
      INTEGER DIMS( NDF__MXDIM ) ! Input NDF dimensions
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER I                  ! Loop index
      INTEGER IAT                ! Number of characters in the string
      INTEGER ICOMP              ! Component index
      INTEGER ICMPR              ! Compression  method index
      INTEGER INDF1              ! Input NDF identifier
      INTEGER INDF2              ! Template NDF identifier
      INTEGER INDFIN             ! Input NDF for current compression
      INTEGER IP1                ! Pointer to mapped input array
      INTEGER IP2                ! Pointer to mapped output array
      INTEGER MAXPOS             ! Index of elements with max value
      INTEGER MINPOS             ! Index of elements with min value
      INTEGER NBAD               ! Number of unaccommodated input pixels
      INTEGER NCMPR              ! No. of compressions to apply
      INTEGER NDIM               ! Number of input NDF dimensions
      INTEGER NINVAL             ! Number of bad values found
      INTEGER PLACE              ! Placeholder for current compression output
      INTEGER PLACE2             ! Placeholder for final output NDF
      INTEGER ZAXIS              ! Delta compression axis
      LOGICAL BAD                ! Bad values in input array component?
      LOGICAL BADOUT             ! Bad values in output array component?
      LOGICAL THERE              ! Does object exists?
      REAL MINRAT                ! Min. acceptable compression ratio
      REAL ZRATIO                ! Compression ratio

      DATA COMP   / 'DATA',   'VARIANCE', 'QUALITY' /
      DATA SCLPAR / 'DSCALE', 'VSCALE' /
      DATA ZERPAR / 'DZERO',  'VZERO' /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Obtain a placeholder for the output NDF.
      CALL LPG_CREPL( 'OUT', PLACE2, STATUS )

*  Obtain the compression method.
      CALL PAR_CHOIC( 'METHOD', METHOD, 'SCALE,DELTA,BOTH', .TRUE.,
     :                 METHOD, STATUS )

*  Store the required compressions
      IF( METHOD .EQ. 'BOTH' ) THEN
         NCMPR = 2
         CMPRS( 1 ) = 'SCALE'
         CMPRS( 2 ) = 'DELTA'
      ELSE
         NCMPR = 1
         CMPRS( 1 ) = METHOD
      END IF

*  Loop over the required compression methods.
      DO ICMPR = 1, NCMPR

*  Note the identifier for the NDF containing the data to be compressed.
*  On the first pass it is the input NDF specified by parameter IN. On
*  subsequent passes it is the output from the previous pass.
         IF( ICMPR .EQ. 1 ) THEN
            INDFIN = INDF1
         ELSE
            INDFIN = INDF2
            INDF2 = NDF__NOID
         END IF

*  Get a placeholder for a new NDF to receive the output from the
*  current compression. The output from the final compression goes into
*  the NDF specified by parameter OUT. Earlier compressions go into
*  temporary NDFs.
         IF( ICMPR .EQ. NCMPR ) THEN
            PLACE = PLACE2
         ELSE
            CALL NDF_TEMP( PLACE, STATUS )
         END IF

*  Tell the user which compression is being performed.
         IF( NCMPR .GT. 1 ) THEN
            CALL MSG_SETC( 'C', CMPRS( ICMPR ) )
            CALL MSG_OUT( ' ','Performing ^C compression...', STATUS )
         END IF

*  First deal with SCALE compression.
         IF( CMPRS( ICMPR ) .EQ. 'SCALE' ) THEN

*  Create the output NDF by propagating everything except the Data and
*  Variance arrays.
            CALL NDF_SCOPY( INDFIN, 'Title,Label,Units,Quality,Axis,' //
     :                     'History,WCS', PLACE, INDF2, STATUS )

*  Get the data type for the scaled values in the output NDF.
            CALL PAR_CHOIC( 'SCALEDTYPE', STYPE, '_INTEGER,_WORD,'//
     :                      '_UWORD,_BYTE,_UBYTE', .TRUE., STYPE,
     :                      STATUS )

*  Set the output NDF to have the selected data type.
            CALL NDF_STYPE( STYPE, INDF2, 'Data,Variance', STATUS )

*  Loop round the Data and Variance arrays.
            CALL MSG_BLANK( STATUS )
            DO I = 1, 2

*  Pass on if the component is not defined in the input NDF.
               CALL NDF_STATE( INDFIN, COMP( I ), THERE, STATUS )
               IF( THERE ) THEN

*  Get the data type of the input NDF array component.
                  CALL NDF_TYPE( INDFIN, COMP( I ), TYPE, STATUS )

*  Map the input array, and see if there may be any bad values in the
*  mapped array.
                  CALL NDF_MAP( INDFIN, COMP( I ), TYPE, 'READ', IP1,
     :                          EL, STATUS )
                  CALL NDF_BAD( INDFIN, COMP( I ), .FALSE., BAD,
     :                          STATUS )

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
                     CALL KPG1_MXMNX( TYPE, BAD, EL, IP1, NINVAL,
     :                                MAXVAL, MINVAL, MAXPOS, MINPOS,
     :                                STATUS )

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
     :                                'Unsupported data type ''^T'' '//
     :                                '(programming error).', STATUS )
                     END IF

*  Calculate the "wide" default scale and zero values so that the scaled
*  values cover 96% of the available scaled data range.
                     IF( MAXVAL .NE. MINVAL ) THEN
                        SCALE = ( MAXVAL - MINVAL )/
     :                          ( 0.96*( VMX - VMN ) )
                     ELSE
                        SCALE = 1.0D0
                     END IF

*  Calculate the ZERO that causes the middle input data value to be mapped
*  onto the mid point of the avilable output data range.
                     ZERO = 0.5*( MAXVAL + MINVAL ) -
     :                      SCALE*0.5*( VMX + VMN )

                  END IF

*  Convert the scale and zero to the data type of the output NDF, and
*  tell the user what values are being used.
                  IF( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL MSG_SETD( 'S', SCALE )
                     CALL MSG_SETD( 'Z', ZERO )

                  ELSE IF( TYPE .EQ. '_REAL' ) THEN
                     SCALE = REAL( SCALE )
                     IF( SCALE .EQ. 0.0 ) SCALE = 1.0
                     ZERO = REAL( ZERO )
                     CALL MSG_SETR( 'S', REAL( SCALE ) )
                     CALL MSG_SETR( 'Z', REAL( ZERO ) )

                  ELSE
                     SCALE = NINT( SCALE )
                     ZERO = NINT( ZERO )
                     IF( SCALE .EQ. 0 ) SCALE = 1
                     CALL MSG_SETI( 'S', NINT( SCALE ) )
                     CALL MSG_SETI( 'Z', NINT( ZERO ) )

                  END IF

                  CALL MSG_SETC( 'C', COMP( I ) )
                  CALL MSG_OUT( 'NDFCOMP_MSG1', '  ^C component: '//
     :                             'using Scale=^S   Zero=^z', STATUS )

*  Map the output array into which will be put the scaled values.
                  CALL NDF_MAP( INDF2, COMP( I ), STYPE, 'WRITE', IP2,
     :                          EL, STATUS )

*  Copy the input array to the output array, scaling them in the
*  process.
                  CALL KPG1_SCALX( SCALE, ZERO, BAD, EL, TYPE, IP1,
     :                             STYPE, IP2, BADOUT, NBAD, STATUS )

*  Unmap the output array, since the scale and zero values cannot be
*  set if the array is mapped.
                  CALL NDF_UNMAP( INDF2, COMP( I ), STATUS )

*  Set the output bad-pixel flag.
                  CALL NDF_SBAD( BADOUT, INDF2, COMP( I ), STATUS )

*  Give a warning if any input pixel values did not fit into the output
*  data range.
                  IF( NBAD .EQ. 1 ) THEN
                     CALL MSG_OUT( 'NDFCOMP_MSG2', '  One pixel did '//
     :                            'not fit into the scaled data range.',
     :                             STATUS )
                     CALL MSG_BLANK( STATUS )

                  ELSE IF( NBAD .GT. 1 ) THEN
                     CALL MSG_SETI( 'N', NBAD )
                     CALL MSG_OUT( 'NDFCOMP_MSG3', '  ^N pixels did '//
     :                            'not fit into the scaled data range.',
     :                             STATUS )
                     CALL MSG_BLANK( STATUS )
                  END IF

*  Store the scale and zero terms in the output NDF. This will convert
*  its storage form from simple to scaled, and change the data type of
*  the NDF to the data type of the scale and zero terms.
                  IF( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL NDF_PTSZD( SCALE, ZERO, INDF2, COMP( I ),
     :                               STATUS )

                  ELSE IF( TYPE .EQ. '_REAL' ) THEN
                     CALL NDF_PTSZR( VAL_DTOR( .FALSE., SCALE, STATUS ),
     :                              VAL_DTOR( .FALSE., ZERO, STATUS ),
     :                              INDF2, COMP( I ), STATUS )

                  ELSE
                     CALL NDF_PTSZI( VAL_DTOI( .FALSE., SCALE, STATUS ),
     :                              VAL_DTOI( .FALSE., ZERO, STATUS ),
     :                              INDF2, COMP( I ), STATUS )
                  END IF

               END IF
            END DO
            CALL MSG_BLANK( STATUS )

*  Now deal with DELTA compression.
         ELSE IF( CMPRS( ICMPR ) .EQ. 'DELTA' ) THEN

*  First form a comma-separated list of the components to be compressed.
*  Loop round each possible array component. Skip components that do no
*  exist in the NDF or contain floating point data. For scaled arrays,
*  the data type used is the internal (scaled) data type.
            COMPS = ' '
            IAT = 0
            DO ICOMP = 1, 3
               CALL NDF_STATE( INDFIN, COMP( ICOMP ), THERE, STATUS )
               IF( THERE ) THEN

                  CALL NDF_FORM( INDFIN, COMP( ICOMP ), FORM, STATUS )
                  IF( FORM .EQ. 'SCALED' .AND. ICOMP .NE. 3 ) THEN
                     CALL NDF_SCTYP( INDFIN, COMP( ICOMP ), TYPE,
     :                               STATUS )
                  ELSE
                     CALL NDF_TYPE( INDFIN, COMP( ICOMP ), TYPE,
     :                              STATUS )
                  END IF

                  IF( TYPE .NE. '_REAL' .AND. TYPE .NE. '_DOUBLE' ) THEN
                     IF( IAT .GT. 0 ) CALL CHR_APPND( ',', COMPS, IAT )
                     CALL CHR_APPND( COMP( ICOMP ), COMPS, IAT )
                  END IF

               END IF
            END DO

*  Report an error if there is nothing to compress.
            IF( IAT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'N', INDFIN )
               CALL ERR_REP( ' ', 'The NDF ''^N'' contains floating '//
     :                       'point data and so cannot be DELTA '//
     :                       'compressed.', STATUS )
               GO TO 999
            END IF

*  Get the data type in which to store the differences between adjacent
*  uncompressed data values. Use a blank default which causes NDF_ZSCAL
*  to choose the best data type.
            ZTYPE = ' '
            CALL PAR_CHOIC( 'ZTYPE', ZTYPE, '_INTEGER,_WORD,_BYTE,',
     :                      .TRUE., ZTYPE, STATUS )

*  Get the minimum acceptable compression ratio.
            CALL PAR_GET0R( 'ZMINRATIO', MINRAT, STATUS )

*  Get the compression axis. Use a zero default which causes NDF_ZSCAL
*  to choose the best compression axis.
            CALL NDF_DIM( INDFIN, NDF__MXDIM, DIMS, NDIM, STATUS )
            CALL PAR_GDR0I( 'ZAXIS', 0, 0, NDIM, .TRUE., ZAXIS, STATUS )

*  Create a copy of the input NDF in which the array components selected
*  above are delta compressed.
            CALL NDF_ZDELT( INDFIN, COMPS( : IAT ), MINRAT, ZAXIS,
     :                      ZTYPE, PLACE, INDF2, ZRATIO, STATUS )

*  Report details of the compression applied to each array component in
*  the output NDF.
            CALL MSG_BLANK( STATUS )
            CALL NDF_MSG( 'N', INDF2 )
            CALL MSG_OUT( ' ', '   Details of delta compression in ^N:',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )

            DO ICOMP = 1, 3
               CALL NDF_STATE( INDF2, COMP( ICOMP ), THERE, STATUS )
               IF( THERE ) THEN

                  CALL CHR_LCASE( COMP( ICOMP )( 2 : ) )
                  CALL MSG_SETC( 'T', COMP( ICOMP ) )
                  CALL MSG_OUT( ' ', '   ^T:', STATUS )

                  CALL NDF_GTDLT( INDF2, COMP( ICOMP ), ZAXIS, ZTYPE,
     :                            ZRATIO, STATUS )
                  IF( ZAXIS .GT. 0 ) THEN
                     CALL MSG_SETI( 'T', ZAXIS )
                     CALL MSG_OUT( ' ', '      Compression axis: ^T',
     :                             STATUS )
                     CALL MSG_SETC( 'T', ZTYPE )
                     CALL MSG_OUT( ' ', '      Compressed data type:'//
     :                             ' ^T', STATUS )
                     CALL MSG_SETR( 'T', ZRATIO )
                     CALL MSG_OUT( ' ', '      Compression ratio: ^T',
     :                             STATUS )
                  ELSE
                     CALL MSG_OUT( ' ', '      Uncompressed', STATUS )
                  END IF

               END IF
            END DO

            CALL MSG_BLANK( STATUS )

*  Report an error if the compression method is not recognised.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'M', CMPRS( ICMPR ) )
            CALL ERR_REP( 'NDFCOMP_ERR3', 'The ^M method has not yet '//
     :                    'been implemented.', STATUS )
         END IF

*  If the input to the compression just completed was a temporary NDF,
*  delete it.
         IF( ICMPR .GT. 1 ) CALL NDF_DELET( INDFIN, STATUS )

      END DO

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
