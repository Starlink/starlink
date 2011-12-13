      SUBROUTINE CON_SPHEA( NDF, FD, DTYPE, FORM, NFLAGS, CMPTHE,
     :                      STATUS )
*+
*  Name:
*     CON_SPHEA

*  Purpose:
*     Writes the special headers derived from an NDF to a sequential
*     file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_SPHEA( NDF, FD, DTYPE, FORM, NFLAGS, CMPTHE, STATUS )

*  Description:
*     This routine serves NDF2ASCII and NDF2UNF.  It finds whether
*     certain special components appear in the NDF so they can be
*     written as FITS-like headers in the ASCII or binary file using
*     their corresponding FITS keywords.  A record of which items have
*     been set is returned.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     FD = INTEGER (Given)
*        File descriptor of the output ASCII file.
*     DTYPE = CHARACTER * ( * ) (Given)
*        The data type of the array being written to the ASCII file.
*     FORM = CHARACTER * ( * ) (Given)
*        The format of the file.  FORM = 'FORMATTED' gives an ASCII
*        formatted file; anything else results in an unformatted file.
*     NFLAGS = INTEGER (Given)
*        The number of flags used to indicate that certain NDF
*        components have been used to write descriptors to the BDF.
*        It should be set to 6.
*     CMPTHE( NFLAGS ) = LOGICAL (Returned)
*        The flags when set to true indicate that certain optional NDF
*        components have been used to write descriptors to the NDF.
*        In order they are 1) CRVARn, CDELTn, and CRPIXn 2) CTYPEn,
*        3) CUNITn, 4) TITLE, 5) LABEL, and 6) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The special keywords and how they are obtained is as follows:
*        -  BITPIX is derived from the type of the NDF data array.
*        -  NAXIS, and NAXISn are derived from the dimensions of
*           the NDF data array.
*        -  The TITLE, LABEL, and BUNIT descriptors are derived from
*           the TITLE, LABEL, and UNITS NDF components respectively.
*        -  The CDELTn, CRVALn, CTYPEn and CUNITn descriptors are
*           derived from a set of linear NDF AXIS structures.
*        -  The standard order of the FITS keywords is preserved.
*           No FITS comments are written following the values of the
*           above exceptions.
*
*     -  An extra header record with keyword UNSIGNED and logical value
*     T is added when the array data type is one of the HDS unsigned
*     integer types.  This is done because standard FITS does not
*     support unsigned integers, and allows (in conjunction with
*     BITPIX) applications reading the ASCII file to determine the data
*     type of the array.

*  Prior Requirements:
*     The ASCII file must be open for write access.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008 Science & Technology Facilities
*     Council. All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     1992 September 15 (MJC):
*        Original version.
*     1996 September 16 (MJC):
*        Corrected usage of CTYPEn (was CRTYPEn) and introduced CUNITn
*        for axis units.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF                ! NDF identifier
      INTEGER FD                 ! ASCII-file identifier
      CHARACTER*( * ) DTYPE      ! Data type of the array
      CHARACTER*( * ) FORM       ! Format of the file
      INTEGER   NFLAGS           ! Number of flags to indicate
                                 ! presence of certain components

*  Arguments Returned:
      LOGICAL CMPTHE( NFLAGS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string
                                 ! excluding trailing blanks

*  Local Constants:
      CHARACTER * 1 APS          ! Apostrophe
      PARAMETER ( APS = '''' )
      INTEGER   SZFITS           ! Size of FITS card-image
      PARAMETER( SZFITS = 80 )
      INTEGER   SZVAL            ! Size of header values
      PARAMETER( SZVAL = 20 )

*  Local Variables:
      INTEGER APNTR( NDF__MXDIM ) ! Pointers to NDF axis arrays
      CHARACTER*( NDF__SZTYP ) ATYPE ! Data type of the axis centres
      LOGICAL ASCII              ! True if output file is formatted
      LOGICAL AXIFND             ! NDF contains a linear axis comps.?
      LOGICAL AXLFND             ! NDF contains axis label?
      LOGICAL AXUFND             ! NDF contains axis units?
      INTEGER BITPIX             ! Number of bits per data value
      CHARACTER*1 C              ! Accommodates character string
      CHARACTER*( SZVAL ) CVALUE ! Accommodates header value
      DOUBLE PRECISION DEND      ! End value for an axis-centre array
      CHARACTER*( SZFITS ) DESCR ! Accommodates header name
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      DOUBLE PRECISION DSTART    ! Start value for an axis-centre
                                 ! array
      REAL END                   ! End value for an axis-centre array
      INTEGER FIOSTA             ! Fortran I/O status
      INTEGER I                  ! Loop variable
      REAL INCREM                ! Incremental value for axis array
      LOGICAL LABFND             ! NDF LABEL found?
      LOGICAL LINEAR             ! An axis is linear?
      INTEGER LUN                ! Logical-unit number for the output
                                 ! file
      INTEGER NCD                ! Length of a dimension in characters
      INTEGER NCHAR              ! Length of a character string
      INTEGER NCT                ! Length of a NDF character object
      INTEGER NDIM               ! Number of dimensions
      INTEGER NELM               ! Number of elements
      REAL START                 ! Start value for an axis-centre array
      LOGICAL THERE              ! NDF has FITS extension?
      LOGICAL TITFND             ! NDF TITLE found?
      LOGICAL UNTFND             ! NDF UNITS found?
      CHARACTER*( SZFITS ) VALUE ! NDF character value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output flags.
*  ========================
      AXIFND = .FALSE.
      TITFND = .FALSE.
      LABFND = .FALSE.
      UNTFND = .FALSE.

*  Find file-access attributes.
*  ============================

*  Determine the file format.
      ASCII = FORM .EQ. 'FORMATTED'

*  Obtain the logical-unit number associated with the file.
      IF ( .NOT. ASCII ) CALL FIO_UNIT( FD, LUN, STATUS )

*  Process BITPIX header.
*  ======================

*  Derive the value of BITPIX.
      CALL CON_BITPX( DTYPE, BITPIX, STATUS )

*  Convert to a character string.
      CALL CHR_ITOC( BITPIX, CVALUE, NCHAR )

*  Form a FITS-like card image for the ASCII file.  Right justify the
*  value.
      VALUE = ' '
      VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
      DESCR = 'BITPIX  = '//VALUE( :SZVAL )//' / Number of bits per '/
     :        /'data element'

      IF ( ASCII ) THEN

*  Write this header to the ASCII file.
         CALL FIO_WRITE( FD, DESCR, STATUS )
      ELSE

*  Write this header to the unformatted file.
         WRITE( LUN, IOSTAT=FIOSTA ) DESCR
         CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :     /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )
      END IF

*  Process NAXIS header.
*  =====================
*
*  Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  This is derived from the actual dimensionality of the NDF data
*  array.
      CALL CHR_ITOC( NDIM, CVALUE, NCHAR )

*  Form a FITS-like card image for the ASCII file.  Right justify the
*  value.
      VALUE = ' '
      VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
      DESCR = 'NAXIS   = '//VALUE( :SZVAL )//' / Number of dimensions'

      IF ( ASCII ) THEN

*  Write this header to the ASCII file.
         CALL FIO_WRITE( FD, DESCR, STATUS )
      ELSE

*  Write this header to the unformatted file.
         WRITE( LUN, IOSTAT=FIOSTA ) DESCR
         CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :     /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )
      END IF

*  Process AXISn headers.
*  ======================
*
*  Now the actual size of each dimension of the NDF data array is
*  written to the appropriate NAXISn headers.  Form a FITS-like card
*  image for the ASCII file.  Right justify the value.
      DO I = 1, NDIM
         CALL CHR_ITOC( DIMS( I ), CVALUE, NCHAR )
         VALUE = ' '
         VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
         DESCR = ' '
         CALL CHR_ITOC( I, C, NCD )
         DESCR( 1:8 ) = 'NAXIS'//C( 1:NCD )
         DESCR( 9: ) = '= '//VALUE( 1:SZVAL )//' / Size of dimension '/
     :                 /C( :NCD )

         IF ( ASCII ) THEN

*  Write this header to the ASCII file.
            CALL FIO_WRITE( FD, DESCR, STATUS )
         ELSE

*  Write this header to the unformatted file.
            WRITE( LUN, IOSTAT=FIOSTA ) DESCR
            CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :        /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )
         END IF

      END DO

*  Handle axis headers.
*  ====================
*  ====================
*
*  For any axis structure present, the routine checks to see if each
*  axis data array is linear.  If it is, the start value and
*  incremental value are written to the appropriate CRVALn and CDELTn
*  keywords defined at a reference pixel 1.0 written to the CRPIXn
*  keyword, as are the label and units, if present, to CTYPEn and
*  CUNITn respectively.  This is rather crude, as it deals with the
*  axis system as a whole, and that the flags to indicate presence of
*  components are for any of the axes.
      DO I = 1, NDIM
         CALL NDF_ASTAT( NDF, 'Centre', I, THERE, STATUS )

         IF ( THERE ) THEN

*  Determine the data type of the axis array.
            CALL NDF_ATYPE( NDF, 'Centre', I, ATYPE, STATUS )

*  The axis structure is found, so map it using an appropriate data
*  type.  Use _REAL for all but double-precision centres.  See if the
*  axis is linear.  Derive the increment between values.
            IF ( ATYPE .EQ. '_DOUBLE' ) THEN
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'READ',
     :                        APNTR( I ), NELM, STATUS )

               CALL KPG1_AXLID( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                          DSTART, DEND, LINEAR, STATUS )

               IF ( LINEAR ) THEN
                  INCREM = REAL( DEND - DSTART ) / REAL( NELM - 1 )
               END IF

*  Repeat for all other axis-centre data types mapped as real.
            ELSE
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'READ',
     :                        APNTR( I ), NELM, STATUS )

               CALL KPG1_AXLIR( NELM, %VAL( CNF_PVAL( APNTR( I ) ) ),
     :                          START, END, LINEAR, STATUS )

               IF ( LINEAR ) THEN
                  INCREM = ( END - START ) / REAL( NELM - 1 )
               END IF
            END IF

            IF ( LINEAR ) THEN

*  It is linear.  Record the fact to prevent copying axis information
*  from the FITS extension.
               AXIFND = .TRUE.

*  Write the start value to header CRVALn.
*  =======================================

*  Form a FITS-like card image for the ASCII file.  Right justify the
*  value.
               CALL CHR_RTOC( START, CVALUE, NCHAR )
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCD )
               DESCR( 1:8 ) = 'CRVAL'//C( 1:NCD )
               DESCR( 9: ) = '= '//VALUE( :SZVAL )//' / Axis '/
     :                       /C( 1:NCD )//' centres'' offset'

               IF ( ASCII ) THEN

*  Write this header to the ASCII file.
                  CALL FIO_WRITE( FD, DESCR, STATUS )
               ELSE

*  Write this header to the unformatted file.
                  WRITE( LUN, IOSTAT=FIOSTA ) DESCR
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :              /'header record to file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )
               END IF

*  Write the incremental value to header CDELTn.
*  =============================================

*  Form a FITS-like card image for the ASCII file.  Right justify the
*  value.
               CALL CHR_RTOC( INCREM, CVALUE, NCHAR )
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCD )
               DESCR( 1:8 ) = 'CDELT'//C( 1:NCD )
               DESCR( 9: ) = '= '//VALUE( :SZVAL )//' / Axis '/
     :                       /C( 1:NCD )//' centres'' increment'

               IF ( ASCII ) THEN

*  Write this header to the ASCII file.
                  CALL FIO_WRITE( FD, DESCR, STATUS )
               ELSE

*  Write this header to the unformatted file.
                  WRITE( LUN, IOSTAT=FIOSTA ) DESCR
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :              /'header record to file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )
               END IF

*  Write the incremental value to header CRPIXn.
*  =============================================

*  Form a FITS-like card image for the ASCII file.  Right justify the
*  value.
               CALL CHR_RTOC( INCREM, CVALUE, NCHAR )
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCD )
               DESCR( 1:8 ) = 'CRPIX'//C( 1:NCD )
               DESCR( 9: ) = '= '//VALUE( :SZVAL )//' / Axis '/
     :                       /C( 1:NCD )//' reference pixel'

               IF ( ASCII ) THEN

*  Write this header to the ASCII file.
                  CALL FIO_WRITE( FD, DESCR, STATUS )
               ELSE

*  Write this header to the unformatted file.
                  WRITE( LUN, IOSTAT=FIOSTA ) DESCR
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :              /'header record to file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )
               END IF

*  Write the label value to header CTYPEn.
*  =======================================

*  See whether an axis label is present or not.
               AXLFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( THERE ) THEN

*  Obtain the label's value and length.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Label', I, NCT, STATUS )

*  Ignore trailing blanks and exclude a blank string.
                  NCT = CHR_LEN( VALUE( :MIN( SZFITS, NCT ) ) )
                  IF ( NCT .GT. 0 ) THEN
                     DESCR = ' '
                     CALL CHR_ITOC( I, C, NCD )
                     DESCR( 1:8 ) = 'CTYPE'//C( 1:NCD )

*  Look to see if the axis-label string is longer than a normal
*  character value (18 characters).  If it is not, append a comment in
*  the normal place.  Also ensure that the value is not less than the
*  minimum length stipulated for a FITS character header value.
                     IF ( NCT .LE. 18 ) THEN
                        DESCR( 9: ) = '= '//APS/
     :                                   /VALUE( :MAX( 8, NCT ) )//APS
                        DESCR( 32: ) = '/ Axis '//C( 1:NCD )//' label'

                     ELSE

*  See if the value can be accommodated in full, with or without a
*  comment.
                        IF ( NCT .LE. 60 ) THEN
                           DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS/
     :                                   /' / Axis '//C( 1:NCD )/
     :                                   /' label'

                        ELSE IF ( NCT .LE. 68 ) THEN
                           DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS

                        ELSE

*  It is truncated so write as much as possible, but indicating the
*  missing text by an ellipsis.
                           DESCR( 9: ) = '= '//APS//VALUE( :65 )/
     :                                   /'...'//APS
                        END IF
                     END IF

                     IF ( ASCII ) THEN

*  Write this header to the ASCII file.
                        CALL FIO_WRITE( FD, DESCR, STATUS )
                     ELSE

*  Write this header to the unformatted file.
                        WRITE( LUN, IOSTAT=FIOSTA ) DESCR
                        CALL FIO_REP( LUN, ' ', FIOSTA, 'Error '/
     :                    /'writing a header record to file ^FNAME.  '/
     :                    /'Reason was ^IOSTAT.', STATUS )
                     END IF
                  END IF
               END IF

*  Write the units value to header CUNITn.
*  =======================================

*  See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( THERE ) THEN

*  Obtain the units' value and length.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Units', I, NCT, STATUS )

*  Ignore trailing blanks and exclude a blank string.
                  NCT = CHR_LEN( VALUE( :MIN( SZFITS, NCT ) ) )
                  IF ( NCT .GT. 0 ) THEN
                     DESCR = ' '
                     CALL CHR_ITOC( I, C, NCD )
                     DESCR( 1:8 ) = 'CUNIT'//C( 1:NCD )

*  Look to see if the axis-units string is longer than a normal
*  character value (18 characters).  If it is not, append a comment in
*  the normal place.  Also ensure that the value is not less than the
*  minimum length stipulated for a FITS character header value.
                     IF ( NCT .LE. 18 ) THEN
                        DESCR( 9: ) = '= '//APS/
     :                                   /VALUE( :MAX( 8, NCT ) )//APS
                        DESCR( 32: ) = '/ Axis '//C( 1:NCD )//' units'

                     ELSE

*  See if the value can be accommodated in full, with or without a
*  comment.
                        IF ( NCT .LE. 60 ) THEN
                           DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS/
     :                                   /' / Axis '//C( 1:NCD )/
     :                                   /' units'

                        ELSE IF ( NCT .LE. 68 ) THEN
                           DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS

                        ELSE

*  It is truncated so write as much as possible, but indicating the
*  missing text by an ellipsis.
                           DESCR( 9: ) = '= '//APS//VALUE( :65 )/
     :                                   /'...'//APS
                        END IF
                     END IF

                     IF ( ASCII ) THEN

*  Write this header to the ASCII file.
                        CALL FIO_WRITE( FD, DESCR, STATUS )
                     ELSE

*  Write this header to the unformatted file.
                        WRITE( LUN, IOSTAT=FIOSTA ) DESCR
                        CALL FIO_REP( LUN, ' ', FIOSTA, 'Error '/
     :                    /'writing a header record to file ^FNAME.  '/
     :                    /'Reason was ^IOSTAT.', STATUS )
                     END IF
                  END IF
               END IF

            END IF

*  Unmap the array of axis centres.
            CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
         END IF
      END DO

*  Process the title.
*  ==================
*
*  If an NDF title is found, this is copied to the ASCII file's title
*  header.  Note that the header must be at least 8 characters long.
      CALL NDF_STATE( NDF, 'TITLE', THERE, STATUS )
      IF ( THERE ) THEN

*  Find the title and its length.  Ignore blank values.
         CALL NDF_CGET( NDF, 'TITLE', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'TITLE', NCT, STATUS )
         NCT = CHR_LEN( VALUE( :MIN( SZFITS, NCT ) ) )
         IF ( NCT .GT. 0 ) THEN
            DESCR( 1:8 ) = 'TITLE   '

*  Look to see if the title is longer than a normal character value
*  (18 characters).  If it is not, append a comment in the normal
*  place.  Also ensure that the value is not less than the minimum
*  length stipulated for a FITS character header value.
            IF ( NCT .LE. 18 ) THEN
               DESCR( 9: ) = '= '//APS//VALUE( :MAX( 8, NCT ) )//APS
               DESCR( 32: ) = '/ Title'

            ELSE

*  See if the value can be accommodated in full, with or without a
*  comment.
               IF ( NCT .LE. 60 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS/
     :                          /' / Title'

               ELSE IF ( NCT .LE. 68 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS

               ELSE

*  It is truncated so write as much as possible, but indicating the
*  missing text by an ellipsis.
                  DESCR( 9: ) = '= '//APS//VALUE( :65 )//'...'//APS
               END IF
            END IF

            IF ( ASCII ) THEN

*  Write this header to the ASCII file.
               CALL FIO_WRITE( FD, DESCR, STATUS )
            ELSE

*  Write this header to the unformatted file.
               WRITE( LUN, IOSTAT=FIOSTA ) DESCR
               CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :           /'header record to file ^FNAME.  Reason was ^IOSTAT.',
     :           STATUS )
            END IF

            TITFND = .TRUE.
         END IF
      END IF

*  Process the label.
*  ==================
*
*  If an NDF label is found, this is copied to the FITS-like label
*  header.
      CALL NDF_STATE( NDF, 'LABEL', THERE, STATUS )
      IF ( THERE ) THEN

*  Find the label and its length.  Ignore blank values.
         CALL NDF_CGET( NDF, 'LABEL', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'LABEL', NCT, STATUS )
         NCT = CHR_LEN( VALUE( :MIN( SZFITS, NCT ) ) )
         IF ( NCT .GT. 0 ) THEN
            DESCR( 1:8 ) = 'LABEL   '

*  Look to see if the label is longer than a normal character value
*  (18 characters).  If it is not, append a comment in the normal
*  place.  Also ensure that the value is not less than the minimum
*  length stipulated for a FITS character header value.
            IF ( NCT .LE. 18 ) THEN
               DESCR( 9: ) = '= '//APS//VALUE( :MAX( 8, NCT ) )//APS
               DESCR( 32: ) = '/ Label'

            ELSE

*  See if the value can be accommodated in full, with or without a
*  comment.
               IF ( NCT .LE. 60 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS/
     :                          /' / Label'

               ELSE IF ( NCT .LE. 68 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS

               ELSE

*  It is truncated so write as much as possible, but indicating the
*  missing text by an ellipsis.
                  DESCR( 9: ) = '= '//APS//VALUE( :65 )//'...'//APS
               END IF
            END IF

            IF ( ASCII ) THEN

*  Write this header to the ASCII file.
               CALL FIO_WRITE( FD, DESCR, STATUS )
            ELSE

*  Write this header to the unformatted file.
               WRITE( LUN, IOSTAT=FIOSTA ) DESCR
               CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :           /'header record to file ^FNAME.  Reason was '/
     :           /'^IOSTAT.', STATUS )
            END IF

            LABFND = .TRUE.
         END IF
      END IF

*  Process the units.
*  ==================
*
*  If an NDF units component is found, this is copied to the FITS
*  BUNIT header.
      CALL NDF_STATE( NDF, 'UNITS', THERE, STATUS )
      IF ( THERE ) THEN

*  Find the units and its length.  Ignore blank values.
         CALL NDF_CGET( NDF, 'UNITS', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'UNITS', NCT, STATUS )
         NCT = CHR_LEN( VALUE( :MIN( SZFITS, NCT ) ) )
         IF ( NCT .GT. 0 ) THEN
            DESCR( 1:8 ) = 'BUNIT   '

*  Look to see if the units string is longer than a normal character
*  value (18 characters).  If it is not, append a comment in the
*  normal place.  Also ensure that the value is not less than the
*  minimum length stipulated for a FITS character header value.
            IF ( NCT .LE. 18 ) THEN
               DESCR( 9: ) = '= '//APS//VALUE( :MAX( 8, NCT ) )//APS
               DESCR( 32: ) = '/ Units'

            ELSE

*  See if the value can be accommodated in full, with or without a
*  comment.
               IF ( NCT .LE. 60 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS/
     :                          /' / Units'

               ELSE IF ( NCT .LE. 68 ) THEN
                  DESCR( 9: ) = '= '//APS//VALUE( :NCT )//APS

               ELSE

*  It is truncated so write as much as possible, but indicating the
*  missing text by an ellipsis.
                  DESCR( 9: ) = '= '//APS//VALUE( :65 )//'...'//APS
               END IF
            END IF

            IF ( ASCII ) THEN

*  Write this header to the ASCII file.
               CALL FIO_WRITE( FD, DESCR, STATUS )
            ELSE

*  Write this header to the unformatted file.
               WRITE( LUN, IOSTAT=FIOSTA ) DESCR
               CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :           /'header record to file ^FNAME.  Reason was '/
     :           /'^IOSTAT.', STATUS )
            END IF

            UNTFND = .TRUE.
         END IF
      END IF

*  Indicate unsigned data.
*  =======================

*  FITS does not support unsigned integer data arrays.  When an
*  application comes to read the ASCII file it may need to know
*  that the data type is unsigned.  So an extra header is added.
*  This is reasonable since it's only a pseudo-FITS file.
      IF ( DTYPE .EQ. '_UBYTE' .OR. DTYPE .EQ. '_UWORD' ) THEN
         DESCR = ' '
         DESCR( 1:9 ) = 'UNSIGNED='
         DESCR( 30: ) = 'T / Indicates that the data array is unsigned'

         IF ( ASCII ) THEN

*  Write this header to the ASCII file.
            CALL FIO_WRITE( FD, DESCR, STATUS )
         ELSE

*  Write this header to the unformatted file.
            WRITE( LUN, IOSTAT=FIOSTA ) DESCR
            CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :        /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )
         END IF
      END IF

  999 CONTINUE

*  Set the array of flags indicating the presence or not of certain
*  NDF components.
      CMPTHE( 1 ) = AXIFND
      CMPTHE( 2 ) = AXLFND
      CMPTHE( 3 ) = AXUFND
      CMPTHE( 4 ) = TITFND
      CMPTHE( 5 ) = LABFND
      CMPTHE( 6 ) = UNTFND

      END
