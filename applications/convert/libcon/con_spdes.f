      SUBROUTINE CON_SPDES( NDF, BDFNAM, DESCRP, NFLAGS, CMPTHE,
     :                      STATUS )
*+
*  Name:
*     CON_SPDES

*  Purpose:
*     Writes out the special BDF descriptors derived from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_SPDES( NDF, BDFNAM, DESCRP, NFLAGS, CMPTHE, STATUS )

*  Description:
*     This routine serves NDF2BDF.  It finds whether certain special
*     components appear in the NDF so they can be written as descriptors
*     in the BDF named with their corresponding FITS keywords.  A record
*     of which items have been set is made.
*
*     The keywords are:
*        o  NAXIS, and NAXISn are derived from the dimensions of
*           the NDF data array.
*        o  The TITLE, LABEL, and BUNITS descriptors are derived from
*           the TITLE, LABEL, and UNITS NDF components.
*        o  The CDELTn, CRVALn, CRTYPEn and CTYPEn descriptors are
*           derived from a set of linear NDF AXIS structures.
*        o  The standard order of the FITS keywords is preserved.
*           No FITS comments are written following the values of the
*           above exceptions.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     BDFNAM = CHARACTER * ( * ) (Given)
*        The name of the INTERIM parameter that accesses the BDF.
*     DESCRP = LOGICAL (Given)
*        If true the values of the descriptors written to the BDF are
*        reported to the user.
*     NFLAGS = INTEGER (Given)
*        The number of flags used to indicate that certain NDF
*        components have been used to write descriptors to the BDF.
*        It should be set to 6.
*     CMPTHE( NFLAGS ) = LOGICAL (Returned)
*        The flags when set to true indicate that certain optional NDF
*        components have been used to write descriptors to the BDF.
*        In order they are 1) CRVARn and CDELTn, 2) CRTYPEn, 3) CTYPEn,
*        4) TITLE, 5) LABEL, and 6) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     1992 September 4 (MJC):
*        Original version.
*     1992 September 16 (MJC):
*        Made to handle double-precision axis centres, and used an
*        improved algorithm to determine whether or not the axis centres
*        are linear, and the increment between adjacent axis centres.
*     1992 November 17 (MJC):
*        Fixed bug that could create erroneous CRTYPEn and CTYPEn BDF
*        descriptors.
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
      CHARACTER*( * ) BDFNAM     ! Parameter name of the BDF
      LOGICAL DESCRP             ! Descriptors output to user?
      INTEGER NFLAGS             ! Number of flags to indicate
                                 ! presence of certain components

*  Arguments Returned:
      LOGICAL CMPTHE( NFLAGS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZDESC             ! Size of descriptor names
      PARAMETER( SZDESC = 8 )
      INTEGER SZVAL              ! Size of descriptor values
      PARAMETER( SZVAL = 70 )

*  Local Variables:
      INTEGER APNTR( NDF__MXDIM ) ! Pointers to NDF axis arrays
      CHARACTER*( NDF__SZTYP ) ATYPE ! Data type of the axis centres
      LOGICAL AXIFND             ! NDF contains a linear axis comps.?
      LOGICAL AXLFND             ! NDF contains axis label?
      LOGICAL AXUFND             ! NDF contains axis units?
      CHARACTER*1 C              ! Accommodates character string
      CHARACTER*( SZVAL ) CVALUE ! Accommodates descriptor value
      DOUBLE PRECISION DEND      ! End value for an axis-centre array
      CHARACTER*( SZDESC ) DESCR ! Accommodates descriptor name
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      DOUBLE PRECISION DSTART    ! Start value for an axis-centre array
      REAL END                   ! End value for an axis-centre array
      INTEGER I                  ! Loop variable
      REAL INCREM                ! Incremental value for axis array
      INTEGER ISTAT              ! Local status return
      LOGICAL LABFND             ! NDF LABEL found?
      LOGICAL LINEAR             ! An axis is linear?
      INTEGER NCHAR              ! Length of a character string
      INTEGER NDIM               ! Number of dimensions
      INTEGER NELM               ! Number of elements
      REAL START                 ! Start value for an axis structure
      LOGICAL THERE              ! NDF has FITS extension?
      LOGICAL TITFND             ! NDF TITLE found?
      LOGICAL UNTFND             ! NDF UNITS found?
      CHARACTER*( SZVAL ) VALUE  ! Accommodates descriptor value
      INTEGER WRSTAT             ! Status for WRDSCR routine

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output flags.
*  ========================
      AXIFND = .FALSE.
      TITFND = .FALSE.
      LABFND = .FALSE.
      UNTFND = .FALSE.

*  Process NAXIS descriptor.
*  =========================
*
*  Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  This is derived from the actual dimensionality of the NDF data array.
      CALL CHR_ITOC( NDIM, CVALUE, NCHAR )
      VALUE = ' '
      VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
      DESCR = 'NAXIS   '

*  Write this descriptor to the BDF.
      CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
      IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output descriptor name and value to user if required.
      IF ( DESCRP ) THEN
         CALL MSG_SETC( 'DESCR', DESCR )
         CALL MSG_SETC( 'VALUE', CVALUE )
         CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
      END IF

*  Process AXISn descriptors.
*  ==========================
*
*  Now the actual size of each dimension of the NDF data array is
*  written to the appropriate NAXISn descriptors.
      DO I = 1, NDIM
         CALL CHR_ITOC( DIMS( I ), CVALUE, NCHAR )
         VALUE = ' '
         VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
         DESCR = ' '
         CALL CHR_ITOC( I, C, NCHAR )
         DESCR = 'NAXIS' // C( 1:NCHAR )

*  Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output descriptor name and value to user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', CVALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF
      END DO

*  Handle axis descriptors.
*  ========================
*
*  For any axis structure present, the routine checks to see if each
*  axis data array is linear.  If it is, the start value and incremental
*  value are written to the appropriate CRVALn and CDELTn descriptors,
*  as are the label and units, if present, to CRTYPEn and CTYPEn
*  respectively.  This is rather crude, as it deals with the axis
*  system as a whole, and that the flags to indicate presence of
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

*  Write the start value to descriptor CRVALn.
*  ===========================================
               CALL CHR_RTOC( START, CVALUE, NCHAR )
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCHAR )
               DESCR = 'CRVAL' // C( 1:NCHAR )

*  Write this descriptor to the BDF.
               CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
               IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to user if required.
               IF ( DESCRP ) THEN
                  CALL MSG_SETC( 'DESCR', DESCR )
                  CALL MSG_SETC( 'VALUE', CVALUE )
                  CALL MSG_OUT ( ' ', '^DESCR : ^VALUE', STATUS )
               END IF

*  Write the incremental value to descriptor CDELTn.
*  =================================================

*  Form a descriptor string for the BDF.
               CALL CHR_RTOC( INCREM, CVALUE, NCHAR )
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCHAR )
               DESCR = 'CDELT' // C( 1:NCHAR )

*  Write this descriptor to the BDF.
               CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
               IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
               IF ( DESCRP ) THEN
                  CALL MSG_SETC( 'DESCR', DESCR )
                  CALL MSG_SETC( 'VALUE', CVALUE )
                  CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
               END IF

*  Write the label value to descriptor CRTYPEn.
*  ============================================

*  See whether an axis label is present or not.
               AXLFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( AXLFND ) THEN

*  Obtain the label's value and length.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Label', I, NCHAR, STATUS )

*  Form a descriptor string for the BDF.
                  DESCR = ' '
                  CALL CHR_ITOC( I, C, NCHAR )
                  DESCR = 'CRTYPE' // C( 1:NCHAR )

*  Write this descriptor to the BDF.
                  CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
                  IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
                  IF ( DESCRP ) THEN
                     CALL MSG_SETC( 'DESCR', DESCR )
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
                  END IF
               END IF

*  Write the units value to descriptor CTYPEn.
*  ===========================================

*  See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( AXUFND ) THEN

*  Obtain the units' value and length.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Units', I, NCHAR, STATUS )

*  Form a descriptor string for the BDF.
                  DESCR = ' '
                  CALL CHR_ITOC( I, C, NCHAR )
                  DESCR = 'CTYPE' // C( 1:NCHAR )

*  Write this descriptor to the BDF.
                  CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
                  IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
                  IF ( DESCRP ) THEN
                     CALL MSG_SETC( 'DESCR', DESCR )
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
                  END IF
               END IF

            END IF
         END IF
      END DO

*  Process the title.
*  ==================
*
*  If an NDF title is found, this is copied to the BDF title
*  descriptor.
      CALL NDF_STATE( NDF, 'TITLE', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'TITLE', VALUE, STATUS )
         DESCR = 'TITLE   '

*  Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         TITFND = .TRUE.
      END IF

*  Process the label.
*  ==================
*
*  If an NDF label is found, this is copied to the BDF label descriptor.
      CALL NDF_STATE( NDF, 'LABEL', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'LABEL', VALUE, STATUS )
         DESCR = 'LABEL   '

*  Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         LABFND = .TRUE.
      END IF

*  Process the units.
*  ==================
*
*  If an NDF units component is found, this is copied to the BDF BUNITS
*  descriptor.
      CALL NDF_STATE( NDF, 'UNITS', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'UNITS', VALUE, STATUS )
         DESCR = 'BUNITS  '

*  Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*  Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         UNTFND = .TRUE.
      END IF

  999 CONTINUE

*  Set the array of flags indicating the presence or not of certain NDF
*  components.
      CMPTHE( 1 ) = AXIFND
      CMPTHE( 2 ) = AXLFND
      CMPTHE( 3 ) = AXUFND
      CMPTHE( 4 ) = TITFND
      CMPTHE( 5 ) = LABFND
      CMPTHE( 6 ) = UNTFND

      END
