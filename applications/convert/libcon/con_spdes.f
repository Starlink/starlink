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
*     in the BDF named with their corresponding FITS keywords.  These
*     items are the

*     When
*     there is a FITS extension
*     
*     components are
*     present

*  [arguments]
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 4 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                 ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'             ! Standard SAE constants
      INCLUDE 'NDF_PAR'             ! NDF_ public constants

*  Arguments Given:
      INTEGER   NDF                 ! NDF identifier
      CHARACTER * ( * ) BDFNAM      ! Parameter name of the BDF
      LOGICAL   DESCRP              ! True if descriptors output to user
      INTEGER   NFLAGS              ! Number of flags to indicate
                                    ! presence of certain components

*  Arguments Returned:
      LOGICAL CMPTHE( NFLAGS )

*  Status:
      INTEGER STATUS                ! Global status

*  Local Constants:
      INTEGER   SZDESC              ! Size of descriptor names
      PARAMETER( SZDESC = 8 )
      INTEGER   SZVAL               ! Size of descriptor values
      PARAMETER( SZVAL = 70 )

*  Local Variables:
      INTEGER   APNTR(DAT__MXDIM)   ! Pointers to NDF axis arrays
      LOGICAL   AXIFND              ! True if NDF contains a linear axis
                                    ! comps.
      LOGICAL   AXLFND              ! True if NDF contains axis label
      LOGICAL   AXUFND              ! True if NDF contains axis units
      CHARACTER C*1                 ! Accommodates character string
      CHARACTER CVALUE*(SZVAL)      ! Accommodates descriptor value
      CHARACTER DESCR*(SZDESC)      ! Accommodates descriptor name
      INTEGER   DIMS(DAT__MXDIM)    ! IMAGE dimensions (axis length)
      INTEGER   I                   ! Loop variable
      REAL      INCREM              ! Incremental value for axis array
      INTEGER   ISTAT               ! Local status return
      LOGICAL   LABFND              ! True if NDF LABEL found
      LOGICAL   LINEAR              ! True if an axis is linear
      INTEGER   NCHAR               ! Length of a character string
      INTEGER   NDIM                ! Number of dimensions
      INTEGER   NELM                ! Number of elements
      REAL      START               ! Start value for an axis structure
      LOGICAL   THERE               ! True if NDF has FITS extension
      LOGICAL   TITFND              ! True if NDF TITLE found
      LOGICAL   UNTFND              ! True if NDF UNITS found
      CHARACTER VALUE*(SZVAL)       ! Accommodates descriptor value
      INTEGER   WRSTAT              ! Status for WRDSCR routine
*.

*   Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise output flags.
*   ========================
      AXIFND = .FALSE.
      TITFND = .FALSE.
      LABFND = .FALSE.
      UNTFND = .FALSE.

*   Process NAXIS descriptor.
*   =========================
*
*   Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*   This is derived from the actual dimensionality of the NDF data
*   array.
      CALL CHR_ITOC( NDIM, CVALUE, NCHAR )      
      VALUE = ' '
      VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
      DESCR = 'NAXIS   '

*   Write this descriptor to the BDF.
      CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
      IF ( WRSTAT .NE. 0 ) GO TO 999

*   Output descriptor name and value to user if required.
      IF ( DESCRP ) THEN
         CALL MSG_SETC( 'DESCR', DESCR )
         CALL MSG_SETC( 'VALUE', CVALUE )
         CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
      END IF

*   Process AXISn descriptors.
*   ==========================
*
*   Now the actual size of each dimension of the NDF data array is
*   written to the appropriate NAXISn descriptors.
      DO I = 1, NDIM
         CALL CHR_ITOC( DIMS(I), CVALUE, NCHAR )      
         VALUE = ' '
         VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
         DESCR = ' '
         CALL CHR_ITOC( I, C, NCHAR )
         DESCR = 'NAXIS'//C( 1:NCHAR )

*      Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*      Output descriptor name and value to user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', CVALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF
      END DO

*   Handle axis descriptors.
*   ========================
*      
*   For any axis structure present, the routine checks to see if each
*   axis data array is linear. If it is, the start value and incremental
*   value are written to the appropriate CRVALn and CDELTn descriptors,
*   as are the label and units , if present, to CRTYPEn and CTYPEn
*   respectively.  This is rather crude, as it deals with the axis 
*   system as a whole, and that the flags to indicate presence of 
*   components are for any of the axes.
      DO I = 1, NDIM 
         CALL NDF_ASTAT( NDF, 'Centre', I, THERE, STATUS )

         IF ( THERE ) THEN

*         Axis structure is found, map it and see if it is linear.
            CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'READ', 
     :                     APNTR(I), NELM, STATUS )
            CALL CON_LNEAR( NELM, %VAL( APNTR( I ) ), LINEAR, START, 
     :                      INCREM, STATUS )
            IF ( LINEAR ) THEN
               AXIFND = .TRUE.

*            It is linear.
*
*            Write the start value to descriptor CRVALn.
*            ===========================================
               CALL CHR_RTOC( START, CVALUE, NCHAR ) 
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCHAR )
               DESCR = 'CRVAL'//C( 1:NCHAR )

*            Write this descriptor to the BDF.
               CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
               IF ( WRSTAT .NE. 0 ) GO TO 999

*            Output the descriptor's name and value to user if required.
               IF ( DESCRP ) THEN
                  CALL MSG_SETC( 'DESCR', DESCR )
                  CALL MSG_SETC( 'VALUE', CVALUE )
                  CALL MSG_OUT ( ' ', '^DESCR : ^VALUE', STATUS )
               END IF

*            Write the incremental value to descriptor CDELTn.
*            =================================================

*            Form a descriptor string for the BDF.
               CALL CHR_RTOC( INCREM, CVALUE, NCHAR )      
               VALUE = ' '
               VALUE( SZVAL-NCHAR+1:SZVAL ) = CVALUE( 1:NCHAR )
               DESCR = ' '
               CALL CHR_ITOC( I, C, NCHAR )
               DESCR = 'CDELT'//C( 1:NCHAR )

*            Write this descriptor to the BDF.
               CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
               IF ( WRSTAT .NE. 0 ) GO TO 999
               
*            Output the descriptor's name and value to the user if
*            required.
               IF ( DESCRP ) THEN
                  CALL MSG_SETC( 'DESCR', DESCR )
                  CALL MSG_SETC( 'VALUE', CVALUE )
                  CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
               END IF

*            Write the label value to descriptor CRTYPEn.
*            ============================================

*            See whether an axis label is present or not.
               AXLFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( THERE ) THEN

*               Obtain the label's value and length.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Label', I, NCHAR, STATUS )

*               Form a descriptor string for the BDF.
                  DESCR = ' '
                  CALL CHR_ITOC( I, C, NCHAR )
                  DESCR = 'CRTYPE'//C( 1:NCHAR )

*               Write this descriptor to the BDF.
                  CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
                  IF ( WRSTAT .NE. 0 ) GO TO 999
               
*               Output the descriptor's name and value to the user if
*               required.
                  IF ( DESCRP ) THEN
                     CALL MSG_SETC( 'DESCR', DESCR )
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
                  END IF
               END IF

*            Write the units value to descriptor CTYPEn.
*            ===========================================

*            See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( THERE ) THEN

*               Obtain the units' value and length.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Units', I, NCHAR, STATUS )

*               Form a descriptor string for the BDF.
                  DESCR = ' '
                  CALL CHR_ITOC( I, C, NCHAR )
                  DESCR = 'CTYPE'//C( 1:NCHAR )

*               Write this descriptor to the BDF.
                  CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
                  IF ( WRSTAT .NE. 0 ) GO TO 999
               
*               Output the descriptor's name and value to the user if
*               required.
                  IF ( DESCRP ) THEN
                     CALL MSG_SETC( 'DESCR', DESCR )
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
                  END IF
               END IF

            END IF
         END IF
      END DO

*   Process the title.
*   ==================
*   
*   If an NDF title is found, this is copied to the BDF title
*   descriptor.
      CALL NDF_STATE( NDF, 'TITLE', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'TITLE', VALUE, STATUS )
         DESCR = 'TITLE   '

*      Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*      Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         TITFND = .TRUE.
      END IF

*   Process the label.
*   ==================
*   
*   If an NDF label is found, this is copied to the BDF label
*   descriptor.
      CALL NDF_STATE( NDF, 'LABEL', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'LABEL', VALUE, STATUS )
         DESCR = 'LABEL   '

*      Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*      Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         LABFND = .TRUE.
      END IF

*   Process the units.
*   ==================
*   
*   If an NDF units component is found, this is copied to the BDF BUNITS
*   descriptor.
      CALL NDF_STATE( NDF, 'UNITS', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'UNITS', VALUE, STATUS )
         DESCR = 'BUNITS  '

*      Write this descriptor to the BDF.
         CALL WRDSCR( BDFNAM, DESCR, VALUE, 1, WRSTAT )
         IF ( WRSTAT .NE. 0 ) GO TO 999

*      Output the descriptor's name and value to the user if required.
         IF ( DESCRP ) THEN
            CALL MSG_SETC( 'DESCR', DESCR )
            CALL MSG_SETC( 'VALUE', VALUE )
            CALL MSG_OUT( ' ', '^DESCR : ^VALUE', STATUS )
         END IF

         UNTFND = .TRUE.
      END IF

  999 CONTINUE

*   Set the array of flags indicating the presence or not of certain
*   NDF components.
      CMPTHE( 1 ) = AXIFND
      CMPTHE( 2 ) = AXLFND
      CMPTHE( 3 ) = AXUFND
      CMPTHE( 4 ) = TITFND
      CMPTHE( 5 ) = LABFND
      CMPTHE( 6 ) = UNTFND

      END
