      SUBROUTINE COF_SBND( FUNIT, NDF, KEYROT, STATUS )
*+
*  Name:
*     COF_SBND

*  Purpose:
*     Sets the bounds of an NDF using a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_SBND( FUNIT, NDF, KEYROT, STATUS )

*  Description:
*     This routine sets the lower and upper bounds of an NDF, by
*     using the information stored in a FITS header.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier of the NDF which is to have new bounds.
*     KEYROT = CHARACTER * ( * ) (Given)
*        The root name of the FITS keyword that defines a lower bound.
*        The full keyword being KEYROTn, where is the dimension number.
*        If this is a blank value, the lower bounds are assumed to be
*        one.  It is limited to 7 characters, comprising numbers,
*        letters, underscore and hyphen.  The normal root is 'LBOUND'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The current header and data unit must either be primary or an
*     IMAGE extension.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 21 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF
      CHARACTER * ( * ) KEYROT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      INTEGER BITPIX             ! FITS BITPIX value (not used)
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      LOGICAL EXTEND             ! FITS EXTEND value (not used)
      INTEGER FSTAT              ! FITSIO error status
      INTEGER GCOUNT             ! FITS group count value (not used)
      INTEGER I                  ! Loop counter
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword for a lower bound
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of the NDF
      INTEGER NDIM               ! Dimensionality of the NDF
      INTEGER NHDU               ! Number of the current HDU
      INTEGER PCOUNT             ! FITS PCOUNT value (not used)
      LOGICAL SIMPLE             ! Simple FITS (not used)
      LOGICAL THERE              ! Keyword is present?
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of the NDF
      CHARACTER * ( 8 ) XTENS    ! Extension name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the current HDU is the primary or IMAGE extension.
*  At present there is no inquiry routine, so inquire the number of
*  the HDU.  1 is the primary HDU.
      CALL FTGHDN( FUNIT, NHDU )
      IF ( NHDU .GT. 1 ) THEN

*  Obtain the value of the XTENSION keyword.
         CALL COF_GKEYC( FUNIT, 'XTENSION', THERE, XTENS, COMENT,
     :                   STATUS )
         IF ( .NOT. ( THERE .AND. STATUS .EQ. SAI__OK
     :        .AND. XTENS .EQ. 'IMAGE' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_SBND',
     :        'Current header and data unit is not primary or IMAGE '/
     :        /'extension, therefore cannot define NDF bounds.',
     :        STATUS )
            GOTO 999
         END IF
      END IF

*  Obtain the bounds and dimensions from the FITS file.
      CALL FTGHPR( FUNIT, NDF__MXDIM, SIMPLE, BITPIX, NDIM, DIMS,
     :             PCOUNT, GCOUNT, EXTEND, FSTAT )

      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SBND_KEYWORD', 'FTGHPR',
     :     'Error obtaining the dimensions of the NDF from the FITS '/
     :     /'keywords.', STATUS )
         GOTO 999
      END IF

*  Shift the dimensions if the first is zero (meaning a random-groups
*  format).  Reduce the number of dimensions to allow for this
*  convention.
      IF ( DIMS( 1 ) .EQ. 0 ) THEN
         NDIM = NDIM - 1
         DO I = 1, NDIM
            DIMS( I ) = DIMS( I + 1 )
         END DO
      END IF

*  Do not search for keywords.
      IF ( KEYROT .EQ. ' ' ) THEN

         DO I = 1, NDIM
            LBND( I ) = 1
         END DO
      
*  Set the dimensionality, and the dimensions of the NDF.
         CALL NDF_SBND( NDIM, LBND, DIMS, NDF, STATUS )

      ELSE

         DO I = 1, NDIM

*  See if there are lower-bounds keywords.  Use the default value if
*  one is not present.
            CALL FTKEYN( KEYROT, I, KEYWRD, FSTAT )
            IF ( FSTAT .NE. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_SBND_KEYWORD', 'FTKEYN',
     :           'FITS keyword for lower bounds is invalid.  Root is '/
     :           /KEYROT//'. (Probable programming error.)', STATUS )
               GOTO 999
            END IF

*  Obtain the value of the keyword.
            CALL COF_GKEYI( FUNIT, KEYWRD, THERE, LBND( I ), COMENT,
     :                      STATUS )

            IF ( THERE ) THEN
               UBND( I ) = LBND( I ) - 1 + DIMS( I )

*  Use the default bounds if the LBOUNDn keyword is absent or is not
*  integer.
            ELSE
               LBND( I ) = 1
               UBND( I ) = DIMS( I )
            END IF
         END DO

*  Set the dimensionality, and the bounds of the NDF.
         CALL NDF_SBND( NDIM, LBND, UBND, NDF, STATUS )
      END IF

  999 CONTINUE

      END
