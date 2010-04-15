      SUBROUTINE PREPB6( NCARD, FITS, LOC, U, FACTOR, PIXSIZ, STATUS )
*+
*  Name:
*     PREPB6

*  Purpose:
*     Create and write extra components to IMAGE_INFO of a DEEPSKY NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB6( NCARD, FITS, LOC, U, FACTOR, PIXSIZ, STATUS )

*  Description:
*     This routine creates following components in theIMAGE_INFO
*     structure of an IRAS Point Observation image NDF.
*                    MAXSOP <_INTEGER>
*                    OBSNO <_INTEGER>
*                    PONOISE <_REAL>
*                    POFLUX <_LOGICAL>
*                    PONMAP <_LOGICAL>
*                    POUNITS <_CHAR>
*     Where
*           MAXSOP - the SOP number of the image.
*           OBSNO - the observation number.
*           PONOISE - the estimated median noise in the same units as
*              the output DATA array.
*           POFLUX - if it is true, the image is a flux grid, otherwise
*              it is a intensity grid.
*           PONMAP - if it is true, the image is a noise grid, otherwise
*              it is an intensity or flux grid.
*           POUNITS - The units in which the PONOISE value is stored.
*
*     These components will be assigned values according to the
*     information obtained from the FITS extenion of the NDF.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER * ( * ) (Given)
*        The locator of the IMAGE_INFO structure of the NDF file.
*     U = CHARACTER * ( * ) (Given)
*        The units in which the output data has been created.
*     FACTOR = REAL (Given)
*        Factor for converting data stored in the input units to the
*        output units.
*     PIXSIZ = DOUBLE PRECISION (Given)
*        Pixel size in steradians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-SEP-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from DSINFO to PREPB6, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRI_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER LOC*(*)
      CHARACTER U*(*)
      REAL FACTOR
      DOUBLE PRECISION PIXSIZ

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.

*  Local Variables:
      CHARACTER BUNIT*5          ! Value of keyword BUNIT
      CHARACTER CMNT*72          ! A comment string from FITS head
      CHARACTER MNSSTR*15        ! String expression of madian nois
      CHARACTER OBSSTR*4         ! String expression of Observation No.
      CHARACTER SOPSTR*4         ! String expression of SOP number


      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER CMNTLN             ! Used length of CMNT
      INTEGER EQLPSN             ! Position of the equal sign
      INTEGER FEQPSN             ! Position of the fisrt equal sign
      INTEGER GRDCRD             ! Card number of keyword SKYGRID
      INTEGER JSKPSN             ! Position of Jansky sign
      INTEGER OBSNO              ! Observation number
      INTEGER SEPPSN             ! Position of the separation sign
      INTEGER SEQPSN             ! Position of the second equal sign
      INTEGER SOPNO              ! SOP number
      INTEGER STCARD             ! Start card number when search FITS


      LOGICAL POFLUX             ! Flux image flag
      LOGICAL PONMAP             ! True if input is a noise map.
      LOGICAL THERE              ! FITS keyword flag


      REAL MNOIS                 ! Madian noise of the image
      REAL RSKGRD                ! Real value of skygrid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0I( LOC, 'MAXSOP', STATUS )
      CALL DAT_NEW0I( LOC, 'OBSNO', STATUS )
      CALL DAT_NEW0R( LOC, 'PONOISE', STATUS )
      CALL DAT_NEW0L( LOC, 'POFLUX', STATUS )
      CALL DAT_NEW0L( LOC, 'PONMAP', STATUS )
      CALL DAT_NEW0C( LOC, 'POUNITS', IRI__SZUNI, STATUS )

*  See what type of PO image this is.
      CALL PREPC7( NCARD, FITS, POFLUX, PONMAP, STATUS )

*  Write the POFLUX component of the IMAGE_INFO structure.
      CALL CMP_PUT0L( LOC, 'POFLUX', POFLUX, STATUS )

*  Write the PONMAP component of the IMAGE_INFO structure.
      CALL CMP_PUT0L( LOC, 'PONMAP', PONMAP, STATUS )

*  Get the value of FITS keyword DSKYGRID.
      CALL IRM_GKEYR( NCARD, FITS, 1, 'DSKYGRID', THERE, RSKGRD, GRDCRD,
     :             STATUS )

*  Get the comment string following keyword DSKYGRID.
      STCARD = GRDCRD - 1
      CALL IRM_COMNT( NCARD, FITS, STCARD, THERE, CMNT, CARD, STATUS )

*  Remove all blanks and get its used length.
      CALL CHR_RMBLK( CMNT )
      CMNTLN = CHR_LEN( CMNT )

*  Get the positions of first '=', the ';' and the second '='.
      FEQPSN = INDEX( CMNT, '=' )
      SEPPSN = INDEX( CMNT, ';' )
      SEQPSN = INDEX( CMNT( SEPPSN + 1 : ), '=' )
      SEQPSN = SEPPSN + SEQPSN

*  Extract the SOP number and observation number from the string.
      SOPSTR = CMNT( FEQPSN + 1 : SEPPSN -1 )
      OBSSTR = CMNT( SEQPSN + 1 : CMNTLN )

*  Covert them to integers.
      CALL CHR_CTOI( SOPSTR, SOPNO, STATUS )
      CALL CHR_CTOI( OBSSTR, OBSNO, STATUS )

*  Write them into MAXSOP and OBSNO components of the IMAGE_INFO
*  structure.
      CALL CMP_PUT0I( LOC, 'MAXSOP', SOPNO, STATUS )
      CALL CMP_PUT0I( LOC, 'OBSNO', OBSNO, STATUS )

*  Get the first comment string after keyword DSKYGRID.
      STCARD = GRDCRD + 1
      CALL IRM_COMNT( NCARD, FITS, STCARD, THERE, CMNT, CARD, STATUS )

*  Remove all blanks and get its used length.
      CALL CHR_RMBLK( CMNT )
      CMNTLN = CHR_LEN( CMNT )

*  Get the positions of equal sign '=' and the string "JY"
      EQLPSN = INDEX( CMNT, '=' )
      JSKPSN = INDEX( CMNT, 'JY' )

*  Extract the median noise from the comment string.
      MNSSTR = CMNT( EQLPSN + 1 : JSKPSN - 1 )

*  Convert it to real value.
      CALL CHR_CTOR( MNSSTR, MNOIS, STATUS )

*  If this is an intensity grid, and the median noise is given in
*  units of Jy, divide it by the pixel size to convert it into units
*  of Jy/sr.
      IF( .NOT. POFLUX .AND. CMNT( JSKPSN : ) .EQ. 'JY' ) THEN
         MNOIS = REAL( DBLE( MNOIS )/PIXSIZ )
         CMNT( JSKPSN : ) = 'JY/SR'
      END IF

*  Get value of FITS keyword BUNIT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'BUNIT', THERE, BUNIT, CARD,
     :             STATUS )
      CALL CHR_RMBLK( BUNIT )

*  If the median noise value is in the same units as the input data,
*  scale it to the same units as the output data, and store the output
*  units as the value for component POUNITS.
      IF( CHR_SIMLR( CMNT( JSKPSN : ), BUNIT ) ) THEN
         MNOIS = FACTOR*MNOIS
         CALL CMP_PUT0C( LOC, 'POUNITS', U, STATUS )

*  Otherwise, store the median noise units directly in POUNITS.
      ELSE
         CALL CMP_PUT0C( LOC, 'POUNITS', CMNT( JSKPSN : ), STATUS )

      END IF

*  Write the noise stimate into the  PONOISE component of the
*  IMAGE_INFO structure.
      CALL CMP_PUT0R( LOC, 'PONOISE', MNOIS, STATUS )

      END
