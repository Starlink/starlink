      SUBROUTINE CHAIN( STATUS )
*+
*  Name:
*     CHAIN

*  Purpose:
*     Concatenates a series of vectorized NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHAIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application concatenates a series of NDFs, in the order
*     supplied and treated as vectors, to form a 1-dimensional output
*     NDF.  The dimensions of the NDFs may be different, and indeed so
*     may their dimensionalities.

*  Usage:
*     chain in c1 [c2] [c3] ... [c25] out=?

*  ADAM Parameters:
*     IN = NDF (Read)
*        The base NDF after which the other input NDFs will be
*        concatenated.
*     OUT = NDF (Write)
*        The one-dimensional NDF resulting from concatenating the input
*        NDFs.
*     C1-C25 = NDF (Read)
*        The NDFs to be concatenated to the base NDF.  The NDFs are
*        joined in the order C1, C2, ... C25.  There can be no missing
*        NDFs, e.g. in order for C3 to be processed there must be a C2
*        given as well.   A null value (!) indicates that there is no
*        NDF.  NDFs C2 to C25 are defaulted to !.  At least one NDF
*        must be pasted, therefore C1 may not be null.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the base NDF to the output NDF. [!]

*  Examples:
*     chain obs1 obs2 out=stream
*        This concatenates the NDF called obs2 on to the arrays in the
*        NDF called obs1 to produce the 1-dimensional NDF stream.
*     chain c1=obs2 c2=obs1 in=obs3 out=stream
*        This concatenates the NDF called obs2 on to the arrays in the
*        NDF called obs3, and then concatenates the arrays from obs1
*        to them to produce the 1-dimensional NDF stream.

*  Related Applications:
*     KAPPA: PASTE, RESHAPE.

*  Implementation Status:
*     -  This routine correctly processes the DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, and HISTORY, components of an NDF
*     data structure and propagates all extensions.  Propagation is from
*     the base NDF.  WCS and AXIS information is lost.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2012 Science & Technology
*     Facilities Council.  All Rights Reserved.

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
*     1997 June 14 (MJC):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     2012 May 10 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDFMAX             ! Maximum number of input NDFs
      PARAMETER ( NDFMAX = 26 )

*  Local Variables:
      LOGICAL BAD                ! Input NDFs' data arrays may have bad
                                 ! values
      LOGICAL BADQUA             ! Input NDFs' quality may have bad
                                 ! values
      LOGICAL BADVAR             ! Input NDFs' variance may have bad
                                 ! values
      CHARACTER * ( 2 ) CIN      ! The number of the input NDF
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Processing type of the data
                                 ! array
      CHARACTER * ( NDF__SZFTP ) DTYPEV ! Processing type of the
                                 ! variance array
      INTEGER ELI                ! Number of elements in a mapped input
                                 ! array
      INTEGER ELO                ! Number of elements in an output array
      INTEGER I                  ! Loop counter
      INTEGER IEL( NDFMAX )      ! Number of elements in each input
                                 ! array
      INTEGER J                  ! Loop counter
      INTEGER IDIMS( NDF__MXDIM ) ! Dimensions of an input array
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the data
                                 ! array
      CHARACTER * ( NDF__SZTYP ) ITYPEV ! Processing type of the
                                 ! variance array
      INTEGER NC                 ! Number of characters in input NDF
                                 ! number
      INTEGER NDFI( NDFMAX )     ! Identifiers for input NDFs
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NUMNDF             ! Number of input NDFs
      INTEGER ODIMS( NDF__MXDIM ) ! Dimensions of output array
      INTEGER OFFSET( NDF__MXDIM ) ! Offsets of an input NDF wrt output
                                 ! array's origin
      CHARACTER * ( 4 ) PNIN     ! Parameter names of each of the input
                                 ! NDFS
      INTEGER PNTRI( 1 )         ! Pointer to input array component
      INTEGER PNTRO( 1 )         ! Pointer to output data array component
      INTEGER PNTROQ( 1 )        ! Pointer to output quality component
      INTEGER PNTROV( 1 )        ! Pointer to output variance component
      LOGICAL QUAPRS             ! Quality is present in the NDF?
      LOGICAL TRANSP             ! Bad values in the input NDFs are
                                 ! transparent when pasted
      LOGICAL VARPRS             ! Variance is present in the NDF?
      INTEGER XDISPL( NDFMAX )   ! X-offsets of an input NDF wrt output

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  By definition the values are chained verbatim.
      TRANSP = .FALSE.

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDFs.
*  ======================

*  First the principal NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI( 1 ), STATUS )

*  Make a loop to input the NDFs, via parameters C1, C2,...  Start an
*  error context because a null is used to end the list of NDFs.  Since
*  the order and bounds given after the NDF name are important, IRG
*  cannot be used safely.
      CALL ERR_MARK
      I = 0
      DO WHILE ( STATUS .EQ. SAI__OK .AND. I .LT. NDFMAX - 1 )
         I = I + 1
         CALL CHR_ITOC( I, CIN, NC )
         PNIN = 'C'//CIN( :NC )
         CALL LPG_ASSOC( PNIN, 'READ', NDFI( I + 1 ), STATUS )
      END DO

*  Look for the expected null.  Note the first pasted NDF may not be
*  null as there must be at least one pasted NDF.
      IF ( STATUS .EQ. PAR__NULL .AND. I .GT. 1 ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Record the number of input NDFs.  Make it at least one to prevent
*  problems exiting.  The number is I, not I-1 because the subtraction
*  for the extra loop is counteracted by plus one for the principal
*  NDF, except for the last input NDF as the loop is not entered after
*  it has been obtained.
      IF ( I .GE. NDFMAX - 1 ) THEN
         NUMNDF = NDFMAX
      ELSE
         NUMNDF = MAX( 1, I )
      END IF

*  Determine the size of the output NDF and offsets of input NDFs.
*  ===============================================================
*
*  To be general we want to obtain the dimensions up to the maximum,
*  even though many of these may be one.
      ELO = 0
      DO I = 1, NUMNDF
         CALL NDF_SIZE( NDFI( I ), IEL( I ), STATUS )

*  Determine the offset for the ith NDF.
         XDISPL( I ) = ELO
         ELO = ELO + IEL( I )
      END DO

*  Specify the dimensions.  The pasting routine works in n-d, so the
*  higher dimensions must be given even though they are insignificant.
      ODIMS( 1 ) = ELO
      DO J = 2, NDF__MXDIM
         ODIMS( J ) = 1
      END DO

*  Match the bad-pixel flags and data types of the input NDFs.
*  ===========================================================

*  Determine which array components are present.
      CALL NDF_STATE( NDFI( 1 ), 'Variance', VARPRS, STATUS )
      CALL NDF_STATE( NDFI( 1 ), 'Quality', QUAPRS, STATUS )

*  Match the bad-pixel flags.
      CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Data', .FALSE., BAD,
     :                STATUS )

      IF ( VARPRS ) CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Variance',
     :   .FALSE., BADVAR, STATUS )

      IF ( QUAPRS ) CALL NDF_MBADN( .TRUE., NUMNDF, NDFI, 'Quality',
     :   .FALSE., BADQUA, STATUS )

*  Match the data types.  Quality must have type _UBYTE.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'/
     :  /'_REAL,_DOUBLE', NUMNDF, NDFI, 'Data', ITYPE, DTYPE, STATUS )

      IF ( VARPRS ) CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,'/
     :  /'_INTEGER,_INT64,_REAL,_DOUBLE', NUMNDF, NDFI, 'Variance',
     :  ITYPEV, DTYPEV, STATUS )

*  Create the output NDF.
*  ======================
*
*  At this point any of array of identifiers or the principal NDF will
*  have the bounds of the output pasted NDF.  Want to propagate from
*  the principal array.
      CALL LPG_PROP( NDFI( 1 ), 'UNITS', 'OUT', NDFO, STATUS )

*  Set the array types.
      CALL NDF_STYPE( DTYPE, NDFO, 'Data', STATUS )
      IF ( VARPRS ) CALL NDF_STYPE( DTYPEV, NDFO, 'Variance', STATUS )

*  Set the bounds and dimensions of the output NDF.
      CALL NDF_SBND( 1, 1, ELO, NDFO, STATUS )

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  A null results in the output title being the same as the input
*  title because the ttitle was already propagated by LPG_PROP above.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Map the output data array component.
*  ====================================
      CALL NDF_MAP( NDFO, 'Data', ITYPE, 'WRITE/BAD', PNTRO, ELO,
     :              STATUS )
      IF ( VARPRS ) CALL NDF_MAP( NDFO, 'Variance', ITYPEV, 'WRITE/BAD',
     :                            PNTROV, ELO, STATUS )
      IF ( QUAPRS ) CALL NDF_MAP( NDFO, 'Quality', '_UBYTE',
     :                            'WRITE/ZERO', PNTROQ, ELO, STATUS )

*  Main loop to paste in the NDFs in turn.
*  =======================================

*  Pasting will apply to all the NDF array components in turn.  First
*  the data array.
      DO I = 1, NUMNDF

*  Derive the offsets of the original input NDFs with respect to the
*  origin of the output NDF.  Also extract the dimensions of the
*  current NDF.  The pasting routine works in n-d, so the higher
*  dimensions must be given even though they are insignificant.
         OFFSET( 1 ) = XDISPL( I )
         IDIMS( 1 ) = IEL( I )

         DO J = 2, NDF__MXDIM
            OFFSET( J ) = 0
            IDIMS( J ) = 1
         END DO

*  Paste the data array.
*  =====================

*  As values are merely passed verbatim, the automatic quality masking
*  is switched off.
         CALL NDF_SQMF( .FALSE., NDFI( I ), STATUS )

*  Map the input NDF data array.
         CALL NDF_MAP( NDFI( I ), 'Data', ITYPE, 'READ', PNTRI, ELI,
     :                 STATUS )

*  Call the appropriate routine that performs the concatenation of the
*  data array.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_PASTR( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS, ELO,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_PASTB( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_PASTD( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS, ELO,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_PASTI( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS, ELO,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPG1_PASTK( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS, ELO,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_PASTUB( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                        %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                        ODIMS, ELO,
     :                        %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_PASTUW( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                        %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                        ODIMS, ELO,
     :                        %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

          ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_PASTW( TRANSP, BAD, OFFSET, IDIMS, ELI,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS, ELO,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         END IF

*  Unmap the input data array.
         CALL NDF_UNMAP( NDFI( I ), 'Data', STATUS )

*  Paste the variance array.
*  =========================
         IF ( VARPRS ) THEN

*  As values are merely passed verbatim, the automatic quality masking
*  is switched off.  It was reset by NDF_UNMAP.
            CALL NDF_SQMF( .FALSE., NDFI( I ), STATUS )

*  Map the input NDF variance, obtaining an array of bad values if
*  there is no variance array in this NDF.
            CALL NDF_MAP( NDFI( I ), 'Variance', ITYPE, 'READ/BAD',
     :                    PNTRI, ELI, STATUS )

*  Call the appropriate routine that performs the concatenation of the
*  data array.
            IF ( ITYPEV .EQ. '_REAL' ) THEN
               CALL KPG1_PASTR( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPEV .EQ. '_BYTE' ) THEN
               CALL KPG1_PASTB( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPEV .EQ. '_DOUBLE' ) THEN
               CALL KPG1_PASTD( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPEV .EQ. '_INTEGER' ) THEN
               CALL KPG1_PASTI( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPEV .EQ. '_INT64' ) THEN
               CALL KPG1_PASTK( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPEV .EQ. '_UBYTE' ) THEN
               CALL KPG1_PASTUB( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPEV .EQ. '_UWORD' ) THEN
               CALL KPG1_PASTUW( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                           %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPEV .EQ. '_WORD' ) THEN
               CALL KPG1_PASTW( TRANSP, BADVAR, OFFSET, IDIMS, ELI,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          ODIMS, ELO,
     :                          %VAL( CNF_PVAL( PNTROV( 1 ) ) ),
     :                          STATUS )
            END IF

*  Unmap the variance array, as we may already have three other arrays
*  mapped.
            CALL NDF_UNMAP( NDFI( I ), 'Variance', STATUS )
         END IF

*  Paste the quality array.
*  ========================
         IF ( QUAPRS ) THEN

*  Map the input NDF quality, which has type unsigned byte.  This will
*  be an array of zeroes if there is no quality array in this NDF.
            CALL NDF_MAP( NDFI( I ), 'Quality', '_UBYTE',
     :                    'READ/ZERO', PNTRI, ELI, STATUS )

*  Call the routine that performs the concatenation of the quality
*  array.
            CALL KPG1_PASTUB( TRANSP, BADQUA, OFFSET, IDIMS, ELI,
     :                        %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                        ODIMS, ELO,
     :                        %VAL( CNF_PVAL( PNTROQ( 1 ) ) ), STATUS )

*  Unmap the quality array, as we may already have three other arrays
*  mapped.
            CALL NDF_UNMAP( NDFI( I ), 'Quality', STATUS )
         END IF

*  Release the input NDF identifier.
         CALL NDF_ANNUL( NDFI( I ), STATUS )
      END DO

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CHAIN_ERR',
     :      'CHAIN: Unable to concatenate vectorized NDFs.', STATUS )
      END IF

      END
