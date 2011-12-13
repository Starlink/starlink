      SUBROUTINE RTD1_ARDST( BAD, SIMPLE, DATSRC, SHOHED, IGRP, DIM1,
     :                       DIM2, LBND, UBND, TYPE, IPIN, STATUS )
*+
* Name:
*    RTD1_ARDST

*  Purpose:
*     Calculates the statistics of an image region.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL RTD1_ARDST( BAD, DATSRC, SHOHED, SIMPLE, IGRP, DIM1, DIM2,
*                      LBND, UBND, TYPE, IPIN, STATUS )

*  Description:
*      This routine calculates the basic statistics of the pixels
*      with a given ARD region. The results are reported via the
*      message system.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether BAD pixels may be present.
*     SIMPLE = LOGICAL (Given)
*        Whether to output results in simple one-line form.
*     DATSRC = CHARACTER *( * ) (Given)
*        A string describing the data source (i.e. Data or NDF
*        component). This will be used to qualify the report.
*     SHOHED = LOGICAL (Given)
*        Whether to show the column headers (SIMPLE=.TRUE. only)
*     IGRP = INTEGER (Given)
*        ARD description of region to use.
*     DIM1 = INTEGER (Given)
*        First dimension of the data array.
*     DIM2 = INTEGER (Given)
*        Second dimension of the data array.
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the original NDF.
*     UBND( 2 ) = INTEGER (Given)
*        The upper bounds of the original NDF.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type of the data pointed to by IPIN.
*     IPIN = INTEGER (Given)
*        Pointer to the data to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998-2004 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1996 (PWD):
*        Original version.
*     28-APR-2004 (PWD):
*        Added DATSRC argument.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'GRP_PAR'         ! GRP constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants
      INCLUDE 'MSG_PAR'         ! Message system buffers
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      LOGICAL BAD
      LOGICAL SIMPLE
      LOGICAL SHOHED
      CHARACTER * ( * ) DATSRC
      INTEGER IGRP
      INTEGER DIM1
      INTEGER DIM2
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      CHARACTER * ( * ) TYPE
      INTEGER IPIN

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Output information buffer
      CHARACTER * ( MSG__SZMSG ) HEADER ! Titles
      INTEGER IPMASK            ! Pointer to ARD mask data
      INTEGER LBNDE( 2 )        ! Lower bounds of ARD bounding box
      INTEGER LBNDI( 2 )        ! Lower bounds of ARD bounding box
      INTEGER REGVAL            ! Region integer
      INTEGER UBNDE( 2 )        ! Upper bounds of ARD bounding box
      INTEGER UBNDI( 2 )        ! Upper bounds of ARD bounding box
      REAL TRCOEF( 1 )          ! ARD description transformation
      DOUBLE PRECISION DMIN     ! Data minimum
      DOUBLE PRECISION DMAX     ! Data maximum
      DOUBLE PRECISION SUM      ! Sum of data
      DOUBLE PRECISION MEAN     ! Data mean
      DOUBLE PRECISION STDEV    ! Standard deviation
      INTEGER NGOOD             ! Number of pixel counted
      INTEGER NOWAT             ! Position in string
      INTEGER WASAT             ! Position in string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get workspace for storing mask.
      CALL PSX_CALLOC( DIM1 * DIM2, '_INTEGER', IPMASK, STATUS )

*  Convert the ARD descriptions into a MASK.
      REGVAL = 2
      TRCOEF( 1 ) = VAL__BADR
      LBNDI( 1 ) = VAL__MAXI
      LBNDE( 1 ) = VAL__MAXI
      CALL ARD_WORK( IGRP, 2, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( CNF_PVAL( IPMASK ) ), LBNDI, UBNDI,
     :               LBNDE, UBNDE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now calculate the statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_STATB( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ), NGOOD,
     :                    DMIN, DMAX, SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_STATUB( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN,
     :                     DMAX, SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_STATW( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN, DMAX,
     :                    SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_STATUW( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN,
     :                     DMAX, SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_STATI( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN, DMAX,
     :                    SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_STATR( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN, DMAX,
     :                    SUM, MEAN, STDEV, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_STATD( BAD, DIM1 * DIM2, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPMASK ) ), NGOOD, DMIN, DMAX,
     :                    SUM, MEAN, STDEV, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'RTD1_ARDST', 'RTD1_ARDST: Unknown'//
     :   ' data type^TYPE - possible programming error', STATUS )
         GO TO 99
      END IF

*  Now write out the values:
      IF ( SIMPLE ) THEN

*  Write each value into the output buffer one-by-one (use this to
*  keep control of the formatting using CHR_ routines, rather than
*  one fixed format for every value).
         BUFFER = ' '
         HEADER = ' '
         NOWAT = 1
         WASAT = 1
         HEADER( NOWAT: ) = ' Mean'
         IF ( MEAN .LT. DBLE( VAL__MAXR ) .AND.
     :        MEAN .GT. DBLE( VAL__MINR ) ) THEN
            CALL CHR_PUTR( REAL( MEAN ), BUFFER, NOWAT )
         ELSE
            CALL CHR_PUTD( MEAN, BUFFER, NOWAT )
         END IF
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         HEADER( NOWAT: ) = ' Std. Dev.'
         IF ( STDEV .LT. DBLE( VAL__MAXR ) .AND.
     :        STDEV .GT. DBLE( VAL__MINR ) ) THEN
            CALL CHR_PUTR( REAL( STDEV ), BUFFER, NOWAT )
         ELSE
            CALL CHR_PUTD( STDEV, BUFFER, NOWAT )
         END IF
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         HEADER( NOWAT: ) = ' Max.'
         IF ( DMAX .LT. DBLE( VAL__MAXR ) .AND.
     :        DMAX .GT. DBLE( VAL__MINR ) ) THEN
            CALL CHR_PUTR( REAL ( DMAX ), BUFFER, NOWAT )
         ELSE
            CALL CHR_PUTD( DMAX, BUFFER, NOWAT )
         END IF
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         HEADER( NOWAT: ) = ' Min.'
         IF ( DMIN .LT. DBLE( VAL__MAXR ) .AND.
     :        DMIN .GT. DBLE( VAL__MINR ) ) THEN
            CALL CHR_PUTR( REAL( DMIN ), BUFFER, NOWAT )
         ELSE
            CALL CHR_PUTD( DMIN, BUFFER, NOWAT )
         END IF
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         HEADER( NOWAT: ) = ' Total'
         IF ( DMIN .LT. DBLE( VAL__MAXR ) .AND.
     :        DMIN .GT. DBLE( VAL__MINR ) ) THEN
            CALL CHR_PUTR( REAL( SUM ), BUFFER, NOWAT )
         ELSE
            CALL CHR_PUTD( SUM, BUFFER, NOWAT )
         END IF
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         HEADER( NOWAT: ) = ' No. Pixels'
         CALL CHR_PUTI( NGOOD, BUFFER, NOWAT )
         NOWAT = MAX( WASAT + 11, NOWAT + 1 )
         WASAT = NOWAT

         IF ( DATSRC .NE. ' ' ) THEN
            BUFFER( NOWAT: ) = '('
            CALL CHR_PUTC( DATSRC, BUFFER, NOWAT )
            NOWAT = NOWAT + 1
            BUFFER( NOWAT: ) = ')'
         END IF

         IF ( SHOHED ) THEN
            CALL MSG_OUT( ' ', HEADER, STATUS )
         END IF
         CALL MSG_OUT( ' ', BUFFER, STATUS )

      ELSE
         IF ( DATSRC .NE. ' ' ) THEN
            CALL MSG_SETC( 'DATSRC', DATSRC )
            CALL MSG_OUT( ' ', ' ^DATSRC:', STATUS )
         END IF

         CALL MSG_SETD( 'MEAN', MEAN )
         CALL MSG_OUT( ' ','   Mean                = ^MEAN', STATUS )

         CALL MSG_SETD( 'STDEV', STDEV )
         CALL MSG_OUT( ' ','   Standard deviation  = ^STDEV', STATUS )

         CALL MSG_SETD( 'DMAX', DMAX )
         CALL MSG_OUT( ' ','   Maximum             = ^DMAX', STATUS )

         CALL MSG_SETD( 'DMIN', DMIN )
         CALL MSG_OUT( ' ','   Minimum             = ^DMIN', STATUS )

         CALL MSG_SETD( 'SUM', SUM )
         CALL MSG_OUT( ' ','   Sum                 = ^SUM', STATUS )

         CALL MSG_SETI( 'NGOOD', NGOOD )
         CALL MSG_OUT( ' ','   Number of pixels    = ^NGOOD', STATUS )
      END IF


*  Exit in error label (put clean up code after this).
 99   CONTINUE

*  Free up dynamic memory.
      CALL PSX_FREE( IPMASK, STATUS )
      END


