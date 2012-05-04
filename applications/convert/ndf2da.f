      SUBROUTINE NDF2DA( STATUS )
*+
*  Name:
*     NDF2DA

*  Purpose:
*     Converts an NDF to a direct-access unformatted file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2DA( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF to a direct-access unformatted
*     file, which is equivalent to fixed-length records, or a data
*     stream suitable for reading by C routines.  Only one of the array
*     components may be copied to the output file.

*  Usage:
*     ndf2da in out [comp] [noperec]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF component to be copied.  It may be "Data", "Quality"
*        or "Variance".  ["Data"]
*     IN = NDF (Read)
*        Input NDF data structure.  The suggested default is the current
*        NDF if one exists, otherwise it is the current value.
*     NOPEREC = _INTEGER (Read)
*        The number of data values per record of the output file.  It
*        must be positive.  The suggested default is the current value.
*        [The first dimension of the NDF]
*     OUT = FILENAME (Write)
*        Name of the output direct-access unformatted file.

*  Examples:
*     ndf2da cluster cluster.dat
*        This copies the data array of the NDF called cluster to a
*        direct-access unformatted file called cluster.dat.  The number
*        of data values per record is equal to the size of the first
*        dimension of the NDF.
*     ndf2da cluster cluster.dat v
*        This copies the variance of the NDF called cluster to a
*        direct-access unformatted file called cluster.dat.  The number
*        of variance values per record is equal to the size of the
*        first dimension of the NDF.
*     ndf2da cluster cluster.dat noperec=12
*        This copies the data array of the NDF called cluster to a
*        direct-access unformatted file called {\tt cluster.dat}.  There are
*        twelve data values per record in cluster.dat.

*  Notes:
*     The details of the conversion are as follows:
*        -  the NDF array as selected by COMP is written to the
*        unformatted file in records.
*        -  all other NDF components are lost.

*  Related Applications:
*     CONVERT: DA2NDF.

*  Copyright:
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*     Councils.  Copyright (C) 2011-2012 Science & Technology Facilities
*     Council.  All Rights Reserved.

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
*     1996 October 20 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2011 January 12 (MJC):
*        Use KPG_TYPSZ instead of COF_TYPSZ.
*     2012 April 30 (MJC):
*        Add _INT64 type.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 25 ) COMLIS  ! List of the NDF components
      INTEGER COMLN              ! The used length of COMLIS
      CHARACTER * ( 8 ) COMP     ! The component of NDF to plot
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER EL                 ! Number of mapped elements
      INTEGER FD                 ! File descriptor
      INTEGER LUN                ! Logical-unit number
      CHARACTER MACHIN * ( 24 )  ! Machine name
      INTEGER NBYTES             ! Number of bytes per value
      INTEGER NDF                ! Identifier for NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER NUMMAX             ! Maximum number of values per record
      CHARACTER NODE * ( 20 )    ! Node name
      INTEGER NUMPRE             ! Number of data values per record
      INTEGER PNTR( 1 )          ! Pointer to NDF mapped array
      INTEGER RECL               ! Maximum recordlength of unformatted
                                 ! file in bytes
      CHARACTER RELEAS * ( 10 )  ! Release of operating system
      CHARACTER SYSNAM * ( 10 )  ! Operating system
      LOGICAL THERE              ! NDF component is present?
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type for processing
      CHARACTER VERSIO * ( 10 )  ! Sub-version of operating system
      LOGICAL VMS                ! True if running on a VAX/VMS system

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*  Find out which component is to be processed.
*  ============================================

*  Form the component list of the input NDF. Data-array component must
*  exist for the file to be an NDF.
      COMLIS = 'Data'
      COMLN = 4

*  If the Quality component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Quality', COMLIS, COMLN )
      END IF

*  If the Variance component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Variance', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Variance', COMLIS, COMLN )
      END IF

*  Find which component to copy.
      CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ), .FALSE., COMP,
     :                 STATUS )

*  Find the data type of the component.
      CALL NDF_TYPE( NDF, COMP, TYPE, STATUS )

*  Determine whether or not the operating system is VMS.
*  =====================================================
*
*  This assumes that the system is either VMS or UNIX.  It is needed
*  to specify the path of the file containing the global parameters.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
      VMS = INDEX( SYSNAM, 'VMS' ) .NE. 0

*  Find the formatting arrangement for the output file.
*  ====================================================

*  Derive the maximum number of values per record for the data type.
*  The maximum is imposed by VMS (8191 maximum number of longwords).
*  On UNIX set it to the largest innteger, i.e. no practical limit.
      IF ( .NOT. VMS ) THEN
         NUMMAX = VAL__MAXI

      ELSE

*  Obtain the number of bytes corresponding to the chosen data type.
         CALL KPG_TYPSZ( TYPE, NBYTES, STATUS )
         NUMMAX = 32764 / NBYTES

      END IF

*   Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Obtain the number of values per record.  Constrain the dynamic
*  default so that it is in range.  In normal circumstances it will
*  be the first dimension of the NDF.
      CALL PAR_GDR0I( 'NOPEREC', MIN( NUMMAX, DIMS( 1 ) ), 1, NUMMAX,
     :                .FALSE., NUMPRE, STATUS )

*  Derive the recordlength in bytes.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         RECL = NUMPRE * VAL__NBD

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         RECL = NUMPRE * VAL__NBR

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         RECL = NUMPRE * VAL__NBI

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         RECL = NUMPRE * VAL__NBK

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         RECL = NUMPRE * VAL__NBUW

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         RECL = NUMPRE * VAL__NBW

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         RECL = NUMPRE * VAL__NBUB

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         RECL = NUMPRE * VAL__NBB

      END IF

*  Open the unformatted file.
*  ==========================

*  Open the direct-access unformatted file.
      CALL RIO_ASSOC( 'OUT', 'WRITE', 'UNFORMATTED', RECL, FD, STATUS )

*  Obtain the logical-unit number to the file.
      CALL FIO_UNIT( FD, LUN, STATUS )

*  Process the input array.
*  ========================

*  Map the input data array using its actual data type.
      CALL NDF_MAP( NDF, COMP, TYPE, 'READ', PNTR, EL, STATUS )

*  Call a routine to write the data to the unformatted direct-access
*  file; the selected routine depending on the data type of the array.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL CON_OUDAB( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CON_OUDAD( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL CON_OUDAI( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL CON_OUDAK( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CON_OUDAR( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL CON_OUDAUB( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL CON_OUDAUW( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL CON_OUDAW( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      END IF

  999 CONTINUE

*  Close the output file.
      CALL FIO_CLOSE( FD, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF2DA_ERR',
     :     'NDF2DA: Error dumping an NDF array to an unformatted '/
     :     /'direct-access file.', STATUS )
      END IF

      END
