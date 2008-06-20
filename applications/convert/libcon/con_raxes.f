      SUBROUTINE CON_RAXES (RLOC, CUBID, STATUS)
*+
*  Name:
*     CON_RAXES
*  Purpose:
*     Construct the NDF axis components for an Asterix data cube.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL SR_CAXES (RLOC, CUBID; STATUS)
*  Description:
*     Construct the NDF axis components for an Asterix data cube.
*  Arguments:
*     RLOC  =  CHARACTER*(*) (Given)
*        Locator to the input cube.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Generate each of the three axis components in turn.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}
*
*  History:
*     14/7/97 (ACD):
*       Original version.
*     3/9/97  (ACD):
*       First stable version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
*  Arguments Given:
      CHARACTER
     :  RLOC*(*)
      INTEGER
     :  CUBID
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN         ! Length of a string less trailing blanks
*  Local Variables:
      CHARACTER
     :  AXLOC*(DAT__SZLOC),  ! Locator to the axis component
     :  CELLOC*(DAT__SZLOC), !    "    "   "   "   cell (array element)
     :  ARLOC*(DAT__SZLOC),  !    "    "   "   "   data array
     :  SCLLOC*(DAT__SZLOC), !    "    "   "   "   scale factor
     :  ESCLOC*(DAT__SZLOC), !    "    "   "  energy axis scale factor
     :  EZPLOC*(DAT__SZLOC), !    "    "   "    "     "   zero point
     :  LABLOC*(DAT__SZLOC), !    "    "   "    "     "   label
     :  UNTLOC*(DAT__SZLOC), !    "    "   "    "     "   units
     :  LABEL*80,            ! Energy axis label
     :  UNITS*80             !   "     "   units
      INTEGER
     :  AXPTR(3), ! Axis pointers
     :  NAXIS(3), ! Number of elements in each axis
     :  NC        ! Used length of string
      REAL
     :  SCALE,    ! Axis scale factor (arcmin)
     :  ESCALE,   ! Energy axis scale factor
     :  EZEROP    !   "     "   zero point
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       First axis; energy.

         CALL NDF_AMAP (CUBID, 'CENTRE', 1, '_REAL', 'WRITE',
     :     AXPTR(1), NAXIS(1), STATUS)

         CALL DAT_FIND (RLOC, 'AXIS', AXLOC, STATUS)
         CALL DAT_CELL (AXLOC, 1, 3, CELLOC, STATUS)
         CALL DAT_FIND (CELLOC, 'DATA_ARRAY', ARLOC, STATUS)

         CALL DAT_FIND (ARLOC, 'SCALE', ESCLOC, STATUS)
         CALL DAT_GET0R (ESCLOC, ESCALE, STATUS)

         CALL DAT_FIND (ARLOC, 'BASE', EZPLOC, STATUS)
         CALL DAT_GET0R (EZPLOC, EZEROP, STATUS)

         CALL CON_LAXIS (EZEROP, ESCALE, NAXIS(1), 
     :                   %VAL(CNF_PVAL(AXPTR(1))),
     :     STATUS)
         CALL NDF_AUNMP (CUBID, 'CENTRE', 1, STATUS)

         CALL DAT_FIND (CELLOC, 'LABEL', LABLOC, STATUS)
         CALL DAT_GET0C (LABLOC, LABEL, STATUS)
         NC = CHR_LEN (LABEL)
         CALL NDF_ACPUT (LABEL(:NC), CUBID, 'LABEL', 1, STATUS)

         CALL DAT_FIND (CELLOC, 'UNITS', UNTLOC, STATUS)
         CALL DAT_GET0C (UNTLOC, UNITS, STATUS)
         NC = CHR_LEN (UNITS)
         CALL NDF_ACPUT (UNITS(:NC), CUBID, 'UNITS', 1, STATUS)

*
*       Second axis; X offset.

         CALL NDF_AMAP (CUBID, 'CENTRE', 2, '_REAL', 'WRITE', 
     :     AXPTR(2), NAXIS(2), STATUS)

         CALL DAT_FIND (RLOC, 'AXIS', AXLOC, STATUS)
         CALL DAT_CELL (AXLOC, 1, 1, CELLOC, STATUS)
         CALL DAT_FIND (CELLOC, 'DATA_ARRAY', ARLOC, STATUS)
         CALL DAT_FIND (ARLOC, 'SCALE', SCLLOC, STATUS)

         CALL DAT_GET0R (SCLLOC, SCALE, STATUS)

*
*       Convert the scale from degrees to arcmin.

         SCALE = SCALE * 6.0E1

         CALL CON_SAXIS (SCALE, NAXIS(2), %VAL(CNF_PVAL(AXPTR(2))), 
     :                   STATUS)

         CALL NDF_AUNMP (CUBID, 'CENTRE', 2, STATUS)
         CALL NDF_ACPUT ('X offset', CUBID, 'LABEL', 2,
     :     STATUS)
         CALL NDF_ACPUT ('Arcmin', CUBID, 'UNITS', 2, STATUS)

*
*       Third axis; Declination offset.

         CALL NDF_AMAP (CUBID, 'CENTRE', 3, '_REAL', 'WRITE',
     :     AXPTR(3), NAXIS(3), STATUS)

         CALL DAT_FIND (RLOC, 'AXIS', AXLOC, STATUS)
         CALL DAT_CELL (AXLOC, 1, 2, CELLOC, STATUS)
         CALL DAT_FIND (CELLOC, 'DATA_ARRAY', ARLOC, STATUS)
         CALL DAT_FIND (ARLOC, 'SCALE', SCLLOC, STATUS)

         CALL DAT_GET0R (SCLLOC, SCALE, STATUS)

*
*       Convert the scale from degrees to arcmin.

         SCALE = SCALE * 6.0E1

         CALL CON_SAXIS (SCALE, NAXIS(3), %VAL(CNF_PVAL(AXPTR(3))), 
     :                   STATUS)

         CALL NDF_AUNMP (CUBID, 'CENTRE', 3, STATUS)
         CALL NDF_ACPUT ('Y offset', CUBID, 'LABEL', 3,
     :     STATUS)
         CALL NDF_ACPUT ('Arcmin', CUBID, 'UNITS', 3, STATUS)

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CON_RAXES_ERR', 'CON_RAXES: Failure '/
     :        /'setting the axis components for the output cube.',
     :        STATUS)
         END IF

      END IF

      END
