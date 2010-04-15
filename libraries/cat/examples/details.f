      SUBROUTINE DETAILS (STATUS)
*+
*  Name:
*     DETAILS
*  Purpose:
*     Extract the details of a catalogue and write them to a text file.
*  Language:
*     Fortran 77
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL DETAILS (STATUS)
*  Arguments:
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Description:
*     Extract the details of a catalogue and write them to a text file.
*     The details include the number of columns rows and parameters in
*     the catalogue.  Additionally, details are given for each
*     individual column and parameter.  These details include the
*     name, data type etc.
*  ADAM Parameters:
*     CNAME  =  CHARACTER (READ)
*        Name of the catalogue.
*  Algorithm:
*     Attempt to open the catalogue.
*     Obtain the details of the catalogue.
*     Do while there are more columns
*       Attempt to obtain an identifier to the next column.
*       If ok then
*         Obtain the details of the column.
*         Write the details of the column.
*       else
*         Set the termination flag.
*       end if
*     end do
*     Do while there are more parameters
*       Attempt to obtain an identifier to the next parameter.
*       If ok then
*         Obtain the details of the parameter.
*         Write the details of the parameter.
*       else
*         Set the termination flag.
*       end if
*     end do
*     Release the identifier for the catalogue.
*  Implementation Deficiencies:
*     The current implementation has the following deficiencies.
*
*     1. Output is simply written to the default Fortran output stream.
*     It is not explicitly opened, and indeed, there are no checks that
*     it is free.  This approach is adopted because the application is
*     intended as an example of using the CAT routines and I do not
*     want to clutter it with too much extraneous checking.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     13/7/93  (ACD): Original version.
*     16/7/93  (ACD): First ADAM version.
*     28/1/94  (ACD): Tidied up for release.
*     23/11/94 (ACD): Changed output from unit 17 to the default
*        output stream.
*     19/4/95  (ACD): Changed parametric constants to correspond to changes
*        in the INCLUDE file CAT_PAR.
*     16/12/96 (ACD): Removed non-standard features revealed by the port
*        to Linux (based on modifications by BKM).
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CI,         ! Catalogue identifier.
     :  QI,         ! Parameter identifier.
     :  FI,         ! Column (or field) identifier.
     :  QCOUNT,     ! Number of the current parameter.
     :  FCOUNT,     !   "    "   "     "    columns.
     :  NUMROW,     ! Number of rows       in the catalogue.
     :  NUMCOL,     !   "    "  columns    "   "     "     .
     :  NUMIND,     !   "    "  indices    "   "     "     .
     :  NUMPAR      !   "    "  parameters "   "     "     .
      CHARACTER
     :  CNAME*80,   ! Catalogue name.
     :  COLFMT*500, ! Format for column    details.
     :  PARFMT*500  !   "     "  parameter    "   .
      DOUBLE PRECISION
     :  DATE        ! Modification date of the catalogue.
      LOGICAL
     :  MORE        ! Flag: more parameters or columns to access?

*
*    The following variables represent the attributes of the current
*    column (or field).

      INTEGER
     :  FCI,         ! Parent catalogue.
     :  FGENUS,      ! Genus.
     :  FDTYPE,      ! Data type.
     :  FCSIZE,      ! Size if a character string.
     :  FDIMS,       ! Dimensionality.
     :  FSIZEA(10),  ! Size of each array dimension.
     :  FNULL,       ! Null flag.
     :  FORDER,      ! Order.
     :  FCTYPS       ! Size of FCTYPE (excl. trail. blanks).
      CHARACTER
     :  FNAME*(CAT__SZCMP),    ! Name.
     :  FEXPR*(CAT__SZEXP),    ! Defining expression.
     :  FXCEPT*(CAT__SZVAL),   ! Exception value.
     :  FUNITS*(CAT__SZUNI),   ! Units.
     :  FXTFMT*(CAT__SZEXF),   ! External format.
     :  FCOMM*(CAT__SZCOM),    ! Comments.
     :  FCTYPE*10              ! Character representation of data type.
      DOUBLE PRECISION
     :  FSCALEF,     ! Scale factor.
     :  FZEROP,      ! Zero point.
     :  FDATE        ! Modification date.
      LOGICAL
     :  FPRFDS      ! Preferential display flag.

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  QCI,         ! Parent catalogue.
     :  QDTYPE,      ! Data type.
     :  QCSIZE,      ! Size if a character string.
     :  QDIMS,       ! Dimensionality.
     :  QSIZEA(10),  ! Size of each array dimension.
     :  QCTYPS       ! Size of QCTYPE (excl. trail. blanks).
      CHARACTER
     :  QNAME*(CAT__SZCMP),    ! Name.
     :  QUNITS*(CAT__SZUNI),   ! Units.
     :  QXTFMT*(CAT__SZEXF),   ! External format.
     :  QCOMM*(CAT__SZCOM),    ! Comments.
     :  QVALUE*(CAT__SZVAL),   ! Value.
     :  QCTYPE*10              ! Character representation of data type.
      LOGICAL
     :  QPRFDS      ! Preferential display flag.
      DOUBLE PRECISION
     :  QDATE        ! Modification date.

*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain the name of the catalogue and attempt to open it.

         CALL PAR_GET0C ('CNAME', CNAME, STATUS)

         CALL CAT_TOPEN (CNAME, 'OLD', 'READ', CI, STATUS)

         WRITE(*, 1001) CNAME
 1001    FORMAT(1H1, 'Details of Catalogue ', A80 / )

*
*       Obtain the details of the catalogue.

         CALL CAT_TDETL (CI, CAT__GALL, NUMROW, NUMCOL, NUMIND, NUMPAR,
     :     DATE, STATUS)
         WRITE(*, 1002) NUMROW, NUMCOL, NUMPAR, NUMIND, DATE
 1002    FORMAT(
     :     3X, 'Number or rows: ', I5/
     :     3X, 'Number of columns: ', I5 /
     :     3X, 'Number of parameters: ', I5 /
     :     3X, 'Number of indices: ', I5 /
     :     3X, 'Modification date: ', 1PD12.3 )

*
*       Obtain the details for all the columns.

         WRITE(*, 1003)
 1003    FORMAT(// 1X, 'Details of the Columns.' / )

         FCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CI, CAT__FITYP, FCOUNT, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN
               CALL CAT_CINQ (FI, 10, FCI, FNAME, FGENUS, FEXPR,
     :           FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALEF,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :           FDATE, STATUS)

               FCTYPE = ' '
               FCTYPS = 0

               CALL CAT_TYFMT (FDTYPE, FCSIZE, FCTYPE, FCTYPS, STATUS)

               WRITE(COLFMT, 1004) CAT__SZCMP, CAT__SZVAL, CAT__SZUNI,
     :           CAT__SZEXF, CAT__SZCOM
 1004          FORMAT( '(',
     :           '1X, ''Column number: '', I5, '':'' /',
     :           '3X, ''Name: '', A', I4, '/',
     :           '3X, ''Genus: '', I5 /',
     :           '3X, ''Expression: '', A80 /',
     :           '3X, ''Data type: '', A10 /',
     :           '3X, ''Dimensionality: '', I5 /',
     :           '3X, ''Size in first dimension: '', I10 /',
     :           '3X, ''Treatment of null values: '', I5 /',
     :           '3X, ''Exception value: '', A', I4, '/',
     :           '3X, ''Scale Factor: '', 1PD12.3 /',
     :           '3X, ''Zero point: '', 1PD12.3 /',
     :           '3X, ''Order: '', I5 /',
     :           '3X, ''Units: '', A', I4, '/',
     :           '3X, ''External format: '', A', I4,  '/',
     :           '3X, ''Preferential display flag: '', L5 /',
     :           '3X, ''Comments: '', A', I4, '/',
     :           '3X, ''Modification date: '', 1PD12.3 / )' )

               WRITE(*, COLFMT) FCOUNT, FNAME, FGENUS, FEXPR(1 : 80),
     :           FCTYPE, FDIMS, FSIZEA(1), FNULL, FXCEPT, FSCALEF,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM, FDATE

            ELSE
               MORE = .FALSE.

            END IF

         END DO

*
*       Obtain the details for all the parameters.

         WRITE(*, 1005)
 1005    FORMAT(/ 1X, 'Details of the parameters.' / )

         QCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            QCOUNT = QCOUNT + 1

            CALL CAT_TNDNT (CI, CAT__QITYP, QCOUNT, QI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  QI .NE. CAT__NOID) THEN
               CALL CAT_PINQ (QI, 10, QCI, QNAME, QDTYPE, QCSIZE, QDIMS,
     :           QSIZEA, QUNITS, QXTFMT, QPRFDS, QCOMM, QVALUE, QDATE,
     :           STATUS)

               QCTYPE = ' '
               QCTYPS = 0

               CALL CAT_TYFMT (QDTYPE, QCSIZE, QCTYPE, QCTYPS, STATUS)

               WRITE(PARFMT, 1006) CAT__SZCMP, CAT__SZUNI, CAT__SZEXF,
     :           CAT__SZCOM, CAT__SZVAL
 1006          FORMAT( '(',
     :           '1X, ''Parameter number: '', I5, '':'' /',
     :           '3X, ''Name: '', A', I4, '/',
     :           '3X, ''Data type:'', A10 /',
     :           '3X, ''Dimensionality: '', I5 /',
     :           '3X, ''Size of first dimension: '', I10 /',
     :           '3X, ''Units: '', A', I4, '/',
     :           '3X, ''External format: '', A', I4, '/',
     :           '3X, ''Preferential display flag: '', L5 /',
     :           '3X, ''Comments: '', A', I4, '/',
     :           '3X, ''Value: '', A', I4, '/',
     :           '3X, ''Modification date: '', 1PD12.3 / )' )

               WRITE(*, PARFMT) QCOUNT, QNAME, QCTYPE, QDIMS,
     :           QSIZEA(1), QUNITS, QXTFMT, QPRFDS, QCOMM, QVALUE,
     :           QDATE

            ELSE
               MORE = .FALSE.

            END IF

         END DO

*
*       Release the identifier for the catalogue.

         CALL CAT_TRLSE (CI, STATUS)

      END IF

      END
