      SUBROUTINE CAP_CPCRD (CIIN, CIOUT, FULL, CRDTYP, MCRDCL, CRDI,
     :  EPOCHI, EQUINI, EPOCHO, EQUINO, CRDOI, MO, NUMCOL, FIIN, FIOUT,
     :  STATUS)
*+
*  Name:
*     CAP_CPCRD
*  Purpose:
*     Copy a table from an input to an output cat, calculating new coords.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPCRD (CIIN, CIOUT, FULL, CRDTYP, MCRDCL, CRDI,
*       EPOCHI, EQUINI, EPOCHO, EQUINO, CRDOI, MO, NUMCOL, FIIN, FIOUT;
*       STATUS)
*  Description:
*     Copy a table from an input to an output catalogue, calculating
*     new celestial coordinates for each row.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input catalogue.  Note that this identifier
*        may be a catalogue or index or a selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output catalogue.
*     FULL  =  LOGICAL (Given)
*        Flag; are full input coordinates to be used?  Coded as follows:
*        .TRUE.  -  Full input coordinates are to be used,
*        .FALSE. -  Only use input Right Ascension and Declination.
*     CRDTYP  =  INTEGER (Given)
*        Code indicating the type of coordinates which are to be
*        calculated.
*     MCRDCL  =  INTEGER (Given)
*        Number of parameters definined a full set of coordinates (=6).
*     CRDI(MCRDCL)  =  INTEGER (Given)
*        Identifiers for the columns containing the coordinates in the
*        input catalogue.
*     EPOCHI  =  CHARACTER*(*) (Given)
*        Input epoch.
*     EQUINI  =  CHARACTER*(*) (Given)
*        Input equinox.
*     EPOCHO  =  CHARACTER*(*) (Given)
*        Output epoch.
*     EQUINO  =  CHARACTER*(*) (Given)
*        Output equinox.
*     CRDOI(2)  =  INTEGER (Given)
*        Identifiers for the columns containing the coordinates in the
*        output catalogue.
*     MO(35)  =  DOUBLE PRECISION (Given)
*        Array for converting mean to observed coordinates.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns in the input (and hence output) catalogue.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the input catalogue.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the epochs and equinoxes to a form which can be used in
*     computations.
*     Expand the identifiers for the list of columns to be copied to
*     replace any vector identifiers with a set of identifiers for
*     every element in the vector.
*     Determine the number of rows in the input catalogue.
*     If all is ok then
*       For every row in the input catalogue.
*         Read the next row from the input catalogue.
*         Compute the new coordinates.
*         Write the new coordinates to the catalogue.
*         Copy the other fields from the input catalogue.
*         Append the current row to the output catalogue.
*       end for
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     29/5/97 (ACD): Original version.
*     20/5/98 (ACD): Modified for additional types of coordinates.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT,
     :  CRDTYP,
     :  MCRDCL,
     :  CRDI(MCRDCL),
     :  CRDOI(2),
     :  NUMCOL,
     :  FIIN(NUMCOL),
     :  FIOUT(NUMCOL)
      LOGICAL
     :  FULL
      CHARACTER
     :  EPOCHI*(*),
     :  EQUINI*(*),
     :  EPOCHO*(*),
     :  EQUINO*(*)
      DOUBLE PRECISION
     :  MO(35)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      CHARACTER
     :  EQINT*1,   ! Time system for input equinox.
     :  EPINT*1,   ! Time system for input epoch.
     :  EQOUTT*1,  ! Time system for output equinox.
     :  EPOUTT*1   ! Time system of output epoch.
      DOUBLE PRECISION
     :  EQIN,    ! Input equinox.
     :  EPIN,    ! Input epoch.
     :  EQOUT,   ! Output equinox.
     :  EPOUT,   ! Output epoch.
     :  NWLNG,   ! New Right Ascension or Galactic longitude.
     :  NWLAT    ! New Declination or Galactic latitude.
      LOGICAL
     :  NWLNGN,  ! Null value flag for NWLNG.
     :  NWLATN   !  "     "    "    "  NWLAT.
      INTEGER
     :  NUMID,   ! Total number of identifiers (inc. vector elements).
     :  FIINE(CAT__MXCOL),  ! List of input  identifiers (inc. vectors).
     :  FIOUTE(CAT__MXCOL), !  "   "  output      "      ( " .    "   ).
     :  FDTYPE(CAT__MXCOL), ! Data types corresponding to identifiers.
     :  ROWS,    ! Number of rows in the input catalogue.
     :  ROW      ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Convert the epochs and equinoxes to a form which can be used in
*       computations.

         CALL CAP_DCEQP (EPOCHI, EPINT, EPIN, STATUS)
         CALL CAP_DCEQP (EQUINI, EQINT, EQIN, STATUS)
         CALL CAP_DCEQP (EPOCHO, EPOUTT, EPOUT, STATUS)
         CALL CAP_DCEQP (EQUINO, EQOUTT, EQOUT, STATUS)

*
*       Expand the list of identifiers to be copied, replacing vector
*       identifiers with a set of identifiers corresponding to all the
*       vector elements.

         CALL CAP_EXPCL (NUMCOL, FIIN, FIOUT, CAT__MXCOL, NUMID,
     :     FIINE, FIOUTE, FDTYPE, STATUS)

*
*       Determine the number of rows in the input catalogue.

         CALL CAT_TROWS (CIIN, ROWS, STATUS)

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Copy all the rows, calculating new coordinates for each row.

            DO ROW = 1, ROWS

*
*             Read the next row from the input catalogue.

               CALL CAT_RGET (CIIN, ROW, STATUS)

*
*             Compute the new coordinates.

               CALL CAP_CLCRD (FULL, CRDTYP, MCRDCL, CRDI,
     :           EQINT, EQIN, EPINT, EPIN, EQOUTT, EQOUT, EPOUTT, EPOUT,
     :           MO, NWLNG, NWLNGN, NWLAT, NWLATN, STATUS)

*
*             Write the new coordinates to the catalogue.

               CALL CAT_PUT0D (CRDOI(1), NWLNG, NWLNGN, STATUS)
               CALL CAT_PUT0D (CRDOI(2), NWLAT, NWLATN, STATUS)

*
*             Copy the other fields from the input catalogue.

               CALL CAP_CPFLD (NUMID, FIINE, FIOUTE, FDTYPE, STATUS)

*
*             Append the current row to the output catalogue.

               CALL CAT_RAPND (CIOUT, STATUS)
            END DO
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_CAP_CPCRD_ERR', 'CAP_CPCRD: Error '/
     :        /'calculating new coordinates and copying table.',
     :        STATUS)
         END IF

      END IF

      END
