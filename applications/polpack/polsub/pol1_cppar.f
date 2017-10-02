      SUBROUTINE POL1_CPPAR( CIIN, CIOUT, REPORT, STATUS )
*+
*  Name:
*     POL1_CPPAR

*  Purpose:
*     Ensure the output cat. parameters corresponding to input cat. ones.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPPAR( CIIN, CIOUT, REPORT, STATUS )

*  Description:
*     Ensure parameters in the output catalogue correspond to those
*     in the input catalogue.

*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     REPORT =  LOGICAL (Given)
*        If .TRUE., then the required parameters should already exist in the
*        output. In which case, report an error if there are any differences
*        between the parameters in the input and ouput catalogues. If
*        .FALSE. we know that there are no parameters in the supplied output
*        catalogue, and this routine will therefore create them.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version, based on cap_cppar.f by ACD.
*     27-SEP-2017 (DSB):
*        Modified to allow new parameters to be added into a pre-existing
*        output catalogue that may already contain parameters of the same
*        name.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing



*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.

*  External References:
      INTEGER CHR_LEN

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT
      LOGICAL REPORT

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL MORE         ! Flag: more parameters to access?
      INTEGER QCOUNT       ! Number of the current parameter.
      INTEGER QIINC        ! Identifier for the current input  parameter.
      INTEGER QIOUTC       !     "       "   "     "    output   "   .

*    The following variables represent the attributes of the current
*    parameter.
      INTEGER
     :  BUFLEN,      ! Length of BUFFER (excl. trail. blanks).
     :  LQNAME,      !   "    "  QNAME  ( "  .   "  .   "   ).
     :  OQI,         ! Parameter identifier in the output catalogue.
     :  OSTAT,       ! Status checking whether parameter in output cat.
     :  PCI,         ! Parent catalogue.
     :  PCSIZE,      ! Size if a character string.
     :  PDIMS,       ! Dimensionality.
     :  PDTYPE,      ! Data type.
     :  PSIZEA(10),  ! Size of each array dimension.
     :  QCI,         ! Parent catalogue.
     :  QCSIZE,      ! Size if a character string.
     :  QDIMS,       ! Dimensionality.
     :  QDTYPE,      ! Data type.
     :  QSIZEA(10)   ! Size of each array dimension.

      CHARACTER
     :  BUFFER*75,             ! Output buffer.
     :  PCOMM*(CAT__SZCOM),    ! Comments.
     :  PNAME*(CAT__SZCMP),    ! Name.
     :  PUNITS*(CAT__SZUNI),   ! Units.
     :  PVALUE*(CAT__SZVAL),   ! Value.
     :  PXTFMT*(CAT__SZEXF),   ! External format.
     :  QCOMM*(CAT__SZCOM),    ! Comments.
     :  QNAME*(CAT__SZCMP),    ! Name.
     :  QUNITS*(CAT__SZUNI),   ! Units.
     :  QVALUE*(CAT__SZVAL),   ! Value.
     :  QXTFMT*(CAT__SZEXF)    ! External format.

      LOGICAL
     :  QPRFDS,       ! Preferential display flag.
     :  PPRFDS,       ! Preferential display flag.
     :  THERE         ! Does parameter exist?

      DOUBLE PRECISION
     :  QDATE,       ! Modification date.
     :  PDATE        ! Modification date.
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Copy each of the parameters in the input catalogue.
      MORE = .TRUE.
      QCOUNT = 0

      DO WHILE( MORE )

*  Attempt to obtain an identifier for the next parameter in the input
*  catalogue, and proceed if ok.
         QCOUNT = QCOUNT + 1

         CALL CAT_TNDNT( CIIN, CAT__QITYP, QCOUNT, QIINC, STATUS )
         IF( STATUS .EQ. CAT__OK  .AND.  QIINC .NE. CAT__NOID ) THEN

*  Inquire the values of all the attributes for this parameter.
            CALL CAT_PINQ( QIINC, 10, QCI, QNAME, QDTYPE, QCSIZE,
     :                     QDIMS, QSIZEA, QUNITS, QXTFMT, QPRFDS,
     :                     QCOMM, QVALUE, QDATE, STATUS )

*  Determine whether the output catalogue already contains a parameter
*  (or column) of the given name.  ( Note: CAT_TIDNT returns a bad status
*  if the output catalogue does not contain the parameter).
            OSTAT = SAI__OK
            CALL CAT_TIDNT( CIOUT, QNAME, OQI, OSTAT )
            IF( OSTAT .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( OSTAT )
               THERE = .FALSE.
            ELSE
               THERE = .TRUE.
            END IF

*  If the parameter already exists in the output catalogue, check its
*  attributes are correct. Report an error if not.
            IF( THERE ) THEN
               CALL CAT_PINQ( OQI, 10, PCI, PNAME, PDTYPE, PCSIZE,
     :                        PDIMS, PSIZEA, PUNITS, PXTFMT, PPRFDS,
     :                        PCOMM, PVALUE, PDATE, STATUS )

               IF( PCI .NE. QCI .OR.
     :             PNAME .NE. QNAME .OR.
     :             PDTYPE .NE. QDTYPE .OR.
     :             PCSIZE .NE. QCSIZE .OR.
     :             PDIMS .NE. QDIMS .OR.
     :             PSIZEA(1) .NE. QSIZEA(1) .OR.
     :             PUNITS .NE. QUNITS .OR.
     :             PXTFMT .NE. QXTFMT .OR.
     :             PPRFDS .NEQV. QPRFDS .OR.
     :             PCOMM .NE. QCOMM .OR.
     :             PVALUE .NE. QVALUE .OR.
     :             PDATE .NE. QDATE ) THEN

                  IF( STATUS .EQ. SAI__OK ) THEN
                     CALL MSG_SETC( 'P', PNAME )

                     IF( PNAME .NE. QNAME )  THEN
                        CALL MSG_SETC( 'P', '/' )
                        CALL MSG_SETC( 'P', QNAME )
                     END IF

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'POL1_CPPAR: Existing '//
     :                             'parameter "^P" has unexpected '//
     :                             'attribute values (possible '//
     :                             'programming error).', STATUS )
                  END IF
               END IF

*  If the parameter does not exist, report an error if it should exist.
            ELSE IF( REPORT ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'C', QNAME )
                  CALL ERR_REP( ' ', 'POL1_CPPAR: Parameter "^C" '//
     :                          'not found in supplied output '//
     :                          'catalogue, and REPORT is TRUE '//
     :                          '(possible programming error).',
     :                            STATUS )
               END IF

*  Otherwise, attempt to create a corresponding parameter in the output
*  catalogue.
            ELSE
               CALL CAT_PNEW0( CIOUT, CAT__QITYP, QNAME, QDTYPE,
     :                         QIOUTC, STATUS )

*  Set the attributes of this parameter to correspond to the input
*  parameter.  Note that only those attributes which can vary in a SCAR/ADC
*  catalogue are set.
               CALL CAT_TATTI( QIOUTC, 'CSIZE', QCSIZE, STATUS )
               CALL CAT_TATTC( QIOUTC, 'UNITS', QUNITS, STATUS )
               CALL CAT_TATTC( QIOUTC, 'EXFMT', QXTFMT, STATUS )
               CALL CAT_TATTC( QIOUTC, 'COMM', QCOMM, STATUS )
               CALL CAT_TATTC( QIOUTC, 'VALUE', QVALUE, STATUS )

            END IF

*  If an error occurred accessing the parametwr wuthin the input
*  catalogue, set the termination criterion.
         ELSE
            MORE = .FALSE.
         END IF

*  Set the termination flag if any error has occurred.
         IF( STATUS .NE. SAI__OK ) MORE = .FALSE.

      END DO

      END


