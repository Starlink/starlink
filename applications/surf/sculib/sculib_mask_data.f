      SUBROUTINE SCULIB_MASK_DATA (USE_SECT, TYPE, N_SPEC, DATA_SPEC,
     :     DEMOD_POINTER, N_SWITCHES ,N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, N_POS, N_BOLS, N_BEAM, SWITCH_EXPECTED,
     :     VALUE, BVALUE, BITNUM, BIT_STATE, DATA_PTR,
     :     STATUS)
*+
*  Name:
*     SCULIB_MASK_DATA

*  Purpose:
*     Set a data array from a SCUBA section

*  Invocation:
*     CALL SCULIB_MASK_DATA( STATUS )

*  Description
*     From an array of SCUBA sections, this routine creates a mask
*     which is then used to modify the given data array

*  Arguments:
*     USE_SECT   = LOGICAL (Given)
*       Are we using the section (TRUE) or inverse (FALSE)
*     TYPE = CHAR (Given)
*       Type of data. Allowed values are:
*         REAL - data is real array
*         BYTE - data is byte array
*         BIT  - affect single bit of byte array
*     N_SPEC                           = INTEGER (Given)
*       Number of specifications supplied in SPEC
*     SPEC( N_SPEC )                   = CHARACTER*(*) (Given)
*       the specification to be decoded
*     DEMOD_POINTER (N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,  N_MEASUREMENTS)
*     = INTEGER (Given)
*       the pointer to the location in the main data array of the data
*       for each switch of the observation
*     N_SWITCHES                       = INTEGER (Given)
*       the number of switches per exposure
*     N_EXPOSURES                      = INTEGER (Given)
*       the number of exposures per integration
*     N_INTEGRATIONS                   = INTEGER (Given)
*       the number of integrations per measurement
*     N_MEASUREMENTS                   = INTEGER (Given)
*       the number of measurements in the observation
*     N_POS                            = INTEGER (Given)
*       the number of positions measured in the observation
*     N_BOLS                           = INTEGER (Given)
*       the number of bolometers measured in the observation
*     N_BEAM = INTEGER (Given)
*       Number of beams in DATA_PTR
*     SWITCH_EXPECTED                  = LOGICAL (Given)
*       .TRUE. if a switch component is allowed in the data-spec
*     VALUE   = REAL (Given)
*       If TYPE is 'REAL' then use this value for masked data
*     BVALUE   = BYTE (Given)
*       If TYPE is 'BYTE' then use this value for masked data
*     BITNUM   = INTEGER (Given)
*       If TYPE is 'BIT' then affect this bit position
*     BIT_STATE = LOGICAL (Given)
*       If TYPE is 'BIT' then clear the bit (FALSE) or set it (TRUE)
*     DATA_PTR = INTEGER (Given & Returned)
*       Pointer to data array that is to be masked
*     STATUS                           = INTEGER (Given and Returned)
*       The global status.


*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)
*     John Lightfoot (jfl@roe.ac.uk)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.6  2004/09/01 00:41:11  timj
*     use CNF_PVAL
*
*     Revision 1.5  1999/08/03 19:35:13  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1999/07/29 23:44:26  timj
*     Header tidy up.
*
*     Revision 1.3  1997/07/17 02:16:36  timj
*     Failed to allocate enough memory for multiple beam (PHOTOM) observations
*     in the mask array.
*     SCULIB_SET_QUAL now requires a logical for bit_value.
*
*     Revision 1.2  1997/06/09 21:43:06  timj
*     Initialise BYTE_PTR and check status before running SCULIB_CFILLB
*
*     Revision 1.1  1997/05/30 02:49:31  timj
*     Initial revision
*
*     {end history}

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! for VAL__BADI
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL       USE_SECT
      CHARACTER*(*) TYPE
      INTEGER       N_SPEC
      CHARACTER*(*) DATA_SPEC(N_SPEC)
      INTEGER       N_SWITCHES
      INTEGER       N_EXPOSURES
      INTEGER       N_INTEGRATIONS
      INTEGER       N_MEASUREMENTS
      INTEGER       N_BEAM
      INTEGER       DEMOD_POINTER (N_SWITCHES, N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS)
      INTEGER       N_POS
      INTEGER       N_BOLS
      LOGICAL       SWITCH_EXPECTED
      REAL          VALUE
      BYTE          BVALUE
      LOGICAL       BIT_STATE
      INTEGER       BITNUM

*  Arguments Given & Returned:
      INTEGER       DATA_PTR

*  Status:
      INTEGER STATUS

*  External routines:

*  Local Constants:

*  Local Variables:
      LOGICAL BIT_SWITCH     ! .TRUE. to call SCULIB_BITON .FALSE. for BITOFF
      INTEGER BOL_S_PTR      ! Selected bolometer
      INTEGER BOL_S_END      ! End of selected bolometer
      INTEGER BYTE_END       ! end of byte mask
      INTEGER BYTE_PTR       ! Byte mask
      BYTE    BTEMP          ! Temporary byte
      INTEGER EXP_S_END      ! end Selected exposures
      INTEGER EXP_S_PTR      ! Selected exposures
      INTEGER I              ! Loop counter
      INTEGER INT_S_END      ! Selected integrations
      INTEGER INT_S_PTR      ! Selected integrations
      INTEGER MASK_BIT       ! Bit number used in mask
      INTEGER MEAS_S_END     ! End selected measurements
      INTEGER MEAS_S_PTR     ! Selected measurements
      LOGICAL POS_SELECTED   ! Was POS selected?
      INTEGER POS_S_PTR      ! Selected positions
      INTEGER POS_S_END      ! End of selected positions
      INTEGER SW_S_END       ! end selected switches
      INTEGER SW_S_PTR       ! Selected switches

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Create the work space arrays
*     I am not going to hardwire the sizes of each array into the code
*     These _S arrays contain a 1 for each switch/bol/etc selected
*     and 0 otherwise.

      POS_S_PTR = 0
      POS_S_END = 0
      EXP_S_END = 0
      EXP_S_PTR = 0
      INT_S_END = 0
      INT_S_PTR = 0
      SW_S_END  = 0
      SW_S_PTR  = 0
      MEAS_S_END= 0
      MEAS_S_PTR= 0
      BOL_S_PTR = 0
      BOL_S_END = 0
      BYTE_PTR  = 0
      BYTE_END  = 0

      CALL SCULIB_MALLOC (N_POS * VAL__NBI, POS_S_PTR, POS_S_END,
     :     STATUS)
      CALL SCULIB_MALLOC (N_BOLS * VAL__NBI, BOL_S_PTR, BOL_S_END,
     :     STATUS)
      CALL SCULIB_MALLOC (N_SWITCHES * VAL__NBI, SW_S_PTR, SW_S_END,
     :     STATUS)
      CALL SCULIB_MALLOC (N_EXPOSURES * VAL__NBI, EXP_S_PTR, EXP_S_END,
     :     STATUS)
      CALL SCULIB_MALLOC (N_INTEGRATIONS * VAL__NBI, INT_S_PTR,
     :     INT_S_END, STATUS)
      CALL SCULIB_MALLOC (N_MEASUREMENTS * VAL__NBI,
     :     MEAS_S_PTR, MEAS_S_END, STATUS)

*     Setup a mask array. This is necessary for multiple specs
*     when inverting the section mask

      CALL SCULIB_MALLOC(N_POS * N_BOLS * N_BEAM * VAL__NBUB, BYTE_PTR,
     :     BYTE_END, STATUS)

*     Now fill this with zeroes. Work out the actual mask first
*     (ie set to one) before masking the QUALITY array itself.

      BTEMP = 0
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLB(N_POS * N_BOLS * N_BEAM, BTEMP,
     :        %VAL(CNF_PVAL(BYTE_PTR)))
      END IF

*     I am setting bit 0 in the mask array to ON

      BIT_SWITCH = .TRUE.
      MASK_BIT = 0

*     decode each data specification in turn


      DO I = 1, N_SPEC

         CALL SCULIB_DECODE_SPEC (DATA_SPEC(I), DEMOD_POINTER,
     :        N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :        N_POS, N_BOLS, SWITCH_EXPECTED, POS_SELECTED,
     :        %VAL(CNF_PVAL(POS_S_PTR)), %VAL(CNF_PVAL(SW_S_PTR)),
     :        %VAL(CNF_PVAL(EXP_S_PTR)),
     :        %VAL(CNF_PVAL(INT_S_PTR)), %VAL(CNF_PVAL(MEAS_S_PTR)),
     :        %VAL(CNF_PVAL(BOL_S_PTR)),
     :        STATUS)

         CALL SCULIB_SET_QUAL (.TRUE., %VAL(CNF_PVAL(BYTE_PTR)), N_BOLS,
     :        N_POS, N_BEAM, %VAL(CNF_PVAL(BOL_S_PTR)),
     :        %VAL(CNF_PVAL(POS_S_PTR)),
     :        MASK_BIT, BIT_SWITCH, STATUS)

      END DO

*     Now we have our mask array where a 1 indicates that
*     the point is in the required section.
*     We need to modify our data

      IF (TYPE .EQ. 'BYTE') THEN

         CALL SCULIB_SET_DATA_UB (USE_SECT,
     :        N_BOLS, N_POS, N_BEAM, %VAL(CNF_PVAL(BYTE_PTR)),
     :        BVALUE, %val(cnf_pval(DATA_PTR)), STATUS)

      ELSE IF (TYPE .EQ. 'REAL') THEN

         CALL SCULIB_SET_DATA (USE_SECT, N_BOLS, N_POS,
     :        N_BEAM, %VAL(CNF_PVAL(BYTE_PTR)),
     :        VALUE, %val(cnf_pval(DATA_PTR)), STATUS)

      ELSE IF (TYPE .EQ. 'BIT') THEN

         CALL SCULIB_SET_DATA_BIT(USE_SECT, N_BOLS, N_POS,
     :        N_BEAM, %VAL(CNF_PVAL(BYTE_PTR)),
     :        BITNUM, BIT_STATE, %val(cnf_pval(DATA_PTR)), STATUS)

      END IF

*     Free memory

      CALL SCULIB_FREE ('POS_S', POS_S_PTR, POS_S_END, STATUS)
      CALL SCULIB_FREE ('BOL_S', BOL_S_PTR, BOL_S_END, STATUS)
      CALL SCULIB_FREE ('SW_S',  SW_S_PTR,  SW_S_END,  STATUS)
      CALL SCULIB_FREE ('EXP_S', EXP_S_PTR, EXP_S_END, STATUS)
      CALL SCULIB_FREE ('INT_S', INT_S_PTR, INT_S_END, STATUS)
      CALL SCULIB_FREE ('MEAS_S',MEAS_S_PTR,MEAS_S_END, STATUS)
      CALL SCULIB_FREE ('BYTES', BYTE_PTR, BYTE_END, STATUS)


      END
