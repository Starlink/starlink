#!/bin/sh

#+
#  Name:
#     extreme_test.sh

#  Purpose:
#     Test script for EXTREME package.

#  Description:
#     Runs through some of the programs which form the EXTREME package.

#  Authors:
#     MBT: Mark Taylor (Starlink)

#  History:
#     27-JUL-2004 (MBT):
#        Initial version, adapted from classic makefile test target.
#-

#  The script is a single statement to ensure that it returns a non-zero
#  status code if any step fails (this is what signals a failure to
#  automake).
   echo "inscnf test:" && \
   echo "------------" && \
   printf "%s\n%s\n%s\n" \
          "      INCLUDE 'SAE_PAR'          ! Standard SAE constants" \
          "" \
          "      CALL EXT_SUB( %VAL( IP1 ), %VAL( IP2 ), STATUS )" \
      | ./inscnf && \
   \
   echo "" && \
   echo "crepint test:" && \
   echo "-------------" && \
   printf "%s\n%s\n%s\n%s\n%s\n" \
          '      int i;              /* int variable    */' \
          '      short int j;        /* short variable  */' \
          '      char *str = "int";  /* string variable */' \
          '' \
          '      printf( "%s variable value: %d\\n", str, i );' \
      | ./crepint && \
   \
   echo "" && \
   echo "frepint test:" && \
   echo "-------------" && \
   printf "%s\n%s\n%s\n%s\n%s\n%s\n" \
          "      INCLUDE 'SAE_PAR'         ! Standard SAE constants" \
          "" \
          "      REAL XSIZE                ! Horizontal size" \
          "      INTEGER NPIX              ! Number of pixels" \
          "      INTEGER I1, I2, I3, I4, I5, I6, I7, I8, I10, I11, I12, I13, I14" \
          "      I1 = EXT_ADD( I2, 4, STATUS )" \
      | ./frepint && \
   echo ""

