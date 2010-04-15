/*
;+
;	B S W A P
;
;	SUBROUTINE BSWAP (ARRAY,WORDS)
;
;	Exchanges the byte order in an array of 16 bit words
;       so that it matches the convention used by non-DEC
;	machines.  This is based on an MTPCKG routine written
;	by Bill Sebok.
;
;	Parameters -    (">" input, "!" modified, "<" output)
;
;	(!) ARRAY    (Integer*2 array ARRAY(WORDS)) The array
;		     to be swapped.
;	(>) WORDS    (Integer) The number of words in ARRAY
;
;	Subroutines / functions used - None
;
;	Modified:
;
;	30th April 1986.  KS / AAO.  Psect name changed.
;	21st Nov   1988.  CKL / CIT. Converted to C for SUN.
;        8th Apr   1990.  SNS / CIT. NULL version added for VMS.
;        5th Feb   1993.  BDC / UNSW. DECstation-specific version.
;	 4th Mar   1993.  KS / AAO.  Now compiles for any machine, using
:                         the 'byteswap' flag.
;        3th May   1999.  RPT / JAC  Rip-off Figaro GEN_BSWAP: strip
;                         preprocessor code since SPECX allows optional
;                         swapping
;+
*/

void bswap_(array,words)
unsigned short int array[];
int *words;
{

   /* #ifdef byteswap */

   int i;

   for(i = 0; i < *words; i++)
      array[i] = (array[i] << 8) | (array[i] >> 8);

   /* #endif */

}
