#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

/* Read one character from terminal, interpreting VT100/VT200
   escape sequences. The program reads from standard input. */

/* To put the terminal into 'keypad application mode' send ESC =;
   to reset, send ESC > */

/* DEC keyboards generate the following escape sequences.
   CSI is either the single character 0x9B or the two
   characters ESC (0x1B) [ (0x5B). SS3 is the character 0x8F or
   the two characters ESC (0x1B) O (0x4F).

   Key		Code generated		Value returned by GRGETC
   Up arrow	CSI A, SS3 A 			-1
   Down arrow	CSI B, SS3 B			-2
   Right arrow	CSI C, SS3 C			-3
   Left arrow	CSI D, SS3 D			-4
   Keypad 0	SS3 p				-20
          1	SS3 q				-21
          2	SS3 r				-22
          3	SS3 s				-23
	  4	SS3 t				-24
	  5	SS3 u				-25
	  6	SS3 v				-26
	  7	SS3 w				-27
	  8	SS3 x				-28
	  9	SS3 y				-29
	  -	SS3 m				-17
	  ,	SS3 l				-16
	  .	SS3 n				-18
   Enter	SS3 M				-8
   PF1          SS3 P				-11
   PF2          SS3 Q				-12
   PF3		SS3 R				-13
   PF4          SS3 S				-14
   The following are not implemented yet:
   Find		CSI 1 ~
   Insert here	CSI 2 ~
   Remove	CSI 3 ~
   Select	CSI 4 ~
   Prev Screen	CSI 5 ~
   Next Screen	CSI 6 ~
   F6		CSI 1 7 ~
   F7		CSI 1 8 ~
   F8		CSI 1 9 ~
   F9		CSI 2 0 ~
   F10		CSI 2 1 ~
   F11		CSI 2 3 ~
   F12		CSI 2 4 ~
   F13		CSI 2 5 ~
   F14		CSI 2 6 ~
   Help		CSI 2 8 ~
   Do		CSI 2 9 ~
   F17		CSI 3 1 ~
   F18		CSI 3 2 ~
   F19		CSI 3 3 ~
   F20		CSI 3 4 ~

   */

#include <stdio.h>
#include <termios.h>
#define CSI (0x9B)
#define SS3 (0x8F)
#define ESC (0x1B)

#ifdef PG_PPU
#define GRGETC grgetc_
#else
#define GRGETC grgetc
#endif

void GRGETC(val)
int *val;
{
    static char valid_table[] = {
		'A','B','C','D', 'P','Q','R','S',
		'p','q','r','s','t','u','v','w','x','y',
		'm','l','n', 'M' };
    static short code_table[] = {
		-1,-2,-3,-4, -11,-12,-13,-14,
		-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,
		-17,-16,-18, -8 };
    static struct termios term, saveterm;
    int i;
    int nextch;
    static int init=1;
    static int raw=0;

    if (init) {
	putchar(ESC);
	putchar('=');
 	init = 0;
    }
    if (raw == 0) {
	tcgetattr(0, &term);
	saveterm = term;
	term.c_lflag &= ~( ICANON );
	term.c_cc[VMIN] = 1;
	tcsetattr(0, TCSADRAIN, &term);
	raw = 1;
    }
    tcflush(0, TCIOFLUSH);
    nextch = getchar();
    if (nextch == ESC) {
	nextch = getchar();
	if (nextch == '[') nextch = CSI;
	if (nextch == 'O') nextch = SS3;
    }
    if (nextch == CSI || nextch == SS3) {
	nextch = getchar();
        for (i=0; i<22; i++)
	    if (valid_table[i] == nextch) {
		nextch = code_table[i];
		break;
	    }
    }
    *val = nextch;
    /* If a special character was received, stay in CBREAK mode; this
       is OK for PGPLOT cursor control, but may not be for other
       applications */
    if (nextch >= 0) {
      	tcsetattr(0, TCSADRAIN, &saveterm);
	raw = 0;
    }
    return;
}
