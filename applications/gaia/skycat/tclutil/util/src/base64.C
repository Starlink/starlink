 /*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: base64.C,v 1.1 1998/07/16 16:28:00 abrighto Exp $
 *
 * base64.C - utility routines for base64 encoding and decoding
 *            (needed for some HTTP protocols, such as password
 *            encoding).
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  Jul 15    Created, based on modified versions of 
 *                           mozilla routines in libi18n/mime2fun.c
 *                           See http://www.mozilla.org/NPL/.
 */
static const char* const rcsId="@(#) $Id: base64.C,v 1.1 1998/07/16 16:28:00 abrighto Exp $";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include "base64.h"

static char basis_64[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int encode_base64_ (const char *in, char *out)
{
    unsigned char c1, c2, c3;
    c1 = in[0];
    c2 = in[1];
    c3 = in[2];

    *out++ = basis_64[c1>>2];
    *out++ = basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)];

    *out++ = basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)];
    *out++ = basis_64[c3 & 0x3F];
    return 1;
}

char *encode_base64(char *input)
{
    char *output = 0;
    char *pSrc, *pDest ;
    int i ;
    size_t size = strlen(input);

    output = (char *)malloc(size * 4 / 3 + 4);
    if (output == NULL)
	return NULL;

    pSrc = input;
    pDest = output ;
    for (i = size; i >= 3; i -= 3) {
	if (encode_base64_(pSrc, pDest) == 0) { /* error */
	    pSrc += 3;
	    pDest += 3;
	}
	else {
	    pSrc += 3;
	    pDest += 4;
	}
    }
    /* in case (i % 3 ) == 1 or 2 */
    if(i > 0) {
	char in[3];
	int j;
	in[0] = in[1] = in[2] ='\0';
	for(j=0;j<i;j++)
	    in[j] = *pSrc++;
	encode_base64_(in, pDest);
	for(j=i+1;j<4;j++)
	    pDest[j] = '=';
	pDest += 4;
    }
    *pDest = '\0';
    return output;
}


static int decode_base64_ (const char *in, char *out)
{
    /* reads 4, writes 3. */
    int j;
    unsigned long num = 0;

    for (j = 0; j < 4; j++) {
	unsigned char c;
	if (in[j] >= 'A' && in[j] <= 'Z')		 c = in[j] - 'A';
	else if (in[j] >= 'a' && in[j] <= 'z') c = in[j] - ('a' - 26);
	else if (in[j] >= '0' && in[j] <= '9') c = in[j] - ('0' - 52);
	else if (in[j] == '+')				 c = 62;
	else if (in[j] == '/')				 c = 63;
	else if (in[j] == '=')				 c = 0;
	else {
	    /*	abort ();	*/
	    strcpy(out, in);	/* I hate abort */
	    return 0;
	}
	num = (num << 6) | c;
    }

    *out++ = (unsigned char) (num >> 16);
    *out++ = (unsigned char) ((num >> 8) & 0xFF);
    *out++ = (unsigned char) (num & 0xFF);
    return 1;
}

char *decode_base64(char *input)
{
    char *output = strdup(input);;
    char *pSrc = input, *pDest = output;
    int i ;

    for (i = strlen(input); i > 3; i -= 4) {
	if (decode_base64_(pSrc, pDest) == 0) {
	    pSrc += 4;
	    pDest += 4;
	}
	else {
	    pSrc += 4;
	    pDest += 3;
	}
    }
	
    *pDest = '\0';
    return output;
}

#if TEST_BASE64  /* for testing */
main()
{
    char* input = "Aladdin:open sesame";
    char* base64 = "QWxhZGRpbjpvcGVuIHNlc2FtZQ==";

    char* s = encode_base64(input);
    char* output = decode_base64(s);

    printf("base64 encoded value of %s is %s (should be %s)\n", 
	   input, s, base64);

    printf("decoded value of input %s is %s\n", s, output);

    printf("decoded value of target %s is %s\n", base64,
	   decode_base64(base64));

    if (strcmp(input, output) == 0) 
	printf("TEST PASSED\n");
    else
	printf("TEST FAILED\n");

    exit(0);
}
#endif


