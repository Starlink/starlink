 /*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: base64.h,v 1.1.1.1 2002/04/04 20:12:04 brighton Exp $
 *
 * base64.h - declarations for base64 encoding and decoding
 *            utility routines.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  Jul 15    Created
 */

char *encode_base64(char *input);
char *decode_base64(char *input);
