/* copyright (c) Mark M Martin. RAL. 1988 */
/*
 * native bitmap layout. dont typedef, cos get decl problems in dd.h
 */
struct layout{
	int	l_xoffset;	/* in bits */
	int	l_yoffset;	/* in scanlines */
	int	l_bytestride;	/* in bytes */
	int	l_width;	/* in bits */
	int	l_height;	/* in lines */
	unsigned char	*l_base;	/* memory (+x and y offsets) */
};
#define BITREV(ch)	(ch)
#define DEREF(ptr)	(*(ptr))
#define GETBYTE(ptr)	BITREV(DEREF(ptr))
