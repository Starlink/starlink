#include <fcntl.h>
#include <unistd.h>
#include "dvi2bitmap.h"
#include "InputByteStream.h"

InputByteStream::InputByteStream (string s, int buflen=256)
    : buflen_(buflen), eof_(false)
{
    buf_ = new IBSByte[buflen];
    fd_ = open (s.c_str(), O_RDONLY);
    if (fd_ >= 0)
    {
	bufcontents_ = read (fd_, buf_, buflen_);
	p_ = buf_;
	eof_ = (bufcontents_ == 0);
    }
    else
	eof_ = true;
}

InputByteStream::~InputByteStream ()
{
    if (fd_ >= 0)
	close (fd_);
    delete[] buf_;
}

unsigned char InputByteStream::getByte()
{
    if (eof_)
	return 0;
    if (p_ == buf_ + bufcontents_)
    {
	bufcontents_ = read (fd_, buf_, buflen_);
	p_ = buf_;
	eof_ = (bufcontents_ == 0);
    }
    return eof_ ? 0 : *p_++;
}

bool InputByteStream::eof()
{
    return eof_;
}


/*
#if (sizeof(unsigned int) != 4)
// The code here is intended to deal with the case where (un)signed
// ints are 4 bytes long.  It's actually simpler on machines where
// ints are longer, because we wouldn't have to do the two-stage
// subtraction (below) when n==4.  I can't test this, however, so
// simply cause an error here.
#error "InputByteStream.cc assumes sizeof(unsigned int)==4"
#endif
*/

// Assumes: DVI file is big-endian - ie, MSB first (I can't find this
// explicitly stated in the spec); DVI integers are 2-complement
// (explicit in the spec).
// I _believe_ that the following code is independent of the host
// machine's bytesex and integer representation (2's-complement or
// not), but....
signed int InputByteStream::getSIS(int n)
{
    if (n<0 || n>4)
	throw DviError("bad argument to getSIS", true);
    unsigned int t = getByte();
    unsigned int pow2 = 128;	// 2^7-1   is largest one-byte signed int
				// 2^7=128 is most negative signed int
				// 2^8-1   is -1
    for (int i=n-1; i>0; i--)
    {
	pow2 *= 256;
	t *= 256;
	t += getByte();
    }
    signed int result;
    if (t < pow2)
	result = t;
    else
	// pow2 <= t < 2*pow2
	// t is actually a negative number - subtract off 2*pow2
	if (n < 4)
	{
	    // t and 2*pow2 will both fit into a signed int
	    // so explicitly convert them to that _first_.
	    // I'm not sure if the result of subtracting an unsigned from 
	    // a smaller unsigned (ie, whether it's a signed negative number,
	    // or, say, zero) is implementation-dependent or not.
	    result = static_cast<signed int>(t)
		- static_cast<signed int>(2*pow2);
	    //result -= 2*pow2;
	}
	else
	{
	    // n==4: t won't fit into a signed int, and 2*pow2 would
	    // overflow, so do the subtraction in two stages.  A
	    // careless optimiser could mess this up.
	    t -= pow2;		// now it'll fit
	    // 0 <= t < pow2
	    result = pow2 - t;	// a positive result in a signed variable
	    result = -result;
	}
    return result;
}

signed int InputByteStream::getSIU(int n)
{
    // disallow n==4 - there are no unsigned 4-byte quantities in the DVI file
    if (n<0 || n>3)
	throw DviError("bad argument to getSIU", true);
    unsigned int t = 0;
    for (; n>0; n--)
    {
	t *= 256;
	t += getByte();
    }
    return static_cast<signed int>(t);
}

unsigned int InputByteStream::getUIU(int n)
{
    if (n<0 || n>4)
	throw DviError("bad argument to getUIU", true);
    unsigned int t = 0;
    for (; n>0; n--)
    {
	t *= 256;
	t += getByte();
    }
    return t;
}

