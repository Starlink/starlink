// part of dvi2bitmap
// $Id$

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <cstdio>
#include "dvi2bitmap.h"
#include "InputByteStream.h"

// Open the requested file.  If preload is true, then open the file and
// read it entire into memory, since the client will be seeking a lot.
InputByteStream::InputByteStream (string s, bool preload=false)
    : preloaded_(preload)
{
    fd_ = open (s.c_str(), O_RDONLY);
    eof_ = (fd_ < 0);		// set true if open failed

    if (!eof_)
    {
	if (preload)
	{
	    struct stat S;
	    if (fstat (fd_, &S))
		throw DviError ("Can't stat open file");
	    buflen_ = S.st_size;
	    buf_ = new Byte[buflen_];
	    int bufcontents = read (fd_, buf_, buflen_);
	    if (bufcontents != buflen_)
		throw DviError ("Couldn't preload file");
	    eob_ = buf_ + bufcontents;
	    cerr << "stat.st_size="<<buflen_
		 <<" bufcontents="<<bufcontents<<'\n';

	    close (fd_);
	    fd_ = -1;
	}
	else
	{
	    buflen_ = 512;
	    buf_ = new Byte[buflen_];
	    eob_ = buf_;	// nothing read in yet - eob at start
	}
	p_ = buf_;
    }
}

InputByteStream::~InputByteStream ()
{
    if (fd_ >= 0)
	close (fd_);
    delete[] buf_;
}

Byte InputByteStream::getByte()
{
    if (eof_)
	return 0;

    if (p_ < buf_)
	throw DviBug ("InputByteStream: pointer before buffer start");
    if (p_ > eob_)
	throw DviBug ("InputByteStream: pointer beyond EOF");

    if (p_ == eob_)
    {
	if (preloaded_)		// end of buffer means end of file
	    eof_ = true;
	else
	{
	    int bufcontents = read (fd_, buf_, buflen_);
	    eof_ = (bufcontents == 0);
	    p_ = buf_;
	    eob_ = buf_ + bufcontents;
	}
    }
    return eof_ ? 0 : *p_++;
}

const Byte *InputByteStream::getBlock (unsigned int pos, unsigned int length)
{
    if (eof_)
	return 0;

    Byte *blockp = buf_ + pos;
    if (blockp < buf_)
	throw DviBug ("InputByteStream: pointer before buffer start");
    if (blockp > eob_)
    {
	char buf[100];
	sprintf (buf, "InputByteStream::getBlock: pointer beyond EOF (%d,%d)",
		 pos, length);
	throw DviBug (buf);
    }
    if (blockp+length > eob_)
    {
	char buf[100];
	sprintf (buf,
		"InputByteStream::getBlock: requested block too large (%d,%d)",
		 pos,length);
	throw DviBug (buf);
    }

    return blockp;
}

bool InputByteStream::eof()
{
    return eof_;
}

void InputByteStream::seek (unsigned int pos)
{
    if (!preloaded_)
	throw DviBug ("Can't seek non-preloaded file");
    if (pos < 0 || pos > buflen_)
	throw DviBug ("seek out of range");
    p_ = buf_ + pos;
}
unsigned int InputByteStream::pos ()
{
    if (!preloaded_)
	throw DviBug ("Can't get pos in non-preloaded file");
    return p_ - buf_;
}
void InputByteStream::skip (unsigned int skipsize)
{
    if (!preloaded_)
	throw DviBug ("Can't skip in non-preloaded file");
    p_ += skipsize;
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
	throw DviBug("bad argument to getSIS");
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
	throw DviBug("bad argument to getSIU");
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
	throw DviBug("bad argument to getUIU");
    unsigned int t = 0;
    for (; n>0; n--)
    {
	t *= 256;
	t += getByte();
    }
    return t;
}

