// file      : xsd/cxx/zc-istream.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_ZC_ISTREAM_HXX
#define XSD_CXX_ZC_ISTREAM_HXX

#include <string>
#include <istream>

#include <xsd/cxx/ro-string.hxx>

namespace xsd
{
  namespace cxx
  {
    // Input streambuffer that does not copy the underlying
    // buffer (zero copy).
    //
    template <typename C>
    class zc_streambuf: public std::basic_streambuf<C>
    {
    public:
      typedef typename std::basic_streambuf<C>::int_type int_type;
      typedef typename std::basic_streambuf<C>::traits_type traits_type;

    public:
      zc_streambuf (const ro_string<C>&);
      zc_streambuf (const std::basic_string<C>&);

    protected:
      virtual std::streamsize
      showmanyc ();

      virtual int_type
      underflow ();

    private:
      void
      init ();

    private:
      zc_streambuf (const zc_streambuf&);

      zc_streambuf&
      operator= (const zc_streambuf&);

    private:
      ro_string<C> str_;
    };


    // Input string stream that does not copy the underlying string.
    //
    template <typename C>
    class zc_istream_base
    {
    protected:
      zc_istream_base (const ro_string<C>&);
      zc_istream_base (const std::basic_string<C>&);

    protected:
      zc_streambuf<C> buf_;
    };

    template <typename C>
    class zc_istream: protected zc_istream_base<C>,
                      public std::basic_istream<C>
    {
    public:
      zc_istream (const ro_string<C>&);
      zc_istream (const std::basic_string<C>&);

      bool
      exhausted ()
      {
        return this->get () == std::basic_istream<C>::traits_type::eof ();
      }

    private:
      zc_istream (const zc_istream&);

      zc_istream&
      operator= (const zc_istream&);
    };
  }
}

#include <xsd/cxx/zc-istream.txx>

#endif  // XSD_CXX_ZC_ISTREAM_HXX
