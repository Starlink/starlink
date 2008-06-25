// file      : xsd/cxx/xml/string.ixx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_XML_STRING_IXX
#define XSD_CXX_XML_STRING_IXX

#include <cassert>
#include <cstring> // std::memcpy

#include <xercesc/util/XMLString.hpp>
#include <xsd/cxx/xml/std-memory-manager.hxx>

// We sometimes need this functionality even if we are building for
// wchar_t.
//
namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
#ifndef XSD_USE_LCP
      namespace bits
      {
        // UTF-16 to/from UTF-8 transcoder.
        //
        template <typename C>
        struct char_transcoder
        {
          static std::basic_string<C>
          to (const XMLCh* s, std::size_t length);

          static XMLCh*
          from (const C* s, std::size_t length);

        private:
          static const unsigned char first_byte_mask_[5];
        };
      }
#endif

      template <>
      inline std::basic_string<char>
      transcode<char> (const XMLCh* s)
      {
        if (s == 0)
          return std::basic_string<char> ();

#ifndef XSD_USE_LCP
        return bits::char_transcoder<char>::to (
          s, xercesc::XMLString::stringLen (s));
#else
        // Use Xerces-C++ local code page transcoding.
        //
        std_memory_manager mm;
        auto_array<char, std_memory_manager> r (
          xercesc::XMLString::transcode (s, &mm), mm);
        return std::basic_string<char> (r.get ());
#endif
      }

      template <>
      inline std::basic_string<char>
      transcode<char> (const XMLCh* s, std::size_t len)
      {
        if (s == 0 || len == 0)
          return std::basic_string<char> ();

#ifndef XSD_USE_LCP
        // Convert UTF-16 to UTF-8
        //
        return bits::char_transcoder<char>::to (s, len);
#else
        // Use Xerces-C++ local code page transcoding.
        //
        auto_array<XMLCh> tmp (new XMLCh[len + 1]);
        std::memcpy (tmp.get (), s, len * sizeof (XMLCh));
        tmp[len] = XMLCh (0);

        std_memory_manager mm;
        auto_array<char, std_memory_manager> r (
          xercesc::XMLString::transcode (tmp.get (), &mm), mm);

        tmp.reset ();

        return std::basic_string<char> (r.get ());
#endif
      }

      template <>
      inline XMLCh*
      transcode_to_xmlch (const char* s)
      {
#ifndef XSD_USE_LCP
        // Convert UTF-8 to UTF-16
        //
        return bits::char_transcoder<char>::from (
          s, std::char_traits<char>::length (s));
#else
        // Use Xerces-C++ local code page transcoding.
        //
        std_memory_manager mm;
        return xercesc::XMLString::transcode (s, &mm);
#endif
      }

      template <>
      inline XMLCh*
      transcode_to_xmlch (const std::basic_string<char>& s)
      {
#ifndef XSD_USE_LCP
        // Convert UTF-8 to UTF-16
        //
        return bits::char_transcoder<char>::from (
          s.c_str (), s.length ());
#else
        // Use Xerces-C++ local code page transcoding.
        //
        std_memory_manager mm;
        return xercesc::XMLString::transcode (s.c_str (), &mm);
#endif
      }
    }
  }
}

#endif // XSD_CXX_XML_STRING_IXX


#if defined(XSD_USE_CHAR) || !defined(XSD_USE_WCHAR)

#ifndef XSD_CXX_XML_STRING_IXX_CHAR
#define XSD_CXX_XML_STRING_IXX_CHAR

#endif // XSD_CXX_XML_STRING_IXX_CHAR
#endif // XSD_USE_CHAR


#if defined(XSD_USE_WCHAR) || !defined(XSD_USE_CHAR)

#ifndef XSD_CXX_XML_STRING_IXX_WCHAR
#define XSD_CXX_XML_STRING_IXX_WCHAR

namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
      namespace bits
      {
        template <typename W, std::size_t S>
        struct wchar_transcoder;

        // Specialization for 2-byte wchar_t (resulting encoding is UTF-16).
        //
        template <typename W>
        struct wchar_transcoder<W, 2>
        {
          static std::basic_string<W>
          to (const XMLCh* s, std::size_t length);

          static XMLCh*
          from (const W* s, std::size_t length);
        };


        // Specialization for 4-byte wchar_t (resulting encoding is UCS-4).
        //
        template <typename W>
        struct wchar_transcoder<W, 4>
        {
          static std::basic_string<W>
          to (const XMLCh* s, std::size_t length);

          static XMLCh*
          from (const W* s, std::size_t length);
        };
      }

      template <>
      inline std::basic_string<wchar_t>
      transcode<wchar_t> (const XMLCh* s)
      {
        if (s == 0)
          return std::basic_string<wchar_t> ();

        return bits::wchar_transcoder<wchar_t, sizeof (wchar_t)>::to (
          s, xercesc::XMLString::stringLen (s));
      }

      template <>
      inline std::basic_string<wchar_t>
      transcode<wchar_t> (const XMLCh* s, std::size_t len)
      {
        if (s == 0 || len == 0)
          return std::basic_string<wchar_t> ();

        return bits::wchar_transcoder<wchar_t, sizeof (wchar_t)>::to (
          s, len);
      }

      template <>
      inline XMLCh*
      transcode_to_xmlch (const wchar_t* s)
      {
        return bits::wchar_transcoder<wchar_t, sizeof (wchar_t)>::from (
          s, std::char_traits<wchar_t>::length (s));
      }

      template <>
      inline XMLCh*
      transcode_to_xmlch (const std::basic_string<wchar_t>& s)
      {
        return bits::wchar_transcoder<wchar_t, sizeof (wchar_t)>::from (
          s.c_str (), s.length ());
      }
    }
  }
}

#endif // XSD_CXX_XML_STRING_IXX_WCHAR
#endif // XSD_USE_WCHAR
