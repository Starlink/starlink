// file      : xsd/cxx/tree/bits/literals.ixx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_BITS_LITERALS_IXX
#define XSD_CXX_TREE_BITS_LITERALS_IXX

#endif // XSD_CXX_TREE_BITS_LITERALS_IXX


#if defined(XSD_CXX_TREE_USE_CHAR) || !defined(XSD_CXX_TREE_USE_WCHAR)

#ifndef XSD_CXX_TREE_BITS_LITERALS_IXX_CHAR
#define XSD_CXX_TREE_BITS_LITERALS_IXX_CHAR

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      namespace bits
      {
        //
        //
        template<>
        inline const char*
        true_<char> ()
        {
          return "true";
        }

        template<>
        inline const char*
        one<char> ()
        {
          return "1";
        }

        //
        //
        template<>
        inline const char*
        positive_inf<char> ()
        {
          return "INF";
        }

        template<>
        inline const char*
        negative_inf<char> ()
        {
          return "-INF";
        }

        template<>
        inline const char*
        nan<char> ()
        {
          return "NaN";
        }

        //
        //
        template<>
        inline const char*
        not_present<char> ()
        {
          return "<not present>";
        }

        //
        //
        template <>
        inline const char*
        xml_schema<char> ()
        {
          return "http://www.w3.org/2001/XMLSchema";
        }

        //
        //
        template <>
        inline const char*
        any_type<char> ()
        {
          return "anyType";
        }

        template <>
        inline const char*
        any_simple_type<char> ()
        {
          return "anySimpleType";
        }

        template <>
        inline const char*
        string<char> ()
        {
          return "string";
        }

        template <>
        inline const char*
        normalized_string<char> ()
        {
          return "normalizedString";
        }

        template <>
        inline const char*
        token<char> ()
        {
          return "token";
        }

        template <>
        inline const char*
        name<char> ()
        {
          return "Name";
        }

        template <>
        inline const char*
        nmtoken<char> ()
        {
          return "NMTOKEN";
        }

        template <>
        inline const char*
        nmtokens<char> ()
        {
          return "NMTOKENS";
        }

        template <>
        inline const char*
        ncname<char> ()
        {
          return "NCName";
        }

        template <>
        inline const char*
        language<char> ()
        {
          return "language";
        }


        template <>
        inline const char*
        id<char> ()
        {
          return "ID";
        }

        template <>
        inline const char*
        idref<char> ()
        {
          return "IDREF";
        }

        template <>
        inline const char*
        idrefs<char> ()
        {
          return "IDREFS";
        }

        template <>
        inline const char*
        any_uri<char> ()
        {
          return "anyURI";
        }

        template <>
        inline const char*
        qname<char> ()
        {
          return "QName";
        }

        template <>
        inline const char*
        base64_binary<char> ()
        {
          return "base64Binary";
        }

        template <>
        inline const char*
        hex_binary<char> ()
        {
          return "hexBinary";
        }

        template <>
        inline const char*
        date<char> ()
        {
          return "date";
        }

        template <>
        inline const char*
        date_time<char> ()
        {
          return "dateTime";
        }

        template <>
        inline const char*
        duration<char> ()
        {
          return "duration";
        }

        template <>
        inline const char*
        gday<char> ()
        {
          return "gDay";
        }

        template <>
        inline const char*
        gmonth<char> ()
        {
          return "gMonth";
        }

        template <>
        inline const char*
        gmonth_day<char> ()
        {
          return "gMonthDay";
        }

        template <>
        inline const char*
        gyear<char> ()
        {
          return "gYear";
        }

        template <>
        inline const char*
        gyear_month<char> ()
        {
          return "gYearMonth";
        }

        template <>
        inline const char*
        time<char> ()
        {
          return "time";
        }

        template <>
        inline const char*
        entity<char> ()
        {
          return "ENTITY";
        }

        template <>
        inline const char*
        entities<char> ()
        {
          return "ENTITIES";
        }

        template <>
        inline const char*
        gday_prefix<char> ()
        {
          return "---";
        }

        template <>
        inline const char*
        gmonth_prefix<char> ()
        {
          return "--";
        }
      }
    }
  }
}

#endif // XSD_CXX_TREE_BITS_LITERALS_IXX_CHAR
#endif // XSD_CXX_TREE_USE_CHAR


#if defined(XSD_CXX_TREE_USE_WCHAR) || !defined(XSD_CXX_TREE_USE_CHAR)

#ifndef XSD_CXX_TREE_BITS_LITERALS_IXX_WCHAR
#define XSD_CXX_TREE_BITS_LITERALS_IXX_WCHAR

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      namespace bits
      {
        //
        //
        template<>
        inline const wchar_t*
        true_<wchar_t> ()
        {
          return L"true";
        }

        template<>
        inline const wchar_t*
        one<wchar_t> ()
        {
          return L"1";
        }

        //
        //
        template<>
        inline const wchar_t*
        positive_inf<wchar_t> ()
        {
          return L"INF";
        }

        template<>
        inline const wchar_t*
        negative_inf<wchar_t> ()
        {
          return L"-INF";
        }

        template<>
        inline const wchar_t*
        nan<wchar_t> ()
        {
          return L"NaN";
        }

        //
        //
        template<>
        inline const wchar_t*
        not_present<wchar_t> ()
        {
          return L"<not present>";
        }

        //
        //
        template <>
        inline const wchar_t*
        xml_schema<wchar_t> ()
        {
          return L"http://www.w3.org/2001/XMLSchema";
        }

        //
        //
        template <>
        inline const wchar_t*
        any_type<wchar_t> ()
        {
          return L"anyType";
        }

        template <>
        inline const wchar_t*
        any_simple_type<wchar_t> ()
        {
          return L"anySimpleType";
        }

        template <>
        inline const wchar_t*
        string<wchar_t> ()
        {
          return L"string";
        }

        template <>
        inline const wchar_t*
        normalized_string<wchar_t> ()
        {
          return L"normalizedString";
        }

        template <>
        inline const wchar_t*
        token<wchar_t> ()
        {
          return L"token";
        }

        template <>
        inline const wchar_t*
        name<wchar_t> ()
        {
          return L"Name";
        }

        template <>
        inline const wchar_t*
        nmtoken<wchar_t> ()
        {
          return L"NMTOKEN";
        }

        template <>
        inline const wchar_t*
        nmtokens<wchar_t> ()
        {
          return L"NMTOKENS";
        }

        template <>
        inline const wchar_t*
        ncname<wchar_t> ()
        {
          return L"NCName";
        }

        template <>
        inline const wchar_t*
        language<wchar_t> ()
        {
          return L"language";
        }


        template <>
        inline const wchar_t*
        id<wchar_t> ()
        {
          return L"ID";
        }

        template <>
        inline const wchar_t*
        idref<wchar_t> ()
        {
          return L"IDREF";
        }

        template <>
        inline const wchar_t*
        idrefs<wchar_t> ()
        {
          return L"IDREFS";
        }

        template <>
        inline const wchar_t*
        any_uri<wchar_t> ()
        {
          return L"anyURI";
        }

        template <>
        inline const wchar_t*
        qname<wchar_t> ()
        {
          return L"QName";
        }

        template <>
        inline const wchar_t*
        base64_binary<wchar_t> ()
        {
          return L"base64Binary";
        }

        template <>
        inline const wchar_t*
        hex_binary<wchar_t> ()
        {
          return L"hexBinary";
        }

        template <>
        inline const wchar_t*
        date<wchar_t> ()
        {
          return L"date";
        }

        template <>
        inline const wchar_t*
        date_time<wchar_t> ()
        {
          return L"dateTime";
        }

        template <>
        inline const wchar_t*
        duration<wchar_t> ()
        {
          return L"duration";
        }

        template <>
        inline const wchar_t*
        gday<wchar_t> ()
        {
          return L"gDay";
        }

        template <>
        inline const wchar_t*
        gmonth<wchar_t> ()
        {
          return L"gMonth";
        }

        template <>
        inline const wchar_t*
        gmonth_day<wchar_t> ()
        {
          return L"gMonthDay";
        }

        template <>
        inline const wchar_t*
        gyear<wchar_t> ()
        {
          return L"gYear";
        }

        template <>
        inline const wchar_t*
        gyear_month<wchar_t> ()
        {
          return L"gYearMonth";
        }

        template <>
        inline const wchar_t*
        time<wchar_t> ()
        {
          return L"time";
        }

        template <>
        inline const wchar_t*
        entity<wchar_t> ()
        {
          return L"ENTITY";
        }

        template <>
        inline const wchar_t*
        entities<wchar_t> ()
        {
          return L"ENTITIES";
        }

        template <>
        inline const wchar_t*
        gday_prefix<wchar_t> ()
        {
          return L"---";
        }

        template <>
        inline const wchar_t*
        gmonth_prefix<wchar_t> ()
        {
          return L"--";
        }
      }
    }
  }
}

#endif // XSD_CXX_TREE_BITS_LITERALS_IXX_WCHAR
#endif // XSD_CXX_TREE_USE_WCHAR
