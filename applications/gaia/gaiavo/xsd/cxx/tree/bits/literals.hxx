// file      : xsd/cxx/tree/bits/literals.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_BITS_LITERALS_HXX
#define XSD_CXX_TREE_BITS_LITERALS_HXX

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      namespace bits
      {
        // Boolean literals
        //
        template<typename C>
        const C*
        true_ ();

        template<typename C>
        const C*
        one ();

        // Float literals: INF -INF NaN.
        //
        template<typename C>
        const C*
        positive_inf ();

        template<typename C>
        const C*
        negative_inf ();

        template<typename C>
        const C*
        nan ();

        // Optional "not present" literal.
        //
        template<typename C>
        const C*
        not_present ();

        // XML Schema namespace
        //
        template <typename C>
        const C*
        xml_schema ();

        // Built-in XML Schema type names.
        //
        template <typename C>
        const C*
        any_type ();

        template <typename C>
        const C*
        any_simple_type ();

        template <typename C>
        const C*
        string ();

        template <typename C>
        const C*
        normalized_string ();

        template <typename C>
        const C*
        token ();

        template <typename C>
        const C*
        name ();

        template <typename C>
        const C*
        nmtoken ();

        template <typename C>
        const C*
        nmtokens ();

        template <typename C>
        const C*
        ncname ();

        template <typename C>
        const C*
        language ();

        template <typename C>
        const C*
        id ();

        template <typename C>
        const C*
        idref ();

        template <typename C>
        const C*
        idrefs ();

        template <typename C>
        const C*
        any_uri ();

        template <typename C>
        const C*
        qname ();

        template <typename C>
        const C*
        base64_binary ();

        template <typename C>
        const C*
        hex_binary ();

        template <typename C>
        const C*
        date ();

        template <typename C>
        const C*
        date_time ();

        template <typename C>
        const C*
        duration ();

        template <typename C>
        const C*
        gday ();

        template <typename C>
        const C*
        gmonth ();

        template <typename C>
        const C*
        gmonth_day ();

        template <typename C>
        const C*
        gyear ();

        template <typename C>
        const C*
        gyear_month ();

        template <typename C>
        const C*
        time ();

        template <typename C>
        const C*
        entity ();

        template <typename C>
        const C*
        entities ();

        // gday ("---") and gmonth ("--") prefixes.
        //
        template <typename C>
        const C*
        gday_prefix ();

        template <typename C>
        const C*
        gmonth_prefix ();
      }
    }
  }
}

#endif  // XSD_CXX_TREE_BITS_LITERALS_HXX

#include <xsd/cxx/tree/bits/literals.ixx>
