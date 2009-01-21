// file      : xsd/cxx/tree/exceptions.ixx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#if defined(XSD_CXX_TREE_USE_CHAR) || !defined(XSD_CXX_TREE_USE_WCHAR)

#ifndef XSD_CXX_TREE_EXCEPTIONS_IXX_CHAR
#define XSD_CXX_TREE_EXCEPTIONS_IXX_CHAR

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {

      // error
      //
      inline
      std::basic_ostream<char>&
      operator<< (std::basic_ostream<char>& os, const error<char>& e)
      {
        return os << e.id () << ':' << e.line () << ':' << e.column ()
                  << (e.severity () == severity::error
                      ? " error: "
                      : " warning: ") << e.message ();
      }


      // diagnostics
      //
      inline
      std::basic_ostream<char>&
      operator<< (std::basic_ostream<char>& os, const diagnostics<char>& d)
      {
        for (diagnostics<char>::const_iterator b (d.begin ()), i (b);
             i != d.end ();
             ++i)
        {
          if (i != b)
            os << "\n";

          os << *i;
        }

        return os;
      }


      // parsing
      //
      template<>
      inline
      void parsing<char>::
      print (std::basic_ostream<char>& os) const
      {
        if (diagnostics_.empty ())
          os << "instance document parsing failed";
        else
          os << diagnostics_;
      }


      // expected_element
      //
      template<>
      inline
      void expected_element<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "expected element '"
           << namespace_ () << (namespace_ ().empty () ? "" : "#")
           << name () << "'";
      }


      // unexpected_element
      //
      template<>
      inline
      void unexpected_element<char>::
      print (std::basic_ostream<char>& os) const
      {
        if (!expected_name ().empty ())
        {
          os << "expected element '"
             << expected_namespace ()
             << (expected_namespace ().empty () ? "" : "#")
             << expected_name ()
             << "' instead of '"
             << encountered_namespace ()
             << (encountered_namespace ().empty () ? "" : "#")
             << encountered_name () << "'";
        }
        else
        {
          os << "unexpected element '"
             << encountered_namespace ()
             << (encountered_namespace ().empty () ? "" : "#")
             << encountered_name () << "'";
        }
      }


      // expected_attribute
      //
      template<>
      inline
      void expected_attribute<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "expected attribute '"
           << namespace_ () << (namespace_ ().empty () ? "" : "#")
           << name () << "'";
      }


      // unexpected_enumerator
      //
      template<>
      inline
      void unexpected_enumerator<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "unexpected enumerator '" << enumerator () << "'";
      }


      // expected_text_content
      //
      template<>
      inline
      void expected_text_content<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "expected text content";
      }


      // no_type_info
      //
      template<>
      inline
      void no_type_info<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "no type information available for type '"
           << type_namespace () << (type_namespace ().empty () ? "" : "#")
           << type_name () << "'";
      }


      // not_derived
      //
      template<>
      inline
      void not_derived<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "type '"
           << derived_type_namespace ()
           << (derived_type_namespace ().empty () ? "" : "#")
           << derived_type_name ()
           << "' is not derived from '"
           << base_type_namespace ()
           << (base_type_namespace ().empty () ? "" : "#")
           << base_type_name () << "'";
      }


      // duplicate_id
      //
      template<>
      inline
      void duplicate_id<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "ID '" << id () << "' already exist";
      }


      // serialization
      //
      template<>
      inline
      void serialization<char>::
      print (std::basic_ostream<char>& os) const
      {
        if (diagnostics_.empty ())
          os << "serialization failed";
        else
          os << diagnostics_;
      }


      // no_prefix_mapping
      //
      template<>
      inline
      void no_prefix_mapping<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "no mapping provided for namespace prefix '"
           << prefix () << "'";
      }


      // bounds
      //
      template<>
      inline
      void bounds<char>::
      print (std::basic_ostream<char>& os) const
      {
        os << "buffer boundary rules have been violated";
      }
    }
  }
}

#endif // XSD_CXX_TREE_EXCEPTIONS_IXX_CHAR
#endif // XSD_CXX_TREE_USE_CHAR


#if defined(XSD_CXX_TREE_USE_WCHAR) || !defined(XSD_CXX_TREE_USE_CHAR)

#ifndef XSD_CXX_TREE_EXCEPTIONS_IXX_WCHAR
#define XSD_CXX_TREE_EXCEPTIONS_IXX_WCHAR

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      // error
      //
      inline
      std::basic_ostream<wchar_t>&
      operator<< (std::basic_ostream<wchar_t>& os, const error<wchar_t>& e)
      {
        return os << e.id () << L':' << e.line () << L':' << e.column ()
                  << (e.severity () == severity::error
                      ? L" error: "
                      : L" warning: ") << e.message ();
      }


      // diagnostics
      //
      inline
      std::basic_ostream<wchar_t>&
      operator<< (std::basic_ostream<wchar_t>& os,
                  const diagnostics<wchar_t>& d)
      {
        for (diagnostics<wchar_t>::const_iterator b (d.begin ()), i (b);
             i != d.end ();
             ++i)
        {
          if (i != b)
            os << L"\n";

          os << *i;
        }

        return os;
      }


      // parsing
      //
      template<>
      inline
      void parsing<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        if (diagnostics_.empty ())
          os << L"instance document parsing failed";
        else
          os << diagnostics_;
      }


      // expected_element
      //
      template<>
      inline
      void expected_element<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"expected element '"
           << namespace_ () << (namespace_ ().empty () ? L"" : L"#")
           << name () << L"'";
      }


      // unexpected_element
      //
      template<>
      inline
      void unexpected_element<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        if (!expected_name ().empty ())
        {
          os << L"expected element '"
             << expected_namespace ()
             << (expected_namespace ().empty () ? L"" : L"#")
             << expected_name ()
             << L"' instead of '"
             << encountered_namespace ()
             << (encountered_namespace ().empty () ? L"" : L"#")
             << encountered_name () << L"'";
        }
        else
        {
          os << L"unexpected element '"
             << encountered_namespace ()
             << (encountered_namespace ().empty () ? L"" : L"#")
             << encountered_name () << L"'";
        }
      }


      // expected_attribute
      //
      template<>
      inline
      void expected_attribute<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"expected attribute '"
           << namespace_ () << (namespace_ ().empty () ? L"" : L"#")
           << name () << L"'";
      }


      // unexpected_enumerator
      //
      template<>
      inline
      void unexpected_enumerator<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"unexpected enumerator '" << enumerator () << L"'";
      }


      // expected_text_content
      //
      template<>
      inline
      void expected_text_content<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"expected text content";
      }


      // no_type_info
      //
      template<>
      inline
      void no_type_info<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"no type information available for type '"
           << type_namespace () << (type_namespace ().empty () ? L"" : L"#")
           << type_name () << L"'";
      }


      // not_derived
      //
      template<>
      inline
      void not_derived<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"type '"
           << derived_type_namespace ()
           << (derived_type_namespace ().empty () ? L"" : L"#")
           << derived_type_name ()
           << L"' is not derived from '"
           << base_type_namespace ()
           << (base_type_namespace ().empty () ? L"" : L"#")
           << base_type_name () << L"'";
      }


      // duplicate_id
      //
      template<>
      inline
      void duplicate_id<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"ID '" << id () << L"' already exist";
      }


      // serialization
      //
      template<>
      inline
      void serialization<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        if (diagnostics_.empty ())
          os << L"serialization failed";
        else
          os << diagnostics_;
      }


      // no_prefix_mapping
      //
      template<>
      inline
      void no_prefix_mapping<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"no mapping provided for namespace prefix '"
           << prefix () << L"'";
      }


      // bounds
      //
      template<>
      inline
      void bounds<wchar_t>::
      print (std::basic_ostream<wchar_t>& os) const
      {
        os << L"buffer boundary rules have been violated";
      }
    }
  }
}

#endif // XSD_CXX_TREE_EXCEPTIONS_IXX_WCHAR
#endif // XSD_CXX_TREE_USE_WCHAR
