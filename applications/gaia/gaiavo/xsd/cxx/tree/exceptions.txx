// file      : xsd/cxx/tree/exceptions.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      // error
      //
      template <typename C>
      error<C>::
      error (tree::severity s,
             const std::basic_string<C>& id,
             unsigned long line,
             unsigned long column,
             const std::basic_string<C>& message)
          : severity_ (s),
            id_ (id),
            line_ (line),
            column_ (column),
            message_ (message)
      {
      }


      // parsing
      //
      template <typename C>
      parsing<C>::
      ~parsing () throw ()
      {
      }

      template <typename C>
      parsing<C>::
      parsing ()
      {
      }

      template <typename C>
      parsing<C>::
      parsing (const tree::diagnostics<C>& diagnostics)
          : diagnostics_ (diagnostics)
      {
      }

      template <typename C>
      const char* parsing<C>::
      what () const throw ()
      {
        return "instance document parsing failed";
      }


      // expected_element
      //
      template <typename C>
      expected_element<C>::
      ~expected_element () throw ()
      {
      }

      template <typename C>
      expected_element<C>::
      expected_element (const std::basic_string<C>& name,
                        const std::basic_string<C>& namespace_)
          : name_ (name), namespace__ (namespace_)
      {
      }

      template <typename C>
      const char* expected_element<C>::
      what () const throw ()
      {
        return "expected element not encountered";
      }


      // unexpected_element
      //
      template <typename C>
      unexpected_element<C>::
      ~unexpected_element () throw ()
      {
      }

      template <typename C>
      unexpected_element<C>::
      unexpected_element (const std::basic_string<C>& encountered_name,
                          const std::basic_string<C>& encountered_namespace,
                          const std::basic_string<C>& expected_name,
                          const std::basic_string<C>& expected_namespace)
          : encountered_name_ (encountered_name),
            encountered_namespace_ (encountered_namespace),
            expected_name_ (expected_name),
            expected_namespace_ (expected_namespace)
      {
      }

      template <typename C>
      const char* unexpected_element<C>::
      what () const throw ()
      {
        return "unexpected element encountered";
      }


      // expected_attribute
      //
      template <typename C>
      expected_attribute<C>::
      ~expected_attribute () throw ()
      {
      }

      template <typename C>
      expected_attribute<C>::
      expected_attribute (const std::basic_string<C>& name,
                          const std::basic_string<C>& namespace_)
          : name_ (name), namespace__ (namespace_)
      {
      }

      template <typename C>
      const char* expected_attribute<C>::
      what () const throw ()
      {
        return "expected attribute not encountered";
      }


      // unexpected_enumerator
      //
      template <typename C>
      unexpected_enumerator<C>::
      ~unexpected_enumerator () throw ()
      {
      }

      template <typename C>
      unexpected_enumerator<C>::
      unexpected_enumerator (const std::basic_string<C>& enumerator)
          : enumerator_ (enumerator)
      {
      }

      template <typename C>
      const char* unexpected_enumerator<C>::
      what () const throw ()
      {
        return "unexpected enumerator encountered";
      }


      // expected_text_content
      //
      template <typename C>
      const char* expected_text_content<C>::
      what () const throw ()
      {
        return "expected text content";
      }


      // no_type_info
      //
      template <typename C>
      no_type_info<C>::
      ~no_type_info () throw ()
      {
      }

      template <typename C>
      no_type_info<C>::
      no_type_info (const std::basic_string<C>& type_name,
                    const std::basic_string<C>& type_namespace)
          : type_name_ (type_name),
            type_namespace_ (type_namespace)
      {
      }

      template <typename C>
      const char* no_type_info<C>::
      what () const throw ()
      {
        return "no type information registered for a type";
      }


      // not_derived
      //
      template <typename C>
      not_derived<C>::
      ~not_derived () throw ()
      {
      }

      template <typename C>
      not_derived<C>::
      not_derived (const std::basic_string<C>& base_type_name,
                   const std::basic_string<C>& base_type_namespace,
                   const std::basic_string<C>& derived_type_name,
                   const std::basic_string<C>& derived_type_namespace)
          : base_type_name_ (base_type_name),
            base_type_namespace_ (base_type_namespace),
            derived_type_name_ (derived_type_name),
            derived_type_namespace_ (derived_type_namespace)
      {
      }

      template <typename C>
      const char* not_derived<C>::
      what () const throw ()
      {
        return "type is not derived";
      }


      // duplicate_id
      //
      template <typename C>
      duplicate_id<C>::
      ~duplicate_id () throw ()
      {
      }

      template <typename C>
      duplicate_id<C>::
      duplicate_id (const std::basic_string<C>& id)
          : id_ (id)
      {
      }

      template <typename C>
      const char* duplicate_id<C>::
      what () const throw ()
      {
        return "ID already exist";
      }


      // serialization
      //
      template <typename C>
      serialization<C>::
      ~serialization () throw ()
      {
      }

      template <typename C>
      serialization<C>::
      serialization ()
      {
      }

      template <typename C>
      serialization<C>::
      serialization (const tree::diagnostics<C>& diagnostics)
          : diagnostics_ (diagnostics)
      {
      }

      template <typename C>
      const char* serialization<C>::
      what () const throw ()
      {
        return "serialization failed";
      }


      // no_prefix_mapping
      //
      template <typename C>
      no_prefix_mapping<C>::
      ~no_prefix_mapping () throw ()
      {
      }

      template <typename C>
      no_prefix_mapping<C>::
      no_prefix_mapping (const std::basic_string<C>& prefix)
          : prefix_ (prefix)
      {
      }

      template <typename C>
      const char* no_prefix_mapping<C>::
      what () const throw ()
      {
        return "no mapping provided for a namespace prefix";
      }


      // bounds
      //
      template <typename C>
      const char* bounds<C>::
      what () const throw ()
      {
        return "buffer boundary rules have been violated";
      }
    }
  }
}

