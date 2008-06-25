// file      : xsd/cxx/tree/istream.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_ISTREAM_HXX
#define XSD_CXX_TREE_ISTREAM_HXX

#include <cstddef> // std::size_t

#include <xsd/cxx/tree/istream-fwd.hxx>

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      class istream_common
      {
      public:
        template <typename X>
        struct as_size
        {
          explicit as_size (X& x) : x_ (x) {}
          X& x_;
        };


        // 8-bit
        //
        template <typename X>
        struct as_int8
        {
          explicit as_int8 (X& x) : x_ (x) {}
          X& x_;
        };

        template <typename X>
        struct as_uint8
        {
          explicit as_uint8 (X& x) : x_ (x) {}
          X& x_;
        };


        // 16-bit
        //
        template <typename X>
        struct as_int16
        {
          explicit as_int16 (X& x) : x_ (x) {}
          X& x_;
        };

        template <typename X>
        struct as_uint16
        {
          explicit as_uint16 (X& x) : x_ (x) {}
          X& x_;
        };


        // 32-bit
        //
        template <typename X>
        struct as_int32
        {
          explicit as_int32 (X& x) : x_ (x) {}
          X& x_;
        };

        template <typename X>
        struct as_uint32
        {
          explicit as_uint32 (X& x) : x_ (x) {}
          X& x_;
        };


        // 64-bit
        //
        template <typename X>
        struct as_int64
        {
          explicit as_int64 (X& x) : x_ (x) {}
          X& x_;
        };

        template <typename X>
        struct as_uint64
        {
          explicit as_uint64 (X& x) : x_ (x) {}
          X& x_;
        };


        // Boolean
        //
        template <typename X>
        struct as_bool
        {
          explicit as_bool (X& x) : x_ (x) {}
          X& x_;
        };


        // Floating-point
        //
        template <typename X>
        struct as_float32
        {
          explicit as_float32 (X& x) : x_ (x) {}
          X& x_;
        };

        template <typename X>
        struct as_float64
        {
          explicit as_float64 (X& x) : x_ (x) {}
          X& x_;
        };
      };

      template<typename S>
      class istream: public istream_common
      {
      public:
        explicit
        istream (S& s)
            : s_ (s)
        {
        }

        S&
        impl ()
        {
          return s_;
        }

      private:
        istream (const istream&);
        istream&
        operator= (const istream&);

      private:
        S& s_;
      };


      // 8-bit
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, signed char& x)
      {
        istream_common::as_int8<signed char> as_int8 (x);
        return s >> as_int8;
      }

      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, unsigned char& x)
      {
        istream_common::as_uint8<unsigned char> as_uint8 (x);
        return s >> as_uint8;
      }


      // 16-bit
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, short& x)
      {
        istream_common::as_int16<short> as_int16 (x);
        return s >> as_int16;
      }

      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, unsigned short& x)
      {
        istream_common::as_uint16<unsigned short> as_uint16 (x);
        return s >> as_uint16;
      }


      // 32-bit
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, int& x)
      {
        istream_common::as_int32<int> as_int32 (x);
        return s >> as_int32;
      }

      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, unsigned int& x)
      {
        istream_common::as_uint32<unsigned int> as_uint32 (x);
        return s >> as_uint32;
      }


      // 64-bit
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, long long& x)
      {
        istream_common::as_int64<long long> as_int64 (x);
        return s >> as_int64;
      }

      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, unsigned long long& x)
      {
        istream_common::as_uint64<unsigned long long> as_uint64 (x);
        return s >> as_uint64;
      }

      // Boolean
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, bool& x)
      {
        istream_common::as_bool<bool> as_bool (x);
        return s >> as_bool;
      }


      // Floating-point
      //
      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, float& x)
      {
        istream_common::as_float32<float> as_float32 (x);
        return s >> as_float32;
      }

      template <typename S>
      inline istream<S>&
      operator>> (istream<S>& s, double& x)
      {
        istream_common::as_float64<double> as_float64 (x);
        return s >> as_float64;
      }
    }
  }
}

#endif  // XSD_CXX_TREE_ISTREAM_HXX
