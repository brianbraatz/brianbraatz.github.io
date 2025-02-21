---
title: Bag Collection Library C++
description: Bag Collection Library in C++ as a Compile-Time Template Meta Program
slug: cpp-bag
date: 2022-02-12
image: bagcover.png
categories:
  - CPP
  - Algorithms
  - CPP-Meta Programming
tags:
  - CPP
  - Mentoring
  - SmallTalk
  - Python
  - MFC
  - Java
  - DesignPatterns
  - VisitorPattern
  - TemplateProgramming
  - WinAPI
weight: 36
draft: false
lastmod: 2025-02-20T22:01:40.413Z
---
[Template Metaprogramming-Wikipedia](https://en.wikipedia.org/wiki/Template_metaprogramming)

# Whats a bag?

## Bag collections in some popular languages:

| Language      | Description                                                                                                                                                                                                                                                                                                 | Source                                                                                                |
| ------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| **Java**      | Java Collections Framework does not include a bag interface by default. BUT! Apache Commons Collections provide a `Bag` interface with implementations like `HashBag` and `TreeBag`                                                                                                                         | [Wikipedia - Set (abstract data type)](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)    |
| **Python**    | Python's `collections.Counter` class can be used as a bag, and the `collections-extended` library offers a `bag` class with  multiset features.                                                                                                                                                             | [Collections Extended](https://collections-extended.lenzm.net/bags/)                                  |
| **C#**        | .NET includes a `Bag` collection within `System.Collections.Concurrent`, which provides a thread-safe implementation for storing element counts. You can also use a List<Object> and just do type checking at runtime and use the visitor pattern. Which is not as efficient as this library, but it works. | [Wikipedia - Comparison of C# and Java](https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java) |
| **Smalltalk** | Smalltalk has a built-in `Bag` class that allows duplicates and provides methods for adding, removing, and counting elements.                                                                                                                                                                               | [Wikipedia - Set (abstract data type)](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)    |

## Bag Explained

A **bag**, also known as a **multiset**, is a collection data structure that allows multiple occurrences of its elements without any particular order.

The bag my library was inspired by, was actually inspired by the runtime type design patterns in MFC.\
https://es.wikipedia.org/wiki/Microsoft\_Foundation\_Classes

Which in turn was likely inspired by the multiset collection in Small talk , otherwise known as a bag.

Why a bag? because you can put anything in it you want!

With a normal dynamic language, you can do this with a List<Object> type of collection.\
The disadvantage is you have to query the type at runtime.

With my bag collection- you have to tell me at compile time what is possible to be in the bag. The templates will generate the proper collection at compile time.

My bag library is more efficient, since you do not have to do any runtime type, but the disadvantage is you have to use a [Visitor Pattern](https://en.wikipedia.org/wiki/Visitor_pattern)  pattern to operate on the contents.

<!--
## Detailed Comparison to Java, Python and C#
While not all programming languages have a built-in bag collection, many offer implementations through standard libraries or third-party packages. Here are some notable examples:
-->

<!-- 
- **Java**: The Java Collections Framework does not include a bag interface by default. BUT! Apache Commons Collections provide a `Bag` interface with implementations like `HashBag` and `TreeBag`. These classes allow for the storage of elements with their occurrence counts.
    
    [en.wikipedia.org](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)
    
- **Python**: Python's standard library includes the `collections.Counter` class, which functions similarly to a bag by counting the occurrences of elements in a collection. The `collections-extended` library offers a `bag` class with more specialized features for multiset operations.
    
    [collections-extended.lenzm.net](https://collections-extended.lenzm.net/bags/)
    
- **C#**: The .NET framework includes a `Bag` collection within its System.Collections.Concurrent namespace, providing a thread-safe implementation that allows for the storage of elements with their counts.
    
    [en.wikipedia.org](https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java)
    
- **Smalltalk**: Smalltalk's standard library includes the `Bag` class, which allows for the collection of elements without any specific order and permits duplicates. This class provides methods to add, remove, and count occurrences of elements.
    
    [en.wikipedia.org](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)
    --?
    -->

<!-- 
In languages that do not have a built-in bag collection, developers can implement their own by using existing data structures such as dictionaries or hash maps to keep track of element counts.

Sources

![Favicon](https://www.google.com/s2/favicons?domain=https://collections-extended.lenzm.net&sz=32)

![Favicon](https://www.google.com/s2/favicons?domain=https://en.wikipedia.org&sz=32)


Details

# 1

# Reference Links

[

![Favicon](https://www.google.com/s2/favicons?domain=https://en.wikipedia.org&sz=32)en.wikipedia.org

Set (abstract data type)

May 13, 2024

](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)

[

collections-extended.lenzm.net

bags (Multisets) — collections_extended 2.0.2 documentation - LenzM.net

Bags are a multiset implementation for Python. Currently, bags have constant time inclusion testing but can only contain hashable elements due to the implementation. There are three classes provided: An abstract base class for bags. A mutable (unhashable) Bag. An immutable (implements collections.abc.Hashable) Bag.

](https://collections-extended.lenzm.net/bags/)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://en.wikipedia.org&sz=32)en.wikipedia.org

Comparison of C Sharp and Java

3 days ago

](https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java)

More

[

![Favicon](https://www.google.com/s2/favicons?domain=https://www.alexomegapy.com&sz=32)alexomegapy.com

Understanding the Bag ADT in Java: A Flexible Data Structure

October 2, 2024 — Learn how the Bag ADT in Java provides flexibility for handling collections with duplicate elements and dynamic sizing. Discover its real-world application through an inventory system example, showcas...

](https://www.alexomegapy.com/post/understanding-the-bag-adt-in-java-a-flexible-data-structure)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://dzone.com&sz=32)dzone.com

The Bag Data Structure From Eclipse Collections - DZone

July 15, 2021 — In this post, we will review the Bag from Eclipse Collections, which supplies both mutable and immutable versions. Before we will proceed with various Bag methods, let observe how to...

](https://dzone.com/articles/the-bag-data-structure-from-eclipse-collections)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://dev.to&sz=32)dev.to

Three Implementations of a Bag in Python - DEV Community

April 4, 2020 — This article will discuss three different implementations of Bag, each using a different internal data structure. By using different internal data structures, a clearer picture is created on the value...

](https://dev.to/farleyknight/three-implementations-of-a-bag-in-python-585p)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://algs4.cs.princeton.edu&sz=32)algs4.cs.princeton.edu

1.3 Bags, Queues, and Stacks - Princeton University

February 12, 2020 — In this section, we consider three such data types, known as the bag, the queue, and the stack. They differ in the specification of which object is to be removed or examined next. APIs. We define the...

](https://algs4.cs.princeton.edu/13stacks/)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://web.engr.oregonstate.edu&sz=32)web.engr.oregonstate.edu

Chapter 8: Bags and Sets - Oregon State University College of Engineering

Either decision can still legitimately be termed a bag type of collection. The following table gives the names for bag-like containers in several programming languages. The set abstraction includes, i...

](https://web.engr.oregonstate.edu/~sinisa/courses/OSU/CS261/CS261_Textbook/Chapter08.pdf)

[

collections-extended.lenzm.net

bags (Multisets) — collections_extended 2.0.2 documentation - LenzM.net

Bags are a multiset implementation for Python. Currently, bags have constant time inclusion testing but can only contain hashable elements due to the implementation. There are three classes provided:...

](https://collections-extended.lenzm.net/bags/)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://en.wikipedia.org&sz=32)en.wikipedia.org

Collection (abstract data type) - Wikipedia

In computer programming, a collection is an abstract data type that is a grouping of items that can be used in a polymorphic way. Often, the items are of the same data type such as int or string. Some...

](https://en.wikipedia.org/wiki/Collection_%28abstract_data_type%29)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://pandaqi.com&sz=32)pandaqi.com

Bags III | Learn to Code | Programming | Pandaqi Tutorials

You don’t know, so these useful functions don’t exist for named bags. Of course, each programming language does provide a way to add or remove elements from a named bag. But I decided that was just on...

](https://pandaqi.com/tutorials/programming/learn-to-code/bags-iii/)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://www.educative.io&sz=32)educative.io

The Data Collection Bag - Educative

In this lesson, we will describe the bag as a data collection, along with its behaviors. Imagine a paper bag, a reusable cloth bag, or even a plastic bag. People use bags when they shop, pack lunch, o...

](https://www.educative.io/courses/java-masterclass-developers/the-data-collection-bag)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://www.cs.ox.ac.uk&sz=32)cs.ox.ac.uk

LECTURE 14: BAGS (MULTISETS) - Department of Computer Science ...

6 Making Bags out of Sequences One last thing we often want to do is to make a bag out of a sequence, by counting up all number of times in a sequence. We do this using items. EXAMPLE. itemsha;b;a;b;c...

](https://www.cs.ox.ac.uk/people/michael.wooldridge/teaching/soft-eng/lect14.pdf)

[

![Favicon](https://www.google.com/s2/favicons?domain=https://www.tutorialspoint.com&sz=32)tutorialspoint.com

Hibernate - Bag Mappings - Online Tutorials Library

A Bag is a java collection that stores elements without caring about the sequencing, but allow duplicate

](https://www.tutorialspoint.com/hibernate/hibernate_bag_mapping.htm)

#### _Brian Braatz_
-->

<!--- 
image embed
![](__/Templates/cover.jpg)

> select * from BasketballPlayer, SoccerPlayer

LINK format
[markdown-syntax](__/OLDContent/markdown-syntax/index.md) 

[DISPLAY](LINK) 

```
{{< embed-pdf url="./path/to/pdf/file/example.pdf" >}}
```
```
{{< embed-pdf url="https://www.brianbraatz.com/portfolio/EffectiveCPP.pdf" >}}
```


-->

<!-- 
# one

```

```

## 1.1 one one

![[__/Templates/cover.jpg]]

-->

## Some Interesting links on Bags, Sets and Multisets

| Source                  | Title                                                        | Link                                                                      |
| ----------------------- | ------------------------------------------------------------ | ------------------------------------------------------------------------- |
| Wikipedia               | Set (abstract data type)                                     | [Link](https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29)        |
| Collections Extended    | Bags (Multisets) — collections\_extended 2.0.2 documentation | [Link](https://collections-extended.lenzm.net/bags/)                      |
| Wikipedia               | Comparison of C# and Java                                    | [Link](https://en.wikipedia.org/wiki/Comparison_of_C_Sharp_and_Java)      |
| AlexOmegaPy             | Understanding the Bag ADT in Java: A Flexible Data Structure | [Link](https://alexomegapy.com)                                           |
| DZone                   | The Bag Data Structure From Eclipse Collections              | [Link](https://dzone.com)                                                 |
| DEV Community           | Three Implementations of a Bag in Python                     | [Link](https://dev.to)                                                    |
| Princeton University    | 1.3 Bags, Queues, and Stacks                                 | [Link](https://algs4.cs.princeton.edu)                                    |
| Oregon State University | Chapter 8: Bags and Sets                                     | [Link](https://web.engr.oregonstate.edu)                                  |
| Wikipedia               | Collection (abstract data type)                              | [Link](https://en.wikipedia.org/wiki/Collection_%28abstract_data_type%29) |
| Educative               | The Data Collection Bag                                      | [Link](https://educative.io)                                              |
| Oxford University       | LECTURE 14: BAGS (MULTISETS)                                 | [Link](https://cs.ox.ac.uk)                                               |
| TutorialsPoint          | Hibernate - Bag Mappings                                     | [Link](https://tutorialspoint.com)                                        |
| ACM Digital Library     | Tutorial: Languages for Collection Types                     | [Link](https://dl.acm.org)                                                |

## Bag Source Code- Compile Time Meta Program

```c++

#ifndef __BOOST_BAG_BAG_HPP
#define __BOOST_BAG_BAG_HPP

/*
 *
 *  Brian Braatz.
 *  Copyright 2003-2022 Brian C Braatz. All rights reserved.
 *
 */

#ifndef BOOST_MPL_MAP_HPP_INCLUDED
	#include <boost/mpl/map.hpp>
#endif

#include <boost/type_traits/is_reference.hpp>
#include <boost/type_traits/is_same.hpp>
#ifndef BOOST_UTILITY_ENABLE_IF_HPP
	#include <boost/utility/enable_if.hpp>
#endif
	#include <boost/mpl/has_xxx.hpp>
#ifndef BOOST_MPL_IF_HPP_INCLUDED
	#include <boost/mpl/if.hpp>
#endif

#include <boost/mpl/count_if.hpp>

#ifndef BOOST_TT_IS_BASE_AND_DERIVED_HPP_INCLUDED
	#include <boost/type_traits/is_base_and_derived.hpp>
#endif

// #include <boost/type_traits.hpp>
// #ifndef BOOST_TT_IS_SAME_HPP_INCLUDED
	#include <boost/type_traits/is_same.hpp>
// #endif


#include <boost/bag/detail/util.hpp>

// #ifndef BOOST_MPL_VECTOR_HPP_INCLUDED
	#include <boost/mpl/vector.hpp>
// #endif

#include <boost/mpl/transform.hpp>

#ifndef BOOST_UTILITY_ENABLE_IF_HPP
	#include <boost/utility/enable_if.hpp>
#endif

#ifndef BOOST_MPL_APPLY_WRAP_HPP_INCLUDED
	#include <boost/mpl/apply_wrap.hpp>
#endif

#ifndef BOOST_MPL_FIND_IF_HPP_INCLUDED
	#include <boost/mpl/find_if.hpp>
#endif
	
#ifndef BOOST_MPL_RANGE_C_HPP_INCLUDED
	#include <boost/mpl/range_c.hpp>
#endif

#ifndef BOOST_MPL_SIZE_HPP_INCLUDED
	#include <boost/mpl/size.hpp>
#endif
#include <boost/mpl/greater.hpp>

#ifndef BOOST_MPL_TRANSFORM_VIEW_HPP_INCLUDED
	#include <boost/mpl/transform_view.hpp>
#endif

#ifndef BOOST_MPL_FILTER_VIEW_HPP_INCLUDED
	#include <boost/mpl/filter_view.hpp>
#endif

#ifndef BOOST_MPL_ZIP_VIEW_HPP_INCLUDED
	#include <boost/mpl/zip_view.hpp>
#endif

#ifndef BOOST_MPL_INHERIT_HPP_INCLUDED
	#include <boost/mpl/inherit.hpp>
#endif

#ifndef BOOST_MPL_INHERIT_FRONT_TO_BACK_HPP_INCLUDED
	#include <boost/mpl/inherit_linearly.hpp>
#endif

#ifndef BOOST_MPL_UNPACK_ARGS_HPP_INCLUDED
	#include <boost/mpl/unpack_args.hpp>
#endif

#include <boost/mpl/less_equal.hpp>

#ifndef BOOST_MPL_CONTAINS_HPP_INCLUDED
	#include <boost/mpl/contains.hpp>
#endif

#ifndef BOOST_MPL_LAMBDA_HPP_INCLUDED
	#include <boost/mpl/lambda.hpp>
#endif
	#include <boost/mpl/filter_view.hpp>
	#include <boost/mpl/and.hpp>
	#include <boost/mpl/at.hpp>
#include <boost/mpl/assert.hpp>

#include <boost/mpl/joint_view.hpp>

#include <boost/mpl/greater.hpp>

#include <boost/bind.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/shared_ptr.hpp>

#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/mpl/set.hpp>
#include <boost/mpl/copy.hpp>
#include <boost/mpl/copy_if.hpp>

#include <vector>

#include <boost/mpl/vector_c.hpp>

#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/not_equal_to.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/count_if.hpp>
#include <boost/mpl/range_c.hpp>


namespace boost
{

namespace bag
{

namespace _mpl_ = boost::mpl;

// custom item filter for filtering pointer
template <class internal_t, class external_t>
struct ptr_filter
{	
	typedef ptr_filter  type;
	typedef external_t& result_type ;

	external_t & operator()( internal_t & v) const
	{	
		return (*v);
	}

	external_t & internal_to_external( internal_t & v) const
	{	
		return (*v);
	}
	internal_t external_to_internal( external_t &v) const
	{	
		return (&v);
	}
};

// Static item filter for custom items
template <class custom_item_t>
struct custom_item_filter
{
	typedef typename custom_item_t::impl_type	internal_t;
	typedef typename custom_item_t::arg_type	external_t;
	
	struct filter : custom_item_t
	{	
		typedef filter  type;
		typedef typename custom_item_t::return_type result_type;
		
		result_type & operator()( internal_t v) const
		{	
			return custom_item_t::impl_to_val(v);
		}

		external_t & internal_to_external( internal_t & v) const
		{	
			return custom_item_t::impl_to_val(v);
		}
		internal_t external_to_internal( external_t &v) const
		{	
			return custom_item_t::val_to_impl(v);
		}
	};
};
	
// vector style container
// holds pointers to objects of type T, but provides an interface as if by reference\value
// does notthing more than re-direct the interface
// i.e. items held by ptr- are NOT freed
//
template <class internal_t,class external_t, class arg_t,  class filter_t >
class filtered_vector : filter_t
{
	typedef std::vector<internal_t> vector_base_type;
	typedef filter_t filter_type;
	
	public:
		typedef vector_base_type		base_type;

		typedef boost::transform_iterator< filter_type ,typename vector_base_type::iterator > trans_iter_type;
		
		struct iterator : trans_iter_type
		{
			typedef iterator type;
			iterator()
			: trans_iter_type(typename vector_base_type::iterator() )
			{}
			iterator(typename vector_base_type::iterator it)
			: trans_iter_type(it )
			{}
			
		};
		typedef filtered_vector							type;
		typedef typename vector_base_type::allocator_type	allocator_type;
		typedef external_t								value_type;
		typedef typename vector_base_type::size_type			size_type;
	private:
		
		vector_base_type  m_vec;

	public:
		base_type & base()	// todo- implement base() function in multiple<by_val>
		{
			return m_vec;
		}
		void clear()
		{
			m_vec.clear();
		}
		void push_front(arg_t obj)
		{
			m_vec.push_front( filter_type::external_to_internal( obj ) );
		}
		void push_back(arg_t obj)
		{
			m_vec.push_back(filter_type::external_to_internal( obj )  );
		}
		void insert( iterator where, value_type& obj)
		{		
			m_vec.insert(where.base(),filter_type::external_to_internal( obj )  );
		}
		void insert( iterator where, size_type count, value_type& obj)
		{
			m_vec.insert(where.base(),count, filter_type::external_to_internal( obj )  );
		}
		value_type & at(size_type pos)
		{
			return filter_type::internal_to_external( m_vec.at(pos) );
		}
		value_type & operator[]( size_type pos)
		{
			return at(pos);
		}
		size_type capacity() const
		{
			return m_vec.capacity();
		}
		size_type size() 
		{
			return m_vec.size();
		}
		iterator erase( iterator where)
		{
			return iterator( m_vec.erase( where.base() ) );
		}	
		iterator begin()
		{
			return iterator(m_vec.begin());
		}                                                                   
	
		iterator end()
		{
			return iterator(m_vec.end());
		}                                                                   
};    

	template<typename T>
	struct shared_ptr_to_ref
	{	
		typedef shared_ptr_to_ref		type;
		typedef T&						result_type ;

		result_type operator()( boost::shared_ptr<T>& v) const
		{	
			return (*v);
		}
	};
  
// vector style container
// takes shared_ptr on push_front,push_back, & insert
// returns the dereferenced version of the shared_ptr on access (i.e. at() operator[] and via iterators)
template <class T>
class shared_ptr_vector
{
		typedef std::vector<boost::shared_ptr<T> > vector_ptrs;
	public:
		typedef shared_ptr_vector					 type;
		typedef typename vector_ptrs::allocator_type allocator_type;
		typedef boost::shared_ptr<T>				value_type;
		typedef T									return_type;
		
		typedef typename vector_ptrs::size_type size_type;

		struct iterator : boost::transform_iterator<shared_ptr_to_ref<T> ,typename vector_ptrs::iterator >
		{
			iterator()
			: boost::transform_iterator<shared_ptr_to_ref<T> ,typename vector_ptrs::iterator >(typename  vector_ptrs::iterator() )
			{}
			iterator(typename vector_ptrs::iterator it)
			: boost::transform_iterator<shared_ptr_to_ref<T> ,typename vector_ptrs::iterator >(it )
			{}
			
		};
	private:
		
		vector_ptrs  m_vec;

	public:
		void clear()
		{
			m_vec.clear();
		}
		void push_front(value_type  obj)
		{
			m_vec.push_back(obj);
		}
	
		void push_back(value_type  obj)
		{
			m_vec.push_back(obj);
		}
		void insert( iterator where, value_type  obj )
		{
			m_vec.insert(where.base(),obj);
		}
		void insert( iterator where, size_type count, value_type& obj)
		{
			m_vec.insert(where.base(),count, obj);
		}
		return_type & at(size_type pos)
		{
			return *(m_vec.at(pos));
		}
		return_type & operator[]( size_type pos)
		{
			return *(m_vec[pos]);
		}
		size_type capacity() const
		{
			return m_vec.capacity();
		}
		size_type size() 
		{
			return m_vec.size();
		}
		iterator erase( iterator where)
		{
			return iterator( m_vec.erase( where.base() ) );
		}	
		iterator begin()
		{
			return iterator(m_vec.begin());
		}                                                                   
	
		iterator end()
		{
			return iterator(m_vec.end());
		}                                                                   
};    

////////////////////////////////////////////////////////////////
// concepts
////////////////////////////////////////////////////////////////

// declarative concepts
// simliar in principle to aspects, but turned inside out
// instead of applying the aspect to an object and not touching the object
// you place metadata about the class into the class itself
// you can then filter on these concepts
// it is "aspects" without the weave

namespace detail
{
	BOOST_MPL_HAS_XXX_TRAIT_NAMED_DEF(defines_concepts_impl, concepts, false)
};


// returns true if the passed type has a "concepts" defined as a nested name
// will return false is the passed type does not define a concepts typedef
template <  class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct defines_concepts;

template <>
struct defines_concepts<>
{
	template <class T, class Enable = void> 
	struct apply : _mpl_::false_
	{};

	template <class T>
	struct apply<T, typename boost::enable_if< detail::defines_concepts_impl<T>  >::type> 
	:	_mpl_::true_
	{};
};

template < class T> 
struct defines_concepts<T>
: _mpl_::apply1<defines_concepts <>, T>::type 
{
};

// returns true if the passed type "has" a given type in it's concept list
// will return false if either the type in question does NOT have the target concept in the concepts list
//		OR if the type in question does not have concepts at all
template < class C0  = _mpl_::void_, class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct has_concept;

template < class C0> 
struct has_concept<C0>
{
	template <class T, class Enable = void> 
	struct apply : _mpl_::false_
	{};

	template <class T>
	struct apply<T, typename boost::enable_if< defines_concepts<T>  >::type> 
	:	_mpl_::if_
		<
			_mpl_::contains
			< 
				typename T::concepts,
				C0
			>,
			_mpl_::true_,
			_mpl_::false_
		>
	{ };
	typedef has_concept type;

};

template < class C0, class T> 
struct has_concept<C0,T>
: _mpl_::apply1<has_concept <C0>, T>::type 
{
};

// returns false if the passed type "has" a given type in it's concept list
// will return true if either the type in question does NOT have the target concept in the concepts list
//		OR if the type in question does not have concepts at all
template < class C0  = _mpl_::void_, class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct not_has_concept;

template < class C0> 
struct not_has_concept<C0>
{
	template <class T, class Enable = void> 
	struct apply : _mpl_::true_
	{};

	template <class T>
	struct apply<T, typename boost::enable_if< defines_concepts<T>  >::type> 
	:	_mpl_::if_
		<
			_mpl_::contains
			< 
				typename T::concepts,
				C0
			>,
			_mpl_::false_,
			_mpl_::true_
		>
	{ };
	typedef not_has_concept type;

};

template < class C0, class T> 
struct not_has_concept<C0,T>
: _mpl_::apply1<not_has_concept <C0>, T>::type 
{
};

// has_any_concept<> which will give a match if any of the concepts match - will take a sequence
// returns true if the passed type "has" a given type in it's concept list
// will return false if either the type in question does NOT have the target concept in the concepts list
//		OR if the type in question does not have concepts at all
template < class SEQ0  = _mpl_::void_, class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct has_any_concept;

template < class SEQ0> 
struct has_any_concept<SEQ0>
{
	template <class T, class Enable = void> 
	struct apply : _mpl_::false_
	{};

	template <class T>
	struct apply<T, typename boost::enable_if< defines_concepts<T>  >::type> 
	:	_mpl_::if_
		<
			_mpl_::greater
			< 
				_mpl_::filter_view
				< 
					typename T::concepts ,
					_mpl_::contains<SEQ0, _mpl_::_1 >
				>, 
				_mpl_::int_<0> 
			>, 
			_mpl_::true_,
			_mpl_::false_
		>
	{ };
	typedef has_any_concept type;
};

template < class SEQ0, class T> 
struct has_any_concept<SEQ0,T>
: _mpl_::apply1<has_any_concept <SEQ0>, T>::type 
{
};

// has_any_concept<> which will give a match if any of the concepts match - will take a sequence
// returns false if the passed type "has" a given type in it's concept list
// will return true if either the type in question does NOT have the target concept in the concepts list
//		OR if the type in question does not have concepts at all
template < class SEQ0  = _mpl_::void_, class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct not_has_any_concept;

template < class SEQ0> 
struct not_has_any_concept<SEQ0>
{
	template <class T, class Enable = void> 
	struct apply : _mpl_::true_
	{};

	template <class T>
	struct apply<T, typename boost::enable_if< defines_concepts<T>  >::type> 
	:	_mpl_::if_
		<
			_mpl_::greater
			< 
				_mpl_::filter_view
				< 
					typename T::concepts ,
					_mpl_::contains<SEQ0, _mpl_::_1 >
				>, 
				_mpl_::int_<0> 
			>, 
			_mpl_::false_,
			_mpl_::true_
		>
	{ };
	typedef not_has_any_concept type;
};

template < class SEQ0, class T> 
struct not_has_any_concept<SEQ0,T>
: _mpl_::apply1<not_has_any_concept <SEQ0>, T>::type 
{
};


// returns the concepts associated with a given type
// will return an empty vector<> if the type does not have a concept
template < class C0  = _mpl_::void_, class T0  = _mpl_::void_,class XX  = _mpl_::void_>
struct get_concepts;

template < > 
struct get_concepts<>
{
	// T does not have an existing concepts
	template <class target_t, class Enable = void> 
	struct apply : _mpl_::vector0<>
	{};

	template <class target_t>
	struct apply<target_t, typename boost::enable_if< defines_concepts<target_t>  >::type> 
	:   target_t::concepts
	{ 
		
	};
};

template < class target_t> 
struct get_concepts<target_t>
: _mpl_::apply1<get_concepts<>, target_t>::type 
{
};

////////////////////////////////////////////////////////////////
// concepts defined
////////////////////////////////////////////////////////////////
// concepts
struct modifier {};
struct local_modifier {};
struct global_modifier {};

////////////////////////////////////////////////////////////////
// target
////////////////////////////////////////////////////////////////
namespace detail
{
	BOOST_MPL_HAS_XXX_TRAIT_NAMED_DEF(has_target_type_impl, target_type, false)
};
// default case returns the type
template <class data_t, class Enable = void> 
struct target
{
	typedef data_t	type;
};	

// if the type HAS a target type- then return it
template <class data_t>
struct target<data_t, typename boost::enable_if< detail::has_target_type_impl< data_t >  >::type> 
{  
	typedef typename data_t::target_type	type;
};

////////////////////////////////////////////////////////////////
// key
////////////////////////////////////////////////////////////////

namespace detail
{
	// returns true if the target defines a "key_type" typedef
	BOOST_MPL_HAS_XXX_TRAIT_NAMED_DEF(has_key_type_impl, key_type, false)
};


// returns either the "key_type" typedef in the passed type
// or returns the type itself if the key_type is not present
// will always return a non-reference type
template <class data_t, class Enable = void> 
struct key
{
	typedef typename boost::remove_reference<data_t>::type	type;
};	
template <class data_t>
struct key<data_t, typename boost::enable_if< detail::has_key_type_impl< data_t >  >::type> 
{  
	typedef typename boost::remove_reference<typename data_t::key_type>::type	type;
};

// mfc for returning the key of a type using the key<> template
struct key_of
{
	template <class data_t>
	struct apply
	{
		typedef typename key<data_t>::type	type;
	};
};


// returns the index of the passed type
template <class data_t>
struct index_
{
	typedef typename data_t::type::idx_type	type;
};	
// returns the instance of the passed type
template <class data_t>
struct instance
{
	typedef typename data_t::instance_type	type;
};	

// returns the implementation of the passed type
template <class data_t>
struct impl
{
	typedef typename data_t::impl_type	type;
};	

// returns the storage of the passed type
template <class data_t>
struct storage
{
	typedef typename data_t::impl_type::storage_type	type;
};	

// mfc returns the arg type from a data<> class
struct arg_of
{
	template <class data_t>
	struct apply
	{
		typedef typename impl<data_t>::type::arg_type	type;
	};
};

// mfc for returning the index
struct index_of
{
	template <class data_t>
	struct apply
	{
		typedef typename index_<data_t>::type	type;
	};
};
// mfc for returning the implementation
struct impl_of
{
	template <class data_t>
	struct apply
	{
		typedef typename impl<data_t>::type	type;
	};
};
// mfc for returning the instance
struct instance_of
{
	template <class data_t>
	struct apply
	{
		typedef typename instance<data_t>::type	type;
	};
};
// mfc for returning the storage of a type
struct storage_of
{
	template <class data_t>
	struct apply
	{
		typedef typename storage<data_t>::type	type;
	};
};
// where how_t is a unary metafunction class and arg_t is a param to that mfc
// returns true if the value_t passed type passes the evaulation of how_t
// the internal apply allows for passing does_match to a algorithm intended to iterate or filter
// a compile time sequence
template <class how_t, class value_t>
struct does_match
{
	template <class arg_t>
	struct apply
	: boost::is_same< typename _mpl_::apply1< how_t,arg_t>::type , value_t> 
	{	};

	typedef does_match type;
};

// locates a class in a sequence
// seq_t is the sequence to search
// how_t is a metafunction class for checking
// value_t is the value that must be equal to the results of the applied how_t
template <class seq_t, class how_t, class value_t>
struct locate
:	_mpl_::identity
	<
		typename _mpl_::deref
		<
			typename _mpl_::find_if
			<
				seq_t, 
				does_match<how_t,value_t>
			>::type 
		>::type
	>
{};	
// mfc
// given data_arg_t, will pull the key from data_arg_t
// and will pass the resulting key to the mfc supplied as exp_t
template <class exp_t>
struct eval_expression_on_key 
{
	template <class data_arg_t>
	struct apply
	: _mpl_::apply1<exp_t, typename _mpl_::apply1< key_of,data_arg_t>::type >::type
	{
	};
	typedef eval_expression_on_key  type;
};

// will return true if the how_t and value_t result in a match 
// against the passed type data_arg_t in the internal apply nested template
template <class how_t, class value_t>
struct does_contain
{
	template <class data_arg_t>	// element in sequence
	struct apply
	: _mpl_::contains< data_arg_t, typename _mpl_::apply1< how_t,value_t>::type > 
	{	};
};
////////////////////////////////////////////////////////////////
// formal_item
////////////////////////////////////////////////////////////////

// base class for formal items
struct formal_item	
{
	typedef _mpl_::vector1< formal_item> concepts;
};

// returns the nested item<> template with the passed base_t class applied from target_t
template <class base_t, class target_t>
struct apply_item_base
{
	typedef typename target_t:: template item<base_t> type;
};

// returns true if T is a formal_item derived class
template <class T>
struct is_formal_item : boost::is_base_and_derived<formal_item, typename key< typename boost::remove_reference<T>::type >::type > {};

struct sample_base {};

// if target_t is a formal item, will apply base_t to the nested item<> template and return the resulting type
// if target_t is NOT a formal item will return the target<> of target_t
// if the target_t IS a formal item and is passed as a reference, 
//		the reference is removed, 
//		the nested item<> has base_t applied
//		the return is then the nested item<> with base applied returned as a reference
template <class base_t, class target_t, class Enable = void> 
struct if_formal_apply_base  {  };

template <class base_t, class target_t>
struct if_formal_apply_base<base_t, target_t, typename boost::enable_if<is_formal_item<target_t>  >::type> 
{ 
	typedef typename boost::remove_reference<target_t>::type target_type;
	typedef typename target_type:: template item<base_t > item_type;
	// if is reference on the original target- re-add it back to the output
	typedef typename _mpl_::if_< boost::is_reference<target_t>, item_type&, item_type>::type type;
};

template <class base_t, class target_t>
struct if_formal_apply_base<base_t, target_t, typename boost::enable_if<_mpl_::not_< is_formal_item<target_t> >  >::type> 
{ 
		typedef typename target<target_t>::type type;
};

//////////////////////////////////////////
// Global Traits
namespace detail
{
	struct any_instance	{};	// concept for "any" instance
};

// defines "by value" storage
struct by_val
{
	typedef detail::any_instance		instance_concept;
	typedef by_val						storage_concept;
	typedef by_val						type;
};

// defines "by reference" storage
struct by_ref
{
	typedef detail::any_instance		instance_concept;
	typedef by_ref						storage_concept;
	typedef by_ref						type;
};

// defines "by any" storage
struct by_any
{
	typedef detail::any_instance		instance_concept;
	typedef by_any						storage_concept;
	typedef by_any						type;
};

// defines "by auto ref " storage
struct by_auto_ref
{
	typedef detail::any_instance		instance_concept;
	typedef by_auto_ref					storage_concept;
	typedef by_auto_ref					type;
};
// defines "by auto ref " storage
struct by_shared_ref
{
	typedef detail::any_instance			instance_concept;
	typedef by_shared_ref					storage_concept;
	typedef by_shared_ref					type;
};

struct removable_items	{	typedef removable_items type; }; // concept that specifies the target has removable items
//////////////////////////////////////////
// Custom item support
struct custom_item
{
	typedef custom_item type;
};

template <class T>
struct is_custom_item : boost::is_base_and_derived<custom_item, T> {};

	// simple base exists to support is_shared_ref<>
	struct shared_ref_base	{};

template <class T>
struct is_shared_ref : boost::is_base_and_derived<shared_ref_base, T> {};

//////////////////////////////////////////
// custom item implementation
template <class T>
struct shared_ref : custom_item , shared_ref_base
{
	BOOST_MPL_ASSERT(( _mpl_::not_< boost::is_reference<T> > )); // reference type keys are NOT allowed with auto_ref!!!!!!

	typedef shared_ref				type;		// our type - defined for _mpl_ compatiblity
	typedef T						key_type;	// type used for item lookup
	typedef boost::shared_ptr<T>	impl_type;	// type used for implementing storage of the item
	typedef boost::shared_ptr<T> 	arg_type;	// type used for receiving a value to store in the item
	typedef T						return_type;// type used to return a dereferenece impl_type  
	typedef T						value_type; // type used to return a dereferenece impl_type  

	typedef by_shared_ref			instance_concept;
	typedef _mpl_::vector< custom_item, by_shared_ref> concepts;

	// policy function for taking the impl object and returning the value
	// bag will never call this function if is_val_empty() returns true
	return_type & impl_to_val(impl_type & obj) const
	{
		assert(obj.get() != 0); // todo throw 
		return *(obj.get());
	}
	// policy function for converting a value to an implementation
	impl_type val_to_impl(arg_type & obj) const
	{
		return impl_type(obj);
	}
	// policy function for checking the impl object for being empty
	bool is_val_empty(impl_type & obj)
	{
		return (obj.get() == 0);
	}
};

	// simple base exists to support is_auto_ref<>
	struct auto_ref_base	{};

// returns true if T is an auto_ref<>
template <class T>
struct is_auto_ref : boost::is_base_and_derived<auto_ref_base, T> {};

template <class T>
struct auto_ref : custom_item, auto_ref_base
{
	BOOST_MPL_ASSERT(( _mpl_::not_< boost::is_reference<T> > )); // reference type keys are NOT allowed with auto_ref!!!!!!

	typedef auto_ref				type;		// our type - defined for mpl compatiblity
	typedef T						key_type;	// type used for item lookup
	typedef std::auto_ptr<T>		impl_type;	// type used for implementing storage of the item
	typedef T *						arg_type;	// type used for receiving a value to store in the item
	typedef T						return_type;// type used to return a dereferenece impl_type  
	typedef T						value_type; // type used to return a dereferenece impl_type  

	typedef by_auto_ref			instance_concept;
	typedef _mpl_::vector< custom_item, by_auto_ref> concepts;
	
	typedef std::auto_ptr<T>		single_storage_type;
	typedef boost::ptr_vector<T>	multiple_storage_type;
	
	// todo- do we need to define set and insert methods?

	// policy function for taking the impl object and returning the value
	// bag will never call this function if is_val_empty() returns true
	return_type & impl_to_val(impl_type & obj)
	{
		assert(obj.get() != 0);
		return *(obj.get());
	}
	// policy function for converting a value to an implementation
	impl_type val_to_impl(arg_type & obj) // const
	{
		return impl_type(obj);
	}
	// policy function for converting a value to an implementation (for multiple)
	arg_type & multiple_val_to_impl(arg_type & obj) // const
	{
		return obj;
	}
	// policy function for checking the impl object for being empty
	bool is_val_empty(impl_type & obj)
	{
		return (obj.get() == 0);
	}
};

	


///////////////////////////////////////
// SINGLE
///////////////////////////////////////

namespace detail
{
	struct bag_impl_base
	{};
};

template <class T>
struct is_bag : boost::is_base_and_derived<detail::bag_impl_base, typename boost::remove_reference<T>::type > {};

//////////////////////////////////////////////
// Internal functors for visiting our target
// special handling for embedded bags and re-passing the original
// meta filter to the embedded bag 

// visit a non-bag
template <class orig_arg_t,class Tar, class visitEnable = void> 
struct visit_impl 
{ 
	template <class functor_t, class value_t >
	visit_impl(functor_t & functor, value_t & val)
	{
		// value is not a bag- so just pass it to the functor
		functor(val);
	}
};
// todo- orig_arg is not a good name- it NOW is the orig filter_exp
// visit a bag
template <class orig_arg_t,class Tar>
struct visit_impl<orig_arg_t, Tar, typename boost::enable_if< is_bag<Tar> >::type> 
{  
	// visit with filter
	template <class orig_arg_t2, class visitEnable2 = void> 
	struct visit_bag_impl
	{
		template <class functor_t, class value_t >
		visit_bag_impl(functor_t & functor, value_t & val)
		{
			// value is a bag- so call for_each
			val.template for_each_raw_filtered_visit_each<orig_arg_t2,functor_t>(functor);
		}
	};
	// visit without filter
	template <class orig_arg_t2 > 
	struct visit_bag_impl<orig_arg_t2, typename boost::enable_if<boost::is_same<orig_arg_t2,_mpl_::void_> >::type > 
	{
		template <class functor_t, class value_t >
		visit_bag_impl(functor_t & functor, value_t & val)
		{
			// value is a bag- so call for_each
			val.for_each(functor);
		}
	};
	template <class functor_t, class value_t >
	visit_impl(functor_t & functor, value_t & val)
	{
		// pick the right way to visit the target
		visit_bag_impl<orig_arg_t>(functor, val);
	}
};

// functor for visiting a target
// will correctly visit the elements based on their
// type and strategy
template <class orig_arg_t, class key_t, class functor_t2>
struct visit_target
{
	functor_t2 & mFunc;
	visit_target(functor_t2 & func)
	: mFunc(func) {};

	template <class value_t>
	void operator()( value_t & val)
	{
		// select correct way to visit based on key type
		visit_impl<orig_arg_t, key_t>(mFunc,val);
	}
};

// exception for uninitialized item access
// this is generally only thrown when 
// an attempt has been made to access a non by_val item
// and the item is uninitialized
struct uninitialized_item_access : std::exception
{
	virtual char const * what() const throw()
	{
		return "boost::bag::uninitialized_item_access";
	}
};


// todo- a way to check for empty on the item

/////////////////////////////////////
// local modifiers

// placeholder so bag::items::strategy::dimension has a simple class to return
struct single_
{	
	typedef single_ type;
};

	
// Single - by_val
template <class target_t, class Enable = void> 
struct single
{
	typedef _mpl_::vector< detail::any_instance, single_, single<by_val>, single<by_any>, by_any, by_val, modifier, local_modifier> concepts;
	
	typedef single		type;
	typedef target_t	key_type;
	typedef target_t	value_type;
	typedef target_t	arg_type;
	typedef target_t	target_type; // original value passed in 
	
	private:
		key_type			m_rawvalue;
	public:
		single()
		{}
		void set(arg_type & obj)
		{
			// by_val so we set by val
			// todo test for this
			m_rawvalue = obj;
		}
		void insert(arg_type & obj)
		{
			set(obj);
		}
		// returns the value as a ref
		value_type & val_as_ref()
		{
			return m_rawvalue;
		}
		// single-by_val tests for equal by value
		bool is_equal(arg_type & obj)
		{
			return m_rawvalue == obj;
		}
	
	public:

		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & f)
		{									  
			// select correct way to visit based on key type
			visit_impl<orig_arg_t, key_type>(f,val_as_ref());
		}

		// apply functions
		// techincally these are "re-apply" functions
		// they allow the caller- to re-apply the target from this type
		// if the target is a &, will sense this and
		//		strip the reference from the key type
		//		force the storage type to be "by_ref"
		template <class new_target_t, class _Enable = void> 
		struct apply
		{
			typedef single<new_target_t > type;
		};

		template< class new_target_t>
		struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
		{
			typedef single<new_target_t& > type;
		};
};

// Single - by_ref
template< class target_t>
struct single< target_t, typename boost::enable_if<boost::is_reference<target_t>  >::type> 
{
	typedef _mpl_::vector< detail::any_instance, single_, single<by_ref>, single<by_any>, by_any, by_ref, modifier, local_modifier, removable_items> concepts;

	typedef single		type;
	typedef typename	boost::remove_reference<target_t>::type	key_type;
	typedef key_type	value_type;
	typedef target_t	arg_type;
	typedef target_t	target_type; // original value passed in 

	private:
		value_type			* m_rawvalue;
	public:
		single()
		: m_rawvalue(0)
		{}
		void set(arg_type  obj)
		{
			// by_ref so we take the address
			m_rawvalue = &obj;
		}
		void insert(arg_type obj)
		{
			set(obj);
		}
		// single-by_val tests for equal by ptr address
		bool is_equal(arg_type  obj)
		{
			return m_rawvalue == &obj;
		}
		// returns the value as a ref
		value_type & val_as_ref()
		{
			if (m_rawvalue == 0)
				throw uninitialized_item_access();

			return *m_rawvalue;
		}
		
		template <class orig_arg_t, class functor_t>
		void visit_each(functor_t & functor)
		{									  
			if (m_rawvalue != 0)
			{
				visit_impl<orig_arg_t, key_type>(functor,val_as_ref());
			}
		}
		template <class T>
		void remove(T&)
		{
			m_rawvalue = 0;	
		}
		template <class new_target_t, class _Enable = void> 
		struct apply
		{
			typedef single<new_target_t > type;
		};
		template< class new_target_t>
		struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
		{
			typedef single<new_target_t& > type;
		};
};

// single - custom_item
template< class target_t>
struct single< target_t, typename boost::enable_if<is_custom_item<target_t>  >::type> : target_t
{
	typedef typename target_t::instance_concept::type instance_concept_t; // instance concept pulled from the target
	
	typedef _mpl_::vector< detail::any_instance, single_, single<instance_concept_t>, single<by_any>, by_any, instance_concept_t,  modifier, local_modifier> concepts;
	
	typedef single							type;		// type defined for mpl compatiblity
	typedef typename target_t::key_type		key_type;	// type used for item lookup
	typedef typename target_t::impl_type	impl_type;	// type used for implementing storage of the item
	typedef typename target_t::arg_type		arg_type;	// type used for receiving a value to store in the item
	typedef typename target_t::return_type	return_type;// type used to return a dereferenece impl_type  

	private:
		impl_type			m_rawvalue;
	public:
		single()
		{}
		void set(arg_type & obj)
		{
			// by_val so we set by val
			// todo test for this
			m_rawvalue = target_t::val_to_impl(obj);
			assert(!is_empty());
		}
		void insert(arg_type & obj)
		{
			set(obj);
		}
		// returns the value as a ref
		return_type & val_as_ref()
		{
			if (is_empty())
				throw uninitialized_item_access();

			return target_t::impl_to_val(m_rawvalue);
		}
	
		bool is_empty()
		{
			return target_t::is_val_empty(m_rawvalue);
		}
		// single-by_val tests for equal by value
		bool is_equal(arg_type & obj)
		{
			assert(!is_empty());
			return m_rawvalue == obj;
		}
	
	public:

		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & f)
		{	
			// if we are not empty, then visitation is allowed								
			if (!is_empty())
			{
				// select correct way to visit based on key type
				visit_impl<orig_arg_t, key_type>(f,val_as_ref());
			}
		}

		// apply functions
		// techincally these are "re-apply" functions
		// they allow the caller- to re-apply the target from this type
		// if the target is a &, will sense this and
		//		strip the reference from the key type
		//		force the storage type to be "by_ref"
		template <class new_target_t, class _Enable = void> 
		struct apply
		{
			typedef single<new_target_t > type;
		};

		template< class new_target_t>
		struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
		{
			typedef single<new_target_t& > type;
		};
};

// global modifier - by_val
template <>
struct single<by_val>	
{
	typedef single type;
	typedef by_val			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef single<new_target_t > type;
	};
	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
	{
		typedef single<new_target_t& > type;
	};
};

// global modifier - by_ref
template <>
struct single<by_ref>
{
	typedef single type;
	typedef by_ref			storage_concept;
    
	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef single<typename boost::remove_reference<new_target_t>::type & > type;
	};
	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
	{
		typedef single<typename boost::remove_reference<new_target_t>::type & > type;
	};
	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<custom_item,new_target_t>  >::type> 
	{
		typedef single<new_target_t > type;
	};

};
// global modifier - by_ref
template <>
struct single<by_auto_ref>
{
	typedef single type;
	typedef by_auto_ref			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef single< auto_ref<new_target_t> > type;
	};

};
// global modifier - by_shared_ref
template <>
struct single<by_shared_ref>
{
	typedef single type;
	typedef by_shared_ref			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef single< shared_ref<new_target_t> > type;
	};
};

// filter expression ONLY
template <>
struct single<by_any>
{
	typedef single type;
	typedef by_any			storage_concept;
};


///////////////////////////////////////
// multiple
///////////////////////////////////////

// placeholder so bag::items::strategy::dimension has a simple class to return
struct multiple_
{	
	typedef multiple_ type;
};

// multiple- by_val
template <class target_t, class Enable = void> 
struct multiple
{
	typedef multiple				type;
	typedef target_t				key_type;
	typedef std::vector<target_t>	value_type;
	typedef target_t				arg_type;
	typedef target_t				target_type; // original value passed in 

	typedef _mpl_::vector< detail::any_instance, multiple_, multiple<by_val>, multiple<by_any>, by_any, by_val, modifier, local_modifier, removable_items> concepts;
		
	private:
		std::vector<key_type>			m_rawvalue;
	public:
		multiple()
		{}
		void set(arg_type & obj)
		{
			// by_val so we set by val
			m_rawvalue.clear();
			m_rawvalue.push_back(obj);
		}
		void insert(arg_type & obj)
		{
			m_rawvalue.push_back(obj);
		}

		template <class T>
		void push_back(T 	val)
		{
			m_rawvalue.push_back(val);
		}

		// multiple-by_val tests for equal by value
		bool is_equal(arg_type & obj)
		{
			using namespace boost;
			typename std::vector<key_type >::iterator  itFound;
			itFound = std::find_if(m_rawvalue.begin(),m_rawvalue.end(),
								bind(std::equal_to<key_type>(), _1, obj ) );	
			if (*itFound == obj)
				return true;
			return false;
		}
		
		void remove(key_type 	pvalue)
		{
			using namespace boost;
			typename std::vector<key_type >::iterator  itFound;
			itFound = std::find_if(m_rawvalue.begin(),m_rawvalue.end(),
								bind(std::equal_to<key_type>(), _1, pvalue ) );	
			// if we found m_pCurrent in the children of m_pParent, then we are valid
			if (*itFound == pvalue)
				m_rawvalue.erase(itFound);
		}	

		// returns the value as a ref
		value_type & val_as_ref()
		{
			return m_rawvalue;
		}
		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & functor)
		{									  
			std::for_each(m_rawvalue.begin(), m_rawvalue.end(), visit_target<orig_arg_t,key_type,fctor_t>(functor) );
		}

        // apply functions
        // techincally these are "re-apply" functions
        // they allow the caller- to re-apply the target from this type
        // if the target is a &, will sense this and
        //		strip the reference from the key type
        //		force the storage type to be "by_ref"
        template <class new_target_t, class _Enable = void> 
        struct apply
        {
            typedef multiple<new_target_t > type;
        };

        template< class new_target_t>
        struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
        {
            typedef multiple<new_target_t& > type;
        };
};

// multiple- by_ref
template< class target_t>
struct multiple< target_t, typename boost::enable_if<boost::is_reference<target_t>  >::type> 
{
	typedef multiple											type;
	typedef typename boost::remove_reference<target_t>::type	key_type;
	typedef filtered_vector
			<
				key_type*, 
				key_type, 
				key_type&, 
				ptr_filter<key_type *, key_type> 
			>													container_type;

	typedef container_type										value_type;
	typedef target_t											arg_type;
	typedef target_t											target_type; // original value passed in 
	
	typedef _mpl_::vector< detail::any_instance, multiple_, multiple<by_ref>, multiple<by_any>, by_any, by_ref, modifier, local_modifier, removable_items> concepts;
		
	private:
		container_type					m_rawvalue;
	public:
		multiple()
		{}
		void set(arg_type obj)
		{
			m_rawvalue.clear();
			m_rawvalue.push_back(obj);
		}
		void insert(arg_type obj)
		{
			m_rawvalue.push_back(obj);
		}

		template <class T>
		void push_back(T 	val)
		{
			m_rawvalue.push_back(val);
		}
		
		// multiple-by_ref tests for equal by ptr
		bool is_equal(target_t  obj)
		{
			using namespace boost;
			typename container_type::base_type::iterator itFound;
			
			itFound = std::find_if(m_rawvalue.base().begin(),m_rawvalue.base().end(),
								bind(std::equal_to<key_type*>(), _1, &obj ) );	
			if (*itFound == &obj)
				return true;
			return false;
		}

		void remove(target_t value)
		{
			using namespace boost;
			typename container_type::base_type::iterator itFound;
			itFound = std::find_if(m_rawvalue.base().begin(),m_rawvalue.base().end(),
								bind(std::equal_to<key_type*>(), _1, &value ) );	
			// if we found m_pCurrent in the children of m_pParent, then we are valid
			if (*itFound == &value)
				m_rawvalue.base().erase(itFound);
		}	

		// returns the value as a ref
		value_type & val_as_ref()
		{
			return m_rawvalue;
		}
		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & functor)
		{									  
			std::for_each(m_rawvalue.begin(), m_rawvalue.end(), visit_target<orig_arg_t,key_type,fctor_t>(functor) );
		}
        // apply functions
        // techincally these are "re-apply" functions
        // they allow the caller- to re-apply the target from this type
        // if the target is a &, will sense this and
        //		strip the reference from the key type
        //		force the storage type to be "by_ref"
        template <class new_target_t, class _Enable = void> 
        struct apply
        {
            typedef multiple<new_target_t > type;
        };

        template< class new_target_t>
        struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
        {
            typedef multiple<new_target_t& > type;
        };
};



// multiple - auto_ref
template< class target_t>
struct multiple< target_t, typename boost::enable_if<is_auto_ref<target_t>  >::type> : target_t
{
	typedef _mpl_::vector< detail::any_instance, multiple_, multiple<by_auto_ref>, multiple<by_any>, by_any, by_auto_ref, modifier, local_modifier> concepts;
	
	typedef multiple									type;			// type defined for mpl compatiblity
	typedef typename target_t::key_type					key_type;		// type used for item lookup
	typedef typename target_t::impl_type				impl_type;		// type used for implementing storage of the item
	typedef typename target_t::arg_type					arg_type;		// type used for receiving a value to store in the item
	typedef typename target_t::return_type				return_type;	// type used to return a dereferenece impl_type  
	typedef typename target_t::multiple_storage_type	storage_type;	// type for how to store multiples
	typedef typename target_t::multiple_storage_type	value_type;		// type for how to store multiples

	private:
		storage_type	m_rawvalue;
	public:
		multiple()
		{}
		void set(arg_type & obj)
		{
			m_rawvalue.clear();
			m_rawvalue.push_back( obj ); 
		}
		void insert(arg_type & obj)
		{
			m_rawvalue.push_back( obj );
		}
		
		template <class T>
		void push_back(T 	val)
		{
			insert(val);
		}

		// returns the value as a ref
		storage_type & val_as_ref()
		{
			return m_rawvalue;
		}
	
		bool is_empty()
		{
			return (m_rawvalue.size() ==0);
		}
		// single-by_val tests for equal by value
		bool is_equal(arg_type & obj)
		{
			assert(!is_empty());
			// todo- fix
			return false; // m_rawvalue == obj;
		}
	
	public:

		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & functor)
		{	
			// if we are not empty, then allow visitation								
			if (!is_empty())
			{
				std::for_each(m_rawvalue.begin(), m_rawvalue.end(), visit_target<orig_arg_t,key_type,fctor_t>(functor) );
			}
		}

		// apply functions
		// techincally these are "re-apply" functions
		// they allow the caller- to re-apply the target from this type
		// if the target is a &, will sense this and
		//		strip the reference from the key type
		//		force the storage type to be "by_ref"
		template <class new_target_t, class _Enable = void> 
		struct apply
		{
			typedef multiple<new_target_t > type;
		};

		template< class new_target_t>
		struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
		{
			typedef multiple<new_target_t& > type;
		};
};


// multiple - shared_ref
template< class target_t>
struct multiple< target_t, typename boost::enable_if<is_shared_ref<target_t>  >::type> : target_t
{
	typedef _mpl_::vector< detail::any_instance, multiple_, multiple<by_shared_ref>, multiple<by_any>, by_any, by_shared_ref, modifier, local_modifier> concepts;
	
	typedef multiple						type;			// type defined for mpl compatiblity
	typedef typename target_t::key_type		key_type;		// type used for item lookup
	typedef typename target_t::arg_type		arg_type;		// type used for receiving a value to store in the item
	typedef shared_ptr_vector<key_type	>	storage_type;	// type for how to store multiples
	typedef shared_ptr_vector<key_type	>	impl_type;		// type for how to store multiples
	typedef storage_type	value_type;
	
	private:
		storage_type	m_rawvalue;
	public:
		multiple()
		{}
		
		void set(arg_type obj)
		{
			m_rawvalue.clear();
			m_rawvalue.push_back( obj ); 
		}
		
		void insert(arg_type  obj)
		{
			m_rawvalue.push_back( obj );
		}
		
		template <class T>
		void push_back(T 	val)
		{
			insert(val);
		}

		// returns the value as a ref
		storage_type & val_as_ref()
		{
			return m_rawvalue;
		}
	
		bool is_empty()
		{
			return (m_rawvalue.size() ==0);
		}
		
		// single-by_val tests for equal by value
		bool is_equal(arg_type & obj)
		{
			assert(!is_empty());
			// todo- fix
			return false; // m_rawvalue == obj;
		}

	public:

		template <class orig_arg_t, class fctor_t>
		void visit_each(fctor_t & functor)
		{	
			// if we are not empty, then visitation is allowed								
			if (!is_empty())
			{
				typename storage_type::iterator it; 
				for( it =m_rawvalue.begin(); it !=m_rawvalue.end(); it++)
				{
					// select correct way to visit based on key type
					visit_impl<orig_arg_t, key_type>(functor,(*it)); 
				}
			}
		}

		// apply functions
		// techincally these are "re-apply" functions
		// they allow the caller- to re-apply the target from this type
		// if the target is a &, will sense this and
		//		strip the reference from the key type
		//		force the storage type to be "by_ref"
		template <class new_target_t, class _Enable = void> 
		struct apply
		{
			typedef multiple<new_target_t > type;
		};

		template< class new_target_t>
		struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
		{
			typedef multiple<new_target_t& > type;
		};
};


// global modifier - by_val
template <>
struct multiple<by_val>	
{
	typedef multiple type;
	typedef by_val			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef multiple<new_target_t > type;
	};
	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
	{
		typedef multiple<new_target_t& > type;
	};
};

// global modifier - by_ref
template <>
struct multiple<by_ref>
{
	typedef multiple type;

	typedef by_ref			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef multiple<typename boost::remove_reference<new_target_t>::type & > type;
	};

	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<by_ref,new_target_t>  >::type> 
	{
		typedef multiple<typename boost::remove_reference<new_target_t>::type & > type;
	};
	template< class new_target_t>
	struct apply< new_target_t, typename boost::enable_if< has_concept<custom_item,new_target_t>  >::type> 
	{
		typedef multiple<new_target_t > type;
	};


};
// global modifier - by_ref
template <>
struct multiple<by_auto_ref>
{
	typedef multiple type;
	typedef by_auto_ref			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef multiple< auto_ref<new_target_t> > type;
	};
};

// global modifier - by_shared_ref
template <>
struct multiple<by_shared_ref>
{
	typedef multiple type;
	typedef by_shared_ref			storage_concept;

	template <class new_target_t, class _Enable = void> 
	struct apply
	{
		typedef multiple< shared_ref<new_target_t> > type;
	};
};

// global modifier - by_ref
template <>
struct multiple<by_any>
{
	typedef multiple type;
	typedef by_any			storage_concept;
};

// ending element
struct end
{	
	typedef end type;
	
	// hack
	template <int irt_arg_count>
	struct get_arg_item_map
	{
		typedef typename mpl::vector0<>::type type;

		// repass the count
		enum { new_count = 0 };
	};

};

// namespace detail {

// takes passed sequence and turns it into a vector
// useful for getting around mpl views not being excactly the same as a vector<>
template <class seq_t>
struct make_vector 
{
	typedef typename 
	_mpl_::copy<	typename seq_t::type , _mpl_::back_inserter< _mpl_::vector0<> >	>::type 
	type;
	
//	BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
};

template <>
struct make_vector<end>
{
	typedef _mpl_::vector0<> type;
};

template <>
struct make_vector<_mpl_::void_>
{
	typedef mpl::vector0<> type;
};

// implementation for unique_all
template <class curr_iter_t, class end_pos , class prev_seq_t >
struct unique_all_impl
{
	// current iterator - dereferenced
	typedef typename _mpl_::deref<curr_iter_t>::type curr_deref_t;
	
	typedef typename _mpl_::eval_if
	< 
		// if the current pos is less than the end pos
		_mpl_::less< typename curr_iter_t::pos, end_pos >,
		unique_all_impl	// recurse into ourselves - with incremeneted iter
		< 
			typename _mpl_::next< curr_iter_t >::type,	// pass next iter
			end_pos,									// repass- endpos	
			typename _mpl_::eval_if	// if previous sequence contains current item,
			<
				_mpl_::contains<prev_seq_t, curr_deref_t >,
				prev_seq_t,		/// do not add
				// ELSE
				// add the current type to the previous sequence
				typename _mpl_::push_back< prev_seq_t, curr_deref_t >::type  // otherwise add
			>::type
		>,
		// all done recursing, so return the previous sequence
		prev_seq_t	
	>::type type;
};

// }; // namespace detail

#ifdef WORKS
// returns a form of passed sequence with ALL duplicate types removed
template <class seq_t >
struct unique_all
// : unique_all_impl< typename _mpl_::begin<seq_t>::type, _mpl_::long_<_mpl_::size<seq_t>::type::value> , typename _mpl_::clear<seq_t>::type >
: unique_all_impl< typename _mpl_::begin<typename make_vector< typename seq_t::type>::type >::type, _mpl_::long_<_mpl_::size<typename seq_t::type >::type::value> , _mpl_::vector0<> >
{
	BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
};
#endif

namespace detail
{
	BOOST_MPL_HAS_XXX_TRAIT_NAMED_DEF(defines_pos_impl, pos, false)
};

	// returns a form of passed sequence with ALL duplicate types removed
	// will work directly with iterators which support ::pos, if the iter 
	// does not support pos, will first copy to a vector (which has an iter which supports pos)
	//	(note this copy into a vector is what enables unique_all to work with views)

	// unique_all for views
	template <class seq_t, class Enable = void> 
	struct unique_all
	: unique_all_impl< typename _mpl_::begin<typename make_vector< typename seq_t::type>::type >::type, _mpl_::long_<_mpl_::size<typename seq_t::type >::type::value> , _mpl_::vector0<> >
	{
		BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
	};

	// unique_all for sequences which have iters which support ::pos (i.e. vector)
	template <class seq_t>
	struct unique_all<seq_t, typename boost::enable_if< detail::defines_pos_impl< typename _mpl_::begin<seq_t>::type >  >::type> 
	: unique_all_impl< typename _mpl_::begin< typename seq_t::type >::type, _mpl_::long_<_mpl_::size<typename seq_t::type >::type::value> , _mpl_::vector0<> >
	{
		BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
	};

	// implementation for at_pos
	// will recurse itself until it finds the position needed
	// will return the type found
	template <class curr_iter_t,  class find_pos_t >
	struct at_pos_impl
	{
		// current iterator - dereferenced
		typedef typename _mpl_::deref<curr_iter_t>::type curr_deref_t;
		
		typedef typename _mpl_::eval_if
		< 
			// if the current pos is less than the end pos
			_mpl_::less< typename curr_iter_t::pos, find_pos_t >,
			at_pos_impl	// recurse into ourselves - with incremeneted iter
			< 
				typename _mpl_::next< curr_iter_t >::type,	// pass next iter
				find_pos_t									// repass- find_pos	
			>,
			// all done recursing, so return type
			_mpl_::identity<curr_deref_t>
		>::type type;
	};

	// }; // namespace detail

	// returns the type at the passed position
	// will compile time ASSERT if the position passed is beyond the size 
	// of seq_t
	template <class seq_t, class request_pos_t >
	struct at_pos 
	 :	at_pos_impl 
		<
			typename _mpl_::begin<seq_t>::type,				// begin iter
			_mpl_::long_<request_pos_t::type::value>			// requested pos
		>
	{
		// ensure type is a sequence
		BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
		
		// ordinal passed must be within range of values in sequence
		BOOST_MPL_ASSERT((_mpl_::less<_mpl_::long_<request_pos_t::type::value>, _mpl_::long_<_mpl_::size<seq_t>::type::value>   > ));
	};

	template <class seq_t, int irequest_pos_t >
	struct at_pos_c 
	 :	at_pos_impl 
		<
			typename _mpl_::begin<seq_t>::type,				// begin iter
			_mpl_::long_<irequest_pos_t>					// requested pos
		>
	{
		// ensure type is a sequence
		BOOST_MPL_ASSERT((_mpl_::is_sequence<typename seq_t::type >));
		
		// ordinal passed must be within range of values in sequence
		BOOST_MPL_ASSERT((_mpl_::less<_mpl_::long_<irequest_pos_t>, _mpl_::long_<_mpl_::size<seq_t>::type::value>   > ));
	};

//////////////////////////////////////////
// is type an mpl integral int?
// 
// returns true if the passed type is an 
// integral int
// will correctly sense if the type is a mpl::int_<T> or an mpl::integral_c<int,T> 
template <class T, class dummy = void>
struct is_mpl_integral_int
: _mpl_::false_ {};

template <int T>
struct is_mpl_integral_int< _mpl_::int_<T> >
: _mpl_::true_ 
{};

template <int T>
struct is_mpl_integral_int< _mpl_::integral_c<int,T> >
: _mpl_::true_ 
{};

// not form (purposely copy-pasted- to reduce instantiations)
template <class T, class dummy = void>
struct not_is_mpl_integral_int
: _mpl_::true_ {};

template <int T>
struct not_is_mpl_integral_int< _mpl_::int_<T> >
: _mpl_::false_ 
{};

template <int T>
struct not_is_mpl_integral_int< _mpl_::integral_c<int,T> >
: _mpl_::false_ 
{};

// promotes passed type to integral<int,>
template <class T, class dummy = void>
struct promote_to_mpl_integral_int
{
	// not an integral int- just repass it
	typedef typename _mpl_::identity<T>::type type;
};

template <int T>
struct promote_to_mpl_integral_int< _mpl_::int_<T> >
: _mpl_::integral_c<int, T>
{};

template <int T>
struct promote_to_mpl_integral_int< _mpl_::integral_c<int,T> >
: _mpl_::integral_c<int,T> 
{};

// returns true if a sequence contains an mpl int<> or an mpl integral_c<int,>
template <class seq_t>
struct seq_contains_mpl_integral_int
: _mpl_::not_< boost::is_same< typename _mpl_::deref< typename _mpl_::find_if<seq_t, is_mpl_integral_int<_mpl_::_1> >::type >::type, _mpl_::void_> >
{}; 

// returns false if a sequence contains an mpl int<> or an mpl integral_c<int,>
template <class seq_t>
struct not_seq_contains_mpl_integral_int
: boost::is_same< typename _mpl_::deref< typename _mpl_::find_if<seq_t, is_mpl_integral_int<_mpl_::_1> >::type >::type, _mpl_::void_>
{}; 

// returns true if a sequence is completely filled with either int<> or integral_c<int,>
template <class seq_t>
struct seq_is_all_mpl_integral_int
:	_mpl_::equal_to
	< 
		_mpl_::count_if<seq_t, is_mpl_integral_int<_mpl_::_1> >,
		_mpl_::size<seq_t>
	>
{}; 

// returns false if a sequence is completely filled with either int<> or integral_c<int,>
template <class seq_t>
struct not_seq_is_all_mpl_integral_int
:	_mpl_::not_equal_to
	< 
		_mpl_::count_if<seq_t, is_mpl_integral_int<_mpl_::_1> >,
		_mpl_::size<seq_t>
	>
{}; 

// turns int<0> into integral<int,>
template <class seq_t>
struct normalized_ordinal_vector
{
	typedef typename make_vector< _mpl_::transform_view< _mpl_::filter_view< seq_t, is_mpl_integral_int<_mpl_::_1> >, promote_to_mpl_integral_int<_mpl_::_1> > >::type type;
};

// takes a sequence and splits it into one sequence of keys and another of ordinals
// ordinals may be expressed as mpl::int_<> or mpl::integral_c<int,>
template <class seq_t>
struct split_keys_and_orginals
{
	typedef split_keys_and_orginals type;
	
	BOOST_MPL_ASSERT((_mpl_::is_sequence<seq_t> ));

	// just the keys in seq_t
	typedef typename make_vector< _mpl_::filter_view< seq_t, not_is_mpl_integral_int<_mpl_::_1>  > >::type keys;

	// just the ordinals in seq_t
	typedef typename normalized_ordinal_vector<seq_t>::type ordinals;
};

// default case
template <class arg_t, class Enable = void>
struct generate_mfc_filter_from_sequence
{
	typedef _mpl_::void_ type;
};

//	only containing ordinals
template <class arg_t>
struct generate_mfc_filter_from_sequence
<
	arg_t, 
	typename boost::enable_if	// if ALL the items in the sequence are ints
	< 
		seq_is_all_mpl_integral_int<arg_t>  
	>::type 
>
{
	// normallize the ordinals
	typedef typename normalized_ordinal_vector<arg_t>::type normalized_ordinals;

	// return a filter expression which will check for a given type to be within one of the ordinals
	typedef  typename _mpl_::lambda< _mpl_::contains< normalized_ordinals  , index_<_mpl_::_1>   > >::type type;
};

// only containing keys - no mpl::ints<>
template <class arg_t>
struct generate_mfc_filter_from_sequence
<
	arg_t, 
	typename boost::enable_if	// if there are no ints or integral<int,>'s in the sequence
	< 
		not_seq_contains_mpl_integral_int<arg_t>  
	>::type 
>
{
	// return a filter expression which will check for a given type to be one of the keys in arg_t
	typedef typename _mpl_::lambda< _mpl_::contains< arg_t  , key<_mpl_::_1>   > >::type type;
};

//  containing both ordinals and keys
template <class arg_t>
struct generate_mfc_filter_from_sequence
<
	arg_t, 
	typename boost::enable_if	// // if arg_t contains ints, and not all of them are ints
	< 
		_mpl_::and_
		< 
			seq_contains_mpl_integral_int<arg_t>, 
			not_seq_is_all_mpl_integral_int<arg_t> 
		>  
	>::type 
>
{
	// split the int<> & integral_c<int,> types into one sequence
	// and put the keys into another one (seperate)
	typedef typename split_keys_and_orginals<arg_t>::type split;

	// return a filter expression which will check for the key of a given arg to be within the key sequence
	// OR a ordinal of a given arg to be within the ordinal sequence
	typedef typename _mpl_::lambda
	 < 
		_mpl_::or_
		< 
			_mpl_::contains		// if the key of arg is in our key sequence
			< 
				typename split::keys, 
				key<_mpl_::_1> 
			> , 
			_mpl_::contains		// (or) if the ordinal of the arg is in our arg sequence
			<  
				typename split::ordinals  , 
				index_<_mpl_::_1>   
			> 
		> 
	>::type type;
};


///////////////////////////////////////
// data
///////////////////////////////////////

template <class idx_t, class key_t, class concepts_t, class impl_t>
struct data
{
	typedef data		type;
	typedef idx_t		idx_type;
	typedef key_t		key_type;
	typedef concepts_t	concepts;
	typedef impl_t		impl_type;
};


namespace detail
{ 
	// helper class for determining the argument type for a given 
	// element
	template <class data_view, class seq_t>
	class arg_type
	{
		template <int idx2>
		struct get_arg_type
		{
			typedef typename _mpl_::at<data_view , _mpl_::int_<idx2> >::type::impl_type::arg_type type;
			
		};
	public:
		template <int idx>
		class get
		{
			public:
			typedef typename _mpl_::eval_if
			<
				_mpl_::less< _mpl_::int_<idx>, _mpl_::size<seq_t> >,  
				get_arg_type<idx>,
				_mpl_::void_
			>::type type;
		};
	};
	
	template <class T>
	struct get_seq
	{
		typedef typename _mpl_::lambda<T>::type::seq_type type;
	};
	template <class T>
	struct get_all_bag_keys
	{
		typedef typename T::all_bag_keys type; 
	};
	
	template <class T>
	struct get_data_view_all
	{
		typedef typename T::data_view_all type; 
	};
	
	template <class T>
	struct get_value_type
	{
		typedef typename T::value_type type;

	};

typedef _mpl_::vector10<by_any,by_val,by_ref,single<by_ref>,single<by_val>,single<by_any>, multiple<by_ref>,multiple<by_val>,multiple<by_any>,formal_item > arg_filters;

template <class arg0_t>
struct test
{
	typedef typename _mpl_::lambda< _mpl_::contains< arg0_t  , key<_mpl_::_1>   > >::type type;
};

			/* 
				can take:
					A where A is a type in seq_t
					by_ref,
					by_val, (anything in arg_filters)
					some _mpl_ sequence i.e. vector<A,B,C>
					a compile time lambda expression
				will generate a unary metafunction class which will 
				return true if the _1 arg passes the generated filter
			*/ 
			template <class arg0_t, class all_bag_keys_t> // todo remove arg_seq_t class arg_seq_t, 
			struct arg_to_filter_impl
			{
				typedef typename _mpl_::lambda< arg0_t >::type arg0_t_lambda;
				typedef typename _mpl_::eval_if
				< 
					// is the arg a specific filter expression? (i.e. by_val, multiple<by_val> etc...)
					_mpl_::contains<arg_filters,arg0_t_lambda >,
					_mpl_::eval_if
					<
						// is the filter speifically a formal_item filter?	
						boost::is_same<arg0_t,formal_item>,

						// return mfc which will filter on key types which are formal items
						_mpl_::lambda< is_formal_item< key<_mpl_::_1>   > >,

						// else return a filter expression to look for the arg as a concept
						has_concept< arg0_t>
					>,
					// ELSE
					_mpl_::eval_if
					< 
						// is the arg a sequence?		
						_mpl_::is_sequence< arg0_t_lambda>, 

						generate_mfc_filter_from_sequence< arg0_t >,

                        // if the type is IN the list of keys for all items (deep)
						_mpl_::eval_if	
						<
							_mpl_::contains		// if the arg is in the sequence
							< 
								all_bag_keys_t, // all the deep types for this and contained bags 
								arg0_t_lambda
							>,  
							does_match<key_of,arg0_t >, // return a mfc for checking on a match of key with the passed arg
							// otherwise assume the arg is a lambda expression and return an MFC which will apply it to the key type
							eval_expression_on_key< arg0_t_lambda  >
						>
					>
				>::type type;
			};


	// actual implementation of bag
	template < class default_wrapper_t, class seq_t>
	struct bag_impl : bag_impl_base
	{
			typedef seq_t seq_type;
			
			typedef mpl::vector1<bag_impl> rt_args; // runtime args for use with adapters

			// base class for formal items
			struct formal_item_base : formal_item
			{
				typedef bag_impl	parent_type;
						
				parent_type * pParent;
				typedef _mpl_::vector1< formal_item> concepts;
			};		
			
			// will re-wrap an item if if is formal
			// result will be formal_item_base is applied 
			// as the base class to the nested 'item<>' template in 
			// passed class. 
			template <class T, class Enable = void> 
			struct rewrap_wrapped_modifier_if_formal  
			{  
				typedef _mpl_::void_ type;
			};

			template <class T>
			struct rewrap_wrapped_modifier_if_formal<T, typename boost::enable_if< has_concept<local_modifier, T >  >::type> 
			:	_mpl_::apply1
				<	T,
					typename if_formal_apply_base
					<	
						formal_item_base, 
						T
					>::type
				>
			{};
			
			// calculates the implementation type for an item
			struct calculate_impl_type
			{
				template <class T> 
				struct apply 
				: _mpl_::if_
				<
					has_concept<local_modifier, T >,		// local modifier in type?
					typename rewrap_wrapped_modifier_if_formal<T>::type, // rewrap it from modifier - else apply default
					typename _mpl_::apply1< default_wrapper_t, typename if_formal_apply_base< formal_item_base, T>::type > ::type
				>
				{};
			};	
			
			// will return a class of type 'data<>' which will have 
			// internal type information for the bag on how to interact 
			// with the item
			struct build_data_row
			{
				template <class idx_element_t, class seq_element_t > 
				struct apply 
				{
					typedef typename _mpl_::apply1<calculate_impl_type,seq_element_t>::type impl_type;
					typedef data
					<
						idx_element_t,
						typename key<seq_element_t>::type,
						typename impl_type::concepts,
						impl_type
					> type;
				};
			};	
			
			// sequential list of numbers for each item in passed sequence
			typedef _mpl_::range_c<int,0, _mpl_::size<seq_t>::value >		 seq_indexes;
	  
			// sequence with indices applied
			// takes the seq_indexes and builds a sequence of data<> elements
			// the resulting sequence will each have a unique index # in each data<> element
			typedef typename 
			_mpl_::transform_view
			<
				_mpl_::zip_view<_mpl_::vector<seq_indexes,seq_t> >
				, _mpl_::unpack_args< _mpl_::apply2<build_data_row,_mpl_::_1, _mpl_::_2 > > 
			>  ::type data_view; 
			
			// the inlined strategies- i.e. single<int,by_val> in a sequence form
			typedef typename _mpl_::transform_view< data_view, impl<_mpl_::_1> >::type data_view_inlined_strategies;

			// make a view of just the keys
			typedef typename _mpl_::transform_view<data_view, key_of>::type data_view__keys;

			// make a view of just the args
			typedef typename _mpl_::transform_view<data_view, arg_of>::type data_view__args;

			// view of just the contained bag types
			typedef typename _mpl_::filter_view
			< 
				data_view__keys , 
				_mpl_::lambda< is_bag< _mpl_::_1 > > 
			>::type bag_types_contained;


			// sequence of this bag's keys combined with any contained bags
			// note this will recursively dig into contained bags and pull out the keys 
			// will recurse until the leaf nodes are hit
			typedef typename  _mpl_::fold
			 <
				bag_types_contained // just the keys of any child bags
				, data_view__keys // seed it with this bag's keys
				, _mpl_::joint_view< _mpl_::_1, _mpl_::lambda< get_all_bag_keys< _mpl_::_2> >  >::type 
			>::type all_bag_keys; 

			// view of just the contained bag types - for all contained bags
			typedef typename _mpl_::filter_view
			< 
				all_bag_keys , 
				_mpl_::lambda< is_bag< _mpl_::_1 > > 
			>::type all_bag_types_contained;
			
			// data view of all items- including embedded bags
			typedef typename  _mpl_::fold
			 <
				bag_types_contained // just the keys of any child bags
				, data_view // seed it with this bag's dataview
				, _mpl_::joint_view< _mpl_::_1, _mpl_::lambda< get_data_view_all< _mpl_::_2> >  >::type 
			>::type data_view_all; 

			// helper template for calling arg_type<>::get-
			// this helper passes in the appropriate data_view and seq_t 
			// allowing the caller to only worry about the index of the item
			template <int idx>	struct argt : arg_type<data_view,seq_t>:: template get<idx> {};
			
			template <class arg_seq_t, class arg0_t> // todo remove arg_seq_t
			struct arg_to_filter_old
			{
				typedef typename _mpl_::lambda< arg0_t >::type arg0_t_lambda;
				typedef typename _mpl_::eval_if
				< 
					// is the arg a specific filter expression? (i.e. by_val, multiple<by_val> etc...)
					_mpl_::contains<arg_filters,arg0_t_lambda >,
					_mpl_::eval_if
					<
						// is the filter speifically a formal_item filter?	
						boost::is_same<arg0_t,formal_item>,

						// return mfc which will filter on key types which are formal items
						typename _mpl_::lambda< is_formal_item< key<_mpl_::_1>   > >::type,

						// else return a filter expression to look for the arg as a concept
						has_concept< arg0_t>
					>,
					// ELSE
					_mpl_::eval_if
					< 
						// is the arg a sequence?		
						typename _mpl_::is_sequence< arg0_t_lambda>::type, 
						// return mfc which will filter on key types which match the raw types in seq_t
						typename _mpl_::lambda< _mpl_::contains< arg0_t_lambda  , key<_mpl_::_1>   > >::type,
						// ELSE 
						// if the type is IN the list of keys for all items (deep)
						_mpl_::eval_if	
						<
							typename _mpl_::contains		// if the arg is in the sequence
							< 
								all_bag_keys, // all the deep types for this and contained bags 
								arg0_t_lambda
							>::type, 
							does_match<key_of,arg0_t >, // return a mfc for checking on a match of key with the passed arg
							// otherwise assume the arg is a lambda expression and return an MFC which will apply it to the key type
							eval_expression_on_key< arg0_t_lambda  >
						>
					>
				>::type type;
			};

			template <class arg0_t > // todo remove arg_seq_t class arg_seq_t, 
			struct arg_to_filter : arg_to_filter_impl <  arg0_t, all_bag_keys> {};

			// wrap for holding the internal implementation of the items
			template <class data_t>
			struct data_to_object_wrap
			{
				typedef data_to_object_wrap type;
				typedef typename impl<data_t>::type value_type;
				value_type value;						
			};
			
			// generate the class holding the objects used to store the item data
			typedef typename _mpl_::inherit_linearly
			<
				data_view, _mpl_::inherit<data_to_object_wrap<_mpl_::_2>,_mpl_::_1>
			>::type generated_objects_t;
			
			// instatiate the class holding all the objects in the bag
			generated_objects_t generated_objs;

			// inserts an item into the bag based on ordinal
			template <int int_idx_t>
			void insert(typename argt<int_idx_t>::type a0) 
			{
				elem<int_idx_t>().insert(a0);
			}

			// inserts an item into the bag based on type
			// will insert into the FIRST KEY FOUND that matches
			template <class T >
			void insert(typename locate<data_view,key_of,T>::type::impl_type::arg_type a0) 
			{
				typedef typename locate<data_view,key_of,T>::type::idx_type found_idx;
				elem< found_idx::value >().insert(a0);
			}
			// set an element based on ordinal
			// if the stategy is single, then the item gets the new value
			// otherwise - the item is cleared and set with a0
			template <int int_idx_t>
			typename get_value_type
			<
				typename data_to_object_wrap 
				< 
					typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
				>::value_type   
			>::type &
			set(typename argt<int_idx_t>::type a0) 
			{
				elem<int_idx_t>().set(a0);
				return elem< int_idx_t >().val_as_ref();
			}

			// set an element based on type
			// if the stategy is single, then the item gets the new value
			// otherwise - the item is cleared and set with a0
			// first item with type found is used
			template <class key_t >
			typename get_value_type
			<
				typename data_to_object_wrap 
				< 
					typename locate<data_view,key_of,key_t >::type 
				>::value_type   
			>::type &
			set(typename locate<data_view,key_of,key_t>::type::impl_type::arg_type a0) 
			{
				typedef typename locate<data_view,key_of,key_t>::type::idx_type found_idx;
				elem< found_idx::value >().set(a0);
				return elem< found_idx::value >().val_as_ref();
			}
			
			// helper for getting the object via the data<> type
			template<class data_t>
			typename data_to_object_wrap < data_t>::value_type  & 
			get_object_by_data_type()
			{
				return static_cast< data_to_object_wrap <data_t> &>(generated_objs).value; 
			}
			
			// helper for getting an object via an mfc used 
			// to locate the data type
			template<class mfc_t, class value_t>
			typename data_to_object_wrap 
			< 
				typename locate<data_view,mfc_t,value_t>::type 
			>::value_type  & 
			get_object_by()
			{
				typedef typename locate<data_view,mfc_t,value_t>::type found_data_type;
				return 	get_object_by_data_type<found_data_type>();		
			}

			// get the raw internal element via ordinal
			template <int int_idx_t>
			typename data_to_object_wrap 
			< 
				typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
			>::value_type  & 
			elem()
			{
				typedef typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type  found_data_type;
				return 	get_object_by_data_type<found_data_type>();		
			}

			// get the item via key
			template <class key_t>
			typename get_value_type
			<
				typename data_to_object_wrap 
				< 
					typename locate<data_view,key_of,key_t >::type 
				>::value_type   
			>::type &
			item()
			{
				typedef typename locate<data_view,key_of,key_t >::type  found_data_type;
				return 	get_object_by_data_type<found_data_type>().val_as_ref();		
			}

			// get the item via ordinal
			template <int int_idx_t>
			typename get_value_type
			<
				typename data_to_object_wrap 
				< 
					typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
				>::value_type   
			>::type &
			item()
			{
				typedef typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type  found_data_type;
				return 	get_object_by_data_type<found_data_type>().val_as_ref();		
			}

			// speficically get the item via ordinal
			template <int int_idx_t>
			typename get_value_type
			<
				typename data_to_object_wrap 
				< 
					typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
				>::value_type   
			>::type &
			item_at()
			{
				typedef typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type  found_data_type;
				return 	get_object_by_data_type<found_data_type>().val_as_ref();		
			}

			// return the type of the item
			template <int int_idx_t>
			struct item_type
				: get_value_type
				<
					typename data_to_object_wrap 
					< 
						typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
					>::value_type   
				>
			{};

			// visit the generated object
			// takes a ref to the bag, and a functor
			// the functor needs to visit the implementation of the item
			// where visit_gen_object needs to look up the item via
			// the internal data<> type
			// visit_gen_object isolates the passed functor from this 
			// internal mechanism and allows the functor to end up visiting
			// the implementation of the generated object
			template <class func_t>
			struct visit_gen_object
			{
				bag_impl		&			mbag;
				func_t	&			mfunc;
				
				visit_gen_object(bag_impl & bag_, func_t & func)	
				: mbag(bag_) , mfunc(func) {};
				
				template <class data_type_to_lookup_t>
				void operator()(data_type_to_lookup_t &)
				{
					mfunc(mbag.get_object_by_data_type<data_type_to_lookup_t>());
				}			
			};

			// iterates all of the internally stored items
			template <class func_t>
			void for_each_raw(func_t  func)
			{
				_mpl_::for_each<data_view>(visit_gen_object<func_t>(*this,func) );
			}

			// takes the received target- and hands the passed functor to the target's visit_each() member
			template <class orig_arg_t, class functor_t> 
			struct call_visit_each_on_target
			{
				typedef call_visit_each_on_target type;
				typedef orig_arg_t	orig_arg_type;
				functor_t & m_functor;

				call_visit_each_on_target(functor_t & functor)
				: m_functor(functor)	{};
				
				template <class target_t>
				void operator()(target_t &t )
				{			
					t.template visit_each< orig_arg_type, functor_t>(m_functor);				
				}
			};

			// will vist each gen object which has an exact raw type match 
			// with the passed sequence
			template <class vec_t, class func_t>
			void for_each_raw_filtered_new(func_t  func)
			{
			
				typedef _mpl_::filter_view< data_view , _mpl_::contains<vec_t, index_< _mpl_::_1> >   >  filtered_sequence;				
				_mpl_::for_each< filtered_sequence >(visit_gen_object<func_t>(*this,func) );
			}

			template <class exp_t>
			void print_storage()
			{
	
				// make a view of all things with match the expression
				typedef _mpl_::filter_view< data_view , _mpl_::or_<_mpl_::apply1<typename exp_t::type::type ,_mpl_::_1>,is_bag< key<_mpl_::_1> > >   >  filtered_sequence;
				print_type pt;
				_mpl_::for_each< filtered_sequence >(visit_gen_object<print_type>(*this,pt) );
			}

			// will vist each gen object which has an exact raw type match 
			// with the passed sequence
			template <class exp_t, class func_t>
			void for_each_raw_filtered(func_t  func)
			{
				// make a view of all things with match the expression
				typedef _mpl_::filter_view< data_view , _mpl_::or_<_mpl_::apply1<typename exp_t::type,_mpl_::_1>,is_bag< key<_mpl_::_1> > >   >  filtered_sequence;
				_mpl_::for_each< filtered_sequence >(visit_gen_object<func_t>(*this,func) );
			}
			// visit the gen objects- applying the filter to the internal objects
			// will then proxy out the functor to the actual implementation of the item
			template <class exp_t, class func_t>
			void for_each_raw_filtered_visit_each(func_t  func)
			{
				call_visit_each_on_target<exp_t,func_t> cvet(func) ;
				typedef _mpl_::filter_view< data_view , _mpl_::or_<_mpl_::apply1<typename exp_t::type,_mpl_::_1>,is_bag< key<_mpl_::_1> > >   >  filtered_sequence;
				_mpl_::for_each< filtered_sequence >(visit_gen_object<call_visit_each_on_target<exp_t,func_t> >(*this,cvet) );
			}

			// visit each item
			template <class func_t>
			void for_each(func_t  func)
			{
				call_visit_each_on_target<_mpl_::void_,func_t> cvet(func) ;
				for_each_raw(cvet );
			}
			// visit each item with a valid argument
			template <class arg_t, class func_t> 
			void for_each(func_t  func)
			{
				for_each_raw_filtered_visit_each<arg_to_filter< arg_t > >(func );
			}
			// todo- go away?
			template <class exp_t, class func_t> 
			void for_each_if(func_t  func)
			{
				call_visit_each_on_target<_mpl_::void_,func_t> cvet(func) ;
				for_each_raw_filtered< eval_expression_on_key< typename _mpl_::lambda< exp_t >::type >  >(cvet );
			}

			// types is any filter param
			// exp_t is a unary metafunction class
			template<class arg_t, class exp_t, class func_t>											 
			void for_each_if(func_t  func)
			{
				typedef typename arg_to_filter<arg_t>::type filter_mfc;
				typedef pass_functor_to_target_matching_exp<   exp_t, func_t > func_proxy_type;
				func_proxy_type func_proxy(func);
				call_visit_each_on_target< _mpl_::void_,func_proxy_type > cvet( func_proxy ) ;
				for_each_raw_filtered< filter_mfc  >(cvet );
			}
		private:
			// passes a functor to a target if the target matches the compile time 
			// expression
			template<class exp_t, class functor_t>											 
			struct pass_functor_to_target_matching_exp
			{	
				public:
				typedef pass_functor_to_target_matching_exp type;
				functor_t & m_functor;

				typedef typename _mpl_::lambda< exp_t >::type exp_func_t;

				pass_functor_to_target_matching_exp(functor_t & functor)
				: m_functor(functor)	{};

				private:
					struct do_visit
					{
						template <class target_t>
						do_visit(target_t &t, functor_t & functor )
						{
							functor(t);
						}
					};
					struct do_not_visit
					{
						template <class target_t>
						do_not_visit(target_t &, functor_t &  )
						{
							// this space for rent
						}
					};
				public:			
				template <class target_t>
				void operator()(target_t &t )
				{			
					typename _mpl_::if_
					<	typename exp_func_t::template apply
						<
							target_t 
						>::type, 
						do_visit, 
						do_not_visit
					>::type (t, m_functor);
				}
			};
			
			public:
			
			template <class arg_t>
			struct remove_first_if_equal
			{
				arg_t		m_arg;
				bool		m_bfound;
				remove_first_if_equal(arg_t  arg)
				: m_arg(arg), m_bfound(false) 
				{ }
				template <class T>
				void operator()(T & obj)
				{
					if (m_bfound)
						return;
					if (obj.is_equal(m_arg) == true)
					{
						obj.remove(m_arg);
						m_bfound = true;
					}
				}
			};
			public:
			 
			template <class by_t, class T>
			bool remove(T & obj, typename enable_if<boost::is_same<by_ref, by_t> >::type* dummy = 0)
			{
				remove_first_if_equal<T&> rm_functor(obj); 
				for_each_raw_filtered< has_concept<by_t> >(rm_functor );
				return rm_functor.m_bfound;
			}
			template <class by_t, class T>
			bool remove(T obj, typename enable_if<boost::is_same<by_val, by_t> >::type* dummy = 0)
			{
				remove_first_if_equal<T> rm_functor(obj); 
				for_each_raw_filtered< has_concept<by_t> >(rm_functor );
				return rm_functor.m_bfound;
			}
			public:
				typedef mpl::false_ is_filter ;	// this is a bag not a filter
				
			////////////////////////////////////////////
			// BAG Metaprogramming interface
			
			// structure for presenting some useful meta interfaces from this bag
			// 
			struct config
			{
				struct error{ typedef error type; };
				
				/////////////////////
				// internal helpers
				private:
					// takes the passed concept list- and returns the matching instance type
					// todo- for the basic and code stuff we DONT need concepts (!)
					template <class target_concepts>
					struct instance_concept_to_type
					:	_mpl_::if_
						< 
							_mpl_::contains< target_concepts, by_val>, 
							by_val,
							// ELSE
							typename _mpl_::if_
							< 
								_mpl_::contains< target_concepts, by_ref>, 
								by_ref,
								// ELSE
								typename _mpl_::if_
								< 
									_mpl_::contains< target_concepts, by_auto_ref>, 
									by_auto_ref,
									// ELSE
									typename _mpl_::if_
									< 
										_mpl_::contains< target_concepts, by_shared_ref>, 
										by_shared_ref,
										// ELSE
										error
									>::type
								>::type
							>::type
						>::type
					{};
					// takes a concept list, finds the dimension and returns it
					// returns multiple_ in place of multiple<> and single_ in place of single<>
					template <class target_concepts>
					struct dimension_concept_to_type
					:	_mpl_::if_
						< 
							_mpl_::contains< target_concepts, multiple_>, 
							multiple_,
							// ELSE
							typename _mpl_::if_
							< 
								_mpl_::contains< target_concepts, single_>, 
								single_,
								// ELSE
								error
							>::type
						>::type
					{};

					// given an internal storage type- will return a string 
					// denoting the dimension 
					template< class T>
					struct impl_to_dimension_string
					{
						typedef impl_to_dimension_string type;

						struct _single	{	std::string name()		{	return "single";	}	};
						struct _multiple{	std::string name()		{	return "multiple";	}	};
						struct _error	{	std::string name()		{	return "*ERROR*";	}	};
						
						std::string operator()()
						{
							return
							typename _mpl_::if_
							<
								has_concept<single<by_any>,T>,
								_single,
								typename _mpl_::if_
								<	
									has_concept<multiple<by_any>,T>, 
									_multiple,
									_error
								>::type 
							>::type().name(); 
						}
					};

					// given an internal data<> class - will return string form of storage
					template< class T>
					struct impl_to_instance_string
					{
						typedef impl_to_instance_string type;
						 
						struct _by_val			{	std::string name()	{	return "by_val";			}	};
						struct _by_ref			{	std::string name()	{	return "by_ref";			}	};
						struct _by_auto_ref		{	std::string name()	{	return "by_auto_ref";		}	};
						struct _by_shared_ref	{	std::string name()	{	return "by_shared_ref";	}	};
						struct _error			{	std::string name()	{	return "*ERROR*";			}	};
						
						std::string operator()()
						{
							return				// this could be replaced by a map, once we have typedef's for these things
							typename _mpl_::if_
							<
								has_concept<by_val,T>,
								_by_val,
								typename _mpl_::if_
								<	
									has_concept<by_ref,T>, 
									_by_ref,
									typename _mpl_::if_
									<
										has_concept<by_auto_ref,T>, 
										_by_auto_ref,
										typename _mpl_::if_
										<
											has_concept<by_shared_ref,T>, 
											_by_shared_ref,
											_error
										>::type
									>::type
								>::type 
							>::type().name() ; 
						}
					};
					
					// given the data<> class will return
					// a strategy for that data class
					// i.e. single<by_val> etc..
					template <class data_t>
					struct impl_to_strategy
					{
						struct error{ typedef error type; };
				
						// lookup the instance type
						typedef typename instance_concept_to_type< typename  data_t::concepts>::type instance_type;
						typedef typename _mpl_::if_
						< 
							// if it is a single
							has_concept< single_,data_t >,
							typename single<instance_type>::type,
							// ELSE if it is multiple
							typename _mpl_::if_
							< 
								has_concept< multiple_,data_t >,
								typename multiple<instance_type>::type,
								// ELSE
								error
							>::type
						>::type type;
					};
				
				public:
					// slot size of bag (i.e. number of slots)
					typedef _mpl_::size<seq_t> slot_size;
					
					// valid strategies
					typedef _mpl_::vector8< single<by_val>, single<by_ref>, single<by_auto_ref>, single<by_shared_ref>, multiple<by_val>, multiple<by_ref>, multiple<by_auto_ref>, multiple<by_shared_ref> > valid_strategies;
					
					// returns true if T is a valid strategy
					template <class T> struct is_valid_strategy : _mpl_::contains< valid_strategies, T> {};

					// valid instances
					typedef _mpl_::vector4< by_val, by_ref, by_auto_ref, by_shared_ref > valid_instances;
					
					// returns true if T is a valid instance
					template <class T> struct is_valid_instance : _mpl_::contains< valid_instances, T> {};

					// valid dimensions
					typedef _mpl_::vector2< single_,multiple_ > valid_dimensions;
					
					// returns true if T is a valid instance
					template <class T> struct is_valid_dimension : _mpl_::contains< valid_dimensions, T> {};
					
					// sequence of all the keys
					typedef data_view__keys	keys ;
					
					// the inlined strategies- i.e. single<int,by_val> in a sequence form
					typedef data_view_inlined_strategies inlined_strategies;
					
					// todo 	* make a all_inlined_strategies view	
					
					// sequence of all the keys - includes items in contained bags
					typedef all_bag_keys	all_keys;
					
					// list of all bag types contained in the bag
					typedef bag_types_contained contained_bags;

					// list of all bag types contained in the bag
					typedef all_bag_types_contained all_contained_bags;
					
					// formal item keys in this bag
					typedef typename _mpl_::filter_view
					< 
						keys , 
						_mpl_::lambda< is_formal_item< _mpl_::_1 > > 
					>::type formal_items;

					// filtered keys
					// will take any valid filter and apply to keys
					template <class filter_arg_t>
					struct filtered_keys
					:	_mpl_::transform_view
						<
							_mpl_::filter_view
							< 
								data_view , 
								_mpl_::apply1
								<
									typename arg_to_filter<  filter_arg_t  >::type 
									,_mpl_::_1
								>
							>,
							key_of
						>
					{};
					// filtered ordinals
					// will take any valid filter and return a list of ordinals
					template <class filter_arg_t>
					struct filtered_ordinals
					:	make_vector
						<
						_mpl_::transform_view
						<
							_mpl_::filter_view
							< 
								data_view , 
								_mpl_::apply1
								<
									typename arg_to_filter<  filter_arg_t  >::type 
									,_mpl_::_1
								>
							>,
							index_of
						>
						>
					{};

					// filtered all keys
					// will take any valid filter and apply to keys
					// like filtered_keys- except includes on keys list from embedded bags
					template <class filter_arg_t>
					struct filtered_all_keys
					:	_mpl_::transform_view
						<
							_mpl_::filter_view
							< 
								data_view_all , 
								_mpl_::apply1
								<
									typename arg_to_filter< filter_arg_t  >::type 
									,_mpl_::_1
								>
							>,
							key_of
						>
					{};

					// all formal item keys in this bag and contained
					typedef typename _mpl_::filter_view
					< 
						all_keys , 
						_mpl_::lambda< is_formal_item< _mpl_::_1 > > 
					>::type all_formal_items;
					
					// list of all the slot ordinals in the bag
					typedef seq_indexes slot_ordinals;
					
					
				/////////////////////
				// DIMENSION 
				public:
					template <class key_t>
					struct dimension_of // returns either single_ or multiple_ to represent single<> and multiple<>
					: dimension_concept_to_type< typename locate<data_view,key_of, key_t >::type::concepts > {};

					template <int int_idx_t>
					struct dimension_at // returns either single_ or multiple_ to represent single<> and multiple<>
					: dimension_concept_to_type< typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type::concepts > {};

					template <class key_t>
					struct dimension_of_string 
					: impl_to_dimension_string< typename locate<data_view,key_of, key_t >::type > {};

					template <int int_idx_t>
					struct dimension_at_string 
					: impl_to_dimension_string< typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type > {};

					
					// true if item at key is of passed dimension
					template <class key_t, class dimension_t>
					struct is_dimension_of 
					: has_concept<dimension_t, typename locate<data_view,key_of, key_t >::type > 
					{	
						BOOST_MPL_ASSERT(( is_valid_dimension<dimension_t > ));
					};

					// true if item at ordinal is of passed dimension
					template <int int_idx_t, class dimension_t>
					struct is_dimension_at 
					: has_concept<dimension_t, typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type > 
					{	
						BOOST_MPL_ASSERT(( is_valid_dimension<dimension_t > ));
					};

				/////////////////////
				// INSTANCE 
				public:
					template <class key_t>
					struct instance_of // returns either single_ or multiple_ to represent single<> and multiple<>
					: instance_concept_to_type< typename locate<data_view,key_of, key_t >::type::concepts > {};

					template <int int_idx_t>
					struct instance_at // returns either single_ or multiple_ to represent single<> and multiple<>
					: instance_concept_to_type< typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type::concepts > {};

					template <class key_t>
					struct instance_of_string 
					: impl_to_instance_string< typename locate<data_view,key_of, key_t >::type > {};

					template <int int_idx_t>
					struct instance_at_string 
					: impl_to_instance_string< typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type > {};

					// true if item at key is of passed instance
					template <class key_t, class instance_t>
					struct is_instance_of 
					: has_concept<instance_t, typename locate<data_view,key_of, key_t >::type > 
					{	
						BOOST_MPL_ASSERT(( is_valid_instance<instance_t > ));
					};

					// true if item at ordinal is of passed dimension
					template <int int_idx_t, class instance_t>
					struct is_instance_at 
					: has_concept<instance_t, typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type > 
					{	
						BOOST_MPL_ASSERT(( is_valid_instance<instance_t > ));
					};
				/////////////////////
				// KEY 
				public:
					template <int int_idx_t>
					struct key_at	// returns the key at the specified orginal
					{
						typedef typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type::key_type type;
					};
					
					// returns true if this bag has passed key
					template< class key_t>	struct has_key : _mpl_::contains< data_view__keys,key_t>	{};

				/////////////////////
				// arg 
				public:
					template <class key_t>
					struct arg_of	// returns the key at the specified orginal
					{
						typedef typename locate<data_view,key_of,key_t >::type::impl_type::arg_type type;
					};
					template <int int_idx_t>
					struct arg_at	// returns the key at the specified orginal
					{
						typedef typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type::impl_type::arg_type type;
					};
					
					// returns true if this bag has passed key
					template< class arg_t>	struct has_arg : _mpl_::contains< data_view__keys,key_t>	{};
					
					typedef typename make_vector< data_view__args >::type all_args;

				/////////////////////
				// return_type_at 
				public:
				
				template <int int_idx_t>
				struct return_type_at
				:	get_value_type
					<
						typename data_to_object_wrap 
						< 
							typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type 
						>::value_type   
					> 
				{};
					
				/////////////////////
				// Item Strategy 
				public:
					template <class key_t>
					struct strategy_of 
					{	
						typedef typename impl_to_strategy<  typename locate<data_view,key_of, key_t >::type  >::type type;
					};
					template <int int_idx_t>
					struct strategy_at	
					{
						typedef typename impl_to_strategy< typename locate<data_view,index_of,_mpl_::integral_c<int, int_idx_t> >::type >::type type;
					};
					template <class key_t>
					struct strategy_of_string 
					{	
						struct type
						{
							std::string operator()() { return typename dimension_of_string<key_t>::type()() + "<" + typename instance_of_string<key_t>::type()() + ">";	}
						};
					};
					template <int int_idx_t>
					struct strategy_at_string 
					{	
						struct type
						{
							std::string operator()() { return typename dimension_at_string<int_idx_t>::type()() + "<" + typename instance_at_string<int_idx_t>::type()() + ">";	}
						};
					};

				/////////////////////
				// Embedded Bags 
				public:
					struct has_embedded_bag : _mpl_::greater< _mpl_::size<contained_bags>, _mpl_::int_<0> > {};
				
			};
			
			// constructor implemenation
			// NOTE: argt<> ends up getting the argument type for the item from the strategy
			bag_impl()
			{
			}
			bag_impl(typename argt<0>::type a0)
			{
				elem<0>().set(a0);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
				elem<10>().set(a10);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
				elem<10>().set(a10);
				elem<11>().set(a11);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
				elem<10>().set(a10);
				elem<11>().set(a11);
				elem<12>().set(a12);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
				elem<10>().set(a10);
				elem<11>().set(a11);
				elem<12>().set(a12);
				elem<13>().set(a13);
			}
			bag_impl(typename argt<0>::type a0, typename argt<1>::type a1, typename argt<2>::type a2, typename argt<3>::type a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13, typename argt<14>::type a14)
			{
				elem<0>().set(a0);
				elem<1>().set(a1);
				elem<2>().set(a2);
				elem<3>().set(a3);
				elem<4>().set(a4);
				elem<5>().set(a5);
				elem<6>().set(a6);
				elem<7>().set(a7);
				elem<8>().set(a8);
				elem<9>().set(a9);
				elem<10>().set(a10);
				elem<11>().set(a11);
				elem<12>().set(a12);
				elem<13>().set(a13);
				elem<14>().set(a13);
			}
	};

}; // namespace detail

// Public interface to the bag

template < class T0  = _mpl_::void_, class T1  = _mpl_::void_,class T2  = _mpl_::void_,class T3  = _mpl_::void_>
struct bag;

template < class seq_t>
struct bag<seq_t> : public detail::bag_impl<single<by_val>, seq_t>
{	
	typedef single<by_val> wrapper_t;
	typedef typename detail::bag_impl<wrapper_t, seq_t>::data_view data_view;
	template <int idx>	struct argt : detail::arg_type<data_view,seq_t>:: template get<idx> {};
		
	bag()	{	}
	bag(typename argt<0>::type a0)	: detail::bag_impl<wrapper_t, seq_t>(a0)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type  a4)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)  { }	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {}	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,a13) {}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13, typename argt<14>::type a14)		: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {}
};


template < class default_wrapper_t, class seq_t>
struct bag<default_wrapper_t,seq_t>  : detail::bag_impl<default_wrapper_t, seq_t>
{
	typedef default_wrapper_t wrapper_t;
	typedef typename detail::bag_impl<wrapper_t, seq_t>::data_view data_view;
	template <int idx>	struct argt : detail::arg_type<data_view,seq_t>:: template get<idx> {};
		
	bag()	{	}
	bag(typename argt<0>::type a0)	: detail::bag_impl<wrapper_t, seq_t>(a0)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type  a4)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)	{	}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)  { }	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {}	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13)	: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,a13) {}
	bag(typename argt<0>::type a0, typename argt<1>::type  a1, typename argt<2>::type  a2, typename argt<3>::type  a3, typename argt<4>::type a4, typename argt<5>::type a5, typename argt<6>::type a6, typename argt<7>::type a7, typename argt<8>::type a8, typename argt<9>::type a9, typename argt<10>::type a10, typename argt<11>::type a11, typename argt<12>::type a12, typename argt<13>::type a13, typename argt<14>::type a14)		: detail::bag_impl<wrapper_t, seq_t>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {}
};

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// HELPERS

template <class T>
struct get_value_c
{
	typedef get_value_c type;
	enum { value=T::value};
};

//////////////////////////////////////////////
// helpers

/*
	join_nested_seq
		takes a sequence of types- which themselves have nested sequences
		uses the passed "nested_resolver" to locate the nested sequences
		joins together the nested sequences
*/
template 
<
	class seq_t,
	class nested_retriever
>
struct join_nested_seq
: mpl::fold
	<
		typename mpl::transform
		<
			typename seq_t::type,
			nested_retriever
		>::type ,
		mpl::vector0<>,
		mpl::joint_view< mpl::_1,mpl::_2> 	
    >
{ };

// retrieves the keys from type as an mpl sequence
template <class T>
struct get_keys
{	
	typedef typename T::iv_keys::sequence type;
};

// retrieves the return types from type as an mpl sequence
template <class T>
struct get_rets
{	
	typedef typename T::iv_rets::sequence type;
};

// retrieves the arg types from type as an mpl sequence
template <class T>
struct get_args
{	
	typedef typename T::iv_args::sequence type;
};

// retrieves an mpl sequence of ordinals from the passed type
template <class T>
struct get_ordinals
{	
	typedef typename make_vector< typename T::iv_ordinals::sequence::type >::type type;
};

// retrieves an mpl sequence of ordinals from the passed type (end case)
// (having an end specialization simplifies recursive usage)
template <>
struct get_ordinals<end>
{	
	typedef mpl::vector0<> type;
};

// retrieves the deep ordinals from a given type as an mpl sequence
template <class T>
struct get_deep_ordinals
{	
	typedef typename make_vector< typename T::iv_deep_ordinals::sequence::type >::type type;
};

// retrieves the deep ordinals from a given type as an mpl sequence (end case)
// (having an end specialization simplifies recursive usage)
template <>
struct get_deep_ordinals<end>
{	
	typedef mpl::vector0<> type;
};

// checks if passed object is of the type passed
template<class looking_for_t, class obj_t>
bool is_obj_of_type(obj_t const & )
{
	if (boost::is_same<looking_for_t,obj_t>::value)
			return true;
	return false;
}

// retrieves the deep target type from passed
template <class T>
struct get_deep_target_type
{
	typedef typename T::deep_target_type type;
};

// retrieves the deep target type (as a pointer) from passed
template <class T>
struct get_deep_target_type_as_ptr
{
	typedef typename T::deep_target_type * type;
};

/*
	make_join_map
	takes a iv sequence of bags\adapaters
	uses exp_t to reteieve the nested sequence from item in iv_scan_t
	
	results in a an ordered sequence of mpl::pair<>s where
		first == the ordinal of the item
		second the ordinal of the item in the first item
	
	for each ordinal in the second item, the ordinal of the first is matched
	
*/
template 
<
	class exp_t,		// the resolver expression
	class iv_scan_t		// the sequence to scan	- a iv sequence of bags
>
struct make_join_map
{
	// implementation for make_join_map
	template <int curr_pos, int end_pos , class prev_seq_t >
	struct make_join_map_impl
	:	_mpl_::if_
		< 
			// if the current pos is less than the end pos
			_mpl_::less< mpl::int_<curr_pos>, mpl::int_<end_pos> >,
			make_join_map_impl	// recurse into ourselves - with incremeneted iter
			< 
				curr_pos +1,
				end_pos,						// repass- endpos					 
				mpl::joint_view	
				< 
					typename prev_seq_t::type,	// join previous sequence
					typename mpl::transform		// with a new sequence made up of pair<> where first is current pos, and second is the element in the range
					<
						typename mpl::apply1	// apply the resolver to the current item (to give us the seq to transform)
						<
							exp_t,	// resolver
							typename iv_scan_t:: template type_at<curr_pos>::type	// dereferenced type
						>::type,
						mpl::pair< mpl::int_<curr_pos>, mpl::_1>
					>::type
				> 
			>  ,
			// all done recursing, so return the previous sequence
			typename prev_seq_t::type
		>::type 
	{};
	
	// instantiation of the recursive zip_filter_impl struct
	typedef typename
	make_join_map_impl
	< 
		0,							// start at element 0
		iv_scan_t::size::value ,	// until the end of the sequence
		_mpl_::vector0<>			// start out with an empty vector
	>::type type;
};

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// INDEXED STORAGE

/*
	purposely flattened out indexed storage
	allows a passed indexed_vector<> to be made into objects 
	which are then retrieveable via integral constant
	
	note: this code is a re-write from a version that utilzed mpl::inherit_linearly<>
			this more "concrete" version of this idiom compiles **much** faster for usage in bag
			(bag and supporting classes require heavy use of lookup via ordinal, which was killing compile times
			with the normal mpl way of doing things in regards to algorithms needed to support bag)
*/

template <int size_c, class seq_t>
struct indexed_storage
{	};

template < class iv_seq_t>
struct indexed_storage<1, iv_seq_t> 
{
	typedef typename iv_seq_t:: template type_at<0>::type T0;

	typedef indexed_storage	type; 
	typedef iv_seq_t		iv_seq;	// publish our iv seq
		
	T0 obj0;
	
	T0 & get_item_at(mpl::int_<0>)
	{
		return obj0;
	}
};

template < class iv_seq_t>
struct indexed_storage<2, iv_seq_t> 
{
	typedef typename iv_seq_t:: template type_at<0>::type T0;
	typedef typename iv_seq_t:: template type_at<1>::type T1;

	typedef indexed_storage	type; 
	typedef iv_seq_t		iv_seq;	// publish our iv seq
	
	T0 obj0;
	T1 obj1;
	
	T0 & get_item_at(mpl::int_<0>)
	{
		return obj0;
	}
	T1 & get_item_at(mpl::int_<1>)
	{
		return obj1;
	}
};

template < class iv_seq_t>
struct indexed_storage<3, iv_seq_t> 
{
	typedef typename iv_seq_t:: template type_at<0>::type T0;
	typedef typename iv_seq_t:: template type_at<1>::type T1;
	typedef typename iv_seq_t:: template type_at<2>::type T2;

	typedef indexed_storage	type; 
	typedef iv_seq_t		iv_seq;	// publish our iv seq
	
	T0 obj0;
	T1 obj1;
	T2 obj2;
	
	T0 & get_item_at(mpl::int_<0>)
	{
		return obj0;
	}
	T1 & get_item_at(mpl::int_<1>)
	{
		return obj1;
	}
	T2 & get_item_at(mpl::int_<2>)
	{
		return obj2;
	}
};

template < class iv_seq_t>
struct indexed_storage<4, iv_seq_t> 
{
	typedef typename iv_seq_t:: template type_at<0>::type T0;
	typedef typename iv_seq_t:: template type_at<1>::type T1;
	typedef typename iv_seq_t:: template type_at<2>::type T2;
	typedef typename iv_seq_t:: template type_at<3>::type T3;

	typedef indexed_storage	type; 
	typedef iv_seq_t		iv_seq;	// publish our iv seq
	
	T0 obj0;
	T1 obj1;
	T2 obj2;
	T3 obj3;
	
	T0 & get_item_at(mpl::int_<0>)
	{
		return obj0;
	}
	T1 & get_item_at(mpl::int_<1>)
	{
		return obj1;
	}
	T2 & get_item_at(mpl::int_<2>)
	{
		return obj2;
	}
	T3 & get_item_at(mpl::int_<3>)
	{
		return obj3;
	}
};

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// FOR EACH ITEM

/* 
	visits each item in passed container
	container must support item_at<> and ::size
	
	as bag makes heavy use of visitation, this code was 
	re-written from a version which used mpl::for_each<> for 
	compile time improvement
	
*/
template <class container_t, class functor_t>
void for_each_impl(mpl::int_<1>, container_t & cont, functor_t & func)
{
	func(cont. template item_at<0>());
}

template <class container_t, class functor_t>
void for_each_impl(mpl::int_<2>, container_t & cont, functor_t &  func)
{
	func(cont. template item_at<0>());
	func(cont. template item_at<1>());
}

template <class container_t, class functor_t>
void for_each_impl(mpl::int_<3>, container_t & cont, functor_t & func)
{
	func(cont. template item_at<0>());
	func(cont. template item_at<1>());
	func(cont. template item_at<2>());
}

template <class container_t, class functor_t>
void for_each_impl(mpl::int_<4>, container_t & cont, functor_t & func)
{
	func(cont. template item_at<0>());
	func(cont. template item_at<1>());
	func(cont. template item_at<2>());
	func(cont. template item_at<3>());
}

template <class container_t, class functor_t>
void for_each_impl(mpl::int_<5>, container_t & cont, functor_t & func)
{
	func(cont. template item_at<0>());
	func(cont. template item_at<1>());
	func(cont. template item_at<2>());
	func(cont. template item_at<3>());
	func(cont. template item_at<4>());
}

template <class container_t, class functor_t>
void for_each_item( container_t & cont, functor_t func)
{
	for_each_impl(typename container_t::size(), cont, func);
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// FOR EACH ORDINAL
/* 
	visits each ordinal in passed container
		in the order of the ordinals
	container must support item_at<> and ::size
	
	as bag makes heavy use of visitation, this code was 
	re-written from a version which used mpl::for_each<> for 
	compile time improvement
	
*/

template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal_impl(mpl::int_<1>, container_t & cont, functor_t func)
{
	// access the iv_ordinals_t to retrieve the actual index into the container
	func(cont. template item_at< iv_ordinals_t::template type_at< 0>::type::value >());
}

template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal_impl(mpl::int_<2>, container_t & cont, functor_t &  func)
{
	// access the iv_ordinals_t to retrieve the actual index into the container
	func(cont. template item_at< iv_ordinals_t::template type_at< 0>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 1>::type::value >());
}

template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal_impl(mpl::int_<3>, container_t & cont, functor_t & func)
{
	// access the iv_ordinals_t to retrieve the actual index into the container
	func(cont. template item_at< iv_ordinals_t::template type_at< 0>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 1>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 2>::type::value >());
}

template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal_impl(mpl::int_<4>, container_t & cont, functor_t & func)
{
	// access the iv_ordinals_t to retrieve the actual index into the container
	func(cont. template item_at< iv_ordinals_t::template type_at< 0>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 1>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 2>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 3>::type::value >());
}

template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal_impl(mpl::int_<5>, container_t & cont, functor_t & func)
{
	// access the iv_ordinals_t to retrieve the actual index into the container
	func(cont. template item_at< iv_ordinals_t::template type_at< 0>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 1>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 2>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 3>::type::value >());
	func(cont. template item_at< iv_ordinals_t::template type_at< 4>::type::value >());
}

// accesses the iv_ordinals_t to retrieve the actual index into the container
template <class iv_ordinals_t, class container_t, class functor_t>
void for_each_ordinal( container_t & cont, functor_t func)
{
	for_each_ordinal_impl<iv_ordinals_t>(typename iv_ordinals_t::size(), cont, func);
}

}; // namespace bag
}; // namespace boost

#endif



```

## Bag Visitation Library- Compile Time Meta Program

```c++
/*  
 *   Brian Braatz.
 *   Copyright 2003-2012 Brian C Braatz. All rights reserved.
 *
 */

#ifndef __BOOST_BAG_FOR_ZIP_HPP
#define __BOOST_BAG_FOR_ZIP_HPP

#include <boost/mpl/not_equal_to.hpp>
#include <boost/mpl/comparison.hpp>
#include <boost/mpl/and.hpp>
#include <boost/mpl/logical.hpp>

namespace boost { namespace bag {

namespace _mpl_ = boost::mpl; // note: it is a common practice to use a "mpl" as an alias- though using it inside THIS namespace
							  // creates issues- so inside the bag namespace we use "_mpl_"

// tags for traits

struct static_model		{ typedef static_model type; };		// static model requires compile time recursion to iterate
struct dynamic_model	{ typedef dynamic_model type; };	// dynamic model changes at runtime

// returns true if the passed type has a nested model typedef which == static_model
template <class model_t>
struct is_model_static_impl
: _mpl_::false_	{};

template <>
struct is_model_static_impl<static_model>
: _mpl_::true_	{};

template <class T>
struct is_model_static : is_model_static_impl<typename T::model>	{};

template <class T>
struct not_is_model_static : _mpl_::not_< is_model_static_impl<typename T::model> >	{};

// trait for static_iterator
// provides functionality for iteration over any forward capable mpl sequence
struct mpl_seq
{
	typedef mpl_seq type;

	// expose iteration constructs
	template <class T>	struct begin	: _mpl_::begin<T> {};	
	template <class T>	struct end		: _mpl_::end<T> {};
	template <class T>	struct next		: _mpl_::next<T> {};
	template <class T>	struct deref	: _mpl_::deref<T> {};

	enum { arg_ordinal = -1}; // -1 means does not want an runtime argument

	// deref function for retrieving the current "value" in the sequence
	template <class static_iterator_t>   
	typename static_iterator_t::deref_current get_value()
	{
		return typename static_iterator_t::deref_current();
	}
	
	// returns mpl::true_ if passed iter is at the last position
	template <class iter_t, class last_iter_t>
	struct is_at_last
	: boost::is_same<iter_t,last_iter_t>
	{};
	
};

// internal trait for a sequence of bag slots
template <int ARG_NUM, class bag_t>
struct bag_slot_seq : mpl_seq 
{
	typedef bag_slot_seq type;

	typedef bag_t bag_type;			// type of bag
	typedef bag_type & arg_type;	// runtime argument type
	typedef bag_type  value_type;   // value type of argument
	
	// returns the return type of the passed iterator applied to the bag
	template <class static_iterator_t>   
	struct return_type
	: bag_type::config::template return_type_at
		<
			static_iterator_t::deref_current::value 
		>
	{};
	
	enum { arg_ordinal = ARG_NUM}; // initial argument ordinal desired
};

// static iterator and static_iterator_next are helpers for compile time iteration
// having these classes allows the (further) rather complicated code 
// which determines which argument to send to a function to be considerably simpler
template <class iter_traits_t, class seq_t, class current_iter_t  >
struct static_iterator_ : iter_traits_t
{
	typedef static_iterator_		type;			// type for this iter
	typedef iter_traits_t			iter_traits;	// traits for this iter
	typedef static_model			model;			// model for this iter
	typedef current_iter_t			current;		// current iter
	typedef typename seq_t::type 	seq_type;	

	typedef typename iter_traits:: template end<seq_type>::type				last;			// iter for last position
	typedef typename iter_traits:: template deref<current_iter_t>::type		deref_current;	// current iter - dereferenced
	
	enum { arg_ordinal = iter_traits_t::arg_ordinal}; // argument desried

	// is last ?
	typedef typename iter_traits:: template is_at_last<current, last>::type is_past_last_type;

	// is last - constant
	enum { is_past_last =   is_past_last_type::value };
};

template <class iter_traits_t, class seq_t >
struct static_iterator : static_iterator_<iter_traits_t, seq_t, typename iter_traits_t::template begin<seq_t>::type>
{};

template <class static_iterator_t>
struct static_iterator_next : static_iterator_t::iter_traits
{
	typedef static_iterator_next						type;			// type for this iter
	typedef typename static_iterator_t::iter_traits		iter_traits;	// traits for this iter
	typedef typename static_iterator_t::last			last;			// iter for last position
	
	typedef typename iter_traits::template next< typename static_iterator_t::current >::type		current;	// current iter
	typedef typename iter_traits::template deref<current>::type									deref_current;	// current iter - dereferenced

	// is last ?
	typedef typename iter_traits:: template is_at_last<current, last>::type is_past_last_type;

	enum { arg_ordinal = iter_traits::arg_ordinal}; // argument desried

	// is last
	enum { is_past_last =   is_past_last_type::value };
};

// template for determining if a iter is past the last value
template  <class static_iterator_t>
struct is_static_iterator_past_last
: static_iterator_t::is_past_last_type {};

// template for dereferencing the iter
template <class static_iter_t>
struct get_deref_current
{
	typedef typename static_iter_t::deref_current type;
};

// returns int_<0> if not at last otherwise returns int_<1>
template <class static_iter_t>
struct is_past_last_to_int
{
	typedef typename _mpl_::if_< _mpl_::bool_< static_iter_t::is_past_last > , _mpl_::int_<1>, _mpl_::int_<0> >::type type;
};

// holds multiple static iterators and allows them to simultanesouly iterate
template <class seq_static_iterators_t>
struct static_iterator_collection
{
	typedef static_iterator_collection type;						// type of this class
	typedef typename seq_static_iterators_t::type static_iterators;	// sequence of iterators we manage
	
	// returns a new static_iterator_collection, with all the iter elements incremented
	typedef static_iterator_collection
	<
		typename _mpl_::transform
		<
			static_iterators,
			static_iterator_next< _mpl_::_1>  
		>::type 
	> next;
	
	private:
		// list of just the values of the iterators past_last values
		typedef typename _mpl_::transform
		<
			static_iterators,
			is_past_last_to_int<_mpl_::_1>
		>::type vec_is_past_lasts;

	public:
	// number of contained iters which are past last
	typedef typename _mpl_::count_if
	<
		static_iterators,
		is_static_iterator_past_last<_mpl_::_1>
	>::type num_past_last;
	
	// if the number of iters which report past_last is greater than 0
	typedef typename _mpl_::if_
	< 
		_mpl_::greater< num_past_last, _mpl_::int_<0> >,
		_mpl_::true_, 
		_mpl_::false_ 

	>::type is_past_last_type; 

	// true if any contined iters are past the last
	enum 
	{ is_past_last =  is_past_last_type::value };
};


// zip trait for forward iteration over a std container
template <int ARG_NUM, class T>
struct std_each
{
	typedef T &							arg_type;			// argument type
	typedef dynamic_model				model;				// model for trait
	typedef _mpl_::true_					argument_wanted;	// defines if this trait receieves a runtime argument
	typedef typename T::value_type &	return_type;		// return type (passed to functor)
	
	enum { arg_ordinal = ARG_NUM};			// ordinal of argument we wish to receieve
	
	typename T::iterator it_current;		// iterator for current element
	typename T::iterator it_end;			// iterator for ending element
	
	// receieves the arg - expecting the arg denoted via arg_ordinal
	void receieve_arg( arg_type arg)
	{
		it_current	= arg.begin();
		it_end		= arg.end();
	}
	
	// returns the current value pointed to
	return_type get_value()
	{
		return*it_current;
	};

	// increments the runtime iterator
	void increment()
	{
		it_current++;
	}

	// returns true if runtime iterator is at end
	bool at_end()
	{
		return (it_current == it_end);
	}
};

// zip trait for passing a reference argument to a functor
template <int ARG_NUM, class T>
struct ref_argument
{
	typedef T &					arg_type;			// argument type
	typedef T &					return_type;		// return type
	typedef dynamic_model		model;				// model for trait
	typedef _mpl_::true_			argument_wanted;	// defines if this trait receieves a runtime argument

	enum { arg_ordinal = ARG_NUM};					// ordinal of argument we wish to receieve

	T * m_pVal;										// storage of argument
	
	// receieves the arg - expecting the arg denoted via arg_ordinal
	void receieve_arg( arg_type arg)
	{
		m_pVal = &arg; 
	}

	// increments the runtime iterator
	void increment()
	{
		// NOP
	}

	// returns the current value pointed to
	arg_type get_value()
	{
		return *m_pVal;
	};

	// returns true if runtime iterator is at end
	bool at_end()
	{
		return false;
	}
};

// public class for wrappering call to static_iterator
template <class seq_t>
struct mpl_each
: static_iterator<mpl_seq, seq_t >
{

};

template <int ARG_NUM = -1, class T0= _mpl_::void_, class T1= _mpl_::void_, class T2= _mpl_::void_>
struct bag_each_slot;
		
template <int ARG_NUM, class bag_t>
struct bag_each_slot<ARG_NUM, bag_t>
: static_iterator< bag_slot_seq<ARG_NUM,bag_t> , typename bag_t::config::slot_ordinals >
{ };


template <int ARG_NUM, class bag_t, class filter_t>
struct bag_each_slot<ARG_NUM, bag_t, filter_t>
: static_iterator< bag_slot_seq<ARG_NUM,bag_t> , typename bag_t::config::template filtered_ordinals<filter_t >::type >
{ };

template <class T0= _mpl_::void_, class T1= _mpl_::void_, class T2= _mpl_::void_>
struct bag_each_slot_ordinal;
		
template <class bag_t>
struct bag_each_slot_ordinal< bag_t>
: static_iterator<mpl_seq, typename bag_t::config::slot_ordinals >
{ };


template < class bag_t, class filter_t>
struct bag_each_slot_ordinal<bag_t, filter_t>
: static_iterator<mpl_seq, typename bag_t::config::template filtered_ordinals<filter_t >::type  >
{ };



// functor for calling the increment() method
struct increment_all
{
	template <class T>
	void operator()(T& obj)
	{
		obj.increment();
	}
};

// bag of functor arguments
template <class seq_t>
struct bag_functor_arguments : bag< seq_t >
{
	private:
		struct any_at_end_fctor	// todo replace this with for_until
		{
			bool & bRet;
			any_at_end_fctor(bool & b)
			: bRet(b)	{};
			
			template <class TT>
			void operator()(TT & v)
			{
				if (v.at_end())
					bRet = true;
			}
		};
		
	public:
		// returns true if any arguments are "at the end"
		bool any_at_end()
		{
			bool bRet = false;
			any_at_end_fctor f(bRet);
			for_each(f);
			
			return bRet;
		}
		// type of this class
		typedef  bag_functor_arguments type;
};

// holds a ordinal which reference the location of a type in a static sequence
// (static_ord_ref  implementation for a static trait which DOES NOT WANT a runtime argument)
template <class iter_t, class Enable = void>
struct static_ord_ref 
{
	typedef static_ord_ref				type;
	typedef static_model				model;
	typedef typename iter_t::pos		pos;
	enum { value = pos::value	};		// the position in the static iter collection we point to
	
	typedef _mpl_::false_ argument_wanted;

	void increment()
	{
		// NOP
	}
};

// (static_ord_ref implementation for a static trait which wants a runtime argument)
template 
<
	class iter_t
>
struct static_ord_ref
<
	iter_t, 
	typename boost::enable_if 
	< 
		_mpl_::not_equal_to	// arg ordinal of dereferenced iter is not equal to -1
		< 
			_mpl_::int_< _mpl_::deref<iter_t >::type::arg_ordinal>, 
			_mpl_::int_<-1> 
		> 
	>::type 
> 
{
	typedef static_ord_ref				type;
	typedef static_model				model;
	typedef typename iter_t::pos		pos;
	enum { value = pos::value	};		// the position in the static iter collection we point to
	typedef typename _mpl_::deref<iter_t >::type static_trait;
	
	enum { arg_ordinal = static_trait::arg_ordinal };	// arg ordinal desired
	
	typedef _mpl_::true_ argument_wanted;					// this class DOES want a runtime arg
							
	void increment()
	{
		// NOP
	}
	typename static_trait::value_type * pValue;			// member for holding onto the argument 

	typedef typename static_trait::value_type &	return_type;
	
	static_ord_ref()									// default ctor
	: pValue(0) {};
	
	void receieve_arg( typename static_trait::arg_type arg)	// receieve an argument
	{
		pValue = &arg;
	}
	
	return_type get_value()								// return the argument
	{
		assert(pValue);
		return *pValue;
	}
};

// returns true of the passed trait wants an argument
template <class T>
struct wants_argument
: T::argument_wanted::type
{};

template <class T>
struct not_wants_argument
: _mpl_::not_< typename T::argument_wanted::type>
{};

template <class T>
struct is_static_ord_ref
: _mpl_::false_	{};

template <class iter_t, class enable_t>
struct is_static_ord_ref< static_ord_ref<iter_t, enable_t> >
: _mpl_::true_	{};


// given a iter, returns a static ordinal ref
template <class iter_t>
struct ordinal_ref
 : static_ord_ref< iter_t> 
{};

// functor for dispatching arguments
// takes a bag of arguments
/// on the operator()(), will ask the type of the passed
// operand which ordinal in the arg bag it wishes to receieve
// will then pass that item to the object
template <class bag_zip_args_t>
struct assign_arguments
{
	bag_zip_args_t & m_bag_zip_args;
	
	assign_arguments(bag_zip_args_t & bag_zip_args)
	: m_bag_zip_args(bag_zip_args)	{}
	
	template <class T>
	void operator()(T& obj)
	{	
		obj.template receieve_arg(	m_bag_zip_args.template item_at<T::arg_ordinal>() );
	}
};
/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

/*	
	takes item ordinal & static iter collection
	
	grab the item type from the bag
	
	ask the item type if it is a static model or not
	
	if it is dynamic
	
		grab the arg from the bag and return that
	if it IS static- then ask for the ordinal in the collection 
		return the value that that iter returns
*/

template <class static_iter_col, class bag_t>
struct get_dispatch_args
{
	typedef get_dispatch_args type;

	template <int iOrdinal, class Enable = void>
	struct arg
	{
		bag_t & m_bg;
		arg(bag_t & bg)
		: m_bg(bg) {};
		
		// ask the bag for the return type trait of current ordinal
		typedef typename bag_t::config::template return_type_at<iOrdinal>::type return_type_trait;
		
		// pull the return type from the trait
		typedef typename return_type_trait::return_type return_type;
		
		// member for pulling the value out of the bag
		return_type get_value()
		{
			// from the arg bag, look up the item at passed ordinal and return value
			return m_bg.template item_at<iOrdinal>().get_value();
		}
	};
	
	// arg retreival for static ord ref items which do not take an argument
	template <int iOrdinal>
	struct arg
	< 
		iOrdinal, 
		typename boost::enable_if
		< 
			_mpl_::and_
			<
				// is it a static ordinal reference?
				is_static_ord_ref < typename bag_t::config:: template key_at< iOrdinal>::type >, 
				// does it NOT want a runtime argument?
				not_wants_argument < typename bag_t::config:: template key_at< iOrdinal>::type >
			>
		>::type 
	>
	{
		arg(bag_t & ) 
		{};
		
		// bag item (static_ord_ref)
		typedef typename bag_t::config:: template key_at< iOrdinal>::type bag_item;
		
		// find the iter for the current iteration
		typedef typename at_pos<typename static_iter_col::static_iterators , typename bag_item::pos>::type static_iter;

		// deref the type the iter is pointing to
		typedef typename static_iter::deref_current return_type;
		
		// go to the iter and ask for the value
		return_type get_value()
		{
			// get the value from the static iter
			return static_iter().template get_value<static_iter>();			
		}
	};
	// arg retreival for static ord ref items which DO take an argument
	template <int iOrdinal>
	struct arg
	< 
		iOrdinal, 
		typename boost::enable_if
		< 
			_mpl_::and_
			<
				// is it a static ordinal reference?
				is_static_ord_ref < typename bag_t::config:: template key_at< iOrdinal>::type >, 
				// DOES it want a runtime argument?
				wants_argument < typename bag_t::config:: template key_at< iOrdinal>::type >
			>
		>::type 
	>
	{
		bag_t & m_bg;
		arg(bag_t & bg) 
		: m_bg(bg) {};
		
		// bag item (static_ord_ref)
		typedef typename bag_t::config:: template key_at< iOrdinal>::type bag_item;
		
		// find the iter for the current iteration
		typedef typename at_pos<typename static_iter_col::static_iterators , typename bag_item::pos>::type static_iter;

		// goto the current iter and ask it for the return type
		typedef typename static_iter::template return_type<static_iter>::type return_type;

		// go to the iter and ask for the value
		return_type & get_value()
		{
			/* 
				go to the bag of functor args,
					get the value of the bag being held there (originally passed into for_zip)
						retrieve the item of that bag at the position denoted by value pointed to by static_iter
			*/
			return 
				m_bg.template item_at<iOrdinal>(). 
					template get_value(). 
						template item_at< static_iter::deref_current::value >();
		}
	};
};

template <class static_iter_col, class bag_t, class func_t>
inline void dispatch_args
( 
	bag_t & bg_args,				// bag of arguments
	func_t & func,					// functor to visit with
	_mpl_::long_<1> bag_slot_size		// bag slot size
)
{
	typedef get_dispatch_args<static_iter_col, bag_t> gda;
	func( 
		typename gda:: template arg<0>(bg_args).get_value()
		);
}

template <class static_iter_col, class bag_t, class func_t>
inline void dispatch_args
( 
	bag_t & bg_args,				// bag of arguments
	func_t & func,					// functor to visit with
	_mpl_::long_<2> bag_slot_size		// bag slot size
)
{
	typedef get_dispatch_args<static_iter_col, bag_t> gda;
	func( 
		typename gda:: template arg<0>(bg_args).get_value(),
		typename gda:: template arg<1>(bg_args).get_value()
		);
}

template <class static_iter_col, class bag_t, class func_t>
inline void dispatch_args
( 
	bag_t & bg_args,				// bag of arguments
	func_t & func,					// functor to visit with
	_mpl_::long_<3> bag_slot_size		// bag slot size
)
{
	typedef get_dispatch_args<static_iter_col, bag_t> gda;
	func( 
		typename gda:: template arg<0>(bg_args).get_value(),
		typename gda:: template arg<1>(bg_args).get_value(), 
		typename gda:: template arg<2>(bg_args).get_value()
		);
}
template <class static_iter_col, class bag_t, class func_t>
inline void dispatch_args
( 
	bag_t & bg_args,				// bag of arguments
	func_t & func,					// functor to visit with
	_mpl_::long_<4> bag_slot_size		// bag slot size
)
{
	typedef get_dispatch_args<static_iter_col, bag_t> gda;
	func( 
		typename gda:: template arg<0>(bg_args).get_value(),
		typename gda:: template arg<1>(bg_args).get_value(), 
		typename gda:: template arg<2>(bg_args).get_value(),
		typename gda:: template arg<3>(bg_args).get_value()
		);
}
template <class static_iter_col, class bag_t, class func_t>
inline void dispatch_args
( 
	bag_t & bg_args,				// bag of arguments
	func_t & func,					// functor to visit with
	_mpl_::long_<5> bag_slot_size		// bag slot size
)
{
	typedef get_dispatch_args<static_iter_col, bag_t> gda;
	func( 
		typename gda:: template arg<0>(bg_args).get_value(),
		typename gda:: template arg<1>(bg_args).get_value(), 
		typename gda:: template arg<2>(bg_args).get_value(),
		typename gda:: template arg<3>(bg_args).get_value(),
		typename gda:: template arg<4>(bg_args).get_value()
		);
}

template< bool done = true> // true == recursion done
struct zip_each_impl
{
	template <class curr_static_iter_t, class arg_t, class func_t>
	inline void call_each(arg_t & args, func_t & func )
	{
		// do nothing
	}
};

// zip each static iter and runtime iter 
template <>
struct zip_each_impl<false> // false == not done
{
	template <class static_iter_col, class arg_t, class func_t>
	inline void call_each(arg_t & args, func_t & func )
	{
//		if(! args.any_at_end())
		{
			// call functor with args:
			// pass the static iterator collection , bag of args and functor
			// to a function which will look up the appropriate arg for each functor param
			// and pass accordingly
			dispatch_args<static_iter_col>( args, func, typename arg_t::config::slot_size::type () );

			// increment all rt iters
			args.for_each(increment_all());
			
			// incremenet all static iters
			typedef typename static_iter_col::next next_static_iter_col;
			
			//initiate recursion \ visitation
			zip_each_impl
			< 
				 next_static_iter_col::is_past_last

			>(). template call_each<next_static_iter_col>(args,func);
		} 
	}
};

// receieves a variable number of arguments and boot-straps up the recursion \ visiation
template <class seq_zip_traits_t, class seq_zip_args_t, class functor_t >
struct zip_visitation_impl
{
	// define a bag of the zip_arguments
	typedef bag
	< 
		single<by_ref>, 
		seq_zip_args_t			// argument types
	
	> bag_zip_args_type;

	// build a bag of the zip_arguments
	bag_zip_args_type bag_zip_args;

	// ctors for arguments
	template <class A0 > zip_visitation_impl(A0 & a0)	
		: bag_zip_args(a0) {};
	template <class A0, class AA1> zip_visitation_impl(A0 & a0, AA1 & a1) 
		: bag_zip_args(a0, a1) {};
	template <class A0, class A1, class A2> zip_visitation_impl(A0 & a0, A1 & a1, A2 & a2) 
		: bag_zip_args(a0, a1, a2) {};
	template <class A0, class A1, class A2, class A3> zip_visitation_impl(A0 & a0, A1 & a1, A2 & a2, A3 & a3) 
		: bag_zip_args(a0, a1, a2, a3) {};
	template <class A0, class A1, class A2, class A3, class A4> zip_visitation_impl(A0 & a0, A1 & a1, A2 & a2, A3 & a3, A4 & a4) 
		: bag_zip_args(a0, a1, a2, a3, a4) {};

	void operator()(functor_t & func)
	{
		// make a static collection of just the static_model traits
		typedef  typename _mpl_::copy_if
		<
			seq_zip_traits_t,
			is_model_static < _mpl_::_1 >,
			_mpl_::back_inserter< _mpl_::vector<> >

		>::type vec_static_traits;
		
		// transform vector of passed traits so that static_iterators are changed 
		// to an reference class instead of the actual class
		// reference points to ordinal in vec_static_traits
		typedef  typename _mpl_::transform
		<
			seq_zip_traits_t
			,	_mpl_::if_
				< 
					is_model_static < _mpl_::_1 >,	// if the model of the trait is static,
					ordinal_ref						
					<
						_mpl_::find					// look up the class in the static only list of traits
						<
							vec_static_traits, 
							_mpl_::_1
						>
					>,
					_mpl_::_1							// otherwise, return the existing type
				>
		>::type vec_arg_bag_slots;
		
		// build a bag of the final functor arguments (from the transformed traits)
		// this bag is now in the ORDER of the args that need to go to the functor
		//		and has static_ord_ref() objects in the places where the static iterator(s) need to go
		bag_functor_arguments< vec_arg_bag_slots > bag_fctor_args;

		// give the trait instantiated objects their arguments
		// scan the vector of passed traits- filter out ones which want a runtime argument
		//		look up the object in the functor arguments (via ordinal)
		//		pass it the object from zip_arguments (via trait.receieve_arg() )
		bag_fctor_args.template 
		for_each
		< 
			_mpl_::lambda< wants_argument<_mpl_::_1> > 

		> (assign_arguments<bag_zip_args_type>(bag_zip_args));

		// define collection of static iters
		typedef static_iterator_collection<vec_static_traits> static_iter_col;
		
		//initiate recursion \ visitation
		zip_each_impl
		< 
			 static_iter_col::is_past_last

		>(). template call_each<static_iter_col>(bag_fctor_args,func);
	}
};

// bring in the detailed impl of for_zip
#include <boost/bag/detail/for_zip_impl.hpp>

} } // end namespace boost { end namespace bag {

#endif // #ifndef __BOOST_BAG_FOR_ZIP_HPP



```
