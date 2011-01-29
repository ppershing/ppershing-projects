// created and maintained by ppershing
// please report any bug or suggestion to ppershing<>fks<>sk
#ifndef H_CIRCULAR_BUFFER
#define H_CIRCULAR_BUFFER

#include <vector>
#include <iterator>
#include "Exceptions.h"

/**
  Circular buffer is convenient way how to maintain fixed number
  of history records. It is like STL container, data[0] refers to
  last added thing, older data are indexed by [1] ... [size()-1].
  Also, it have implemented iterator, so one can use algorithms
  */

template<class T>
class CircularBuffer {
    private:
        /**
          actual data
          */
        std::vector<T> data;

        /**
          position of start, 
          [0]=data[start], [1]=data[start-1] ...
          */
        unsigned int start;

    public:
        /**
          get size
          */
        int size()const {
            return data.size();

        }
        /**
          public constructor, default data size is 2
          (minimum)
          */
        // {{{
        CircularBuffer():data(std::vector<T>(2)),start(0){
        }
        // }}}

        /**
          public constructor with defined data size
          */
        // {{{
        CircularBuffer(int size):start(0){
            if (size<2) throw EIllegalArgument(
                    "CircularBuffer size too small");
            data=std::vector<T>(size);
        }
        // }}}

        /**
          destructor
          */
        // {{{
        ~CircularBuffer(){
        };
        // }}}

        /**
          resize buffer
          note that it is not efective and invalidates
          all iterators
          */
        // {{{
        void resize(const int newSize){
            if (newSize<2) throw EIllegalArgument(
                    "CircularBuffer:: resize, new size is too small");
            if (1+start!=data.size()) {
                std::rotate(data.begin(),data.begin()+data.size(),data.end());
            }
            std::reverse(data.begin(),data.end());
            data.resize(newSize);
            std::reverse(data.begin(),data.end());
            start=(int)data.size()-1;
        }
        // }}}


        /**
          returns i-th newest element of buffer
          */
        // {{{
        inline T& operator[](const unsigned int pos){
            if (pos>=data.size()) throw EIllegalArgument(
                    "index out of bounds");
            if (pos<=start) return data[start-pos];
            else return data[data.size()+start-pos];
        }
        // }}}

        /**
          inserts new element to buffer
          */
        // {{{
        void insert(const T& item){
            start++;
            if (start==data.size()) start=0;
            data[start]=item;
        }
        // }}}

    // {{{ iterator
    /**
      (bidirectional) iterator over CircularBuffer
      */
    class iterator;
    friend class iterator;

// {{{ iterator
    class iterator: public 
    std::iterator<std::bidirectional_iterator_tag,T,
    ptrdiff_t>{
        /**
          we are using vector's iterator
          */
        typename std::vector<T>::iterator it;

        /**
          also we need pointer to data
          */
        std::vector<T>* r;

        /**
          and position of start in data
          */
        unsigned int start;

        public:
            /**
              iterator constructor
              */
            iterator(std::vector<T>& data,
            const typename std::vector<T>::iterator& i,unsigned int
            st): it(i),r(&data),start(st){}

            /**
              equality cheack
              */
            bool operator==(const iterator& x) const{
                return it==x.it;
            }

            /**
              inequality check
              */
            bool operator!=(const iterator& x) const{
                return !(*this==x);
            }

            /**
              reference operator
              */
            typename std::vector<T>::reference operator*() const {
                return *it;
            }

        private:
            /**
              return offset of iterator from start
              */
            int getOffset() const{
                unsigned int offset=this->it-r->begin(); 
                if (offset<=start) return start-offset;
                else return start+r->size()-offset;

            }

        public:
            /**
              return difference between two iterators
              */
            int operator-(const iterator& x){
                return this->getOffset()-x.getOffset();
            }

            /**
              pre-decrement
              */
            iterator& operator--(){
                ++it;
                if (it == r->end()) 
                    it=r->begin();
                return *this;
            }

            
            /**
              post-decrement
              */
            iterator operator--(int){
                iterator tmp=*this;
                --*this;
                return tmp;
            }
            
        
            /**
              pre-increment
              */
            iterator& operator++(){
                if (it == r->begin()) 
                    it=r->end();
                --it;
                return *this;
            }

            
            /**
              post-increment
              */
            iterator operator++(int){
                iterator tmp=*this;
                ++*this;
                return tmp;
            }
            
    };
// }}}

    /**
      return iterator to newest element
      */
    iterator begin() {
        return iterator(data,data.begin()+start,start);
    }

    /**
      return iterator to oldest element
      */
    iterator end() {
        iterator i=iterator(data,data.begin()+start,start);
        --i;
        return i;
    }
    // }}}

    // {{{ reverse iterator

    /**
      (bidirectional) reverse iterator over CircularBuffer
      */
    class reverse_iterator;
    friend class reverse_iterator;

// {{{ reverse_iterator
    class reverse_iterator: public 
    std::iterator<std::bidirectional_iterator_tag,T,
    ptrdiff_t>{
        /**
          we are using vector's iterator
          */
        typename std::vector<T>::iterator it;

        /**
          also we need pointer to data
          */
        std::vector<T>* r;

        /**
          and position of start in data
          */
        unsigned int start;

        public:
            /**
              iterator constructor
              */
            reverse_iterator(std::vector<T>& data,
            const typename std::vector<T>::iterator& i,unsigned int
            st): it(i),r(&data),start(st){}

            /**
              equality cheack
              */
            bool operator==(const reverse_iterator& x) const{
                return it==x.it;
            }

            /**
              inequality check
              */
            bool operator!=(const reverse_iterator& x) const{
                return !(*this==x);
            }

            /**
              reference operator
              */
            typename std::vector<T>::reference operator*() const {
                return *it;
            }

        private:
            /**
              return offset of iterator from start
              */
            int getOffset() const{
                unsigned int offset=this->it-r->begin(); 
                if (offset<=start) return start-offset;
                else return start+r->size()-offset;

            }

        public:
            /**
              return difference between two iterators
              */
            int operator-(const reverse_iterator& x){
                return -this->getOffset()+x.getOffset();
            }

            /**
              pre-decrement
              */
            reverse_iterator& operator--(){
                if (it == r->begin()) 
                    it=r->end();
                --it;
                return *this;
            }

            
            /**
              post-decrement
              */
            reverse_iterator operator--(int){
                reverse_iterator tmp=*this;
                --*this;
                return tmp;
            }
            
        
            /**
              pre-increment
              */
            reverse_iterator& operator++(){
                ++it;
                if (it == r->end()) 
                    it=r->begin();
                return *this;
            }

            
            /**
              post-increment
              */
            reverse_iterator operator++(int){
                reverse_iterator tmp=*this;
                ++*this;
                return tmp;
            }
            
    };
// }}}

    /**
      return iterator to newest element
      */
    reverse_iterator rbegin() {
        reverse_iterator i=reverse_iterator(data,data.begin()+start,start);
        ++i;
        return i;
    }

    /**
      return iterator to oldest element
      */
    reverse_iterator rend() {
        return reverse_iterator(data,data.begin()+start,start);
    }

    // }}}

};

#endif
