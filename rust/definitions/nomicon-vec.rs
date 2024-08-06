// === Vec as implemented in the Rustonomicon ===
// Rustonomicon, Chapter 9.11
// https://doc.rust-lang.org/nomicon/vec/vec-final.html
//
// +-------------------------------------+
// | Vec                                 |
// | +---------------------------------+ |
// | | buf | RawVec                    | |
// | |     | +-----------------------+ | |
// | |     | | ptr | NonNull         | | |
// | |     | |     | +-------------+ | | |
// | |     | |     | | pointer | ●-|-|-|-|--> The Heap
// | |     | |     | +-------------+ | | |
// | |     | |-----|-----------------| | |
// | |     | | cap | usize           | | |
// | |     | +-----+-----------------+ | |
// | |-----|---------------------------| |
// | | len | usize                     | |
// | +---------------------------------+ |
// +-------------------------------------+
//
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, NonNull};

struct RawVec<T> {
    ptr: NonNull<T>,
    cap: usize,
}

unsafe impl<T: Send> Send for RawVec<T> {}
unsafe impl<T: Sync> Sync for RawVec<T> {}

impl<T> RawVec<T> {
    fn new() -> Self {
        // This branch should be stripped at compile time.
        let cap = if mem::size_of::<T>() == 0 { usize::MAX } else { 0 };

       // "NonNull::dangling()" returns a placeholder for non-allocated memory.
       // The recommended placeholder is "mem::align_of<T>()", which returns a
       // non-zero "usize", which can be cast to a "*mut T".
       //
       // A zero-sized memory block, which is different than non-allocated memory,
       // is not allowed by the global allocator.       
        RawVec {
            ptr: NonNull::dangling(),
            cap: cap,
        }
    }

    fn grow(&mut self) {
        // Since we set the capacity to "usize::MAX" when T has size 0,
        // getting to here necessarily means the Vec is overfull.
        assert!(mem::size_of::<T>() != 0, "capacity overflow");

        let (new_cap, new_layout) = if self.cap == 0 {
            (1, Layout::array::<T>(1).unwrap())
        } else {
            // This can't overflow because we ensure "self.cap" <= "isize::MAX".
            let new_cap = 2 * self.cap;

            // "Layout::array" checks that the number of bytes is <= "usize::MAX",
            // but this is redundant since "old_layout.size()" <= "isize::MAX",
            // so the "unwrap" should never fail.
            let new_layout = Layout::array::<T>(new_cap).unwrap();
            (new_cap, new_layout)
        };

        // Ensure that the new allocation doesn't exceed "isize::MAX" bytes.
        assert!(
            new_layout.size() <= isize::MAX as usize,
            "Allocation too large"
        );

        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<T>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };

        // If allocation fails, "new_ptr" will be null, in which case we abort.
        self.ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            // Abort in a platform-specific manner. Panic, unlike abort, unwinds,
            // which may try to allocate memory that isn't there.
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }
}

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let elem_size = mem::size_of::<T>();

        if self.cap != 0 && elem_size != 0 {
            unsafe {
                alloc::dealloc(
                    self.ptr.as_ptr() as *mut u8,
                    Layout::array::<T>(self.cap).unwrap(),
                );
            }
        }
    }
}

pub struct Vec<T> {
    buf: RawVec<T>,
    len: usize,
}

impl<T> Vec<T> {
    fn ptr(&self) -> *mut T {
        self.buf.ptr.as_ptr()
    }

    fn cap(&self) -> usize {
        self.buf.cap
    }

    pub fn new() -> Self {
        Vec {
            buf: RawVec::new(),
            len: 0,
        }
    }
    pub fn push(&mut self, elem: T) {
        if self.len == self.cap() {
            self.buf.grow();
        }
        // By blindly writing to the pointer offset, we avoid evaluating
        // invalid memory through indexing and dereferencing. We also avoid
        // subsequently calling "drop" on an invalid value.
        unsafe {
            ptr::write(self.ptr().add(self.len), elem);
        }

        // Can't overflow, we'll OOM first.
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            // To prevent uninitialized memory, "ptr::read" copies bits from target
            // address and interprets those bits as type "T".
            unsafe { Some(ptr::read(self.ptr().add(self.len))) }
        }
    }

    pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len, "index out of bounds");
        if self.cap() == self.len {
            self.buf.grow();
        }

        unsafe {
            // ptr::copy(source, destination, length)
            ptr::copy(
                self.ptr().add(index),
                self.ptr().add(index + 1),
                self.len - index,
            );
            ptr::write(self.ptr().add(index), elem);
            self.len += 1;
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        assert!(index < self.len, "index out of bounds");
        unsafe {
            self.len -= 1;
            let result = ptr::read(self.ptr().add(index));
            // ptr::copy(source, destination, length)
            ptr::copy(
                self.ptr().add(index + 1),
                self.ptr().add(index),
                self.len - index,
            );
            result
        }
    }

    pub fn drain(&mut self) -> Drain<T> {
        unsafe {
            let iter = RawValIter::new(&self);

            // This is a "mem::forget" safety thing. If Drain is forgotten, we just
            // leak the whole Vec's contents. Also we need to do this *eventually*
            // anyway, so why not do it now?
            self.len = 0;

            Drain {
                iter: iter,
                vec: PhantomData,
            }
        }
    }
}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        while let Some(_) = self.pop() {}
        // Deallocation is handled by RawVec
    }
}

// Deref coercion grants "Vec" access to all the methods implemented
// on an immutable "slice" reference.
impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.ptr(), self.len) }
    }
}

// Deref coercion grants "Vec" access to all the methods implemented
// on a mutable "slice" reference.
impl<T> DerefMut for Vec<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

impl<T> IntoIterator for Vec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> IntoIter<T> {
        unsafe {
            let iter = RawValIter::new(&self);
            let buf = ptr::read(&self.buf);
            mem::forget(self);

            IntoIter {
                iter: iter,
                _buf: buf,
            }
        }
    }
}

struct RawValIter<T> {
    start: *const T,
    end: *const T,
}

impl<T> RawValIter<T> {
    unsafe fn new(slice: &[T]) -> Self {
        RawValIter {
            start: slice.as_ptr(),
            end: if mem::size_of::<T>() == 0 {
                ((slice.as_ptr() as usize) + slice.len()) as *const _
            } else if slice.len() == 0 {
                slice.as_ptr()
            } else {
                slice.as_ptr().add(slice.len())
            },
        }
    }
}

impl<T> Iterator for RawValIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                if mem::size_of::<T>() == 0 {
                    self.start = (self.start as usize + 1) as *const _;
                    Some(ptr::read(NonNull::<T>::dangling().as_ptr()))
                } else {
                    let old_ptr = self.start;
                    self.start = self.start.offset(1);
                    Some(ptr::read(old_ptr))
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let elem_size = mem::size_of::<T>();
        let len = (self.end as usize - self.start as usize)
                  / if elem_size == 0 { 1 } else { elem_size };
        (len, Some(len))
    }
}

impl<T> DoubleEndedIterator for RawValIter<T> {
    fn next_back(&mut self) -> Option<T> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                if mem::size_of::<T>() == 0 {
                    self.end = (self.end as usize - 1) as *const _;
                    Some(ptr::read(NonNull::<T>::dangling().as_ptr()))
                } else {
                    self.end = self.end.offset(-1);
                    Some(ptr::read(self.end))
                }
            }
        }
    }
}

pub struct IntoIter<T> {
    // We don't actually care about this. Just need it to live.
    _buf: RawVec<T>, 
    iter: RawValIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        for _ in &mut *self {}
    }
}

pub struct Drain<'a, T: 'a> {
    vec: PhantomData<&'a mut Vec<T>>,
    iter: RawValIter<T>,
}

impl<'a, T> Iterator for Drain<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for Drain<'a, T> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<'a, T> Drop for Drain<'a, T> {
    fn drop(&mut self) {
        // pre-drain the iter
        for _ in &mut *self {}
    }
}
