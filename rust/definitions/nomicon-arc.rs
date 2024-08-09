// An atomic reference counter as implemented in The Rustonomicon, Chapter 10.

use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{self, AtomicUsize, Ordering};

pub struct Arc<T> {
    // "NonNull" is covariant over "T". Pointer is never null.
    ptr: NonNull<ArcInner<T>>,
    // At some point, there will be an instance of this
    // structure that entirely owns its data.
    // The "PhantomData" marker tells the drop checker
    // that "Arc" has ownership over "ArcInner<T>".
    phantom: PhantomData<ArcInner<T>>,
}

pub struct ArcInner<T> {
    rc: AtomicUsize,
    data: T,
}

impl<T> Arc<T> {
    pub fn new(data: T) -> Arc<T> {
        // Reference count starts at 1, the current pointer.
        let boxed = Box::new(ArcInner {
            rc: AtomicUsize::new(1),
            data,
        });
        Arc {
            // It is okay to call "unwrap" as the pointer from
            // "Box::into_raw" is guaranteed to be non-null.
            ptr: NonNull::new(Box::into_raw(boxed)).unwrap(),
            phantom: PhantomData,
        }
    }
}

// Ensure the data type "T" contained within "Arc"
// is synchronizable and sendable.
unsafe impl<T: Sync + Send> Send for Arc<T> {}
unsafe impl<T: Sync + Send> Sync for Arc<T> {}

impl<T> Deref for Arc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        let inner = unsafe { self.ptr.as_ref() };
        &inner.data
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Arc<T> {
        let inner = unsafe { self.ptr.as_ref() };
        // Use relaxed ordering. The internal value is not modified while cloning.
        // Implies no happens-before relationship but is atomic.
        let old_rc = inner.rc.fetch_add(1, Ordering::Relaxed);

        if old_rc >= isize::MAX as usize {
            std::process::abort();
        }

        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        let inner = unsafe { self.ptr.as_ref() };
        if inner.rc.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        // Ensures data use happens before decreasing the reference count,
        // which happens before this fence, which happens before the
        // deletion of data.
        atomic::fence(Ordering::Acquire);
        // "Box" will take ownership of this pointer and subsequently run its
        // destructor over "T" and free allocated memory.
        // Operation is safe. We have the last valid pointer to the "ArcInner".
        unsafe { Box::from_raw(self.ptr.as_ptr()); }
    }
}
