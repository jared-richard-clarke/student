// === A spin lock as implemented by Mara Bos in "Rust Atomics and Locks" ===
// A spin lock is a mutex that busy-loops, or spins, while waiting on a lock.
// Spinning can reduce latency, but it can also waste clockcycles and
// reduce performance.

use std::ops::{Deref, DerefMut};
use std::cell::UnsafeCell;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::{Acquire, Release};

pub struct SpinLock<T> {
    locked: AtomicBool,
    // Data can be mutated, even though the spinlock itself is shared by immutable reference.
    // Interior mutability is provided by "UnsafeCell".
    value: UnsafeCell<T>,
}

// "UnsafeCell" does not automatically implement "Sync". We promise the compiler that
// "UnsafeCell" is sychronisable as long its interior type implements "Send".
unsafe impl<T> Sync for SpinLock<T> where T: Send {}

// "Guard" provides a safe interface to "SpinLock" by tying "Spinlock's" unlocking
// mechanism to its "Drop" implementation. A "Guard" can only be created by calling
// "lock" on "SpinLock", therefore "Guard" implies a locked "SpinLock".
pub struct Guard<'a, T> {
    lock: &'a SpinLock<T>,
}

// Ensure "Guard" is "Sync" only when its internal type "T" is "Sync".
unsafe impl<T> Sync for Guard<'_, T> where T: Sync {}

impl<T> SpinLock<T> {
    pub const fn new(value: T) -> Self {
        Self {
            locked: AtomicBool::new(false),
            value: UnsafeCell::new(value),
        }
    }

    pub fn lock(&self) -> Guard<T> {
        while self.locked.swap(true, Acquire) {
            // Emits a machine instruction to signal to the processor
            // that it is running in a busy-wait spin-loop.
            std::hint::spin_loop();
        }
        Guard { lock: self }
    }
}

impl<T> Deref for Guard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        // Safety: Guard guarantees exclusive access.
        unsafe { &*self.lock.value.get() }
    }
}

impl<T> DerefMut for Guard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        // Safety: Guard guarantees exclusive access.
        unsafe { &mut *self.lock.value.get() }
    }
}

// Hides the unsafe "unlock" operation.
impl<T> Drop for Guard<'_, T> {
    fn drop(&mut self) {
        self.lock.locked.store(false, Release);
    }
}

#[test]
fn main() {
    use std::thread;
    let x = SpinLock::new(Vec::new());
    thread::scope(|s| {
        s.spawn(|| x.lock().push(1));
        s.spawn(|| {
            let mut g = x.lock();
            g.push(2);
            g.push(2);
        });
    });
    let g = x.lock();
    assert!(g.as_slice() == [1, 2, 2] || g.as_slice() == [2, 2, 1]);
}
