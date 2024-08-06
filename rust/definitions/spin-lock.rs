// === A spin lock as implemented by Mara Bos in "Rust Atomics and Locks" ===
// A spin lock is a mutex that busy-loops, or spins, while waiting.
// Spinning can reduce latency, but it can also waste clockcycles and
// reduce performance.

use std::ops::{ Deref, DerefMut };
use std::cell::UnsafeCell;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::{ Acquire, Release };

pub struct SpinLock<T> {
    locked: AtomicBool,
    value: UnsafeCell<T>,
}

unsafe impl<T> Sync for SpinLock<T> where T: Send {}

// A lock guard is a special type that represents safe access to a locked lock.
pub struct Guard<'a, T> {
    lock: &'a SpinLock<T>,
}

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
