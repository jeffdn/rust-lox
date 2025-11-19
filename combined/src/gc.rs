use std::{
    alloc::{Layout, alloc, dealloc},
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::object::Object;

pub struct ObjHeader {
    pub next: *mut ObjHeader,
    pub is_marked: bool,
}

#[repr(C)]
pub struct GcObject {
    pub header: ObjHeader,
    pub obj: Object,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ObjPtr(pub NonNull<GcObject>);

impl ObjPtr {
    pub fn new(ptr: NonNull<GcObject>) -> Self {
        Self(ptr)
    }

    pub fn as_ptr(&self) -> *mut GcObject {
        self.0.as_ptr()
    }
}

impl Deref for ObjPtr {
    type Target = GcObject;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for ObjPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0.as_ptr() }
    }
}

pub struct Heap {
    pub objects: *mut ObjHeader,
    pub bytes_allocated: usize,
    pub next_gc: usize,
    pub gray_stack: Vec<ObjPtr>,
}

pub trait Trace {
    fn trace(&self, heap: &mut Heap);
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: std::ptr::null_mut(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024 * 1024, // Start with 1GB threshold
            gray_stack: Vec::new(),
        }
    }

    pub fn mark_object(&mut self, mut ptr: ObjPtr) {
        let header = &mut ptr.header;
        if header.is_marked {
            return;
        }
        header.is_marked = true;
        self.gray_stack.push(ptr);
    }

    pub fn trace_references(&mut self) {
        while let Some(ptr) = self.gray_stack.pop() {
            let obj = &ptr.obj;
            obj.trace(self);
        }
    }

    pub fn sweep(&mut self) {
        let mut previous: *mut ObjHeader = std::ptr::null_mut();
        let mut current = self.objects;

        unsafe {
            while !current.is_null() {
                let object = &mut *current;
                if object.is_marked {
                    object.is_marked = false;
                    previous = current;
                    current = object.next;
                } else {
                    let unreached = current;
                    current = object.next;
                    if !previous.is_null() {
                        (*previous).next = current;
                    } else {
                        self.objects = current;
                    }

                    // Free unreached object
                    // We need to reconstruct the layout to dealloc
                    // Since all objects are GcObject, layout is same
                    let layout = Layout::new::<GcObject>();

                    // We also need to drop the payload (Object) to free its resources (like Strings, Vecs)
                    // But `Object` contains `Box` which manages its own memory.
                    // `std::ptr::drop_in_place` will call `drop` on `GcObject`.
                    // `GcObject` contains `Object`. `Object` drop will drop its fields.
                    std::ptr::drop_in_place(unreached as *mut GcObject);
                    dealloc(unreached as *mut u8, layout);

                    self.bytes_allocated -= layout.size();
                }
            }
        }
    }

    pub fn allocate(&mut self, obj: Object) -> ObjPtr {
        let layout = Layout::new::<GcObject>();
        unsafe {
            let ptr = alloc(layout) as *mut GcObject;
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let header = ObjHeader {
                next: self.objects,
                is_marked: false,
            };

            std::ptr::write(ptr, GcObject { header, obj });

            self.objects = ptr as *mut ObjHeader;
            self.bytes_allocated += layout.size();

            // TODO: Trigger GC if bytes_allocated > next_gc

            ObjPtr(NonNull::new_unchecked(ptr))
        }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}
