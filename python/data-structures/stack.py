# Stack() -> Stack
# Wraps list in a series of methods that implement a stack.
# .push(item) appends variable number of items to top of stack.
# stack.push(1, 2, 3)
# .pop() removes top item from stack and returns it to the caller. Returns None if stack is empty.
# stack.pop() -> 3
# .peek() returns top item to caller without removing it from the stack. Returns None if stack is empty.
# stack.peek() -> 2
# .empty() If stack is empty, returns boolean True. Otherwise returns boolean False.
# stack.empty() -> False

class Stack:
    def __init__(self):
        self.store = list()
        
    def push(self, *items):
        self.store.extend(items)
        
    def pop(self):
        if len(self.store) == 0:
            return None
        else:
            return self.store.pop()
          
    def peek(self):
        if len(self.store) == 0:
            return None
        else:
            return self.store[-1]
          
    def empty(self):
        return len(self.store) == 0
