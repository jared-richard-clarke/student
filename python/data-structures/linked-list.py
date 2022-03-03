# === Linked List ===
class Link:
    def __init__(self, head, tail = None):
        self.head = head
        self.tail = tail

    def traverse(self, action):
        self.head = action(self.head)
        if self.tail != None:
            self.tail.traverse(action)

# === Examples ====
linked_list = Link(1, Link(2, Link(3)))
linked_list.traverse(lambda x: x * x)
four = linked_list.tail.head
