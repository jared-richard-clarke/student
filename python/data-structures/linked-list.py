# === Linked List: Object Oriented ===
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

# === Linked List: Functional ===
def link(head, tail = None):
    linker = dict()
    linker["head"] = head
    linker["tail"] = tail
    return linker

def traverse(linked_list, action):
    if linked_list["tail"] == None:
        return link(action(linked_list["head"]))
    else:
        return link(action(linked_list["head"]), traverse(linked_list["tail"], action))

# === Examples ===
fun_list = link(1, link(2, link(3)))
square_list = traverse(fun_list, lambda x: x * x)
