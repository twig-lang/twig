class Peek:
  def __init__(self, gen):
    self.generator = gen
    self.peeked = None

  def peek(self):
    if self.peeked is None:
      self.peeked = next(self.generator)

    return self.peeked

  def __next__(self):
    temp = self.peek()
    self.peeked = None
    return temp

  def next(self):
    return self.__next__()

def peekable(iter):
  return Peek(iter)
