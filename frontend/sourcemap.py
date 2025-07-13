# a mapping from byte offsets -> line+column positions.
class Sourcemap:
    def __init__(self, text):
        self.text = text
        self.lines = text.split("\n")
        self.offsets = []

        offset = 0
        for line in self.lines:
            offsets.append(offset)
            offset += len(line)
