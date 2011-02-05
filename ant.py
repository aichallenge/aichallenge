class Ant:
    def __init__(self, x, y, owner):
        self.x = x
        self.y = y
        self.owner = owner
    def __repr__(self):
        return '(%s,%s,%s)' % (self.x, self.y, self.owner)
