class Histogram:
    def __init__(self, width, height, char):
        self.width = width
        self.height = height
        self.char = char

    def printHistogram(self, values, fmt, stream):
        values = [x for x in values] # copy
        values.sort();
        values.reverse()
        data = [ ['.' for y in range(self.height)] for x in range(self.width)]
        maxval = max(values) + 1e-7
        for x in range(self.width):
            sample = int(x * 1.0 / self.width * len(values))
            h = int(values[sample] * 1.001 / maxval  * self.height)
            for y in range(h):
                data[x][y] = self.char
        for y in range(self.height):
            for x in range(self.width):
                stream.write(data[x][self.height - 1 - y])
            print >>stream, fmt % (maxval * (self.height - y) / self.height)
            #stream.write('\n')
        return
