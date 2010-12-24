import numpy as np

x = np.array([1.0, 0, 1, 0])
y = np.array([0.0, 0, 1, 0])

print (np.dot(x, y)) / (x.sum() * y.sum())
