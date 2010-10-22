import numpy as np


#x = np.array([1, 2, 3, 2.5, 5, 4, 3.5, 4, 3, 6, 7, 1, 2])
x = np.array([1, 2, 3, 4, 5, 4, 3, 4, 5, 6, 7, 6, 5, 6, 7, 8, 7, 6])
#x = np.array([1, 2, 2, 1, 3, 1])
#dx = np.sign(np.diff(x))
#ddx = dx[:-1]-dx[1:]

#indexes = np.add(np.where(ddx>0), 1)

#values = (x[1:])[ddx>0]

#print indexes, values

ddx = np.diff(x, 2)
indexes = np.add(np.where(ddx<0), 1)
values = (x[1:])[ddx<0]

print indexes, values