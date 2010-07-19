##int solve(int m, int *Y, int n, int *R) {
##    if(m == 0) return 1;
##    if(n == 0) return 0;
##
##    R[0] = 0;
##    if(solve(m, Y+1, n-1, R+1)) {
##        return 1;
##    } else {
##        R[0] = 1;
##        return solve(m-Y[0], Y+1, n-1, R+1);
##    }
##}

import itertools as it

def solve(m, Y, R, i):
    #print m, Y, R
    if m == 0:
        return True
    if i == len(Y):
        return False

    R[i] = not R[i]
    if solve(m-Y[i], Y, R, i+1):
        return True
    else:
        R[i] = not R[i]
        return solve(m, Y, R, i+1)


vectors = [[5, 7, 3, 1],
           [5, 7, 1, 3],
           [8, 2, 2, 1],
           [13, 1, 1, 1],
           [13, 0, 0, 0],
           [0, 10, 0, 3],
           [0, 10, 1, 3],

           [15, -5, 3, 4],
           [15, -5, 4, 3],
           ]

m = 13
for vec in vectors:
    mask = [0, ] * len(vec)
    if solve(m, vec, mask, 0):
        print vec, mask
    else:
        print 'fail', vec, mask

