from fractions import gcd

def lcm(numbers):
    return reduce(lambda x, y: (x*y)/gcd(x,y), numbers, 1)

print lcm((3, 4))
print lcm((3, 6))
print lcm((4, 6))
print lcm((3, 6))
print lcm(range(1, 20))
