def count_alnum(s):
    alphabetic = 0
    numbers = 0
    def categorize(c):
        if c.isalpha():
            alphabetic += 1
        elif c.isdigit():
            numbers += 1
    map(categorize, s)
    return alphabetic, numbers


print count_alnum('hi7 y7')