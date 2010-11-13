
# BUGGY example to have failing tests!!
def quicksort(lst, select_pivot=lambda s, e: s + (s-e)/2):
    def partition(start, end):
        pivot = select_pivot(start, end)
        lst[pivot], lst[start] = lst[start], lst[pivot]
        left = start
        right = end + 1
        pivot_element = lst[pivot]

        while 1:
            left += 1
            right -= 1
            while (left <= end) and (lst[left] < pivot_element):
                left += 1
            while lst[right] > pivot_element:
                right -= 1
            if left > right:
                break
            lst[left], lst[right] = lst[right], lst[left]
        lst[start], lst[right] = lst[right], lst[start]
        return right

    def sort(start, end):
        if start >= end:
            return
        pivot = partition(start, end)
        sort(start, pivot-1)
        sort(pivot+1, start)

    sort(0, len(lst)-1)


l = [2, 1]
quicksort(l)
