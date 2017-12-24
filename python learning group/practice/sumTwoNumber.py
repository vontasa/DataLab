# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class ListNode:
   def __init__(self, x):
        self.val = x
        self.next = None



class Solution:
    # @return a ListNode
    def addTwoNumbers(self, l1, l2):

        head = ListNode(0)
        l = head
        carry = 0
        while l1 or l2 or carry:
            sum, carry = carry, 0
            if l1:
                sum += l1.val
                l1 = l1.next
            if l2:
                sum += l2.val
                l2 = l2.next
            sum, carry = sum%10, int(sum/10)
            l.next = ListNode(sum)
            l = l.next
        return head.next

if __name__ == '__main__':
    a, a.next, a.next.next = ListNode(2), ListNode(4), ListNode(3)
    b, b.next, b.next.next = ListNode(5), ListNode(6), ListNode(4)
    result = Solution().addTwoNumbers(a, b)
    print ("{0},{1},{2}".format(result.val, result.next.val, result.next.next.val))
