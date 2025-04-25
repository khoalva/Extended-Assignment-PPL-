import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):
    def test_simple_program(self):        
        input = """[[],[],[call(writeInt,[3])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 401))

    def test_simple_expression(self):        
        input = """[[],[],[call(writeInt,[sub(3,1)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 402))

    def test_redeclaration(self):        
        input = """[[var(a,integer),var(b,integer),var(a,float)],[],[call(writeInt,[1])]]."""
        expect = "Redeclared identifier: var(a,float)"
        self.assertTrue(TestVM.test(input, expect, 403))

    def test_minus_expression(self):        
        input = """[[],[],[call(writeInt,[sub(3,1)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 404))

    def test_nested_expression(self):        
        input = """[[],[],[call(writeInt,[add(4,sub(3,1))])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 405))

    def test_simple_minus_expression(self):        
        input = """[[],[],[call(writeInt,[sub(3)])]]."""
        expect = "-3"
        self.assertTrue(TestVM.test(input, expect, 406))
    
    def test_times_expression(self):        
        input = """[[],[],[call(writeInt,[times(3,2)])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 407))
    
    def test_constant_declare(self):        
        input = """[[const(a,integer)],[],[call(writeInt,[1])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 408))

    def test_assignment(self):        
        input = """[[var(a,integer)],[],[assign(a,1),call(writeInt,1)]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 409))
