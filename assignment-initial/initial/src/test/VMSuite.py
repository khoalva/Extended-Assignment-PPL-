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
    
    def test_writeInt_with_assignment(self):        
        input = """[[var(a, integer)],[],[assign(a,1),call(writeInt,[a])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 409))

    def test_assignment(self):        
        input = """[[var(a,integer)],[],[assign(a,add(1,2)),call(writeInt,[a])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 410))
    
    def test_rdiv_int_expression(self):        
        input = """[[],[],[call(writeInt,[rdiv(3,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 411))

    def test_rdiv_float_expression(self):        
        input = """[[],[],[call(writeInt,[rdiv(3.0,2.0)])]]."""
        expect = "Type mismatch: call(writeInt,[3.0 rdiv 2.0])"
        self.assertTrue(TestVM.test(input, expect, 412))

    def test_idiv_expression(self):        
        input = """[[],[],[call(writeInt,[idiv(3,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 413))

    def test_mod_expression(self):        
        input = """[[],[],[call(writeInt,[imod(3,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 414))

    def test_bor_expression(self):        
        input = """[[var(a, boolean), var(b, boolean)],[],[assign(a,true),assign(b,false),call(writeBool,[bor(a,b)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 415))

    def test_writeStr(self):        
        input = """[[var(a,string)],[],[assign(a,"Hello"),call(writeStr,[a])]]."""
        expect = "Hello"
        self.assertTrue(TestVM.test(input, expect, 416))
    
    def test_band_expression(self):        
        input = """[[var(a, boolean), var(b, boolean)],[],[assign(a,true),assign(b,bnot(false)),call(writeBool,[band(a,b)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 417))

    def test_le_expression(self):        
        input = """[[],[],[call(writeBool,[le(3,3)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 418))
    
    def test_complex_bool_expression(self):        
        input = """[[],[],[call(writeBool,[band(le(3,2),bor(true,false))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 419))
    
    def test_complex_bool_expression_2(self):        
        input = """[[var(a,integer),var(b,integer)],[],[assign(a,10),assign(b,10),call(writeBool,[band(eql(a,b),ne(3,2))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 420))
    
    def test_if_statement(self):        
        input = """[[],[],[if(le(3,3), call(writeInt,[1])), call(writeInt,[2])]]."""
        expect = "12"
        self.assertTrue(TestVM.test(input, expect, 421))

    def test_if_else_statement(self):
        input = """[[],[],[if(le(3,2), call(writeInt,[1]), call(writeInt,[2]))]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 422))

    def test_block_statement(self):        
        input = """[[var(a,integer)],[],[block([],[assign(a,1),call(writeInt,[a])])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 423))
    
    def test_block_statement_2(self):        
        input = """[[var(a,integer)],[],[block([],[assign(a,1)]),call(writeInt,[a])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 424))
    
    def test_while_statement(self):        
        input = """[[var(a,integer)],[],[assign(a,0),while(le(a,3),block([],[call(writeInt,[a]),assign(a,add(a,1))]))]]."""
        expect = "0123"
        self.assertTrue(TestVM.test(input, expect, 425))
    
    def test_do_statement(self):        
        input = """[[var(a,integer)],[],[assign(a,0),do([call(writeInt,[a]),assign(a,add(a,1))],le(a,3))]]."""
        expect = "0123"
        self.assertTrue(TestVM.test(input, expect, 426))
    
    def test_loop_statement(self):        
        input = """[[],[],[loop(5, call(writeInt,[1]))]]."""
        expect = "11111"
        self.assertTrue(TestVM.test(input, expect, 427))
    
    def test_break_statement(self):        
        input = """[[],[],[break(null), call(writeInt,[1]), call(writeInt,[2])]]."""
        expect = "Break not in a loop: null"
        self.assertTrue(TestVM.test(input, expect, 428))
    
    def test_continue_statement(self):
        input = """[[],[],[continue(null), call(writeInt,[1]), call(writeInt,[2])]]."""
        expect = "Continue not in a loop: null"
        self.assertTrue(TestVM.test(input, expect, 429))
    
    def test_while_break_statement(self):        
        input = """[[var(a,integer)],[],[assign(a,0),while(le(a,3),block([],[call(writeInt,[a]),if(le(a,2),break(null)),assign(a,add(a,1))]))]]."""
        expect = "0"
        self.assertTrue(TestVM.test(input, expect, 430))
    
    def test_while_continue_statement(self):
        input = """[[var(a,integer)],[],[assign(a,0),while(le(a,5),block([],[assign(a,add(a,1)),if(le(a,2),continue(null)), call(writeInt,[a])]))]]."""
        expect = "3456"
        self.assertTrue(TestVM.test(input, expect, 431))
    
    def test_do_break_statement(self):
        input = """[[var(a,integer)],[],[assign(a,0),do([call(writeInt,[a]),if(ge(a,2),break(null)),assign(a,add(a,1))],le(a,3))]]."""
        expect = "012"
        self.assertTrue(TestVM.test(input, expect, 432))

    def test_do_continue_statement(self):
        input = """[[var(a,integer)],[],[assign(a,0),do([assign(a,add(a,1)),if(le(a,2),continue(null),call(writeInt,[a]))],le(a,5))]]."""
        expect = "3456"
        self.assertTrue(TestVM.test(input, expect, 433))
    
    def test_loop_break_statement(self):
        input = """[[],[],[loop(5, block([],[call(writeInt,[1]),if(le(2,3),break(null)),call(writeInt,[2])]))]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 434))

    def test_loop_continue_statement(self):
        input = """[[],[],[loop(5, block([],[call(writeInt,[1]),if(le(2,3),continue(null)),call(writeInt,[2])]))]]."""
        expect = "11111"
        self.assertTrue(TestVM.test(input, expect, 435))

    def test_function_declaration(self):        
        input = """[[var(a,integer)],[func(foo,[par(m,integer),par(n,integer)],integer,[assign(m,1), assign(n,2), assign(foo, add(m,n))])],[call(writeInt,[1])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 436))
    
    def test_function_declaration_2(self):        
        input = """[[var(a,integer)],[func(foo,[par(m,integer),par(n,integer)],integer,[assign(m,1), assign(n,2), assign(foo, add(m,n))]), func(foo,[],string,[])],[]]."""
        expect = "Redeclared function: foo"
        self.assertTrue(TestVM.test(input, expect, 437))
    
    def test_function_declaration_3(self):        
        input = """[[var(a,integer)],[func(foo,[par(m,integer),par(n,integer)],integer,[assign(m,1), assign(n,2), assign(foo, add(m,n))]), proc(foo,[],[])],[]]."""
        expect = "Redeclared procedure: foo"
        self.assertTrue(TestVM.test(input, expect, 438))
    
    def test_proc_call(self):        
        input = """[[var(a,integer)],[proc(foo,[par(m,integer),par(n,integer)],[assign(m,1), assign(n,2), call(writeInt,[add(m,n)])])],[call(foo,[1,2])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 439))
    
    def test_proc_call_2(self):        
        input = """[[var(a,integer)],[proc(foo,[par(m,integer),par(n,integer)],[assign(m,1), assign(n,2), call(writeInt,[add(m,n)])])],[call(foo,[1])]]."""
        expect = "Wrong number of arguments: call(foo,[1])"
        self.assertTrue(TestVM.test(input, expect, 440))
    
    def test_func_call(self):
        input = """[[var(a,integer)],[func(foo,[par(m,integer),par(n,integer)],integer,[assign(m,1), assign(n,2), assign(foo, add(m,n))])],[call(writeInt,[call(foo,[1,2])])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 441))
    
    def test_assign_to_constant(self):
        input = """[[const(a,integer)],[],[assign(a, 1)]]."""
        expect = "Cannot assign to a constant: a"
        self.assertTrue(TestVM.test(input, expect, 442))
    
    def test_undeclared_procedure(self):
        input = """[[],[],[call(foo,[1,2])]]."""
        expect = "Undeclared procedure: foo"
        self.assertTrue(TestVM.test(input, expect, 443))
    
    def test_undeclared_function(self):
        input = """[[],[],[call(writeInt, [call(foo,[1,2])])]]."""
        expect = "Undeclared function: foo"
        self.assertTrue(TestVM.test(input, expect, 444))

    def test_invalid_expression(self):
        input = """[[var(a,integer)],[func(foo,[],integer,[])],[assign(a,call(foo,[]))]]."""
        expect = "Invalid expression: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 445))
    
    def test_invalid_expression_2(self):
        input = """[[var(a,integer)],[],[call(writeInt,[a])]]."""
        expect = "Invalid expression: a"
        self.assertTrue(TestVM.test(input, expect, 446))