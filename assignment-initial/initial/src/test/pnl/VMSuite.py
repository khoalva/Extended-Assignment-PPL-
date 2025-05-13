import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):
    def test_simple_program(self):        
        input = """[[],[],[call(writeInt,[3])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 401))

    def test_simple_expression(self):        
        input = """[[],[],[call(writeInt,[add(3,1)])]]."""
        expect = "4"
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
        
    def test_406(self):        
        input = """[[],[],[call(writeInt,[2]), call(writeInt,[3])]]."""
        expect = "23"
        self.assertTrue(TestVM.test(input, expect, 406))
        
    def test_407(self):        
        input = """[[var(a,integer)],[],[assign(a,123),call(writeInt,[a])]]."""
        expect = "123"
        self.assertTrue(TestVM.test(input, expect, 407))
        
    def test_408(self):        
        input = """[[var(a,integer)],[],[assign(a,123),call(writeInt,[b])]]."""
        expect = "Undeclared identifier: b"
        self.assertTrue(TestVM.test(input, expect, 408))
        
    def test_409(self):        
        input = """[[const(a,1)],[],[assign(a,123)]]."""
        expect = "Cannot assign to a constant: assign(a,123)"
        self.assertTrue(TestVM.test(input, expect, 409))
        
    def test_410(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a,123), block([var(a,integer)],[assign(a,23), call(writeInt, [a])] ), call(writeInt, [a])]]."""
        expect = "23123"
        self.assertTrue(TestVM.test(input, expect, 410))
        
    def test_411(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a,123), block([var(b,integer)],[assign(b,23), call(writeInt, [a])])]]."""
        expect = "123"
        self.assertTrue(TestVM.test(input, expect, 411))
        
    def test_412(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a, add(true, 5))]]."""
        expect = "Type mismatch: add(true,5)"
        self.assertTrue(TestVM.test(input, expect, 412))
        
    def test_413(self):        
        input = """[[var(a,boolean)],
                    [],
                    [assign(a, add(4, 5))]]."""
        expect = "Type mismatch: assign(a,add(4,5))"
        self.assertTrue(TestVM.test(input, expect, 413))
    
    def test_414(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a, sub(add(4, 5))), call(writeInt, [a])]]."""
        expect = "-9"
        self.assertTrue(TestVM.test(input, expect, 414))
        
    def test_415(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a, imod( times(sub(add(-4, -5)), 5), idiv(37,5))), call(writeInt, [a])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 415))
        
    def test_416(self):        
        input = """[[var(a,integer)],
                    [],
                    [assign(a, imod( times(sub(add(-4.0, -5)), 5), idiv(37,5))), call(writeInt, [a])]]."""
        expect = "Type mismatch: imod(times(sub(add(-4.0,-5)),5),idiv(37,5))"
        self.assertTrue(TestVM.test(input, expect, 416))
        
    def test_417(self):        
        input = """[[var(a,boolean)],
                    [],
                    [assign(a, bnot(true)), call(writeBool, [a])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 417))
        
        
    def test_418(self):        
        input = """[[var(a,boolean)],
                    [],
                    [assign(a, band(bnot(true), add(3,4))), call(writeBool, [a])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 418))
        
    def test_419(self):        
        input = """[[var(a,boolean)],
                    [],
                    [assign(a, greater(9,5)), call(writeBool, [a])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 419))
        
    def test_420(self):        
        input = """[[var(a,boolean), var(b,integer), var(c,integer)],
                    [],
                    [assign(b,5), assign(c,5), assign(a, eql(b,c)), call(writeBool, [a]) ]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 420))
    
    def test_421(self):        
        input = """[[var(a,boolean), var(b,integer), var(c,integer)],
                    [],
                    [assign(b,5), assign(c, add(b,2)), assign(a, ne(b,c)), call(writeBool, [a]) ]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 421))
    
    def test_422(self):        
        input = """[[var(a,boolean), var(b,integer), var(c,integer)],
                    [func(foo, [par(x, integer), par(y, boolean)], integer, [assign(x, 6), assign(foo, 2), assign(foo, x)])],
                    [assign(b, call(foo, [add(2,4), true])), call(writeInt, [b])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 422))
        
    def test_423(self):        
        input = """[[var(a,boolean)],
                    [func(foo, [par(y, boolean), par(x, integer)], boolean, [assign(y, bnot(y)), assign(foo, band(y, eql(x,5)))])],
                    [call(writeBoolLn, [call(foo, [false, add(2,3)])]),  call(writeBoolLn, [call(foo, [true, add(3,3)])]), call(writeLn, [])]]."""
        expect = "true\nfalse\n\n"
        self.assertTrue(TestVM.test(input, expect, 423))
        
    def test_424(self):        
        input = """[[var(a,boolean)],
                    [func(foo, [par(y, boolean), par(x, integer)], boolean, [assign(y, bnot(y)), assign(foo, band(y, eql(x,5)))])],
                    [call(writeBoolLn, [call(foo, [false])]) ]]."""
        expect = "Wrong number of arguments: call(foo,[false])"
        self.assertTrue(TestVM.test(input, expect, 424))
        
    def test_425(self):        
        input = """[[var(a,boolean)],
                    [func(foo, [par(y, boolean), par(x, integer)], boolean, [assign(y, bnot(y)), assign(foo, band(y, eql(x,5)))])],
                    [call(writeBoolLn, [call(foo, [sub(6,7), add(2,3)])]) ]]."""
        expect = "Type mismatch: call(foo,[sub(6,7),add(2,3)])"
        self.assertTrue(TestVM.test(input, expect, 425))
        
    def test_426(self):        
        input = """[[var(a,boolean)],
                    [],
                    [call(writeInt, [3, 4])]]."""
        expect = "Wrong number of arguments: call(writeInt,[3,4])"
        self.assertTrue(TestVM.test(input, expect, 426))
        
    def test_427(self):        
        input = """[[var(a,boolean)],
                    [],
                    [call(writeInt, [])]]."""
        expect = "Wrong number of arguments: call(writeInt,[])"
        self.assertTrue(TestVM.test(input, expect, 427))
        
    def test_428(self):        
        input = """[[var(a,boolean), var(b, integer)],
                    [],
                    [assign(a,true),assign(b,4),if(a,call(writeIntLn,[add(b,4)])), if(band(a,eql(b,5)), 
                                                        call(writeStr,["then"]),call(writeStr,["else"]))]]."""
        expect = "8\nelse"
        self.assertTrue(TestVM.test(input, expect, 428))
        
    def test_429(self):        
        input = """[[var(a,integer), var(b, boolean)],
                    [func(foo, [par(a, integer)], boolean, [ assign(b, true), assign(c, 5), assign(foo, neq(a, c)) ])],
                    [ block([var(c, integer)], [call(writeBool, [call(foo, [1])])]) ]]."""
        expect = "Undeclared identifier: c"
        self.assertTrue(TestVM.test(input, expect, 429))
        
    # def test_430(self):        
    #     input = """[[var(a,integer), var(b, boolean), var(c, integer)],
    #                 [func(foo, [par(a, integer)], boolean, [
    #                     assign(b, true), 
    #                     assign(c, 5), 
    #                     block([], [
    #                         assign(b, eql(c, 3)),
    #                         assign(foo, true) ]
    #                         ),
    #                     assign(foo, ne(a, c)) ]
    #                     )
    #                 ],
    #                 [ block([], [
    #                     assign(a, 2),
    #                     call(writeBool, [call(foo, [5])]) ]
    #                     ) 
    #                 ]]."""
    #     expect = "false"
    #     self.assertTrue(TestVM.test(input, expect, 430))
        
    # def test_431(self):        
    #     input = """[[var(a,integer), var(b, boolean), var(c, integer)],
    #                 [func(foo, [par(a, integer)], integer, [
    #                     assign(b, true), 
    #                     assign(c, 5), 
    #                     block([], [
    #                         block([], [
    #                             block([], [
    #                                 assign(foo, 125)
    #                             ])
    #                         ])
    #                     ])
    #                 ])
    #                 ],
    #                 [ block([], [
    #                     assign(a, 2),
    #                     call(writeInt, [call(foo, [5])]) ]
    #                     ) 
    #                 ]]."""
    #     expect = "125"
    #     self.assertTrue(TestVM.test(input, expect, 431))
        
        
    # def test_432(self):        
    #     input = """[[var(a,integer), var(b, boolean), var(c, integer)],
    #                 [func(foo, [par(a, integer)], integer, [
    #                     assign(b, true), 
    #                     assign(c, 5), 
    #                     block([], [
    #                         block([], [
    #                             block([], [
                                    
    #                             ])
    #                         ])
    #                     ])
    #                 ])
    #                 ],
    #                 [ block([], [
    #                     assign(a, 2),
    #                     call(writeInt, [call(foo, [5])]) ]
    #                     ) 
    #                 ]]."""
    #     expect = "Invalid expression: call(foo,[5])"
    #     self.assertTrue(TestVM.test(input, expect, 432))
        
    # def test_433(self):        
    #     input = """[[var(a,integer), var(b, boolean), var(c, integer)],
    #                 [],
    #                 [
    #                     assign(a, 0),
    #                     while(less(a, 7), block([], 
    #                     [
    #                         assign(a, add(a, 1)),
    #                         call(writeInt, [a])
    #                     ]))
    #                 ]]."""
    #     expect = "1234567"
    #     self.assertTrue(TestVM.test(input, expect, 433))
        
    def test_434(self):        
        input = """[[var(a,integer)],
                    [func(foo, [], integer, [assign(a, 7), assign(foo, a)])],
                    [
                        assign(a, 3),
                        call(writeInt, [call(foo, [])]),
                        call(writeInt, [a])
                    ]]."""
        expect = "77"
        self.assertTrue(TestVM.test(input, expect, 434))
        
    def test_435(self):        
        input = """[[var(a,integer)],
                    [func(foo, [], integer, [assign(a, sub(a,1)), assign(foo, a)])],
                    [
                        assign(a, 10),
                        while( greater(call(foo, []), 0), block([], [
                            assign(a, sub(a,1)),
                            call(writeInt, [a])
                        ]))
                    ]].
                """
        expect = "86420"
        self.assertTrue(TestVM.test(input, expect, 435))
        
        
    def test_436(self):        
        input = """[[var(a,integer)],
                    [
                        func(foo, [], integer, [assign(a, add(a,1)), assign(foo, a)]),
                        func(fun, [par(x, integer), par(y, integer), par(z, integer)], integer, [assign(fun, times(times(x,y),z))])
                    ],
                    [
                        assign(a, 1),
                        call(writeInt,[call(fun,[add(call(foo, []), call(foo, [])), call(foo, []), call(foo, [])])])
                    ]].
                """
        expect = "100"
        self.assertTrue(TestVM.test(input, expect, 436))
        
    def test_437(self):        
        input = """[[var(a,integer)],
                    [
                        func(foo, [], integer, [assign(a, add(a,1)), assign(foo, a)])
                    ],
                    [
                        assign(a, 1),
                        if( band( le(call(foo, []), 2), band( le(call(foo, []), 2), le(call(foo, []), 2))),call(writeStr,["Y"]),
                        call(writeInt, [a])) 
                    ]].
                """
        expect = "3" 
        self.assertTrue(TestVM.test(input, expect, 437))
        
    def test_438(self):        
        input = """[[var(a,integer)],
                    [
                        func(foo, [], integer, [assign(a, add(a,1)), assign(foo, a)])
                    ],
                    [
                        assign(a, 1),
                        if(less(call(foo, []), call(foo, [])), call(writeStr,["Y"]), call(writeInt, [a])) 
                    ]].
                """
        expect = "Y" 
        self.assertTrue(TestVM.test(input, expect, 438))
        
    # def test_439(self):        
    #     input = """[[var(x,integer)],
    #                 [
    #                     func(factorial, [par(a, integer)], integer, [
    #                         if(le(a, 1), assign(factorial, 1), assign(factorial, times(a,call(factorial, [sub(a,1)]))))
    #                     ])
    #                 ],
    #                 [
    #                     assign(x, call(factorial, [5])),
    #                     call(writeInt, [x])
    #                 ]].
    #             """
    #     expect = "120" 
    #     self.assertTrue(TestVM.test(input, expect, 439))
        
    # def test_440(self):        
    #     input = """[[var(x,integer)],
    #                 [
    #                     func(f, [], integer, [assign(h, 3)])
    #                 ],
    #                 [
    #                     assign(x, call(f, []))
    #                 ]].
    #             """
    #     expect = "Undeclared identifier: h" 
    #     self.assertTrue(TestVM.test(input, expect, 440))
        
    # def test_441(self):        
    #     input = """[[var(x,integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     do([
    #                         call(writeStr, ["Number "]),
    #                         call(writeInt, [x]),
    #                         call(writeStr, [" is "]),
    #                         if(eql(imod(x, 2),0), call(writeStrLn, ["even"]), call(writeStrLn, ["odd"])),
    #                         assign(x, add(x, 1))
    #                     ], le(x, 4))
    #                 ]].
    #             """
    #     expect = "Number 0 is even\nNumber 1 is odd\nNumber 2 is even\nNumber 3 is odd\nNumber 4 is even\n" 
    #     self.assertTrue(TestVM.test(input, expect, 441))
        
    # def test_442(self):        
    #     input = """[[var(x,integer)],
    #                 [],
    #                 [
    #                     assign(x, 2),
    #                     do([
    #                         assign(x, sub(x, 1))
    #                     ], add(x, 4))
    #                 ]].
    #             """
    #     expect = "Type mismatch: do([assign(x,sub(x,1))],add(x,4))" 
    #     self.assertTrue(TestVM.test(input, expect, 442))
        
    def test_443(self):        
        input = """[[],
                    [],
                    [
                        loop(5, call(writeStr, ["H"])),
                        loop(-4, call(writeStr, ["T"]))
                    ]].
                """
        expect = "HHHHH" 
        self.assertTrue(TestVM.test(input, expect, 443))
    
    # def test_444(self):        
    #     input = """[[],
    #                 [],
    #                 [
    #                     while(true, break(null))
    #                 ]].
    #             """
    #     expect = ""
    #     self.assertTrue(TestVM.test(input, expect, 444))
        
    # def test_445(self):        
    #     input = """[[var(x, integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     while(le(x, 5), block([], [
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1)),
    #                         if(eql(x, 3), break(null))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "012"
    #     self.assertTrue(TestVM.test(input, expect, 445))
        
    # def test_446(self):        
    #     input = """[[var(x, integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     break(null),
    #                     while(le(x, 5), block([], [
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "Break not in a loop: break(null)"
    #     self.assertTrue(TestVM.test(input, expect, 446))
        
    # def test_447(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(x, 1),
    #                     assign(y, 0),
    #                     while(le(x, 3), block([], [
    #                         call(writeInt, [x]),
    #                         assign(y, x),
    #                         while(true, block([], [block([], [
    #                             call(writeInt, [y]),
    #                             assign(y, add(y, 1)),
    #                             if(ge(y, times(2, x)), break(null))
    #                         ])])),
    #                         assign(x, add(x, 1))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "112233345"
    #     self.assertTrue(TestVM.test(input, expect, 447))
        
    # def test_448(self):        
    #     input = """[[var(x, integer)],
    #                 [],
    #                 [
    #                     assign(x, 1),
    #                     while(le(x, 6), block([], [
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1)),
    #                         if(ge(x, 4), continue(null)),
    #                         call(writeStr, [","])
    #                     ]))
    #                 ]].
    #             """
    #     expect = "1,2,3456"
    #     self.assertTrue(TestVM.test(input, expect, 448))
        
    # def test_449(self):        
    #     input = """[[var(x, integer)],
    #                 [],
    #                 [
    #                     assign(x, 1),
    #                     continue(null),
    #                     while(le(x, 6), block([], [
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "Continue not in a loop: continue(null)"
    #     self.assertTrue(TestVM.test(input, expect, 449))
        
    # def test_450(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(x, 1),
    #                     while(le(x, 3), block([], [
    #                         call(writeInt, [x]),
    #                         assign(y, 0),
    #                         while(le(y, times(2, x)), block([], [block([], [
    #                             assign(y, add(y, 1)),
    #                             if(ge(y, add(x, 1)), continue(null)),
    #                             call(writeInt, [y])
    #                         ])])),
    #                         assign(x, add(x, 1))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "112123123"
    #     self.assertTrue(TestVM.test(input, expect, 450))
        
    # def test_451(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [func(foo, [], integer, [break(null), assign(foo, 1)])],
    #                 [
    #                     assign(x, 0),
    #                     while(eql(x, 0), assign(x, call(foo, [])))
    #                 ]].
    #             """
    #     expect = "Break not in a loop: break(null)"
    #     self.assertTrue(TestVM.test(input, expect, 451))
        
        
    # def test_452(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     do([
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1)),
    #                         if(ge(x, 7), break(null))
    #                     ], true)
    #                 ]].
    #             """
    #     expect = "0123456"
    #     self.assertTrue(TestVM.test(input, expect, 452))
        
    # def test_453(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     do([
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1)),
    #                         assign(y, x),
                            
    #                         do([
    #                             call(writeInt, [y]),
    #                             assign(y, add(y, 2)),
    #                             if(ge(y, times(2,x)), break(null))
    #                         ], true),
                            
    #                         call(writeStr, [";"]),
    #                         if(ge(x, 7), break(null))
    #                     ], true)
    #                 ]].
    #             """
    #     expect = "01;12;235;346;4579;56810;6791113;"
    #     self.assertTrue(TestVM.test(input, expect, 453))
        
    # def test_454(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(x, 0),
    #                     loop(5, block([], [
    #                         call(writeInt, [x]),
    #                         assign(x, add(x, 1)),
    #                         if(greater(x, 3), break(null))
    #                     ]))
    #                 ]].
    #             """
    #     expect = "0123"
    #     self.assertTrue(TestVM.test(input, expect, 454))
        
        
    # def test_455(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     assign(y, 0),
    #                     while(true, block([], [
    #                         assign(x, y),    
    #                         loop(5, block([], [
    #                             call(writeInt, [x]),
    #                             assign(x, add(x, 1)),
    #                             if(greater(x, 3), break(null))
    #                         ])),
    #                         assign(y, add(y, 1)),
    #                         if(greater(y, 5), break(null)),
    #                         call(writeStr, ["."])
    #                     ]))
    #                 ]].
    #             """
    #     expect = "0123.123.23.3.4.5"
    #     self.assertTrue(TestVM.test(input, expect, 455))
        
    # def test_456(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [],
    #                 [
    #                     do([
    #                         call(writeStr, ["HELLO"])
    #                     ], false)
    #                 ]].
    #             """
    #     expect = "HELLO"
    #     self.assertTrue(TestVM.test(input, expect, 456))
        
    def test_457(self):        
        input = """[[var(x, integer)],
                    [proc(foo, [par(a, integer)], [assign(x, add(x, 1)), call(writeInt, [a])])],
                    [
                        assign(x, 1),
                        call(foo, [1]),
                        call(foo, [2]),
                        call(foo, [3]),
                        call(writeInt, [x])
                    ]].
                """
        expect = "1234"
        self.assertTrue(TestVM.test(input, expect, 457))
        
    def test_458(self):        
        input = """[[var(x, integer)],
                    [proc(foo, [par(a, integer)], [assign(x, add(x, 1)), call(writeInt, [a])])],
                    [
                        assign(x, 1),
                        call(writeint, [x])
                    ]].
                """
        expect = "Undeclared procedure: call(writeint,[x])"
        self.assertTrue(TestVM.test(input, expect, 458))
        
    def test_459(self):        
        input = """[[var(x, float), var(y, integer)],
                    [proc(foo, [par(x, integer)], [])],
                    [
                        assign(y, 4),
                        assign(x, add(y, 1.0)),
                        call(foo, [x])
                    ]].
                """
        expect = "Type mismatch: call(foo,[x])"
        self.assertTrue(TestVM.test(input, expect, 459))
        
    # def test_460(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [func(max, [par(a, integer), par(b, integer)], integer, [
    #                         if(ge(a, b), assign(max, a), assign(max, b))
    #                     ])
    #                 ],
    #                 [
    #                     assign(y, 4),
    #                     assign(x, add(y, 1)),
    #                     assign(y, call(max, [x, y])),
    #                     call(writeInt, [y])
    #                 ]].
    #             """
    #     expect = "5"
    #     self.assertTrue(TestVM.test(input, expect, 460))
        
        
    # def test_461(self):        
    #     input = """[[var(x, integer), var(y, integer)],
    #                 [func(gcd, [par(a, integer), par(b, integer)], integer, [
    #                         if(eql(b, 0), assign(gcd, a), assign(gcd, call(gcd, [b, imod(a, b)])))
    #                     ])
    #                 ],
    #                 [
    #                     call(writeIntLn, [call(gcd, [7,4])]),
    #                     call(writeIntLn, [call(gcd, [7,7])]),
    #                     call(writeIntLn, [call(gcd, [60,45])]),
    #                     call(writeIntLn, [call(gcd, [120,24])])
    #                 ]].
    #             """
    #     expect = "1\n7\n15\n24\n"
    #     self.assertTrue(TestVM.test(input, expect, 461))
        