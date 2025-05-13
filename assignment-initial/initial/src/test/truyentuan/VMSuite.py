import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):
    def test_501(self):
        input = """[[],[],[call(writeInt,[3])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 501))

    def test_502(self):
        input = """[[],[],[call(writeInt,[add(3,1)])]]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 502))

    def test_503(self):
        input = """[[],[],[call(writeInt,[sub(5)])]]."""
        expect = "-5"
        self.assertTrue(TestVM.test(input, expect, 503))

    def test_504(self):
        input = """[[],[],[call(writeInt,[sub(5,2)])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 504))

    def test_505(self):
        input = """[[],[],[call(writeInt,[add(4,sub(3,1))])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 505))

    def test_506(self):
        input = """[[],[],[call(writeInt,[times(2,3)])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 506))

    def test_507(self):
        input = """[[],[],[call(writeReal,[rdiv(7,2)])]]."""
        expect = "3.5"
        self.assertTrue(TestVM.test(input, expect, 507))

    def test_508(self):
        input = """[[],[],[call(writeInt,[idiv(7,2)])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 508))

    def test_509(self):
        input = """[[],[],[call(writeInt,[imod(7,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 509))

    def test_510(self):
        input = """[[],[],[call(writeBool,[bnot(true)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 510))

    def test_511(self):
        input = """[[],[],[call(writeBool,[band(true,false)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 511))

    def test_512(self):
        input = """[[],[],[call(writeBool,[band(true,true)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 512))

    def test_513(self):
        input = """[[],[],[call(writeBool,[bor(true,false)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 513))

    def test_514(self):
        input = """[[],[],[call(writeBool,[bor(false,false)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 514))

    def test_515(self):
        input = """[[],[],[call(writeBool,[greater(5,3)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 515))

    def test_516(self):
        input = """[[],[],[call(writeBool,[less(5,3)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 516))

    def test_517(self):
        input = """[[],[],[call(writeBool,[ge(3,3)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 517))

    def test_518(self):
        input = """[[],[],[call(writeBool,[le(2.5,3.5)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 518))

    def test_519(self):
        input = """[[],[],[call(writeBool,[eq(true,true)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 519))

    def test_520(self):
        input = """[[],[],[call(writeBool,[ne(true,false)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 520))

    def test_521(self):
        input = """[[],[],[call(writeBool,[eq(add(7,2),add(7,2))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 521))

    def test_522(self):
        input = """[[],[],[call(writeBool,[ne(7,8)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 522))

    def test_523(self):
        input = """[[const(c,10)],[],[call(writeInt,[c])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 523))

    def test_524(self):
        input = """[[const(a,7),const(b,3)],[],[call(writeInt,[sub(a,b)])]]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 524))

    def test_525(self):
        input = """[[const(x,4),const(y,2)],[],[call(writeInt,[times(x,add(y,3))])]]."""
        expect = "20"
        self.assertTrue(TestVM.test(input, expect, 525))

    def test_526(self):
        input = """[[const(f1,5.0),const(f2,2.5)],[],[call(writeReal,[rdiv(f1,f2)])]]."""
        expect = "2.0"
        self.assertTrue(TestVM.test(input, expect, 526))

    def test_527(self):
        input = """[[const(m,17),const(n,5)],[],[call(writeInt,[idiv(m,n)])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 527))

    def test_528(self):
        input = """[[const(m,17),const(n,5)],[],[call(writeInt,[imod(m,n)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 528))

    def test_529(self):
        input = """[[const(p,4),const(q,6)],[],[call(writeBool,[less(times(p,3),add(q,10))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 529))

    def test_530(self):
        input = """[[const(t,true),const(f,false)],[],[call(writeBool,[bor(bnot(f),band(t,f))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 530))

    def test_531(self):
        input = """[[const(i,5),const(j,5)],[],[call(writeBool,[eq(add(i,j),times(2,i))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 531))

    def test_532(self):
        input = """[[const(a,2),const(b,3.5)],[],[call(writeBool,[ge(rdiv(b,a),2.0)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 532))

    def test_533(self):
        input = """[[var(x,integer)],[],[assign(x,5),call(writeInt,[x])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 533))

    def test_534(self):
        input = """[[var(a,integer),const(b,2)],[],[assign(a,add(b,3)),call(writeInt,[a])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 534))

    def test_535(self):
        input = """[[var(a,integer)],[],[assign(a,1),block([var(a,integer)],[assign(a,2),call(writeInt,[a])]),call(writeInt,[a])]]."""
        expect = "21"
        self.assertTrue(TestVM.test(input, expect, 535))

    def test_536(self):
        input = """[[const(c,5)],[],[block([const(c,3)],[call(writeInt,[c])]),call(writeInt,[c])]]."""
        expect = "35"
        self.assertTrue(TestVM.test(input, expect, 536))

    def test_537(self):
        input = """[[],[func(foo,[par(x,integer),par(y,integer)],integer,[assign(foo,add(x,y))])],[call(writeInt,[call(foo,[3,4])])]]."""
        expect = "7"
        self.assertTrue(TestVM.test(input, expect, 537))

    def test_538(self):
        input = """[[],[func(double,[par(n,integer)],integer,[assign(double,times(n,2))])],[call(writeInt,[call(double,[add(2,3)])])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 538))

    def test_539(self):
        input = """[[],[func(inv,[par(f,float)],float,[assign(inv,rdiv(1.0,f))])],[call(writeReal,[call(inv,[2.0])])]]."""
        expect = "0.5"
        self.assertTrue(TestVM.test(input, expect, 539))

    def test_540(self):
        input = """[[],[func(notf,[par(b,boolean)],boolean,[assign(notf,bnot(b))])],[call(writeBool,[call(notf,[false])])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 540))

    def test_541(self):
        input = """[[var(x,integer)],[func(incx,[],integer,[assign(incx,add(x,1))])],[assign(x,5),call(writeInt,[call(incx,[])])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 541))

    def test_542(self):
        input = """[[var(a,integer)],[proc(seta,[par(v,integer)],[assign(a,v)])],[call(seta,[9]),call(writeInt,[a])]]."""
        expect = "9"
        self.assertTrue(TestVM.test(input, expect, 542))

    def test_543(self):
        input = """[[var(a,integer)],[proc(setb,[],[assign(a,add(2,3))])],[call(setb,[]),call(writeInt,[a])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 543))

    def test_544(self):
        input = """[[const(s,"Hello")],[],[call(writeStr,[s])]]."""
        expect = "Hello"
        self.assertTrue(TestVM.test(input, expect, 544))

    def test_545(self):
        input = """[[var(a,float),var(b,float)],[],[assign(a,1.5),assign(b,2.5),call(writeBool,[ge(a,b)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 545))

    def test_546(self):
        input = """[[var(b1,boolean),var(b2,boolean)],[],[assign(b1,true),assign(b2,true),call(writeBool,[eq(b1,b2)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 546))

    def test_547(self):
        input = """[[],[],[call(writeBool,[bor(true,add(1,2))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 547))

    def test_548(self):
        input = """[[],[],[call(writeBool,[band(false,add(1,2))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 548))

    def test_549(self):
        # need pow2 declared:
        input = """[[const(i,2),const(j,3)],[func(pow2,[par(x,integer)],integer,[assign(pow2,times(x,x))])],[call(writeInt,[call(pow2,[add(i,j)])])]]."""
        expect = "25"
        self.assertTrue(TestVM.test(input, expect, 549))

    def test_550(self):
        input = """[[var(x,integer),const(y,4)],[func(f,[par(z,integer)],integer,[assign(f,sub(z,y))])],[assign(x,10),call(writeInt,[call(f,[x])])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 550))

    def test_551(self):
        input = """[[const(a,2),const(b,3.5)],[func(cmp,[par(p,float),par(q,integer)],boolean,[assign(cmp,less(p,q))])],[call(writeBool,[call(cmp,[b,a])])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 551))
    
    def test_552(self):
        input = """[[],[],[loop(3,call(writeInt,[add(1,1)]))]]."""
        expect = "222"
        self.assertTrue(TestVM.test(input, expect, 552))

    def test_553(self):
        input = """[[var(x,integer)],[],[assign(x,10),call(writeInt,[x])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 553))

    def test_554(self):
        input = """[[var(a,integer)],[],[assign(a,1),block([var(a,integer)],[assign(a,2),call(writeInt,[a])]),call(writeInt,[a])]]."""
        expect = "21"
        self.assertTrue(TestVM.test(input, expect, 554))

    def test_555(self):
        input = """[[],[],[if(true,call(writeInt,[1]),call(writeInt,[2]))]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 555))

    def test_556(self):
        input = """[[],[],[if(false,call(writeInt,[1])),call(writeInt,[2])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 556))

    def test_557(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(i,1),assign(sum,0),while(less(i,4),block([], [assign(sum,add(sum,i)),assign(i,add(i,1))])),call(writeInt,[sum])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 557))

    def test_558(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(i,1),assign(sum,0),do([assign(sum,add(sum,i)),assign(i,add(i,1))],less(i,4)),call(writeInt,[sum])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 558))

    def test_559(self):
        input = """[[],[],[loop(4,call(writeInt,[1]))]]."""
        expect = "1111"
        self.assertTrue(TestVM.test(input, expect, 559))

    def test_560(self):
        input = """[[var(i,integer)],[],[assign(i,1),while(true,block([], [call(writeInt,[i]),assign(i,add(i,1)),if(greater(i,3),break(null))])),call(writeInt,[0])]]."""
        expect = "1230"
        self.assertTrue(TestVM.test(input, expect, 560))

    def test_561(self):
        input = """[[var(i,integer)],[],[assign(i,0),while(less(i,6),block([], [assign(i,add(i,1)),if(eq(imod(i,2),0),continue(null)),call(writeInt,[i])]))]]."""
        expect = "135"
        self.assertTrue(TestVM.test(input, expect, 561))

    def test_562(self):
        input = """[[],[],[loop(2,block([], [call(writeInt,[1]),loop(3,call(writeInt,[2]))]))]]."""
        expect = "12221222"
        self.assertTrue(TestVM.test(input, expect, 562))

    def test_563(self):
        input = """[[],[],[call(writeInt,[1]),call(writeLn,[]),call(writeInt,[2])]]."""
        expect = "1\n2"
        self.assertTrue(TestVM.test(input, expect, 563))

    def test_564(self):
        input = """[[],[],[break(null)]]."""
        expect = "Break not in a loop: break(null)"
        self.assertTrue(TestVM.test(input, expect, 564))

    def test_565(self):
        input = """[[],[],[continue(null)]]."""
        expect = "Continue not in a loop: continue(null)"
        self.assertTrue(TestVM.test(input, expect, 565))

    def test_566(self):
        input = """[[],[],[if(1,call(writeInt,[1]))]]."""
        expect = "Type mismatch: if(1,call(writeInt,[1]))"
        self.assertTrue(TestVM.test(input, expect, 566))

    def test_567(self):
        input = """[[],[],[assign(a,1)]]."""
        expect = "Undeclared identifier: a"
        self.assertTrue(TestVM.test(input, expect, 567))

    def test_568(self):
        input = """[[],[],[call(foo,[])]]."""
        expect = "Undeclared procedure: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 568))

    def test_569(self):
        input = """[[],[func(pow2,[par(x,integer)],integer,[assign(pow2,times(x,x))])],[call(writeInt,[call(pow2,[3,4,5])])]]."""
        expect = "Wrong number of arguments: call(pow2,[3,4,5])"
        self.assertTrue(TestVM.test(input, expect, 569))

    def test_570(self):
        input = """[[],[func(g,[par(a,integer),par(b,integer)],integer,[assign(g,add(a,b))])],[call(writeInt,[call(g,[7])])]]."""
        expect = "Wrong number of arguments: call(g,[7])"
        self.assertTrue(TestVM.test(input, expect, 570))

    def test_571(self):
        input = """[[const(a,5)],[],[assign(a,6)]]."""
        expect = "Cannot assign to a constant: assign(a,6)"
        self.assertTrue(TestVM.test(input, expect, 571))

    def test_572(self):
        input = """[[var(a,integer)],[],[assign(a,1.5)]]."""
        expect = "Type mismatch: assign(a,1.5)"
        self.assertTrue(TestVM.test(input, expect, 572))

    def test_573(self):
        input = """[[var(x,integer)],[],[block([var(x,integer)],[assign(x,2),block([var(y,integer)],[assign(y,add(x,3)),call(writeInt,[y])])])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 573))

    def test_574(self):
        input = """[[var(a,boolean)],[proc(p,[],[assign(a,true)])],[assign(a,false),if(bnot(a),call(p,[]),call(writeInt,[false])),call(writeBool,[a])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 574))

    def test_575(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(i,1),assign(sum,0),while(less(i,5),block([], [assign(sum,add(sum,i)),assign(i,add(i,1)),if(greater(sum,6),break(null))])),call(writeInt,[sum])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 575))

    def test_576(self):
        input = """[
            [var(i,integer),var(prod,integer)],
            [],
            [
            assign(i,1),
            assign(prod,1),
            do(
                [
                    if( eq(i,4),
                        block([],[assign(i,add(i,1)), continue(null)]),
                        assign(prod,times(prod,i))),
                    assign(i,add(i,1))
                ],
                less(i,6)
            ),
            call(writeInt,[prod])]]."""
        # multiply 1*2*3*5 = 30 (skips i=4)
        expect = "30"
        self.assertTrue(TestVM.test(input, expect, 576))

    def test_577(self):
        input = """[[],[],[loop(2,block([], [call(writeInt,[1]),loop(3,call(writeInt,[2]))])),call(writeLn,[])]]."""
        expect = "12221222\n"
        self.assertTrue(TestVM.test(input, expect, 577))

    def test_578(self):
        input = """[[],[func(foo,[par(n,integer)],integer,[assign(foo,times(n,n))])],[call(writeIntLn,[call(foo,[4])])]]."""
        expect = "16\n"
        self.assertTrue(TestVM.test(input, expect, 578))

    def test_579(self):
        input = """[[],[func(bar,[par(x,integer)],integer,[assign(bar,add(x,1))])],[call(writeInt,[call(bar,[])])]]."""
        expect = "Wrong number of arguments: call(bar,[])"
        self.assertTrue(TestVM.test(input, expect, 579))

    def test_580(self):
        input = """[[],[],[assign(x,1)]]."""
        expect = "Undeclared identifier: x"
        self.assertTrue(TestVM.test(input, expect, 580))

    def test_581(self):
        input = """[[var(a,integer),var(a,boolean)],[],[]]."""
        expect = "Redeclared identifier: var(a,boolean)"
        self.assertTrue(TestVM.test(input, expect, 581))

    def test_582(self):
        input = """[[],[func(f,[],integer,[]),proc(f,[],[])] ,[]]."""
        expect = "Redeclared procedure: f"
        self.assertTrue(TestVM.test(input, expect, 582))

    def test_583(self):
        input = """[[],[],[call(writeInt,[add(1,true)])]]."""
        expect = "Type mismatch: add(1,true)"
        self.assertTrue(TestVM.test(input, expect, 583))

    def test_584(self):
        input = """[[],[],[call(writeBool,[greater(1,true)])]]."""
        expect = "Type mismatch: greater(1,true)"
        self.assertTrue(TestVM.test(input, expect, 584))

    def test_585(self):
        input = """[[var(a,integer)],[func(foo,[],integer,[])],[assign(a,call(foo,[]))]]."""
        expect = "Invalid expression: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 585))

    def test_586(self):
        input = """[[var(a,integer)],[],[call(writeInt,[a])]]."""
        expect = "Invalid expression: a"
        self.assertTrue(TestVM.test(input, expect, 586))

    def test_587(self):
        input = """[[var(a,integer)],[],[assign(b,2)]]."""
        expect = "Undeclared identifier: b"
        self.assertTrue(TestVM.test(input, expect, 587))

    def test_588(self):
        input = """[[],[proc(p,[par(x,integer)],[])],[call(p,[])]]."""
        expect = "Wrong number of arguments: call(p,[])"
        self.assertTrue(TestVM.test(input, expect, 588))

    def test_589(self):
        input = """[[],[],[call(foo,[])]]."""
        expect = "Undeclared procedure: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 589))

    def test_590(self):
        input = """[[const(c,5)],[],[assign(c,10)]]."""
        expect = "Cannot assign to a constant: assign(c,10)"
        self.assertTrue(TestVM.test(input, expect, 590))

    def test_591(self):
        input = """[[],[],[call(writeBool,[band(false,add(1,true))])]]."""
        # second operand not evaluated, so no error
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 591))

    def test_592(self):
        input = """[[],[],[call(writeBool,[bor(true,sub(1,"x"))])]]."""
        # second operand not evaluated
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 592))

    def test_593(self):
        input = """[[],[],[block([const(x,1),const(x,2)],[call(writeInt,[x])])]]."""
        expect = "Redeclared identifier: const(x,2)"
        self.assertTrue(TestVM.test(input, expect, 593))

    def test_594(self):
        input = """[[],[],[loop(3,block([], [call(writeInt,[1]),break(null)])),call(writeInt,[0])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 594))

    def test_595(self):
        input = """[[],[],[loop(3,block([], [call(writeInt,[1]),continue(null),call(writeInt,[2])]))]]."""
        expect = "111"
        self.assertTrue(TestVM.test(input, expect, 595))

    def test_596(self):
        input = """[[const(s,"Hi"),var(i,integer)],[],[assign(i,1),while(less(i,3),block([], [call(writeStr,[s]),assign(i,add(i,1))])),call(writeLn,[])]]."""
        expect = "HiHi\n"
        self.assertTrue(TestVM.test(input, expect, 596))

    def test_597(self):
        input = """[[var(f,float)],[],[assign(f,add(1.2,3.3)),call(writeReal,[f])]]."""
        expect = "4.5"
        self.assertTrue(TestVM.test(input, expect, 597))

    def test_598(self):
        input = """[[var(b,boolean)],[],[assign(b,bor(false,band(true,true))),call(writeBool,[b])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 598))

    def test_599(self):
        input = """[[var(x,integer),const(y,2)],[func(pow,[par(a,integer),par(b,integer)],integer,[assign(pow,times(a,b))])],[assign(x,5),call(writeInt,[call(pow,[x,y])])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 599))

    def test_600(self):
        input = """[[var(acc,integer)],[proc(init,[],[assign(acc,0)]),proc(add1,[],[assign(acc,add(acc,1))])],[call(init,[]),loop(5,call(add1,[])),call(writeInt,[acc])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 600))

    def test_601(self):
        input = """[[],[],[call(writeInt,[add(times(2,3),sub(10,4))])]]."""
        expect = "12"
        self.assertTrue(TestVM.test(input, expect, 601))

    def test_602(self):
        input = """[[],[],[call(writeReal,[rdiv(add(5.0,2.5),sub(7.5,2.5))])]]."""
        expect = "1.5"
        self.assertTrue(TestVM.test(input, expect, 602))

    def test_603(self):
        input = """[[],[],[call(writeInt,[imod(add(20,5),times(3,7))])]]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 603))

    def test_604(self):
        input = """[[],[],[call(writeBool,[bor(band(true,false),bnot(false))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 604))

    def test_605(self):
        input = """[[],[],[call(writeBool,[greater(times(add(1,2),4),sub(20,3))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 605))

    def test_606(self):
        input = """[[],[],[call(writeBool,[ne(eq(add(2,3),times(1,5)),false)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 606))

    def test_607(self):
        input = """[[],[],[call(writeReal,[add(2,rdiv(9,2))])]]."""
        expect = "6.5"
        self.assertTrue(TestVM.test(input, expect, 607))

    def test_608(self):
        input = """[[],[],[call(writeBool,[less(add(1,2),times(1,add(1,1)))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 608))

    def test_609(self):
        input = """[[],[],[call(writeInt,[sub(sub(100,times(2,add(3,2))),10)])]]."""
        expect = "80"
        self.assertTrue(TestVM.test(input, expect, 609))

    def test_610(self):
        input = """[[],[],[call(writeBool,[band(false,imod(5,2))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 610))

    def test_611(self):
        input = """[[var(x,integer),const(y,5)],[proc(p,[],[assign(x,add(x,y))])],[assign(x,2),call(p,[]),call(writeInt,[x])]]."""
        expect = "7"
        self.assertTrue(TestVM.test(input, expect, 611))

    def test_612(self):
        input = """[[var(a,integer),var(b,integer)],[proc(swap,[par(u,integer),par(v,integer)],[assign(a,v),assign(b,u)])],[assign(a,1),assign(b,2),call(swap,[a,b]),call(writeInt,[a]),call(writeInt,[b])]]."""
        expect = "21"
        self.assertTrue(TestVM.test(input, expect, 612))

    def test_613(self):
        input = """[[var(a,integer),var(b,integer)],[proc(check,[par(u,integer),par(v,integer)],[assign(a,u),assign(b,v)])],[call(check,[3,3]),call(writeBool,[eq(a,b)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 613))

    def test_614(self):
        input = """[
            [var(a,integer),const(c,10)],
            [],
            [   
                assign(a,10),
                block(
                    [var(a,integer)],
                    [assign(a,20),call(writeInt,[a])]
                ),
                call(writeInt,[a]),
                call(writeInt,[c])
            ]
        ]."""
        expect = "201010"
        self.assertTrue(TestVM.test(input, expect, 614))

    def test_615(self):
        input = """[[var(sum,integer)],[proc(init,[],[assign(sum,0)]),proc(addN,[par(n,integer)],[assign(sum,add(sum,n))])],[call(init,[]),loop(3,call(addN,[block([var(x,integer)],[assign(x,add(1,1))])[1]])),call(writeInt,[sum])]]."""
        # simplified: block returns x? Actually block cannot return. Replace with direct arg 2.
        input = """[[var(sum,integer)],[proc(init,[],[assign(sum,0)]),proc(addN,[par(n,integer)],[assign(sum,add(sum,n))])],[call(init,[]),loop(3,call(addN,[2])),call(writeInt,[sum])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 615))

    def test_616(self):
        input = """[[var(f,float)],[proc(scale,[par(x,float)],[assign(f,times(x,1.5))])],[call(scale,[2.0]),call(writeReal,[f])]]."""
        expect = "3.0"
        self.assertTrue(TestVM.test(input, expect, 616))

    def test_617(self):
        input = """[[var(flag,boolean)],[proc(toggle,[],[assign(flag,bnot(flag))])],[assign(flag,false),call(toggle,[]),call(writeBool,[flag])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 617))

    def test_618(self):
        input = """[[const(msg,"Hi"),var(count,integer)],[proc(show,[],[assign(count,add(count,1)),call(writeStr,[msg])])],[assign(count,0),call(show,[]),call(show,[]),call(writeInt,[count])]]."""
        expect = "HiHi2"
        self.assertTrue(TestVM.test(input, expect, 618))

    def test_619(self):
        input = """[
            [var(a,integer),var(b,integer),var(c,integer)],
            [proc(calc,[],[
                assign(a,add(a,1)),
                assign(b,add(b,2)),
                assign(c,add(c,3))
                ])
            ],
            [
                assign(a,1),
                assign(b,2),
                assign(c,3),
                call(calc,[]),
                call(writeInt,[a]),
                call(writeInt,[b]),
                call(writeInt,[c])
            ]
        ]."""
        expect = "246"
        self.assertTrue(TestVM.test(input, expect, 619))

    def test_620(self):
        input = """[
            [var(x,integer),const(y,3)],
            [proc(foo,[par(z,integer)],[
                assign(x,times(z,y))
                ])
            ],
            [
                call(foo,[4]),
                block(
                    [var(x,integer)],
                    [call(foo,[5]),call(writeInt,[x])]
                ),
                call(writeInt,[x])
            ]]."""
        expect = "1512"
        self.assertTrue(TestVM.test(input, expect, 620))

    def test_621(self):
        input = """[[var(a,integer)],[proc(p,[],[assign(a,add(a,2))])],[assign(a,1),if(greater(a,0),block([], [call(p,[]),call(writeInt,[a])]),call(writeInt,[0]))]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 621))

    def test_622(self):
        input = """[
            [var(flag,boolean)],
            [proc(enable,[],[assign(flag,false)])],
            [
                assign(flag,true),
                if(flag,
                    block(
                        [],
                        [call(enable,[]),call(writeBool,[flag])]
                    ),
                    call(writeBool,[flag])
                )
            ]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 622))

    def test_623(self):
        input = """[[var(x,integer)],[proc(incr,[],[assign(x,add(x,1))])],[assign(x,0),if(less(x,1),if(eq(x,0),block([], [call(incr,[]),call(writeInt,[x])]),call(writeInt,[0])),call(writeInt,[100]))]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 623))

    def test_624(self):
        input = """[[var(a,integer),var(b,integer)],[proc(p,[],[assign(a,add(a,1)),assign(b,add(b,2))])],[assign(a,5),assign(b,3),if(band(greater(a,b),less(a,10)),block([], [call(p,[]),call(writeInt,[a]),call(writeInt,[b])]),call(writeInt,[0]))]]."""
        expect = "65"
        self.assertTrue(TestVM.test(input, expect, 624))

    def test_625(self):
        input = """[[var(x,integer),const(y,2)],[func(double,[par(n,integer)],integer,[assign(double,times(n,2))])],[assign(x,4),if(eq(call(double,[x]),8),call(writeInt,[1]),call(writeInt,[0]))]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 625))

    def test_626(self):
        input = """[
            [const(s1,"foo"),const(s2,"foo")],
            [],
            [if(eq(s1,s2),call(writeStr,[s1]),call(writeStr,["bar"]))]]."""
        expect = "Type mismatch: eq(s1,s2)"
        self.assertTrue(TestVM.test(input, expect, 626))

    def test_627(self):
        input = """[[var(c,integer)],[],[assign(c,0),if(false,call(writeInt,[1])),call(writeInt,[2]),call(writeInt,[c])]]."""
        expect = "20"
        self.assertTrue(TestVM.test(input, expect, 627))

    def test_628(self):
        input = """[[],[proc(test,[],[block([var(r,integer)],[assign(r,10),call(writeInt,[r])])])],[call(test,[]),if(false,call(writeInt,[1]),call(writeInt,[2]))]]."""
        expect = "102"
        self.assertTrue(TestVM.test(input, expect, 628))

    def test_629(self):
        input = """[[var(x,integer)],[proc(a,[],[block([var(y,integer)],[assign(y,5),assign(x,y)])])],[assign(x,1),call(a,[]),if(eq(x,5),call(writeInt,[x]),call(writeInt,[0]))]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 629))

    def test_630(self):
        input = """[[var(a,integer),var(b,integer),var(c,integer)],[],[assign(a,1),assign(b,2),assign(c,3),if(eq(a,1),block([], [if(eq(b,2),block([], [call(writeInt,[a]),call(writeInt,[b])]),call(writeInt,[0]))]),call(writeInt,[0])),call(writeInt,[c])]]."""
        expect = "123"
        self.assertTrue(TestVM.test(input, expect, 630))
    
    def test_631(self):
        input = """[[var(i,integer),var(sum,integer)],[proc(acc2,[],[assign(sum,add(sum,2))])],[assign(i,0),assign(sum,0),while(less(i,5),block([], [call(acc2,[]),assign(i,add(i,1))])),call(writeInt,[sum])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 631))

    def test_632(self):
        input = """[[var(i,integer),var(prod,integer)],[proc(mul3,[],[assign(prod,times(prod,3))])],[assign(i,0),assign(prod,1),while(less(i,4),block([], [call(mul3,[]),assign(i,add(i,1))])),call(writeInt,[prod])]]."""
        expect = "81"
        self.assertTrue(TestVM.test(input, expect, 632))

    def test_633(self):
        input = """[[var(i,integer),var(cnt,integer)],[proc(inc,[],[assign(cnt,add(cnt,1))])],[assign(i,0),assign(cnt,0),while(less(i,5),block([], [if(eq(imod(i,2),0),call(inc,[])),assign(i,add(i,1))])),call(writeInt,[cnt])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 633))

    def test_634(self):
        input = """[[var(i,integer),var(sum,integer)],[proc(addi,[par(x,integer)],[]),[assign(sum,add(sum,x))])],[assign(i,1),assign(sum,0),while(less(i,6),block([], [call(addi,[i]),assign(i,add(i,1))])),call(writeInt,[sum])]]."""
        # Note: declaration bracket corrected
        input = """[[var(i,integer),var(sum,integer)],[proc(addi,[par(x,integer)],[assign(sum,add(sum,x))])],[assign(i,1),assign(sum,0),while(less(i,6),block([], [call(addi,[i]),assign(i,add(i,1))])),call(writeInt,[sum])]]."""
        expect = "15"
        self.assertTrue(TestVM.test(input, expect, 634))

    def test_635(self):
        input = """[
            [var(a,integer),var(b,integer)],
            [],
            [
                assign(a,3),
                assign(b,4),
                while(greater(a,0),
                    block(
                        [], 
                        [
                            assign(a,sub(a,1)),
                            assign(b,add(b,2))
                        ]
                    )
                ),
                call(writeInt,[b])
            ]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 635))

    def test_636(self):
        input = """[
            [var(i,integer),var(output,integer)],
            [proc(ping,[],[assign(output,add(output,1))])],
            [
                assign(i,0),
                assign(output,0),
                while(less(i,3),
                    block(
                        [var(temp,integer)],
                        [
                            assign(temp,i),
                            call(ping,[]),
                            if(eq(temp,1),break(null)),
                            assign(i,add(i,1))
                        ]
                    )
                ),
                call(writeInt,[output])
            ]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 636))

    def test_637(self):
        input = """[[var(i,integer)],[proc(dec,[],[assign(i,sub(i,1))])],[assign(i,5),while(less(i,1),call(dec,[])),call(writeInt,[i])]]."""
        #  while(less(i,1)) is false immediately, so no change
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 637))

    def test_638(self):
        input = """[[var(i,integer),var(sum,integer)],[proc(addup,[],[assign(sum,add(sum,i)),assign(i,add(i,1))])],[assign(i,1),assign(sum,0),while(less(i,5),call(addup,[])),call(writeInt,[sum])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 638))

    def test_639(self):
        input = """[
            [var(i,integer),var(cnt,integer)],
            [],
            [
                assign(i,0),
                assign(cnt,0),
                while(less(i,5),block(
                    [],
                    [
                        if(greater(i,2),break(null)),
                        assign(cnt,add(cnt,1)),
                        assign(i,add(i,1))
                    ]
                )),
                call(writeInt,[cnt])
            ]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 639))

    def test_640(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(i,1),assign(sum,0),do([assign(sum,add(sum,i)),assign(i,add(i,1))],less(i,5)),call(writeInt,[sum])]]."""
        # i=1..4 sum=1+2+3+4=10
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 640))

    def test_641(self):
        input = """[[var(i,integer),var(cnt,integer)],[],[assign(i,1),assign(cnt,0),do([assign(cnt,add(cnt,1)),assign(i,add(i,1)),if(greater(i,3),break(null))],true),call(writeInt,[cnt])]]."""
        # cnt increments for i=1,2,3 then break
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 641))

    def test_642(self):
        input = """[[var(i,integer),var(evens,integer)],[],[assign(i,0),assign(evens,0),do([assign(i,add(i,1)),if(ne(imod(i,2),0),continue(null)),assign(evens,add(evens,1))],less(i,5)),call(writeInt,[evens])]]."""
        # count even i from 1..4: i=2,4 => 2
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 642))

    def test_643(self):
        input = """[[var(x,integer)],[proc(incx,[],[assign(x,add(x,1))])],[assign(x,0),do([block([var(tmp,integer)],[assign(tmp,add(x,2)),assign(x,tmp)]),call(incx,[])],less(x,5)),call(writeInt,[x])]]."""
        # x: tmp=x+2 then x=tmp then incx => x increases by 3 each iteration: 0->3->6 stop when tmp step? Actually less(x,5) check: 0->3, 3<5->6, 6<5 false => x=6
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 643))

    def test_644(self):
        input = """[[var(i,integer)],[proc(reset,[],[assign(i,0)])],[assign(i,5),loop(4,call(reset,[])),call(writeInt,[i])]]."""
        # reset sets i=0 each iteration => remains 0
        expect = "0"
        self.assertTrue(TestVM.test(input, expect, 644))

    def test_645(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(i,0),assign(sum,0),loop(add(2,3),block([], [assign(i,add(i,1)),if(eq(imod(i,2),0),continue(null)),assign(sum,add(sum,i))])),call(writeInt,[sum])]]."""
        # sum odd i from 1..5: 1+3+5=9
        expect = "9"
        self.assertTrue(TestVM.test(input, expect, 645))

    def test_646(self):
        input = """[[var(a,integer),var(b,integer)],[proc(foo,[],[assign(a,add(a,1)),assign(b,add(b,2))])],[assign(a,0),assign(b,0),loop(3,call(foo,[])),call(writeInt,[a]),call(writeInt,[b])]]."""
        # foo increments a by1 and b by2 three times => a=3,b=6
        expect = "36"
        self.assertTrue(TestVM.test(input, expect, 646))

    def test_647(self):
        input = """[
            [var(i,integer),var(cnt,integer)],
            [],
            [
                assign(i,1),
                assign(cnt,0),
                loop(3,
                    do(
                        [assign(cnt,add(cnt,i)),assign(i,add(i,1))],
                    less(i,5))
                ),
                call(writeInt,[cnt])
            ]]."""
        expect = "21"
        self.assertTrue(TestVM.test(input, expect, 647))

    def test_648(self):
        input = """[[var(i,integer),var(sum,integer)],[],[assign(sum,0),assign(i,1),do([loop(2,assign(sum,add(sum,i))) ,assign(i,add(i,1))],less(i,4)),call(writeInt,[sum])]]."""
        # do: i=1: loop2 sum+=1 twice=2,i=2; 2<4->i=2: loop sum+=2 twice=4+2->6,i=3;3<4->i=3 loop sum+=3*2=6->12,i=4;4<4 false => sum=12
        expect = "12"
        self.assertTrue(TestVM.test(input, expect, 648))

    def test_649(self):
        input = """[[var(i,integer),var(j,integer),var(cnt,integer)],[],[assign(i,0),assign(cnt,0),loop(2,do([assign(j,add(i,1)),assign(cnt,add(cnt,j)),assign(i,add(i,1))],less(i,3))),call(writeInt,[cnt])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 649))

    def test_650(self):
        input = """[[var(x,integer),var(y,integer),var(z,integer)],[],[assign(x,1),assign(y,1),assign(z,1),loop(3,block([], [assign(x,times(x,2)),assign(y,add(y,3)),assign(z,sub(z,1))])),call(writeInt,[x]),call(writeInt,[y]),call(writeInt,[z])]]."""
        # x:1->2->4->8; y:1->4->7->10; z:1->0->-1->-2 => "8 10 -2"
        expect = "810-2"
        self.assertTrue(TestVM.test(input, expect, 650))
    
    def test_651(self):
        input = """[
            [var(sum,integer),var(i,integer)],
            [func(rs,[par(n,integer)],integer,[
                assign(rs,0),
                assign(i,1),
                do(
                    [assign(rs,add(rs,i)),assign(i,add(i,1))],
                    less(i,add(n,1))
                )
            ])],
            [call(writeInt,[call(rs,[4])])]
        ]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 651))

    def test_652(self):
        input = """[
            [const(x,5),var(y,integer)],
            [],
            [
                assign(y,3),
                block(
                    [const(x,2),var(z,integer)],
                    [
                        assign(z,times(x,y)),
                        call(writeInt,[z])
                    ]
                ),
                call(writeInt,[x])
            ]
        ]."""
        expect = "65"
        self.assertTrue(TestVM.test(input, expect, 652))

    def test_653(self):
        input = """[
            [var(a,integer),var(b,integer),var(c,integer)],
            [proc(updateABC,[],[
                assign(a,add(a,1)),
                if(bnot(eq(a,2)),assign(b,add(b,2))),
                assign(c,add(c,3))
            ])],
            [
                assign(a,1),assign(b,0),assign(c,0),
                call(updateABC,[]),
                call(writeInt,[a]),call(writeInt,[b]),call(writeInt,[c])
            ]
        ]."""
        expect = "203"
        self.assertTrue(TestVM.test(input, expect, 653))

    def test_654(self):
        input = """[
            [var(cnt,integer)],
            [],
            [
                assign(cnt,0),
                loop(5,block([],[
                    if(eq(imod(cnt,2),0),call(writeInt,[cnt])),
                    assign(cnt,add(cnt,1))
                ])),
                call(writeInt,[cnt])
            ]
        ]."""
        expect = "0245"
        self.assertTrue(TestVM.test(input, expect, 654))

    def test_655(self):
        input = """[
            [var(flag,boolean)],
            [proc(toggle,[],[assign(flag,bnot(flag))])],
            [
                assign(flag,false),
                call(toggle,[]),
                call(writeBool,[flag]),
                call(toggle,[]),
                call(writeBool,[flag])
            ]
        ]."""
        expect = "truefalse"
        self.assertTrue(TestVM.test(input, expect, 655))

    def test_656(self):
        input = """[
            [var(sum,integer),var(i,integer)],
            [],
            [
                assign(sum,0),assign(i,1),
                while(less(i,5),block([],[
                    if(eq(imod(i,2),1),assign(sum,add(sum,i))),
                    assign(i,add(i,1))
                ])),
                call(writeInt,[sum])
            ]
        ]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 656))

    def test_657(self):
        input = """[
            [var(msg,string),var(n,integer)],
            [],
            [
                assign(msg,"x"),
                assign(n,3),
                loop(n,call(writeStr,[msg])),
                call(writeLn,[])
            ]
        ]."""
        expect = "xxx\n"
        self.assertTrue(TestVM.test(input, expect, 657))

    def test_658(self):
        input = """[
            [var(i,integer),var(total,integer)],
            [proc(inner,[],[
                do([assign(total,add(total,i)),assign(i,add(i,1))],less(i,3))
            ])],
            [
                assign(i,0),assign(total,0),
                call(inner,[]),
                call(writeInt,[total])
            ]
        ]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 658))

    def test_659(self):
        input = """[
            [var(x,integer),var(y,integer)],
            [],
            [
                assign(x,2),assign(y,3),
                block([var(t,integer)],[
                    assign(t,times(x,y)),
                    assign(x,add(t,1))
                ]),
                call(writeInt,[x])
            ]
        ]."""
        expect = "7"
        self.assertTrue(TestVM.test(input, expect, 659))

    def test_660(self):
        input = """[
            [const(a,2),const(b,3),var(result,integer)],
            [func(mulAdd,[par(x,integer),par(y,integer)],integer,[
                assign(mulAdd,add(times(x,y),a))
            ])],
            [
                assign(result,call(mulAdd,[b,b])),
                call(writeInt,[result])
            ]
        ]."""
        expect = "11"
        self.assertTrue(TestVM.test(input, expect, 660))

    def test_661(self):
        input = """[[var(writeInt,integer)],[],[]]."""
        expect = "Redeclared identifier: var(writeInt,integer)"
        self.assertTrue(TestVM.test(input, expect, 661))

    def test_662(self):
        input = """[[const(a,1),const(a,2)],[],[]]."""
        expect = "Redeclared identifier: const(a,2)"
        self.assertTrue(TestVM.test(input, expect, 662))

    def test_663(self):
        input = """[[],[proc(p,[par(x,integer),par(x,boolean)],[assign(x,1)])],[]]."""
        expect = "Redeclared identifier: par(x,boolean)"
        self.assertTrue(TestVM.test(input, expect, 663))

    def test_664(self):
        input = """[[],[],[block([var(a,integer),var(a,boolean)],[call(writeInt,[0])])]]."""
        expect = "Redeclared identifier: var(a,boolean)"
        self.assertTrue(TestVM.test(input, expect, 664))

    def test_665(self):
        input = """[[var(foo,integer)],[func(foo,[],integer,[])],[]]."""
        expect = "Redeclared function: foo"
        self.assertTrue(TestVM.test(input, expect, 665))

    def test_666(self):
        input = """[[],[proc(bar,[],[]) ,proc(bar,[par(x,integer)],[])],[]]."""
        expect = "Redeclared procedure: bar"
        self.assertTrue(TestVM.test(input, expect, 666))

    def test_667(self):
        input = """[[],[],[call(writeBool,[band(0,1)])]]."""
        expect = "Type mismatch: band(0,1)"
        self.assertTrue(TestVM.test(input, expect, 667))

    def test_668(self):
        input = """[[var(a,integer)],[],[assign(a,"oops")]]."""
        expect = "Type mismatch: assign(a,oops)"
        self.assertTrue(TestVM.test(input, expect, 668))

    def test_669(self):
        input = """[[],[],[if(5,call(writeInt,[1]),call(writeInt,[0]))]]."""
        expect = "Type mismatch: if(5,call(writeInt,[1]),call(writeInt,[0]))"
        self.assertTrue(TestVM.test(input, expect, 669))

    def test_670(self):
        input = """[[],[],[while(add(1,1),call(writeInt,[0]))]]."""
        expect = "Type mismatch: while(add(1,1),call(writeInt,[0]))"
        self.assertTrue(TestVM.test(input, expect, 670))

    def test_671(self):
        input = """[[var(a,integer),var(b,integer)],[],[assign(b,add(a,1))]]."""
        expect = "Invalid expression: a"
        self.assertTrue(TestVM.test(input, expect, 671))

    def test_672(self):
        input = """[[var(a,integer)],[],[assign(a,call(foo,[]))]]."""
        expect = "Undeclared function: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 672))

    def test_673(self):
        input = """[[],[],[call(foo,[])]]."""
        expect = "Undeclared procedure: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 673))

    def test_674(self):
        input = """[[],[func(bad,[],integer,[])],[call(writeInt,[call(bad,[])])]]."""
        expect = "Invalid expression: call(bad,[])"
        self.assertTrue(TestVM.test(input, expect, 674))

    def test_675(self):
        input = """[[],[],[break(null)]]."""
        expect = "Break not in a loop: break(null)"
        self.assertTrue(TestVM.test(input, expect, 675))

    def test_676(self):
        input = """[[],[],[continue(null)]]."""
        expect = "Continue not in a loop: continue(null)"
        self.assertTrue(TestVM.test(input, expect, 676))

    def test_679(self):
        input = """[[const(a,7)],[],[assign(a,8)]]."""
        expect = "Cannot assign to a constant: assign(a,8)"
        self.assertTrue(TestVM.test(input, expect, 679))

    def test_678(self):
        input = """[[],[],[block([const(x,3)],[assign(x,4)])]]."""
        expect = "Cannot assign to a constant: assign(x,4)"
        self.assertTrue(TestVM.test(input, expect, 678))
