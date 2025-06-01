#include <iostream>
using namespace std;

struct X {
    int x;

    X() {
        cout << "X()";
        x = 7;
        f();
    }
    X(int val) {
        cout << "X(int)";
        x = val;
        f();
    }
    virtual void f() {
        cout << "X::f()";
        g2();
    }
    virtual void g2() {
        cout << "X::g2()";
        }
};



struct Y1 : virtual X {
    Y1(): X(2) {
        cout << "Y1()";
        f();
    }
    virtual void f() {
        cout << "Y1::f()";
        g1();
    }
    virtual void g1() {
    cout << "Y1::g1()";
    }
};


struct Y2 : virtual X {
    Y2(): X(2) {
        cout << "Y2()";
        f();
    }
    virtual void f() {
        cout << "Y2::f()";
        g2();
    }
    virtual void g2() {
        cout << "Y2::g2()";
    }
};

struct Z : Y1, Y2 {
    Z() {
        cout << "Z()";
        f();    
    }
    virtual void f() {
        cout << "Z::f()";
        g1();
    }
    virtual void g1() {
        cout << "Z::g1()";
    }
};
