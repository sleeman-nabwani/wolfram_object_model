class A{
    public:
    virtual void f();
    virtual void G();
    void h();
};

class B : public A {
    public:
    virtual void f();
    virtual void k();
};
///////////////////////////////////////////////////////section B///////////////////////////////////////////////////
class C{
    public:
    int x;
    virtual void f();
};

class D : virtual public C {
    public:
    int y;
    virtual void f();
};