class A{};
class B1 : virtual public A {};
class B2 : virtual public A {};
class C : public B1, public B2 {};
class D1 : virtual public C{};
class D2 : public C{};
class D3 : virtual public C{};
class E : public D1, public D2, public D3 {};