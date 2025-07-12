// Multiple inheritance example
class Base1 {
public:
    virtual void base1Method() {}
    virtual ~Base1() {}
    
private:
    int base1Data;
};

class Base2 {
public:
    virtual void base2Method() {}
    virtual ~Base2() {}
    
private:
    double base2Data;
};

class MultipleDerived : public Base1, public Base2 {
public:
    virtual void derivedMethod() {}
    virtual void base1Method() override {}
    virtual void base2Method() override {}
    virtual ~MultipleDerived() {}
    
private:
    char derivedData;
}; 