// Single inheritance example
class Base {
public:
    virtual void baseMethod() {}
    virtual ~Base() {}
    
private:
    int baseData;
};

class Derived : public Base {
public:
    virtual void derivedMethod() {}
    virtual void baseMethod() override {}
    virtual ~Derived() {}
    
private:
    double derivedData;
}; 