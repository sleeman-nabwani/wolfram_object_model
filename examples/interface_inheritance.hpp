// Interface-based inheritance example
class Interface1 {
public:
    virtual void interface1Method() = 0;
    virtual ~Interface1() = default;
};

class Interface2 {
public:
    virtual void interface2Method() = 0;
    virtual ~Interface2() = default;
};

class ConcreteBase {
public:
    virtual void concreteMethod() {}
    virtual ~ConcreteBase() {}
    
private:
    int concreteData;
};

class InterfaceImpl1 : public Interface1, public ConcreteBase {
public:
    virtual void interface1Method() override {}
    virtual void concreteMethod() override {}
    virtual ~InterfaceImpl1() {}
    
private:
    double impl1Data;
};

class InterfaceImpl2 : public Interface2, public ConcreteBase {
public:
    virtual void interface2Method() override {}
    virtual void concreteMethod() override {}
    virtual ~InterfaceImpl2() {}
    
private:
    char impl2Data;
};

class MultiInterface : public Interface1, public Interface2, public ConcreteBase {
public:
    virtual void interface1Method() override {}
    virtual void interface2Method() override {}
    virtual void concreteMethod() override {}
    virtual ~MultiInterface() {}
    
private:
    float multiData;
}; 