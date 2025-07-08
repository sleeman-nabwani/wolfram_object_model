// Virtual inheritance example
class VirtualBase {
public:
    virtual void virtualBaseMethod() {}
    virtual ~VirtualBase() {}
    
private:
    int virtualBaseData;
};

class VirtualDerived1 : virtual public VirtualBase {
public:
    virtual void derived1Method() {}
    virtual ~VirtualDerived1() {}
    
private:
    double derived1Data;
};

class VirtualDerived2 : virtual public VirtualBase {
public:
    virtual void derived2Method() {}
    virtual ~VirtualDerived2() {}
    
private:
    char derived2Data;
};

class VirtualMultiple : public VirtualDerived1, public VirtualDerived2 {
public:
    virtual void multipleMethod() {}
    virtual ~VirtualMultiple() {}
    
private:
    float multipleData;
}; 