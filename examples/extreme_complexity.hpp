// Extreme complexity example with multiple virtual bases and deep hierarchies
class VirtualRoot1 {
public:
    virtual void vr1Method() {}
    virtual ~VirtualRoot1() {}
    
private:
    int vr1Data;
};

class VirtualRoot2 {
public:
    virtual void vr2Method() {}
    virtual ~VirtualRoot2() {}
    
private:
    double vr2Data;
};

class VirtualRoot3 {
public:
    virtual void vr3Method() {}
    virtual ~VirtualRoot3() {}
    
private:
    char vr3Data;
};

class Middle1 : virtual public VirtualRoot1, virtual public VirtualRoot2 {
public:
    virtual void middle1Method() {}
    virtual void vr1Method() override {}
    virtual void vr2Method() override {}
    virtual ~Middle1() {}
    
private:
    float middle1Data;
};

class Middle2 : virtual public VirtualRoot2, virtual public VirtualRoot3 {
public:
    virtual void middle2Method() {}
    virtual void vr2Method() override {}
    virtual void vr3Method() override {}
    virtual ~Middle2() {}
    
private:
    long middle2Data;
};

class Middle3 : virtual public VirtualRoot1, virtual public VirtualRoot3 {
public:
    virtual void middle3Method() {}
    virtual void vr1Method() override {}
    virtual void vr3Method() override {}
    virtual ~Middle3() {}
    
private:
    short middle3Data;
};

class ComplexDerived1 : public Middle1, public Middle2 {
public:
    virtual void complex1Method() {}
    virtual void vr1Method() override {}
    virtual void vr2Method() override {}
    virtual void vr3Method() override {}
    virtual ~ComplexDerived1() {}
    
private:
    int complex1Data;
};

class ComplexDerived2 : public Middle2, public Middle3 {
public:
    virtual void complex2Method() {}
    virtual void vr1Method() override {}
    virtual void vr2Method() override {}
    virtual void vr3Method() override {}
    virtual ~ComplexDerived2() {}
    
private:
    double complex2Data;
};

class UltimateComplex : public ComplexDerived1, public ComplexDerived2 {
public:
    virtual void ultimateMethod() {}
    virtual void vr1Method() override {}
    virtual void vr2Method() override {}
    virtual void vr3Method() override {}
    virtual ~UltimateComplex() {}
    
private:
    char ultimateData;
}; 